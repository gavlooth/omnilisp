module lisp;


import std::io;
import main;

fn bool boundary_state_matches_snapshot(Interp* interp, BoundaryInterpState saved) @inline {
    return interp.current_scope == saved.current_scope &&
           interp.releasing_scope == saved.releasing_scope;
}

fn bool boundary_session_nested_stress_iteration(Interp* interp, main::ScopeRegion* outer_scope, main::ScopeRegion* inner_scope) {
    bool iter_ok = false;
    BoundarySession outer = boundary_session_begin(interp);
    defer boundary_session_end(interp, &outer);

    interp.current_scope = outer_scope;
    interp.releasing_scope = inner_scope;
    {
        BoundarySession inner = boundary_session_begin(interp);
        defer boundary_session_end(interp, &inner);

        interp.current_scope = inner_scope;
        interp.releasing_scope = outer_scope;
        EvalResult r = run("(if)", interp);
        bool inner_eval_ok = r.error.has_error || (r.value != null && r.value.tag == ERROR);
        iter_ok = inner_eval_ok;
    }

    return iter_ok &&
           interp.current_scope == outer_scope &&
           interp.releasing_scope == inner_scope;
}

fn void run_memory_lifetime_boundary_session_nested_stress_test(Interp* interp, int* pass, int* fail) {
    // Invariant: repeated nested boundary sessions preserve strict LIFO state restoration.
    {
        BoundaryInterpState saved = boundary_save_interp_state(interp);
        main::ScopeRegion* scope_a = main::scope_create(saved.current_scope);
        main::ScopeRegion* scope_b = main::scope_create(saved.current_scope);
        if (scope_a == null || scope_b == null) {
            if (scope_b != null) main::scope_release(scope_b);
            if (scope_a != null) main::scope_release(scope_a);
            io::printn("[FAIL] lifetime: boundary session nested stress");
            (*fail)++;
            return;
        }

        bool ok = true;
        usz failed_iter = usz.max;
        for (usz i = 0; i < 192 && ok; i++) {
            BoundaryInterpState iter_saved = boundary_save_interp_state(interp);
            main::ScopeRegion* outer_scope = (i & 1) == 0 ? scope_a : scope_b;
            main::ScopeRegion* inner_scope = (i & 1) == 0 ? scope_b : scope_a;
            bool iter_ok = boundary_session_nested_stress_iteration(interp, outer_scope, inner_scope) &&
                           boundary_state_matches_snapshot(interp, iter_saved);
            if (!iter_ok) {
                ok = false;
                failed_iter = i;
            }
        }

        boundary_restore_interp_state(interp, saved);
        main::scope_release(scope_b);
        main::scope_release(scope_a);

        if (ok && boundary_state_matches_snapshot(interp, saved)) {
            emit_pass_literal("[PASS] lifetime: boundary session nested stress");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: boundary session nested stress (failed_iter=%d state_ok=%s)",
                (long)failed_iter,
                boundary_state_matches_snapshot(interp, saved) ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn bool boundary_mixed_event_operation(
    Interp* interp,
    Value* src,
    main::ScopeRegion* parent,
    main::ScopeRegion* child,
    usz case_id
) {
    switch (case_id) {
        case 0: {
            EvalResult parse_err = run_program(
                "(define __boundary_mix_parse 1\n"
                "(+ __boundary_mix_parse 2)",
                interp
            );
            return parse_err.error.has_error;
        }
        case 1: {
            EvalResult eval_err = run("(car 1)", interp);
            return eval_err.error.has_error || (eval_err.value != null && eval_err.value.tag == ERROR);
        }
        case 2: {
            Value* copied = test_copy_from_releasing_scope(
                src, interp, child, COPY_SITE_RUN_JIT_RESULT);
            return copied != null &&
                   copied != src &&
                   main::is_in_scope((void*)copied, parent) &&
                   !main::is_in_scope((void*)copied, child);
        }
        case 3: {
            BoundaryDecisionStats saved_boundary_stats = boundary_decision_stats_snapshot();
            defer boundary_restore_decision_stats(saved_boundary_stats);
            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, 0);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                interp,
                parent,
                child,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            return committed.value != null &&
                   committed.outcome == BOUNDARY_COMMIT_FALLBACK_DISALLOWED &&
                   committed.fault_code == BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED &&
                   !committed.releasing_scope_spliced &&
                   !committed.reused_in_target_chain &&
                   ctx.aborted &&
                   committed.value.tag == ERROR &&
                   main::is_in_scope((void*)committed.value, parent) &&
                   !main::is_in_scope((void*)committed.value, child);
        }
        default:
            return false;
    }
}

fn void run_memory_lifetime_boundary_restore_mixed_event_order_stress_test(Interp* interp, int* pass, int* fail) {
    // Invariant: mixed success/failure event orderings restore boundary state on every iteration.
    {
        BoundaryInterpState saved = boundary_save_interp_state(interp);
        main::ScopeRegion* parent = main::scope_create(saved.current_scope);
        main::ScopeRegion* child = parent == null ? null : main::scope_create(parent);
        if (parent == null || child == null) {
            if (child != null) main::scope_release(child);
            if (parent != null) main::scope_release(parent);
            io::printn("[FAIL] lifetime: boundary restore mixed event-order stress");
            (*fail)++;
            return;
        }

        bool ok = true;
        usz failed_iter = usz.max;
        usz failed_case = usz.max;
        for (usz i = 0; i < 160 && ok; i++) {
            interp.current_scope = child;
            Value* src = make_cons(interp, make_int(interp, (long)(1000 + i)), make_nil(interp));
            if (src == null) {
                ok = false;
                failed_iter = i;
                failed_case = 99;
                break;
            }

            interp.current_scope = parent;
            interp.releasing_scope = child;
            BoundaryInterpState iter_saved = boundary_save_interp_state(interp);
            usz case_id = (i * 5 + 1) & 3;

            bool op_ok = boundary_mixed_event_operation(interp, src, parent, child, case_id);
            bool state_ok = boundary_state_matches_snapshot(interp, iter_saved) &&
                            interp.active_promotion_ctx == null;
            if (!op_ok || !state_ok) {
                ok = false;
                failed_iter = i;
                failed_case = case_id;
            }
        }

        boundary_restore_interp_state(interp, saved);
        main::scope_release(child);
        main::scope_release(parent);

        if (ok && boundary_state_matches_snapshot(interp, saved)) {
            emit_pass_literal("[PASS] lifetime: boundary restore mixed event-order stress");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: boundary restore mixed event-order stress (failed_iter=%d failed_case=%d state_ok=%s)",
                (long)failed_iter,
                (long)failed_case,
                boundary_state_matches_snapshot(interp, saved) ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn BoundaryScopeTransferResult boundary_splice_fuzz_case(
    main::ScopeRegion* parent,
    main::ScopeRegion* child,
    main::ScopeRegion* sibling,
    main::ScopeRegion* foreign,
    usz case_id
) {
    switch (case_id) {
        case 0:
            return boundary_try_scope_splice_escapes_result(null, child);
        case 1:
            return boundary_try_scope_splice_escapes_result(parent, null);
        case 2:
            return boundary_try_scope_splice_escapes_result(parent, parent);
        case 3:
            return boundary_try_scope_splice_escapes_result(sibling, child);
        case 4:
            return boundary_try_scope_splice_escapes_result(foreign, child);
        case 5: {
            main::scope_retain(child);
            BoundaryScopeTransferResult r = boundary_try_scope_splice_escapes_result(parent, child);
            main::scope_release(child);
            return r;
        }
        case 6: {
            void* saved_owner = child.owner_thread_token;
            child.owner_thread_token = null;
            BoundaryScopeTransferResult r = boundary_try_scope_splice_escapes_result(parent, child);
            child.owner_thread_token = saved_owner;
            return r;
        }
        case 7: {
            char* saved_escape_bump = parent.escape_bump;
            char* saved_escape_limit = parent.escape_limit;
            main::ScopeChunk* saved_escape_chunks = parent.escape_chunks;
            parent.escape_bump = null;
            parent.escape_limit = null;
            parent.escape_chunks = parent.chunks;
            BoundaryScopeTransferResult r = boundary_try_scope_splice_escapes_result(parent, child);
            parent.escape_bump = saved_escape_bump;
            parent.escape_limit = saved_escape_limit;
            parent.escape_chunks = saved_escape_chunks;
            return r;
        }
        case 8: {
            char* saved_bump = child.bump;
            child.bump = null;
            BoundaryScopeTransferResult r = boundary_try_scope_splice_escapes_result(parent, child);
            child.bump = saved_bump;
            return r;
        }
        case 9: {
            char* saved_escape_bump = child.escape_bump;
            char* saved_escape_limit = child.escape_limit;
            main::ScopeChunk* saved_escape_chunks = child.escape_chunks;
            child.escape_bump = null;
            child.escape_limit = null;
            child.escape_chunks = child.chunks;
            BoundaryScopeTransferResult r = boundary_try_scope_splice_escapes_result(parent, child);
            child.escape_bump = saved_escape_bump;
            child.escape_limit = saved_escape_limit;
            child.escape_chunks = saved_escape_chunks;
            return r;
        }
        default:
            return boundary_try_scope_splice_escapes_result(parent, child);
    }
}

fn BoundaryScopeTransferReason boundary_splice_fuzz_expected_reason(usz case_id) @inline {
    switch (case_id) {
        case 0:
            return BOUNDARY_SCOPE_TRANSFER_NULL_PARENT;
        case 1:
            return BOUNDARY_SCOPE_TRANSFER_NULL_CHILD;
        case 2:
            return BOUNDARY_SCOPE_TRANSFER_SAME_SCOPE;
        case 3:
            return BOUNDARY_SCOPE_TRANSFER_NOT_IMMEDIATE_CHILD;
        case 4:
            return BOUNDARY_SCOPE_TRANSFER_NOT_IMMEDIATE_CHILD;
        case 5:
            return BOUNDARY_SCOPE_TRANSFER_CHILD_REFCOUNT_NOT_ONE;
        case 6:
            return BOUNDARY_SCOPE_TRANSFER_OWNER_THREAD_MISMATCH;
        case 7:
            return BOUNDARY_SCOPE_TRANSFER_PARENT_ESCAPE_LANE_INVALID;
        case 8:
            return BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID;
        case 9:
            return BOUNDARY_SCOPE_TRANSFER_CHILD_ESCAPE_LANE_INVALID;
        default:
            return BOUNDARY_SCOPE_TRANSFER_OK;
    }
}

fn void run_memory_lifetime_splice_legality_fuzz_test(Interp* interp, int* pass, int* fail) {
    // Invariant: splice legality guards return stable reason codes across mixed invalid inputs.
    {
        BoundaryInterpState saved = boundary_save_interp_state(interp);
        main::ScopeRegion* parent = main::scope_create(saved.current_scope);
        main::ScopeRegion* child = parent == null ? null : main::scope_create(parent);
        main::ScopeRegion* sibling = parent == null ? null : main::scope_create(parent);
        main::ScopeRegion* foreign = main::scope_create(saved.current_scope);
        if (parent == null || child == null || sibling == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (sibling != null) main::scope_release(sibling);
            if (child != null) main::scope_release(child);
            if (parent != null) main::scope_release(parent);
            io::printn("[FAIL] lifetime: splice legality fuzz matrix");
            (*fail)++;
            return;
        }

        bool ok = true;
        usz failed_iter = usz.max;
        BoundaryScopeTransferReason got_reason = BOUNDARY_SCOPE_TRANSFER_OK;
        BoundaryScopeTransferReason want_reason = BOUNDARY_SCOPE_TRANSFER_OK;
        for (usz i = 0; i < 220 && ok; i++) {
            usz case_id = i % 10;
            BoundaryScopeTransferResult r = boundary_splice_fuzz_case(
                parent, child, sibling, foreign, case_id);
            want_reason = boundary_splice_fuzz_expected_reason(case_id);
            got_reason = r.reason;
            bool should_allow = want_reason == BOUNDARY_SCOPE_TRANSFER_OK;
            bool reason_ok = r.reason == want_reason && r.allowed == should_allow;
            bool topology_ok = child.parent == parent &&
                               sibling.parent == parent &&
                               foreign.parent == saved.current_scope;
            if (!reason_ok || !topology_ok) {
                ok = false;
                failed_iter = i;
            }
        }

        boundary_restore_interp_state(interp, saved);
        main::scope_release(foreign);
        main::scope_release(sibling);
        main::scope_release(child);
        main::scope_release(parent);

        if (ok && boundary_state_matches_snapshot(interp, saved)) {
            emit_pass_literal("[PASS] lifetime: splice legality fuzz matrix");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: splice legality fuzz matrix (failed_iter=%d got=%s want=%s state_ok=%s)",
                (long)failed_iter,
                boundary_scope_transfer_reason_name(got_reason),
                boundary_scope_transfer_reason_name(want_reason),
                boundary_state_matches_snapshot(interp, saved) ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_boundary_mixed_jit_interp_stress_test(Interp* interp, int* pass, int* fail) {
    // Invariant: alternating interpreter and JIT execution preserves boundary state across stress iterations.
    {
        BoundaryInterpState saved = boundary_save_interp_state(interp);
        char[] expr = "(+ 20 22)";

        JitCompiledFn compiled = jit_compiled_fn_null();
        bool jit_ready = true;
        if (jit_checks_enabled()) {
            Expr* ast = parse_for_jit(expr, interp);
            if (ast == null) {
                jit_ready = false;
            } else {
                compiled = jit_compile(ast, interp);
                jit_ready = !jit_compiled_fn_is_null(compiled);
            }
        }

        bool ok = jit_ready;
        usz failed_iter = usz.max;
        for (usz i = 0; i < 144 && ok; i++) {
            BoundaryInterpState iter_saved = boundary_save_interp_state(interp);

            EvalResult ri = run(expr, interp);
            bool interp_ok = !ri.error.has_error &&
                             ri.value != null &&
                             ri.value.tag == INT &&
                             ri.value.int_val == 42;

            bool jit_ok = true;
            if (jit_checks_enabled()) {
                Value* jv = jit_exec(compiled, interp);
                jit_ok = jv != null && jv.tag == INT && jv.int_val == 42;
            }

            bool state_ok = boundary_state_matches_snapshot(interp, iter_saved) &&
                            interp.active_promotion_ctx == null;
            if (!interp_ok || !jit_ok || !state_ok) {
                ok = false;
                failed_iter = i;
            }
        }

        boundary_restore_interp_state(interp, saved);
        bool final_state_ok = boundary_state_matches_snapshot(interp, saved);
        if (ok && final_state_ok) {
            emit_pass_literal("[PASS] lifetime: boundary mixed jit/interp stress");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary mixed jit/interp stress (jit_ready=%s failed_iter=%d final_state_ok=%s)",
                jit_ready ? "yes" : "no",
                (long)failed_iter,
                final_state_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_boundary_negative_stress_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_boundary_session_nested_stress_test(interp, pass, fail);
    run_memory_lifetime_boundary_restore_mixed_event_order_stress_test(interp, pass, fail);
    run_memory_lifetime_splice_legality_fuzz_test(interp, pass, fail);
    run_memory_lifetime_boundary_mixed_jit_interp_stress_test(interp, pass, fail);
}
