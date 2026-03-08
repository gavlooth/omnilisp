module lisp;

import std::io;
import main;

fn void run_memory_lifetime_boundary_scope_restore_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: boundary API helpers restore current/releasing scope state after temporary overrides.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* scope_a = main::scope_create(saved);
        main::ScopeRegion* scope_b = main::scope_create(saved);
        main::ScopeRegion* scope_c = main::scope_create(saved);
        main::ScopeRegion* scope_d = main::scope_create(saved);
        if (scope_a == null || scope_b == null || scope_c == null || scope_d == null) {
            if (scope_d != null) main::scope_release(scope_d);
            if (scope_c != null) main::scope_release(scope_c);
            if (scope_b != null) main::scope_release(scope_b);
            if (scope_a != null) main::scope_release(scope_a);
            io::printn("[FAIL] lifetime: boundary state restore helpers");
            (*fail)++;
            return;
        }

        interp.current_scope = scope_a;
        interp.releasing_scope = scope_b;

        Value* noop = boundary_copy_to_scope_site(
            null, interp, scope_c, scope_d, COPY_SITE_COPY_ENV_TO_SCOPE);
        bool copy_scope_ok = noop == null &&
                             interp.current_scope == scope_a &&
                             interp.releasing_scope == scope_b;

        Value* local_val = make_int(interp, 42);
        Value* copied = boundary_copy_from_releasing_scope(
            local_val, interp, scope_d, COPY_SITE_RUN_JIT_RESULT);
        bool copy_release_ok = copied != null &&
                               copied.tag == INT &&
                               copied.int_val == 42 &&
                               interp.current_scope == scope_a &&
                               interp.releasing_scope == scope_b;

        interp.current_scope = saved;
        interp.releasing_scope = null;
        main::scope_release(scope_d);
        main::scope_release(scope_c);
        main::scope_release(scope_b);
        main::scope_release(scope_a);

        bool ok = copy_scope_ok && copy_release_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary state restore helpers");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: boundary state restore helpers (copy_scope_ok=%s copy_release_ok=%s)",
                copy_scope_ok ? "yes" : "no",
                copy_release_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_boundary_scope_interleaving_test(Interp* interp, int* pass, int* fail) {
    // Invariant: nested boundary enter/leave + child push/pop restores state exactly.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* scope_a = main::scope_create(saved);
        main::ScopeRegion* scope_b = main::scope_create(saved);
        main::ScopeRegion* scope_c = main::scope_create(saved);
        if (scope_a == null || scope_b == null || scope_c == null) {
            if (scope_c != null) main::scope_release(scope_c);
            if (scope_b != null) main::scope_release(scope_b);
            if (scope_a != null) main::scope_release(scope_a);
            io::printn("[FAIL] lifetime: boundary interleaving restores state");
            (*fail)++;
            return;
        }

        interp.current_scope = scope_a;
        interp.releasing_scope = scope_b;
        BoundaryInterpState state0 = boundary_save_interp_state(interp);

        main::ScopeRegion* enter_saved = boundary_enter_scope(interp, scope_c);
        main::ScopeRegion* push_saved = null;
        main::ScopeRegion* child = boundary_push_child_scope(interp, &push_saved);

        bool during_ok = child != null &&
                         enter_saved == scope_a &&
                         push_saved == scope_c &&
                         interp.current_scope == child &&
                         interp.releasing_scope == scope_b;

        if (child != null) {
            boundary_pop_child_scope(interp, push_saved, child);
        }
        boundary_leave_scope(interp, enter_saved);

        bool restore_ok = interp.current_scope == scope_a &&
                          interp.releasing_scope == scope_b;

        // After interleaving, releasing-scope copy path still works and leaves state unchanged.
        main::ScopeRegion* src_saved = boundary_enter_scope(interp, scope_b);
        Value* src = make_int(interp, 77);
        boundary_leave_scope(interp, src_saved);

        Value* copied = boundary_copy_from_releasing_scope(
            src, interp, scope_b, COPY_SITE_RUN_JIT_RESULT);
        bool copy_ok = copied != null &&
                       copied.tag == INT &&
                       copied.int_val == 77 &&
                       copied != src &&
                       !main::is_in_scope((void*)copied, scope_b) &&
                       interp.current_scope == scope_a &&
                       interp.releasing_scope == scope_b;

        boundary_restore_interp_state(interp, state0);
        bool final_ok = interp.current_scope == scope_a &&
                        interp.releasing_scope == scope_b;

        interp.current_scope = saved;
        interp.releasing_scope = null;
        main::scope_release(scope_c);
        main::scope_release(scope_b);
        main::scope_release(scope_a);

        bool ok = during_ok && restore_ok && copy_ok && final_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary interleaving restores state");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: boundary interleaving restores state (during_ok=%s restore_ok=%s copy_ok=%s final_ok=%s)",
                during_ok ? "yes" : "no",
                restore_ok ? "yes" : "no",
                copy_ok ? "yes" : "no",
                final_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_run_program_boundary_state_test(Interp* interp, int* pass, int* fail) {
    // Invariant: run_program preserves boundary scope state across multi-form execution.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        EvalResult r = run_program(
            "(define __boundary_prog_x 10)\n"
            "(define __boundary_prog_y 32)\n"
            "(+ __boundary_prog_x __boundary_prog_y)",
            interp);

        bool ok_eval = !r.error.has_error &&
                       r.value != null &&
                       r.value.tag == INT &&
                       r.value.int_val == 42;
        bool ok_state = interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;
        bool ok = ok_eval && ok_state;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: run_program boundary state restore");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: run_program boundary state restore (ok_eval=%s ok_state=%s scope_match=%s releasing_match=%s)",
                ok_eval ? "yes" : "no",
                ok_state ? "yes" : "no",
                interp.current_scope == saved_scope ? "yes" : "no",
                interp.releasing_scope == saved_releasing ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_run_program_error_boundary_state_test(Interp* interp, int* pass, int* fail) {
    // Invariant: run_program preserves boundary scope state on parse/runtime error exits.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        EvalResult parse_err = run_program(
            "(define __boundary_prog_err 1\n"
            "(+ __boundary_prog_err 2)",
            interp);
        bool parse_ok = parse_err.error.has_error &&
                        interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;

        EvalResult eval_err = run_program(
            "(define __boundary_prog_eval_err 1)\n"
            "(car 1)",
            interp);
        bool eval_ok = eval_err.error.has_error &&
                       interp.current_scope == saved_scope &&
                       interp.releasing_scope == saved_releasing;

        bool ok = parse_ok && eval_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: run_program error boundary state restore");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: run_program error boundary state restore (parse_ok=%s eval_ok=%s)",
                parse_ok ? "yes" : "no",
                eval_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: wrapper values in target chain are reused, while disjoint wrappers are defensively copied.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || releasing == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: wrapper reuse vs defensive copy");
            (*fail)++;
            return;
        }

        interp.current_scope = target;
        void* h_reuse = mem::malloc(8);
        Value* reusable = make_ffi_handle_ex(interp, h_reuse, "__reuse", null, true);

        interp.current_scope = foreign;
        void* h_foreign = mem::malloc(8);
        Value* disjoint = make_ffi_handle_ex(interp, h_foreign, "__disjoint", null, true);

        interp.current_scope = target;
        interp.releasing_scope = releasing;

        Value* reused = copy_to_parent(reusable, interp);
        Value* copied = copy_to_parent(disjoint, interp);

        bool reuse_ok = reused == reusable;
        bool copied_ok = copied != null &&
                         copied.tag == FFI_HANDLE &&
                         copied != disjoint &&
                         copied.ffi_val != null &&
                         copied.ffi_val == disjoint.ffi_val &&
                         copied.ffi_val.refcount == 2;

        main::scope_release(foreign);

        bool post_ok = copied != null &&
                       copied.tag == FFI_HANDLE &&
                       copied.ffi_val != null &&
                       copied.ffi_val.refcount == 1;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(releasing);
        main::scope_release(target);

        bool ok = reusable != null && disjoint != null && reuse_ok && copied_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: wrapper reuse vs defensive copy");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: wrapper reuse vs defensive copy (reusable=%s disjoint=%s reuse_ok=%s copied_ok=%s post_ok=%s)",
                reusable != null ? "yes" : "no",
                disjoint != null ? "yes" : "no",
                reuse_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }

    // Invariant: splice fast-path rejects sibling/foreign/retained-child transfers with explicit reasons.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved);
        main::ScopeRegion* child = main::scope_create(parent);
        main::ScopeRegion* sibling = main::scope_create(parent);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (parent == null || child == null || sibling == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (sibling != null) main::scope_release(sibling);
            if (child != null) main::scope_release(child);
            if (parent != null) main::scope_release(parent);
            io::printn("[FAIL] lifetime: splice legality explicit reasons");
            (*fail)++;
            return;
        }

        interp.current_scope = child;
        void* escaped = child.alloc_escape(16);
        interp.current_scope = saved;

        BoundaryScopeTransferResult sibling_result = boundary_try_scope_splice_escapes_result(sibling, child);
        BoundaryScopeTransferResult foreign_result = boundary_try_scope_splice_escapes_result(foreign, child);

        void* saved_child_owner = child.owner_thread_token;
        child.owner_thread_token = null;
        BoundaryScopeTransferResult owner_result = boundary_try_scope_splice_escapes_result(parent, child);
        child.owner_thread_token = saved_child_owner;

        main::scope_retain(child);
        BoundaryScopeTransferResult retained_result = boundary_try_scope_splice_escapes_result(parent, child);

        bool sibling_ok = !sibling_result.allowed &&
                          sibling_result.reason == BOUNDARY_SCOPE_TRANSFER_NOT_IMMEDIATE_CHILD;
        bool foreign_ok = !foreign_result.allowed &&
                          foreign_result.reason == BOUNDARY_SCOPE_TRANSFER_NOT_IMMEDIATE_CHILD;
        bool owner_ok = !owner_result.allowed &&
                        owner_result.reason == BOUNDARY_SCOPE_TRANSFER_OWNER_THREAD_MISMATCH;
        bool retained_ok = !retained_result.allowed &&
                           retained_result.reason == BOUNDARY_SCOPE_TRANSFER_CHILD_REFCOUNT_NOT_ONE;
        bool state_ok = escaped != null &&
                        child.parent == parent &&
                        child.refcount == 2 &&
                        main::is_in_scope(escaped, child) &&
                        !main::is_in_scope(escaped, parent) &&
                        !main::is_in_scope(escaped, sibling) &&
                        !main::is_in_scope(escaped, foreign);

        main::scope_release(child); // Drop extra retain before normal teardown.
        main::scope_release(sibling);
        main::scope_release(child);
        main::scope_release(foreign);
        main::scope_release(parent);

        bool ok = sibling_ok && foreign_ok && owner_ok && retained_ok && state_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: splice legality explicit reasons");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: splice legality explicit reasons (sibling=%s foreign=%s owner=%s retained=%s state_ok=%s reasons=%s/%s/%s/%s)",
                sibling_ok ? "yes" : "no",
                foreign_ok ? "yes" : "no",
                owner_ok ? "yes" : "no",
                retained_ok ? "yes" : "no",
                state_ok ? "yes" : "no",
                boundary_scope_transfer_reason_name(sibling_result.reason),
                boundary_scope_transfer_reason_name(foreign_result.reason),
                boundary_scope_transfer_reason_name(owner_result.reason),
                boundary_scope_transfer_reason_name(retained_result.reason)
            );
            (*fail)++;
        }
    }
}

fn Value* boundary_commit_make_temp_int_list(Interp* interp, usz len) {
    Value* acc = make_nil(interp);
    for (usz i = 0; i < len; i++) {
        acc = make_cons(interp, make_int(interp, (long)(i + 1)), acc);
    }
    return acc;
}

fn bool boundary_commit_cons_head_matches(Value* value, long expected_head) @inline {
    return value != null &&
           value.tag == CONS &&
           value.cons_val.car != null &&
           value.cons_val.car.tag == INT &&
           value.cons_val.car.int_val == expected_head;
}

fn void run_memory_lifetime_boundary_return_classification_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: boundary return classification provides one explicit provenance class.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        main::ScopeRegion* foreign_scope = main::scope_create(saved_scope);
        if (target_scope == null || releasing_scope == null || foreign_scope == null) {
            if (foreign_scope != null) main::scope_release(foreign_scope);
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary return classification matrix");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* reusable = make_int(interp, 101);

            interp.current_scope = releasing_scope;
            Value* temp_owned = make_int(interp, 202);
            Value* escape_seed = make_int(interp, 303);
            Value* escape_owned = promote_to_escape(escape_seed, interp);

            interp.current_scope = foreign_scope;
            Value* mixed_uncertain = make_int(interp, 404);

            interp.current_scope = target_scope;
            interp.releasing_scope = releasing_scope;

            BoundaryReturnClassification reusable_class =
                boundary_classify_return_value(reusable, interp, target_scope, releasing_scope);
            BoundaryReturnClassification temp_class =
                boundary_classify_return_value(temp_owned, interp, target_scope, releasing_scope);
            BoundaryReturnClassification escape_class =
                boundary_classify_return_value(escape_owned, interp, target_scope, releasing_scope);
            BoundaryReturnClassification mixed_class =
                boundary_classify_return_value(mixed_uncertain, interp, target_scope, releasing_scope);

            bool reusable_ok =
                reusable_class.provenance == BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN &&
                reusable_class.in_target_scope_chain &&
                !reusable_class.in_releasing_scope;
            bool temp_ok =
                temp_class.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP &&
                !temp_class.in_target_scope_chain &&
                temp_class.in_releasing_scope;
            bool escape_ok =
                escape_class.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE &&
                !escape_class.in_target_scope_chain &&
                escape_class.in_releasing_scope;
            bool mixed_ok =
                mixed_class.provenance == BOUNDARY_RETURN_MIXED_UNCERTAIN &&
                !mixed_class.in_target_scope_chain &&
                !mixed_class.in_releasing_scope;

            interp.current_scope = saved_scope;
            interp.releasing_scope = null;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
            main::scope_release(foreign_scope);

            bool ok = reusable_ok && temp_ok && escape_ok && mixed_ok;
            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary return classification matrix");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: boundary return classification matrix (reusable=%s temp=%s escape=%s mixed=%s tags=%s/%s/%s/%s)",
                    reusable_ok ? "yes" : "no",
                    temp_ok ? "yes" : "no",
                    escape_ok ? "yes" : "no",
                    mixed_ok ? "yes" : "no",
                    boundary_return_provenance_name(reusable_class.provenance),
                    boundary_return_provenance_name(temp_class.provenance),
                    boundary_return_provenance_name(escape_class.provenance),
                    boundary_return_provenance_name(mixed_class.provenance)
                );
                (*fail)++;
            }
        }
    }
}

fn void run_memory_lifetime_boundary_commit_escape_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: TEMP cons survivor commits through destination-aware ESCAPE build.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape TEMP survivor commit");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = make_cons(interp, make_int(interp, 17), make_nil(interp));
            usz base_site = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                src,
                interp,
                target_scope,
                releasing_scope,
                COPY_SITE_RUN_JIT_RESULT,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            usz site_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_site;

            bool ok = !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      !committed.fallback_copied &&
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_CONS_BUILT &&
                      site_delta == 0 &&
                      boundary_commit_cons_head_matches(committed.value, 17) &&
                      committed.value != src &&
                      main::is_in_scope((void*)committed.value, target_scope);

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape TEMP survivor commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP survivor commit (outcome=%d spliced=%s reused=%s fallback=%s site_delta=%d same=%s)",
                    (int)committed.outcome,
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    committed.fallback_copied ? "yes" : "no",
                    (long)site_delta,
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: values already reusable in target chain are returned unchanged.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape already-safe reuse");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* safe = make_cons(interp, make_int(interp, 55), make_nil(interp));
            usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_SINGLE_SCOPE_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                safe,
                safe,
                interp,
                target_scope,
                releasing_scope,
                COPY_SITE_JIT_SINGLE_SCOPE_RETURN
            );
            usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_SINGLE_SCOPE_RETURN) - base_site;

            bool ok = !committed.releasing_scope_spliced &&
                      committed.reused_in_target_chain &&
                      !committed.fallback_copied &&
                      committed.value == safe &&
                      site_delta == 0;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape already-safe reuse");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape already-safe reuse (spliced=%s reused=%s fallback=%s site_delta=%d same=%s)",
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    committed.fallback_copied ? "yes" : "no",
                    (long)site_delta,
                    committed.value == safe ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: TEMP partial-primitive survivor commits through destination-aware ESCAPE build.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape TEMP partial survivor commit");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = interp.alloc_value();
            src.tag = PARTIAL_PRIM;
            src.partial_val.func = null;
            src.partial_val.remaining = 1;
            src.partial_val.first_arg = make_int(interp, 71);
            src.partial_val.second_arg = make_int(interp, 72);

            usz base_generic = copy_to_parent_site_calls(COPY_SITE_GENERIC);
            usz base_commit = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                src,
                interp,
                target_scope,
                releasing_scope,
                COPY_SITE_RUN_JIT_RESULT,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            usz generic_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_generic;
            usz commit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_commit;

            bool ok = !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      !committed.fallback_copied &&
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_PARTIAL_BUILT &&
                      committed.value != src &&
                      committed.value != null &&
                      committed.value.tag == PARTIAL_PRIM &&
                      committed.value.partial_val.remaining == src.partial_val.remaining &&
                      committed.value.partial_val.first_arg != null &&
                      committed.value.partial_val.second_arg != null &&
                      committed.value.partial_val.first_arg.scope_gen == target_scope.escape_generation &&
                      committed.value.partial_val.second_arg.scope_gen == target_scope.escape_generation &&
                      generic_delta >= 2 &&
                      commit_delta == 0 &&
                      main::is_in_scope((void*)committed.value, target_scope);

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape TEMP partial survivor commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP partial survivor commit (outcome=%d spliced=%s reused=%s fallback=%s generic_delta=%d commit_delta=%d same=%s)",
                    (int)committed.outcome,
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    committed.fallback_copied ? "yes" : "no",
                    (long)generic_delta,
                    (long)commit_delta,
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: TEMP iterator survivor commits through destination-aware ESCAPE build.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape TEMP iterator survivor commit");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = interp.alloc_value();
            src.tag = ITERATOR;
            src.iterator_val = make_int(interp, 181);

            usz base_generic = copy_to_parent_site_calls(COPY_SITE_GENERIC);
            usz base_commit = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                src,
                interp,
                target_scope,
                releasing_scope,
                COPY_SITE_RUN_JIT_RESULT,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            usz generic_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_generic;
            usz commit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_commit;

            bool ok = !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      !committed.fallback_copied &&
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_ITERATOR_BUILT &&
                      committed.value != src &&
                      committed.value != null &&
                      committed.value.tag == ITERATOR &&
                      committed.value.iterator_val != null &&
                      committed.value.iterator_val.scope_gen == target_scope.escape_generation &&
                      generic_delta >= 1 &&
                      commit_delta == 0 &&
                      main::is_in_scope((void*)committed.value, target_scope);

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape TEMP iterator survivor commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP iterator survivor commit (outcome=%d spliced=%s reused=%s fallback=%s generic_delta=%d commit_delta=%d same=%s)",
                    (int)committed.outcome,
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    committed.fallback_copied ? "yes" : "no",
                    (long)generic_delta,
                    (long)commit_delta,
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: TEMP scalar survivor commits through direct destination ESCAPE promotion.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape TEMP scalar direct destination commit");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = make_int(interp, 777);

            usz base_generic = copy_to_parent_site_calls(COPY_SITE_GENERIC);
            usz base_commit = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                src,
                interp,
                target_scope,
                releasing_scope,
                COPY_SITE_RUN_JIT_RESULT,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            usz generic_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_generic;
            usz commit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_commit;

            bool ok = !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      !committed.fallback_copied &&
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED &&
                      committed.value != src &&
                      committed.value != null &&
                      committed.value.tag == INT &&
                      committed.value.int_val == 777 &&
                      committed.value.scope_gen == target_scope.escape_generation &&
                      generic_delta >= 1 &&
                      commit_delta == 0 &&
                      main::is_in_scope((void*)committed.value, target_scope);

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape TEMP scalar direct destination commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP scalar direct destination commit (outcome=%d spliced=%s reused=%s fallback=%s generic_delta=%d commit_delta=%d same=%s)",
                    (int)committed.outcome,
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    committed.fallback_copied ? "yes" : "no",
                    (long)generic_delta,
                    (long)commit_delta,
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: mixed/uncertain provenance can commit through destination ESCAPE
    // promotion before falling back to deep boundary copy.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        main::ScopeRegion* disjoint_scope = main::scope_create(root_scope);
        if (target_scope == null || releasing_scope == null || disjoint_scope == null) {
            if (disjoint_scope != null) main::scope_release(disjoint_scope);
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape mixed uncertain destination commit");
            (*fail)++;
        } else {
            interp.current_scope = disjoint_scope;
            Value* src = make_int(interp, 909);

            usz base_generic = copy_to_parent_site_calls(COPY_SITE_GENERIC);
            usz base_commit = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                src,
                interp,
                target_scope,
                releasing_scope,
                COPY_SITE_RUN_JIT_RESULT,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            usz generic_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_generic;
            usz commit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_commit;

            bool ok = !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      !committed.fallback_copied &&
                      committed.outcome == BOUNDARY_COMMIT_MIXED_DESTINATION_PROMOTED &&
                      committed.value != src &&
                      committed.value != null &&
                      committed.value.tag == INT &&
                      committed.value.int_val == 909 &&
                      committed.value.scope_gen == target_scope.escape_generation &&
                      generic_delta >= 1 &&
                      commit_delta == 0 &&
                      main::is_in_scope((void*)committed.value, target_scope) &&
                      !main::is_in_scope((void*)committed.value, disjoint_scope);

            interp.current_scope = root_scope;
            main::scope_release(disjoint_scope);
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape mixed uncertain destination commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape mixed uncertain destination commit (outcome=%d spliced=%s reused=%s fallback=%s generic_delta=%d commit_delta=%d same=%s)",
                    (int)committed.outcome,
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    committed.fallback_copied ? "yes" : "no",
                    (long)generic_delta,
                    (long)commit_delta,
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: aborted promotion contexts trigger copy fallback with caller site accounting.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape fallback copy path");
            (*fail)++;
        } else {
            usz list_len = PROMOTION_CTX_BUDGET_RETURN + 64;
            interp.current_scope = releasing_scope;
            Value* src = boundary_commit_make_temp_int_list(interp, list_len);

            BoundaryDecisionStats saved_boundary_stats = boundary_decision_stats_snapshot();
            defer boundary_restore_decision_stats(saved_boundary_stats);
            usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_SINGLE_SCOPE_RETURN);
            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, 0);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                src,
                interp,
                target_scope,
                releasing_scope,
                COPY_SITE_JIT_SINGLE_SCOPE_RETURN,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_SINGLE_SCOPE_RETURN) - base_site;

            bool ok = !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      committed.fallback_copied &&
                      committed.value != src &&
                      boundary_commit_cons_head_matches(committed.value, (long)list_len) &&
                      site_delta >= 1 &&
                      main::is_in_scope((void*)committed.value, target_scope) &&
                      !main::is_in_scope((void*)committed.value, releasing_scope);

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape fallback copy path");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape fallback copy path (spliced=%s reused=%s fallback=%s site_delta=%d same=%s)",
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    committed.fallback_copied ? "yes" : "no",
                    (long)site_delta,
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }
}

fn void run_memory_lifetime_boundary_graph_audit_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: graph audit accepts an ESCAPE-root graph with no TEMP-reachable edges.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        if (target_scope == null) {
            io::printn("[FAIL] lifetime: boundary graph audit accepts escape graph");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* temp_list = boundary_commit_make_temp_int_list(interp, 8);
            Value* escape_root = promote_to_escape(temp_list, interp);

            bool ok = escape_root != null &&
                      escape_root.scope_gen == target_scope.escape_generation &&
                      boundary_graph_audit_escape_reachability(escape_root, target_scope);

            interp.current_scope = saved_scope;
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary graph audit accepts escape graph");
                (*pass)++;
            } else {
                io::printn("[FAIL] lifetime: boundary graph audit accepts escape graph");
                (*fail)++;
            }
        }
    }

    // Invariant: graph audit rejects an ESCAPE-root graph that reaches TEMP-lane values.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        if (target_scope == null) {
            io::printn("[FAIL] lifetime: boundary graph audit rejects temp edge");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* temp_leaf = make_int(interp, 901);
            Value* escape_nil = interp.alloc_value_escape();
            Value* escape_root = interp.alloc_value_escape();

            if (escape_nil != null) escape_nil.tag = NIL;
            if (escape_root != null) {
                escape_root.tag = CONS;
                escape_root.cons_val.car = temp_leaf;
                escape_root.cons_val.cdr = escape_nil;
            }

            bool ok = escape_root != null &&
                      temp_leaf != null &&
                      escape_root.scope_gen == target_scope.escape_generation &&
                      temp_leaf.scope_gen == target_scope.generation &&
                      !boundary_graph_audit_escape_reachability(escape_root, target_scope);

            interp.current_scope = saved_scope;
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary graph audit rejects temp edge");
                (*pass)++;
            } else {
                io::printn("[FAIL] lifetime: boundary graph audit rejects temp edge");
                (*fail)++;
            }
        }
    }

    // Invariant: opaque wrappers are excluded from Omni-owned edge traversal.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        if (target_scope == null) {
            io::printn("[FAIL] lifetime: boundary graph audit opaque wrapper exclusion");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* opaque = interp.alloc_value_escape();
            if (opaque != null) {
                opaque.tag = FFI_HANDLE;
                opaque.ffi_val = null;
            }

            bool ok = opaque != null &&
                      opaque.scope_gen == target_scope.escape_generation &&
                      boundary_graph_audit_escape_reachability(opaque, target_scope);

            interp.current_scope = saved_scope;
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary graph audit opaque wrapper exclusion");
                (*pass)++;
            } else {
                io::printn("[FAIL] lifetime: boundary graph audit opaque wrapper exclusion");
                (*fail)++;
            }
        }
    }
}

fn void run_memory_lifetime_boundary_txn_protocol_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: transition policy rejects protocol misuse states.
    {
        bool matrix_ok =
            !boundary_txn_transition_allowed(BOUNDARY_TXN_INIT, BOUNDARY_TXN_ACTION_COMMIT) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_INIT, BOUNDARY_TXN_ACTION_ABORT) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_INIT, BOUNDARY_TXN_ACTION_CLOSE) &&
            boundary_txn_transition_allowed(BOUNDARY_TXN_OPEN, BOUNDARY_TXN_ACTION_COMMIT) &&
            boundary_txn_transition_allowed(BOUNDARY_TXN_OPEN, BOUNDARY_TXN_ACTION_ABORT) &&
            boundary_txn_transition_allowed(BOUNDARY_TXN_OPEN, BOUNDARY_TXN_ACTION_CLOSE) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_COMMITTED, BOUNDARY_TXN_ACTION_COMMIT) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_COMMITTED, BOUNDARY_TXN_ACTION_ABORT) &&
            boundary_txn_transition_allowed(BOUNDARY_TXN_COMMITTED, BOUNDARY_TXN_ACTION_CLOSE) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_ABORTED, BOUNDARY_TXN_ACTION_COMMIT) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_ABORTED, BOUNDARY_TXN_ACTION_ABORT) &&
            boundary_txn_transition_allowed(BOUNDARY_TXN_ABORTED, BOUNDARY_TXN_ACTION_CLOSE) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_CLOSED, BOUNDARY_TXN_ACTION_COMMIT) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_CLOSED, BOUNDARY_TXN_ACTION_ABORT) &&
            !boundary_txn_transition_allowed(BOUNDARY_TXN_CLOSED, BOUNDARY_TXN_ACTION_CLOSE);

        if (matrix_ok) {
            emit_pass_literal("[PASS] lifetime: boundary txn transition policy matrix");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: boundary txn transition policy matrix");
            (*fail)++;
        }
    }

    // Invariant: begin->commit->close is valid and session close restores state.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        BoundaryTxn txn = boundary_txn_begin(interp);
        bool begin_ok = txn.state == BOUNDARY_TXN_OPEN &&
                        txn.session.active &&
                        txn.session.current_scope == saved_scope &&
                        txn.session.releasing_scope == saved_releasing;
        boundary_txn_mark_committed(&txn);
        bool commit_ok = txn.state == BOUNDARY_TXN_COMMITTED &&
                         txn.session.active;
        boundary_txn_close(interp, &txn);
        bool close_ok = txn.state == BOUNDARY_TXN_CLOSED &&
                        !txn.session.active &&
                        interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;

        bool ok = begin_ok && commit_ok && close_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary txn commit protocol");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary txn commit protocol (begin_ok=%s commit_ok=%s close_ok=%s)",
                begin_ok ? "yes" : "no",
                commit_ok ? "yes" : "no",
                close_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    // Invariant: begin->abort->close is valid and session close restores state.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing = interp.releasing_scope;

        BoundaryTxn txn = boundary_txn_begin(interp);
        bool begin_ok = txn.state == BOUNDARY_TXN_OPEN &&
                        txn.session.active &&
                        txn.session.current_scope == saved_scope &&
                        txn.session.releasing_scope == saved_releasing;
        boundary_txn_mark_aborted(&txn);
        bool abort_ok = txn.state == BOUNDARY_TXN_ABORTED &&
                        txn.session.active;
        boundary_txn_close(interp, &txn);
        bool close_ok = txn.state == BOUNDARY_TXN_CLOSED &&
                        !txn.session.active &&
                        interp.current_scope == saved_scope &&
                        interp.releasing_scope == saved_releasing;

        bool ok = begin_ok && abort_ok && close_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary txn abort protocol");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary txn abort protocol (begin_ok=%s abort_ok=%s close_ok=%s)",
                begin_ok ? "yes" : "no",
                abort_ok ? "yes" : "no",
                close_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

const usz BOUNDARY_BENCH_DECISION_ITERS = 2048;
const usz BOUNDARY_BENCH_SPLICE_ITERS = 2048;
const usz BOUNDARY_BENCH_SPLICE_ESCAPES_PER_ITER = 64;

fn bool boundary_bench_enabled() @inline {
    return boundary_c_getenv("OMNI_BOUNDARY_BENCH") != null;
}

fn long boundary_bench_now_ms() @inline {
    long now_ms = scheduler_monotonic_ms();
    if (now_ms > 0) return now_ms;
    return 0;
}

fn void run_memory_lifetime_boundary_benchmarks(Interp* interp) {
    if (!boundary_bench_enabled()) return;

    main::ScopeRegion* root_scope = interp.current_scope;

    usz decision_splice_ok = 0;
    usz decision_fallback_ok = 0;
    usz decision_reuse_ok = 0;
    usz splice_tail_ok = 0;

    long decision_splice_ms = 0;
    long decision_fallback_ms = 0;
    long decision_reuse_ms = 0;
    long splice_tail_ms = 0;

    // Decision benchmark: splice-commit path.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = releasing_scope;
            Value* src = make_cons(interp, make_int(interp, (long)(i + 1)), make_nil(interp));

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                src,
                interp,
                target_scope,
                releasing_scope,
                COPY_SITE_RUN_JIT_RESULT,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);

            bool ok = (committed.releasing_scope_spliced ||
                       committed.outcome == BOUNDARY_COMMIT_DESTINATION_CONS_BUILT) &&
                      !committed.fallback_copied &&
                      committed.value != null;
            if (ok) decision_splice_ok++;

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }
        }
        long end_ms = boundary_bench_now_ms();
        decision_splice_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Decision benchmark: budget-abort fallback copy path.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = releasing_scope;
            Value* src = boundary_commit_make_temp_int_list(interp, 8);

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, 0);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                src,
                interp,
                target_scope,
                releasing_scope,
                COPY_SITE_JIT_SINGLE_SCOPE_RETURN,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);

            bool ok = !committed.releasing_scope_spliced &&
                      committed.fallback_copied &&
                      committed.value != null;
            if (ok) decision_fallback_ok++;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
        }
        long end_ms = boundary_bench_now_ms();
        decision_fallback_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Decision benchmark: target-chain reuse path.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_DECISION_ITERS; i++) {
            main::ScopeRegion* target_scope = main::scope_create(root_scope);
            main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
            if (target_scope == null || releasing_scope == null) {
                if (releasing_scope != null) main::scope_release(releasing_scope);
                if (target_scope != null) main::scope_release(target_scope);
                continue;
            }

            interp.current_scope = target_scope;
            Value* safe = make_cons(interp, make_int(interp, (long)(i + 17)), make_nil(interp));
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                safe,
                safe,
                interp,
                target_scope,
                releasing_scope,
                COPY_SITE_JIT_SINGLE_SCOPE_RETURN
            );

            bool ok = !committed.releasing_scope_spliced &&
                      committed.reused_in_target_chain &&
                      !committed.fallback_copied &&
                      committed.value == safe;
            if (ok) decision_reuse_ok++;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
        }
        long end_ms = boundary_bench_now_ms();
        decision_reuse_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    // Splice benchmark: focus on escape-lane concat throughput/tail consistency.
    {
        long start_ms = boundary_bench_now_ms();
        for (usz i = 0; i < BOUNDARY_BENCH_SPLICE_ITERS; i++) {
            main::ScopeRegion* parent = main::scope_create(root_scope);
            main::ScopeRegion* child = parent == null ? null : main::scope_create(parent);
            if (parent == null || child == null) {
                if (child != null) main::scope_release(child);
                if (parent != null) main::scope_release(parent);
                continue;
            }

            interp.current_scope = child;
            Value* first_escape = null;
            for (usz j = 0; j < BOUNDARY_BENCH_SPLICE_ESCAPES_PER_ITER; j++) {
                Value* v = interp.alloc_value_escape();
                if (v != null) {
                    v.tag = INT;
                    v.int_val = (long)(j + 1);
                    if (j == 0) first_escape = v;
                }
            }
            interp.current_scope = root_scope;

            BoundaryScopeTransferResult ready = boundary_try_scope_splice_escapes_result(parent, child);
            bool ok = false;
            if (ready.allowed) {
                ok = first_escape != null &&
                     main::is_in_scope((void*)first_escape, parent) &&
                     main::scope_escape_chunk_tail_consistent(parent) &&
                     main::scope_escape_dtor_tail_consistent(parent);
            }
            if (ok) splice_tail_ok++;
            main::scope_release(parent);
        }
        long end_ms = boundary_bench_now_ms();
        splice_tail_ms = end_ms > start_ms ? end_ms - start_ms : 0;
    }

    interp.current_scope = root_scope;
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=boundary_decision_cost iters=%d splice_ms=%d fallback_ms=%d reuse_ms=%d splice_ok=%d fallback_ok=%d reuse_ok=%d",
        (long)BOUNDARY_BENCH_DECISION_ITERS,
        decision_splice_ms,
        decision_fallback_ms,
        decision_reuse_ms,
        (long)decision_splice_ok,
        (long)decision_fallback_ok,
        (long)decision_reuse_ok
    );
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=scope_splice_tail iters=%d escapes_per_iter=%d splice_ms=%d splice_ok=%d",
        (long)BOUNDARY_BENCH_SPLICE_ITERS,
        (long)BOUNDARY_BENCH_SPLICE_ESCAPES_PER_ITER,
        splice_tail_ms,
        (long)splice_tail_ok
    );
}
