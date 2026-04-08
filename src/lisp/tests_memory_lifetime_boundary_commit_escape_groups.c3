module lisp;

import std::io;
import main;

fn void run_memory_lifetime_boundary_commit_escape_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_boundary_commit_escape_primary_tests(interp, pass, fail);

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
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            usz generic_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_generic;
            usz commit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_commit;

            bool ok = !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      committed.outcome == BOUNDARY_COMMIT_MIXED_DESTINATION_PROMOTED &&
                      committed.value != src &&
                      committed.value != null &&
                      committed.value.tag == INT &&
                      committed.value.int_val == 909 &&
                      committed.value.scope_gen == target_scope.escape_generation &&
                      generic_delta == 0 &&
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
                io::printfn("[FAIL] lifetime: boundary_commit_escape mixed uncertain destination commit (outcome=%d spliced=%s reused=%s generic_delta=%d commit_delta=%d same=%s)",
                    (int)committed.outcome,
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    (long)generic_delta,
                    (long)commit_delta,
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: destination partial build reuses already-target-escape routed args
    // without introducing extra boundary fallback traffic.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape destination partial routed escape reuse");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* escaped_first = promote_to_escape(make_int(interp, 1201), interp);
            Value* escaped_second = promote_to_escape(make_int(interp, 1202), interp);

            interp.current_scope = releasing_scope;
            Value* src = interp.alloc_value();
            src.tag = PARTIAL_PRIM;
            src.partial_val.func = null;
            src.partial_val.remaining = 1;
            src.partial_val.first_arg = escaped_first;
            src.partial_val.second_arg = escaped_second;

            usz base_generic = copy_to_parent_site_calls(COPY_SITE_GENERIC);
            usz base_commit = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                src,
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            usz generic_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_generic;
            usz commit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_commit;

            bool ok = escaped_first != null &&
                      escaped_second != null &&
                      !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_PARTIAL_BUILT &&
                      committed.value != null &&
                      committed.value.tag == PARTIAL_PRIM &&
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
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape destination partial routed escape reuse");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape destination partial routed escape reuse (outcome=%d spliced=%s reused=%s generic_delta=%d commit_delta=%d first_same=%s second_same=%s)",
                    (int)committed.outcome,
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    (long)generic_delta,
                    (long)commit_delta,
                    (committed.value != null && committed.value.tag == PARTIAL_PRIM && committed.value.partial_val.first_arg.scope_gen == target_scope.escape_generation) ? "yes" : "no",
                    (committed.value != null && committed.value.tag == PARTIAL_PRIM && committed.value.partial_val.second_arg.scope_gen == target_scope.escape_generation) ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: aborted promotion contexts now hard-fail commit fallback paths.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape fallback disallowed path");
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
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_SINGLE_SCOPE_RETURN) - base_site;

            bool ok = !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      committed.outcome == BOUNDARY_COMMIT_FALLBACK_DISALLOWED &&
                      committed.fault_code == BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED &&
                      committed.value != null &&
                      committed.value.tag == ERROR &&
                      site_delta == 0 &&
                      main::is_in_scope((void*)committed.value, target_scope) &&
                      !main::is_in_scope((void*)committed.value, releasing_scope);

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape fallback disallowed path");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape fallback disallowed path (outcome=%d fault=%s spliced=%s reused=%s site_delta=%d same=%s)",
                    (int)committed.outcome,
                    (ZString)boundary_commit_fault_name(committed.fault_code),
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    (long)site_delta,
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: failed boundary commits must roll back staged closure-env
    // normalization so aliased closures retain their original env/env_scope.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape aliased closure rollback on failed commit");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            SymbolId payload_sym = interp.symbols.intern("__boundary_commit_alias_closure_payload");
            Env* captured_env = make_env(interp, interp.global_env);
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 731), interp);

            main::ScopeRegion* detached_env_scope = null;
            Value* closure = make_env_scope_closure_for_test(interp, captured_env, &detached_env_scope);
            bool setup_ok = closure != null &&
                            closure.tag == CLOSURE &&
                            detached_env_scope != null;
            if (setup_ok) {
                Value* promoted = promote_to_escape(closure, interp);
                setup_ok = promoted != null && promoted.tag == CLOSURE;
                closure = promoted;
            }
            if (setup_ok) {
                closure.closure_val.env = captured_env;
                setup_ok = closure.closure_val.env_scope != null;
            }

            Value* alias_closure = closure;
            Env* env_before = setup_ok ? closure.closure_val.env : null;
            main::ScopeRegion* env_scope_before = setup_ok ? closure.closure_val.env_scope : null;
            int rc_before = env_scope_before != null ? (int)env_scope_before.refcount : -1;
            BoundaryCommitEscapeResult committed = {};
            if (setup_ok) {
                committed = boundary_commit_escape(
                    closure,
                    interp,
                    target_scope,
                    releasing_scope,
                    null,
                    false
                );
            }

            int rc_after = env_scope_before != null ? (int)env_scope_before.refcount : -1;
            bool unchanged_ok = setup_ok &&
                                alias_closure != null &&
                                alias_closure.tag == CLOSURE &&
                                alias_closure.closure_val != null &&
                                alias_closure.closure_val.env == env_before &&
                                alias_closure.closure_val.env_scope == env_scope_before &&
                                closure_env_lookup_int(alias_closure, payload_sym, 731) &&
                                env_scope_before != null &&
                                rc_before == rc_after;
            bool commit_ok = setup_ok &&
                             !committed.releasing_scope_spliced &&
                             !committed.reused_in_target_chain &&
                             committed.outcome == BOUNDARY_COMMIT_FALLBACK_DISALLOWED &&
                             committed.fault_code == BOUNDARY_COMMIT_FAULT_FALLBACK_DISALLOWED &&
                             committed.value != null &&
                             committed.value.tag == ERROR;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (commit_ok && unchanged_ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape aliased closure rollback on failed commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape aliased closure rollback on failed commit (setup_ok=%s outcome=%d fault=%s unchanged=%s env_same=%s env_scope_same=%s rc_before=%d rc_after=%d)",
                    setup_ok ? "yes" : "no",
                    (int)committed.outcome,
                    (ZString)boundary_commit_fault_name(committed.fault_code),
                    unchanged_ok ? "yes" : "no",
                    setup_ok && alias_closure != null && alias_closure.tag == CLOSURE && alias_closure.closure_val != null && alias_closure.closure_val.env == env_before ? "yes" : "no",
                    setup_ok && alias_closure != null && alias_closure.tag == CLOSURE && alias_closure.closure_val != null && alias_closure.closure_val.env_scope == env_scope_before ? "yes" : "no",
                    rc_before,
                    rc_after);
                (*fail)++;
            }
        }
    }

    // Invariant: destination error escape build fails closed when the error
    // string allocation fails mid-build.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        if (target_scope == null) {
            io::printn("[FAIL] lifetime: boundary destination error escape OOM fails closed");
            (*fail)++;
        } else {
            uint target_escape_gen = target_scope.escape_generation;
            Value* source_error = make_error(interp, "boundary error escape oom");
            g_boundary_force_destination_error_escape_alloc_fail = true;
            Value* result = boundary_build_destination_error_escape(source_error, interp, target_scope);
            g_boundary_force_destination_error_escape_alloc_fail = false;

            bool ok = result == source_error &&
                      result != null &&
                      result.tag == ERROR &&
                      result.scope_gen != target_escape_gen &&
                      interp.current_scope == saved_scope &&
                      !main::is_in_scope((void*)result, target_scope);

            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary destination error escape OOM fails closed");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary destination error escape OOM fails closed (same=%s tag=%d scope_gen=%d target_escape=%d in_target=%s current_restored=%s)",
                    result == source_error ? "yes" : "no",
                    result != null ? (int)result.tag : -1,
                    result != null ? (int)result.scope_gen : -1,
                    (int)target_escape_gen,
                    result != null && main::is_in_scope((void*)result, target_scope) ? "yes" : "no",
                    interp.current_scope == saved_scope ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: generic STRING escape promotion fails closed when ESCAPE
    // allocation fails (no destination builder involved).
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* releasing_scope = main::scope_create(saved_scope);
        if (releasing_scope == null) {
            io::printn("[FAIL] lifetime: generic STRING escape promotion OOM fails closed");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = make_string(interp, "generic string promotion oom");

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            g_boundary_force_escape_string_or_error_promotion_alloc_fail = true;
            Value* promoted = promote_to_escape(src, interp);
            g_boundary_force_escape_string_or_error_promotion_alloc_fail = false;
            boundary_promotion_context_end(interp, &ctx);

            bool ok = promoted == src &&
                      promoted != null &&
                      promoted.tag == STRING &&
                      promoted.str_chars != null &&
                      interp.current_scope == releasing_scope;

            interp.current_scope = saved_scope;
            main::scope_release(releasing_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: generic STRING escape promotion OOM fails closed");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: generic STRING escape promotion OOM fails closed (same=%s tag=%d chars=%s scope_ok=%s)",
                    promoted == src ? "yes" : "no",
                    promoted != null ? (int)promoted.tag : -1,
                    promoted != null && promoted.str_chars != null ? "yes" : "no",
                    interp.current_scope == saved_scope ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: generic ERROR escape promotion fails closed when ESCAPE
    // allocation fails (no destination builder involved).
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* releasing_scope = main::scope_create(saved_scope);
        if (releasing_scope == null) {
            io::printn("[FAIL] lifetime: generic ERROR escape promotion OOM fails closed");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = make_error(interp, "generic error promotion oom");

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            g_boundary_force_escape_string_or_error_promotion_alloc_fail = true;
            Value* promoted = promote_to_escape(src, interp);
            g_boundary_force_escape_string_or_error_promotion_alloc_fail = false;
            boundary_promotion_context_end(interp, &ctx);

            bool ok = promoted == src &&
                      promoted != null &&
                      promoted.tag == ERROR &&
                      promoted.str_chars != null &&
                      interp.current_scope == releasing_scope;

            interp.current_scope = saved_scope;
            main::scope_release(releasing_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: generic ERROR escape promotion OOM fails closed");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: generic ERROR escape promotion OOM fails closed (same=%s tag=%d chars=%s scope_ok=%s)",
                    promoted == src ? "yes" : "no",
                    promoted != null ? (int)promoted.tag : -1,
                    promoted != null && promoted.str_chars != null ? "yes" : "no",
                    interp.current_scope == saved_scope ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: generic string constructor fails closed under forced allocation
    // failure without writing through null pointers.
    {
        g_value_force_make_string_alloc_fail = true;
        Value* made = make_string(interp, "constructor oom");
        g_value_force_make_string_alloc_fail = false;

        bool ok = made != null &&
                  made.tag == ERROR &&
                  made.str_chars != null &&
                  made.str_len > 0;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: make_string OOM fails closed");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: make_string OOM fails closed (tag=%d chars=%s len=%d)",
                made != null ? (int)made.tag : -1,
                made != null && made.str_chars != null ? "yes" : "no",
                made != null ? (int)made.str_len : -1);
            (*fail)++;
        }
    }
}
