module lisp;

import std::io;
import main;

fn void run_memory_lifetime_boundary_commit_escape_rollback_error_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: copied/materialized destination commits must roll back staged
    // source closure-env normalization so aliases retain original env/env_scope.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape aliased closure rollback on copied commit");
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
                             committed.outcome == BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED &&
                             committed.fault_code == BOUNDARY_COMMIT_FAULT_NONE &&
                             committed.planned_route == BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION &&
                             committed.selected_route == BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION &&
                             committed.selected_reason == BOUNDARY_PLAN_REASON_DESTINATION_COMMITTED_STABLE &&
                             committed.stable_materialization_used &&
                             committed.value != null &&
                             committed.value != alias_closure &&
                             committed.value.tag == CLOSURE;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (commit_ok && unchanged_ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape aliased closure rollback on copied commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape aliased closure rollback on copied commit (setup_ok=%s outcome=%d fault=%s unchanged=%s env_same=%s env_scope_same=%s rc_before=%d rc_after=%d)",
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

    // Invariant: direct closure escape promotion through a boundary route fails
    // closed when closure destructor registration fails, and rolls back the
    // retained env_scope immediately.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing_scope = interp.releasing_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        main::ScopeRegion* disjoint_scope = main::scope_create(saved_scope);
        if (target_scope == null || releasing_scope == null || disjoint_scope == null) {
            interp.current_scope = saved_scope;
            interp.releasing_scope = saved_releasing_scope;
            if (disjoint_scope != null) main::scope_release(disjoint_scope);
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary direct closure dtor OOM fails closed");
            (*fail)++;
        } else {
            interp.current_scope = disjoint_scope;
            SymbolId payload_sym = interp.symbols.intern("__boundary_direct_closure_dtor_oom_payload");
            Env* captured_env = make_env(interp, interp.global_env);
            main::ScopeRegion* closure_env_scope = null;
            int closure_env_destroy_count = 0;
            Value* closure = null;
            if (captured_env != null) {
                env_define_with_barrier(captured_env, payload_sym, make_int(interp, 733), interp);
                closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
                if (closure_env_scope != null) {
                    main::scope_register_dtor(closure_env_scope, (void*)&closure_env_destroy_count, fn void(void* ptr) {
                        int* counter = (int*)ptr;
                        if (counter != null) (*counter)++;
                    });
                }
            }

            bool setup_ok = closure != null &&
                            closure.tag == CLOSURE &&
                            closure_env_scope != null &&
                            closure_env_scope.refcount == 1;
            BoundaryCommitEscapeResult committed = {};
            bool saved_escape_dtor_oom = main::g_scope_force_escape_dtor_alloc_oom;
            main::g_scope_force_escape_dtor_alloc_oom = true;
            if (setup_ok) {
                PromotionContext ctx = {};
                boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
                committed = boundary_commit_escape(
                    closure,
                    interp,
                    target_scope,
                    releasing_scope,
                    &ctx
                );
                boundary_promotion_context_end(interp, &ctx);
            }
            main::g_scope_force_escape_dtor_alloc_oom = saved_escape_dtor_oom;
            interp.releasing_scope = saved_releasing_scope;

            int rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
            bool fault_ok = setup_ok &&
                            committed.outcome == BOUNDARY_COMMIT_FALLBACK_DISALLOWED &&
                            committed.fault_code == BOUNDARY_COMMIT_FAULT_MIXED_DESTINATION_PROMOTION &&
                            committed.selected_route == BOUNDARY_PLAN_ROUTE_FAIL_CLOSED &&
                            committed.value != null &&
                            committed.value.tag == ERROR &&
                            main::is_in_scope((void*)committed.value, target_scope);
            bool rollback_ok = rc_after_fail == 1 &&
                               closure_env_lookup_int(closure, payload_sym, 733);

            interp.current_scope = saved_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);
            int rc_after_target_release = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
            int destroy_count_after_target_release = closure_env_destroy_count;
            bool target_release_ok = rc_after_target_release == 1 &&
                                     destroy_count_after_target_release == 0;
            main::scope_release(disjoint_scope);
            bool owner_release_ok = closure_env_destroy_count == 1;

            bool ok = setup_ok && fault_ok && rollback_ok && target_release_ok && owner_release_ok;
            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary direct closure dtor OOM fails closed");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: boundary direct closure dtor OOM fails closed (setup_ok=%s outcome=%d fault=%s selected=%d rc_after_fail=%d rc_after_target=%d destroy_target=%d destroy_final=%d rollback=%s)",
                    setup_ok ? "yes" : "no",
                    (int)committed.outcome,
                    (ZString)boundary_commit_fault_name(committed.fault_code),
                    (int)committed.selected_route,
                    rc_after_fail,
                    rc_after_target_release,
                    destroy_count_after_target_release,
                    closure_env_destroy_count,
                    rollback_ok ? "yes" : "no"
                );
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

    // Invariant: destination error escape build fails closed when destructor
    // registration fails after the escaped error string has been allocated.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        if (target_scope == null) {
            io::printn("[FAIL] lifetime: boundary destination error escape dtor OOM fails closed");
            (*fail)++;
        } else {
            uint target_escape_gen = target_scope.escape_generation;
            Value* source_error = make_error(interp, "boundary error escape dtor oom");
            bool setup_ok = source_error != null && source_error.tag == ERROR;
            bool saved_dtor_oom = main::g_scope_force_escape_dtor_alloc_oom;
            main::g_scope_force_escape_dtor_alloc_oom = true;
            Value* result = setup_ok
                ? boundary_build_destination_error_escape(source_error, interp, target_scope)
                : null;
            main::g_scope_force_escape_dtor_alloc_oom = saved_dtor_oom;

            bool ok = setup_ok &&
                      result == source_error &&
                      result != null &&
                      result.tag == ERROR &&
                      result.scope_gen != target_escape_gen &&
                      interp.current_scope == saved_scope &&
                      !main::is_in_scope((void*)result, target_scope);

            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary destination error escape dtor OOM fails closed");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: boundary destination error escape dtor OOM fails closed (setup_ok=%s same=%s tag=%d scope_gen=%d target_escape=%d in_target=%s current_restored=%s)",
                    setup_ok ? "yes" : "no",
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

    // Invariant: destination builders fail closed when ESCAPE wrapper
    // allocation itself fails.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary destination wrapper alloc OOM fails closed");
            (*fail)++;
        } else {
            interp.current_scope = target_scope;
            Value* escaped_first = promote_to_escape(make_int(interp, 9101), interp);
            Value* escaped_second = promote_to_escape(make_int(interp, 9102), interp);
            Value* escaped_nil = promote_to_escape(make_nil(interp), interp);

            interp.current_scope = releasing_scope;
            Value* cons_tail = make_cons(interp, escaped_second, escaped_nil);
            Value* cons_src = make_cons(interp, escaped_first, cons_tail);

            Value* partial_src = interp.alloc_value();
            partial_src.tag = PARTIAL_PRIM;
            partial_src.partial_val.func = null;
            partial_src.partial_val.remaining = 1;
            partial_src.partial_val.first_arg = escaped_first;
            partial_src.partial_val.second_arg = escaped_second;

            Value* iterator_src = interp.alloc_value();
            iterator_src.tag = ITERATOR;
            iterator_src.iterator_val = escaped_first;

            Value* error_src = make_error(interp, "destination wrapper alloc oom");

            int saved_force = g_boundary_force_alloc_escape_value_fail_nth;

            g_boundary_force_alloc_escape_value_fail_nth = 1;
            Value* error_result = boundary_build_destination_error_escape(error_src, interp, target_scope);

            g_boundary_force_alloc_escape_value_fail_nth = 1;
            Value* cons_head_result = boundary_build_destination_cons_escape(
                cons_src, interp, target_scope, releasing_scope);

            g_boundary_force_alloc_escape_value_fail_nth = 2;
            Value* cons_tail_result = boundary_build_destination_cons_escape(
                cons_src, interp, target_scope, releasing_scope);

            g_boundary_force_alloc_escape_value_fail_nth = 1;
            Value* partial_result = boundary_build_destination_partial_escape(
                partial_src, interp, target_scope, releasing_scope);

            g_boundary_force_alloc_escape_value_fail_nth = 1;
            Value* iterator_result = boundary_build_destination_iterator_escape(
                iterator_src, interp, target_scope, releasing_scope);

            g_boundary_force_alloc_escape_value_fail_nth = saved_force;

            bool ok = error_result == error_src &&
                      cons_head_result == cons_src &&
                      cons_tail_result == cons_src &&
                      partial_result == partial_src &&
                      iterator_result == iterator_src &&
                      !main::is_in_scope((void*)error_result, target_scope) &&
                      !main::is_in_scope((void*)cons_head_result, target_scope) &&
                      !main::is_in_scope((void*)cons_tail_result, target_scope) &&
                      !main::is_in_scope((void*)partial_result, target_scope) &&
                      !main::is_in_scope((void*)iterator_result, target_scope) &&
                      interp.current_scope == releasing_scope;

            interp.current_scope = saved_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary destination wrapper alloc OOM fails closed");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: boundary destination wrapper alloc OOM fails closed (error_same=%s cons_head_same=%s cons_tail_same=%s partial_same=%s iterator_same=%s scope_ok=%s)",
                    error_result == error_src ? "yes" : "no",
                    cons_head_result == cons_src ? "yes" : "no",
                    cons_tail_result == cons_src ? "yes" : "no",
                    partial_result == partial_src ? "yes" : "no",
                    iterator_result == iterator_src ? "yes" : "no",
                    interp.current_scope == saved_scope ? "yes" : "no"
                );
                (*fail)++;
            }
        }
    }

    // Invariant: generic STRING escape promotion returns a deterministic
    // boundary error when escape-side allocation fails.
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

            bool ok = promoted != null &&
                      promoted.tag == ERROR &&
                      promoted.str_chars != null &&
                      str_starts_with(
                          promoted.str_chars[:promoted.str_len],
                          "boundary: failed to allocate string/error escape copy"
                      ) &&
                      interp.current_scope == releasing_scope;

            interp.current_scope = saved_scope;
            main::scope_release(releasing_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: generic STRING escape promotion OOM fails closed");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: generic STRING escape promotion OOM fails closed (tag=%d chars=%s scope_ok=%s)",
                    promoted != null ? (int)promoted.tag : -1,
                    promoted != null && promoted.str_chars != null ? "yes" : "no",
                    interp.current_scope == saved_scope ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: generic ERROR escape promotion returns a deterministic
    // boundary error when escape-side allocation fails.
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

            bool ok = promoted != null &&
                      promoted.tag == ERROR &&
                      promoted.str_chars != null &&
                      str_starts_with(
                          promoted.str_chars[:promoted.str_len],
                          "boundary: failed to allocate string/error escape copy"
                      ) &&
                      interp.current_scope == releasing_scope;

            interp.current_scope = saved_scope;
            main::scope_release(releasing_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: generic ERROR escape promotion OOM fails closed");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: generic ERROR escape promotion OOM fails closed (tag=%d chars=%s scope_ok=%s)",
                    promoted != null ? (int)promoted.tag : -1,
                    promoted != null && promoted.str_chars != null ? "yes" : "no",
                    interp.current_scope == saved_scope ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: generic STRING and ERROR escape promotion return deterministic
    // boundary errors when wrapper allocation fails after payload allocation.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* releasing_scope = main::scope_create(saved_scope);
        if (releasing_scope == null) {
            io::printn("[FAIL] lifetime: generic STRING/ERROR wrapper escape OOM fails closed");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src_string = make_string(interp, "generic string wrapper oom");
            Value* src_error = make_error(interp, "generic error wrapper oom");
            int saved_force = g_boundary_force_alloc_escape_value_fail_nth;

            PromotionContext ctx_string = {};
            boundary_promotion_context_begin(interp, &ctx_string, PROMOTION_CTX_BUDGET_RETURN);
            g_boundary_force_alloc_escape_value_fail_nth = 1;
            Value* promoted_string = promote_to_escape(src_string, interp);
            boundary_promotion_context_end(interp, &ctx_string);

            PromotionContext ctx_error = {};
            boundary_promotion_context_begin(interp, &ctx_error, PROMOTION_CTX_BUDGET_RETURN);
            g_boundary_force_alloc_escape_value_fail_nth = 1;
            Value* promoted_error = promote_to_escape(src_error, interp);
            boundary_promotion_context_end(interp, &ctx_error);
            g_boundary_force_alloc_escape_value_fail_nth = saved_force;

            bool ok = promoted_string != null &&
                      promoted_error != null &&
                      promoted_string.tag == ERROR &&
                      promoted_error.tag == ERROR &&
                      promoted_string.str_chars != null &&
                      promoted_error.str_chars != null &&
                      str_starts_with(
                          promoted_string.str_chars[:promoted_string.str_len],
                          "boundary: failed to allocate string/error escape copy"
                      ) &&
                      str_starts_with(
                          promoted_error.str_chars[:promoted_error.str_len],
                          "boundary: failed to allocate string/error escape copy"
                      ) &&
                      interp.current_scope == releasing_scope;

            interp.current_scope = saved_scope;
            main::scope_release(releasing_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: generic STRING/ERROR wrapper escape OOM fails closed");
                (*pass)++;
            } else {
                io::printfn(
                    "[FAIL] lifetime: generic STRING/ERROR wrapper escape OOM fails closed (string_tag=%d error_tag=%d scope_ok=%s)",
                    promoted_string != null ? (int)promoted_string.tag : -1,
                    promoted_error != null ? (int)promoted_error.tag : -1,
                    interp.current_scope == saved_scope ? "yes" : "no"
                );
                (*fail)++;
            }
        }
    }

}
