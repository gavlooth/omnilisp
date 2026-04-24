module lisp;

import std::core::mem;
import std::io;
import main;

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

fn void run_memory_lifetime_boundary_commit_escape_primary_tests(Interp* interp, int* pass, int* fail) {
    {
        FfiHandle opaque_handle = {};
        opaque_handle.boundary_mode = FFI_BRIDGE_BOUNDARY_OPAQUE;
        Value opaque_value = {};
        opaque_value.tag = FFI_HANDLE;
        opaque_value.ffi_val = &opaque_handle;

        FfiHandle trace_handle = {};
        trace_handle.boundary_mode = FFI_BRIDGE_BOUNDARY_TRACE_HOOK;
        Value trace_value = {};
        trace_value.tag = FFI_HANDLE;
        trace_value.ffi_val = &trace_handle;

        FfiBridgeBoundaryDeclaration opaque_declaration =
            ffi_bridge_boundary_declaration_for_value(&opaque_value);
        FfiBridgeBoundaryDeclaration trace_declaration =
            ffi_bridge_boundary_declaration_for_value(&trace_value);

        bool ok = boundary_value_ownership_policy(FFI_HANDLE) == BOUNDARY_VALUE_OPAQUE_FOREIGN &&
                  boundary_value_tag_is_opaque_foreign(FFI_HANDLE) &&
                  !boundary_value_tag_has_omni_owned_edges(FFI_HANDLE) &&
                  !boundary_value_tag_is_stable_materialization_candidate(FFI_HANDLE) &&
                  boundary_graph_audit_edge_class(FFI_HANDLE) == BOUNDARY_GRAPH_EDGE_NONE &&
                  opaque_declaration.opaque &&
                  !opaque_declaration.unsafe &&
                  ffi_bridge_boundary_value_is_opaque(&opaque_value) &&
                  !ffi_bridge_boundary_value_requires_native_traversal(&opaque_value) &&
                  ffi_bridge_boundary_value_is_transplant_safe(&opaque_value) &&
                  trace_declaration.trace_hook &&
                  ffi_bridge_boundary_value_requires_native_traversal(&trace_value) &&
                  !ffi_bridge_boundary_value_is_transplant_safe(&trace_value) &&
                  boundary_value_tag_is_stable_materialization_candidate(ARRAY) &&
                  boundary_value_tag_requires_materialized_payload_clone(BIG_FLOAT) &&
                  boundary_graph_audit_edge_class(CLOSURE) == BOUNDARY_GRAPH_EDGE_ENV;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary ValueTag ownership policy keeps FFI opaque");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: boundary ValueTag ownership policy keeps FFI opaque");
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        FfiHandle trace_handle = {};
        trace_handle.boundary_mode = FFI_BRIDGE_BOUNDARY_TRACE_HOOK;
        Value trace_root = {};
        trace_root.tag = FFI_HANDLE;
        trace_root.ffi_val = &trace_handle;
        if (releasing_scope != null) trace_root.scope_gen = releasing_scope.escape_generation;

        BoundaryTransplantProof proof = target_scope != null && releasing_scope != null
            ? boundary_build_transplant_root_proof(target_scope, releasing_scope, &trace_root, interp.global_env)
            : (BoundaryTransplantProof){};
        bool ok = target_scope != null &&
                  releasing_scope != null &&
                  !proof.transfer.allowed &&
                  proof.transfer.reason == BOUNDARY_SCOPE_TRANSFER_CHILD_ESCAPE_LANE_INVALID &&
                  proof.ffi_native_traversal_required &&
                  !proof.ffi_native_edges_safe;

        if (releasing_scope != null) main::scope_release(releasing_scope);
        if (target_scope != null) main::scope_release(target_scope);
        interp.current_scope = saved_scope;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary FFI bridge trace hook fails closed");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: boundary FFI bridge trace hook fails closed");
            (*fail)++;
        }
    }

    {
        Value* probe_scalar = make_int(interp, 9);
        Value* probe_cons = make_cons(interp, make_int(interp, 1), make_nil(interp));
        BoundaryReturnClassification reusable = {
            .provenance = BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN,
            .in_target_scope_chain = true,
            .in_releasing_scope = false,
        };
        BoundaryReturnClassification temp_owned = {
            .provenance = BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP,
            .in_target_scope_chain = false,
            .in_releasing_scope = true,
        };
        BoundaryReturnClassification mixed = {
            .provenance = BOUNDARY_RETURN_MIXED_UNCERTAIN,
            .in_target_scope_chain = false,
            .in_releasing_scope = false,
        };
        BoundaryReturnClassification escape_owned = {
            .provenance = BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE,
            .in_target_scope_chain = false,
            .in_releasing_scope = true,
        };
        BoundaryPlanDecision reuse_plan = boundary_plan_commit_escape(probe_scalar, reusable, true, null);
        BoundaryPlanDecision scalar_plan = boundary_plan_commit_escape(probe_scalar, temp_owned, true, null);
        BoundaryPlanDecision cons_plan = boundary_plan_commit_escape(probe_cons, temp_owned, true, null);
        BoundaryPlanDecision escape_cons_plan = boundary_plan_commit_escape(probe_cons, escape_owned, false, null);
        BoundaryPlanDecision mixed_plan = boundary_plan_commit_escape(probe_scalar, mixed, true, null);
        bool ok = reuse_plan.route == BOUNDARY_PLAN_ROUTE_STABLE_PUBLISH_REUSE &&
                  reuse_plan.reason == BOUNDARY_PLAN_REASON_REUSABLE_IN_TARGET_CHAIN &&
                  reuse_plan.stable_publish_candidate &&
                  scalar_plan.route == BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION &&
                  scalar_plan.reason == BOUNDARY_PLAN_REASON_RELEASING_TEMP_COMPATIBILITY &&
                  scalar_plan.compatibility_candidate &&
                  cons_plan.route == BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION &&
                  cons_plan.reason == BOUNDARY_PLAN_REASON_RELEASING_TEMP_STABLE_MATERIALIZATION &&
                  cons_plan.stable_materialization_candidate &&
                  cons_plan.compatibility_candidate &&
                  escape_cons_plan.route == BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION &&
                  escape_cons_plan.reason == BOUNDARY_PLAN_REASON_RELEASING_ESCAPE_DESTINATION &&
                  escape_cons_plan.stable_materialization_candidate &&
                  mixed_plan.route == BOUNDARY_PLAN_ROUTE_MIXED_DESTINATION &&
                  mixed_plan.reason == BOUNDARY_PLAN_REASON_MIXED_UNCERTAIN &&
                  mixed_plan.compatibility_candidate;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary planner route ladder baseline");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: boundary planner route ladder baseline (reuse=%d scalar=%d cons=%d mixed=%d)",
                (int)reuse_plan.route,
                (int)scalar_plan.route,
                (int)cons_plan.route,
                (int)mixed_plan.route);
            (*fail)++;
        }
    }

    // Invariant: TEMP cons survivor commits through proof-backed transplant
    // before destination materialization when the ownership island is movable.
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
                interp,
                target_scope,
                releasing_scope,
                &ctx
            );
            boundary_promotion_context_end(interp, &ctx);
            usz site_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_site;

            bool ok = committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      !committed.stable_publication_attempted &&
                      !committed.stable_materialization_used &&
                      !committed.compatibility_path_used &&
                      committed.planned_route == BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION &&
                      committed.selected_route == BOUNDARY_PLAN_ROUTE_REGION_TRANSPLANT &&
                      committed.selected_reason == BOUNDARY_PLAN_REASON_TRANSPLANT_COMMITTED &&
                      committed.outcome == BOUNDARY_COMMIT_SPLICED_RELEASING_SCOPE &&
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
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP survivor commit (outcome=%d spliced=%s reused=%s stable_attempted=%s stable_materialized=%s compatibility=%s site_delta=%d same=%s)",
                    (int)committed.outcome,
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    committed.stable_publication_attempted ? "yes" : "no",
                    committed.stable_materialization_used ? "yes" : "no",
                    committed.compatibility_path_used ? "yes" : "no",
                    (long)site_delta,
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: TEMP array survivor materializes from prepared graph metadata.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape TEMP array stable materialization");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = interp.alloc_value();
            src.tag = ARRAY;
            src.array_val = null;
            main::scope_register_dtor(releasing_scope, (void*)src, &scope_dtor_value);
            Array* src_arr = (Array*)mem::malloc(Array.sizeof);
            bool setup_ok = src_arr != null;
            if (setup_ok) {
                src_arr.items = null;
                src_arr.length = 4;
                src_arr.capacity = 4;
                src_arr.items = (Value**)mem::malloc(Value*.sizeof * src_arr.capacity);
                setup_ok = src_arr.items != null;
            }
            if (setup_ok) {
                for (usz i = 0; i < src_arr.capacity; i++) src_arr.items[i] = null;
                src_arr.items[0] = make_string(interp, "stable-array");
                src_arr.items[1] = make_int(interp, 909);
                src_arr.items[2] = make_big_float_from_double(interp, 3.5);
                src_arr.items[3] = make_big_complex_from_double(interp, 4.5);
                setup_ok = src_arr.items[2] != null &&
                           src_arr.items[2].tag == BIG_FLOAT &&
                           src_arr.items[2].big_float_val != null &&
                           src_arr.items[3] != null &&
                           src_arr.items[3].tag == BIG_COMPLEX &&
                           src_arr.items[3].big_complex_val != null;
                src.array_val = src_arr;
            } else if (src_arr != null) {
                if (src_arr.items != null) mem::free(src_arr.items);
                mem::free(src_arr);
            }

            usz base_commit = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = {};
            if (setup_ok) committed = boundary_commit_escape(src, interp, target_scope, releasing_scope, &ctx);
            boundary_promotion_context_end(interp, &ctx);
            usz commit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_commit;

            Array* dst_arr = committed.value != null && committed.value.tag == ARRAY ? committed.value.array_val : null;
            bool array_ok = dst_arr != null &&
                            dst_arr.length == 4 &&
                            dst_arr.items[0] != null &&
                            dst_arr.items[0].tag == STRING &&
                            dst_arr.items[0].str_len == 12 &&
                            dst_arr.items[0] != src_arr.items[0] &&
                            dst_arr.items[0].scope_gen == target_scope.escape_generation &&
                            dst_arr.items[1] != null &&
                            dst_arr.items[1].tag == INT &&
                            dst_arr.items[1].int_val == 909 &&
                            dst_arr.items[1].scope_gen == target_scope.escape_generation &&
                            dst_arr.items[2] != null &&
                            dst_arr.items[2].tag == BIG_FLOAT &&
                            dst_arr.items[2].big_float_val != null &&
                            dst_arr.items[2].big_float_val != src_arr.items[2].big_float_val &&
                            dst_arr.items[2].scope_gen == target_scope.escape_generation &&
                            dst_arr.items[3] != null &&
                            dst_arr.items[3].tag == BIG_COMPLEX &&
                            dst_arr.items[3].big_complex_val != null &&
                            dst_arr.items[3].big_complex_val != src_arr.items[3].big_complex_val &&
                            dst_arr.items[3].scope_gen == target_scope.escape_generation;
            bool ok = setup_ok &&
                      !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      committed.stable_materialization_used &&
                      !committed.compatibility_path_used &&
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED &&
                      committed.value != src &&
                      array_ok &&
                      committed.value.scope_gen == target_scope.escape_generation &&
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
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape TEMP array stable materialization");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP array stable materialization (setup_ok=%s outcome=%d stable_materialized=%s compatibility=%s commit_delta=%d same=%s)",
                    setup_ok ? "yes" : "no",
                    (int)committed.outcome,
                    committed.stable_materialization_used ? "yes" : "no",
                    committed.compatibility_path_used ? "yes" : "no",
                    (long)commit_delta,
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: TEMP BigInteger survivor materializes with a cloned payload.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape TEMP BigInteger stable materialization");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = make_big_integer_from_i64(interp, 123456789);
            bool setup_ok = src != null && src.tag == BIG_INTEGER && src.big_integer_val != null;

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = {};
            if (setup_ok) committed = boundary_commit_escape(src, interp, target_scope, releasing_scope, &ctx);
            boundary_promotion_context_end(interp, &ctx);

            bool big_ok = committed.value != null &&
                          committed.value.tag == BIG_INTEGER &&
                          committed.value.big_integer_val != null &&
                          committed.value.big_integer_val != src.big_integer_val &&
                          big_integer_values_equal(committed.value, src);
            bool ok = setup_ok &&
                      !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      committed.stable_materialization_used &&
                      !committed.compatibility_path_used &&
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED &&
                      committed.value != src &&
                      big_ok &&
                      committed.value.scope_gen == target_scope.escape_generation &&
                      main::is_in_scope((void*)committed.value, target_scope);

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape TEMP BigInteger stable materialization");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP BigInteger stable materialization (setup_ok=%s outcome=%d stable_materialized=%s compatibility=%s same=%s)",
                    setup_ok ? "yes" : "no",
                    (int)committed.outcome,
                    committed.stable_materialization_used ? "yes" : "no",
                    committed.compatibility_path_used ? "yes" : "no",
                    committed.value == src ? "yes" : "no");
                (*fail)++;
            }
        }
    }

    // Invariant: typed TEMP closure survivor materializes its signature into ESCAPE.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape typed TEMP closure stable materialization");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            Value* src = make_closure(interp, interp.sym_true, null, null);
            bool setup_ok = src != null && src.tag == CLOSURE && src.closure_val != null;
            MethodSignature* sig = null;
            if (setup_ok) {
                sig = (MethodSignature*)releasing_scope.alloc(MethodSignature.sizeof);
                setup_ok = sig != null;
            }
            if (setup_ok) {
                method_signature_init_empty(sig);
                setup_ok = method_signature_alloc_arrays_in_scope(sig, releasing_scope, 1, 0);
            }
            if (setup_ok) {
                sig.param_types[0] = interp.tid_Integer;
                sig.param_type_vars[0] = (SymbolId)0;
                sig.val_literals[0].tag = NIL;
                sig.val_literals[0].int_value = 0;
                sig.val_literals[0].sym_value = (SymbolId)0;
                sig.has_val_literal[0] = false;
                src.closure_val.type_sig = sig;
            }

            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = {};
            if (setup_ok) committed = boundary_commit_escape(src, interp, target_scope, releasing_scope, &ctx);
            boundary_promotion_context_end(interp, &ctx);

            Closure* dst_closure = committed.value != null && committed.value.tag == CLOSURE
                ? committed.value.closure_val
                : null;
            MethodSignature* dst_sig = dst_closure != null ? dst_closure.type_sig : null;
            bool sig_ok = dst_sig != null &&
                          dst_sig != sig &&
                          dst_sig.param_count == 1 &&
                          dst_sig.param_types != null &&
                          dst_sig.param_types != sig.param_types &&
                          dst_sig.param_types[0] == interp.tid_Integer &&
                          dst_sig.has_val_literal != null &&
                          !dst_sig.has_val_literal[0];
            bool ok = setup_ok &&
                      !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      committed.stable_materialization_used &&
                      !committed.compatibility_path_used &&
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED &&
                      committed.value != src &&
                      dst_closure != null &&
                      dst_closure != src.closure_val &&
                      sig_ok &&
                      committed.value.scope_gen == target_scope.escape_generation &&
                      main::is_in_scope((void*)committed.value, target_scope);

            interp.current_scope = root_scope;
            if (committed.releasing_scope_spliced) {
                main::scope_release(target_scope);
            } else {
                main::scope_release(releasing_scope);
                main::scope_release(target_scope);
            }

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape typed TEMP closure stable materialization");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape typed TEMP closure stable materialization (setup_ok=%s outcome=%d stable_materialized=%s compatibility=%s same=%s sig_ok=%s)",
                    setup_ok ? "yes" : "no",
                    (int)committed.outcome,
                    committed.stable_materialization_used ? "yes" : "no",
                    committed.compatibility_path_used ? "yes" : "no",
                    committed.value == src ? "yes" : "no",
                    sig_ok ? "yes" : "no");
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
            Value* safe_seed = make_cons(interp, make_int(interp, 55), make_nil(interp));
            Value* safe = safe_seed != null ? promote_to_escape(safe_seed, interp) : null;
            usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_SINGLE_SCOPE_RETURN);
            usz publication_hooks_before = stable_escape_store_publication_hook_count_snapshot();
            usz prepared_hooks_before = stable_escape_store_prepared_publication_hook_count_snapshot();
            BoundaryCommitEscapeResult committed = boundary_commit_escape(
                safe,
                interp,
                target_scope,
                releasing_scope
            );
            usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_SINGLE_SCOPE_RETURN) - base_site;
            usz publication_hooks_after = stable_escape_store_publication_hook_count_snapshot();
            usz prepared_hooks_after = stable_escape_store_prepared_publication_hook_count_snapshot();

            bool ok = !committed.releasing_scope_spliced &&
                      committed.reused_in_target_chain &&
                      committed.stable_publication_attempted &&
                      !committed.stable_materialization_used &&
                      !committed.compatibility_path_used &&
                      committed.planned_route == BOUNDARY_PLAN_ROUTE_STABLE_PUBLISH_REUSE &&
                      committed.selected_route == BOUNDARY_PLAN_ROUTE_STABLE_PUBLISH_REUSE &&
                      committed.selected_reason == BOUNDARY_PLAN_REASON_REUSABLE_IN_TARGET_CHAIN &&
                      committed.value == safe &&
                      site_delta == 0 &&
                      publication_hooks_after == publication_hooks_before + 1 &&
                      prepared_hooks_after == prepared_hooks_before + 1;

            interp.current_scope = root_scope;
            main::scope_release(releasing_scope);
            main::scope_release(target_scope);

            if (ok) {
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape already-safe reuse");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape already-safe reuse (spliced=%s reused=%s stable_attempted=%s stable_materialized=%s compatibility=%s site_delta=%d same=%s pub_hooks_before=%d pub_hooks_after=%d prepared_before=%d prepared_after=%d)",
                    committed.releasing_scope_spliced ? "yes" : "no",
                    committed.reused_in_target_chain ? "yes" : "no",
                    committed.stable_publication_attempted ? "yes" : "no",
                    committed.stable_materialization_used ? "yes" : "no",
                    committed.compatibility_path_used ? "yes" : "no",
                    (long)site_delta,
                    committed.value == safe ? "yes" : "no",
                    (long)publication_hooks_before,
                    (long)publication_hooks_after,
                    (long)prepared_hooks_before,
                    (long)prepared_hooks_after);
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
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP partial survivor commit (outcome=%d spliced=%s reused=%s generic_delta=%d commit_delta=%d same=%s)",
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
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP iterator survivor commit (outcome=%d spliced=%s reused=%s generic_delta=%d commit_delta=%d same=%s)",
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

    // Invariant: TEMP tensor survivor commits through direct destination ESCAPE promotion.
    {
        main::ScopeRegion* root_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(root_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary_commit_escape TEMP tensor direct destination commit");
            (*fail)++;
        } else {
            interp.current_scope = releasing_scope;
            usz[1] tensor_shape = { 2 };
            Value* src = make_tensor_double_uninitialized(interp, tensor_shape[0:1]);
            bool setup_ok = src != null &&
                            src.tag == TENSOR &&
                            src.tensor_val != null &&
                            src.tensor_val.data != null;
            if (setup_ok) {
                double* src_data = (double*)src.tensor_val.data;
                src_data[0] = 11.0;
                src_data[1] = 22.0;
            }

            usz base_generic = copy_to_parent_site_calls(COPY_SITE_GENERIC);
            usz base_commit = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT);
            PromotionContext ctx = {};
            boundary_promotion_context_begin(interp, &ctx, PROMOTION_CTX_BUDGET_RETURN);
            BoundaryCommitEscapeResult committed = {};
            if (setup_ok) {
                committed = boundary_commit_escape(src, interp, target_scope, releasing_scope, &ctx);
            }
            boundary_promotion_context_end(interp, &ctx);
            usz generic_delta = copy_to_parent_site_calls(COPY_SITE_GENERIC) - base_generic;
            usz commit_delta = copy_to_parent_site_calls(COPY_SITE_RUN_JIT_RESULT) - base_commit;

            bool tensor_ok = committed.value != null &&
                             committed.value.tag == TENSOR &&
                             committed.value.tensor_val != null &&
                             committed.value.tensor_val.rank == 1 &&
                             committed.value.tensor_val.shape != null &&
                             committed.value.tensor_val.shape[0] == 2 &&
                             committed.value.tensor_val.data != null &&
                             ((double*)committed.value.tensor_val.data)[0] == 11.0 &&
                             ((double*)committed.value.tensor_val.data)[1] == 22.0;
            bool ok = setup_ok &&
                      !committed.releasing_scope_spliced &&
                      !committed.reused_in_target_chain &&
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED &&
                      committed.planned_route == BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION &&
                      committed.selected_route == BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION &&
                      committed.selected_reason == BOUNDARY_PLAN_REASON_DESTINATION_COMMITTED_COMPATIBILITY &&
                      committed.value != src &&
                      tensor_ok &&
                      committed.value.scope_gen == target_scope.escape_generation &&
                      generic_delta == 0 &&
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
                emit_pass_literal("[PASS] lifetime: boundary_commit_escape TEMP tensor direct destination commit");
                (*pass)++;
            } else {
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP tensor direct destination commit (setup_ok=%s outcome=%d spliced=%s reused=%s generic_delta=%d commit_delta=%d same=%s)",
                    setup_ok ? "yes" : "no",
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
                      committed.outcome == BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED &&
                      committed.planned_route == BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION &&
                      committed.selected_route == BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION &&
                      committed.selected_reason == BOUNDARY_PLAN_REASON_DESTINATION_COMMITTED_COMPATIBILITY &&
                      committed.value != src &&
                      committed.value != null &&
                      committed.value.tag == INT &&
                      committed.value.int_val == 777 &&
                      committed.value.scope_gen == target_scope.escape_generation &&
                      generic_delta == 0 &&
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
                io::printfn("[FAIL] lifetime: boundary_commit_escape TEMP scalar direct destination commit (outcome=%d spliced=%s reused=%s generic_delta=%d commit_delta=%d same=%s)",
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
}
