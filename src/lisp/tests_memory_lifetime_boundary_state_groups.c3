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

        Value* noop = null;
        {
            BoundarySession copy_session = boundary_session_begin(interp);
            defer boundary_session_end(interp, &copy_session);
            interp.current_scope = scope_c;
            interp.releasing_scope = scope_d;
            BoundaryCopyResult noop_result = boundary_copy_to_parent_site_ctx_checked(
                null,
                interp,
                COPY_SITE_COPY_ENV_TO_SCOPE,
                null
            );
            noop = noop_result.value;
        }
        bool copy_scope_ok = noop == null &&
                             interp.current_scope == scope_a &&
                             interp.releasing_scope == scope_b;

        Value* local_val = make_int(interp, 42);
        Value* copied = test_copy_from_releasing_scope(
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

fn void run_memory_lifetime_boundary_copy_fault_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: checked boundary copy path reports typed faults for missing provenance.
    {
        main::ScopeRegion* saved_releasing = interp.releasing_scope;
        interp.releasing_scope = null;

        Value* src = make_int(interp, 901);
        BoundaryCopyResult checked = boundary_copy_to_parent_site_ctx_checked(
            src,
            interp,
            COPY_SITE_GENERIC,
            null
        );
        BoundaryCopyFault direct_fault = BOUNDARY_COPY_FAULT_NONE;
        Value* direct_checked = copy_to_parent_with_fault(src, interp, &direct_fault);

        interp.releasing_scope = saved_releasing;

        bool ok = !boundary_copy_result_ok(checked) &&
                  checked.value == null &&
                  checked.fault_code == BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE &&
                  direct_checked == null &&
                  direct_fault == BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary copy fault classification");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary copy fault classification (ok_checked=%s checked_fault=%s direct_fault=%s direct_null=%s result_tag=%d)",
                (!boundary_copy_result_ok(checked) && checked.fault_code == BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE) ? "yes" : "no",
                (ZString)boundary_copy_fault_name(checked.fault_code),
                (ZString)boundary_copy_fault_name(direct_fault),
                direct_checked == null ? "yes" : "no",
                checked.value != null ? (int)checked.value.tag : -1
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_boundary_invariant_policy_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: boundary invariant checks are enabled by default in tests.
    {
        (void)interp;
        bool disabled_via_env = boundary_env_flag_enabled("OMNI_DISABLE_BOUNDARY_INVARIANTS");
        bool enabled = boundary_invariant_checks_enabled();
        bool ok = disabled_via_env ? !enabled : enabled;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary invariant policy defaults");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary invariant policy defaults (env_disabled=%s enabled=%s)",
                disabled_via_env ? "yes" : "no",
                enabled ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_boundary_scope_scan_budget_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: one logical hinted scope-chain lookup consumes the scan budget once.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved_scope);
        main::ScopeRegion* child = parent != null ? main::scope_create(parent) : null;
        if (parent == null || child == null) {
            if (child != null) main::scope_release(child);
            if (parent != null) main::scope_release(parent);
            io::printn("[FAIL] lifetime: boundary scope scan budget applies per lookup");
            (*fail)++;
            return;
        }

        interp.current_scope = parent;
        Value* target_owned = make_int(interp, 9182);
        interp.current_scope = saved_scope;

        BoundaryRuntimePolicy saved_policy = g_boundary_runtime_policy;
        BoundaryDecisionStats saved_stats = boundary_decision_stats_snapshot();
        g_boundary_runtime_policy.initialized = true;
        g_boundary_runtime_policy.scope_chain_scan_bypass = false;
        g_boundary_runtime_policy.scope_chain_scan_budget = 1;
        boundary_reset_decision_stats();

        bool found = boundary_ptr_in_scope_chain_with_hint(
            (void*)target_owned,
            child,
            0
        );
        BoundaryDecisionStats stats = boundary_decision_stats_snapshot();

        boundary_restore_decision_stats(saved_stats);
        g_boundary_runtime_policy = saved_policy;
        main::scope_release(child);
        main::scope_release(parent);

        bool telemetry_ok = !boundary_telemetry_enabled() ||
            (stats.scope_chain_scan_total == 1 &&
             stats.scope_chain_scan_fallback == 1 &&
             stats.scope_chain_scan_suppressed == 0);
        bool ok = found && telemetry_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary scope scan budget applies per lookup");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary scope scan budget applies per lookup (found=%s total=%d fallback=%d suppressed=%d)",
                found ? "yes" : "no",
                (long)stats.scope_chain_scan_total,
                (long)stats.scope_chain_scan_fallback,
                (long)stats.scope_chain_scan_suppressed
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_boundary_zero_hint_scope_cache_test(Interp* interp, int* pass, int* fail) {
    // Invariant: zero-generation hinted lookups participate in the same route cache.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* parent = main::scope_create(saved_scope);
        main::ScopeRegion* child = parent != null ? main::scope_create(parent) : null;
        if (parent == null || child == null) {
            if (child != null) main::scope_release(child);
            if (parent != null) main::scope_release(parent);
            io::printn("[FAIL] lifetime: boundary zero-hint scope cache");
            (*fail)++;
            return;
        }

        interp.current_scope = parent;
        Value* parent_value = make_int(interp, 17701);
        interp.current_scope = child;

        PromotionContext ctx = {};
        promotion_context_begin(interp, &ctx, usz.max);
        boundary_reset_decision_stats();

        bool all_found = true;
        for (usz i = 0; i < 128; i++) {
            if (!boundary_ptr_in_target_scope_chain_with_hint((void*)parent_value, interp, 0)) {
                all_found = false;
                break;
            }
        }

        BoundaryDecisionStats stats = boundary_decision_stats_snapshot();
        promotion_context_end(interp, &ctx);

        interp.current_scope = saved_scope;
        main::scope_release(child);
        main::scope_release(parent);

        bool telemetry_ok = !boundary_telemetry_enabled() ||
            (stats.scope_chain_scan_total == 1 &&
             stats.scope_chain_scan_fallback == 1 &&
             stats.scope_chain_scan_suppressed == 0);
        bool ok = all_found &&
                  interp.active_promotion_ctx == null &&
                  telemetry_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary zero-hint scope cache");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary zero-hint scope cache (found=%s active_ctx=%s total=%d fallback=%d suppressed=%d)",
                all_found ? "yes" : "no",
                interp.active_promotion_ctx == null ? "yes" : "no",
                (long)stats.scope_chain_scan_total,
                (long)stats.scope_chain_scan_fallback,
                (long)stats.scope_chain_scan_suppressed
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_boundary_explicit_target_scope_cache_test(Interp* interp, int* pass, int* fail) {
    // Invariant: explicit target-scope lookups use the active promotion-context route cache.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved_scope);
        main::ScopeRegion* current = main::scope_create(saved_scope);
        if (target == null || current == null) {
            if (current != null) main::scope_release(current);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: boundary explicit-target scope cache");
            (*fail)++;
            return;
        }

        interp.current_scope = target;
        Value* target_value = make_int(interp, 17702);
        interp.current_scope = current;

        PromotionContext ctx = {};
        promotion_context_begin(interp, &ctx, usz.max);
        boundary_reset_decision_stats();

        bool all_found = true;
        for (usz i = 0; i < 128; i++) {
            if (!boundary_ptr_in_scope_chain_with_hint_cached((void*)target_value, interp, target, 0)) {
                all_found = false;
                break;
            }
        }

        BoundaryDecisionStats stats = boundary_decision_stats_snapshot();
        promotion_context_end(interp, &ctx);

        interp.current_scope = saved_scope;
        main::scope_release(current);
        main::scope_release(target);

        bool telemetry_ok = !boundary_telemetry_enabled() ||
            (stats.scope_chain_scan_total == 1 &&
             stats.scope_chain_scan_fallback == 1 &&
             stats.scope_chain_scan_suppressed == 0);
        bool ok = all_found &&
                  interp.active_promotion_ctx == null &&
                  telemetry_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary explicit-target scope cache");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary explicit-target scope cache (found=%s active_ctx=%s total=%d fallback=%d suppressed=%d)",
                all_found ? "yes" : "no",
                interp.active_promotion_ctx == null ? "yes" : "no",
                (long)stats.scope_chain_scan_total,
                (long)stats.scope_chain_scan_fallback,
                (long)stats.scope_chain_scan_suppressed
            );
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

        Value* copied = test_copy_from_releasing_scope(
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

fn void run_memory_lifetime_boundary_nested_effect_payload_test(Interp* interp, int* pass, int* fail) {
    // Invariant: boundary provenance checks must handle nested dict/list payloads
    // returned through an effect handler without recursive stack overflow.
    test_truthy(interp, "lifetime: boundary nested effect payload graph",
        "(block "
        "  (define sample-view "
        "    {'kind 'window "
        "     'props {} "
        "     'children (list {'kind 'vbox "
        "                      'props {} "
        "                      'children (list {'kind 'graph "
        "                                       'props {'series (list 0 1)} "
        "                                       'children nil})})}) "
        "  (define captured "
        "    (let (events nil) "
        "      (handle "
        "        (block (signal render sample-view) (reverse events)) "
        "        (render view "
        "          (set! events (cons {'tag 'render 'arg view} events)) "
        "          (resolve nil))))) "
        "  (= (ref (ref captured 0) 'tag) 'render))",
        pass,
        fail);
}

fn void run_memory_lifetime_boundary_wide_scalar_payload_test(Interp* interp, int* pass, int* fail) {
    // Scalar leaves should not consume alias traversal slots; only values with
    // child payloads need worklist/seen tracking.
    usz count = BOUNDARY_ALIAS_MAX_VISITS + 8;
    Value* arr = make_array_checked(interp, count, "wide scalar payload: array allocation failed");
    if (arr == null || arr.tag == ERROR || arr.array_val == null) {
        io::printn("[FAIL] lifetime: boundary wide scalar payload reuse");
        (*fail)++;
        return;
    }

    for (usz i = 0; i < count; i++) {
        arr.array_val.items[i] = make_int(interp, (long)i);
        arr.array_val.length++;
    }

    bool unsafe = boundary_graph_alias_unsafe_for_reuse(arr, interp);
    if (!unsafe) {
        emit_pass_literal("[PASS] lifetime: boundary wide scalar payload reuse");
        (*pass)++;
    } else {
        io::printn("[FAIL] lifetime: boundary wide scalar payload reuse");
        (*fail)++;
    }
}

fn void run_memory_lifetime_boundary_shared_composite_cycle_payload_test(Interp* interp, int* pass, int* fail) {
    // Repeated composite aliases should not consume visit slots, and a
    // root/shared cycle must be recognized through exact identity tracking.
    usz count = BOUNDARY_ALIAS_MAX_VISITS + 8;
    Value* root = make_array_checked(interp, count, "shared composite payload: root allocation failed");
    Value* shared = make_array_checked(interp, 1, "shared composite payload: shared allocation failed");
    if (root == null || root.tag == ERROR || root.array_val == null ||
        shared == null || shared.tag == ERROR || shared.array_val == null) {
        io::printn("[FAIL] lifetime: boundary shared composite cycle payload reuse");
        (*fail)++;
        return;
    }

    shared.array_val.items[0] = root;
    shared.array_val.length++;
    for (usz i = 0; i < count; i++) {
        root.array_val.items[i] = shared;
        root.array_val.length++;
    }

    bool unsafe = boundary_graph_alias_unsafe_for_reuse(root, interp);
    if (!unsafe) {
        emit_pass_literal("[PASS] lifetime: boundary shared composite cycle payload reuse");
        (*pass)++;
    } else {
        io::printn("[FAIL] lifetime: boundary shared composite cycle payload reuse");
        (*fail)++;
    }
}

fn void run_memory_lifetime_boundary_instance_payload_alias_test(Interp* interp, int* pass, int* fail) {
    // Invariant: instance wrappers can live in a surviving scope while their
    // field graph still carries stale releasing-scope payloads after mutation
    // or malformed construction. Alias reuse must reject that graph.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* target = main::scope_create(saved);
    main::ScopeRegion* releasing = target != null ? main::scope_create(target) : null;
    if (target == null || releasing == null) {
        if (releasing != null) main::scope_release(releasing);
        if (target != null) main::scope_release(target);
        io::printn("[FAIL] lifetime: boundary instance payload alias rejected");
        (*fail)++;
        return;
    }

    interp.current_scope = target;
    Value* field_seed = make_int(interp, 1);
    Value*[1] fields = { field_seed };
    Value* inst = make_instance(interp, (TypeId)1, &fields[0], 1);

    interp.current_scope = releasing;
    Value* stale_payload = make_array_checked(interp, 1, "instance payload alias: stale payload allocation failed");
    if (stale_payload != null && stale_payload.tag == ARRAY && stale_payload.array_val != null) {
        stale_payload.array_val.items[0] = make_int(interp, 99);
        stale_payload.array_val.length++;
    }

    bool setup_ok = inst != null &&
                    inst.tag == INSTANCE &&
                    inst.instance_val != null &&
                    inst.instance_val.fields != null &&
                    stale_payload != null &&
                    stale_payload.tag == ARRAY &&
                    stale_payload.array_val != null;
    if (setup_ok) {
        inst.instance_val.fields[0] = *stale_payload;
        interp.current_scope = target;
        interp.releasing_scope = releasing;
    }

    bool unsafe = setup_ok && boundary_graph_alias_unsafe_for_reuse(inst, interp);

    interp.releasing_scope = null;
    interp.current_scope = saved;
    main::scope_release(releasing);
    main::scope_release(target);

    if (unsafe) {
        emit_pass_literal("[PASS] lifetime: boundary instance payload alias rejected");
        (*pass)++;
    } else {
        io::printn("[FAIL] lifetime: boundary instance payload alias rejected");
        (*fail)++;
    }
}
