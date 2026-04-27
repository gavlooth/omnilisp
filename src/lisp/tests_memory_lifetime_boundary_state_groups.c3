module lisp;

import std::io;
import std::thread;
import main;

const usz BOUNDARY_TELEMETRY_THREAD_WORKERS = 8;
const usz BOUNDARY_TELEMETRY_THREAD_ITERS = 256;
const usz BOUNDARY_TELEMETRY_THREAD_NODES = 2;
const usz BOUNDARY_TELEMETRY_THREAD_COPY_BYTES = 32;
const usz BOUNDARY_TELEMETRY_THREAD_ARRAY_CAP = 8;
const usz BOUNDARY_TELEMETRY_THREAD_ARRAY_GROW = 16;
const usz BOUNDARY_TELEMETRY_THREAD_STRING_BYTES = 11;
const usz BOUNDARY_TELEMETRY_THREAD_ENV_DEPTH = 3;
const usz BOUNDARY_TELEMETRY_THREAD_ENV_BINDINGS = 5;

struct BoundaryTelemetryThreadState {
    bool ok;
}

fn usz boundary_telemetry_test_delta(usz after, usz before) @inline {
    return after >= before ? after - before : 0;
}

fn BoundaryCommitEscapeResult boundary_telemetry_test_commit() @inline {
    BoundaryCommitEscapeResult committed = {};
    committed.planned_route = BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION;
    committed.planned_reason = BOUNDARY_PLAN_REASON_RELEASING_TEMP_STABLE_MATERIALIZATION;
    committed.planned_value_tag = CONS;
    committed.selected_route = BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION;
    committed.selected_reason = BOUNDARY_PLAN_REASON_DESTINATION_COMMITTED_COMPATIBILITY;
    committed.selected_value_tag = CONS;
    return committed;
}

fn int boundary_telemetry_thread_worker(void* arg) {
    BoundaryTelemetryThreadState* state = (BoundaryTelemetryThreadState*)arg;
    if (state != null) state.ok = true;
    BoundaryCommitEscapeResult committed = boundary_telemetry_test_commit();
    for (usz i = 0; i < BOUNDARY_TELEMETRY_THREAD_ITERS; i++) {
        boundary_note_commit_decision(committed);
        boundary_note_materialization_copy_debt_by_tag(
            CONS,
            BOUNDARY_TELEMETRY_THREAD_NODES,
            BOUNDARY_TELEMETRY_THREAD_COPY_BYTES,
            true
        );
        boundary_value_shape_note_array_construct(BOUNDARY_TELEMETRY_THREAD_ARRAY_CAP);
        boundary_value_shape_note_array_growth(
            BOUNDARY_TELEMETRY_THREAD_ARRAY_CAP,
            BOUNDARY_TELEMETRY_THREAD_ARRAY_GROW
        );
        boundary_value_shape_note_string_payload(BOUNDARY_TELEMETRY_THREAD_STRING_BYTES);
        boundary_value_shape_note_env_copy_frame(
            BOUNDARY_TELEMETRY_THREAD_ENV_DEPTH,
            BOUNDARY_TELEMETRY_THREAD_ENV_BINDINGS
        );
    }
    return 0;
}

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

fn void run_memory_lifetime_boundary_list_child_scope_alias_test(Interp* interp, int* pass, int* fail) {
    // Invariant: releasing-scope list children must be rejected even when the
    // child itself is not a CONS shell.
    main::ScopeRegion* saved = interp.current_scope;
    main::ScopeRegion* target = main::scope_create(saved);
    main::ScopeRegion* releasing = target != null ? main::scope_create(target) : null;
    if (target == null || releasing == null) {
        if (releasing != null) main::scope_release(releasing);
        if (target != null) main::scope_release(target);
        io::printn("[FAIL] lifetime: boundary list child scope alias rejected");
        (*fail)++;
        return;
    }

    Value* root = null;
    Value* stale_payload = null;
    bool setup_ok = false;

    interp.current_scope = target;
    root = make_cons(interp, make_nil(interp), make_nil(interp));
    interp.current_scope = releasing;
    stale_payload = make_array_checked(interp, 1, "boundary list child scope alias: stale payload allocation failed");
    if (root != null && root.tag == CONS &&
        stale_payload != null && stale_payload.tag == ARRAY && stale_payload.array_val != null) {
        stale_payload.array_val.items[0] = make_int(interp, 7);
        stale_payload.array_val.length++;
        interp.current_scope = target;
        root.cons_val.car = stale_payload;
        setup_ok = true;
    }
    interp.current_scope = saved;

    interp.current_scope = target;
    interp.releasing_scope = releasing;
    bool unsafe = setup_ok && boundary_graph_alias_unsafe_for_reuse(root, interp);
    interp.releasing_scope = null;
    interp.current_scope = saved;

    main::scope_release(releasing);
    main::scope_release(target);

    if (unsafe) {
        emit_pass_literal("[PASS] lifetime: boundary list child scope alias rejected");
        (*pass)++;
    } else {
        io::printn("[FAIL] lifetime: boundary list child scope alias rejected");
        (*fail)++;
    }
}

fn void run_memory_lifetime_boundary_telemetry_saturation_tests(Interp* interp, int* pass, int* fail) {
    (void)interp;
    if (!boundary_telemetry_enabled()) {
        emit_pass_literal("[PASS] lifetime: boundary telemetry saturation skipped when counters disabled");
        (*pass)++;
        return;
    }

    BoundaryDecisionStats saved_decisions = boundary_decision_stats_snapshot();
    BoundaryValueShapeStats saved_shapes = boundary_value_shape_stats_snapshot();
    defer boundary_restore_decision_stats(saved_decisions);
    defer boundary_restore_value_shape_stats(saved_shapes);

    BoundaryDecisionStats decision_seed = {};
    decision_seed.copy_fallback_total = usz.max - (usz)1;
    decision_seed.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION] = usz.max - (usz)1;
    decision_seed.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION] = usz.max - (usz)1;
    decision_seed.planned_route_by_tag[boundary_decision_route_tag_index(BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, ValueTag.CONS)] = usz.max - (usz)1;
    decision_seed.selected_route_by_tag[boundary_decision_route_tag_index(BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION, ValueTag.CONS)] = usz.max - (usz)1;
    decision_seed.stable_materialization_node_count_total = usz.max - (usz)1;
    decision_seed.stable_materialization_copy_bytes_total = usz.max - (usz)1;
    decision_seed.stable_materialization_node_count_by_tag[(usz)ValueTag.CONS] = usz.max - (usz)1;
    decision_seed.stable_materialization_copy_bytes_by_tag[(usz)ValueTag.CONS] = usz.max - (usz)1;
    decision_seed.forced_no_splice_materialization_node_count_total = usz.max - (usz)1;
    decision_seed.forced_no_splice_materialization_copy_bytes_total = usz.max - (usz)1;
    decision_seed.forced_no_splice_materialization_node_count_by_tag[(usz)ValueTag.CONS] = usz.max - (usz)1;
    decision_seed.forced_no_splice_materialization_copy_bytes_by_tag[(usz)ValueTag.CONS] = usz.max - (usz)1;
    boundary_restore_decision_stats(decision_seed);

    BoundaryValueShapeStats shape_seed = {};
    shape_seed.array_capacity_total = usz.max - (usz)1;
    shape_seed.array_growth_old_capacity_total = usz.max - (usz)1;
    shape_seed.array_growth_new_capacity_total = usz.max - (usz)1;
    shape_seed.string_payload_bytes_total = usz.max - (usz)1;
    shape_seed.closure_env_copy_depth_total = usz.max - (usz)1;
    shape_seed.closure_env_copy_binding_count_total = usz.max - (usz)1;
    boundary_restore_value_shape_stats(shape_seed);

    BoundaryCommitEscapeResult committed = boundary_telemetry_test_commit();
    boundary_note_commit_decision(committed);
    boundary_note_materialization_copy_debt_by_tag(ValueTag.CONS, 2, 2, true);
    boundary_value_shape_note_array_construct(8);
    boundary_value_shape_note_array_growth(3, 21);
    boundary_value_shape_note_string_payload(13);
    boundary_value_shape_note_env_copy_frame(5, 7);

    BoundaryDecisionStats decisions = boundary_decision_stats_snapshot();
    BoundaryValueShapeStats shapes = boundary_value_shape_stats_snapshot();
    bool ok =
        decisions.copy_fallback_total == usz.max &&
        decisions.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION] == usz.max &&
        decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION] == usz.max &&
        decisions.planned_route_by_tag[boundary_decision_route_tag_index(BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION, ValueTag.CONS)] == usz.max &&
        decisions.selected_route_by_tag[boundary_decision_route_tag_index(BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION, ValueTag.CONS)] == usz.max &&
        decisions.stable_materialization_node_count_total == usz.max &&
        decisions.stable_materialization_copy_bytes_total == usz.max &&
        decisions.stable_materialization_node_count_by_tag[(usz)ValueTag.CONS] == usz.max &&
        decisions.stable_materialization_copy_bytes_by_tag[(usz)ValueTag.CONS] == usz.max &&
        decisions.forced_no_splice_materialization_node_count_total == usz.max &&
        decisions.forced_no_splice_materialization_copy_bytes_total == usz.max &&
        decisions.forced_no_splice_materialization_node_count_by_tag[(usz)ValueTag.CONS] == usz.max &&
        decisions.forced_no_splice_materialization_copy_bytes_by_tag[(usz)ValueTag.CONS] == usz.max &&
        shapes.array_capacity_total == usz.max &&
        shapes.array_growth_old_capacity_total == usz.max &&
        shapes.array_growth_new_capacity_total == usz.max &&
        shapes.string_payload_bytes_total == usz.max &&
        shapes.closure_env_copy_depth_total == usz.max &&
        shapes.closure_env_copy_binding_count_total == usz.max &&
        shapes.array_capacity_peak == 8 &&
        shapes.array_growth_new_capacity_peak == 21 &&
        shapes.string_payload_bytes_peak == 13 &&
        shapes.closure_env_copy_depth_peak == 5 &&
        shapes.closure_env_copy_binding_count_peak == 7;

    if (ok) {
        emit_pass_literal("[PASS] lifetime: boundary telemetry counters saturate");
        (*pass)++;
    } else {
        io::printn("[FAIL] lifetime: boundary telemetry counters saturate");
        (*fail)++;
    }
}

fn void run_memory_lifetime_boundary_threaded_telemetry_tests(Interp* interp, int* pass, int* fail) {
    (void)interp;
    if (!boundary_telemetry_enabled()) {
        emit_pass_literal("[PASS] lifetime: threaded boundary telemetry skipped when counters disabled");
        (*pass)++;
        return;
    }

    BoundaryDecisionStats saved_decisions = boundary_decision_stats_snapshot();
    BoundaryValueShapeStats saved_shapes = boundary_value_shape_stats_snapshot();
    defer boundary_restore_decision_stats(saved_decisions);
    defer boundary_restore_value_shape_stats(saved_shapes);

    BoundaryDecisionStats zero_decisions = {};
    BoundaryValueShapeStats zero_shapes = {};
    boundary_restore_decision_stats(zero_decisions);
    boundary_restore_value_shape_stats(zero_shapes);

    Thread[BOUNDARY_TELEMETRY_THREAD_WORKERS] threads;
    BoundaryTelemetryThreadState[BOUNDARY_TELEMETRY_THREAD_WORKERS] states;
    bool[BOUNDARY_TELEMETRY_THREAD_WORKERS] created;
    bool ok = true;

    foreach (i, &t : threads) {
        states[i].ok = false;
        created[i] = false;
        if (catch err = t.create(&boundary_telemetry_thread_worker, (void*)&states[i])) {
            (void)err;
            ok = false;
        } else {
            created[i] = true;
        }
    }

    foreach (i, &t : threads) {
        if (!created[i]) continue;
        if (catch err = t.join()) {
            (void)err;
            ok = false;
        }
        ok = ok && states[i].ok;
    }

    BoundaryDecisionStats decisions = boundary_decision_stats_snapshot();
    BoundaryValueShapeStats shapes = boundary_value_shape_stats_snapshot();
    usz expected_ops = BOUNDARY_TELEMETRY_THREAD_WORKERS * BOUNDARY_TELEMETRY_THREAD_ITERS;
    ok = ok &&
        boundary_telemetry_test_delta(decisions.copy_fallback_total, (usz)0) == expected_ops &&
        boundary_telemetry_test_delta(decisions.planned_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_STABLE_MATERIALIZE_DESTINATION], (usz)0) == expected_ops &&
        boundary_telemetry_test_delta(decisions.selected_route_by_route[(usz)BoundaryPlanRoute.BOUNDARY_PLAN_ROUTE_COMPATIBILITY_DESTINATION], (usz)0) == expected_ops &&
        boundary_telemetry_test_delta(decisions.stable_materialization_node_count_total, (usz)0) == expected_ops * BOUNDARY_TELEMETRY_THREAD_NODES &&
        boundary_telemetry_test_delta(decisions.stable_materialization_copy_bytes_total, (usz)0) == expected_ops * BOUNDARY_TELEMETRY_THREAD_COPY_BYTES &&
        boundary_telemetry_test_delta(decisions.forced_no_splice_materialization_node_count_total, (usz)0) == expected_ops * BOUNDARY_TELEMETRY_THREAD_NODES &&
        boundary_telemetry_test_delta(decisions.forced_no_splice_materialization_copy_bytes_total, (usz)0) == expected_ops * BOUNDARY_TELEMETRY_THREAD_COPY_BYTES &&
        boundary_telemetry_test_delta(shapes.array_construct_count, (usz)0) == expected_ops &&
        boundary_telemetry_test_delta(shapes.array_capacity_total, (usz)0) == expected_ops * BOUNDARY_TELEMETRY_THREAD_ARRAY_CAP &&
        shapes.array_capacity_peak == BOUNDARY_TELEMETRY_THREAD_ARRAY_CAP &&
        boundary_telemetry_test_delta(shapes.array_growth_count, (usz)0) == expected_ops &&
        boundary_telemetry_test_delta(shapes.array_growth_old_capacity_total, (usz)0) == expected_ops * BOUNDARY_TELEMETRY_THREAD_ARRAY_CAP &&
        boundary_telemetry_test_delta(shapes.array_growth_new_capacity_total, (usz)0) == expected_ops * BOUNDARY_TELEMETRY_THREAD_ARRAY_GROW &&
        shapes.array_growth_new_capacity_peak == BOUNDARY_TELEMETRY_THREAD_ARRAY_GROW &&
        boundary_telemetry_test_delta(shapes.string_payload_count, (usz)0) == expected_ops &&
        boundary_telemetry_test_delta(shapes.string_payload_bytes_total, (usz)0) == expected_ops * BOUNDARY_TELEMETRY_THREAD_STRING_BYTES &&
        shapes.string_payload_bytes_peak == BOUNDARY_TELEMETRY_THREAD_STRING_BYTES &&
        boundary_telemetry_test_delta(shapes.closure_env_copy_frame_count, (usz)0) == expected_ops &&
        boundary_telemetry_test_delta(shapes.closure_env_copy_depth_total, (usz)0) == expected_ops * BOUNDARY_TELEMETRY_THREAD_ENV_DEPTH &&
        boundary_telemetry_test_delta(shapes.closure_env_copy_binding_count_total, (usz)0) == expected_ops * BOUNDARY_TELEMETRY_THREAD_ENV_BINDINGS &&
        shapes.closure_env_copy_depth_peak == BOUNDARY_TELEMETRY_THREAD_ENV_DEPTH &&
        shapes.closure_env_copy_binding_count_peak == BOUNDARY_TELEMETRY_THREAD_ENV_BINDINGS;

    if (ok) {
        emit_pass_literal("[PASS] lifetime: threaded boundary telemetry counters");
        (*pass)++;
    } else {
        io::printn("[FAIL] lifetime: threaded boundary telemetry counters");
        (*fail)++;
    }
}

fn void run_memory_lifetime_boundary_telemetry_counter_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_boundary_telemetry_saturation_tests(interp, pass, fail);
    run_memory_lifetime_boundary_threaded_telemetry_tests(interp, pass, fail);
}
