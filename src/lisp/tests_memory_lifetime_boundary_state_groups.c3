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
        Value* fault_result = boundary_copy_to_parent_site_ctx(
            src,
            interp,
            COPY_SITE_GENERIC,
            null
        );

        interp.releasing_scope = saved_releasing;

        bool ok = !boundary_copy_result_ok(checked) &&
                  checked.value == null &&
                  checked.fault_code == BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE &&
                  fault_result != null &&
                  fault_result.tag == ERROR &&
                  str_starts_with(
                      fault_result.str_chars[:fault_result.str_len],
                      "boundary: copy_to_parent_site_ctx requires target/releasing scope provenance"
                  );

        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary copy fault classification");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary copy fault classification (ok_checked=%s checked_fault=%s result_tag=%d)",
                (!boundary_copy_result_ok(checked) && checked.fault_code == BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE) ? "yes" : "no",
                (ZString)boundary_copy_fault_name(checked.fault_code),
                fault_result != null ? (int)fault_result.tag : -1
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
