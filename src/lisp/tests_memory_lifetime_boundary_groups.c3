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
            noop = boundary_copy_to_parent_site(null, interp, COPY_SITE_COPY_ENV_TO_SCOPE);
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
        Value* legacy = boundary_copy_to_parent_site_ctx(
            src,
            interp,
            COPY_SITE_GENERIC,
            null
        );

        interp.releasing_scope = saved_releasing;

        bool ok = !boundary_copy_result_ok(checked) &&
                  checked.value == null &&
                  checked.fault_code == BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE &&
                  legacy != null &&
                  legacy.tag == ERROR &&
                  str_starts_with(
                      legacy.str_chars[:legacy.str_len],
                      "boundary: copy_to_parent_site_ctx requires target/releasing scope provenance"
                  );

        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary copy fault classification");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary copy fault classification (ok_checked=%s checked_fault=%s legacy_tag=%d)",
                (!boundary_copy_result_ok(checked) && checked.fault_code == BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE) ? "yes" : "no",
                (ZString)boundary_copy_fault_name(checked.fault_code),
                legacy != null ? (int)legacy.tag : -1
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
