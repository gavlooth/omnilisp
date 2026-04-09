module lisp;

import std::io;
import main;

fn void run_memory_lifetime_boundary_transition_regression_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: return/env/splice boundary transitions produce deterministic outcomes.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target_scope = main::scope_create(saved_scope);
        main::ScopeRegion* releasing_scope = target_scope == null ? null : main::scope_create(target_scope);
        if (target_scope == null || releasing_scope == null) {
            if (releasing_scope != null) main::scope_release(releasing_scope);
            if (target_scope != null) main::scope_release(target_scope);
            io::printn("[FAIL] lifetime: boundary transition regression pack");
            (*fail)++;
            return;
        }

        // Return transition: budget-0 commit must deterministically hard-fail with promotion-aborted fault.
        interp.current_scope = releasing_scope;
        Value* return_src = make_cons(interp, make_int(interp, 11), make_nil(interp));
        PromotionContext return_ctx = {};
        boundary_promotion_context_begin(interp, &return_ctx, 0);
        BoundaryCommitEscapeResult committed = boundary_commit_escape(
            return_src,
            interp,
            target_scope,
            releasing_scope,
            &return_ctx
        );
        boundary_promotion_context_end(interp, &return_ctx);
        bool return_ok = committed.outcome == BOUNDARY_COMMIT_FALLBACK_DISALLOWED &&
                         committed.fault_code == BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED &&
                         committed.value != null &&
                         committed.value.tag == ERROR;

        // Env transition: depth-limit path must deterministically report depth fault.
        interp.current_scope = releasing_scope;
        Env* env_src = make_env(interp, interp.global_env);
        SymbolId env_sym = interp.symbols.intern("__boundary_transition_env");
        env_define_with_barrier(env_src, env_sym, make_int(interp, 22), interp);
        BoundaryEnvCopyResult env_copied = boundary_copy_env_to_target_scope_checked(
            env_src,
            interp,
            target_scope,
            256
        );
        bool env_ok = !boundary_env_copy_result_ok(env_copied) &&
                      env_copied.fault_code == BOUNDARY_ENV_COPY_FAULT_DEPTH_LIMIT;

        // Splice transition: non-unique child refcount must deterministically reject with explicit reason.
        main::scope_retain(releasing_scope);
        BoundaryScopeTransferResult splice_result =
            boundary_try_scope_splice_escapes_result(target_scope, releasing_scope);
        bool splice_ok = !splice_result.allowed &&
                         splice_result.reason == BOUNDARY_SCOPE_TRANSFER_CHILD_REFCOUNT_NOT_ONE;
        main::scope_release(releasing_scope);

        interp.current_scope = saved_scope;
        main::scope_release(releasing_scope);
        main::scope_release(target_scope);

        bool ok = return_ok && env_ok && splice_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary transition regression pack");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary transition regression pack (return_ok=%s env_ok=%s splice_ok=%s env_fault=%s splice_reason=%s commit_fault=%s)",
                return_ok ? "yes" : "no",
                env_ok ? "yes" : "no",
                splice_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(env_copied.fault_code),
                (ZString)boundary_scope_transfer_reason_name(splice_result.reason),
                (ZString)boundary_commit_fault_name(committed.fault_code)
            );
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

    // Invariant: primitive wrappers already in target chain are reused, while
    // disjoint plain primitive wrappers are cloned and opaque payloads fail as
    // typed boundary-copy faults.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || releasing == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: primitive wrapper copy policy");
            (*fail)++;
            return;
        }

        interp.current_scope = target;
        Value* reusable = interp.alloc_value();
        reusable.tag = PRIMITIVE;
        main::scope_register_dtor(target, (void*)reusable, &scope_dtor_value);
        Primitive* reusable_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        if (reusable_prim == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: primitive wrapper copy policy (setup reusable)");
            (*fail)++;
            return;
        }
        *reusable_prim = {};
        reusable_prim.name[0] = 'r';
        reusable_prim.name[1] = 0;
        reusable_prim.arity = 1;
        reusable_prim.tag = 601;
        reusable.prim_val = reusable_prim;

        interp.current_scope = foreign;
        Value* disjoint = interp.alloc_value();
        disjoint.tag = PRIMITIVE;
        main::scope_register_dtor(foreign, (void*)disjoint, &scope_dtor_value);
        Primitive* disjoint_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        if (disjoint_prim == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: primitive wrapper copy policy (setup disjoint)");
            (*fail)++;
            return;
        }
        *disjoint_prim = {};
        disjoint_prim.name[0] = 'd';
        disjoint_prim.name[1] = 0;
        disjoint_prim.arity = 2;
        disjoint_prim.tag = 602;
        disjoint.prim_val = disjoint_prim;

        Value* opaque = interp.alloc_value();
        opaque.tag = PRIMITIVE;
        main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)mem::malloc(int.sizeof);
        if (opaque_prim == null || opaque_payload == null) {
            if (opaque_prim != null) mem::free(opaque_prim);
            if (opaque_payload != null) mem::free(opaque_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: primitive wrapper copy policy (setup opaque)");
            (*fail)++;
            return;
        }
        *opaque_prim = {};
        *opaque_payload = 603;
        opaque_prim.name[0] = 'o';
        opaque_prim.name[1] = 0;
        opaque_prim.arity = 3;
        opaque_prim.tag = 603;
        opaque_prim.user_data = (void*)opaque_payload;
        opaque.prim_val = opaque_prim;

        interp.current_scope = target;
        interp.releasing_scope = releasing;

        Value* reused = copy_to_parent(reusable, interp);
        Value* copied = copy_to_parent(disjoint, interp);
        BoundaryCopyResult rejected = boundary_copy_to_parent_site_ctx_checked(
            opaque,
            interp,
            COPY_SITE_GENERIC,
            null
        );

        bool reuse_ok = reused == reusable;
        bool copied_ok = copied != null &&
                         copied.tag == PRIMITIVE &&
                         copied != disjoint &&
                         copied.prim_val != null &&
                         copied.prim_val != disjoint.prim_val &&
                         copied.prim_val.arity == 2 &&
                         copied.prim_val.tag == 602 &&
                         copied.prim_val.name[0] == 'd' &&
                         copied.prim_val.name[1] == 0;
        bool reject_ok = !boundary_copy_result_ok(rejected) &&
                         rejected.value == null &&
                         rejected.fault_code == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;

        main::scope_release(foreign);

        bool post_ok = copied != null &&
                       copied.tag == PRIMITIVE &&
                       copied.prim_val != null &&
                       copied.prim_val.arity == 2 &&
                       copied.prim_val.tag == 602 &&
                       copied.prim_val.name[0] == 'd' &&
                       copied.prim_val.name[1] == 0;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(releasing);
        main::scope_release(target);

        bool ok = reuse_ok && copied_ok && reject_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: primitive wrapper copy policy");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: primitive wrapper copy policy (reuse_ok=%s copied_ok=%s reject_ok=%s post_ok=%s fault=%s)",
                reuse_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                reject_ok ? "yes" : "no",
                post_ok ? "yes" : "no",
                (ZString)boundary_copy_fault_name(rejected.fault_code));
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
