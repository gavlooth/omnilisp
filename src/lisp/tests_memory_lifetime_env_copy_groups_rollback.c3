module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_env_copy_self_referential_closure_payload_test(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy handles self-referential closure payloads through
    // closure memoization instead of recursive descent over the same wrapper.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy self-referential closure payload");
            (*fail)++;
            return;
        }

        SymbolId closure_sym = interp.symbols.intern("__copy_env_self_ref_closure");
        SymbolId self_ref_sym = interp.symbols.intern("__copy_env_self_ref_binding");

        interp.current_scope = source;
        Env* captured_env = make_env(interp, interp.global_env);
        Value* source_closure = captured_env != null ? make_closure_no_param(interp, null, captured_env) : null;
        if (source_closure != null && captured_env != null) {
            env_define_with_barrier(captured_env, self_ref_sym, source_closure, interp);
            env_define_with_barrier(captured_env, interp.symbols.intern("__copy_env_self_ref_payload"), make_int(interp, 999), interp);
        }

        Env* source_frame = make_env(interp, interp.global_env);
        bool setup_ok = source_frame != null &&
                        source_closure != null &&
                        source_closure.tag == CLOSURE &&
                        source_closure.closure_val != null &&
                        source_closure.closure_val.env == captured_env;
        if (setup_ok) {
            env_define_with_barrier(source_frame, closure_sym, source_closure, interp);
        }

        BoundaryEnvCopyResult copied =
            setup_ok ? boundary_copy_env_to_target_scope_checked(source_frame, interp, target)
                     : boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, 0);

        Value* copied_closure = boundary_env_copy_result_ok(copied) && copied.env != null
            ? copied.env.lookup(closure_sym)
            : null;
        Value* copied_self_ref =
            copied_closure != null && copied_closure.tag == CLOSURE && copied_closure.closure_val != null
                ? copied_closure.closure_val.env.lookup(self_ref_sym)
                : null;

        bool copied_ok = boundary_env_copy_result_ok(copied) &&
                         copied.env != null &&
                         copied_closure != null &&
                         copied_closure != source_closure &&
                         copied_closure.tag == CLOSURE &&
                         copied_closure.closure_val != null &&
                         copied_closure.closure_val.env != null &&
                         copied_closure.closure_val.env != captured_env &&
                         copied_self_ref != null &&
                         copied_self_ref == copied_closure &&
                         main::is_in_scope((void*)copied_closure, target);

        main::scope_release(source);
        Value* copied_after_release = copied.env != null ? copied.env.lookup(closure_sym) : null;
        Value* copied_after_release_self_ref =
            copied_after_release != null && copied_after_release.tag == CLOSURE && copied_after_release.closure_val != null
                ? copied_after_release.closure_val.env.lookup(self_ref_sym)
                : null;
        bool post_release_ok = copied_after_release != null &&
                               copied_after_release == copied_closure &&
                               copied_after_release_self_ref == copied_after_release;

        interp.current_scope = saved_scope;
        main::scope_release(target);

        bool ok = setup_ok && copied_ok && post_release_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy self-referential closure payload");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy self-referential closure payload (setup_ok=%s copied_ok=%s post_release_ok=%s fault=%s depth=%d)",
                setup_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                post_release_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                (long)copied.depth
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_transactional_binding_rollback_test(Interp* interp, int* pass, int* fail) {
    // Invariant: mid-frame env-copy failure rolls back already materialized
    // target-scope wrapper retain side effects (closure/iterator/instance).
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy transactional mid-frame rollback");
            (*fail)++;
            return;
        }

        SymbolId closure_sym = interp.symbols.intern("__copy_env_txn_closure");
        SymbolId iter_sym = interp.symbols.intern("__copy_env_txn_iterator");
        SymbolId inst_sym = interp.symbols.intern("__copy_env_txn_instance");
        SymbolId fail_sym = interp.symbols.intern("__copy_env_txn_fail");
        SymbolId payload_sym = interp.symbols.intern("__copy_env_txn_payload");

        interp.current_scope = source;

        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        Value* shared_closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 321), interp);
            shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
        }

        Value* iter_wrapper = shared_closure != null ? make_iterator(interp, shared_closure) : null;
        Value* inst_value = make_instance(interp, (TypeId)1, null, 0);
        main::ScopeRegion* inst_owner_scope =
            inst_value != null && inst_value.tag == INSTANCE ? inst_value.instance_val.owner_scope : null;

        Env* bad_capture = make_env(interp, interp.global_env);
        Value* bad_closure = bad_capture != null ? make_closure_no_param(interp, null, bad_capture) : null;

        Env* frame = make_env(interp, interp.global_env);
        bool setup_ok = frame != null &&
                        shared_closure != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        iter_wrapper != null &&
                        inst_value != null &&
                        inst_value.tag == INSTANCE &&
                        inst_owner_scope != null &&
                        inst_owner_scope.refcount == 1 &&
                        bad_closure != null &&
                        bad_closure.tag == CLOSURE &&
                        bad_closure.closure_val != null &&
                        bad_closure.closure_val.env_scope == null;

        if (setup_ok) {
            env_define_with_barrier(frame, closure_sym, shared_closure, interp);
            env_define_with_barrier(frame, iter_sym, iter_wrapper, interp);
            env_define_with_barrier(frame, inst_sym, inst_value, interp);
            env_define_with_barrier(frame, fail_sym, bad_closure, interp);
        }

        BoundaryEnvCopyResult copied =
            setup_ok ? boundary_copy_env_to_target_scope_checked(frame, interp, target)
                     : boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, 0);

        int closure_rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        int inst_owner_rc_after_fail = inst_owner_scope != null ? (int)inst_owner_scope.refcount : -1;

        bool fault_ok = copied.env == null &&
                        copied.fault_code == BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY &&
                        copied.depth == 0;
        bool rollback_ok = closure_rc_after_fail == 1 && inst_owner_rc_after_fail == 1;
        bool state_ok = interp.current_scope == source;

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(target);

        bool ok = setup_ok && fault_ok && rollback_ok && state_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy transactional mid-frame rollback");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy transactional mid-frame rollback (setup_ok=%s fault=%s depth=%d closure_rc=%d instance_rc=%d state_ok=%s)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                (long)copied.depth,
                closure_rc_after_fail,
                inst_owner_rc_after_fail,
                state_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_failure_invalidates_promotion_ctx_test(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy rollback invalidates a shared promotion context so
    // stale memoized target values cannot be reused after failure.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy rollback invalidates promotion ctx");
            (*fail)++;
            return;
        }

        SymbolId closure_sym = interp.symbols.intern("__copy_env_ctx_closure");
        SymbolId fail_sym = interp.symbols.intern("__copy_env_ctx_fail");
        SymbolId payload_sym = interp.symbols.intern("__copy_env_ctx_payload");

        interp.current_scope = source;
        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        Value* shared_closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 333), interp);
            shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
        }

        Env* bad_capture = make_env(interp, interp.global_env);
        Value* bad_closure = bad_capture != null ? make_closure_no_param(interp, null, bad_capture) : null;

        Env* frame = make_env(interp, interp.global_env);
        bool setup_ok = frame != null &&
                        shared_closure != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        bad_closure != null &&
                        bad_closure.tag == CLOSURE &&
                        bad_closure.closure_val != null &&
                        bad_closure.closure_val.env_scope == null;

        if (setup_ok) {
            env_define_with_barrier(frame, closure_sym, shared_closure, interp);
            env_define_with_barrier(frame, fail_sym, bad_closure, interp);
        }

        PromotionContext ctx = {};
        boundary_promotion_context_begin(interp, &ctx);
        BoundaryEnvCopyResult first =
            setup_ok ? boundary_copy_env_to_target_scope_checked(frame, interp, target, 0, &ctx)
                     : boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, 0);
        Value* cached_after_fail = promotion_context_lookup(&ctx, shared_closure);
        BoundaryEnvCopyResult retry =
            setup_ok ? boundary_copy_env_to_target_scope_checked(frame, interp, target, 0, &ctx)
                     : boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, 0);
        boundary_promotion_context_end(interp, &ctx);

        int closure_rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        bool first_fault = first.env == null &&
                           first.fault_code == BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY &&
                           first.depth == 0;
        bool ctx_invalidated = ctx.aborted &&
                               ctx.abort_reason == PROMOTION_ABORT_INVALIDATED_BY_ROLLBACK &&
                               ctx.memo_head == null &&
                               cached_after_fail == null;
        bool retry_fault = retry.env == null &&
                           retry.fault_code == BOUNDARY_ENV_COPY_FAULT_CONTEXT_ABORTED &&
                           retry.depth == 0;
        bool rollback_ok = closure_rc_after_fail == 1;

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(target);

        bool ok = setup_ok && first_fault && ctx_invalidated && retry_fault && rollback_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy rollback invalidates promotion ctx");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy rollback invalidates promotion ctx (setup_ok=%s first_fault=%s ctx_invalidated=%s retry_fault=%s rollback_ok=%s first_fault_name=%s retry_fault_name=%s abort_reason=%d closure_rc=%d)",
                setup_ok ? "yes" : "no",
                first_fault ? "yes" : "no",
                ctx_invalidated ? "yes" : "no",
                retry_fault ? "yes" : "no",
                rollback_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(first.fault_code),
                (ZString)boundary_env_copy_fault_name(retry.fault_code),
                (int)ctx.abort_reason,
                closure_rc_after_fail
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_rollback_preserves_shared_iterator_payload_test(Interp* interp, int* pass, int* fail) {
    // Invariant: rollback must not destruct a shared iterator payload closure
    // that was merely aliased into the copied iterator wrapper.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved_scope);
        main::ScopeRegion* source = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy rollback preserves shared iterator payload");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__copy_env_shared_iterator_payload");
        SymbolId iter_sym = interp.symbols.intern("__copy_env_shared_iterator");
        SymbolId fail_sym = interp.symbols.intern("__copy_env_shared_iterator_fail");

        interp.current_scope = target;
        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        Value* shared_closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 654), interp);
            shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
        }

        interp.current_scope = source;
        Value* iter_wrapper = shared_closure != null ? make_iterator(interp, shared_closure) : null;
        Env* bad_capture = make_env(interp, interp.global_env);
        Value* bad_closure = bad_capture != null ? make_closure_no_param(interp, null, bad_capture) : null;

        Env* frame = make_env(interp, interp.global_env);
        bool setup_ok = frame != null &&
                        shared_closure != null &&
                        shared_closure.tag == CLOSURE &&
                        shared_closure.closure_val != null &&
                        shared_closure.closure_val.env_scope == closure_env_scope &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        iter_wrapper != null &&
                        bad_closure != null &&
                        bad_closure.tag == CLOSURE &&
                        bad_closure.closure_val != null &&
                        bad_closure.closure_val.env_scope == null;

        if (setup_ok) {
            env_define_with_barrier(frame, iter_sym, iter_wrapper, interp);
            env_define_with_barrier(frame, fail_sym, bad_closure, interp);
        }

        BoundaryEnvCopyResult copied =
            setup_ok ? boundary_copy_env_to_target_scope_checked(frame, interp, target)
                     : boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, 0);

        int rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        bool fault_ok = copied.env == null &&
                        copied.fault_code == BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY &&
                        copied.depth == 0;
        bool shared_ok = shared_closure != null &&
                         shared_closure.tag == CLOSURE &&
                         shared_closure.closure_val != null &&
                         shared_closure.closure_val.env_scope == closure_env_scope &&
                         closure_env_lookup_int(shared_closure, payload_sym, 654) &&
                         rc_after_fail == 1;

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(target);

        bool ok = setup_ok && fault_ok && shared_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy rollback preserves shared iterator payload");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy rollback preserves shared iterator payload (setup_ok=%s fault=%s rc_after_fail=%d shared_ok=%s)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                rc_after_fail,
                shared_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_iterator_wrapper_failure_rollback_test(Interp* interp, int* pass, int* fail) {
    // Invariant: iterator env-copy self-cleans any already-copied inner payload
    // when the outer iterator wrapper cannot be materialized.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved_scope);
        main::ScopeRegion* source = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy iterator wrapper failure rollback");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__copy_env_iterator_wrapper_fail_payload");
        SymbolId iter_sym = interp.symbols.intern("__copy_env_iterator_wrapper_fail_iter");

        interp.current_scope = source;
        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        Value* shared_closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 777), interp);
            shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
        }

        Value* partial = null;
        Value* iter_wrapper = null;
        Env* frame = make_env(interp, interp.global_env);
        bool setup_ok = frame != null &&
                        shared_closure != null &&
                        shared_closure.tag == CLOSURE &&
                        shared_closure.closure_val != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1;
        if (setup_ok) {
            partial = interp.alloc_value();
            partial.tag = PARTIAL_PRIM;
            partial.partial_val.func = &prim_add;
            partial.partial_val.remaining = 1;
            partial.partial_val.first_arg = shared_closure;
            partial.partial_val.second_arg = make_int(interp, 778);
            iter_wrapper = make_iterator(interp, partial);
            setup_ok = partial != null && iter_wrapper != null;
        }
        if (setup_ok) {
            env_define_with_barrier(frame, iter_sym, iter_wrapper, interp);
        }

        bool saved_force = g_copy_env_force_iterator_wrapper_alloc_fail;
        g_copy_env_force_iterator_wrapper_alloc_fail = true;
        BoundaryEnvCopyResult copied =
            setup_ok ? boundary_copy_env_to_target_scope_checked(frame, interp, target)
                     : boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, 0);
        g_copy_env_force_iterator_wrapper_alloc_fail = saved_force;

        int rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        bool fault_ok = copied.env == null &&
                        copied.fault_code == BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY &&
                        copied.depth == 0;
        bool rollback_ok = rc_after_fail == 1 &&
                           closure_env_lookup_int(shared_closure, payload_sym, 777);

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(target);

        bool ok = setup_ok && fault_ok && rollback_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy iterator wrapper failure rollback");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy iterator wrapper failure rollback (setup_ok=%s fault=%s rc_after_fail=%d rollback_ok=%s)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                rc_after_fail,
                rollback_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}
