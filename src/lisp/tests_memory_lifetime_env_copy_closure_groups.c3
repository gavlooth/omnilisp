module lisp;

import std::io;
import main;

fn Value* make_env_scope_closure_for_test(
    Interp* interp,
    Env* captured_env,
    main::ScopeRegion** out_env_scope
) {
    Value* closure = make_closure_no_param(interp, null, captured_env);
    if (closure == null) return null;

    main::ScopeRegion* env_scope = main::scope_create(null);
    if (env_scope == null) return null;

    Env* copied_env = boundary_copy_env_to_target_scope(captured_env, interp, env_scope);
    if (copied_env == null) {
        main::scope_release(env_scope);
        return null;
    }

    closure.closure_val.env = copied_env;
    closure.closure_val.env_scope = env_scope;
    if (out_env_scope != null) *out_env_scope = env_scope;
    return closure;
}

fn bool closure_env_lookup_int(Value* closure, SymbolId sym, long expected) @inline {
    if (closure == null || closure.tag != CLOSURE || closure.closure_val.env == null) return false;
    Value* found = closure.closure_val.env.lookup(sym);
    return found != null && found.tag == INT && found.int_val == expected;
}

fn void run_memory_lifetime_copy_parent_closure_env_scope_retain_test(Interp* interp, int* pass, int* fail) {
    // Invariant: copy_to_parent clones retain standalone closure env scopes via
    // scope_retain and release them through scope_dtor_closure symmetry.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing_scope = interp.releasing_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            interp.current_scope = saved_scope;
            interp.releasing_scope = saved_releasing_scope;
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: copy_to_parent closure env_scope retain symmetry");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__copy_parent_closure_payload");

        interp.current_scope = source;
        Env* captured_env = make_env(interp, interp.global_env);
        env_define_with_barrier(captured_env, payload_sym, make_int(interp, 77), interp);

        main::ScopeRegion* closure_env_scope = null;
        Value* closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
        bool setup_ok = closure != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1;

        interp.current_scope = target;
        interp.releasing_scope = source;
        Value* copied = setup_ok ? copy_to_parent(closure, interp) : null;
        interp.releasing_scope = saved_releasing_scope;

        bool copied_ok = copied != null &&
                         copied != closure &&
                         copied.tag == CLOSURE &&
                         copied.closure_val.env_scope == closure_env_scope &&
                         closure_env_scope != null &&
                         closure_env_scope.refcount == 2;

        int rc_before_release = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        main::scope_release(source);

        int rc_after_release = -1;
        bool post_release_ok = false;
        if (copied_ok) {
            rc_after_release = (int)closure_env_scope.refcount;
            post_release_ok = rc_after_release == 1 &&
                              closure_env_lookup_int(copied, payload_sym, 77);
        }

        interp.current_scope = saved_scope;
        main::scope_release(target);

        bool ok = setup_ok && copied_ok && post_release_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: copy_to_parent closure env_scope retain symmetry");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: copy_to_parent closure env_scope retain symmetry (setup_ok=%s copied_ok=%s post_release_ok=%s rc_before_release=%d rc_after_release=%d)",
                setup_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                post_release_ok ? "yes" : "no",
                rc_before_release,
                rc_after_release);
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_copy_parent_closure_wrapper_alloc_fail_test(Interp* interp, int* pass, int* fail) {
    // Invariant: copy_to_parent fails closed on closure wrapper allocation
    // failure and does not retain detached closure env scopes.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing_scope = interp.releasing_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            interp.current_scope = saved_scope;
            interp.releasing_scope = saved_releasing_scope;
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: copy_to_parent closure wrapper alloc fail");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__copy_parent_closure_wrapper_alloc_payload");

        interp.current_scope = source;
        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        int closure_env_destroy_count = 0;
        Value* closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 179), interp);
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

        interp.current_scope = target;
        interp.releasing_scope = source;
        bool saved_force = g_boundary_force_copy_closure_wrapper_alloc_fail;
        g_boundary_force_copy_closure_wrapper_alloc_fail = true;
        Value* copied = setup_ok ? copy_to_parent(closure, interp) : null;
        g_boundary_force_copy_closure_wrapper_alloc_fail = saved_force;
        interp.releasing_scope = saved_releasing_scope;

        int rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        bool fault_ok = copied == null;
        bool rollback_ok = rc_after_fail == 1 &&
                           closure_env_lookup_int(closure, payload_sym, 179);

        interp.current_scope = saved_scope;
        main::scope_release(target);
        int rc_after_target_release = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        int destroy_count_after_target_release = closure_env_destroy_count;
        bool teardown_ok = rc_after_target_release == 1 &&
                           destroy_count_after_target_release == 0;

        main::scope_release(source);
        int destroy_count_after_source_release = closure_env_destroy_count;
        bool owner_release_ok = destroy_count_after_source_release == 1;

        bool ok = setup_ok && fault_ok && rollback_ok && teardown_ok && owner_release_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: copy_to_parent closure wrapper alloc fail");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: copy_to_parent closure wrapper alloc fail (setup_ok=%s fault_ok=%s rc_after_fail=%d rc_after_target_release=%d destroy_count_after_target_release=%d destroy_count_after_source_release=%d rollback_ok=%s)",
                setup_ok ? "yes" : "no",
                fault_ok ? "yes" : "no",
                rc_after_fail,
                rc_after_target_release,
                destroy_count_after_target_release,
                destroy_count_after_source_release,
                rollback_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_nested_closure_retain_test(Interp* interp, int* pass, int* fail) {
    // Invariant: nested env-copy memoizes one cloned closure per source wrapper,
    // retains standalone closure env scopes once for that clone, and source
    // release drops only the original owner edge.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            interp.current_scope = saved_scope;
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: copy_env nested closure env_scope retain symmetry");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__copy_env_nested_payload");
        SymbolId outer_sym = interp.symbols.intern("__copy_env_outer_closure");
        SymbolId inner_sym = interp.symbols.intern("__copy_env_inner_closure");

        interp.current_scope = source;
        Env* captured_env = make_env(interp, interp.global_env);
        env_define_with_barrier(captured_env, payload_sym, make_int(interp, 88), interp);

        main::ScopeRegion* closure_env_scope = null;
        Value* shared_closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);

        Env* parent_env = make_env(interp, interp.global_env);
        env_define_with_barrier(parent_env, outer_sym, shared_closure, interp);

        Env* child_env = make_env(interp, parent_env);
        env_define_with_barrier(child_env, inner_sym, shared_closure, interp);

        bool setup_ok = shared_closure != null &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1;

        Env* copied = setup_ok ? boundary_copy_env_to_target_scope(child_env, interp, target) : null;
        Value* copied_inner = copied != null ? copied.lookup(inner_sym) : null;
        Value* copied_outer = copied != null ? copied.lookup(outer_sym) : null;

        bool copied_ok = copied != null &&
                         copied.parent != null &&
                         copied_inner != null &&
                         copied_outer != null &&
                         copied_inner.tag == CLOSURE &&
                         copied_outer.tag == CLOSURE &&
                         copied_inner != shared_closure &&
                         copied_outer == copied_inner &&
                         copied_inner.closure_val.env_scope == closure_env_scope &&
                         copied_outer.closure_val.env_scope == closure_env_scope &&
                         closure_env_scope != null &&
                         closure_env_scope.refcount == 2;

        int rc_before_release = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        main::scope_release(source);

        int rc_after_release = -1;
        bool post_release_ok = false;
        if (copied_ok) {
            rc_after_release = (int)closure_env_scope.refcount;
            post_release_ok = rc_after_release == 1 &&
                              closure_env_lookup_int(copied_inner, payload_sym, 88) &&
                              closure_env_lookup_int(copied_outer, payload_sym, 88);
        }

        interp.current_scope = saved_scope;
        main::scope_release(target);

        bool ok = setup_ok && copied_ok && post_release_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: copy_env nested closure env_scope retain symmetry");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: copy_env nested closure env_scope retain symmetry (setup_ok=%s copied_ok=%s post_release_ok=%s rc_before_release=%d rc_after_release=%d)",
                setup_ok ? "yes" : "no",
                copied_ok ? "yes" : "no",
                post_release_ok ? "yes" : "no",
                rc_before_release,
                rc_after_release);
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_closure_wrapper_alloc_fail_test(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy fails closed on closure wrapper allocation failure
    // and does not retain detached closure env scopes.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            interp.current_scope = saved_scope;
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy closure wrapper alloc fail");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__copy_env_closure_wrapper_alloc_payload");
        SymbolId closure_sym = interp.symbols.intern("__copy_env_closure_wrapper_alloc");

        interp.current_scope = source;
        Env* captured_env = make_env(interp, interp.global_env);
        main::ScopeRegion* closure_env_scope = null;
        int closure_env_destroy_count = 0;
        Value* closure = null;
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 180), interp);
            closure = make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope);
            if (closure_env_scope != null) {
                main::scope_register_dtor(closure_env_scope, (void*)&closure_env_destroy_count, fn void(void* ptr) {
                    int* counter = (int*)ptr;
                    if (counter != null) (*counter)++;
                });
            }
        }

        Env* frame = make_env(interp, interp.global_env);
        bool setup_ok = frame != null &&
                        closure != null &&
                        closure.tag == CLOSURE &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1;
        if (setup_ok) {
            env_define_with_barrier(frame, closure_sym, closure, interp);
        }

        bool saved_force = g_boundary_force_copy_closure_wrapper_alloc_fail;
        g_boundary_force_copy_closure_wrapper_alloc_fail = true;
        BoundaryEnvCopyResult copied =
            setup_ok ? boundary_copy_env_to_target_scope_checked(frame, interp, target)
                     : boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, 0);
        g_boundary_force_copy_closure_wrapper_alloc_fail = saved_force;

        int rc_after_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        bool fault_ok = copied.env == null &&
                        copied.fault_code == BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY &&
                        copied.depth == 0;
        bool rollback_ok = rc_after_fail == 1 &&
                           closure_env_lookup_int(closure, payload_sym, 180);

        interp.current_scope = saved_scope;
        main::scope_release(target);
        int rc_after_target_release = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        int destroy_count_after_target_release = closure_env_destroy_count;
        bool teardown_ok = rc_after_target_release == 1 &&
                           destroy_count_after_target_release == 0;

        main::scope_release(source);
        int destroy_count_after_source_release = closure_env_destroy_count;
        bool owner_release_ok = destroy_count_after_source_release == 1;

        bool ok = setup_ok && fault_ok && rollback_ok && teardown_ok && owner_release_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy closure wrapper alloc fail");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy closure wrapper alloc fail (setup_ok=%s fault=%s rc_after_fail=%d rc_after_target_release=%d destroy_count_after_target_release=%d destroy_count_after_source_release=%d rollback_ok=%s)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                rc_after_fail,
                rc_after_target_release,
                destroy_count_after_target_release,
                destroy_count_after_source_release,
                rollback_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}
