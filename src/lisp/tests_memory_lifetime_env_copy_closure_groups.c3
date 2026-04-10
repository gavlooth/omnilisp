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
