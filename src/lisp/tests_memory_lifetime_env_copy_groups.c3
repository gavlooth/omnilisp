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
    // Invariant: nested env-copy cloning retains shared closure env scopes once
    // per cloned closure and source release drops only the original owner edge.
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
                         copied_outer != shared_closure &&
                         copied_inner != copied_outer &&
                         copied_inner.closure_val.env_scope == closure_env_scope &&
                         copied_outer.closure_val.env_scope == closure_env_scope &&
                         closure_env_scope != null &&
                         closure_env_scope.refcount == 3;

        int rc_before_release = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        main::scope_release(source);

        int rc_after_release = -1;
        bool post_release_ok = false;
        if (copied_ok) {
            rc_after_release = (int)closure_env_scope.refcount;
            post_release_ok = rc_after_release == 2 &&
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

fn void run_memory_lifetime_env_copy_memo_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: env copy uses one boundary epoch and memoizes repeated values.
    // This keeps copy_env_to_scope churn bounded for shared captured values.
    {
        reset_memory_promotion_stats();

        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* source = main::scope_create(saved);

        interp.current_scope = source;
        Env* src_env = make_env(interp, null);
        Value* shared = make_cons(interp, make_int(interp, 123), make_nil(interp));
        for (usz i = 0; i < 24; i++) {
            char[64] nbuf;
            char[] name = io::bprintf(&nbuf, "__copy_env_shared_%d", (long)i)!!;
            env_define_with_barrier(src_env, interp.symbols.intern(name), shared, interp);
        }

        usz base_site = copy_to_parent_site_calls(COPY_SITE_COPY_ENV_TO_SCOPE);
        Env* copied = boundary_copy_env_to_target_scope(src_env, interp, target);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_COPY_ENV_TO_SCOPE) - base_site;

        bool copied_ok = copied != null && copied.binding_count == src_env.binding_count;
        bool alias_ok = copied_ok;
        Value* first = null;
        if (copied_ok) {
            for (usz i = 0; i < copied.binding_count; i++) {
                Value* cur = copied.bindings[i].value;
                if (cur == null || cur.tag != CONS ||
                    cur.cons_val.car == null || cur.cons_val.car.tag != INT ||
                    cur.cons_val.car.int_val != 123) {
                    alias_ok = false;
                    break;
                }
                if (i == 0) first = cur;
                if (i > 0 && cur != first) {
                    alias_ok = false;
                    break;
                }
            }
        }

        // Source release must not invalidate target copies.
        main::scope_release(source);
        bool post_ok = first != null &&
                       first.tag == CONS &&
                       first.cons_val.car != null &&
                       first.cons_val.car.tag == INT &&
                       first.cons_val.car.int_val == 123;

        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = copied_ok && alias_ok && post_ok && site_delta <= 4;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: copy_env shared-value memo gate");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: copy_env shared-value memo gate (site_delta=%d copied_ok=%s alias_ok=%s post_ok=%s)",
                (long)site_delta,
                copied_ok ? "yes" : "no",
                alias_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_persistent_parent_rewrite_test(Interp* interp, int* pass, int* fail) {
    // Invariant: copy_env_to_scope rewrites persistent frame parents away from
    // releasing source scopes, so persistent boxes never retain dangling parent links.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: copy_env persistent parent rewrite");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__copy_env_parent_payload");
        SymbolId box_sym = interp.symbols.intern("__copy_env_parent_box");

        interp.current_scope = source;
        Env* transient_parent = make_env(interp, interp.global_env);
        env_define_with_barrier(transient_parent, payload_sym, make_int(interp, 99), interp);

        Env* persistent_box = boundary_env_extend_in_root(
            interp,
            transient_parent,
            box_sym,
            make_root_int_for_test(interp, 7)
        );
        bool created_as_persistent = persistent_box != null && persistent_box.is_root_persistent_box();
        Env* original_parent = persistent_box != null ? persistent_box.parent : null;

        Env* copied = boundary_copy_env_to_target_scope(persistent_box, interp, target);

        bool copied_ok = copied != null &&
                         copied == persistent_box &&
                         copied.is_root_persistent_box() &&
                         created_as_persistent;
        bool rewritten_ok = copied_ok &&
                           copied.parent != null &&
                           copied.parent != original_parent &&
                           main::is_in_scope((void*)copied.parent, target);

        Value* before_release = rewritten_ok ? copied.lookup(payload_sym) : null;
        main::scope_release(source);
        Value* after_release = rewritten_ok ? copied.lookup(payload_sym) : null;

        bool post_ok = before_release != null &&
                       before_release.tag == INT &&
                       before_release.int_val == 99 &&
                       after_release != null &&
                       after_release.tag == INT &&
                       after_release.int_val == 99;

        // Restore persistent parent chain to stable root path before releasing target.
        if (copied_ok) {
            copied.parent = interp.global_env;
        }

        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = copied_ok && rewritten_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: copy_env persistent parent rewrite");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: copy_env persistent parent rewrite (copied_ok=%s rewritten_ok=%s post_ok=%s)",
                copied_ok ? "yes" : "no",
                rewritten_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_mixed_chain_rewrite_test(Interp* interp, int* pass, int* fail) {
    // Invariant: mixed transient/persistent parent chains are fully rewritten
    // when copying from a releasing scope into a target scope.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: copy_env mixed persistent chain rewrite");
            (*fail)++;
            return;
        }

        SymbolId leaf_a = interp.symbols.intern("__copy_env_leaf_a");
        SymbolId leaf_b = interp.symbols.intern("__copy_env_leaf_b");
        SymbolId box_a = interp.symbols.intern("__copy_env_box_a");
        SymbolId box_b = interp.symbols.intern("__copy_env_box_b");

        interp.current_scope = source;
        Env* transient_a = make_env(interp, interp.global_env);
        env_define_with_barrier(transient_a, leaf_a, make_int(interp, 11), interp);

        Env* persistent_a = boundary_env_extend_in_root(
            interp, transient_a, box_a, make_root_int_for_test(interp, 1));
        bool persistent_a_kind = persistent_a != null && persistent_a.is_root_persistent_box();

        Env* transient_b = make_env(interp, persistent_a);
        env_define_with_barrier(transient_b, leaf_b, make_int(interp, 22), interp);

        Env* persistent_b = boundary_env_extend_in_root(
            interp, transient_b, box_b, make_root_int_for_test(interp, 2));
        bool persistent_b_kind = persistent_b != null && persistent_b.is_root_persistent_box();

        Env* orig_parent_a = persistent_a != null ? persistent_a.parent : null;
        Env* orig_parent_b = persistent_b != null ? persistent_b.parent : null;

        Env* copied = boundary_copy_env_to_target_scope(persistent_b, interp, target);

        bool id_ok = copied != null &&
                     copied == persistent_b &&
                     copied.is_root_persistent_box() &&
                     persistent_a_kind &&
                     persistent_b_kind;
        bool parent_b_ok = id_ok &&
                           copied.parent != null &&
                           copied.parent != orig_parent_b &&
                           main::is_in_scope((void*)copied.parent, target);
        bool parent_a_ok = id_ok &&
                           persistent_a != null &&
                           persistent_a.parent != null &&
                           persistent_a.parent != orig_parent_a &&
                           main::is_in_scope((void*)persistent_a.parent, target);

        Value* va_before = id_ok ? copied.lookup(leaf_a) : null;
        Value* vb_before = id_ok ? copied.lookup(leaf_b) : null;

        main::scope_release(source);

        Value* va_after = id_ok ? copied.lookup(leaf_a) : null;
        Value* vb_after = id_ok ? copied.lookup(leaf_b) : null;

        bool values_ok = va_before != null && va_before.tag == INT && va_before.int_val == 11 &&
                         vb_before != null && vb_before.tag == INT && vb_before.int_val == 22 &&
                         va_after != null && va_after.tag == INT && va_after.int_val == 11 &&
                         vb_after != null && vb_after.tag == INT && vb_after.int_val == 22;

        // Detach persistent nodes from target-owned copied parents before target release.
        if (persistent_b != null) persistent_b.parent = persistent_a;
        if (persistent_a != null) persistent_a.parent = interp.global_env;

        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = id_ok && parent_b_ok && parent_a_ok && values_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: copy_env mixed persistent chain rewrite");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: copy_env mixed persistent chain rewrite (id_ok=%s parent_b_ok=%s parent_a_ok=%s values_ok=%s)",
                id_ok ? "yes" : "no",
                parent_b_ok ? "yes" : "no",
                parent_a_ok ? "yes" : "no",
                values_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_closure_retain_stress_test(Interp* interp, int* pass, int* fail) {
    // Invariant: repeated closure env-copy boundaries keep env_scope retain/release symmetry.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* saved_releasing_scope = interp.releasing_scope;
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (target == null) {
            io::printn("[FAIL] lifetime: copy_env closure retain symmetry stress");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__copy_env_retain_stress_payload");
        bool ok = true;
        usz failed_iter = usz.max;
        int failed_before = -1;
        int failed_after = -1;
        for (usz i = 0; i < 128 && ok; i++) {
            main::ScopeRegion* source = main::scope_create(saved_scope);
            if (source == null) {
                ok = false;
                failed_iter = i;
                break;
            }

            interp.current_scope = source;
            Env* captured_env = make_env(interp, interp.global_env);
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, (long)(500 + i)), interp);

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
                             copied.tag == CLOSURE &&
                             copied != closure &&
                             copied.closure_val.env_scope == closure_env_scope &&
                             closure_env_scope.refcount == 2;
            int rc_before = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
            main::scope_release(source);
            int rc_after = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;

            bool post_ok = copied_ok &&
                           rc_before == 2 &&
                           rc_after == 1 &&
                           closure_env_lookup_int(copied, payload_sym, (long)(500 + i));
            bool state_ok = interp.current_scope == target &&
                            interp.releasing_scope == saved_releasing_scope;
            if (!setup_ok || !post_ok || !state_ok) {
                ok = false;
                failed_iter = i;
                failed_before = rc_before;
                failed_after = rc_after;
            }
        }

        interp.current_scope = saved_scope;
        interp.releasing_scope = saved_releasing_scope;
        main::scope_release(target);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: copy_env closure retain symmetry stress");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: copy_env closure retain symmetry stress (failed_iter=%d rc_before=%d rc_after=%d)",
                (long)failed_iter,
                failed_before,
                failed_after);
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_escape_env_lane_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: unified one-scope escape-env mode allocates env frames in ESCAPE lane.
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        bool saved_escape_mode = interp.escape_env_mode;

        main::ScopeRegion* s = main::scope_create(saved_scope);
        interp.current_scope = s;
        interp.escape_env_mode = true;

        Env* e = make_env(interp, null);
        bool ok = e != null &&
                  e.scope_gen == s.escape_generation &&
                  main::is_in_scope((void*)e, s);

        interp.escape_env_mode = saved_escape_mode;
        interp.current_scope = saved_scope;
        main::scope_release(s);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: unified escape-env uses current ESCAPE lane");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: unified escape-env uses current ESCAPE lane");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_fault_surface_tests(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy failure paths return typed boundary faults (not silent null-only failures).
    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved_scope);
        main::ScopeRegion* target = main::scope_create(saved_scope);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy typed fault surfacing");
            (*fail)++;
            return;
        }

        interp.current_scope = source;
        Env* deep_leaf = interp.global_env;
        bool setup_ok = true;
        for (usz i = 0; i < 300; i++) {
            deep_leaf = make_env(interp, deep_leaf);
            if (deep_leaf == null) {
                setup_ok = false;
                break;
            }
        }

        BoundaryEnvCopyResult copied = boundary_copy_env_to_target_scope_checked(deep_leaf, interp, target);
        bool depth_fault = copied.fault_code == BOUNDARY_ENV_COPY_FAULT_DEPTH_LIMIT;
        bool parent_fault_with_depth = copied.fault_code == BOUNDARY_ENV_COPY_FAULT_PARENT_COPY &&
                                       copied.depth >= 256;
        bool ok = setup_ok &&
                  copied.env == null &&
                  (depth_fault || parent_fault_with_depth) &&
                  copied.depth >= 256;

        interp.current_scope = saved_scope;
        main::scope_release(source);
        main::scope_release(target);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy typed fault surfacing");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: env-copy typed fault surfacing (fault=%s depth=%d env=%s)",
                (ZString)boundary_env_copy_fault_name(copied.fault_code),
                (long)copied.depth,
                copied.env == null ? "null" : "non-null");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_env_copy_escape_mode_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_copy_parent_closure_env_scope_retain_test(interp, pass, fail);
    run_memory_lifetime_env_copy_nested_closure_retain_test(interp, pass, fail);
    run_memory_lifetime_env_copy_closure_retain_stress_test(interp, pass, fail);
    run_memory_lifetime_env_copy_memo_tests(interp, pass, fail);
    run_memory_lifetime_env_copy_persistent_parent_rewrite_test(interp, pass, fail);
    run_memory_lifetime_env_copy_mixed_chain_rewrite_test(interp, pass, fail);
    run_memory_lifetime_escape_env_lane_tests(interp, pass, fail);
    run_memory_lifetime_env_copy_fault_surface_tests(interp, pass, fail);
}
