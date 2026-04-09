module lisp;


import std::io;
import main;

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
            if (try char[] name = io::bprintf(&nbuf, "__copy_env_shared_%d", (long)i)) {
                env_define_with_barrier(src_env, interp.symbols.intern(name), shared, interp);
            } else {
                src_env = null;
                break;
            }
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
    // releasing source scopes into root-owned lifetime so persistent boxes never
    // retain dangling links to transient target scopes.
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
                           main::is_in_scope((void*)copied.parent, interp.root_scope);

        Value* before_release = rewritten_ok ? copied.lookup(payload_sym) : null;
        main::scope_release(source);
        Value* after_source_release = rewritten_ok ? copied.lookup(payload_sym) : null;

        interp.current_scope = saved;
        main::scope_release(target);
        Value* after_target_release = rewritten_ok ? copied.lookup(payload_sym) : null;

        bool post_ok = before_release != null &&
                       before_release.tag == INT &&
                       before_release.int_val == 99 &&
                       after_source_release != null &&
                       after_source_release.tag == INT &&
                       after_source_release.int_val == 99 &&
                       after_target_release != null &&
                       after_target_release.tag == INT &&
                       after_target_release.int_val == 99;

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
    // when copying from a releasing scope, and rewritten parents become root-owned.
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
                           main::is_in_scope((void*)copied.parent, interp.root_scope);
        bool parent_a_ok = id_ok &&
                           persistent_a != null &&
                           persistent_a.parent != null &&
                           persistent_a.parent != orig_parent_a &&
                           main::is_in_scope((void*)persistent_a.parent, interp.root_scope);

        Value* va_before = id_ok ? copied.lookup(leaf_a) : null;
        Value* vb_before = id_ok ? copied.lookup(leaf_b) : null;

        main::scope_release(source);

        Value* va_after_source_release = id_ok ? copied.lookup(leaf_a) : null;
        Value* vb_after_source_release = id_ok ? copied.lookup(leaf_b) : null;

        interp.current_scope = saved;
        main::scope_release(target);
        Value* va_after_target_release = id_ok ? copied.lookup(leaf_a) : null;
        Value* vb_after_target_release = id_ok ? copied.lookup(leaf_b) : null;

        bool values_ok = va_before != null && va_before.tag == INT && va_before.int_val == 11 &&
                         vb_before != null && vb_before.tag == INT && vb_before.int_val == 22 &&
                         va_after_source_release != null && va_after_source_release.tag == INT && va_after_source_release.int_val == 11 &&
                         vb_after_source_release != null && vb_after_source_release.tag == INT && vb_after_source_release.int_val == 22 &&
                         va_after_target_release != null && va_after_target_release.tag == INT && va_after_target_release.int_val == 11 &&
                         vb_after_target_release != null && vb_after_target_release.tag == INT && vb_after_target_release.int_val == 22;

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

fn void run_memory_lifetime_env_copy_primitive_policy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy clones disjoint plain primitive headers and rejects
    // opaque primitive payloads with binding-value-copy fault.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy primitive policy");
            (*fail)++;
            return;
        }

        SymbolId plain_sym = interp.symbols.intern("__copy_env_plain_primitive");
        SymbolId opaque_sym = interp.symbols.intern("__copy_env_opaque_primitive");

        interp.current_scope = source;
        Env* plain_env = make_env(interp, interp.global_env);
        Value* plain = interp.alloc_value();
        plain.tag = PRIMITIVE;
        main::scope_register_dtor(source, (void*)plain, &scope_dtor_value);
        Primitive* plain_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        if (plain_env == null || plain_prim == null) {
            if (plain_prim != null) mem::free(plain_prim);
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy primitive policy (setup plain)");
            (*fail)++;
            return;
        }
        *plain_prim = {};
        plain_prim.name[0] = 'p';
        plain_prim.name[1] = 0;
        plain_prim.arity = 4;
        plain_prim.tag = 701;
        plain.prim_val = plain_prim;
        env_define_with_barrier(plain_env, plain_sym, plain, interp);

        BoundaryEnvCopyResult plain_copy = boundary_copy_env_to_target_scope_checked(plain_env, interp, target);
        Value* plain_copied = boundary_env_copy_result_ok(plain_copy) && plain_copy.env != null
            ? plain_copy.env.lookup(plain_sym)
            : null;

        bool plain_ok = boundary_env_copy_result_ok(plain_copy) &&
                        plain_copy.env != null &&
                        plain_copied != null &&
                        plain_copied.tag == PRIMITIVE &&
                        plain_copied != plain &&
                        plain_copied.prim_val != null &&
                        plain_copied.prim_val != plain.prim_val &&
                        plain_copied.prim_val.arity == 4 &&
                        plain_copied.prim_val.tag == 701 &&
                        plain_copied.prim_val.name[0] == 'p' &&
                        plain_copied.prim_val.name[1] == 0;

        Env* opaque_env = make_env(interp, interp.global_env);
        Value* opaque = interp.alloc_value();
        opaque.tag = PRIMITIVE;
        main::scope_register_dtor(source, (void*)opaque, &scope_dtor_value);
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)mem::malloc(int.sizeof);
        if (opaque_env == null || opaque_prim == null || opaque_payload == null) {
            if (opaque_prim != null) mem::free(opaque_prim);
            if (opaque_payload != null) mem::free(opaque_payload);
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy primitive policy (setup opaque)");
            (*fail)++;
            return;
        }
        *opaque_prim = {};
        *opaque_payload = 702;
        opaque_prim.name[0] = 'q';
        opaque_prim.name[1] = 0;
        opaque_prim.arity = 5;
        opaque_prim.tag = 702;
        opaque_prim.user_data = (void*)opaque_payload;
        opaque.prim_val = opaque_prim;
        env_define_with_barrier(opaque_env, opaque_sym, opaque, interp);

        BoundaryEnvCopyResult opaque_copy = boundary_copy_env_to_target_scope_checked(opaque_env, interp, target);
        bool opaque_ok = !boundary_env_copy_result_ok(opaque_copy) &&
                         opaque_copy.env == null &&
                         opaque_copy.fault_code == BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY;

        main::scope_release(source);

        bool post_ok = plain_copied != null &&
                       plain_copied.tag == PRIMITIVE &&
                       plain_copied.prim_val != null &&
                       plain_copied.prim_val.arity == 4 &&
                       plain_copied.prim_val.tag == 701 &&
                       plain_copied.prim_val.name[0] == 'p' &&
                       plain_copied.prim_val.name[1] == 0;

        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = plain_ok && opaque_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy primitive policy");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: env-copy primitive policy (plain_ok=%s opaque_ok=%s post_ok=%s opaque_fault=%s)",
                plain_ok ? "yes" : "no",
                opaque_ok ? "yes" : "no",
                post_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(opaque_copy.fault_code));
            (*fail)++;
        }
    }
}
