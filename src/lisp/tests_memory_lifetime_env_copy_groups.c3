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

fn void run_memory_lifetime_env_copy_shared_wrapper_policy_test(Interp* interp, int* pass, int* fail) {
    // Invariant: env-copy clones disjoint shared wrappers instead of reusing
    // source-scope ARRAY / HASHMAP / SET / METHOD_TABLE wrappers by identity,
    // and it also clones target-chain PARTIAL_PRIM / ITERATOR wrappers when
    // their nested shared-wrapper payload still points into the source scope.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* source = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        if (source == null || target == null) {
            if (source != null) main::scope_release(source);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy");
            (*fail)++;
            return;
        }

        SymbolId array_sym = interp.symbols.intern("__copy_env_shared_array");
        SymbolId hashmap_sym = interp.symbols.intern("__copy_env_shared_hashmap");
        SymbolId set_sym = interp.symbols.intern("__copy_env_shared_set");
        SymbolId mt_sym = interp.symbols.intern("__copy_env_shared_method_table");
        SymbolId partial_sym = interp.symbols.intern("__copy_env_partial_alias");
        SymbolId iterator_sym = interp.symbols.intern("__copy_env_iterator_alias");

        interp.current_scope = source;
        Env* src_env = make_env(interp, interp.global_env);
        if (src_env == null) {
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (setup env)");
            (*fail)++;
            return;
        }

        Value* array_src = interp.alloc_value();
        array_src.tag = ARRAY;
        main::scope_register_dtor(source, (void*)array_src, &scope_dtor_value);
        Array* array_payload = (Array*)mem::malloc(Array.sizeof);
        if (array_payload == null) {
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (setup array)");
            (*fail)++;
            return;
        }
        array_payload.capacity = 4;
        array_payload.length = 1;
        array_payload.items = (Value**)mem::malloc(Value*.sizeof * array_payload.capacity);
        if (array_payload.items == null) {
            mem::free(array_payload);
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (setup array items)");
            (*fail)++;
            return;
        }
        for (usz i = 0; i < array_payload.capacity; i++) {
            array_payload.items[i] = null;
        }
        array_payload.items[0] = make_int(interp, 71);
        array_src.array_val = array_payload;
        env_define_with_barrier(src_env, array_sym, array_src, interp);

        Value* hashmap_src = interp.alloc_value();
        hashmap_src.tag = HASHMAP;
        main::scope_register_dtor(source, (void*)hashmap_src, &scope_dtor_value);
        HashMap* hashmap_payload = (HashMap*)mem::malloc(HashMap.sizeof);
        if (hashmap_payload == null) {
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (setup hashmap)");
            (*fail)++;
            return;
        }
        hashmap_payload.capacity = 8;
        hashmap_payload.count = 1;
        hashmap_payload.mask = hashmap_payload.capacity - 1;
        hashmap_payload.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * hashmap_payload.capacity);
        if (hashmap_payload.entries == null) {
            mem::free(hashmap_payload);
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (setup hashmap entries)");
            (*fail)++;
            return;
        }
        for (uint i = 0; i < hashmap_payload.capacity; i++) {
            hashmap_payload.entries[i].key = null;
            hashmap_payload.entries[i].value = null;
        }
        Value* hashmap_key = make_int(interp, 7);
        Value* hashmap_value = make_int(interp, 99);
        uint hashmap_idx = hash_value(hashmap_key) & hashmap_payload.mask;
        hashmap_payload.entries[hashmap_idx].key = hashmap_key;
        hashmap_payload.entries[hashmap_idx].value = hashmap_value;
        hashmap_src.hashmap_val = hashmap_payload;
        env_define_with_barrier(src_env, hashmap_sym, hashmap_src, interp);

        Value* set_src = interp.alloc_value();
        set_src.tag = SET;
        main::scope_register_dtor(source, (void*)set_src, &scope_dtor_value);
        HashMap* set_payload = (HashMap*)mem::malloc(HashMap.sizeof);
        if (set_payload == null) {
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (setup set)");
            (*fail)++;
            return;
        }
        set_payload.capacity = 8;
        set_payload.count = 1;
        set_payload.mask = set_payload.capacity - 1;
        set_payload.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * set_payload.capacity);
        if (set_payload.entries == null) {
            mem::free(set_payload);
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (setup set entries)");
            (*fail)++;
            return;
        }
        for (uint i = 0; i < set_payload.capacity; i++) {
            set_payload.entries[i].key = null;
            set_payload.entries[i].value = null;
        }
        Value* set_key = make_int(interp, 11);
        Value* set_value = make_int(interp, 1);
        uint set_idx = hash_value(set_key) & set_payload.mask;
        set_payload.entries[set_idx].key = set_key;
        set_payload.entries[set_idx].value = set_value;
        set_src.hashmap_val = set_payload;
        env_define_with_barrier(src_env, set_sym, set_src, interp);

        Value* mt_src = interp.alloc_value();
        mt_src.tag = METHOD_TABLE;
        main::scope_register_dtor(source, (void*)mt_src, &scope_dtor_value);
        MethodTable* mt_payload = (MethodTable*)mem::malloc(MethodTable.sizeof);
        if (mt_payload == null) {
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (setup method table)");
            (*fail)++;
            return;
        }
        mt_payload.name = interp.symbols.intern("__copy_env_shared_wrapper_mt");
        mt_payload.capacity = 1;
        mt_payload.entry_count = 1;
        mt_payload.fallback = make_int(interp, 123);
        mt_payload.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * mt_payload.capacity);
        if (mt_payload.entries == null) {
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (setup method table entries)");
            (*fail)++;
            return;
        }
        method_signature_init_empty(&mt_payload.entries[0].sig);
        MethodSignature mt_sig_src;
        method_signature_init_empty(&mt_sig_src);
        if (!method_signature_alloc_arrays_in_scope(&mt_sig_src, source, 1, 0)) {
            mem::free(mt_payload.entries);
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (setup method table signature)");
            (*fail)++;
            return;
        }
        mt_sig_src.param_types[0] = interp.tid_Integer;
        mt_sig_src.param_type_vars[0] = (SymbolId)0;
        mt_sig_src.has_val_literal[0] = false;
        if (!method_signature_copy_to_heap(&mt_payload.entries[0].sig, &mt_sig_src)) {
            mem::free(mt_payload.entries);
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (copy method signature)");
            (*fail)++;
            return;
        }
        mt_payload.entries[0].implementation = make_int(interp, 456);
        mt_src.method_table_val = mt_payload;
        env_define_with_barrier(src_env, mt_sym, mt_src, interp);

        interp.current_scope = target;
        Value* alias_array = interp.alloc_value();
        alias_array.tag = ARRAY;
        main::scope_register_dtor(target, (void*)alias_array, &scope_dtor_value);
        Array* alias_array_payload = (Array*)mem::malloc(Array.sizeof);
        if (alias_array_payload == null) {
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (setup alias array)");
            (*fail)++;
            return;
        }
        alias_array_payload.capacity = 4;
        alias_array_payload.length = 1;
        alias_array_payload.items = (Value**)mem::malloc(Value*.sizeof * alias_array_payload.capacity);
        if (alias_array_payload.items == null) {
            mem::free(alias_array_payload);
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (setup alias array items)");
            (*fail)++;
            return;
        }
        for (usz i = 0; i < alias_array_payload.capacity; i++) {
            alias_array_payload.items[i] = null;
        }
        alias_array.array_val = alias_array_payload;

        Value* partial_src = interp.alloc_value();
        partial_src.tag = PARTIAL_PRIM;
        partial_src.partial_val.func = &prim_add;
        partial_src.partial_val.remaining = 1;
        partial_src.partial_val.first_arg = alias_array;
        partial_src.partial_val.second_arg = make_int(interp, 88);

        Value* iterator_src = make_iterator(interp, partial_src);
        if (iterator_src == null) {
            interp.current_scope = saved;
            main::scope_release(source);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: env-copy shared wrapper policy (setup alias iterator)");
            (*fail)++;
            return;
        }

        interp.current_scope = source;
        Value* alias_child = make_int(interp, 777);
        alias_array_payload.items[0] = alias_child;
        env_define_with_barrier(src_env, partial_sym, partial_src, interp);
        env_define_with_barrier(src_env, iterator_sym, iterator_src, interp);

        BoundaryEnvCopyResult copied = boundary_copy_env_to_target_scope_checked(src_env, interp, target);

        Value* array_copied = boundary_env_copy_result_ok(copied) && copied.env != null
            ? copied.env.lookup(array_sym)
            : null;
        Value* hashmap_copied = boundary_env_copy_result_ok(copied) && copied.env != null
            ? copied.env.lookup(hashmap_sym)
            : null;
        Value* set_copied = boundary_env_copy_result_ok(copied) && copied.env != null
            ? copied.env.lookup(set_sym)
            : null;
        Value* mt_copied = boundary_env_copy_result_ok(copied) && copied.env != null
            ? copied.env.lookup(mt_sym)
            : null;
        Value* partial_copied = boundary_env_copy_result_ok(copied) && copied.env != null
            ? copied.env.lookup(partial_sym)
            : null;
        Value* iterator_copied = boundary_env_copy_result_ok(copied) && copied.env != null
            ? copied.env.lookup(iterator_sym)
            : null;

        bool array_ok = array_copied != null &&
                        array_copied.tag == ARRAY &&
                        array_copied != array_src &&
                        array_copied.array_val != null &&
                        array_copied.array_val != array_src.array_val &&
                        array_copied.array_val.items != null &&
                        array_copied.array_val.items[0] != null &&
                        array_copied.array_val.items[0] != array_src.array_val.items[0] &&
                        array_copied.array_val.items[0].tag == INT &&
                        array_copied.array_val.items[0].int_val == 71;

        Value* copied_hashmap_key = null;
        Value* copied_hashmap_value = null;
        if (hashmap_copied != null && hashmap_copied.tag == HASHMAP && hashmap_copied.hashmap_val != null && hashmap_copied.hashmap_val.entries != null) {
            for (uint i = 0; i < hashmap_copied.hashmap_val.capacity; i++) {
                if (hashmap_copied.hashmap_val.entries[i].key != null) {
                    copied_hashmap_key = hashmap_copied.hashmap_val.entries[i].key;
                    copied_hashmap_value = hashmap_copied.hashmap_val.entries[i].value;
                    break;
                }
            }
        }
        bool hashmap_ok = hashmap_copied != null &&
                          hashmap_copied.tag == HASHMAP &&
                          hashmap_copied != hashmap_src &&
                          hashmap_copied.hashmap_val != null &&
                          hashmap_copied.hashmap_val != hashmap_src.hashmap_val &&
                          copied_hashmap_key != null &&
                          copied_hashmap_value != null &&
                          copied_hashmap_key != hashmap_src.hashmap_val.entries[hashmap_idx].key &&
                          copied_hashmap_value != hashmap_src.hashmap_val.entries[hashmap_idx].value &&
                          copied_hashmap_value.tag == INT &&
                          copied_hashmap_value.int_val == 99;

        Value* copied_set_key = null;
        Value* copied_set_value = null;
        if (set_copied != null && set_copied.tag == SET && set_copied.hashmap_val != null && set_copied.hashmap_val.entries != null) {
            for (uint i = 0; i < set_copied.hashmap_val.capacity; i++) {
                if (set_copied.hashmap_val.entries[i].key != null) {
                    copied_set_key = set_copied.hashmap_val.entries[i].key;
                    copied_set_value = set_copied.hashmap_val.entries[i].value;
                    break;
                }
            }
        }
        bool set_ok = set_copied != null &&
                      set_copied.tag == SET &&
                      set_copied != set_src &&
                      set_copied.hashmap_val != null &&
                      set_copied.hashmap_val != set_src.hashmap_val &&
                      copied_set_key != null &&
                      copied_set_value != null &&
                      copied_set_key != set_src.hashmap_val.entries[set_idx].key &&
                      copied_set_value != set_src.hashmap_val.entries[set_idx].value &&
                      copied_set_value.tag == INT &&
                      copied_set_value.int_val == 1;

        bool mt_ok = mt_copied != null &&
                     mt_copied.tag == METHOD_TABLE &&
                     mt_copied != mt_src &&
                     mt_copied.method_table_val != null &&
                     mt_copied.method_table_val != mt_src.method_table_val &&
                     mt_copied.method_table_val.entry_count == 1 &&
                     mt_copied.method_table_val.entries != null &&
                     mt_copied.method_table_val.entries[0].implementation != null &&
                     mt_copied.method_table_val.entries[0].implementation != mt_src.method_table_val.entries[0].implementation &&
                     mt_copied.method_table_val.entries[0].implementation.tag == INT &&
                     mt_copied.method_table_val.entries[0].implementation.int_val == 456 &&
                     mt_copied.method_table_val.fallback != null &&
                     mt_copied.method_table_val.fallback != mt_src.method_table_val.fallback &&
                     mt_copied.method_table_val.fallback.tag == INT &&
                     mt_copied.method_table_val.fallback.int_val == 123;

        Value* partial_copy_first = partial_copied != null && partial_copied.tag == PARTIAL_PRIM
            ? partial_copied.partial_val.first_arg
            : null;
        Value* iterator_copy_partial = iterator_copied != null && iterator_copied.tag == ITERATOR
            ? iterator_copied.iterator_val
            : null;
        Value* iterator_copy_first = iterator_copy_partial != null && iterator_copy_partial.tag == PARTIAL_PRIM
            ? iterator_copy_partial.partial_val.first_arg
            : null;

        bool partial_ok = partial_copied != null &&
                          partial_copied.tag == PARTIAL_PRIM &&
                          partial_copied != partial_src &&
                          partial_copy_first != null &&
                          partial_copy_first.tag == ARRAY &&
                          partial_copy_first != alias_array &&
                          partial_copy_first.array_val != null &&
                          partial_copy_first.array_val.items != null &&
                          partial_copy_first.array_val.items[0] != null &&
                          partial_copy_first.array_val.items[0] != alias_child &&
                          partial_copy_first.array_val.items[0].tag == INT &&
                          partial_copy_first.array_val.items[0].int_val == 777;

        bool iterator_ok = iterator_copied != null &&
                           iterator_copied.tag == ITERATOR &&
                           iterator_copied != iterator_src &&
                           iterator_copy_partial != null &&
                           iterator_copy_partial.tag == PARTIAL_PRIM &&
                           iterator_copy_partial == partial_copied &&
                           iterator_copy_first != null &&
                           iterator_copy_first.tag == ARRAY &&
                           iterator_copy_first == partial_copy_first &&
                           iterator_copy_first.array_val != null &&
                           iterator_copy_first.array_val.items != null &&
                           iterator_copy_first.array_val.items[0] != null &&
                           iterator_copy_first.array_val.items[0] != alias_child &&
                           iterator_copy_first.array_val.items[0].tag == INT &&
                           iterator_copy_first.array_val.items[0].int_val == 777;

        main::scope_release(source);

        bool post_ok = array_copied != null &&
                       array_copied.array_val != null &&
                       array_copied.array_val.items != null &&
                       array_copied.array_val.items[0] != null &&
                       array_copied.array_val.items[0].tag == INT &&
                       array_copied.array_val.items[0].int_val == 71 &&
                       copied_hashmap_value != null &&
                       copied_hashmap_value.tag == INT &&
                       copied_hashmap_value.int_val == 99 &&
                       copied_set_value != null &&
                       copied_set_value.tag == INT &&
                       copied_set_value.int_val == 1 &&
                       mt_copied != null &&
                       mt_copied.method_table_val != null &&
                       mt_copied.method_table_val.entries != null &&
                       mt_copied.method_table_val.entries[0].implementation != null &&
                       mt_copied.method_table_val.entries[0].implementation.tag == INT &&
                       mt_copied.method_table_val.entries[0].implementation.int_val == 456 &&
                       partial_copied != null &&
                       partial_copy_first != null &&
                       partial_copy_first.tag == ARRAY &&
                       partial_copy_first.array_val != null &&
                       partial_copy_first.array_val.items != null &&
                       partial_copy_first.array_val.items[0] != null &&
                       partial_copy_first.array_val.items[0].tag == INT &&
                       partial_copy_first.array_val.items[0].int_val == 777;

        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = boundary_env_copy_result_ok(copied) &&
                  copied.env != null &&
                  array_ok && hashmap_ok && set_ok && mt_ok && partial_ok && iterator_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: env-copy shared wrapper policy");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env-copy shared wrapper policy (copy_ok=%s array_ok=%s hashmap_ok=%s set_ok=%s mt_ok=%s partial_ok=%s iterator_ok=%s post_ok=%s fault=%s)",
                boundary_env_copy_result_ok(copied) ? "yes" : "no",
                array_ok ? "yes" : "no",
                hashmap_ok ? "yes" : "no",
                set_ok ? "yes" : "no",
                mt_ok ? "yes" : "no",
                partial_ok ? "yes" : "no",
                iterator_ok ? "yes" : "no",
                post_ok ? "yes" : "no",
                (ZString)boundary_env_copy_fault_name(copied.fault_code)
            );
            (*fail)++;
        }
    }
}
