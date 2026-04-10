module lisp;

import std::io;
import main;

fn void run_memory_lifetime_root_boundary_hashmap_clone_test(Interp* interp, int* pass, int* fail) {
    // Invariant: child-owned HASHMAP payload crossing root-store boundary is
    // cloned into root lifetime and source wrapper remains valid until release.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: root-boundary HASHMAP clone path (setup child)");
            (*fail)++;
            return;
        }

        interp.current_scope = child;

        Value* local = interp.alloc_value();
        local.tag = HASHMAP;
        main::scope_register_dtor(child, (void*)local, &scope_dtor_value);

        HashMap* map = (HashMap*)mem::malloc(HashMap.sizeof);
        if (map == null) {
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary HASHMAP clone path (setup map)");
            (*fail)++;
            return;
        }

        map.capacity = 8;
        map.count = 1;
        map.mask = map.capacity - 1;
        map.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * map.capacity);
        if (map.entries == null) {
            mem::free(map);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary HASHMAP clone path (setup entries)");
            (*fail)++;
            return;
        }
        for (uint i = 0; i < map.capacity; i++) {
            map.entries[i].key = null;
            map.entries[i].value = null;
        }
        Value* key_one = make_int(interp, 1);
        Value* val_ninety_nine = make_int(interp, 99);
        uint key_idx = hash_value(key_one) & map.mask;
        map.entries[key_idx].key = key_one;
        map.entries[key_idx].value = val_ninety_nine;
        local.hashmap_val = map;

        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET);
        Value* promoted = promote_to_root_site(local, interp, COPY_SITE_JIT_EVAL_SET);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET) - base_site;

        Value* promoted_key = null;
        Value* promoted_val = null;
        if (promoted != null && promoted.hashmap_val != null && promoted.hashmap_val.entries != null) {
            for (uint i = 0; i < promoted.hashmap_val.capacity; i++) {
                if (promoted.hashmap_val.entries[i].key != null) {
                    promoted_key = promoted.hashmap_val.entries[i].key;
                    promoted_val = promoted.hashmap_val.entries[i].value;
                    break;
                }
            }
        }

        bool pre_ok = promoted != null &&
                      promoted.tag == HASHMAP &&
                      promoted != local &&
                      promoted.hashmap_val != null &&
                      local.hashmap_val != null &&
                      promoted.hashmap_val != local.hashmap_val &&
                      promoted.hashmap_val.count == 1 &&
                      promoted_key != null &&
                      promoted_val != null &&
                      boundary_ptr_in_scope((void*)promoted_key, interp.root_scope) &&
                      boundary_ptr_in_scope((void*)promoted_val, interp.root_scope) &&
                      local.hashmap_val.count == 1 &&
                      site_delta == 0;

        interp.current_scope = saved;
        main::scope_release(child);

        Value* got = (promoted != null && promoted.hashmap_val != null)
            ? hashmap_get(promoted.hashmap_val, make_int(interp, 1))
            : null;
        bool post_ok = got != null && got.tag == INT && got.int_val == 99;

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary HASHMAP clone path");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary HASHMAP clone path (site_delta=%d pre_ok=%s post_ok=%s)",
                (long)site_delta,
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_boundary_method_table_clone_test(Interp* interp, int* pass, int* fail) {
    // Invariant: child-owned METHOD_TABLE payload crossing root-store boundary is
    // cloned into root lifetime and source wrapper remains valid until release.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: root-boundary METHOD_TABLE clone path (setup child)");
            (*fail)++;
            return;
        }

        interp.current_scope = child;

        Value* local = interp.alloc_value();
        local.tag = METHOD_TABLE;
        main::scope_register_dtor(child, (void*)local, &scope_dtor_value);
        local.method_table_val = null;

        MethodTable* mt = (MethodTable*)mem::malloc(MethodTable.sizeof);
        if (mt == null) {
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary METHOD_TABLE clone path (setup table)");
            (*fail)++;
            return;
        }

        mt.name = interp.symbols.intern("root-boundary-method-table");
        mt.capacity = 1;
        mt.entry_count = 1;
        mt.fallback = make_int(interp, 33);
        mt.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * mt.capacity);
        if (mt.entries == null) {
            mem::free(mt);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary METHOD_TABLE clone path (setup entries)");
            (*fail)++;
            return;
        }

        method_signature_init_empty(&mt.entries[0].sig);
        MethodSignature src_sig;
        if (!method_signature_alloc_arrays_in_scope(&src_sig, child, 1, 0)) {
            mem::free(mt.entries);
            mem::free(mt);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary METHOD_TABLE clone path (setup signature)");
            (*fail)++;
            return;
        }
        src_sig.param_types[0] = interp.tid_Integer;
        src_sig.val_literals[0].tag = NIL;
        src_sig.val_literals[0].int_value = 0;
        src_sig.val_literals[0].sym_value = 0;
        src_sig.has_val_literal[0] = false;
        if (!method_signature_copy_to_heap(&mt.entries[0].sig, &src_sig)) {
            mem::free(mt.entries);
            mem::free(mt);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary METHOD_TABLE clone path (copy signature)");
            (*fail)++;
            return;
        }
        mt.entries[0].implementation = make_int(interp, 42);
        local.method_table_val = mt;

        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_DEFINE);
        Value* promoted = promote_to_root_site(local, interp, COPY_SITE_JIT_EVAL_DEFINE);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_DEFINE) - base_site;

        bool pre_ok = promoted != null &&
                      promoted.tag == METHOD_TABLE &&
                      promoted != local &&
                      promoted.method_table_val != null &&
                      local.method_table_val != null &&
                      promoted.method_table_val != local.method_table_val &&
                      promoted.method_table_val.entry_count == 1 &&
                      promoted.method_table_val.entries != null &&
                      promoted.method_table_val.entries[0].sig.param_count == 1 &&
                  promoted.method_table_val.entries[0].sig.param_types[0] == interp.tid_Integer &&
                      promoted.method_table_val.entries[0].implementation != null &&
                      promoted.method_table_val.entries[0].implementation.tag == INT &&
                      promoted.method_table_val.entries[0].implementation.int_val == 42 &&
                      promoted.method_table_val.fallback != null &&
                      promoted.method_table_val.fallback.tag == INT &&
                      promoted.method_table_val.fallback.int_val == 33 &&
                      site_delta == 0;

        interp.current_scope = saved;
        main::scope_release(child);

        bool post_ok = promoted != null &&
                       promoted.tag == METHOD_TABLE &&
                       promoted.method_table_val != null &&
                       promoted.method_table_val.entry_count == 1 &&
                       promoted.method_table_val.entries != null &&
                       promoted.method_table_val.entries[0].implementation != null &&
                       promoted.method_table_val.entries[0].implementation.tag == INT &&
                       promoted.method_table_val.entries[0].implementation.int_val == 42 &&
                       promoted.method_table_val.fallback != null &&
                       promoted.method_table_val.fallback.tag == INT &&
                       promoted.method_table_val.fallback.int_val == 33;

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary METHOD_TABLE clone path");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary METHOD_TABLE clone path (site_delta=%d pre_ok=%s post_ok=%s)",
                (long)site_delta,
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_boundary_method_table_abort_no_wrapper_slot_leak_test(Interp* interp, int* pass, int* fail) {
    // Invariant: failed root-store METHOD_TABLE clone does not consume root
    // wrapper allocation slots before the wrapper is actually committed.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: root-boundary method-table abort does not leak wrapper slots (setup child)");
            (*fail)++;
            return;
        }

        interp.current_scope = child;

        Value* local = interp.alloc_value();
        local.tag = METHOD_TABLE;
        main::scope_register_dtor(child, (void*)local, &scope_dtor_value);
        local.method_table_val = null;

        MethodTable* mt = (MethodTable*)mem::malloc(MethodTable.sizeof);
        if (mt == null) {
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary method-table abort does not leak wrapper slots (setup table)");
            (*fail)++;
            return;
        }
        mt.name = interp.symbols.intern("root-boundary-method-table-abort");
        mt.capacity = 1;
        mt.entry_count = 1;
        mt.fallback = null;
        mt.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof);
        if (mt.entries == null) {
            mem::free(mt);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary method-table abort does not leak wrapper slots (setup entries)");
            (*fail)++;
            return;
        }

        method_signature_init_empty(&mt.entries[0].sig);
        MethodSignature src_sig;
        method_signature_init_empty(&src_sig);
        if (!method_signature_alloc_arrays_in_scope(&src_sig, child, 1, 0) ||
            !method_signature_copy_to_heap(&mt.entries[0].sig, &src_sig)) {
            method_table_free_partial_heap(mt, mt.entry_count);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary method-table abort does not leak wrapper slots (setup signature)");
            (*fail)++;
            return;
        }
        mt.entries[0].implementation = make_int(interp, 2042);
        local.method_table_val = mt;

        usz alloc_before = interp.root_scope.alloc_count;
        g_method_signature_heap_copy_call_count = 0;
        g_method_signature_force_heap_copy_fail_on_call = 1;
        Value* first_promoted = root_store_clone_method_table_to_scope(
            local,
            interp,
            interp.root_scope,
            child
        );
        usz alloc_after_first = interp.root_scope.alloc_count;

        g_method_signature_heap_copy_call_count = 0;
        g_method_signature_force_heap_copy_fail_on_call = 1;
        Value* second_promoted = root_store_clone_method_table_to_scope(
            local,
            interp,
            interp.root_scope,
            child
        );
        usz alloc_after_second = interp.root_scope.alloc_count;
        g_method_signature_force_heap_copy_fail_on_call = 0;

        interp.current_scope = saved;
        main::scope_release(child);

        bool ok = first_promoted == null &&
                  second_promoted == null &&
                  alloc_after_first == alloc_before &&
                  alloc_after_second == alloc_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary method-table abort does not leak wrapper slots");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: root-boundary method-table abort does not leak wrapper slots (first_null=%s second_null=%s alloc_before=%d alloc_after_first=%d alloc_after_second=%d)",
                first_promoted == null ? "yes" : "no",
                second_promoted == null ? "yes" : "no",
                (long)alloc_before,
                (long)alloc_after_first,
                (long)alloc_after_second
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_boundary_primitive_clone_test(Interp* interp, int* pass, int* fail) {
    // Invariant: child-owned PRIMITIVE wrapper crossing root-store boundary is
    // cloned only when its payload is plain data; opaque user_data must fail
    // closed instead of aliasing through a raw struct copy.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: root-boundary PRIMITIVE clone path (setup child)");
            (*fail)++;
            return;
        }

        interp.current_scope = child;

        Value* local = interp.alloc_value();
        local.tag = PRIMITIVE;
        main::scope_register_dtor(child, (void*)local, &scope_dtor_value);
        local.prim_val = null;

        Primitive* prim = (Primitive*)mem::malloc(Primitive.sizeof);
        if (prim == null) {
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary PRIMITIVE clone path (setup primitive)");
            (*fail)++;
            return;
        }
        *prim = {};
        prim.name[0] = 'p';
        prim.name[1] = 0;
        prim.arity = 2;
        prim.tag = 123;
        local.prim_val = prim;

        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET);
        Value* promoted = promote_to_root_site(local, interp, COPY_SITE_JIT_EVAL_SET);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET) - base_site;

        bool pre_ok = promoted != null &&
                      promoted.tag == PRIMITIVE &&
                      promoted != local &&
                      promoted.prim_val != null &&
                      local.prim_val != null &&
                      promoted.prim_val != local.prim_val &&
                      promoted.prim_val.arity == 2 &&
                      promoted.prim_val.tag == 123 &&
                      promoted.prim_val.name[0] == 'p' &&
                      promoted.prim_val.name[1] == 0 &&
                      site_delta == 0;

        interp.current_scope = saved;
        main::scope_release(child);

        bool post_ok = promoted != null &&
                       promoted.tag == PRIMITIVE &&
                       promoted.prim_val != null &&
                       promoted.prim_val.arity == 2 &&
                       promoted.prim_val.tag == 123 &&
                       promoted.prim_val.name[0] == 'p' &&
                       promoted.prim_val.name[1] == 0;

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary PRIMITIVE clone path");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary PRIMITIVE clone path (site_delta=%d pre_ok=%s post_ok=%s)",
                (long)site_delta,
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: root-boundary PRIMITIVE opaque payload reject (setup child)");
            (*fail)++;
            return;
        }

        interp.current_scope = child;

        Value* local = interp.alloc_value();
        local.tag = PRIMITIVE;
        main::scope_register_dtor(child, (void*)local, &scope_dtor_value);
        local.prim_val = null;

        Primitive* prim = (Primitive*)mem::malloc(Primitive.sizeof);
        if (prim == null) {
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary PRIMITIVE opaque payload reject (setup primitive)");
            (*fail)++;
            return;
        }
        *prim = {};
        prim.name[0] = 'q';
        prim.name[1] = 0;
        prim.arity = 1;
        prim.tag = 321;
        int* opaque_payload = (int*)mem::malloc(int.sizeof);
        if (opaque_payload == null) {
            mem::free(prim);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary PRIMITIVE opaque payload reject (setup payload)");
            (*fail)++;
            return;
        }
        *opaque_payload = 77;
        prim.user_data = (void*)opaque_payload;
        local.prim_val = prim;

        usz base_site = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET);
        Value* promoted = promote_to_root_site(local, interp, COPY_SITE_JIT_EVAL_SET);
        usz site_delta = copy_to_parent_site_calls(COPY_SITE_JIT_EVAL_SET) - base_site;

        bool pre_ok = promoted != null &&
                      promoted.tag == ERROR &&
                      promoted.str_chars != null &&
                      str_contains(
                          promoted.str_chars[:promoted.str_len],
                          "root-store: refusing primitive clone with opaque user_data"
                      ) &&
                      site_delta == 0;

        interp.current_scope = saved;
        main::scope_release(child);

        bool post_ok = promoted != null &&
                       promoted.tag == ERROR &&
                       promoted.str_chars != null &&
                       str_contains(
                           promoted.str_chars[:promoted.str_len],
                           "root-store: refusing primitive clone with opaque user_data"
                       );

        bool ok = pre_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary PRIMITIVE opaque payload reject");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: root-boundary PRIMITIVE opaque payload reject (site_delta=%d pre_ok=%s post_ok=%s)",
                (long)site_delta,
                pre_ok ? "yes" : "no",
                post_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_root_boundary_shared_wrapper_partial_cleanup_test(Interp* interp, int* pass, int* fail) {
    // Invariant: root-store shared-wrapper clone failure unwinds already-copied
    // child ownership side effects instead of leaking closure-env retains.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        if (child == null) {
            io::printn("[FAIL] lifetime: root-boundary shared-wrapper partial cleanup");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__root_boundary_partial_cleanup_payload");

        interp.current_scope = child;

        Env* captured_env = make_env(interp, interp.global_env);
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 911), interp);
        }
        main::ScopeRegion* closure_env_scope = null;
        int closure_env_destroy_count = 0;
        Value* shared_closure =
            captured_env != null ? make_env_scope_closure_for_test(interp, captured_env, &closure_env_scope) : null;
        if (closure_env_scope != null) {
            main::scope_register_dtor(closure_env_scope, (void*)&closure_env_destroy_count, fn void(void* ptr) {
                int* counter = (int*)ptr;
                if (counter != null) (*counter)++;
            });
        }
        Value* shared_cons =
            shared_closure != null ? make_cons(interp, shared_closure, make_nil(interp)) : null;

        Value* opaque = interp.alloc_value();
        opaque.tag = PRIMITIVE;
        main::scope_register_dtor(child, (void*)opaque, &scope_dtor_value);
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)mem::malloc(int.sizeof);
        if (opaque_prim == null || opaque_payload == null) {
            if (opaque_prim != null) mem::free(opaque_prim);
            if (opaque_payload != null) mem::free(opaque_payload);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary shared-wrapper partial cleanup (setup opaque)");
            (*fail)++;
            return;
        }
        *opaque_prim = {};
        *opaque_payload = 912;
        opaque_prim.name[0] = 'r';
        opaque_prim.name[1] = 0;
        opaque_prim.arity = 1;
        opaque_prim.tag = 912;
        opaque_prim.user_data = (void*)opaque_payload;
        opaque.prim_val = opaque_prim;

        Value* array_src = interp.alloc_value();
        array_src.tag = ARRAY;
        main::scope_register_dtor(child, (void*)array_src, &scope_dtor_value);
        Array* array_payload = (Array*)mem::malloc(Array.sizeof);
        if (array_payload == null) {
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary shared-wrapper partial cleanup (setup array)");
            (*fail)++;
            return;
        }
        array_payload.capacity = 2;
        array_payload.length = 2;
        array_payload.items = (Value**)mem::malloc(Value*.sizeof * array_payload.capacity);
        if (array_payload.items == null) {
            mem::free(array_payload);
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary shared-wrapper partial cleanup (setup array items)");
            (*fail)++;
            return;
        }
        array_payload.items[0] = shared_cons;
        array_payload.items[1] = opaque;
        array_src.array_val = array_payload;

        Value* hashmap_src = interp.alloc_value();
        hashmap_src.tag = HASHMAP;
        main::scope_register_dtor(child, (void*)hashmap_src, &scope_dtor_value);
        HashMap* hashmap_payload = (HashMap*)mem::malloc(HashMap.sizeof);
        if (hashmap_payload == null) {
            interp.current_scope = saved;
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary shared-wrapper partial cleanup (setup hashmap)");
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
            main::scope_release(child);
            io::printn("[FAIL] lifetime: root-boundary shared-wrapper partial cleanup (setup hashmap entries)");
            (*fail)++;
            return;
        }
        for (uint i = 0; i < hashmap_payload.capacity; i++) {
            hashmap_payload.entries[i].key = null;
            hashmap_payload.entries[i].value = null;
        }
        Value* hashmap_key_good = make_int(interp, 13);
        Value* hashmap_key_bad = make_int(interp, 14);
        uint hashmap_idx_good = hash_value(hashmap_key_good) & hashmap_payload.mask;
        uint hashmap_idx_bad = hash_value(hashmap_key_bad) & hashmap_payload.mask;
        if (hashmap_idx_bad == hashmap_idx_good) {
            hashmap_idx_bad = (hashmap_idx_bad + 1) & hashmap_payload.mask;
        }
        hashmap_payload.count = 2;
        hashmap_payload.entries[hashmap_idx_good].key = hashmap_key_good;
        hashmap_payload.entries[hashmap_idx_good].value = shared_cons;
        hashmap_payload.entries[hashmap_idx_bad].key = hashmap_key_bad;
        hashmap_payload.entries[hashmap_idx_bad].value = opaque;
        hashmap_src.hashmap_val = hashmap_payload;

        bool setup_ok = shared_closure != null &&
                        shared_cons != null &&
                        shared_cons.tag == CONS &&
                        shared_cons.cons_val.car == shared_closure &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        opaque.prim_val != null &&
                        opaque.prim_val.user_data != null &&
                        array_src.array_val != null &&
                        array_src.array_val.length == 2 &&
                        hashmap_src.hashmap_val != null &&
                        hashmap_src.hashmap_val.entries != null;

        Value* array_promoted = setup_ok ? promote_to_root_site(array_src, interp, COPY_SITE_JIT_EVAL_SET) : null;
        int rc_after_array = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        Value* hashmap_promoted = setup_ok ? promote_to_root_site(hashmap_src, interp, COPY_SITE_JIT_EVAL_SET) : null;
        int rc_after_hashmap = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        int destroy_count_before_child_release = closure_env_destroy_count;

        interp.current_scope = saved;
        main::scope_release(child);
        int destroy_count_after_child_release = closure_env_destroy_count;

        bool array_ok = array_promoted != null &&
                        array_promoted.tag == ERROR &&
                        array_promoted.str_chars != null &&
                        str_contains(
                            array_promoted.str_chars[:array_promoted.str_len],
                            "boundary: refusing primitive copy with opaque user_data"
                        ) &&
                        rc_after_array == 1;
        bool hashmap_ok = hashmap_promoted != null &&
                          hashmap_promoted.tag == ERROR &&
                          hashmap_promoted.str_chars != null &&
                        str_contains(
                            hashmap_promoted.str_chars[:hashmap_promoted.str_len],
                            "boundary: refusing primitive copy with opaque user_data"
                          ) &&
                          rc_after_hashmap == 1;
        bool teardown_ok = destroy_count_before_child_release == 0 &&
                           destroy_count_after_child_release == 1;
        bool ok = setup_ok && array_ok && hashmap_ok && teardown_ok;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: root-boundary shared-wrapper partial cleanup");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: root-boundary shared-wrapper partial cleanup (setup_ok=%s array_ok=%s hashmap_ok=%s rc_array=%d rc_hashmap=%d destroy_before_child_release=%d destroy_after_child_release=%d)",
                setup_ok ? "yes" : "no",
                array_ok ? "yes" : "no",
                hashmap_ok ? "yes" : "no",
                rc_after_array,
                rc_after_hashmap,
                destroy_count_before_child_release,
                destroy_count_after_child_release
            );
            (*fail)++;
        }
    }
}
