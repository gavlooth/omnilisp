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
        src_sig.param_types[0] = interp.tid_Int;
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
                      promoted.method_table_val.entries[0].sig.param_types[0] == interp.tid_Int &&
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

fn void run_memory_lifetime_root_boundary_primitive_clone_test(Interp* interp, int* pass, int* fail) {
    // Invariant: child-owned PRIMITIVE wrapper crossing root-store boundary is
    // cloned (wrapper + Primitive payload) to avoid shared-ownership aliasing.
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
}
