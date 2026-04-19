module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_target_chain_shared_wrapper_payload_alias_test(Interp* interp, int* pass, int* fail) {
    // Invariant: a shared wrapper already in the target chain must still clone
    // when one nested child remains owned by the releasing scope, including
    // when that wrapper is nested inside PARTIAL_PRIM / ITERATOR payloads.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = main::scope_create(saved);
        if (target == null || releasing == null) {
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: target-chain shared wrapper payload alias policy");
            (*fail)++;
            return;
        }

        interp.current_scope = target;

        Value* array_src = interp.alloc_value();
        array_src.tag = ARRAY;
        main::scope_register_dtor(target, (void*)array_src, &scope_dtor_value);
        Array* array_payload = (Array*)mem::malloc(Array.sizeof);
        if (array_payload == null) {
            interp.current_scope = saved;
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: target-chain shared wrapper payload alias policy (setup array)");
            (*fail)++;
            return;
        }
        array_payload.capacity = 4;
        array_payload.length = 1;
        array_payload.items = (Value**)mem::malloc(Value*.sizeof * array_payload.capacity);
        if (array_payload.items == null) {
            mem::free(array_payload);
            interp.current_scope = saved;
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: target-chain shared wrapper payload alias policy (setup array items)");
            (*fail)++;
            return;
        }
        for (usz i = 0; i < array_payload.capacity; i++) array_payload.items[i] = null;
        array_src.array_val = array_payload;

        Value* hashmap_src = interp.alloc_value();
        hashmap_src.tag = HASHMAP;
        main::scope_register_dtor(target, (void*)hashmap_src, &scope_dtor_value);
        HashMap* hashmap_payload = (HashMap*)mem::malloc(HashMap.sizeof);
        if (hashmap_payload == null) {
            interp.current_scope = saved;
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: target-chain shared wrapper payload alias policy (setup hashmap)");
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
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: target-chain shared wrapper payload alias policy (setup hashmap entries)");
            (*fail)++;
            return;
        }
        for (uint i = 0; i < hashmap_payload.capacity; i++) {
            hashmap_payload.entries[i].key = null;
            hashmap_payload.entries[i].value = null;
        }
        Value* hashmap_key = make_int(interp, 801);
        uint hashmap_idx = hash_value(hashmap_key) & hashmap_payload.mask;
        hashmap_payload.entries[hashmap_idx].key = hashmap_key;
        hashmap_src.hashmap_val = hashmap_payload;

        Value* set_src = interp.alloc_value();
        set_src.tag = SET;
        main::scope_register_dtor(target, (void*)set_src, &scope_dtor_value);
        HashMap* set_payload = (HashMap*)mem::malloc(HashMap.sizeof);
        if (set_payload == null) {
            interp.current_scope = saved;
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: target-chain shared wrapper payload alias policy (setup set)");
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
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: target-chain shared wrapper payload alias policy (setup set entries)");
            (*fail)++;
            return;
        }
        for (uint i = 0; i < set_payload.capacity; i++) {
            set_payload.entries[i].key = null;
            set_payload.entries[i].value = null;
        }
        uint set_idx = hash_value(make_int(interp, 802)) & set_payload.mask;
        set_src.hashmap_val = set_payload;

        Value* mt_src = interp.alloc_value();
        mt_src.tag = METHOD_TABLE;
        main::scope_register_dtor(target, (void*)mt_src, &scope_dtor_value);
        MethodTable* mt_payload = (MethodTable*)mem::malloc(MethodTable.sizeof);
        if (mt_payload == null) {
            interp.current_scope = saved;
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: target-chain shared wrapper payload alias policy (setup method table)");
            (*fail)++;
            return;
        }
        mt_payload.name = interp.symbols.intern("__target_chain_wrapper_mt");
        mt_payload.capacity = 1;
        mt_payload.entry_count = 1;
        mt_payload.fallback = null;
        mt_payload.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * mt_payload.capacity);
        if (mt_payload.entries == null) {
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: target-chain shared wrapper payload alias policy (setup method table entries)");
            (*fail)++;
            return;
        }
        method_signature_init_empty(&mt_payload.entries[0].sig);
        mt_payload.entries[0].implementation = null;
        MethodSignature mt_sig_src;
        method_signature_init_empty(&mt_sig_src);
        if (!method_signature_alloc_arrays_in_scope(&mt_sig_src, target, 1, 0)) {
            mem::free(mt_payload.entries);
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: target-chain shared wrapper payload alias policy (setup method table signature)");
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
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: target-chain shared wrapper payload alias policy (copy method signature)");
            (*fail)++;
            return;
        }
        mt_src.method_table_val = mt_payload;

        Value* partial_src = interp.alloc_value();
        partial_src.tag = PARTIAL_PRIM;
        partial_src.partial_val.func = &prim_add;
        partial_src.partial_val.remaining = 1;
        partial_src.partial_val.first_arg = array_src;
        partial_src.partial_val.second_arg = make_int(interp, 903);

        Value* iter_src = make_iterator(interp, partial_src);
        if (iter_src == null) {
            interp.current_scope = saved;
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: target-chain shared wrapper payload alias policy (setup iterator)");
            (*fail)++;
            return;
        }

        interp.current_scope = releasing;
        Value* array_child = make_int(interp, 1701);
        Value* hashmap_value = make_int(interp, 1702);
        Value* set_key = make_int(interp, 1703);
        Value* mt_impl = make_int(interp, 1704);
        Value* mt_fallback = make_int(interp, 1705);

        interp.current_scope = target;
        array_payload.items[0] = array_child;
        hashmap_payload.entries[hashmap_idx].value = hashmap_value;
        set_payload.entries[set_idx].key = set_key;
        set_payload.entries[set_idx].value = make_int(interp, 1);
        mt_payload.entries[0].implementation = mt_impl;
        mt_payload.fallback = mt_fallback;

        interp.releasing_scope = releasing;

        Value* array_copied = copy_to_parent(array_src, interp);
        Value* hashmap_copied = copy_to_parent(hashmap_src, interp);
        Value* set_copied = copy_to_parent(set_src, interp);
        Value* mt_copied = copy_to_parent(mt_src, interp);
        Value* partial_copied = copy_to_parent(partial_src, interp);
        Value* iter_copied = copy_to_parent(iter_src, interp);
        Value* array_escaped = promote_to_escape(array_src, interp);
        Value* hashmap_escaped = promote_to_escape(hashmap_src, interp);
        Value* set_escaped = promote_to_escape(set_src, interp);
        Value* mt_escaped = promote_to_escape(mt_src, interp);
        Value* partial_escaped = promote_to_escape(partial_src, interp);
        Value* iter_escaped = promote_to_escape(iter_src, interp);

        bool array_copy_ok = array_copied != null &&
            array_copied != array_src &&
            array_copied.tag == ARRAY &&
            array_copied.array_val != null &&
            array_copied.array_val.items != null &&
            array_copied.array_val.items[0] != null &&
            array_copied.array_val.items[0] != array_child &&
            array_copied.array_val.items[0].tag == INT &&
            array_copied.array_val.items[0].int_val == 1701;

        Value* copied_hashmap_value = null;
        if (hashmap_copied != null && hashmap_copied.tag == HASHMAP && hashmap_copied.hashmap_val != null) {
            copied_hashmap_value = hashmap_copied.hashmap_val.entries[hashmap_idx].value;
        }
        bool hashmap_copy_ok = hashmap_copied != null &&
            hashmap_copied != hashmap_src &&
            hashmap_copied.tag == HASHMAP &&
            hashmap_copied.hashmap_val != null &&
            copied_hashmap_value != null &&
            copied_hashmap_value != hashmap_value &&
            copied_hashmap_value.tag == INT &&
            copied_hashmap_value.int_val == 1702;

        Value* copied_set_key = null;
        if (set_copied != null && set_copied.tag == SET && set_copied.hashmap_val != null) {
            copied_set_key = set_copied.hashmap_val.entries[set_idx].key;
        }
        bool set_copy_ok = set_copied != null &&
            set_copied != set_src &&
            set_copied.tag == SET &&
            set_copied.hashmap_val != null &&
            copied_set_key != null &&
            copied_set_key != set_key &&
            copied_set_key.tag == INT &&
            copied_set_key.int_val == 1703;

        bool mt_copy_ok = mt_copied != null &&
            mt_copied != mt_src &&
            mt_copied.tag == METHOD_TABLE &&
            mt_copied.method_table_val != null &&
            mt_copied.method_table_val.entries != null &&
            mt_copied.method_table_val.entries[0].implementation != null &&
            mt_copied.method_table_val.entries[0].implementation != mt_impl &&
            mt_copied.method_table_val.entries[0].implementation.tag == INT &&
            mt_copied.method_table_val.entries[0].implementation.int_val == 1704 &&
            mt_copied.method_table_val.fallback != null &&
            mt_copied.method_table_val.fallback != mt_fallback &&
            mt_copied.method_table_val.fallback.tag == INT &&
            mt_copied.method_table_val.fallback.int_val == 1705;

        bool array_escape_ok = array_escaped != null &&
            array_escaped != array_src &&
            array_escaped.tag == ARRAY &&
            array_escaped.scope_gen == target.escape_generation &&
            array_escaped.array_val != null &&
            array_escaped.array_val.items != null &&
            array_escaped.array_val.items[0] != null &&
            array_escaped.array_val.items[0] != array_child &&
            array_escaped.array_val.items[0].tag == INT &&
            array_escaped.array_val.items[0].int_val == 1701;

        bool hashmap_escape_ok = hashmap_escaped != null &&
            hashmap_escaped != hashmap_src &&
            hashmap_escaped.tag == HASHMAP &&
            hashmap_escaped.scope_gen == target.escape_generation &&
            hashmap_escaped.hashmap_val != null &&
            hashmap_escaped.hashmap_val.entries[hashmap_idx].value != null &&
            hashmap_escaped.hashmap_val.entries[hashmap_idx].value != hashmap_value &&
            hashmap_escaped.hashmap_val.entries[hashmap_idx].value.tag == INT &&
            hashmap_escaped.hashmap_val.entries[hashmap_idx].value.int_val == 1702;

        bool set_escape_ok = set_escaped != null &&
            set_escaped != set_src &&
            set_escaped.tag == SET &&
            set_escaped.scope_gen == target.escape_generation &&
            set_escaped.hashmap_val != null &&
            set_escaped.hashmap_val.entries[set_idx].key != null &&
            set_escaped.hashmap_val.entries[set_idx].key != set_key &&
            set_escaped.hashmap_val.entries[set_idx].key.tag == INT &&
            set_escaped.hashmap_val.entries[set_idx].key.int_val == 1703;

        bool mt_escape_ok = mt_escaped != null &&
            mt_escaped != mt_src &&
            mt_escaped.tag == METHOD_TABLE &&
            mt_escaped.scope_gen == target.escape_generation &&
            mt_escaped.method_table_val != null &&
            mt_escaped.method_table_val.entries != null &&
            mt_escaped.method_table_val.entries[0].implementation != null &&
            mt_escaped.method_table_val.entries[0].implementation != mt_impl &&
            mt_escaped.method_table_val.entries[0].implementation.tag == INT &&
            mt_escaped.method_table_val.entries[0].implementation.int_val == 1704 &&
            mt_escaped.method_table_val.fallback != null &&
            mt_escaped.method_table_val.fallback != mt_fallback &&
            mt_escaped.method_table_val.fallback.tag == INT &&
            mt_escaped.method_table_val.fallback.int_val == 1705;

        Value* partial_copy_first = partial_copied != null && partial_copied.tag == PARTIAL_PRIM
            ? partial_copied.partial_val.first_arg
            : null;
        Value* partial_escape_first = partial_escaped != null && partial_escaped.tag == PARTIAL_PRIM
            ? partial_escaped.partial_val.first_arg
            : null;
        Value* iter_copy_partial = iter_copied != null && iter_copied.tag == ITERATOR
            ? iter_copied.iterator_val
            : null;
        Value* iter_escape_partial = iter_escaped != null && iter_escaped.tag == ITERATOR
            ? iter_escaped.iterator_val
            : null;
        Value* iter_copy_first = iter_copy_partial != null && iter_copy_partial.tag == PARTIAL_PRIM
            ? iter_copy_partial.partial_val.first_arg
            : null;
        Value* iter_escape_first = iter_escape_partial != null && iter_escape_partial.tag == PARTIAL_PRIM
            ? iter_escape_partial.partial_val.first_arg
            : null;

        bool partial_copy_ok = partial_copied != null &&
            partial_copied != partial_src &&
            partial_copied.tag == PARTIAL_PRIM &&
            partial_copy_first != null &&
            partial_copy_first != array_src &&
            partial_copy_first.tag == ARRAY &&
            partial_copy_first.array_val != null &&
            partial_copy_first.array_val.items != null &&
            partial_copy_first.array_val.items[0] != null &&
            partial_copy_first.array_val.items[0] != array_child &&
            partial_copy_first.array_val.items[0].tag == INT &&
            partial_copy_first.array_val.items[0].int_val == 1701;

        bool iter_copy_ok = iter_copied != null &&
            iter_copied != iter_src &&
            iter_copied.tag == ITERATOR &&
            iter_copy_partial != null &&
            iter_copy_partial != partial_src &&
            iter_copy_partial.tag == PARTIAL_PRIM &&
            iter_copy_first != null &&
            iter_copy_first != array_src &&
            iter_copy_first.tag == ARRAY &&
            iter_copy_first.array_val != null &&
            iter_copy_first.array_val.items != null &&
            iter_copy_first.array_val.items[0] != null &&
            iter_copy_first.array_val.items[0] != array_child &&
            iter_copy_first.array_val.items[0].tag == INT &&
            iter_copy_first.array_val.items[0].int_val == 1701;

        bool partial_escape_ok = partial_escaped != null &&
            partial_escaped != partial_src &&
            partial_escaped.tag == PARTIAL_PRIM &&
            partial_escaped.scope_gen == target.escape_generation &&
            partial_escape_first != null &&
            partial_escape_first != array_src &&
            partial_escape_first.tag == ARRAY &&
            partial_escape_first.scope_gen == target.escape_generation &&
            partial_escape_first.array_val != null &&
            partial_escape_first.array_val.items != null &&
            partial_escape_first.array_val.items[0] != null &&
            partial_escape_first.array_val.items[0] != array_child &&
            partial_escape_first.array_val.items[0].tag == INT &&
            partial_escape_first.array_val.items[0].int_val == 1701;

        bool iter_escape_ok = iter_escaped != null &&
            iter_escaped != iter_src &&
            iter_escaped.tag == ITERATOR &&
            iter_escaped.scope_gen == target.escape_generation &&
            iter_escape_partial != null &&
            iter_escape_partial != partial_src &&
            iter_escape_partial.tag == PARTIAL_PRIM &&
            iter_escape_partial.scope_gen == target.escape_generation &&
            iter_escape_first != null &&
            iter_escape_first != array_src &&
            iter_escape_first.tag == ARRAY &&
            iter_escape_first.scope_gen == target.escape_generation &&
            iter_escape_first.array_val != null &&
            iter_escape_first.array_val.items != null &&
            iter_escape_first.array_val.items[0] != null &&
            iter_escape_first.array_val.items[0] != array_child &&
            iter_escape_first.array_val.items[0].tag == INT &&
            iter_escape_first.array_val.items[0].int_val == 1701;

        main::scope_release(releasing);

        bool post_ok = array_copied != null &&
            array_copied.tag == ARRAY &&
            array_copied.array_val != null &&
            array_copied.array_val.items[0] != null &&
            array_copied.array_val.items[0].tag == INT &&
            array_copied.array_val.items[0].int_val == 1701 &&
            partial_copied != null &&
            partial_copied.tag == PARTIAL_PRIM &&
            partial_copy_first != null &&
            partial_copy_first.tag == ARRAY &&
            partial_copy_first.array_val != null &&
            partial_copy_first.array_val.items != null &&
            partial_copy_first.array_val.items[0] != null &&
            partial_copy_first.array_val.items[0].tag == INT &&
            partial_copy_first.array_val.items[0].int_val == 1701 &&
            mt_copied != null &&
            mt_copied.tag == METHOD_TABLE &&
            mt_copied.method_table_val != null &&
            mt_copied.method_table_val.entries[0].implementation != null &&
            mt_copied.method_table_val.entries[0].implementation.tag == INT &&
            mt_copied.method_table_val.entries[0].implementation.int_val == 1704;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = array_copy_ok && hashmap_copy_ok && set_copy_ok && mt_copy_ok &&
            partial_copy_ok && iter_copy_ok &&
            array_escape_ok && hashmap_escape_ok && set_escape_ok && mt_escape_ok &&
            partial_escape_ok && iter_escape_ok &&
            post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: target-chain shared wrapper payload alias policy");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: target-chain shared wrapper payload alias policy (array_copy=%s hashmap_copy=%s set_copy=%s mt_copy=%s partial_copy=%s iter_copy=%s array_escape=%s hashmap_escape=%s set_escape=%s mt_escape=%s partial_escape=%s iter_escape=%s post_ok=%s)",
                array_copy_ok ? "yes" : "no",
                hashmap_copy_ok ? "yes" : "no",
                set_copy_ok ? "yes" : "no",
                mt_copy_ok ? "yes" : "no",
                partial_copy_ok ? "yes" : "no",
                iter_copy_ok ? "yes" : "no",
                array_escape_ok ? "yes" : "no",
                hashmap_escape_ok ? "yes" : "no",
                set_escape_ok ? "yes" : "no",
                mt_escape_ok ? "yes" : "no",
                partial_escape_ok ? "yes" : "no",
                iter_escape_ok ? "yes" : "no",
                post_ok ? "yes" : "no"
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_nested_wrapper_boundary_copy_fail_closed_test(Interp* interp, int* pass, int* fail) {
    // Invariant: nested CONS/PARTIAL_PRIM/ITERATOR boundary copies fail closed
    // when they transitively encounter an opaque primitive payload.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: nested boundary copy fail-closed");
            (*fail)++;
            return;
        }

        interp.current_scope = foreign;
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
            main::scope_release(target);
            io::printn("[FAIL] lifetime: nested boundary copy fail-closed (setup opaque)");
            (*fail)++;
            return;
        }
        *opaque_prim = {};
        *opaque_payload = 701;
        opaque_prim.name[0] = 'n';
        opaque_prim.name[1] = 0;
        opaque_prim.arity = 2;
        opaque_prim.tag = 701;
        opaque_prim.user_data = (void*)opaque_payload;
        opaque.prim_val = opaque_prim;

        Value* partial = interp.alloc_value();
        partial.tag = PARTIAL_PRIM;
        partial.partial_val.func = &prim_add;
        partial.partial_val.remaining = 1;
        partial.partial_val.first_arg = opaque;
        partial.partial_val.second_arg = make_int(interp, 5);

        Value* iter = make_iterator(interp, partial);
        Value* cons = make_cons(interp, iter, make_nil(interp));

        interp.current_scope = target;
        interp.releasing_scope = foreign;

        BoundaryCopyResult cons_copy = boundary_copy_to_parent_site_ctx_checked(cons, interp, COPY_SITE_GENERIC, null);
        BoundaryCopyResult partial_copy = boundary_copy_to_parent_site_ctx_checked(partial, interp, COPY_SITE_GENERIC, null);
        BoundaryCopyResult iter_copy = boundary_copy_to_parent_site_ctx_checked(iter, interp, COPY_SITE_GENERIC, null);

        bool cons_ok = !boundary_copy_result_ok(cons_copy) &&
                       cons_copy.value == null &&
                       cons_copy.fault_code == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;
        bool partial_ok = !boundary_copy_result_ok(partial_copy) &&
                          partial_copy.value == null &&
                          partial_copy.fault_code == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;
        bool iter_ok = !boundary_copy_result_ok(iter_copy) &&
                       iter_copy.value == null &&
                       iter_copy.fault_code == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD;

        main::scope_release(foreign);

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(target);

        bool ok = cons_ok && partial_ok && iter_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested boundary copy fail-closed");
            (*pass)++;
        } else {
            io::printfn("[FAIL] lifetime: nested boundary copy fail-closed (cons_ok=%s partial_ok=%s iter_ok=%s)",
                cons_ok ? "yes" : "no",
                partial_ok ? "yes" : "no",
                iter_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}
