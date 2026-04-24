module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_runtime_alloc_groups_checked_collections_tests(Interp* interp, int* pass, int* fail) {
    {
        Value* array_value = make_array(interp, 1);
        bool setup_ok = array_value != null && array_value.tag == ARRAY && array_value.array_val != null;
        if (setup_ok) {
            array_value.array_val.items[0] = make_int(interp, 1);
            array_value.array_val.length = array_value.array_val.capacity;
            for (usz i = 1; i < array_value.array_val.length; i++) {
                array_value.array_val.items[i] = make_int(interp, (long)(i + 1));
            }
        }

        Value*[2] push_args = { array_value, make_int(interp, 2) };
        bool saved_grow_fail = g_array_push_force_grow_alloc_fail;
        g_array_push_force_grow_alloc_fail = true;
        Value* push_fail = setup_ok ? prim_array_push(push_args[0:2], null, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        g_array_push_force_grow_alloc_fail = saved_grow_fail;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(push_fail, "push!: array backing storage out of memory") &&
            array_value.array_val.length == array_value.array_val.capacity &&
            array_value.array_val.items[0] != null &&
            array_value.array_val.items[0].tag == INT &&
            array_value.array_val.items[0].int_val == 1;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: array push grow failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: array push grow failures fail closed (setup_ok=%s msg=%s array_intact=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(push_fail),
                setup_ok &&
                    array_value != null &&
                    array_value.tag == ARRAY &&
                    array_value.array_val != null &&
                    array_value.array_val.length == array_value.array_val.capacity &&
                    array_value.array_val.items[0] != null &&
                    array_value.array_val.items[0].tag == INT &&
                    array_value.array_val.items[0].int_val == 1 ? "yes" : "no");
            (*fail)++;
        }
    }

    {
        Value*[2] dict_args = { make_symbol(interp, interp.symbols.intern("k")), make_int(interp, 1) };
        Value*[2] set_args = { make_int(interp, 1), make_int(interp, 2) };

        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        bool saved_entries_fail = g_hashmap_force_entries_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* dict_fail = prim_dict(dict_args[0:2], null, interp);
        g_hashmap_force_struct_alloc_fail = false;
        g_hashmap_force_entries_alloc_fail = true;
        Value* set_fail = prim_set(set_args[0:2], null, interp);
        g_hashmap_force_entries_alloc_fail = saved_entries_fail;
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        Value* checked_map = make_hashmap_checked(interp, 2, "hashmap test: ctor oom");
        bool grow_ok = false;
        bool no_slot_fail_cleans_promoted_values = false;
        if (checked_map != null && checked_map.tag == HASHMAP) {
            bool saved_grow_fail = g_hashmap_force_grow_entries_alloc_fail;
            grow_ok =
                hashmap_set_checked(checked_map.hashmap_val, make_int(interp, 1), make_int(interp, 1), interp) &&
                hashmap_set_checked(checked_map.hashmap_val, make_int(interp, 2), make_int(interp, 2), interp);
            g_hashmap_force_grow_entries_alloc_fail = true;
            bool grow_fail = hashmap_set_checked(checked_map.hashmap_val, make_int(interp, 3), make_int(interp, 3), interp);
            g_hashmap_force_grow_entries_alloc_fail = saved_grow_fail;
            grow_ok = grow_ok && !grow_fail;

            if (checked_map.hashmap_val != null && checked_map.hashmap_val.capacity == 2) {
                uint saved_count = checked_map.hashmap_val.count;
                checked_map.hashmap_val.count = 0;
                main::ScopeRegion* saved_scope = interp.current_scope;
                main::ScopeRegion* child = main::scope_create(saved_scope);
                usz root_values_before = runtime_alloc_active_value_dtor_count(interp.root_scope);
                bool no_slot_fail = true;
                if (child != null) {
                    interp.current_scope = child;
                    Value* nested_value = boundary_alloc_value_in_scope(interp, child, true);
                    Array* nested_array = (Array*)mem::malloc(Array.sizeof);
                    Value* nested_item = boundary_alloc_value_in_scope(interp, child, true);
                    bool nested_array_attached = false;
                    if (nested_value != null && nested_array != null && nested_item != null) {
                        nested_array.capacity = 4;
                        nested_array.length = 1;
                        nested_array.items = (Value**)mem::malloc(Value*.sizeof * nested_array.capacity);
                        if (nested_array.items != null) {
                            for (usz i = 0; i < nested_array.capacity; i++) nested_array.items[i] = null;
                            nested_item.tag = INT;
                            nested_item.int_val = 100;
                            nested_array.items[0] = nested_item;
                            nested_value.tag = ARRAY;
                            nested_value.array_val = nested_array;
                            nested_array_attached = true;
                        }
                    }
                    if (nested_array != null && !nested_array_attached) mem::free(nested_array);
                    no_slot_fail = hashmap_set_checked(
                        checked_map.hashmap_val,
                        make_int(interp, 99),
                        nested_value,
                        interp
                    );
                    interp.current_scope = saved_scope;
                }
                usz root_values_after = runtime_alloc_active_value_dtor_count(interp.root_scope);
                if (child != null) main::scope_release(child);
                checked_map.hashmap_val.count = saved_count;
                no_slot_fail_cleans_promoted_values =
                    child != null &&
                    !no_slot_fail &&
                    root_values_after == root_values_before;
            }
        }

        bool ok =
            runtime_alloc_error_matches(dict_fail, "Dictionary: out of memory") &&
            runtime_alloc_error_matches(set_fail, "Set: out of memory") &&
            grow_ok &&
            no_slot_fail_cleans_promoted_values;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: checked collection constructors fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: checked collection constructors fail closed (dict_ok=%s set_ok=%s grow_ok=%s no_slot_cleanup=%s dict_msg=%s set_msg=%s)",
                runtime_alloc_error_matches(dict_fail, "Dictionary: out of memory") ? "yes" : "no",
                runtime_alloc_error_matches(set_fail, "Set: out of memory") ? "yes" : "no",
                grow_ok ? "yes" : "no",
                no_slot_fail_cleans_promoted_values ? "yes" : "no",
                runtime_alloc_error_text(dict_fail),
                runtime_alloc_error_text(set_fail));
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* foreign = main::scope_create(saved);
        Value* checked_map = make_hashmap_checked(interp, 4, "hashmap test: ctor oom");
        Value* set_map = make_set_checked(interp, 4, "set test: ctor oom");
        bool setup_ok = foreign != null &&
            checked_map != null && checked_map.tag == HASHMAP &&
            set_map != null && set_map.tag == SET;

        Value* opaque_value = null;
        Value* opaque_key = null;
        Primitive* opaque_value_prim = null;
        Primitive* opaque_key_prim = null;
        int* opaque_value_payload = null;
        int* opaque_key_payload = null;

        if (setup_ok) {
            interp.current_scope = foreign;
            opaque_value = interp.alloc_value();
            opaque_key = interp.alloc_value();
            opaque_value_prim = (Primitive*)mem::malloc(Primitive.sizeof);
            opaque_key_prim = (Primitive*)mem::malloc(Primitive.sizeof);
            opaque_value_payload = (int*)memory_lifetime_test_opaque_payload(interp.current_scope);
            opaque_key_payload = (int*)memory_lifetime_test_opaque_payload(interp.current_scope);
            if (opaque_value != null) {
                opaque_value.tag = PRIMITIVE;
                main::scope_register_dtor(foreign, (void*)opaque_value, &scope_dtor_value);
            }
            if (opaque_key != null) {
                opaque_key.tag = PRIMITIVE;
                main::scope_register_dtor(foreign, (void*)opaque_key, &scope_dtor_value);
            }
            setup_ok = opaque_value != null &&
                opaque_key != null &&
                opaque_value_prim != null &&
                opaque_key_prim != null &&
                opaque_value_payload != null &&
                opaque_key_payload != null;
        }

        if (setup_ok) {
            *opaque_value_prim = {};
            *opaque_key_prim = {};
            *opaque_value_payload = 911;
            *opaque_key_payload = 912;
            opaque_value_prim.name[0] = 'v';
            opaque_value_prim.name[1] = 0;
            opaque_value_prim.func = &prim_add;
            opaque_value_prim.arity = 2;
            opaque_value_prim.tag = 911;
            opaque_value_prim.user_data = (void*)opaque_value_payload;
            opaque_key_prim.name[0] = 'k';
            opaque_key_prim.name[1] = 0;
            opaque_key_prim.func = &prim_add;
            opaque_key_prim.arity = 2;
            opaque_key_prim.tag = 912;
            opaque_key_prim.user_data = (void*)opaque_key_payload;
            opaque_value.prim_val = opaque_value_prim;
            opaque_key.prim_val = opaque_key_prim;
        }

        bool symbol_insert_ok = false;
        bool value_insert_ok = false;
        if (setup_ok) {
            interp.current_scope = foreign;
            symbol_insert_ok = hashmap_set_symbol_checked(set_map.hashmap_val, interp.symbols.intern("bad"), opaque_value, interp);
            value_insert_ok = hashmap_set_checked(checked_map.hashmap_val, opaque_key, make_int(interp, 1), interp);
            interp.current_scope = saved;
        }

        Value* symbol_lookup = setup_ok
            ? hashmap_get(set_map.hashmap_val, make_symbol(interp, interp.symbols.intern("bad")))
            : null;
        Value* value_lookup = setup_ok ? hashmap_get(checked_map.hashmap_val, opaque_key) : null;

        bool ok = setup_ok &&
            !symbol_insert_ok &&
            !value_insert_ok &&
            set_map.hashmap_val.count == 0 &&
            checked_map.hashmap_val.count == 0 &&
            symbol_lookup == null &&
            value_lookup == null;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: checked hashmap insertion rejects promoted error state");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: checked hashmap insertion rejects promoted error state (setup_ok=%s symbol_ok=%s value_ok=%s set_count=%d dict_count=%d symbol_missing=%s value_missing=%s)",
                setup_ok ? "yes" : "no",
                symbol_insert_ok ? "yes" : "no",
                value_insert_ok ? "yes" : "no",
                setup_ok ? (int)set_map.hashmap_val.count : -1,
                setup_ok ? (int)checked_map.hashmap_val.count : -1,
                symbol_lookup == null ? "yes" : "no",
                value_lookup == null ? "yes" : "no");
            (*fail)++;
        }

        if (foreign != null) main::scope_release(foreign);
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* foreign = main::scope_create(saved);
        Value* fs_array = make_array_checked(interp, 4, "fs-array test: ctor oom");
        Value* csv_array = make_array_checked(interp, 4, "csv-array test: ctor oom");
        Value* opaque_value = null;
        Primitive* opaque_prim = null;
        int* opaque_payload = null;
        bool setup_ok = foreign != null &&
            fs_array != null && fs_array.tag == ARRAY &&
            csv_array != null && csv_array.tag == ARRAY;

        if (setup_ok) {
            interp.current_scope = foreign;
            opaque_value = interp.alloc_value();
            opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
            opaque_payload = (int*)memory_lifetime_test_opaque_payload(interp.current_scope);
            if (opaque_value != null) {
                opaque_value.tag = PRIMITIVE;
                main::scope_register_dtor(foreign, (void*)opaque_value, &scope_dtor_value);
            }
            setup_ok = opaque_value != null && opaque_prim != null && opaque_payload != null;
        }

        if (setup_ok) {
            *opaque_prim = {};
            *opaque_payload = 913;
            opaque_prim.name[0] = 'p';
            opaque_prim.name[1] = 0;
            opaque_prim.func = &prim_add;
            opaque_prim.arity = 2;
            opaque_prim.tag = 913;
            opaque_prim.user_data = (void*)opaque_payload;
            opaque_value.prim_val = opaque_prim;
        }

        bool fs_push_ok = false;
        bool csv_push_ok = false;
        if (setup_ok) {
            interp.current_scope = foreign;
            fs_push_ok = fs_array_push(fs_array, opaque_value, interp);
            csv_push_ok = csv_array_push(csv_array, opaque_value, interp);
            interp.current_scope = saved;
        }

        bool ok = setup_ok &&
            !fs_push_ok &&
            !csv_push_ok &&
            fs_array.array_val.length == 0 &&
            csv_array.array_val.length == 0;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: array helper pushes reject promoted error state");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: array helper pushes reject promoted error state (setup_ok=%s fs_ok=%s csv_ok=%s fs_len=%d csv_len=%d)",
                setup_ok ? "yes" : "no",
                fs_push_ok ? "yes" : "no",
                csv_push_ok ? "yes" : "no",
                setup_ok ? (int)fs_array.array_val.length : -1,
                setup_ok ? (int)csv_array.array_val.length : -1);
            (*fail)++;
        }

        if (foreign != null) main::scope_release(foreign);
    }

    {
        Value* dict_value = make_hashmap_checked(interp, 4, "dict mutator test: ctor oom");
        Value* set_value = make_set_checked(interp, 4, "set mutator test: ctor oom");
        bool setup_ok = dict_value != null &&
            dict_value.tag == HASHMAP &&
            set_value != null &&
            set_value.tag == SET;

        if (setup_ok) {
            setup_ok =
                hashmap_set_checked(dict_value.hashmap_val, make_int(interp, 1), make_int(interp, 11), interp) &&
                hashmap_set_checked(dict_value.hashmap_val, make_int(interp, 2), make_int(interp, 22), interp) &&
                hashmap_set_checked(dict_value.hashmap_val, make_int(interp, 3), make_int(interp, 33), interp) &&
                hashmap_set_checked(set_value.hashmap_val, make_int(interp, 1), make_symbol(interp, interp.sym_true), interp) &&
                hashmap_set_checked(set_value.hashmap_val, make_int(interp, 2), make_symbol(interp, interp.sym_true), interp) &&
                hashmap_set_checked(set_value.hashmap_val, make_int(interp, 3), make_symbol(interp, interp.sym_true), interp);
        }

        Value*[3] dict_set_args = { dict_value, make_int(interp, 4), make_int(interp, 44) };
        Value*[2] set_add_args = { set_value, make_int(interp, 4) };

        bool saved_grow_fail = g_hashmap_force_grow_entries_alloc_fail;
        g_hashmap_force_grow_entries_alloc_fail = true;
        Value* dict_set_fail = setup_ok ? prim_dict_set(dict_set_args[0:3], null, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        Value* set_add_fail = setup_ok ? prim_set_add(set_add_args[0:2], null, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        g_hashmap_force_grow_entries_alloc_fail = saved_grow_fail;

        Value* dict_lookup = setup_ok ? hashmap_get(dict_value.hashmap_val, make_int(interp, 4)) : null;
        Value* set_lookup = setup_ok ? hashmap_get(set_value.hashmap_val, make_int(interp, 4)) : null;
        bool ok = setup_ok &&
            runtime_alloc_error_matches(dict_set_fail, "set!: dict backing storage out of memory") &&
            runtime_alloc_error_matches(set_add_fail, "set-add: set backing storage out of memory") &&
            dict_lookup == null &&
            set_lookup == null;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: checked collection mutators fail closed on grow");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: checked collection mutators fail closed on grow (setup_ok=%s dict=%s set=%s dict_missing=%s set_missing=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(dict_set_fail),
                runtime_alloc_error_text(set_add_fail),
                dict_lookup == null ? "yes" : "no",
                set_lookup == null ? "yes" : "no");
            (*fail)++;
        }
    }

    {
        Value* dict_value = make_hashmap_checked(interp, 8, "dict canonical order test: ctor oom");
        Value* set_value = make_set_checked(interp, 8, "set canonical order test: ctor oom");
        bool setup_ok = dict_value != null &&
            dict_value.tag == HASHMAP &&
            set_value != null &&
            set_value.tag == SET;

        if (setup_ok) {
            setup_ok =
                hashmap_set_checked(dict_value.hashmap_val, make_symbol(interp, interp.symbols.intern("b")), make_int(interp, 2), interp) &&
                hashmap_set_checked(dict_value.hashmap_val, make_symbol(interp, interp.symbols.intern("a")), make_int(interp, 1), interp) &&
                hashmap_set_checked(set_value.hashmap_val, make_symbol(interp, interp.symbols.intern("b")), make_symbol(interp, interp.sym_true), interp) &&
                hashmap_set_checked(set_value.hashmap_val, make_symbol(interp, interp.symbols.intern("a")), make_symbol(interp, interp.sym_true), interp);
        }

        Value*[1] dict_args = { dict_value };
        Value*[1] set_args = { set_value };
        bool saved_slots_fail = g_hashmap_force_sorted_slots_alloc_fail;
        g_hashmap_force_sorted_slots_alloc_fail = true;
        Value* keys_fail = setup_ok ? prim_keys(dict_args[0:1], null, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        Value* values_fail = setup_ok ? prim_values(dict_args[0:1], null, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        Value* set_list_fail = setup_ok ? prim_set_to_list(set_args[0:1], null, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        g_hashmap_force_sorted_slots_alloc_fail = saved_slots_fail;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(keys_fail, "keys: failed to allocate canonical order scratch") &&
            runtime_alloc_error_matches(values_fail, "values: failed to allocate canonical order scratch") &&
            runtime_alloc_error_matches(set_list_fail, "keys: failed to allocate canonical order scratch");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: canonical collection materializers fail closed on order scratch OOM");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: canonical collection materializers fail closed on order scratch OOM (setup_ok=%s keys=%s values=%s set=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(keys_fail),
                runtime_alloc_error_text(values_fail),
                runtime_alloc_error_text(set_list_fail));
            (*fail)++;
        }
    }

}
