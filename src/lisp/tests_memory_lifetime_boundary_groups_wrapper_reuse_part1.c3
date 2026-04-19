module lisp;

import std::io;
import std::core::mem;
import main;

fn bool run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test_reuse_and_copy(Interp* interp, int* pass, int* fail) {
    // Invariant: wrapper values in target chain are reused, while disjoint wrappers are defensively copied.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || releasing == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: wrapper reuse vs defensive copy");
            (*fail)++;
            return false;
        }

        interp.current_scope = target;
        void* h_reuse = mem::malloc(8);
        Value* reusable = make_ffi_handle_ex(interp, h_reuse, "__reuse", null, true);

        interp.current_scope = foreign;
        void* h_foreign = mem::malloc(8);
        Value* disjoint = make_ffi_handle_ex(interp, h_foreign, "__disjoint", null, true);

        interp.current_scope = target;
        interp.releasing_scope = releasing;

        Value* reused = copy_to_parent(reusable, interp);
        Value* copied = copy_to_parent(disjoint, interp);

        bool reuse_ok = reused == reusable;
        bool copied_ok = copied != null &&
                         copied.tag == FFI_HANDLE &&
                         copied != disjoint &&
                         copied.ffi_val != null &&
                         copied.ffi_val == disjoint.ffi_val &&
                         copied.ffi_val.refcount == 2;

        main::scope_release(foreign);

        bool post_ok = copied != null &&
                       copied.tag == FFI_HANDLE &&
                       copied.ffi_val != null &&
                       copied.ffi_val.refcount == 1;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(releasing);
        main::scope_release(target);

        bool ok = reusable != null && disjoint != null && reuse_ok && copied_ok && post_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: wrapper reuse vs defensive copy");
            (*pass)++;
            return true;
        }

        io::printfn("[FAIL] lifetime: wrapper reuse vs defensive copy (reusable=%s disjoint=%s reuse_ok=%s copied_ok=%s post_ok=%s)",
            reusable != null ? "yes" : "no",
            disjoint != null ? "yes" : "no",
            reuse_ok ? "yes" : "no",
            copied_ok ? "yes" : "no",
            post_ok ? "yes" : "no");
        (*fail)++;
        return false;
    }
}

fn bool run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test_leaf_alloc_failure_rollback(Interp* interp, int* pass, int* fail) {
    // Invariant: boundary copy fails closed on instance/ffi/time-point wrapper
    // allocation failure and does not introduce retained ownership side effects.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: leaf wrapper alloc failure rollback");
            (*fail)++;
            return false;
        }

        int ffi_destroy_count = 0;

        interp.current_scope = foreign;
        Value* inst_value = make_instance(interp, (TypeId)1, null, 0);
        main::ScopeRegion* inst_owner_scope =
            inst_value != null && inst_value.tag == INSTANCE ? inst_value.instance_val.owner_scope : null;
        Value* ffi_value = make_ffi_handle_ex(
            interp,
            (void*)0x1234,
            "__leaf-wrapper-fail",
            fn void(void* ptr) {
                int* counter = (int*)ptr;
                if (counter != null) (*counter)++;
            },
            false
        );
        if (ffi_value != null && ffi_value.tag == FFI_HANDLE && ffi_value.ffi_val != null) {
            ffi_value.ffi_val.finalizer = fn void(void* ptr) {
                int* counter = (int*)ptr;
                if (counter != null) (*counter)++;
            };
            ffi_value.ffi_val.lib_handle = (void*)&ffi_destroy_count;
            ffi_value.ffi_val.free_lib_handle = false;
        }
        Value* time_point = make_time_point(interp, TIME_POINT_DATETIME, 2026, 4, 10, 1, 2, 3, 0, 0);

        bool setup_ok = inst_value != null &&
                        inst_value.tag == INSTANCE &&
                        inst_owner_scope != null &&
                        inst_owner_scope.refcount == 1 &&
                        ffi_value != null &&
                        ffi_value.tag == FFI_HANDLE &&
                        ffi_value.ffi_val != null &&
                        ffi_value.ffi_val.refcount == 1 &&
                        time_point != null &&
                        time_point.tag == TIME_POINT;

        interp.current_scope = target;
        interp.releasing_scope = foreign;

        bool saved_force_instance = g_boundary_force_copy_instance_wrapper_alloc_fail;
        bool saved_force_ffi = g_boundary_force_copy_ffi_handle_wrapper_alloc_fail;
        bool saved_force_time = g_boundary_force_copy_time_point_wrapper_alloc_fail;
        g_boundary_force_copy_instance_wrapper_alloc_fail = true;
        g_boundary_force_copy_ffi_handle_wrapper_alloc_fail = true;
        g_boundary_force_copy_time_point_wrapper_alloc_fail = true;

        BoundaryCopyResult copied_instance =
            setup_ok ? boundary_copy_to_parent_site_ctx_checked(inst_value, interp, COPY_SITE_GENERIC, null)
                     : boundary_copy_fail(BOUNDARY_COPY_FAULT_NULL_RESULT);
        BoundaryCopyResult copied_ffi =
            setup_ok ? boundary_copy_to_parent_site_ctx_checked(ffi_value, interp, COPY_SITE_GENERIC, null)
                     : boundary_copy_fail(BOUNDARY_COPY_FAULT_NULL_RESULT);
        BoundaryCopyResult copied_time =
            setup_ok ? boundary_copy_to_parent_site_ctx_checked(time_point, interp, COPY_SITE_GENERIC, null)
                     : boundary_copy_fail(BOUNDARY_COPY_FAULT_NULL_RESULT);

        g_boundary_force_copy_instance_wrapper_alloc_fail = saved_force_instance;
        g_boundary_force_copy_ffi_handle_wrapper_alloc_fail = saved_force_ffi;
        g_boundary_force_copy_time_point_wrapper_alloc_fail = saved_force_time;

        interp.releasing_scope = null;
        interp.current_scope = saved;

        int inst_owner_rc_after_fail = inst_owner_scope != null ? (int)inst_owner_scope.refcount : -1;
        int ffi_rc_after_fail = ffi_value != null && ffi_value.tag == FFI_HANDLE && ffi_value.ffi_val != null
            ? (int)ffi_value.ffi_val.refcount
            : -1;

        bool instance_fault_ok = !boundary_copy_result_ok(copied_instance) &&
                                 copied_instance.fault_code == BOUNDARY_COPY_FAULT_NULL_RESULT &&
                                 inst_owner_rc_after_fail == 1;
        bool ffi_fault_ok = !boundary_copy_result_ok(copied_ffi) &&
                            copied_ffi.fault_code == BOUNDARY_COPY_FAULT_NULL_RESULT &&
                            ffi_rc_after_fail == 1;
        bool time_fault_ok = boundary_copy_result_ok(copied_time) &&
                             copied_time.value != null &&
                             copied_time.value.tag == ERROR &&
                             copied_time.value.str_chars != null &&
                             str_starts_with(copied_time.value.str_chars[:copied_time.value.str_len], "boundary: failed to allocate time-point copy value");

        main::scope_release(target);
        int inst_owner_rc_after_target_release = inst_owner_scope != null ? (int)inst_owner_scope.refcount : -1;
        int ffi_rc_after_target_release = ffi_value != null && ffi_value.tag == FFI_HANDLE && ffi_value.ffi_val != null
            ? (int)ffi_value.ffi_val.refcount
            : -1;
        bool teardown_ok = inst_owner_rc_after_target_release == 1 &&
                           ffi_rc_after_target_release == 1 &&
                           ffi_destroy_count == 0;

        main::scope_release(foreign);
        bool owner_release_ok = ffi_destroy_count == 1;

        bool ok = setup_ok &&
                  instance_fault_ok &&
                  ffi_fault_ok &&
                  time_fault_ok &&
                  teardown_ok &&
                  owner_release_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: leaf wrapper alloc failure rollback");
            (*pass)++;
            return true;
        }

        io::printfn(
            "[FAIL] lifetime: leaf wrapper alloc failure rollback (setup_ok=%s inst_fault_ok=%s ffi_fault_ok=%s time_fault_ok=%s inst_rc_fail=%d ffi_rc_fail=%d inst_rc_target=%d ffi_rc_target=%d ffi_destroy_count=%d)",
            setup_ok ? "yes" : "no",
            instance_fault_ok ? "yes" : "no",
            ffi_fault_ok ? "yes" : "no",
            time_fault_ok ? "yes" : "no",
            inst_owner_rc_after_fail,
            ffi_rc_after_fail,
            inst_owner_rc_after_target_release,
            ffi_rc_after_target_release,
            ffi_destroy_count
        );
        (*fail)++;
        return false;
    }
}

fn bool run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test_shared_wrapper_defensive_clone(Interp* interp, int* pass, int* fail) {
    // Invariant: disjoint shared wrappers (collections/method tables) are
    // defensively cloned rather than aliased once reuse is declined.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || releasing == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy");
            (*fail)++;
            return false;
        }

        // ARRAY
        interp.current_scope = foreign;
        Value* array_src = interp.alloc_value();
        array_src.tag = ARRAY;
        main::scope_register_dtor(foreign, (void*)array_src, &scope_dtor_value);
        Array* array_payload = (Array*)mem::malloc(Array.sizeof);
        if (array_payload == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup array)");
            (*fail)++;
            return false;
        }
        array_payload.capacity = 4;
        array_payload.length = 1;
        array_payload.items = (Value**)mem::malloc(Value*.sizeof * array_payload.capacity);
        if (array_payload.items == null) {
            mem::free(array_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup array items)");
            (*fail)++;
            return false;
        }
        for (usz i = 0; i < array_payload.capacity; i++) {
            array_payload.items[i] = null;
        }
        array_payload.items[0] = make_int(interp, 71);
        array_src.array_val = array_payload;

        // HASHMAP
        Value* hashmap_src = interp.alloc_value();
        hashmap_src.tag = HASHMAP;
        main::scope_register_dtor(foreign, (void*)hashmap_src, &scope_dtor_value);
        HashMap* hashmap_payload = (HashMap*)mem::malloc(HashMap.sizeof);
        if (hashmap_payload == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup hashmap)");
            (*fail)++;
            return false;
        }
        hashmap_payload.capacity = 8;
        hashmap_payload.count = 1;
        hashmap_payload.mask = hashmap_payload.capacity - 1;
        hashmap_payload.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * hashmap_payload.capacity);
        if (hashmap_payload.entries == null) {
            mem::free(hashmap_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup hashmap entries)");
            (*fail)++;
            return false;
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

        // SET
        Value* set_src = interp.alloc_value();
        set_src.tag = SET;
        main::scope_register_dtor(foreign, (void*)set_src, &scope_dtor_value);
        HashMap* set_payload = (HashMap*)mem::malloc(HashMap.sizeof);
        if (set_payload == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup set)");
            (*fail)++;
            return false;
        }
        set_payload.capacity = 8;
        set_payload.count = 1;
        set_payload.mask = set_payload.capacity - 1;
        set_payload.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * set_payload.capacity);
        if (set_payload.entries == null) {
            mem::free(set_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup set entries)");
            (*fail)++;
            return false;
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

        // METHOD_TABLE
        Value* mt_src = interp.alloc_value();
        mt_src.tag = METHOD_TABLE;
        main::scope_register_dtor(foreign, (void*)mt_src, &scope_dtor_value);
        MethodTable* mt_payload = (MethodTable*)mem::malloc(MethodTable.sizeof);
        if (mt_payload == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup method table)");
            (*fail)++;
            return false;
        }
        mt_payload.name = interp.symbols.intern("__shared_wrapper_mt");
        mt_payload.capacity = 1;
        mt_payload.entry_count = 1;
        mt_payload.fallback = make_int(interp, 123);
        mt_payload.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * mt_payload.capacity);
        if (mt_payload.entries == null) {
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup method table entries)");
            (*fail)++;
            return false;
        }
        method_signature_init_empty(&mt_payload.entries[0].sig);
        MethodSignature mt_sig_src;
        method_signature_init_empty(&mt_sig_src);
        if (!method_signature_alloc_arrays_in_scope(&mt_sig_src, foreign, 1, 0)) {
            mem::free(mt_payload.entries);
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (setup method table signature)");
            (*fail)++;
            return false;
        }
        mt_sig_src.param_types[0] = interp.tid_Integer;
        mt_sig_src.param_type_vars[0] = (SymbolId)0;
        mt_sig_src.has_val_literal[0] = false;
        if (!method_signature_copy_to_heap(&mt_payload.entries[0].sig, &mt_sig_src)) {
            mem::free(mt_payload.entries);
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared wrapper defensive clone policy (copy method signature)");
            (*fail)++;
            return false;
        }
        mt_payload.entries[0].implementation = make_int(interp, 456);
        mt_src.method_table_val = mt_payload;

        interp.current_scope = target;
        interp.releasing_scope = releasing;

        Value* array_copied = copy_to_parent(array_src, interp);
        Value* hashmap_copied = copy_to_parent(hashmap_src, interp);
        Value* set_copied = copy_to_parent(set_src, interp);
        Value* mt_copied = copy_to_parent(mt_src, interp);
        Value* array_escaped = promote_to_escape(array_src, interp);
        Value* hashmap_escaped = promote_to_escape(hashmap_src, interp);
        Value* set_escaped = promote_to_escape(set_src, interp);
        Value* mt_escaped = promote_to_escape(mt_src, interp);

        bool array_copy_ok = array_copied != null &&
                             array_copied != array_src &&
                             array_copied.tag == ARRAY &&
                             array_copied.array_val != null &&
                             array_copied.array_val != array_src.array_val &&
                             array_copied.array_val.length == 1 &&
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
        bool hashmap_copy_ok = hashmap_copied != null &&
                               hashmap_copied != hashmap_src &&
                               hashmap_copied.tag == HASHMAP &&
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
        bool set_copy_ok = set_copied != null &&
                           set_copied != set_src &&
                           set_copied.tag == SET &&
                           set_copied.hashmap_val != null &&
                           set_copied.hashmap_val != set_src.hashmap_val &&
                           copied_set_key != null &&
                           copied_set_value != null &&
                           copied_set_key != set_src.hashmap_val.entries[set_idx].key &&
                           copied_set_value != set_src.hashmap_val.entries[set_idx].value &&
                           copied_set_value.tag == INT &&
                           copied_set_value.int_val == 1;

        bool mt_copy_ok = mt_copied != null &&
                          mt_copied != mt_src &&
                          mt_copied.tag == METHOD_TABLE &&
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

        bool array_escape_ok = array_escaped != null &&
                               array_escaped != array_src &&
                               array_escaped.tag == ARRAY &&
                               array_escaped.scope_gen == target.escape_generation &&
                               array_escaped.array_val != null &&
                               array_escaped.array_val != array_src.array_val &&
                               array_escaped.array_val.items != null &&
                               array_escaped.array_val.items[0] != null &&
                               array_escaped.array_val.items[0] != array_src.array_val.items[0] &&
                               array_escaped.array_val.items[0].tag == INT &&
                               array_escaped.array_val.items[0].int_val == 71;

        bool hashmap_escape_ok = hashmap_escaped != null &&
                                 hashmap_escaped != hashmap_src &&
                                 hashmap_escaped.tag == HASHMAP &&
                                 hashmap_escaped.scope_gen == target.escape_generation &&
                                 hashmap_escaped.hashmap_val != null &&
                                 hashmap_escaped.hashmap_val != hashmap_src.hashmap_val;

        bool set_escape_ok = set_escaped != null &&
                             set_escaped != set_src &&
                             set_escaped.tag == SET &&
                             set_escaped.scope_gen == target.escape_generation &&
                             set_escaped.hashmap_val != null &&
                             set_escaped.hashmap_val != set_src.hashmap_val;

        bool mt_escape_ok = mt_escaped != null &&
                            mt_escaped != mt_src &&
                            mt_escaped.tag == METHOD_TABLE &&
                            mt_escaped.scope_gen == target.escape_generation &&
                            mt_escaped.method_table_val != null &&
                            mt_escaped.method_table_val != mt_src.method_table_val &&
                            mt_escaped.method_table_val.entry_count == 1 &&
                            mt_escaped.method_table_val.entries != null &&
                            mt_escaped.method_table_val.entries[0].implementation != null &&
                            mt_escaped.method_table_val.entries[0].implementation.tag == INT &&
                            mt_escaped.method_table_val.entries[0].implementation.int_val == 456 &&
                            mt_escaped.method_table_val.fallback != null &&
                            mt_escaped.method_table_val.fallback.tag == INT &&
                            mt_escaped.method_table_val.fallback.int_val == 123;

        main::scope_release(foreign);

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(releasing);
        main::scope_release(target);

        bool ok = array_copy_ok && hashmap_copy_ok && set_copy_ok && mt_copy_ok &&
                  array_escape_ok && hashmap_escape_ok && set_escape_ok && mt_escape_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: shared wrapper defensive clone policy");
            (*pass)++;
            return true;
        }

        io::printfn(
            "[FAIL] lifetime: shared wrapper defensive clone policy (array_copy=%s hashmap_copy=%s set_copy=%s mt_copy=%s array_escape=%s hashmap_escape=%s set_escape=%s mt_escape=%s)",
            array_copy_ok ? "yes" : "no",
            hashmap_copy_ok ? "yes" : "no",
            set_copy_ok ? "yes" : "no",
            mt_copy_ok ? "yes" : "no",
            array_escape_ok ? "yes" : "no",
            hashmap_escape_ok ? "yes" : "no",
            set_escape_ok ? "yes" : "no",
            mt_escape_ok ? "yes" : "no"
        );
        (*fail)++;
        return false;
    }
}

fn void run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test(Interp* interp, int* pass, int* fail) {
    if (!run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test_reuse_and_copy(interp, pass, fail)) return;
    if (!run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test_leaf_alloc_failure_rollback(interp, pass, fail)) return;
    if (!run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test_shared_wrapper_defensive_clone(interp, pass, fail)) return;
    if (!run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test_nested_cons_wrapper_defensive_copy(interp, pass, fail)) return;
    if (!run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test_primitive_wrapper_copy_policy(interp, pass, fail)) return;
    run_memory_lifetime_wrapper_reuse_vs_defensive_copy_test_shared_wrapper_slot_abort(interp, pass, fail);
}
