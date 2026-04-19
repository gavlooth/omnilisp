module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_method_table_abort_cleanup_test(Interp* interp, int* pass, int* fail) {
    // Invariant: method-table copy/escape abort paths reclaim already-copied
    // heap signatures when a later signature copy fails.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* target = main::scope_create(saved);
        main::ScopeRegion* releasing = main::scope_create(saved);
        main::ScopeRegion* foreign = main::scope_create(saved);
        if (target == null || releasing == null || foreign == null) {
            if (foreign != null) main::scope_release(foreign);
            if (releasing != null) main::scope_release(releasing);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: method table abort cleanup telemetry");
            (*fail)++;
            return;
        }

        interp.current_scope = foreign;
        Value* mt_src = interp.alloc_value();
        mt_src.tag = METHOD_TABLE;
        main::scope_register_dtor(foreign, (void*)mt_src, &scope_dtor_value);

        MethodTable* mt_payload = (MethodTable*)mem::malloc(MethodTable.sizeof);
        if (mt_payload == null) {
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: method table abort cleanup telemetry (setup table)");
            (*fail)++;
            return;
        }
        mt_payload.name = interp.symbols.intern("__mt_abort_cleanup");
        mt_payload.capacity = 2;
        mt_payload.entry_count = 2;
        mt_payload.fallback = null;
        mt_payload.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * mt_payload.capacity);
        if (mt_payload.entries == null) {
            mem::free(mt_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: method table abort cleanup telemetry (setup entries)");
            (*fail)++;
            return;
        }

        bool setup_ok = true;
        for (usz i = 0; i < mt_payload.entry_count; i++) {
            method_signature_init_empty(&mt_payload.entries[i].sig);
            MethodSignature sig_src;
            method_signature_init_empty(&sig_src);
            if (!method_signature_alloc_arrays_in_scope(&sig_src, foreign, 1, 0)) {
                setup_ok = false;
                break;
            }
            sig_src.param_types[0] = interp.tid_Integer;
            sig_src.param_type_vars[0] = (SymbolId)0;
            sig_src.has_val_literal[0] = false;
            if (!method_signature_copy_to_heap(&mt_payload.entries[i].sig, &sig_src)) {
                setup_ok = false;
                break;
            }
            mt_payload.entries[i].implementation = make_int(interp, (long)(900 + i));
        }

        if (!setup_ok) {
            method_table_free_partial_heap(mt_payload, mt_payload.entry_count);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(releasing);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: method table abort cleanup telemetry (setup signature)");
            (*fail)++;
            return;
        }
        mt_src.method_table_val = mt_payload;

        interp.current_scope = target;
        interp.releasing_scope = releasing;

        method_table_partial_cleanup_reset();
        g_method_signature_heap_copy_call_count = 0;
        g_method_signature_force_heap_copy_fail_on_call = 2;
        BoundaryCopyResult copy_result = boundary_copy_to_parent_site_ctx_checked(
            mt_src,
            interp,
            COPY_SITE_GENERIC,
            null
        );
        usz copy_cleanup_calls = method_table_partial_cleanup_calls();
        usz copy_cleanup_entries = method_table_partial_cleanup_entries();

        method_table_partial_cleanup_reset();
        g_method_signature_heap_copy_call_count = 0;
        g_method_signature_force_heap_copy_fail_on_call = 2;
        Value* escape_result = promote_to_escape(mt_src, interp);
        usz escape_cleanup_calls = method_table_partial_cleanup_calls();
        usz escape_cleanup_entries = method_table_partial_cleanup_entries();
        g_method_signature_force_heap_copy_fail_on_call = 0;
        bool escape_ok = escape_result != null &&
            escape_result.tag == ERROR &&
            escape_cleanup_calls == 1 &&
            escape_cleanup_entries == 1;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(foreign);
        main::scope_release(releasing);
        main::scope_release(target);

        bool copy_ok = !boundary_copy_result_ok(copy_result) &&
            copy_result.value == null &&
            copy_result.fault_code == BOUNDARY_COPY_FAULT_NULL_RESULT &&
            copy_cleanup_calls == 1 &&
            copy_cleanup_entries == 1;

        bool ok = copy_ok && escape_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: method table abort cleanup telemetry");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: method table abort cleanup telemetry (copy_ok=%s escape_ok=%s copy_fault=%s copy_calls=%d copy_entries=%d escape_tag=%d escape_calls=%d escape_entries=%d)",
                copy_ok ? "yes" : "no",
                escape_ok ? "yes" : "no",
                (ZString)boundary_copy_fault_name(copy_result.fault_code),
                (long)copy_cleanup_calls,
                (long)copy_cleanup_entries,
                escape_result != null ? (int)escape_result.tag : -1,
                (long)escape_cleanup_calls,
                (long)escape_cleanup_entries
            );
            (*fail)++;
        }
    }
}

fn void run_memory_lifetime_shared_wrapper_partial_cleanup_test(Interp* interp, int* pass, int* fail) {
    // Invariant: shared-wrapper partial-copy failure unwinds already-copied
    // child ownership side effects instead of leaking retained env scopes.
    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* foreign = main::scope_create(saved);
        main::ScopeRegion* target = main::scope_create(saved);
        if (foreign == null || target == null) {
            if (foreign != null) main::scope_release(foreign);
            if (target != null) main::scope_release(target);
            io::printn("[FAIL] lifetime: shared-wrapper partial cleanup releases retains");
            (*fail)++;
            return;
        }

        SymbolId payload_sym = interp.symbols.intern("__shared_wrapper_partial_cleanup_payload");

        interp.current_scope = foreign;
        Env* captured_env = make_env(interp, interp.global_env);
        if (captured_env != null) {
            env_define_with_barrier(captured_env, payload_sym, make_int(interp, 808), interp);
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
        main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
        Primitive* opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
        int* opaque_payload = (int*)mem::malloc(int.sizeof);
        if (opaque_prim == null || opaque_payload == null) {
            if (opaque_prim != null) mem::free(opaque_prim);
            if (opaque_payload != null) mem::free(opaque_payload);
            interp.current_scope = saved;
            main::scope_release(foreign);
            main::scope_release(target);
            io::printn("[FAIL] lifetime: shared-wrapper partial cleanup releases retains (setup opaque)");
            (*fail)++;
            return;
        }
        *opaque_prim = {};
        *opaque_payload = 809;
        opaque_prim.name[0] = 'p';
        opaque_prim.name[1] = 0;
        opaque_prim.arity = 1;
        opaque_prim.tag = 809;
        opaque_prim.user_data = (void*)opaque_payload;
        opaque.prim_val = opaque_prim;

        Value* src_array = null;
        if (shared_cons != null) {
            src_array = interp.alloc_value();
            if (src_array != null) {
                src_array.tag = ARRAY;
                main::scope_register_dtor(foreign, (void*)src_array, &scope_dtor_value);
                Array* src_array_payload = (Array*)mem::malloc(Array.sizeof);
                if (src_array_payload != null) {
                    src_array_payload.capacity = 2;
                    src_array_payload.length = 2;
                    src_array_payload.items = (Value**)mem::malloc(Value*.sizeof * src_array_payload.capacity);
                    if (src_array_payload.items != null) {
                        src_array_payload.items[0] = shared_cons;
                        src_array_payload.items[1] = opaque;
                        src_array.array_val = src_array_payload;
                    } else {
                        mem::free(src_array_payload);
                        src_array.array_val = null;
                    }
                } else {
                    src_array.array_val = null;
                }
            }
        }

        Value* src_hashmap = null;
        if (shared_cons != null) {
            src_hashmap = interp.alloc_value();
            if (src_hashmap != null) {
                src_hashmap.tag = HASHMAP;
                main::scope_register_dtor(foreign, (void*)src_hashmap, &scope_dtor_value);
                HashMap* src_hashmap_payload = (HashMap*)mem::malloc(HashMap.sizeof);
                if (src_hashmap_payload != null) {
                    src_hashmap_payload.capacity = 8;
                    src_hashmap_payload.count = 2;
                    src_hashmap_payload.mask = src_hashmap_payload.capacity - 1;
                    src_hashmap_payload.entries = (HashEntry*)mem::malloc(HashEntry.sizeof * src_hashmap_payload.capacity);
                    if (src_hashmap_payload.entries != null) {
                        for (uint i = 0; i < src_hashmap_payload.capacity; i++) {
                            src_hashmap_payload.entries[i].key = null;
                            src_hashmap_payload.entries[i].value = null;
                        }
                        Value* good_key = make_int(interp, 31);
                        Value* bad_key = make_int(interp, 32);
                        uint good_idx = hash_value(good_key) & src_hashmap_payload.mask;
                        uint bad_idx = hash_value(bad_key) & src_hashmap_payload.mask;
                        if (bad_idx == good_idx) bad_idx = (bad_idx + 1) & src_hashmap_payload.mask;
                        src_hashmap_payload.entries[good_idx].key = good_key;
                        src_hashmap_payload.entries[good_idx].value = shared_cons;
                        src_hashmap_payload.entries[bad_idx].key = bad_key;
                        src_hashmap_payload.entries[bad_idx].value = opaque;
                        src_hashmap.hashmap_val = src_hashmap_payload;
                    } else {
                        mem::free(src_hashmap_payload);
                        src_hashmap.hashmap_val = null;
                    }
                } else {
                    src_hashmap.hashmap_val = null;
                }
            }
        }

        bool setup_ok = shared_closure != null &&
                        shared_cons != null &&
                        shared_cons.tag == CONS &&
                        shared_cons.cons_val.car == shared_closure &&
                        closure_env_scope != null &&
                        closure_env_scope.refcount == 1 &&
                        opaque != null &&
                        opaque.tag == PRIMITIVE &&
                        opaque.prim_val != null &&
                        opaque.prim_val.user_data != null &&
                        src_array != null &&
                        src_array.tag == ARRAY &&
                        src_array.array_val != null &&
                        src_array.array_val.length == 2 &&
                        src_hashmap != null &&
                        src_hashmap.tag == HASHMAP &&
                        src_hashmap.hashmap_val != null &&
                        src_hashmap.hashmap_val.entries != null &&
                        src_hashmap.hashmap_val.count == 2;

        interp.current_scope = target;
        interp.releasing_scope = foreign;
        BoundaryCopyResult array_copy_result = setup_ok
            ? boundary_copy_to_parent_site_ctx_checked(src_array, interp, COPY_SITE_GENERIC, null)
            : boundary_copy_fail(BOUNDARY_COPY_FAULT_NULL_RESULT);
        int rc_after_array_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        BoundaryCopyResult hashmap_copy_result = setup_ok
            ? boundary_copy_to_parent_site_ctx_checked(src_hashmap, interp, COPY_SITE_GENERIC, null)
            : boundary_copy_fail(BOUNDARY_COPY_FAULT_NULL_RESULT);
        int rc_after_hashmap_fail = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;

        interp.releasing_scope = null;
        interp.current_scope = saved;
        main::scope_release(target);
        int rc_after_target_release = closure_env_scope != null ? (int)closure_env_scope.refcount : -1;
        int destroy_count_after_target_release = closure_env_destroy_count;
        main::scope_release(foreign);
        int destroy_count_after_foreign_release = closure_env_destroy_count;

        bool array_ok = !boundary_copy_result_ok(array_copy_result) &&
                        array_copy_result.value == null &&
                        array_copy_result.fault_code == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD &&
                        rc_after_array_fail == 1;
        bool hashmap_ok = !boundary_copy_result_ok(hashmap_copy_result) &&
                          hashmap_copy_result.value == null &&
                          hashmap_copy_result.fault_code == BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD &&
                          rc_after_hashmap_fail == 1;
        bool teardown_ok = rc_after_target_release == 1 &&
                           destroy_count_after_target_release == 0 &&
                           destroy_count_after_foreign_release == 1;
        bool ok = setup_ok && array_ok && hashmap_ok && teardown_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: shared-wrapper partial cleanup releases retains");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: shared-wrapper partial cleanup releases retains (setup_ok=%s array_fault=%s hashmap_fault=%s rc_after_array_fail=%d rc_after_hashmap_fail=%d rc_after_target_release=%d destroy_count_after_target_release=%d destroy_count_after_foreign_release=%d)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_copy_fault_name(array_copy_result.fault_code),
                (ZString)boundary_copy_fault_name(hashmap_copy_result.fault_code),
                rc_after_array_fail,
                rc_after_hashmap_fail,
                rc_after_target_release,
                destroy_count_after_target_release,
                destroy_count_after_foreign_release
            );
            (*fail)++;
        }
    }
}
