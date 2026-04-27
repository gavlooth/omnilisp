module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_runtime_alloc_groups_core_tests(Interp* interp, int* pass, int* fail) {
    run_memory_lifetime_runtime_alloc_groups_core_constructor_tests(interp, pass, fail);

    {
        usz values_before = runtime_alloc_active_value_dtor_count(interp.current_scope);
        int* payload = (int*)mem::malloc(int.sizeof);
        bool payload_ok = payload != null;
        if (payload_ok) {
            *payload = 0x7BEEF;
        }

        g_runtime_alloc_ffi_finalizer_calls = 0;
        FfiHandle* box = payload_ok
            ? make_ffi_box((void*)payload, "null-scope-ffi-cleanup", &runtime_alloc_ffi_finalizer, false, ownership: FFI_HANDLE_OWNED)
            : null;
        Value* value = box != null ? interp.alloc_value() : null;
        if (value != null) {
            value.tag = FFI_HANDLE;
            value.ffi_val = box;
        }
        bool registered = value != null && scope_register_value_dtor_or_cleanup(null, value);
        usz values_after = runtime_alloc_active_value_dtor_count(interp.current_scope);

        bool ok = payload_ok &&
            box != null &&
            value != null &&
            !registered &&
            g_runtime_alloc_ffi_finalizer_calls == 1 &&
            *payload == 0 &&
            values_after == values_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: value dtor helper cleans up when scope is null");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: value dtor helper cleans up when scope is null (payload_ok=%s box=%s value=%s registered=%s calls=%d payload=%d before_dtors=%d after_dtors=%d)",
                payload_ok ? "yes" : "no",
                box != null ? "value" : "null",
                value != null ? "value" : "null",
                registered ? "yes" : "no",
                g_runtime_alloc_ffi_finalizer_calls,
                payload_ok ? *payload : -1,
                (long)values_before,
                (long)values_after);
            (*fail)++;
            if (box != null && (value == null || registered)) {
                ffi_handle_release(box);
            }
        }

        if (payload_ok) {
            mem::free(payload);
        }
    }

    {
        int* payload = (int*)mem::malloc(int.sizeof);
        bool payload_ok = payload != null;
        if (payload_ok) {
            *payload = 0x7BEEF;
        }

        bool constructor_owns = false;
        g_runtime_alloc_ffi_finalizer_calls = 0;
        Value* failed = payload_ok
            ? make_ffi_handle_ex(
                null,
                (void*)payload,
                "null-interp-ffi-cleanup",
                &runtime_alloc_ffi_finalizer,
                false,
                constructor_owns_handle: &constructor_owns)
            : null;

        bool ok = payload_ok &&
            failed == null &&
            constructor_owns &&
            g_runtime_alloc_ffi_finalizer_calls == 1 &&
            *payload == 0;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: ffi handle constructor releases owned payload without interp");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: ffi handle constructor releases owned payload without interp (payload_ok=%s result=%s owns=%s calls=%d payload=%d)",
                payload_ok ? "yes" : "no",
                failed == null ? "null" : "value",
                constructor_owns ? "yes" : "no",
                g_runtime_alloc_ffi_finalizer_calls,
                payload_ok ? *payload : -1);
            (*fail)++;
        }

        if (payload_ok) {
            mem::free(payload);
        }
    }

    {
        int* payload = (int*)mem::malloc(int.sizeof);
        bool payload_ok = payload != null;
        if (payload_ok) {
            *payload = 0x7BEEF;
        }

        main::ScopeRegion* saved_scope = interp.current_scope;
        bool constructor_owns = false;
        g_runtime_alloc_ffi_finalizer_calls = 0;
        interp.current_scope = null;
        Value* failed = payload_ok
            ? make_ffi_handle_ex(
                interp,
                (void*)payload,
                "null-target-scope-ffi-cleanup",
                &runtime_alloc_ffi_finalizer,
                false,
                constructor_owns_handle: &constructor_owns)
            : null;
        interp.current_scope = saved_scope;

        bool ok = payload_ok &&
            failed == null &&
            constructor_owns &&
            g_runtime_alloc_ffi_finalizer_calls == 1 &&
            *payload == 0;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: ffi handle constructor releases owned payload without target scope");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: ffi handle constructor releases owned payload without target scope (payload_ok=%s result=%s owns=%s calls=%d payload=%d)",
                payload_ok ? "yes" : "no",
                failed == null ? "null" : "value",
                constructor_owns ? "yes" : "no",
                g_runtime_alloc_ffi_finalizer_calls,
                payload_ok ? *payload : -1);
            (*fail)++;
        }

        interp.current_scope = saved_scope;
        if (payload_ok) {
            mem::free(payload);
        }
    }

    {
        EvalResult cb_eval = run("(lambda (sig) nil)", interp);
        bool setup_ok = !cb_eval.error.has_error && cb_eval.value != null;

        usz root_before = interp.root_scope != null ? interp.root_scope.refcount : 0;
        usz registry_before = 0;
        SignalHandle* sh = (SignalHandle*)interp.signal_handles_head;
        while (sh != null) {
            registry_before++;
            sh = sh.registry_next;
        }

        Value*[2] signal_args;
        signal_args[0] = make_int(interp, 10);
        signal_args[1] = setup_ok ? cb_eval.value : null;
        int saved_force = g_boundary_force_alloc_value_fail_nth;
        g_boundary_force_alloc_value_fail_nth = 1;
        Value* signal_fail = setup_ok ? prim_signal_handle(signal_args[..], null, interp) : null;
        g_boundary_force_alloc_value_fail_nth = saved_force;

        usz root_after = interp.root_scope != null ? interp.root_scope.refcount : 0;
        usz registry_after = 0;
        sh = (SignalHandle*)interp.signal_handles_head;
        while (sh != null) {
            registry_after++;
            sh = sh.registry_next;
        }

        bool ok = setup_ok &&
            runtime_alloc_error_matches(signal_fail, "signal-handle: out of memory") &&
            root_after == root_before &&
            registry_after == registry_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: signal-handle alloc wrapper failure returns signal-handle OOM and cleans runtime state");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: signal-handle alloc wrapper failure returns signal-handle OOM and cleans runtime state (setup_ok=%s root_before=%d root_after=%d registry_before=%d registry_after=%d)",
                setup_ok ? "yes" : "no",
                (long)root_before,
                (long)root_after,
                (long)registry_before,
                (long)registry_after);
            (*fail)++;
        }
    }

    {
        Value*[1] string_args = { make_string(interp, "ab") };
        Value*[2] variadic_args = { make_int(interp, 1), make_int(interp, 2) };
        Value* arr = make_array_checked(interp, 2, "test: failed to allocate array");
        bool setup_ok = arr != null && arr.tag == ARRAY && arr.array_val.items != null;
        if (setup_ok) {
            arr.array_val.items[0] = make_int(interp, 1);
            arr.array_val.items[1] = make_int(interp, 2);
            arr.array_val.length = 2;
        }
        Value*[1] array_args = { arr };

        int saved_force = g_value_force_make_cons_or_error_alloc_fail_nth;
        g_value_force_make_cons_or_error_alloc_fail_nth = 1;
        Value* string_fail = prim_string_to_list(string_args[0:1], null, interp);
        g_value_force_make_cons_or_error_alloc_fail_nth = 1;
        Value* variadic_fail = prim_list(variadic_args[0:2], null, interp);
        g_value_force_make_cons_or_error_alloc_fail_nth = 1;
        Value* array_fail = setup_ok ? prim_list(array_args[0:1], null, interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        g_value_force_make_cons_or_error_alloc_fail_nth = saved_force;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(string_fail, "string->list: failed to allocate result list") &&
            runtime_alloc_error_matches(variadic_fail, "list: failed to allocate result list") &&
            runtime_alloc_error_matches(array_fail, "list: failed to allocate result list");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: list builders fail closed on cons allocation failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: list builders fail closed on cons allocation failure (setup_ok=%s string=%s variadic=%s array=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(string_fail),
                runtime_alloc_error_text(variadic_fail),
                runtime_alloc_error_text(array_fail));
            (*fail)++;
        }
    }

    {
        Value* map = make_hashmap_checked(interp, 4, "test: failed to allocate hashmap");
        bool setup_ok = map != null && map.tag == HASHMAP && map.hashmap_val != null;
        bool saved_force = g_value_force_make_string_alloc_fail;
        g_value_force_make_string_alloc_fail = true;
        Value* lookup_fail = setup_ok ? json_pointer_lookup(map, "missing", interp) : runtime_raise(interp, "runtime/evaluation-error", "setup failed");
        g_value_force_make_string_alloc_fail = saved_force;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(lookup_fail, "out of memory while allocating string");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: json pointer string key allocation fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: json pointer string key allocation fails closed (setup_ok=%s msg=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(lookup_fail));
            (*fail)++;
        }
    }

    {
        Value*[2] split_args = {
            make_string(interp, "a,b"),
            make_string(interp, ",")
        };

        int saved_cons_fail = g_value_force_make_cons_or_error_alloc_fail_nth;
        g_value_force_make_cons_or_error_alloc_fail_nth = 1;
        Value* split_fail = prim_string_split(split_args[0:2], null, interp);
        g_value_force_make_cons_or_error_alloc_fail_nth = saved_cons_fail;

        bool ok = runtime_alloc_error_matches(split_fail, "string-split: failed to allocate result list");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: string split list construction fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: string split list construction fails closed (msg=%s)",
                runtime_alloc_error_text(split_fail));
            (*fail)++;
        }
    }

    {
        Value* key_a = make_symbol(interp, interp.symbols.intern("a"));
        Value* key_b = make_symbol(interp, interp.symbols.intern("b"));
        Value* map_value = make_hashmap_checked(interp, 4, "test: failed to allocate hashmap");
        bool setup_ok = map_value != null && map_value.tag == HASHMAP;
        if (setup_ok) {
            setup_ok =
                hashmap_set_checked(map_value.hashmap_val, key_a, make_int(interp, 1), interp) &&
                hashmap_set_checked(map_value.hashmap_val, key_b, make_int(interp, 2), interp);
        }

        Value* keys_fail = null;
        Value* values_fail = null;
        int saved_cons_fail = g_value_force_make_cons_or_error_alloc_fail_nth;
        if (setup_ok) {
            g_value_force_make_cons_or_error_alloc_fail_nth = 1;
            keys_fail = prim_keys((Value*[]){ map_value }, null, interp);
            g_value_force_make_cons_or_error_alloc_fail_nth = 1;
            values_fail = prim_values((Value*[]){ map_value }, null, interp);
        }
        g_value_force_make_cons_or_error_alloc_fail_nth = saved_cons_fail;
        bool ok = setup_ok &&
            runtime_alloc_error_matches(keys_fail, "keys: failed to allocate result list") &&
            runtime_alloc_error_matches(values_fail, "values: failed to allocate result list");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: hashmap key/value list construction fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: hashmap key/value list construction fails closed (setup_ok=%s keys=%s values=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(keys_fail),
                runtime_alloc_error_text(values_fail));
            (*fail)++;
        }
    }

    {
        Value* proc_argv =
            make_cons(interp,
                make_string(interp, "-c"),
                make_cons(interp, make_string(interp, "exit 0"), make_nil(interp)));
        Value*[3] spawn_args = {
            make_string(interp, "/bin/sh"),
            proc_argv,
            make_nil(interp)
        };

        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* spawn_fail = prim_process_spawn(spawn_args[0:3], null, interp);
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok = runtime_alloc_error_matches(spawn_fail, "process-spawn: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: process spawn status payload constructor fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: process spawn status payload constructor fails closed (msg=%s)",
                runtime_alloc_error_text(spawn_fail));
            (*fail)++;
        }
    }

    runtime_alloc_process_spawn_cleanup_case(
        interp,
        pass,
        fail,
        "lifetime: process spawn stdin wrapper alloc failure closes process and pipes",
        2,
        PROCESS_SPAWN_TRACE_STAGE_STDIN,
        false,
        false,
        false,
        true,
        true,
        true);

    runtime_alloc_process_spawn_cleanup_case(
        interp,
        pass,
        fail,
        "lifetime: process spawn stdout wrapper alloc failure closes process and pipes",
        3,
        PROCESS_SPAWN_TRACE_STAGE_STDOUT,
        true,
        false,
        false,
        true,
        true,
        true);

    runtime_alloc_process_spawn_cleanup_case(
        interp,
        pass,
        fail,
        "lifetime: process spawn stderr wrapper alloc failure closes process and pipes",
        4,
        PROCESS_SPAWN_TRACE_STAGE_STDERR,
        true,
        true,
        false,
        true,
        true,
        true);

    {
        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* wait_fail = make_process_wait_result(interp, 42, 0, 0);
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok = runtime_alloc_error_matches(wait_fail, "process-wait: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: process wait status payload constructor fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: process wait status payload constructor fails closed (msg=%s)",
                runtime_alloc_error_text(wait_fail));
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null && interp.root_scope != null;
        usz root_hashmaps_before = runtime_alloc_active_hashmap_dtor_count(interp.root_scope);
        Value* wait_fail = null;

        bool saved_force = g_process_wait_force_result_insert_fail;
        if (setup_ok) {
            interp.current_scope = child;
            g_process_wait_force_result_insert_fail = true;
            wait_fail = make_process_wait_result(interp, 42, 0, 0);
            interp.current_scope = saved;
        }
        g_process_wait_force_result_insert_fail = saved_force;
        usz root_hashmaps_after = runtime_alloc_active_hashmap_dtor_count(interp.root_scope);

        bool ok = setup_ok &&
            runtime_alloc_error_matches(wait_fail, "process-wait: out of memory") &&
            root_hashmaps_after == root_hashmaps_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: process wait result promotion failure cleans partial root dictionary");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: process wait result promotion failure cleans partial root dictionary (setup_ok=%s msg=%s before=%d after=%d)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(wait_fail),
                (long)root_hashmaps_before,
                (long)root_hashmaps_after);
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        SharedHandle* missing = null;
        Value* projected = scheduler_project_shared_to_local_value(&missing, interp);

        OffloadCompletion read_completion = {};
        bool read_ok = scheduler_offload_read_file(null, &read_completion);
        OffloadCompletion exists_completion = {};
        bool exists_ok = scheduler_offload_file_exists(null, &exists_completion);

        bool ok =
            projected != null &&
            projected.tag == ERROR &&
            !read_ok &&
            read_completion.kind == OFFLOAD_RES_ERROR &&
            !exists_ok &&
            exists_completion.kind == OFFLOAD_RES_ERROR;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: scheduler shared/offload projection faults fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: scheduler shared/offload projection faults fail closed (projected_tag=%d read_ok=%s read_kind=%d exists_ok=%s exists_kind=%d)",
                projected != null ? (int)projected.tag : -1,
                read_ok ? "yes" : "no",
                (int)read_completion.kind,
                exists_ok ? "yes" : "no",
                (int)exists_completion.kind);
            (*fail)++;
        }

        if (read_completion.error_msg != null) mem::free(read_completion.error_msg);
        if (exists_completion.error_msg != null) mem::free(exists_completion.error_msg);
    }

}
