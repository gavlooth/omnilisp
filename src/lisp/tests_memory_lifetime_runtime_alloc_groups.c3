module lisp;

import std::io;
import std::core::mem;
import main;

fn bool runtime_alloc_error_matches(Value* value, char[] expected) @inline {
    return value != null &&
        value.tag == ERROR &&
        value.str_chars != null &&
        str_starts_with(value.str_chars[:value.str_len], expected);
}

fn ZString runtime_alloc_error_text(Value* value) @inline {
    return value != null && value.tag == ERROR && value.str_chars != null
        ? (ZString)value.str_chars[:value.str_len]
        : (ZString)"<none>";
}

fn Value* runtime_alloc_null_iterator_next(Value*[] args, Env* env, Interp* interp) {
    return null;
}

int g_runtime_alloc_ffi_finalizer_calls = 0;
int g_runtime_alloc_tensor_device_finalizer_calls = 0;
int g_runtime_alloc_tensor_cuda_free_finalizer_calls = 0;

fn void runtime_alloc_ffi_finalizer(void* payload) {
    g_runtime_alloc_ffi_finalizer_calls += 1;
    if (payload != null) {
        *(int*)payload = 0;
    }
}

fn void runtime_alloc_tensor_device_finalizer(void* payload) {
    g_runtime_alloc_tensor_device_finalizer_calls += 1;
    if (payload != null) {
        *(int*)payload = 0;
    }
}

fn void runtime_alloc_tensor_cuda_free_finalizer(void* payload) {
    g_runtime_alloc_tensor_cuda_free_finalizer_calls += 1;
    if (payload != null) {
        omni_tensor_backend_cuda_free(payload);
    }
}

fn usz runtime_alloc_active_tensor_dtor_count(main::ScopeRegion* scope) {
    usz count = 0;
    if (scope == null) return count;
    main::ScopeDtor* dtor = scope.dtors;
    while (dtor != null) {
        if (dtor.ptr != null && dtor.func == &scope_dtor_value) {
            Value* v = (Value*)dtor.ptr;
            if (v.tag == TENSOR && v.tensor_val != null) count++;
        }
        dtor = dtor.next;
    }
    return count;
}

fn usz runtime_alloc_active_hashmap_dtor_count(main::ScopeRegion* scope) {
    usz count = 0;
    if (scope == null) return count;
    main::ScopeDtor* dtor = scope.dtors;
    while (dtor != null) {
        if (dtor.ptr != null && dtor.func == &scope_dtor_value) {
            Value* v = (Value*)dtor.ptr;
            if ((v.tag == HASHMAP || v.tag == SET) && v.hashmap_val != null) count++;
        }
        dtor = dtor.next;
    }
    return count;
}

fn usz runtime_alloc_active_value_dtor_count(main::ScopeRegion* scope) {
    usz count = 0;
    if (scope == null) return count;
    main::ScopeDtor* dtor = scope.dtors;
    while (dtor != null) {
        if (dtor.ptr != null && dtor.func == &scope_dtor_value) count++;
        dtor = dtor.next;
    }
    return count;
}

fn bool runtime_alloc_process_spawn_cleanup_case(
    Interp* interp,
    int* pass,
    int* fail,
    char[] label,
    int forced_fail_nth,
    int expected_stage,
    bool expect_stdin_wrapper_created,
    bool expect_stdout_wrapper_created,
    bool expect_stderr_wrapper_created,
    bool expect_stdin_cleanup_called,
    bool expect_stdout_cleanup_called,
    bool expect_stderr_cleanup_called
) {
    Value* proc_argv = make_cons(interp,
        make_string(interp, "-c"),
        make_cons(interp, make_string(interp, "sleep 0.05"), make_nil(interp)));
    Value*[3] spawn_args = {
        make_string(interp, "/bin/sh"),
        proc_argv,
        make_nil(interp)
    };

    ProcessSpawnCleanupTrace trace = {};
    int saved_force = g_value_force_make_ffi_box_alloc_fail_nth;
    g_value_force_make_ffi_box_alloc_fail_nth = forced_fail_nth;
    Value* spawn_fail = process_spawn_impl(spawn_args[0:3], null, interp, &trace);
    g_value_force_make_ffi_box_alloc_fail_nth = saved_force;

    bool ok = runtime_alloc_error_matches(spawn_fail, "fs-open: out of memory") &&
        trace.spawn_succeeded &&
        trace.process_handle_created &&
        trace.failed_stage == expected_stage &&
        trace.stdin_wrapper_created == expect_stdin_wrapper_created &&
        trace.stdout_wrapper_created == expect_stdout_wrapper_created &&
        trace.stderr_wrapper_created == expect_stderr_wrapper_created &&
        trace.stdin_cleanup_called == expect_stdin_cleanup_called &&
        trace.stdout_cleanup_called == expect_stdout_cleanup_called &&
        trace.stderr_cleanup_called == expect_stderr_cleanup_called &&
        trace.process_closed;

    if (ok) {
        io::printfn("[PASS] %s", (ZString)label);
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] %s (error=%s spawned=%s handle=%s stage=%d stdin_created=%s stdout_created=%s stderr_created=%s stdin_cleanup=%s stdout_cleanup=%s stderr_cleanup=%s process_closed=%s)",
            (ZString)label,
            runtime_alloc_error_text(spawn_fail),
            trace.spawn_succeeded ? "yes" : "no",
            trace.process_handle_created ? "yes" : "no",
            trace.failed_stage,
            trace.stdin_wrapper_created ? "yes" : "no",
            trace.stdout_wrapper_created ? "yes" : "no",
            trace.stderr_wrapper_created ? "yes" : "no",
            trace.stdin_cleanup_called ? "yes" : "no",
            trace.stdout_cleanup_called ? "yes" : "no",
            trace.stderr_cleanup_called ? "yes" : "no",
            trace.process_closed ? "yes" : "no");
        (*fail)++;
    }

    return ok;
}

fn void run_memory_lifetime_runtime_helper_alloc_tests(Interp* interp, int* pass, int* fail) {
    {
        int saved_force = g_value_force_make_error_chars_alloc_fail_nth;
        g_value_force_make_error_chars_alloc_fail_nth = 1;
        Value* err = make_error(interp, "constructor test message");
        g_value_force_make_error_chars_alloc_fail_nth = saved_force;

        bool ok = runtime_alloc_error_matches(err, "out of memory while allocating error message");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: make_error alloc failure keeps printable fallback");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: make_error alloc failure keeps printable fallback (ok=%s msg=%s)",
                ok ? "yes" : "no",
                runtime_alloc_error_text(err));
            (*fail)++;
        }
    }

    {
        Value*[1] fs_stat_args = { make_string(interp, "/tmp") };
        Value*[1] fs_readdir_args = { make_string(interp, "/tmp") };

        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        bool saved_array_items_fail = g_value_force_make_array_items_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* fs_stat_fail = prim_fs_stat(fs_stat_args[0:1], null, interp);
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        g_value_force_make_array_items_alloc_fail = true;
        Value* fs_readdir_fail = prim_fs_readdir(fs_readdir_args[0:1], null, interp);
        g_value_force_make_array_items_alloc_fail = saved_array_items_fail;

        bool ok =
            runtime_alloc_error_matches(fs_stat_fail, "fs-stat: out of memory") &&
            runtime_alloc_error_matches(fs_readdir_fail, "fs-readdir: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: fs status payload constructors fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: fs status payload constructors fail closed (stat=%s readdir=%s)",
                runtime_alloc_error_text(fs_stat_fail),
                runtime_alloc_error_text(fs_readdir_fail));
            (*fail)++;
        }
    }

    {
        int saved_force = g_boundary_force_alloc_value_fail_nth;
        g_boundary_force_alloc_value_fail_nth = 1;
        Value* array_fail = make_array_checked(interp, 4, "array: out of memory");
        g_boundary_force_alloc_value_fail_nth = 1;
        Value* dict_fail = make_hashmap_checked(interp, 4, "Dictionary: out of memory");
        g_boundary_force_alloc_value_fail_nth = saved_force;

        bool ok =
            runtime_alloc_error_matches(array_fail, "array: out of memory") &&
            runtime_alloc_error_matches(dict_fail, "Dictionary: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: boundary wrapper constructors fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: boundary wrapper constructors fail closed (array=%s dict=%s)",
                runtime_alloc_error_text(array_fail),
                runtime_alloc_error_text(dict_fail)
            );
            (*fail)++;
        }
    }

    {
        Value* prim = interp.alloc_value();
        if (prim != null) {
            prim.tag = PRIMITIVE;
            prim.prim_val = null;
        }
        bool ok = prim != null &&
            !primitive_name_equals(prim, "aot-closure") &&
            !primitive_name_equals(prim, "this-name-is-longer-than-the-primitive-name-backing-store-and-must-not-read-past-it");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: primitive name predicate handles null and long names");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: primitive name predicate handles null and long names");
            (*fail)++;
        }
    }

    {
        usz root_before = interp.root_scope.alloc_count;
        int saved_force = g_value_force_make_primitive_payload_alloc_fail_nth;
        g_value_force_make_primitive_payload_alloc_fail_nth = 1;
        Value* first = make_primitive(interp, "oom-prim", &prim_add, 2);
        g_value_force_make_primitive_payload_alloc_fail_nth = 1;
        Value* second = make_primitive(interp, "oom-prim", &prim_add, 2);
        g_value_force_make_primitive_payload_alloc_fail_nth = saved_force;
        usz root_after = interp.root_scope.alloc_count;

        bool ok = first == null &&
            second == null &&
            root_after == root_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: primitive payload alloc faults do not consume root wrapper slots");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: primitive payload alloc faults do not consume root wrapper slots (first=%s second=%s before=%d after=%d)",
                first == null ? "null" : "value",
                second == null ? "null" : "value",
                (long)root_before,
                (long)root_after);
            (*fail)++;
        }
    }

    {
        usz before = interp.current_scope.alloc_count;
        int saved_force = g_value_force_make_ffi_box_alloc_fail_nth;
        g_value_force_make_ffi_box_alloc_fail_nth = 1;
        Value* first = make_ffi_handle_ex(interp, null, "oom-ffi", null, false, false);
        g_value_force_make_ffi_box_alloc_fail_nth = 1;
        Value* second = make_ffi_handle_ex(interp, null, "oom-ffi", null, false, false);
        g_value_force_make_ffi_box_alloc_fail_nth = saved_force;
        usz after = interp.current_scope.alloc_count;

        bool ok = first == null &&
            second == null &&
            after == before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: ffi box alloc faults do not consume wrapper slots");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: ffi box alloc faults do not consume wrapper slots (first=%s second=%s before=%d after=%d)",
                first == null ? "null" : "value",
                second == null ? "null" : "value",
                (long)before,
                (long)after);
            (*fail)++;
        }
    }

    {
        int* payload = (int*)mem::malloc(int.sizeof);
        bool payload_ok = payload != null;
        if (payload_ok) {
            *payload = 0x7BEEF;
        }

        g_runtime_alloc_ffi_finalizer_calls = 0;
        int saved_force = g_boundary_force_alloc_value_fail_nth;
        g_boundary_force_alloc_value_fail_nth = 1;
        Value* failed = payload_ok
            ? make_ffi_handle_ex(interp, payload, "oom-ffi-finalizer", &runtime_alloc_ffi_finalizer, false, false)
            : null;
        g_boundary_force_alloc_value_fail_nth = saved_force;

        bool ok = payload_ok &&
            failed == null &&
            g_runtime_alloc_ffi_finalizer_calls == 1 &&
            *payload == 0;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: ffi finalizer-owned payload release survives value alloc failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: ffi finalizer-owned payload release survives value alloc failure (payload_ok=%s result=%s calls=%d payload=%d)",
                payload_ok ? "yes" : "no",
                failed == null ? "null" : "value",
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

        g_runtime_alloc_ffi_finalizer_calls = 0;
        int saved_force = g_value_force_make_ffi_box_alloc_fail_nth;
        g_value_force_make_ffi_box_alloc_fail_nth = 1;
        Value* failed = payload_ok
            ? make_ffi_handle_ex(interp, payload, "oom-ffi-box-finalizer", &runtime_alloc_ffi_finalizer, false, false)
            : null;
        g_value_force_make_ffi_box_alloc_fail_nth = saved_force;

        bool ok = payload_ok &&
            failed == null &&
            g_runtime_alloc_ffi_finalizer_calls == 1 &&
            *payload == 0;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: ffi finalizer-owned payload release survives box alloc failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: ffi finalizer-owned payload release survives box alloc failure (payload_ok=%s result=%s calls=%d payload=%d)",
                payload_ok ? "yes" : "no",
                failed == null ? "null" : "value",
                g_runtime_alloc_ffi_finalizer_calls,
                payload_ok ? *payload : -1);
            (*fail)++;
        }

        if (payload_ok) {
            mem::free(payload);
        }
    }

    {
        void* payload = mem::malloc(8);
        bool payload_ok = payload != null;

        int saved_force = g_boundary_force_alloc_value_fail_nth;
        g_boundary_force_alloc_value_fail_nth = 1;
        Value* failed = payload_ok
            ? make_ffi_handle_ex(interp, payload, "oom-ffi-free-only", null, true, false)
            : null;
        g_boundary_force_alloc_value_fail_nth = saved_force;

        bool ok = payload_ok && failed == null;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: ffi free-owned payload release survives value alloc failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: ffi free-owned payload release survives value alloc failure (payload_ok=%s result=%s)",
                payload_ok ? "yes" : "no",
                failed == null ? "null" : "value");
            (*fail)++;
            if (payload_ok && failed == null) {
                mem::free(payload);
            }
        }
    }

    {
        void* payload = mem::malloc(8);
        bool payload_ok = payload != null;

        int saved_force = g_value_force_make_ffi_box_alloc_fail_nth;
        g_value_force_make_ffi_box_alloc_fail_nth = 1;
        Value* failed = payload_ok
            ? make_ffi_handle_ex(interp, payload, "oom-ffi-box-free-only", null, true, false)
            : null;
        g_value_force_make_ffi_box_alloc_fail_nth = saved_force;

        bool ok = payload_ok && failed == null;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: ffi free-owned payload release survives box alloc failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: ffi free-owned payload release survives box alloc failure (payload_ok=%s result=%s)",
                payload_ok ? "yes" : "no",
                failed == null ? "null" : "value");
            (*fail)++;
            if (payload_ok && failed == null) {
                mem::free(payload);
            }
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
        Value* array_fail = setup_ok ? prim_list(array_args[0:1], null, interp) : raise_error(interp, "setup failed");
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
        Value* lookup_fail = setup_ok ? json_pointer_lookup(map, "missing", interp) : raise_error(interp, "setup failed");
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

    {
        MethodSignature sig = {
            .param_count = 1,
        };
        MethodConstraint[1] constraints = {
            { .param_sym = interp.symbols.intern("T"), .bound_type = INVALID_TYPE_ID }
        };
        sig.constraints = &constraints[0];
        sig.constraint_count = 1;

        TypeId[1] arg_types = { INVALID_TYPE_ID };
        SymbolId[1] handler_tags = { interp.symbols.intern("sample") };
        EffectHandler handler = {
            .tags = &handler_tags[0],
            .clauses = null,
            .closures = null,
            .clause_count = 1,
            .handler_env = null,
            .body_expr = null,
            .body_env = null,
            .strict_mode = false,
            .effect_state = null,
        };

        MethodEntry[1] entries = {
            {
                .sig = sig,
                .implementation = make_nil(interp),
            }
        };
        MethodTable mt = {
            .name = interp.symbols.intern("explain-test"),
            .entries = &entries[0],
            .entry_count = 1,
            .fallback = null,
        };
        Value* callee = interp.alloc_value();
        bool setup_ok = callee != null;
        if (setup_ok) {
            callee.tag = METHOD_TABLE;
            callee.method_table_val = &mt;
        }

        Value* params_fail = null;
        Value* constraints_fail = null;
        Value* arg_types_fail = null;
        Value* handler_tags_fail = null;
        Value* dispatch_fail = null;
        Value* effect_fail = null;
        Value*[] no_args = {};

        int saved_cons_fail = g_schema_explain_force_cons_alloc_fail_nth;

        g_schema_explain_force_cons_alloc_fail_nth = 1;
        params_fail = explain_signature_params(interp, &sig);
        g_schema_explain_force_cons_alloc_fail_nth = 1;
        constraints_fail = explain_constraints(interp, &sig);
        g_schema_explain_force_cons_alloc_fail_nth = 1;
        arg_types_fail = explain_arg_types(interp, arg_types[0:1]);
        g_schema_explain_force_cons_alloc_fail_nth = 1;
        handler_tags_fail = explain_effect_handler_tags(interp, &handler);
        g_schema_explain_force_cons_alloc_fail_nth = 1;
        dispatch_fail = setup_ok
            ? explain_dispatch_result_from_evaluated_call(interp, make_nil(interp), callee, no_args)
            : raise_error(interp, "setup failed");

        if (interp.handler_count >= interp.handler_capacity && !interp.grow_handler_stack()) setup_ok = false;
        if (setup_ok) {
            interp.handler_stack[interp.handler_count++] = handler;
            g_schema_explain_force_cons_alloc_fail_nth = 1;
            effect_fail = explain_effect_result_from_compiled_signal(
                interp,
                "(perform sample 1)",
                interp.symbols.intern("sample"),
                make_int(interp, 1));
            interp.handler_count--;
        } else {
            effect_fail = raise_error(interp, "setup failed");
        }

        g_schema_explain_force_cons_alloc_fail_nth = saved_cons_fail;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(params_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(constraints_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(arg_types_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(handler_tags_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(dispatch_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(effect_fail, "schema explain: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: schema explain list builders fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: schema explain list builders fail closed (setup_ok=%s params=%s constraints=%s arg_types=%s handler_tags=%s dispatch=%s effect=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(params_fail),
                runtime_alloc_error_text(constraints_fail),
                runtime_alloc_error_text(arg_types_fail),
                runtime_alloc_error_text(handler_tags_fail),
                runtime_alloc_error_text(dispatch_fail),
                runtime_alloc_error_text(effect_fail));
            (*fail)++;
        }
    }

    {
        MethodSignature sig = { .param_count = 1 };
        MethodTable ambiguous_mt = { .name = interp.symbols.intern("ambiguous-test") };
        DispatchMatchBreakdown breakdown = {
            .failure_reason = DISPATCH_MATCH_FAIL_ARITY,
            .failure_arg_index = usz.max
        };
        Value*[] no_args = {};
        TypeId[] no_types = {};
        Value*[1] ambiguous_args = { make_int(interp, 1) };
        Value* tie_indices =
            make_cons(interp, make_int(interp, 0), make_cons(interp, make_int(interp, 1), make_nil(interp)));

        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* lambda_fail = lambda_call_type_error(null, &sig, no_args, 0, no_types, &breakdown, interp);
        Value* ambiguous_fail = dispatch_ambiguous_error(
            &ambiguous_mt,
            ambiguous_args[0:1],
            10,
            2,
            tie_indices,
            interp);
        Value* ctor_data = ctor_mismatch_data(
            interp,
            interp.symbols.intern("Box"),
            make_cons(interp, make_symbol(interp, interp.sym_Integer), make_nil(interp)),
            make_nil(interp),
            false,
            0,
            null);
        Value* runtime_effect_fail =
            jit_unhandled_effect_error(interp.symbols.intern("sample"), make_int(interp, 1), interp);
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok =
            runtime_alloc_error_matches(lambda_fail, "lambda: expected 1 argument(s), got 0") &&
            runtime_alloc_error_matches(runtime_effect_fail, "runtime effect payload: out of memory") &&
            runtime_alloc_error_matches(ambiguous_fail, "ambiguous method call: multiple methods match with equal specificity") &&
            ctor_data == null;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: dispatch and ctor payload constructors fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: dispatch and ctor payload constructors fail closed (lambda=%s runtime=%s ambiguous=%s ctor_null=%s)",
                runtime_alloc_error_text(lambda_fail),
                runtime_alloc_error_text(runtime_effect_fail),
                runtime_alloc_error_text(ambiguous_fail),
                ctor_data == null ? "yes" : "no");
            (*fail)++;
        }
    }

    {
        usz saved_handler_count = interp.handler_count;
        bool setup_ok = interp.handler_count < interp.handler_capacity || interp.grow_handler_stack();
        SymbolId[1] handler_tags = { interp.sym_raise };
        EffectHandler handler = {
            .tags = &handler_tags[0],
            .clauses = null,
            .closures = null,
            .clause_count = 1,
            .handler_env = null,
            .body_expr = null,
            .body_env = null,
            .strict_mode = false,
            .effect_state = null,
        };

        Value* ctor_data = null;
        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        if (setup_ok) {
            interp.flags.raise_pending = false;
            interp.raise_payload = null;
            interp.raise_msg_len = 0;
            interp.handler_stack[interp.handler_count++] = handler;
            g_hashmap_force_struct_alloc_fail = true;
            ctor_data = ctor_mismatch_data(
                interp,
                interp.symbols.intern("Box"),
                make_cons(interp, make_symbol(interp, interp.sym_Integer), make_nil(interp)),
                make_nil(interp),
                false,
                0,
                null);
            g_hashmap_force_struct_alloc_fail = saved_struct_fail;
            interp.handler_count = saved_handler_count;
        }
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok = setup_ok &&
                  ctor_data == null &&
                  !interp.flags.raise_pending &&
                  interp.raise_payload == null &&
                  interp.raise_msg_len == 0;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: ctor mismatch payload allocation failure does not preseed raise");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: ctor mismatch payload allocation failure does not preseed raise (setup_ok=%s ctor_null=%s pending=%s)",
                setup_ok ? "yes" : "no",
                ctor_data == null ? "yes" : "no",
                interp.flags.raise_pending ? "yes" : "no");
            interp.handler_count = saved_handler_count;
            interp.flags.raise_pending = false;
            interp.raise_payload = null;
            interp.raise_msg_len = 0;
            (*fail)++;
        }
    }

    {
        usz saved_handler_count = interp.handler_count;
        bool setup_ok = interp.handler_count < interp.handler_capacity || interp.grow_handler_stack();
        SymbolId[1] handler_tags = { interp.sym_raise };
        EffectHandler handler = {
            .tags = &handler_tags[0],
            .clauses = null,
            .closures = null,
            .clause_count = 1,
            .handler_env = null,
            .body_expr = null,
            .body_env = null,
            .strict_mode = false,
            .effect_state = null,
        };

        SymbolId rel_sym = interp.symbols.intern("integrity-audit-rel");
        SymbolId col_sym = interp.symbols.intern("id");
        SymbolId[1] columns = { col_sym };
        Relation rel = {};
        rel.name = rel_sym;
        rel.columns = &columns[0];
        rel.col_count = 1;
        rel.key_col = col_sym;

        Value*[1] attempted = { make_int(interp, 1) };
        Value*[1] existing = { make_int(interp, 2) };
        Value* integrity_data = null;
        Value* iteration_data = null;
        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        if (setup_ok) {
            interp.flags.raise_pending = false;
            interp.raise_payload = null;
            interp.raise_msg_len = 0;
            interp.handler_stack[interp.handler_count++] = handler;
            g_hashmap_force_struct_alloc_fail = true;
            integrity_data = deduce_build_key_integrity_payload(&rel, attempted[..], existing[..], 0, interp);
            iteration_data = deduce_analyze_iteration_limit_payload(interp, 1, 32);
            g_hashmap_force_struct_alloc_fail = saved_struct_fail;
            interp.handler_count = saved_handler_count;
        }
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok = setup_ok &&
                  integrity_data == null &&
                  iteration_data == null &&
                  !interp.flags.raise_pending &&
                  interp.raise_payload == null &&
                  interp.raise_msg_len == 0;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: deduce optional payload allocation failures do not preseed raise");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: deduce optional payload allocation failures do not preseed raise (setup_ok=%s integrity_null=%s iteration_null=%s pending=%s)",
                setup_ok ? "yes" : "no",
                integrity_data == null ? "yes" : "no",
                iteration_data == null ? "yes" : "no",
                interp.flags.raise_pending ? "yes" : "no");
            interp.handler_count = saved_handler_count;
            interp.flags.raise_pending = false;
            interp.raise_payload = null;
            interp.raise_msg_len = 0;
            (*fail)++;
        }
    }

    {
        char[] response = "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n";
        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* response_fail = parse_response(response, interp);
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok = runtime_alloc_error_matches(response_fail, "http: failed to allocate response payload");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: http response payload constructor fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: http response payload constructor fails closed (msg=%s)",
                runtime_alloc_error_text(response_fail));
            (*fail)++;
        }
    }

    {
        char key_byte = 'k';
        Value* support_map = null;
        DeduceDeltaTupleSet delta_set = {};
        DeduceSemiNaiveRuleExec proof_exec = {};
        proof_exec.interp = interp;
        proof_exec.aggregate_component_mode = true;
        bool already_seen = false;
        bool[] empty_bound = {};
        Value*[] empty_values = {};

        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* delta_err = deduce_delta_set_insert(&delta_set, &key_byte, 1, interp);
        Value* support_err = deduce_tuple_support_table_set_count(&support_map, &key_byte, 1, 1, interp);
        Value* proof_err = deduce_seminaive_mark_current_proof_seen(
            &proof_exec,
            empty_bound,
            empty_values,
            &already_seen
        );
        Value* selector_err = deduce_analyze_goal_directed_selector_error(7, 1, make_nil(interp), interp);
        Value* surface_err = deduce_goal_directed_selector_surface_error(
            "deduce/test-selector",
            "deduce: selector test",
            7,
            1,
            make_nil(interp),
            interp
        );
        Value* relation_err = deduce_goal_directed_relation_surface_error(
            "deduce/test-relation",
            "deduce: relation test",
            7,
            1,
            interp.symbols.intern("rel"),
            interp
        );
        Value* iteration_payload = deduce_analyze_iteration_limit_payload(interp, 1, 32);
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;
        bool ok =
            runtime_alloc_error_matches(delta_err, "deduce/analyze: out of memory") &&
            runtime_alloc_error_matches(support_err, "deduce/analyze: out of memory") &&
            runtime_alloc_error_matches(proof_err, "deduce/analyze: out of memory") &&
            runtime_alloc_error_matches(selector_err, "deduce/analyze: out of memory") &&
            runtime_alloc_error_matches(surface_err, "deduce: out of memory") &&
            runtime_alloc_error_matches(relation_err, "deduce: out of memory") &&
            iteration_payload == null;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: deduce runtime helper state payload constructors fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: deduce runtime helper state payload constructors fail closed (delta=%s support=%s proof=%s selector=%s surface=%s relation=%s iteration_null=%s)",
                runtime_alloc_error_text(delta_err),
                runtime_alloc_error_text(support_err),
                runtime_alloc_error_text(proof_err),
                runtime_alloc_error_text(selector_err),
                runtime_alloc_error_text(surface_err),
                runtime_alloc_error_text(relation_err),
                iteration_payload == null ? "yes" : "no");
            (*fail)++;
        }
    }

    {
        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* iter_raise = iterator_ctor_raise(interp, "type/arg-mismatch", "__iterator-map: expected iterator state");
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;
        bool ok = runtime_alloc_error_matches(iter_raise, "__iterator-map: expected iterator state");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: iterator ctor raise survives payload hashmap alloc failure");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: iterator ctor raise survives payload hashmap alloc failure (ok=%s msg=%s)",
                ok ? "yes" : "no",
                runtime_alloc_error_text(iter_raise));
            (*fail)++;
        }
    }

    {
        Value* invalid_iter = interp.alloc_value();
        bool setup_ok = invalid_iter != null;
        if (setup_ok) {
            invalid_iter.tag = ITERATOR;
            invalid_iter.iterator_val = make_nil(interp);
        }
        Value*[1] args = { invalid_iter };
        Value* ctor_err = setup_ok ? prim_ctor_iterator(args[0:1], null, interp) : raise_error(interp, "setup failed");
        Value* next_err = setup_ok ? prim_next(args[0:1], null, interp) : raise_error(interp, "setup failed");

        bool ok = setup_ok &&
            runtime_alloc_error_matches(ctor_err, "Iterator: expected valid iterator") &&
            runtime_alloc_error_matches(next_err, "next: expected iterator");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: malformed iterator wrapper fails constructor and terminal gates");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: malformed iterator wrapper fails constructor and terminal gates (setup_ok=%s ctor=%s next=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(ctor_err),
                runtime_alloc_error_text(next_err));
            (*fail)++;
        }
    }

    {
        Value*[1] no_args = { null };
        Value* pred = prim_coroutine_p(no_args[0:0], null, interp);
        Value* null_ctx = make_coroutine(interp, null);

        bool ok =
            pred != null &&
            pred.tag == NIL &&
            runtime_alloc_error_matches(null_ctx, "coroutine: missing context");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: coroutine predicate and constructor reject malformed inputs");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: coroutine predicate and constructor reject malformed inputs (pred_tag=%d msg=%s)",
                pred != null ? (int)pred.tag : -1,
                runtime_alloc_error_text(null_ctx));
            (*fail)++;
        }
    }

    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        bool setup_ok = tmp != null;
        if (setup_ok) {
            tmp.init();
            setup_ok = register_primitives(tmp);
        }

        main::ScopeRegion* saved = setup_ok ? tmp.current_scope : null;
        main::ScopeRegion* foreign = setup_ok ? main::scope_create(saved) : null;
        Value* thunk = null;
        usz before_all = 0;
        usz before_pool = 0;
        Value* result = null;

        if (setup_ok) {
            setup_ok = foreign != null;
        }
        if (setup_ok) {
            tmp.current_scope = foreign;
            thunk = make_closure_no_param(tmp, null, tmp.global_env);
            tmp.current_scope = saved;
            setup_ok = thunk != null && thunk.tag == CLOSURE;
        }
        if (setup_ok) {
            Value*[1] coro_args = { thunk };
            bool saved_force = g_boundary_force_copy_closure_wrapper_alloc_fail;
            before_all = tmp.stack_ctx_pool.all_count;
            before_pool = tmp.stack_ctx_pool.pool_size;
            tmp.current_scope = foreign;
            g_boundary_force_copy_closure_wrapper_alloc_fail = true;
            result = prim_coroutine(coro_args[0:1], null, tmp);
            g_boundary_force_copy_closure_wrapper_alloc_fail = saved_force;
            tmp.current_scope = saved;
        }

        bool ok = setup_ok &&
            result != null &&
            result.tag == ERROR &&
            tmp.stack_ctx_pool.all_count == before_all &&
            tmp.stack_ctx_pool.pool_size == before_pool;
        int after_all = tmp != null ? (int)tmp.stack_ctx_pool.all_count : -1;
        int after_pool = tmp != null ? (int)tmp.stack_ctx_pool.pool_size : -1;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: coroutine thunk promotion failure aborts before context creation");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: coroutine thunk promotion failure aborts before context creation (setup_ok=%s tag=%d all=%d pool=%d before_all=%d before_pool=%d)",
                setup_ok ? "yes" : "no",
                result != null ? (int)result.tag : -1,
                after_all,
                after_pool,
                (int)before_all,
                (int)before_pool);
            (*fail)++;
        }

        if (foreign != null) main::scope_release(foreign);
        if (tmp != null) {
            tmp.destroy();
            mem::free(tmp);
        }
    }

    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        bool setup_ok = tmp != null;
        if (setup_ok) {
            tmp.init();
            setup_ok = register_primitives(tmp);
        }

        Value* thunk = null;
        usz before_all = 0;
        usz before_pool = 0;
        Value* result = null;

        if (setup_ok) {
            thunk = make_closure_no_param(tmp, null, tmp.global_env);
            setup_ok = thunk != null && thunk.tag == CLOSURE;
        }
        if (setup_ok) {
            Value*[1] coro_args = { thunk };
            int saved_force = g_boundary_force_alloc_value_fail_nth;
            before_all = tmp.stack_ctx_pool.all_count;
            before_pool = tmp.stack_ctx_pool.pool_size;
            g_boundary_force_alloc_value_fail_nth = 1;
            result = prim_coroutine(coro_args[0:1], null, tmp);
            g_boundary_force_alloc_value_fail_nth = saved_force;
        }

        bool asan_stack_pool = main::stack_runtime_asan_enabled();
        usz expected_all = before_all + (asan_stack_pool ? 0 : 1);
        usz expected_pool = before_pool + (asan_stack_pool ? 0 : 1);

        bool ok = setup_ok &&
            runtime_alloc_error_matches(result, "coroutine: failed to allocate coroutine value") &&
            tmp.stack_ctx_pool.all_count == expected_all &&
            tmp.stack_ctx_pool.pool_size == expected_pool;
        int after_all = tmp != null ? (int)tmp.stack_ctx_pool.all_count : -1;
        int after_pool = tmp != null ? (int)tmp.stack_ctx_pool.pool_size : -1;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: coroutine wrapper allocation failure cleans up context");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: coroutine wrapper allocation failure cleans up context (setup_ok=%s msg=%s all=%d pool=%d before_all=%d before_pool=%d)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(result),
                after_all,
                after_pool,
                (int)before_all,
                (int)before_pool
            );
            (*fail)++;
        }

        if (tmp != null) {
            tmp.destroy();
            mem::free(tmp);
        }
    }

    {
        Value* partial = interp.alloc_value();
        Value* arg = make_int(interp, 42);
        bool setup_ok = partial != null && arg != null && arg.tag == INT;
        if (setup_ok) {
            partial.tag = PARTIAL_PRIM;
            partial.partial_val.func = &prim_add;
            partial.partial_val.first_arg = make_int(interp, 1);
            partial.partial_val.second_arg = null;
            partial.partial_val.remaining = 2;
        }

        bool saved_force = g_eval_force_partial_alloc_fail;
        g_eval_force_partial_alloc_fail = true;
        EvalResult result = setup_ok
            ? apply_partial(partial, arg, interp)
            : eval_error("setup failed");
        g_eval_force_partial_alloc_fail = saved_force;

        bool ok = setup_ok &&
            result.error.has_error &&
            str_starts_with(result.error.message[:256], "out of memory while allocating partial application");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: apply_partial alloc failure returns eval error");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: apply_partial alloc failure returns eval error (setup_ok=%s error=%s)",
                setup_ok ? "yes" : "no",
                result.error.has_error ? (ZString)result.error.message[:256] : (ZString)"<none>");
            (*fail)++;
        }
    }

    {
        Value* partial = interp.alloc_value();
        Value* arg = make_int(interp, 42);
        bool setup_ok = partial != null && arg != null && arg.tag == INT;
        if (setup_ok) {
            partial.tag = PARTIAL_PRIM;
            partial.partial_val.func = &prim_add;
            partial.partial_val.first_arg = make_int(interp, 1);
            partial.partial_val.second_arg = make_int(interp, 2);
            partial.partial_val.remaining = 3;
        }

        EvalResult result = setup_ok
            ? apply_partial(partial, arg, interp)
            : eval_error("setup failed");

        bool ok = setup_ok &&
            result.error.has_error &&
            str_starts_with(result.error.message[:256], "partial application state exceeds supported captured-arg width");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: apply_partial oversized state fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: apply_partial oversized state fails closed (setup_ok=%s error=%s)",
                setup_ok ? "yes" : "no",
                result.error.has_error ? (ZString)result.error.message[:256] : (ZString)"<none>");
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* promoted = null;
        if (setup_ok) {
            interp.current_scope = child;
            Value* escape_first = interp.alloc_value_escape();
            Value* escape_second = interp.alloc_value_escape();
            Value* partial = interp.alloc_value();
            setup_ok = escape_first != null && escape_second != null && partial != null;
            if (setup_ok) {
                escape_first.tag = INT;
                escape_first.int_val = 1;
                escape_second.tag = INT;
                escape_second.int_val = 2;
                partial.tag = PARTIAL_PRIM;
                partial.partial_val.func = &prim_add;
                partial.partial_val.first_arg = escape_first;
                partial.partial_val.second_arg = escape_second;
                partial.partial_val.remaining = 1;
                int saved_force = g_boundary_force_alloc_escape_value_fail_nth;
                g_boundary_force_alloc_escape_value_fail_nth = 1;
                promoted = promote_to_escape(partial, interp);
                g_boundary_force_alloc_escape_value_fail_nth = saved_force;
            }
            interp.current_scope = saved;
        }

        bool ok = setup_ok &&
            runtime_alloc_error_matches(promoted, "boundary: failed to allocate partial primitive escape copy");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: partial primitive escape wrapper alloc faults fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: partial primitive escape wrapper alloc faults fail closed (setup_ok=%s msg=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(promoted));
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        Value* arg = make_int(interp, 42);
        Value* negative_partial = interp.alloc_value();
        Value* null_func_partial = interp.alloc_value();
        Value* inconsistent_partial = interp.alloc_value();
        Value* null_first_arg_partial = interp.alloc_value();
        Value* invalid_prim = interp.alloc_value();
        bool setup_ok = arg != null && arg.tag == INT &&
            negative_partial != null &&
            null_func_partial != null &&
            inconsistent_partial != null &&
            null_first_arg_partial != null &&
            invalid_prim != null;
        if (setup_ok) {
            *negative_partial = {};
            negative_partial.tag = PARTIAL_PRIM;
            negative_partial.partial_val.func = &prim_add;
            negative_partial.partial_val.first_arg = make_int(interp, 1);
            negative_partial.partial_val.second_arg = null;
            negative_partial.partial_val.remaining = -1;

            *null_func_partial = {};
            null_func_partial.tag = PARTIAL_PRIM;
            null_func_partial.partial_val.func = null;
            null_func_partial.partial_val.first_arg = make_int(interp, 1);
            null_func_partial.partial_val.second_arg = null;
            null_func_partial.partial_val.remaining = 1;

            *inconsistent_partial = {};
            inconsistent_partial.tag = PARTIAL_PRIM;
            inconsistent_partial.partial_val.func = &prim_add;
            inconsistent_partial.partial_val.first_arg = make_int(interp, 1);
            inconsistent_partial.partial_val.second_arg = make_int(interp, 2);
            inconsistent_partial.partial_val.remaining = 2;

            *null_first_arg_partial = {};
            null_first_arg_partial.tag = PARTIAL_PRIM;
            null_first_arg_partial.partial_val.func = &prim_add;
            null_first_arg_partial.partial_val.first_arg = null;
            null_first_arg_partial.partial_val.second_arg = null;
            null_first_arg_partial.partial_val.remaining = 1;

            *invalid_prim = {};
            invalid_prim.tag = PRIMITIVE;
            invalid_prim.prim_val = null;

            setup_ok = negative_partial.partial_val.first_arg != null &&
                null_func_partial.partial_val.first_arg != null &&
                inconsistent_partial.partial_val.first_arg != null &&
                inconsistent_partial.partial_val.second_arg != null;
        }

        EvalResult negative_result = setup_ok
            ? apply_partial(negative_partial, arg, interp)
            : eval_error("setup failed");
        EvalResult null_func_result = setup_ok
            ? apply_partial(null_func_partial, arg, interp)
            : eval_error("setup failed");
        EvalResult inconsistent_result = setup_ok
            ? apply_partial(inconsistent_partial, arg, interp)
            : eval_error("setup failed");
        EvalResult null_first_arg_result = setup_ok
            ? apply_partial(null_first_arg_partial, arg, interp)
            : eval_error("setup failed");
        EvalResult invalid_prim_result = setup_ok
            ? apply_primitive(invalid_prim, arg, interp)
            : eval_error("setup failed");

        bool ok = setup_ok &&
            negative_result.error.has_error &&
            null_func_result.error.has_error &&
            inconsistent_result.error.has_error &&
            null_first_arg_result.error.has_error &&
            invalid_prim_result.error.has_error &&
            str_starts_with(negative_result.error.message[:256], "invalid partial application state") &&
            str_starts_with(null_func_result.error.message[:256], "invalid partial application state") &&
            str_starts_with(inconsistent_result.error.message[:256], "invalid partial application state") &&
            str_starts_with(null_first_arg_result.error.message[:256], "invalid partial application state") &&
            str_starts_with(invalid_prim_result.error.message[:256], "invalid primitive application state");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: apply helpers invalid states fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: apply helpers invalid states fail closed (setup_ok=%s neg=%s null_fn=%s inconsistent=%s null_first=%s invalid_prim=%s)",
                setup_ok ? "yes" : "no",
                negative_result.error.has_error ? (ZString)negative_result.error.message[:256] : (ZString)"<none>",
                null_func_result.error.has_error ? (ZString)null_func_result.error.message[:256] : (ZString)"<none>",
                inconsistent_result.error.has_error ? (ZString)inconsistent_result.error.message[:256] : (ZString)"<none>",
                null_first_arg_result.error.has_error ? (ZString)null_first_arg_result.error.message[:256] : (ZString)"<none>",
                invalid_prim_result.error.has_error ? (ZString)invalid_prim_result.error.message[:256] : (ZString)"<none>");
            (*fail)++;
        }
    }

    {
        Value* list = make_cons(interp, make_int(interp, 1), make_cons(interp, make_int(interp, 2), make_nil(interp)));
        Value* iterator = iterator_from_list_value(list, interp);
        Value*[1] to_array_args = { iterator };

        bool saved_array_items_fail = g_value_force_make_array_items_alloc_fail;
        g_value_force_make_array_items_alloc_fail = true;
        Value* array_fail = prim_to_array(to_array_args[0:1], null, interp);
        g_value_force_make_array_items_alloc_fail = saved_array_items_fail;

        bool ok = runtime_alloc_error_matches(array_fail, "to-array: failed to allocate result array");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: to-array constructor alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: to-array constructor alloc failures fail closed (ok=%s msg=%s)",
                ok ? "yes" : "no",
                runtime_alloc_error_text(array_fail));
            (*fail)++;
        }
    }

    {
        Value* list = make_cons(interp, make_int(interp, 1), make_cons(interp, make_int(interp, 2), make_nil(interp)));
        Value* direct_fail = null;
        Value* prim_array_fail = null;
        Value* list_to_array_fail = null;

        bool saved_struct_fail = g_value_force_make_array_struct_alloc_fail;
        bool saved_items_fail = g_value_force_make_array_items_alloc_fail;

        g_value_force_make_array_struct_alloc_fail = true;
        direct_fail = make_array(interp, 2);
        g_value_force_make_array_struct_alloc_fail = false;
        g_value_force_make_array_items_alloc_fail = true;

        Value*[2] prim_array_args = { make_int(interp, 1), make_int(interp, 2) };
        prim_array_fail = prim_array(prim_array_args[0:2], null, interp);
        Value*[1] list_to_array_args = { list };
        list_to_array_fail = prim_list_to_array(list_to_array_args[0:1], null, interp);

        g_value_force_make_array_items_alloc_fail = saved_items_fail;
        g_value_force_make_array_struct_alloc_fail = saved_struct_fail;

        bool ok =
            runtime_alloc_error_matches(direct_fail, "array: out of memory") &&
            runtime_alloc_error_matches(prim_array_fail, "array: out of memory") &&
            runtime_alloc_error_matches(list_to_array_fail, "array: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: raw array constructor alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: raw array constructor alloc failures fail closed (direct=%s prim=%s list=%s)",
                runtime_alloc_error_text(direct_fail),
                runtime_alloc_error_text(prim_array_fail),
                runtime_alloc_error_text(list_to_array_fail));
            (*fail)++;
        }
    }

    {
        usz[2] shape_dims = { 2, 3 };
        Value* tensor = make_tensor_double_uninitialized(interp, shape_dims[0:2]);
        TensorVal* clone = tensor != null && tensor.tag == TENSOR ? tensor_clone_payload(tensor.tensor_val) : null;
        Value*[1] tensor_args = { tensor };
        Value* dtype_val = tensor != null ? prim_dtype(tensor_args[0:1], null, interp) : null;
        Value* rank_val = tensor != null ? prim_rank(tensor_args[0:1], null, interp) : null;
        Value* shape_val = tensor != null ? prim_shape(tensor_args[0:1], null, interp) : null;
        Value* length_val = tensor != null ? prim_length(tensor_args[0:1], null, interp) : null;
        char[128] printed;
        usz printed_len = tensor != null ? print_value_to_buf(tensor, &interp.symbols, &printed[0], 128) : 0;

        bool ok = tensor != null &&
            tensor.tag == TENSOR &&
            tensor.tensor_val != null &&
            tensor.tensor_val.dtype == TENSOR_DTYPE_DOUBLE &&
            tensor.tensor_val.rank == 2 &&
            tensor.tensor_val.shape != null &&
            tensor.tensor_val.shape[0] == 2 &&
            tensor.tensor_val.shape[1] == 3 &&
            tensor.tensor_val.strides != null &&
            tensor.tensor_val.strides[0] == 3 &&
            tensor.tensor_val.strides[1] == 1 &&
            tensor.tensor_val.element_count == 6 &&
            tensor.tensor_val.byte_len == double.sizeof * 6 &&
            tensor.tensor_val.data != null &&
            clone != null &&
            clone != tensor.tensor_val &&
            clone.shape != tensor.tensor_val.shape &&
            clone.strides != tensor.tensor_val.strides &&
            clone.data != tensor.tensor_val.data &&
            clone.element_count == 6 &&
            is_tensor(tensor) &&
            value_type_name(tensor, interp) == interp.sym_Tensor &&
            infer_value_type(tensor, interp) == interp.tid_Tensor &&
            dtype_val != null &&
            dtype_val.tag == SYMBOL &&
            dtype_val.sym_val == interp.sym_Double &&
            rank_val != null &&
            rank_val.tag == INT &&
            rank_val.int_val == 2 &&
            shape_val != null &&
            shape_val.tag == ARRAY &&
            shape_val.array_val != null &&
            shape_val.array_val.length == 2 &&
            shape_val.array_val.items[0] != null &&
            shape_val.array_val.items[0].tag == INT &&
            shape_val.array_val.items[0].int_val == 2 &&
            shape_val.array_val.items[1] != null &&
            shape_val.array_val.items[1].tag == INT &&
            shape_val.array_val.items[1].int_val == 3 &&
            length_val != null &&
            length_val.tag == INT &&
            length_val.int_val == 6 &&
            printed_len > 0 &&
            str_contains(printed[:printed_len], "#<tensor Float64 shape [2 3]>");

        tensor_free_payload(clone);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor payload and introspection primitives");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor payload and introspection primitives (tag=%d dtype=%s rank=%s shape=%s length=%s print=%s)",
                tensor != null ? (int)tensor.tag : -1,
                dtype_val != null && dtype_val.tag == SYMBOL ? "yes" : "no",
                rank_val != null && rank_val.tag == INT ? "yes" : "no",
                shape_val != null && shape_val.tag == ARRAY ? "yes" : "no",
                length_val != null && length_val.tag == INT ? "yes" : "no",
                printed_len > 0 ? (ZString)&printed : (ZString)"<none>");
            (*fail)++;
        }
    }

    {
        usz[1] shape_dims = { 3 };
        TensorVal* tensor = tensor_alloc_payload_checked(TENSOR_DTYPE_FLOAT32, shape_dims[0:1], true, "test tensor allocation failed");
        bool zero_initialized = false;
        if (tensor != null && tensor.data != null) {
            float* values = (float*)tensor.data;
            zero_initialized = values[0] == 0.0f && values[1] == 0.0f && values[2] == 0.0f;
            values[1] = 2.5f;
        }
        TensorVal* clone = tensor != null ? tensor_clone_payload(tensor) : null;
        bool clone_valid = clone != null &&
            clone != tensor &&
            clone.data != tensor.data &&
            clone.dtype == TENSOR_DTYPE_FLOAT32 &&
            clone.rank == 1 &&
            clone.element_count == 3 &&
            clone.byte_len == float.sizeof * 3 &&
            clone.data != null &&
            ((float*)clone.data)[1] == 2.5f;

        bool ok = tensor != null &&
            tensor.dtype == TENSOR_DTYPE_FLOAT32 &&
            tensor.rank == 1 &&
            tensor.shape != null &&
            tensor.shape[0] == 3 &&
            tensor.strides != null &&
            tensor.strides[0] == 1 &&
            tensor.element_count == 3 &&
            tensor.byte_len == float.sizeof * 3 &&
            zero_initialized &&
            clone_valid;

        tensor_free_payload(clone);
        tensor_free_payload(tensor);

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor Float32 payload clone and byte length");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor Float32 payload clone and byte length (tensor=%s clone=%s zero=%s)",
                tensor != null ? "yes" : "no",
                clone_valid ? "yes" : "no",
                zero_initialized ? "yes" : "no");
            (*fail)++;
        }
    }

    {
        g_runtime_alloc_tensor_device_finalizer_calls = 0;
        int device_marker = 123;
        usz[1] shape_dims = { 1 };
        TensorVal* tensor = tensor_alloc_payload_checked(TENSOR_DTYPE_DOUBLE, shape_dims[0:1], true, "test tensor allocation failed");
        bool cpu_storage_valid = tensor_has_valid_storage(tensor);
        if (tensor != null) {
            if (tensor.data != null) {
                mem::free(tensor.data);
                tensor.data = null;
            }
            tensor.device = TENSOR_DEVICE_CUDA;
            tensor.device_handle = (void*)&device_marker;
            tensor.device_finalizer = &runtime_alloc_tensor_device_finalizer;
        }
        bool cuda_device_metadata_valid = tensor_has_valid_device_storage(tensor, TENSOR_DEVICE_CUDA);
        TensorVal* clone = tensor != null ? tensor_clone_payload(tensor) : null;
        tensor_free_payload(tensor);

        bool ok = cpu_storage_valid &&
            cuda_device_metadata_valid &&
            clone == null &&
            g_runtime_alloc_tensor_device_finalizer_calls == 1 &&
            device_marker == 0;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor opaque device payload finalizer");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor opaque device payload finalizer (cpu_valid=%s cuda_metadata=%s clone_null=%s calls=%d marker=%d)",
                cpu_storage_valid ? "yes" : "no",
                cuda_device_metadata_valid ? "yes" : "no",
                clone == null ? "yes" : "no",
                g_runtime_alloc_tensor_device_finalizer_calls,
                device_marker);
            (*fail)++;
            tensor_free_payload(clone);
        }
    }

    {
        g_runtime_alloc_tensor_cuda_free_finalizer_calls = 0;
        bool cuda_available = omni_tensor_backend_cuda_available() != 0;
        bool ok = true;

        if (cuda_available) {
            usz[1] shape_dims = { 2 };
            double[2] host_values = { 1.25, 2.5 };
            double[2] clone_values = { 0.0, 0.0 };
            void* source_device = null;
            int alloc_status = omni_tensor_backend_cuda_alloc(double.sizeof * 2, &source_device);
            int copy_status = alloc_status == OMNI_TENSOR_CUDA_SUCCESS
                ? omni_tensor_backend_cuda_copy_to_existing_device((void*)&host_values[0], double.sizeof * 2, source_device)
                : OMNI_TENSOR_CUDA_UNAVAILABLE;

            TensorVal* tensor = tensor_alloc_payload_checked(TENSOR_DTYPE_DOUBLE, shape_dims[0:1], true, "test tensor allocation failed");
            bool source_owned_by_tensor = false;
            if (tensor != null && copy_status == OMNI_TENSOR_CUDA_SUCCESS && source_device != null) {
                if (tensor.data != null) {
                    mem::free(tensor.data);
                    tensor.data = null;
                }
                tensor.device = TENSOR_DEVICE_CUDA;
                tensor.device_handle = source_device;
                tensor.device_finalizer = &runtime_alloc_tensor_cuda_free_finalizer;
                source_owned_by_tensor = true;
            }

            TensorVal* clone = tensor != null ? tensor_clone_payload(tensor) : null;
            int copyback_status = clone != null && clone.device_handle != null
                ? omni_tensor_backend_cuda_copy_to_host(clone.device_handle, clone.byte_len, (void*)&clone_values[0])
                : OMNI_TENSOR_CUDA_UNAVAILABLE;

            ok = alloc_status == OMNI_TENSOR_CUDA_SUCCESS &&
                copy_status == OMNI_TENSOR_CUDA_SUCCESS &&
                source_device != null &&
                tensor != null &&
                clone != null &&
                clone.device == TENSOR_DEVICE_CUDA &&
                clone.device_handle != null &&
                clone.device_handle != source_device &&
                clone.device_finalizer == &tensor_cuda_device_finalizer &&
                copyback_status == OMNI_TENSOR_CUDA_SUCCESS &&
                clone_values[0] == 1.25 &&
                clone_values[1] == 2.5;

            if (clone != null && clone.device_handle == source_device) {
                clone.device_handle = null;
                clone.device_finalizer = null;
            }

            tensor_free_payload(tensor);
            tensor_free_payload(clone);
            if (!source_owned_by_tensor && source_device != null) {
                omni_tensor_backend_cuda_free(source_device);
            }

            ok = ok && g_runtime_alloc_tensor_cuda_free_finalizer_calls == 1;
        }

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor foreign CUDA payload clone deep-copies to Omni-owned storage");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor foreign CUDA payload clone deep-copies to Omni-owned storage (cuda=%s calls=%d)",
                cuda_available ? "yes" : "no",
                g_runtime_alloc_tensor_cuda_free_finalizer_calls);
            (*fail)++;
        }
    }

    {
        g_runtime_alloc_tensor_device_finalizer_calls = 0;
        int device_marker = 124;
        usz[1] shape_dims = { 1 };
        TensorVal* tensor = tensor_alloc_payload_checked(TENSOR_DTYPE_DOUBLE, shape_dims[0:1], true, "test tensor allocation failed");
        bool cpu_storage_valid = tensor_has_valid_storage(tensor);
        if (tensor != null) {
            if (tensor.data != null) {
                mem::free(tensor.data);
                tensor.data = null;
            }
            tensor.device = TENSOR_DEVICE_VULKAN;
            tensor.device_handle = (void*)&device_marker;
            tensor.device_finalizer = &runtime_alloc_tensor_device_finalizer;
        }
        bool vulkan_storage_rejected = tensor != null && !tensor_has_valid_storage(tensor);
        TensorVal* clone = tensor != null ? tensor_clone_payload(tensor) : null;
        tensor_free_payload(tensor);

        bool ok = cpu_storage_valid &&
            vulkan_storage_rejected &&
            clone == null &&
            g_runtime_alloc_tensor_device_finalizer_calls == 1 &&
            device_marker == 0;

        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor opaque Vulkan device payload finalizer");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor opaque Vulkan device payload finalizer (cpu_valid=%s vulkan_rejected=%s clone_null=%s calls=%d marker=%d)",
                cpu_storage_valid ? "yes" : "no",
                vulkan_storage_rejected ? "yes" : "no",
                clone == null ? "yes" : "no",
                g_runtime_alloc_tensor_device_finalizer_calls,
                device_marker);
            (*fail)++;
            tensor_free_payload(clone);
        }
    }

    {
        usz[1] tensor_shape = { 4 };
        bool saved_struct_fail = g_value_force_make_tensor_struct_alloc_fail;
        bool saved_shape_fail = g_value_force_make_tensor_shape_alloc_fail;
        bool saved_strides_fail = g_value_force_make_tensor_strides_alloc_fail;
        bool saved_data_fail = g_value_force_make_tensor_data_alloc_fail;

        g_value_force_make_tensor_struct_alloc_fail = true;
        Value* struct_fail = make_tensor_double_uninitialized_checked(interp, tensor_shape[0:1], "Tensor: out of memory");
        g_value_force_make_tensor_struct_alloc_fail = false;
        g_value_force_make_tensor_shape_alloc_fail = true;
        Value* shape_fail = make_tensor_double_uninitialized_checked(interp, tensor_shape[0:1], "Tensor: out of memory");
        g_value_force_make_tensor_shape_alloc_fail = false;
        g_value_force_make_tensor_strides_alloc_fail = true;
        Value* strides_fail = make_tensor_double_uninitialized_checked(interp, tensor_shape[0:1], "Tensor: out of memory");
        g_value_force_make_tensor_strides_alloc_fail = false;
        g_value_force_make_tensor_data_alloc_fail = true;
        Value* data_fail = make_tensor_double_uninitialized_checked(interp, tensor_shape[0:1], "Tensor: out of memory");

        g_value_force_make_tensor_data_alloc_fail = saved_data_fail;
        g_value_force_make_tensor_strides_alloc_fail = saved_strides_fail;
        g_value_force_make_tensor_shape_alloc_fail = saved_shape_fail;
        g_value_force_make_tensor_struct_alloc_fail = saved_struct_fail;

        bool ok =
            runtime_alloc_error_matches(struct_fail, "Tensor: out of memory") &&
            runtime_alloc_error_matches(shape_fail, "Tensor: out of memory") &&
            runtime_alloc_error_matches(strides_fail, "Tensor: out of memory") &&
            runtime_alloc_error_matches(data_fail, "Tensor: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: tensor constructor alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: tensor constructor alloc failures fail closed (struct=%s shape=%s strides=%s data=%s)",
                runtime_alloc_error_text(struct_fail),
                runtime_alloc_error_text(shape_fail),
                runtime_alloc_error_text(strides_fail),
                runtime_alloc_error_text(data_fail));
            (*fail)++;
        }
    }

    {
        EvalResult expr_eval = run("(map + (Tensor Float64 [2] [1 2]) 3)", interp);
        EvalResult index_eval = run("[1]", interp);
        bool setup_ok = !expr_eval.error.has_error &&
            expr_eval.value != null &&
            expr_eval.value.tag == TENSOR &&
            expr_eval.value.tensor_val != null &&
            expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
            !index_eval.error.has_error &&
            index_eval.value != null;

        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        Value* ref_result = setup_ok ? tensor_ref_value(expr_eval.value, index_eval.value, interp) : null;
        usz active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = setup_ok &&
            ref_result != null &&
            ref_result.tag == DOUBLE &&
            ref_result.double_val == 5.0 &&
            active_tensor_dtors_after == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: lazy tensor ref cleans temporary realized tensor");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: lazy tensor ref cleans temporary realized tensor (setup=%s result=%s before=%d after=%d)",
                setup_ok ? "yes" : "no",
                ref_result != null && ref_result.tag == DOUBLE ? (ZString)"double" : runtime_alloc_error_text(ref_result),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }
    }

    {
        EvalResult expr_eval = run("(map + (map + (Tensor Float64 [2] [1 2]) 1) 2)", interp);
        bool setup_ok = !expr_eval.error.has_error &&
            expr_eval.value != null &&
            expr_eval.value.tag == TENSOR &&
            expr_eval.value.tensor_val != null &&
            expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP;

        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        Value* realized = setup_ok ? tensor_realize_to_value(expr_eval.value, interp) : null;
        usz active_tensor_dtors_realized = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool value_ok = realized != null &&
            realized.tag == TENSOR &&
            realized.tensor_val != null &&
            realized.tensor_val.kind == TENSOR_PAYLOAD_CONCRETE &&
            realized.tensor_val.element_count == 2 &&
            realized.tensor_val.data != null;
        if (value_ok) {
            double* data = (double*)realized.tensor_val.data;
            value_ok = data[0] == 4.0 && data[1] == 5.0;
        }

        if (realized != null && realized != expr_eval.value && realized.tag != ERROR) {
            boundary_cleanup_materialized_value(null, realized, interp.current_scope);
        }
        usz active_tensor_dtors_after_cleanup = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = setup_ok &&
            value_ok &&
            active_tensor_dtors_realized == active_tensor_dtors_before + 1 &&
            active_tensor_dtors_after_cleanup == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested lazy tensor realize cleans child temporaries");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: nested lazy tensor realize cleans child temporaries (setup=%s realized=%s before=%d during=%d after=%d)",
                setup_ok ? "yes" : "no",
                realized != null && realized.tag == TENSOR ? (ZString)"tensor" : runtime_alloc_error_text(realized),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_realized,
                (long)active_tensor_dtors_after_cleanup);
            (*fail)++;
        }
    }

    {
        EvalResult expr_eval = run(
            "(contract (contract (Tensor Float64 [2 2] [1 2 3 4]) (Tensor Float64 [2 2] [1 0 0 1]) [1] [0]) (Tensor Float64 [2] [10 20]) [1] [0])",
            interp);
        bool setup_ok = !expr_eval.error.has_error &&
            expr_eval.value != null &&
            expr_eval.value.tag == TENSOR &&
            expr_eval.value.tensor_val != null &&
            expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_CONTRACT;

        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        Value* realized = setup_ok ? tensor_realize_to_value(expr_eval.value, interp) : null;
        usz active_tensor_dtors_realized = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool value_ok = realized != null &&
            realized.tag == TENSOR &&
            realized.tensor_val != null &&
            realized.tensor_val.kind == TENSOR_PAYLOAD_CONCRETE &&
            realized.tensor_val.element_count == 2 &&
            realized.tensor_val.data != null;
        if (value_ok) {
            double* data = (double*)realized.tensor_val.data;
            value_ok = data[0] == 50.0 && data[1] == 110.0;
        }

        if (realized != null && realized != expr_eval.value && realized.tag != ERROR) {
            boundary_cleanup_materialized_value(null, realized, interp.current_scope);
        }
        usz active_tensor_dtors_after_cleanup = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = setup_ok &&
            value_ok &&
            active_tensor_dtors_realized == active_tensor_dtors_before + 1 &&
            active_tensor_dtors_after_cleanup == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: nested lazy tensor contract realize cleans child temporaries");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: nested lazy tensor contract realize cleans child temporaries (setup=%s realized=%s before=%d during=%d after=%d)",
                setup_ok ? "yes" : "no",
                realized != null && realized.tag == TENSOR ? (ZString)"tensor" : runtime_alloc_error_text(realized),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_realized,
                (long)active_tensor_dtors_after_cleanup);
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* copied = null;
        BoundaryCopyFault copy_fault = BOUNDARY_COPY_FAULT_NONE;
        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(saved);
        usz active_tensor_dtors_after = active_tensor_dtors_before;

        int saved_force = g_tensor_force_expr_edge_copy_fail_nth;
        if (setup_ok) {
            interp.current_scope = child;
            EvalResult expr_eval = run("(map + (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [3 4]))", interp);
            setup_ok = !expr_eval.error.has_error &&
                expr_eval.value != null &&
                expr_eval.value.tag == TENSOR &&
                expr_eval.value.tensor_val != null &&
                expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP;

            interp.current_scope = saved;
            interp.releasing_scope = child;
            g_tensor_force_expr_edge_copy_fail_nth = 3;
            copied = setup_ok ? copy_to_parent_with_fault(expr_eval.value, interp, &copy_fault) : null;
            g_tensor_force_expr_edge_copy_fail_nth = saved_force;
            interp.releasing_scope = null;
            active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(saved);
        }
        g_tensor_force_expr_edge_copy_fail_nth = saved_force;
        interp.current_scope = saved;
        interp.releasing_scope = null;

        bool ok = setup_ok &&
            copied == null &&
            copy_fault == BOUNDARY_COPY_FAULT_NULL_RESULT &&
            active_tensor_dtors_after == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: lazy tensor edge copy rollback cleans partial child copies");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: lazy tensor edge copy rollback cleans partial child copies (setup=%s copied=%s fault=%s before=%d after=%d)",
                setup_ok ? "yes" : "no",
                copied == null ? (ZString)"null" : runtime_alloc_error_text(copied),
                (ZString)boundary_copy_fault_name(copy_fault),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* promoted = null;
        bool map_left_in_child = true;
        bool map_right_in_child = true;

        if (setup_ok) {
            interp.current_scope = child;
            EvalResult expr_eval = run("(map + (Tensor Float64 [2] [1 2]) (Tensor Float64 [2] [3 4]))", interp);
            setup_ok = !expr_eval.error.has_error &&
                expr_eval.value != null &&
                expr_eval.value.tag == TENSOR &&
                expr_eval.value.tensor_val != null &&
                expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP;
            promoted = setup_ok ? promote_to_root(expr_eval.value, interp) : null;
            if (promoted != null &&
                promoted.tag == TENSOR &&
                promoted.tensor_val != null &&
                promoted.tensor_val.kind == TENSOR_PAYLOAD_MAP) {
                map_left_in_child = boundary_ptr_in_scope((void*)promoted.tensor_val.map_left, child);
                map_right_in_child = boundary_ptr_in_scope((void*)promoted.tensor_val.map_right, child);
            }
            interp.current_scope = saved;
            interp.releasing_scope = null;
        }

        bool ok = setup_ok &&
            promoted != null &&
            promoted.tag == TENSOR &&
            promoted.scope_gen == saved.escape_generation &&
            promoted.tensor_val != null &&
            promoted.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
            !map_left_in_child &&
            !map_right_in_child;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: lazy tensor root promotion keeps expression edges out of child scope");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: lazy tensor root promotion keeps expression edges out of child scope (setup=%s promoted=%s left_child=%s right_child=%s)",
                setup_ok ? "yes" : "no",
                promoted != null && promoted.tag == TENSOR ? (ZString)"tensor" : runtime_alloc_error_text(promoted),
                map_left_in_child ? "yes" : "no",
                map_right_in_child ? "yes" : "no");
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* promoted = null;
        bool map_func_value_in_child = true;
        bool map_func_env_in_child = true;

        if (setup_ok) {
            interp.current_scope = child;
            EvalResult expr_eval = run("(let (bias 10) (map (lambda (x) (+ x bias)) (Tensor Float64 [2] [1 2])))", interp);
            setup_ok = !expr_eval.error.has_error &&
                expr_eval.value != null &&
                expr_eval.value.tag == TENSOR &&
                expr_eval.value.tensor_val != null &&
                expr_eval.value.tensor_val.kind == TENSOR_PAYLOAD_MAP;
            promoted = setup_ok ? promote_to_root(expr_eval.value, interp) : null;
            if (promoted != null &&
                promoted.tag == TENSOR &&
                promoted.tensor_val != null &&
                promoted.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
                promoted.tensor_val.map_func != null &&
                promoted.tensor_val.map_func.tag == CLOSURE &&
                promoted.tensor_val.map_func.closure_val != null) {
                map_func_value_in_child = boundary_ptr_in_scope((void*)promoted.tensor_val.map_func, child);
                map_func_env_in_child = env_chain_has_frame_in_scope(
                    promoted.tensor_val.map_func.closure_val.env,
                    child,
                    interp.global_env
                );
            }
            interp.current_scope = saved;
            interp.releasing_scope = null;
        }

        bool ok = setup_ok &&
            promoted != null &&
            promoted.tag == TENSOR &&
            promoted.scope_gen == saved.escape_generation &&
            promoted.tensor_val != null &&
            promoted.tensor_val.kind == TENSOR_PAYLOAD_MAP &&
            !map_func_value_in_child &&
            !map_func_env_in_child;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: lazy tensor closure map root promotion detaches callable env");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: lazy tensor closure map root promotion detaches callable env (setup=%s promoted=%s func_child=%s env_child=%s)",
                setup_ok ? "yes" : "no",
                promoted != null && promoted.tag == TENSOR ? (ZString)"tensor" : runtime_alloc_error_text(promoted),
                map_func_value_in_child ? "yes" : "no",
                map_func_env_in_child ? "yes" : "no");
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        EvalResult expr_eval = run("(realize (map (lambda (x) 'bad) (Tensor Float64 [1] [1])))", interp);
        usz active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = expr_eval.error.has_error &&
            str_starts_with(expr_eval.error.message[:256], "map: function result dtype mismatch") &&
            active_tensor_dtors_after == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: failed lazy tensor realize cleans fresh result tensor");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: failed lazy tensor realize cleans fresh result tensor (error=%s msg=%s before=%d after=%d)",
                expr_eval.error.has_error ? "yes" : "no",
                expr_eval.error.has_error ? (ZString)expr_eval.error.message[:256] : runtime_alloc_error_text(expr_eval.value),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }
    }

    {
        usz active_tensor_dtors_before = runtime_alloc_active_tensor_dtor_count(interp.current_scope);
        EvalResult expr_eval = run("(Tensor Float64 [2] [1 'bad])", interp);
        usz active_tensor_dtors_after = runtime_alloc_active_tensor_dtor_count(interp.current_scope);

        bool ok = expr_eval.error.has_error &&
            str_starts_with(expr_eval.error.message[:256], "Tensor: data elements must be real numbers representable as Float64") &&
            active_tensor_dtors_after == active_tensor_dtors_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: failed tensor constructor cleans partial tensor result");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: failed tensor constructor cleans partial tensor result (error=%s msg=%s before=%d after=%d)",
                expr_eval.error.has_error ? "yes" : "no",
                expr_eval.error.has_error ? (ZString)expr_eval.error.message[:256] : runtime_alloc_error_text(expr_eval.value),
                (long)active_tensor_dtors_before,
                (long)active_tensor_dtors_after);
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* foreign = main::scope_create(saved);
        int* opaque_payload = null;
        Value* opaque = null;
        Primitive* opaque_prim = null;
        Value* list = null;
        Value* iterator = null;
        Value* array_value = null;

        bool setup_ok = foreign != null;
        if (setup_ok) {
            interp.current_scope = foreign;
            opaque_payload = (int*)interp.current_scope.alloc(int.sizeof);
            opaque = interp.alloc_value();
            if (opaque != null) {
                opaque.tag = PRIMITIVE;
                main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
            }
            setup_ok = opaque_payload != null && opaque != null;
        }

        if (setup_ok) {
            opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
            setup_ok = opaque_prim != null;
        }

        if (setup_ok) {
            *opaque_payload = 701;
            *opaque_prim = {};
            opaque_prim.name[0] = 'o';
            opaque_prim.name[1] = 0;
            opaque_prim.func = &prim_add;
            opaque_prim.arity = 2;
            opaque_prim.tag = 701;
            opaque_prim.user_data = (void*)opaque_payload;
            opaque.tag = PRIMITIVE;
            opaque.prim_val = opaque_prim;

            list = make_cons(interp, opaque, make_nil(interp));
            iterator = iterator_from_list_value(list, interp);
            interp.current_scope = saved;
            array_value = make_array(interp, 1);
            setup_ok = list != null && list.tag == CONS &&
                iterator != null && iterator.tag == ITERATOR &&
                array_value != null && array_value.tag == ARRAY &&
                array_value.array_val != null;
        }
        interp.current_scope = saved;

        if (setup_ok) {
            array_value.array_val.items[0] = make_int(interp, 1);
            array_value.array_val.length = 1;
        }

        Value*[1] prim_array_args = { opaque };
        Value*[1] list_to_array_args = { list };
        Value*[3] array_set_args = { array_value, make_int(interp, 0), opaque };
        Value*[1] to_array_args = { iterator };

        if (setup_ok) interp.current_scope = foreign;
        Value* prim_array_boundary_fail = setup_ok ? prim_array(prim_array_args[0:1], null, interp) : raise_error(interp, "setup failed");
        Value* list_to_array_boundary_fail = setup_ok ? prim_list_to_array(list_to_array_args[0:1], null, interp) : raise_error(interp, "setup failed");
        Value* array_set_boundary_fail = setup_ok ? prim_array_set(array_set_args[0:3], null, interp) : raise_error(interp, "setup failed");
        Value* to_array_boundary_fail = setup_ok ? prim_to_array(to_array_args[0:1], null, interp) : raise_error(interp, "setup failed");
        interp.current_scope = saved;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(prim_array_boundary_fail, "root-store: refusing primitive clone with opaque user_data") &&
            runtime_alloc_error_matches(list_to_array_boundary_fail, "root-store: refusing primitive clone with opaque user_data") &&
            runtime_alloc_error_matches(array_set_boundary_fail, "root-store: refusing primitive clone with opaque user_data") &&
            runtime_alloc_error_matches(to_array_boundary_fail, "root-store: refusing primitive clone with opaque user_data") &&
            array_value.array_val.length == 1 &&
            array_value.array_val.items[0] != null &&
            array_value.array_val.items[0].tag == INT &&
            array_value.array_val.items[0].int_val == 1;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: array write paths fail closed on boundary promotion faults");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: array write paths fail closed on boundary promotion faults (setup_ok=%s ctor=%s list=%s set=%s iter=%s array_intact=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(prim_array_boundary_fail),
                runtime_alloc_error_text(list_to_array_boundary_fail),
                runtime_alloc_error_text(array_set_boundary_fail),
                runtime_alloc_error_text(to_array_boundary_fail),
                setup_ok &&
                    array_value != null &&
                    array_value.tag == ARRAY &&
                    array_value.array_val != null &&
                    array_value.array_val.length == 1 &&
                    array_value.array_val.items[0] != null &&
                    array_value.array_val.items[0].tag == INT &&
                    array_value.array_val.items[0].int_val == 1 ? "yes" : "no");
            (*fail)++;
        }
        if (foreign != null) main::scope_release(foreign);
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* foreign = main::scope_create(saved);
        SymbolId type_sym = interp.symbols.intern("__JitFieldBoundaryBox");
        SymbolId field_sym = interp.symbols.intern("slot");
        TypeFieldInfo* type_fields = (TypeFieldInfo*)mem::malloc(TypeFieldInfo.sizeof);
        bool setup_ok = foreign != null &&
            type_sym != INVALID_SYMBOL_ID &&
            field_sym != INVALID_SYMBOL_ID &&
            type_fields != null;

        TypeId type_id = INVALID_TYPE_ID;
        if (setup_ok) {
            type_fields[0] = {
                .name = field_sym,
                .field_type = INVALID_TYPE_ID,
                .annotation_sym = INVALID_SYMBOL_ID,
            };
            TypeInfo info = {
                .name = type_sym,
                .kind = TK_CONCRETE,
                .parent = INVALID_TYPE_ID,
                .fields = type_fields,
                .field_count = 1,
                .type_params = null,
                .type_param_count = 0,
                .constraints = null,
                .constraint_count = 0,
                .variants = null,
                .variant_count = 0,
                .alias_target = INVALID_TYPE_ID,
            };
            type_id = interp.types.register_type(info, &interp.symbols);
            setup_ok = type_id != INVALID_TYPE_ID;
            if (!setup_ok && type_fields != null) {
                mem::free(type_fields);
                type_fields = null;
            }
        }

        Value* initial = setup_ok ? make_int(interp, 1) : null;
        Value*[1] fields = { initial };
        Value* inst = setup_ok ? make_instance(interp, type_id, &fields[0], 1) : null;
        setup_ok = setup_ok &&
            inst != null &&
            inst.tag == INSTANCE &&
            inst.instance_val != null &&
            inst.instance_val.fields != null;

        int* opaque_payload = null;
        Value* opaque = null;
        Primitive* opaque_prim = null;
        if (setup_ok) {
            interp.current_scope = foreign;
            opaque_payload = (int*)interp.current_scope.alloc(int.sizeof);
            opaque = interp.alloc_value();
            if (opaque != null) {
                opaque.tag = PRIMITIVE;
                main::scope_register_dtor(foreign, (void*)opaque, &scope_dtor_value);
            }
            setup_ok = opaque_payload != null && opaque != null;
        }

        if (setup_ok) {
            opaque_prim = (Primitive*)mem::malloc(Primitive.sizeof);
            setup_ok = opaque_prim != null;
        }

        if (setup_ok) {
            *opaque_payload = 719;
            *opaque_prim = {};
            opaque_prim.name[0] = 'f';
            opaque_prim.name[1] = 0;
            opaque_prim.func = &prim_add;
            opaque_prim.arity = 2;
            opaque_prim.tag = 719;
            opaque_prim.user_data = (void*)opaque_payload;
            opaque.prim_val = opaque_prim;
        }

        Value* set_fail = null;
        if (setup_ok) {
            set_fail = jit_set_path_mutate_instance(inst, field_sym, opaque, interp);
        } else {
            set_fail = raise_error(interp, "setup failed");
        }
        interp.current_scope = saved;

        bool field_intact = inst != null &&
            inst.tag == INSTANCE &&
            inst.instance_val != null &&
            inst.instance_val.fields != null &&
            inst.instance_val.fields[0].tag == INT &&
            inst.instance_val.fields[0].int_val == 1;
        bool ok = setup_ok &&
            runtime_alloc_error_matches(set_fail, "boundary: refusing primitive copy with opaque user_data") &&
            field_intact;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: JIT instance field write fails closed on boundary promotion faults");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: JIT instance field write fails closed on boundary promotion faults (setup_ok=%s set=%s field_intact=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(set_fail),
                field_intact ? "yes" : "no");
            (*fail)++;
        }

        if (foreign != null) main::scope_release(foreign);
    }

    {
        SymbolId export_sym = interp.symbols.intern("__module_growth_value");
        SymbolId first_name = interp.symbols.intern("__module_growth_first");
        Module* first = first_name != INVALID_SYMBOL_ID
            ? jit_create_file_module(first_name, "/tmp/omni-module-growth-first.omni", interp)
            : null;
        bool setup_ok = export_sym != INVALID_SYMBOL_ID &&
            first != null &&
            first.env != null &&
            module_append_export(first, export_sym) &&
            env_define_with_barrier_result(first.env, export_sym, make_int(interp, 42), interp) == null;
        if (setup_ok) first.loaded = true;

        Module* table_before = interp.modules;
        usz initial_capacity = interp.module_capacity;
        Value* mod_val = setup_ok ? make_module(interp, first) : null;
        setup_ok = setup_ok &&
            mod_val != null &&
            mod_val.tag == MODULE &&
            mod_val.module_val != null;

        usz target_count = initial_capacity + 2;
        for (usz i = 0; setup_ok && interp.module_count < target_count; i++) {
            char[80] name_buf;
            if (try char[] name = io::bprintf(
                &name_buf,
                "__module_growth_extra_%d_%d",
                (int)initial_capacity,
                (int)i
            )) {
                SymbolId name_sym = interp.symbols.intern(name);
                Module* extra = name_sym != INVALID_SYMBOL_ID
                    ? jit_create_file_module(name_sym, "/tmp/omni-module-growth-extra.omni", interp)
                    : null;
                setup_ok = extra != null;
                if (setup_ok) extra.loaded = true;
            } else {
                setup_ok = false;
            }
        }

        bool grew = table_before != null &&
            interp.modules != null &&
            interp.modules != table_before &&
            interp.module_capacity > initial_capacity;
        EvalResult step = setup_ok ? eval_path_step(mod_val, export_sym, interp) : eval_error("setup failed");
        bool ok = setup_ok &&
            grew &&
            !step.error.has_error &&
            step.value != null &&
            step.value.tag == INT &&
            step.value.int_val == 42;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: module values survive module table growth");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: module values survive module table growth (setup_ok=%s grew=%s value_ok=%s)",
                setup_ok ? "yes" : "no",
                grew ? "yes" : "no",
                step.value != null && step.value.tag == INT && step.value.int_val == 42 ? "yes" : "no");
            (*fail)++;
        }
    }

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
        Value* push_fail = setup_ok ? prim_array_push(push_args[0:2], null, interp) : raise_error(interp, "setup failed");
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
            opaque_value_payload = (int*)mem::malloc(int.sizeof);
            opaque_key_payload = (int*)mem::malloc(int.sizeof);
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
            opaque_payload = (int*)mem::malloc(int.sizeof);
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
        Value* dict_set_fail = setup_ok ? prim_dict_set(dict_set_args[0:3], null, interp) : raise_error(interp, "setup failed");
        Value* set_add_fail = setup_ok ? prim_set_add(set_add_args[0:2], null, interp) : raise_error(interp, "setup failed");
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
        Value*[1] json_obj_args = { make_string(interp, "{\"a\":1}") };
        Value*[1] json_nested_args = { make_string(interp, "{\"a\":[1]}") };
        Value*[1] toml_table_args = { make_string(interp, "a = 1\n") };
        Value*[1] toml_nested_args = { make_string(interp, "a = [1]\n") };
        Value*[1] csv_args = { make_string(interp, "") };

        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        bool saved_array_items_fail = g_value_force_make_array_items_alloc_fail;

        g_hashmap_force_struct_alloc_fail = true;
        Value* json_obj_fail = prim_json_parse(json_obj_args[0:1], null, interp);
        Value* toml_table_fail = prim_toml_parse(toml_table_args[0:1], null, interp);
        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        g_value_force_make_array_items_alloc_fail = true;
        Value* json_nested_fail = prim_json_parse(json_nested_args[0:1], null, interp);
        Value* toml_nested_fail = prim_toml_parse(toml_nested_args[0:1], null, interp);
        Value* csv_fail = prim_csv_parse(csv_args[0:1], null, interp);
        g_value_force_make_array_items_alloc_fail = saved_array_items_fail;

        bool ok =
            runtime_alloc_error_matches(json_obj_fail, "json-parse: out of memory") &&
            runtime_alloc_error_matches(json_nested_fail, "json-parse: out of memory") &&
            runtime_alloc_error_matches(toml_table_fail, "toml-parse: out of memory") &&
            runtime_alloc_error_matches(toml_nested_fail, "toml-parse: out of memory") &&
            runtime_alloc_error_matches(csv_fail, "csv-parse: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: data-format constructor alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: data-format constructor alloc failures fail closed (json_obj=%s json_nested=%s toml_table=%s toml_nested=%s csv=%s)",
                runtime_alloc_error_text(json_obj_fail),
                runtime_alloc_error_text(json_nested_fail),
                runtime_alloc_error_text(toml_table_fail),
                runtime_alloc_error_text(toml_nested_fail),
                runtime_alloc_error_text(csv_fail));
            (*fail)++;
        }
    }

    {
        Interp* tmp = (Interp*)mem::malloc(Interp.sizeof);
        bool setup_ok = tmp != null;
        main::ScopeRegion* saved = null;
        main::ScopeRegion* foreign = null;
        Value* toml_fail = null;
        if (setup_ok) {
            tmp.init();
            register_primitives(tmp);
            saved = tmp.current_scope;
            foreign = main::scope_create(saved);
            setup_ok = foreign != null;
        }

        if (setup_ok) {
            char[] toml_src = "a = [1979-05-27]\n";
            char[256] toml_err = {};
            OmniTomlParse* toml_handle =
                omni_toml_parse_with_options(
                    toml_src.ptr,
                    toml_src.len,
                    &toml_err,
                    toml_err.len,
                    false
                );
            bool saved_force = g_boundary_force_copy_time_point_wrapper_alloc_fail;
            g_boundary_force_copy_time_point_wrapper_alloc_fail = true;
            tmp.current_scope = foreign;
            if (toml_handle != null) {
                toml_fail = toml_datum_to_omni(omni_toml_result_root(toml_handle), tmp);
            }
            tmp.current_scope = saved;
            g_boundary_force_copy_time_point_wrapper_alloc_fail = saved_force;
            if (toml_handle != null) omni_toml_result_free(toml_handle);
            if (toml_fail == null) toml_fail = raise_error(tmp, "toml setup failed");
        }

        bool ok = setup_ok &&
            runtime_alloc_error_matches(toml_fail, "boundary: failed to allocate time-point copy value");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: toml array element promotion faults fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: toml array element promotion faults fail closed (setup_ok=%s toml=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(toml_fail));
            (*fail)++;
        }

        if (tmp != null) {
            if (foreign != null) main::scope_release(foreign);
            tmp.destroy();
            mem::free(tmp);
        }
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* pair = null;
        if (setup_ok) {
            interp.current_scope = child;
            Value* temp_string = make_string(interp, "x");
            Value* escape_nil = interp.alloc_value_escape();
            if (temp_string == null || temp_string.tag == ERROR || escape_nil == null) {
                setup_ok = false;
            } else {
                escape_nil.tag = NIL;
                bool saved_force = g_boundary_force_escape_string_or_error_promotion_alloc_fail;
                g_boundary_force_escape_string_or_error_promotion_alloc_fail = true;
                pair = make_cons(interp, temp_string, escape_nil);
                g_boundary_force_escape_string_or_error_promotion_alloc_fail = saved_force;
            }
            interp.current_scope = saved;
        }

        bool ok = setup_ok &&
            runtime_alloc_error_matches(pair, "boundary: failed to allocate string/error escape copy");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: make_cons escape promotion faults fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: make_cons escape promotion faults fail closed (setup_ok=%s pair=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(pair));
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* pair = null;
        if (setup_ok) {
            interp.current_scope = child;
            Value* escape_car = interp.alloc_value_escape();
            Value* escape_nil = interp.alloc_value_escape();
            if (escape_car == null || escape_nil == null) {
                setup_ok = false;
            } else {
                escape_car.tag = INT;
                escape_car.int_val = 7;
                escape_nil.tag = NIL;
                int saved_force = g_boundary_force_alloc_escape_value_fail_nth;
                g_boundary_force_alloc_escape_value_fail_nth = 1;
                pair = make_cons(interp, escape_car, escape_nil);
                g_boundary_force_alloc_escape_value_fail_nth = saved_force;
            }
            interp.current_scope = saved;
        }

        bool ok = setup_ok &&
            runtime_alloc_error_matches(pair, "cons: failed to allocate escape pair");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: make_cons escape wrapper alloc faults fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: make_cons escape wrapper alloc faults fail closed (setup_ok=%s pair=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(pair));
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
    }

    {
        int saved_force = g_value_force_closure_scope_alloc_fail_nth;
        g_value_force_closure_scope_alloc_fail_nth = 1;
        Value* closure_fail = make_closure_no_param(interp, null, interp.global_env);
        g_value_force_closure_scope_alloc_fail_nth = saved_force;

        bool ok = runtime_alloc_error_matches(closure_fail, "closure: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: closure payload alloc faults fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: closure payload alloc faults fail closed (msg=%s)",
                runtime_alloc_error_text(closure_fail));
            (*fail)++;
        }
    }

    {
        int saved_force = g_value_force_closure_scope_alloc_fail_nth;
        g_value_force_closure_scope_alloc_fail_nth = 2;
        Value* closure_fail = make_closure(interp, interp.symbols.intern("__closure_param_oom"), null, interp.global_env);
        g_value_force_closure_scope_alloc_fail_nth = saved_force;

        bool ok = runtime_alloc_error_matches(closure_fail, "closure: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: closure param alloc faults fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: closure param alloc faults fail closed (msg=%s)",
                runtime_alloc_error_text(closure_fail));
            (*fail)++;
        }
    }

    {
        bool saved_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;

        Value*[] no_args = {};
        Value* dispatch_fail =
            explain_dispatch_result_from_evaluated_call(interp, make_nil(interp), make_nil(interp), no_args);
        Value* effect_fail =
            explain_effect_result_from_compiled_signal(
                interp,
                "(perform sample 1)",
                interp.symbols.intern("sample"),
                make_int(interp, 1));
        Value* helper_fail = explain_method_source(interp, make_nil(interp));

        g_hashmap_force_struct_alloc_fail = saved_struct_fail;

        bool ok =
            runtime_alloc_error_matches(dispatch_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(effect_fail, "schema explain: out of memory") &&
            runtime_alloc_error_matches(helper_fail, "schema explain: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: schema explain constructor alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: schema explain constructor alloc failures fail closed (dispatch=%s effect=%s helper=%s)",
                runtime_alloc_error_text(dispatch_fail),
                runtime_alloc_error_text(effect_fail),
                runtime_alloc_error_text(helper_fail));
            (*fail)++;
        }
    }

    {
        bool saved_partial_force = g_iterator_force_partial_alloc_fail;
        g_iterator_force_partial_alloc_fail = true;
        Value* iterator_fail = iterator_from_list_value(make_nil(interp), interp);
        g_iterator_force_partial_alloc_fail = saved_partial_force;

        bool saved_wrapper_force = g_value_force_make_iterator_alloc_fail;
        g_value_force_make_iterator_alloc_fail = true;
        Value* wrapper_fail = make_iterator(interp, make_nil(interp));
        g_value_force_make_iterator_alloc_fail = saved_wrapper_force;

        bool ok = runtime_alloc_error_matches(iterator_fail, "iterator: failed to allocate thunk") &&
                  runtime_alloc_error_matches(wrapper_fail, "iterator: failed to allocate iterator value");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: iterator helper alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: iterator helper alloc failures fail closed (thunk_ok=%s wrapper_ok=%s)",
                runtime_alloc_error_matches(iterator_fail, "iterator: failed to allocate thunk") ? "yes" : "no",
                runtime_alloc_error_matches(wrapper_fail, "iterator: failed to allocate iterator value") ? "yes" : "no");
            (*fail)++;
        }
    }

    {
        Value* list = make_cons(interp, make_int(interp, 1), make_cons(interp, make_int(interp, 2), make_nil(interp)));
        Value* base_iterator = iterator_from_list_value(list, interp);
        Value*[1] repeat_args = { make_int(interp, 7) };
        Value*[2] take_args = { make_int(interp, 2), base_iterator };

        bool saved_force = g_iterator_force_partial_alloc_fail;
        g_iterator_force_partial_alloc_fail = true;
        Value* repeat_fail = prim_iterator_repeat_thunk(repeat_args[0:1], null, interp);
        Value* take_fail = prim_iterator_take_thunk(take_args[0:2], null, interp);
        g_iterator_force_partial_alloc_fail = saved_force;

        bool ok = runtime_alloc_error_matches(repeat_fail, "iterator: failed to allocate thunk") &&
                  runtime_alloc_error_matches(take_fail, "iterator: failed to allocate thunk");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: iterator tail constructor errors propagate");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: iterator tail constructor errors propagate (repeat=%s take=%s repeat_msg=%s take_msg=%s)",
                runtime_alloc_error_matches(repeat_fail, "iterator: failed to allocate thunk") ? "yes" : "no",
                runtime_alloc_error_matches(take_fail, "iterator: failed to allocate thunk") ? "yes" : "no",
                runtime_alloc_error_text(repeat_fail),
                runtime_alloc_error_text(take_fail));
            (*fail)++;
        }
    }

    {
        Value* left_list = make_cons(interp, make_int(interp, 1), make_nil(interp));
        Value* right_list = make_cons(interp, make_int(interp, 2), make_nil(interp));
        Value* left_iterator = iterator_from_list_value(left_list, interp);
        Value* right_iterator = iterator_from_list_value(right_list, interp);
        Value*[2] zip_args = { left_iterator, right_iterator };

        int saved_force = g_iterator_force_cons_alloc_fail_nth;
        g_iterator_force_cons_alloc_fail_nth = 1;
        Value* zip_fail = prim_iterator_zip_thunk(zip_args[0:2], null, interp);
        g_iterator_force_cons_alloc_fail_nth = saved_force;

        bool setup_ok = left_iterator != null && left_iterator.tag == ITERATOR &&
            right_iterator != null && right_iterator.tag == ITERATOR;
        bool ok = setup_ok &&
            runtime_alloc_error_matches(zip_fail, "__iterator-zip: failed to allocate item pair");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: iterator zip item pair allocation failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: iterator zip item pair allocation failures fail closed (setup_ok=%s msg=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(zip_fail));
            (*fail)++;
        }
    }

    {
        Value*[1] no_args = { null };
        Value*[1] one_arg = { make_int(interp, 1) };
        Value*[2] filter_bad_args = { make_int(interp, 1), make_nil(interp) };
        Value* map_missing_state = prim_iterator_map_thunk(no_args[0:0], null, interp);
        Value* filter_missing_state = prim_iterator_filter_thunk(no_args[0:0], null, interp);
        Value* take_missing_state = prim_iterator_take_thunk(no_args[0:0], null, interp);
        Value* zip_missing_state = prim_iterator_zip_thunk(no_args[0:0], null, interp);
        Value* list_missing_state = prim_iterator_from_list_thunk(no_args[0:0], null, interp);
        Value* array_missing_state = prim_iterator_from_array_thunk(one_arg[0:1], null, interp);
        Value* filter_bad_iter_state = prim_iterator_filter_thunk(filter_bad_args[0:2], null, interp);

        bool ok =
            runtime_alloc_error_matches(map_missing_state, "__iterator-map: expected iterator state") &&
            runtime_alloc_error_matches(filter_missing_state, "__iterator-filter: expected iterator state") &&
            runtime_alloc_error_matches(take_missing_state, "__iterator-take: expected iterator state") &&
            runtime_alloc_error_matches(zip_missing_state, "__iterator-zip: expected iterators") &&
            runtime_alloc_error_matches(list_missing_state, "__iterator-from-list: expected list state") &&
            runtime_alloc_error_matches(array_missing_state, "__iterator-from-array: expected array state") &&
            runtime_alloc_error_matches(filter_bad_iter_state, "__iterator-filter: expected iterator state");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: malformed iterator thunk state fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: malformed iterator thunk state fails closed (map=%s filter=%s take=%s zip=%s list=%s array=%s bad_filter=%s)",
                runtime_alloc_error_text(map_missing_state),
                runtime_alloc_error_text(filter_missing_state),
                runtime_alloc_error_text(take_missing_state),
                runtime_alloc_error_text(zip_missing_state),
                runtime_alloc_error_text(list_missing_state),
                runtime_alloc_error_text(array_missing_state),
                runtime_alloc_error_text(filter_bad_iter_state)
            );
            (*fail)++;
        }
    }

    {
        Value*[1] no_args = { null };
        Value*[1] list_null_args = { null };
        Value*[1] range_bad_args = { make_nil(interp) };
        Value*[2] cycle_bad_args = { make_int(interp, 1), make_nil(interp) };
        Value* range_missing_state = prim_iterator_range_from_thunk(no_args[0:0], null, interp);
        Value* list_null_state = prim_iterator_from_list_thunk(list_null_args[0:1], null, interp);
        Value* range_bad_state = prim_iterator_range_from_thunk(range_bad_args[0:1], null, interp);
        Value* repeat_missing_state = prim_iterator_repeat_thunk(no_args[0:0], null, interp);
        Value* cycle_missing_state = prim_iterator_cycle_thunk(no_args[0:0], null, interp);
        Value* cycle_bad_state = prim_iterator_cycle_thunk(cycle_bad_args[0:2], null, interp);

        bool ok =
            runtime_alloc_error_matches(range_missing_state, "__iterator-range-from: expected integer state") &&
            runtime_alloc_error_matches(list_null_state, "__iterator-from-list: expected list state") &&
            runtime_alloc_error_matches(range_bad_state, "__iterator-range-from: expected integer state") &&
            runtime_alloc_error_matches(repeat_missing_state, "__iterator-repeat: expected item state") &&
            runtime_alloc_error_matches(cycle_missing_state, "__iterator-cycle: expected iterator state") &&
            runtime_alloc_error_matches(cycle_bad_state, "__iterator-cycle: expected iterator state");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: malformed iterator source state fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: malformed iterator source state fails closed (range_missing=%s list_null=%s range_bad=%s repeat=%s cycle_missing=%s cycle_bad=%s)",
                runtime_alloc_error_text(range_missing_state),
                runtime_alloc_error_text(list_null_state),
                runtime_alloc_error_text(range_bad_state),
                runtime_alloc_error_text(repeat_missing_state),
                runtime_alloc_error_text(cycle_missing_state),
                runtime_alloc_error_text(cycle_bad_state)
            );
            (*fail)++;
        }
    }

    {
        Value* null_next = make_primitive(interp, "__test-null-iterator-next", &runtime_alloc_null_iterator_next, 1);
        Value* null_iter = null_next != null && null_next.tag != ERROR ? make_iterator(interp, null_next) : null_next;
        Value*[3] fold_args = {
            make_primitive(interp, "+", &prim_add, 2),
            make_int(interp, 0),
            null_iter
        };
        Value* fold_null_pair = prim_iterator_foldl(fold_args[0:3], null, interp);

        bool ok = runtime_alloc_error_matches(fold_null_pair, "__iterator-foldl: expected iterator pair");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: malformed iterator fold state fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: malformed iterator fold state fails closed (msg=%s)",
                runtime_alloc_error_text(fold_null_pair)
            );
            (*fail)++;
        }
    }

    {
        EvalResult func_run = run("(lambda (a x) (+ a x))", interp);
        Value* list = make_cons(interp, make_int(interp, 1), make_nil(interp));
        Value* iterator = iterator_from_list_value(list, interp);
        Value*[3] fold_args = {
            func_run.error.has_error ? null : func_run.value,
            make_int(interp, 0),
            iterator
        };

        int saved_force = g_iterator_force_cons_alloc_fail_nth;
        g_iterator_force_cons_alloc_fail_nth = 1;
        Value* fold_fail =
            !func_run.error.has_error && iterator != null && iterator.tag == ITERATOR
            ? prim_iterator_foldl(fold_args[0:3], null, interp)
            : raise_error(interp, "setup failed");
        g_iterator_force_cons_alloc_fail_nth = saved_force;

        bool setup_ok = !func_run.error.has_error && iterator != null && iterator.tag == ITERATOR;
        bool ok = setup_ok &&
            runtime_alloc_error_matches(fold_fail, "__iterator-foldl: failed to allocate call args");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: iterator foldl arg list allocation failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: iterator foldl arg list allocation failures fail closed (setup_ok=%s msg=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(fold_fail));
            (*fail)++;
        }
    }

    {
        bool saved_force = g_string_force_result_wrapper_alloc_fail;
        g_string_force_result_wrapper_alloc_fail = true;
        StringVal* builder = strval_new_from_slice("hello");
        Value* wrapper_fail = strval_materialize_result(builder, interp, "string helper: failed to allocate result value");
        g_string_force_result_wrapper_alloc_fail = saved_force;

        bool ok = runtime_alloc_error_matches(wrapper_fail, "string helper: failed to allocate result value");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: string helper wrapper alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: string helper wrapper alloc failures fail closed (ok=%s msg=%s)",
                runtime_alloc_error_matches(wrapper_fail, "string helper: failed to allocate result value") ? "yes" : "no",
                runtime_alloc_error_text(wrapper_fail));
            (*fail)++;
        }
    }

    {
        Value*[3] replace_args = {
            make_string(interp, "aaaa"),
            make_string(interp, "a"),
            make_string(interp, "0123456789")
        };
        Value*[2] repeat_args = {
            make_string(interp, "ab"),
            make_int(interp, 3)
        };
        Value*[2] format_args = {
            make_string(interp, "%s"),
            make_string(interp, "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
        };
        Value*[2] cl_format_args = {
            make_string(interp, "~a"),
            make_string(interp, "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz")
        };

        int saved_alloc_nth = g_string_force_builder_alloc_fail_nth;
        int saved_grow_nth = g_string_force_builder_grow_fail_nth;

        g_string_force_builder_alloc_fail_nth = 1;
        Value* replace_alloc_fail = prim_string_replace(replace_args[0:3], null, interp);
        g_string_force_builder_alloc_fail_nth = 1;
        Value* repeat_alloc_fail = prim_string_repeat(repeat_args[0:2], null, interp);
        g_string_force_builder_alloc_fail_nth = 1;
        Value* format_alloc_fail = prim_format(format_args[0:2], null, interp);
        g_string_force_builder_alloc_fail_nth = 1;
        Value* cl_format_alloc_fail = prim_cl_format(cl_format_args[0:2], null, interp);

        g_string_force_builder_alloc_fail_nth = saved_alloc_nth;
        g_string_force_builder_grow_fail_nth = 1;
        Value* replace_grow_fail = prim_string_replace(replace_args[0:3], null, interp);
        g_string_force_builder_grow_fail_nth = 1;
        Value* format_grow_fail = prim_format(format_args[0:2], null, interp);
        g_string_force_builder_grow_fail_nth = 1;
        Value* cl_format_grow_fail = prim_cl_format(cl_format_args[0:2], null, interp);
        g_string_force_builder_grow_fail_nth = saved_grow_nth;

        bool ok =
            runtime_alloc_error_matches(replace_alloc_fail, "string-replace: failed to allocate builder") &&
            runtime_alloc_error_matches(repeat_alloc_fail, "string-repeat: failed to allocate builder") &&
            runtime_alloc_error_matches(format_alloc_fail, "format: failed to allocate builder") &&
            runtime_alloc_error_matches(cl_format_alloc_fail, "cl-format: failed to allocate builder") &&
            runtime_alloc_error_matches(replace_grow_fail, "string-replace: failed to grow builder") &&
            runtime_alloc_error_matches(format_grow_fail, "format: failed to grow temporary builder") &&
            runtime_alloc_error_matches(cl_format_grow_fail, "cl-format: failed to grow result buffer");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: string builder alloc and growth failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: string builder alloc and growth failures fail closed (replace_alloc=%s repeat_alloc=%s format_alloc=%s cl_alloc=%s replace_grow=%s format_grow=%s cl_grow=%s replace_alloc_msg=%s repeat_alloc_msg=%s format_alloc_msg=%s cl_alloc_msg=%s replace_grow_msg=%s format_grow_msg=%s cl_grow_msg=%s)",
                runtime_alloc_error_matches(replace_alloc_fail, "string-replace: failed to allocate builder") ? "yes" : "no",
                runtime_alloc_error_matches(repeat_alloc_fail, "string-repeat: failed to allocate builder") ? "yes" : "no",
                runtime_alloc_error_matches(format_alloc_fail, "format: failed to allocate builder") ? "yes" : "no",
                runtime_alloc_error_matches(cl_format_alloc_fail, "cl-format: failed to allocate builder") ? "yes" : "no",
                runtime_alloc_error_matches(replace_grow_fail, "string-replace: failed to grow builder") ? "yes" : "no",
                runtime_alloc_error_matches(format_grow_fail, "format: failed to grow temporary builder") ? "yes" : "no",
                runtime_alloc_error_matches(cl_format_grow_fail, "cl-format: failed to grow result buffer") ? "yes" : "no",
                runtime_alloc_error_text(replace_alloc_fail),
                runtime_alloc_error_text(repeat_alloc_fail),
                runtime_alloc_error_text(format_alloc_fail),
                runtime_alloc_error_text(cl_format_alloc_fail),
                runtime_alloc_error_text(replace_grow_fail),
                runtime_alloc_error_text(format_grow_fail),
                runtime_alloc_error_text(cl_format_grow_fail));
            (*fail)++;
        }
    }

    {
        Value*[1] upcase_args = { make_string(interp, "abc") };
        Value*[1] downcase_args = { make_string(interp, "ABC") };
        Value*[1] to_list_args = { make_string(interp, "abc") };
        Value*[2] split_args = { make_string(interp, "a,b"), make_string(interp, ",") };
        Value*[1] grapheme_args = { make_string(interp, "abc") };

        bool setup_ok =
            upcase_args[0] != null && upcase_args[0].tag == STRING &&
            downcase_args[0] != null && downcase_args[0].tag == STRING &&
            to_list_args[0] != null && to_list_args[0].tag == STRING &&
            split_args[0] != null && split_args[0].tag == STRING &&
            split_args[1] != null && split_args[1].tag == STRING &&
            grapheme_args[0] != null && grapheme_args[0].tag == STRING;

        bool saved_force = g_value_force_make_string_alloc_fail;
        g_value_force_make_string_alloc_fail = true;
        Value* upcase_fail = setup_ok ? prim_string_upcase(upcase_args[0:1], null, interp) : raise_error(interp, "setup failed");
        Value* downcase_fail = setup_ok ? prim_string_downcase(downcase_args[0:1], null, interp) : raise_error(interp, "setup failed");
        Value* to_list_fail = setup_ok ? prim_string_to_list(to_list_args[0:1], null, interp) : raise_error(interp, "setup failed");
        Value* split_fail = setup_ok ? prim_string_split(split_args[0:2], null, interp) : raise_error(interp, "setup failed");
        Value* grapheme_fail = setup_ok ? prim_string_graphemes(grapheme_args[0:1], null, interp) : raise_error(interp, "setup failed");
        g_value_force_make_string_alloc_fail = saved_force;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(upcase_fail, "out of memory while allocating string") &&
            runtime_alloc_error_matches(downcase_fail, "out of memory while allocating string") &&
            runtime_alloc_error_matches(to_list_fail, "out of memory while allocating string") &&
            runtime_alloc_error_matches(split_fail, "out of memory while allocating string") &&
            runtime_alloc_error_matches(grapheme_fail, "out of memory while allocating string");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: string list materializers fail closed on string wrapper alloc");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: string list materializers fail closed on string wrapper alloc (setup_ok=%s upcase=%s downcase=%s to_list=%s split=%s grapheme=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(upcase_fail),
                runtime_alloc_error_text(downcase_fail),
                runtime_alloc_error_text(to_list_fail),
                runtime_alloc_error_text(split_fail),
                runtime_alloc_error_text(grapheme_fail));
            (*fail)++;
        }
    }

    {
        Value*[2] shell_args = { make_string(interp, "printf ok"), make_symbol(interp, interp.sym_true) };
        Value*[1] readdir_args = { make_string(interp, "/") };
        Value*[2] schema_explain_args = { make_symbol(interp, interp.symbols.intern("int")), make_string(interp, "nope") };

        bool setup_ok =
            shell_args[0] != null && shell_args[0].tag == STRING &&
            shell_args[1] != null &&
            readdir_args[0] != null && readdir_args[0].tag == STRING &&
            schema_explain_args[0] != null &&
            schema_explain_args[1] != null && schema_explain_args[1].tag == STRING;

        bool saved_force = g_value_force_make_string_alloc_fail;
        g_value_force_make_string_alloc_fail = true;
        Value* shell_fail = setup_ok ? prim_shell(shell_args[0:2], null, interp) : raise_error(interp, "setup failed");
        Value* readdir_fail = setup_ok ? prim_fs_readdir(readdir_args[0:1], null, interp) : raise_error(interp, "setup failed");
        Value* schema_explain_fail = setup_ok ? prim_schema_explain(schema_explain_args[0:2], null, interp) : raise_error(interp, "setup failed");
        g_value_force_make_string_alloc_fail = saved_force;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(shell_fail, "out of memory while allocating string") &&
            runtime_alloc_error_matches(readdir_fail, "out of memory while allocating string") &&
            runtime_alloc_error_matches(schema_explain_fail, "out of memory while allocating string");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: string-backed payload materializers fail closed on string wrapper alloc");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: string-backed payload materializers fail closed on string wrapper alloc (setup_ok=%s shell=%s readdir=%s schema=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(shell_fail),
                runtime_alloc_error_text(readdir_fail),
                runtime_alloc_error_text(schema_explain_fail));
            (*fail)++;
        }
    }

    {
        Value*[2] shell_args = { make_string(interp, "printf ok"), make_symbol(interp, interp.sym_true) };
        bool setup_ok =
            shell_args[0] != null && shell_args[0].tag == STRING &&
            shell_args[1] != null;

        int saved_force = g_value_force_make_list2_alloc_fail_nth;
        g_value_force_make_list2_alloc_fail_nth = 1;
        Value* shell_fail = setup_ok ? prim_shell(shell_args[0:2], null, interp) : raise_error(interp, "setup failed");
        g_value_force_make_list2_alloc_fail_nth = saved_force;

        bool ok = setup_ok &&
            runtime_alloc_error_matches(shell_fail, "shell: failed to construct result list");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: shell result list construction fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: shell result list construction fails closed (setup_ok=%s msg=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(shell_fail));
            (*fail)++;
        }
    }

    {
        bool saved_force = g_value_force_make_string_alloc_fail;
        g_value_force_make_string_alloc_fail = true;
        EvalResult raise_run = run("(handle (/ 1 0) (raise msg 42))", interp);
        g_value_force_make_string_alloc_fail = saved_force;

        char[] msg = raise_run.error.message[:256];
        bool ok = raise_run.error.has_error &&
            str_starts_with(msg, "out of memory while allocating string");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: pending raise string materialization fails closed before handler bind");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: pending raise string materialization fails closed before handler bind (run_error=%s msg=%s)",
                raise_run.error.has_error ? "yes" : "no",
                raise_run.error.has_error ? (ZString)msg : runtime_alloc_error_text(raise_run.value));
            (*fail)++;
        }
    }

    {
        bool saved_force = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        Value* raise_payload = make_raise_payload_names(interp, "runtime", "runtime/error", "boom", make_int(interp, 42));
        g_hashmap_force_struct_alloc_fail = saved_force;

        bool ok = raise_payload == null;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: pending raise payload map helper fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: pending raise payload map helper fails closed (tag=%d msg=%s)",
                raise_payload != null ? (int)raise_payload.tag : -1,
                runtime_alloc_error_text(raise_payload));
            (*fail)++;
        }
    }

    {
        usz root_hashmaps_before = runtime_alloc_active_hashmap_dtor_count(interp.root_scope);
        bool saved_force = g_value_force_make_string_alloc_fail;
        g_value_force_make_string_alloc_fail = true;
        Value* raise_payload = make_raise_payload_names(interp, "runtime", "runtime/error", "boom", make_int(interp, 42));
        g_value_force_make_string_alloc_fail = saved_force;
        usz root_hashmaps_after = runtime_alloc_active_hashmap_dtor_count(interp.root_scope);

        bool ok = raise_payload == null &&
            root_hashmaps_after == root_hashmaps_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: pending raise payload mid-build failure cleans partial root dictionary");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: pending raise payload mid-build failure cleans partial root dictionary (result=%s before=%d after=%d)",
                raise_payload == null ? "null" : "value",
                (long)root_hashmaps_before,
                (long)root_hashmaps_after);
            (*fail)++;
        }
    }

    {
        bool saved_force = g_value_force_alloc_lisp_continuation_fail;
        g_value_force_alloc_lisp_continuation_fail = true;
        EvalResult continuation_run = run("(handle (+ 1 (signal ask 0)) (ask x x))", interp);
        g_value_force_alloc_lisp_continuation_fail = saved_force;

        char[] msg = continuation_run.error.message[:256];
        bool ok = continuation_run.error.has_error &&
            str_starts_with(msg, "handle: failed to allocate continuation");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: handled effect continuation allocation fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: handled effect continuation allocation fails closed (run_error=%s msg=%s)",
                continuation_run.error.has_error ? "yes" : "no",
                continuation_run.error.has_error ? (ZString)msg : runtime_alloc_error_text(continuation_run.value));
            (*fail)++;
        }
    }

    {
        EvalResult cmp_run = run("(lambda (a) (lambda (b) (error \"boom\")))", interp);
        Value* list = make_cons(interp, make_int(interp, 3), make_cons(interp, make_int(interp, 1), make_cons(interp, make_int(interp, 2), make_nil(interp))));
        Value*[2] sort_args = { cmp_run.error.has_error ? null : cmp_run.value, list };
        Value* sort_fail = !cmp_run.error.has_error && cmp_run.value != null
            ? prim_sort_by(sort_args[0:2], null, interp)
            : raise_error(interp, "setup failed");

        bool ok = !cmp_run.error.has_error &&
            cmp_run.value != null &&
            runtime_alloc_error_matches(sort_fail, "boom");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: sort-by comparator failures propagate directly");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: sort-by comparator failures propagate directly (setup_ok=%s msg=%s)",
                !cmp_run.error.has_error && cmp_run.value != null ? "yes" : "no",
                runtime_alloc_error_text(sort_fail));
            (*fail)++;
        }
    }

    {
        bool first_ok = false;
        bool second_ok = false;
        bool third_ok = false;

        int saved_nth = g_http_force_parse_response_key_alloc_fail_nth;

        g_http_force_parse_response_key_alloc_fail_nth = 1;
        Value* first_fail = parse_response("HTTP/1.1 200 OK\r\nX-Test: yes\r\n\r\nbody", interp);
        first_ok = runtime_alloc_error_matches(first_fail, "http: failed to allocate response field key");

        g_http_force_parse_response_key_alloc_fail_nth = 2;
        Value* second_fail = parse_response("HTTP/1.1 200 OK\r\nX-Test: yes\r\n\r\nbody", interp);
        second_ok = runtime_alloc_error_matches(second_fail, "http: failed to allocate response field key");

        g_http_force_parse_response_key_alloc_fail_nth = 3;
        Value* third_fail = parse_response("HTTP/1.1 200 OK\r\nX-Test: yes\r\n\r\nbody", interp);
        third_ok = runtime_alloc_error_matches(third_fail, "http: failed to allocate response field key");

        g_http_force_parse_response_key_alloc_fail_nth = saved_nth;

        bool ok = first_ok && second_ok && third_ok;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: http response key alloc failures fail closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: http response key alloc failures fail closed (first=%s second=%s third=%s)",
                first_ok ? "yes" : "no",
                second_ok ? "yes" : "no",
                third_ok ? "yes" : "no");
            (*fail)++;
        }
    }
}
