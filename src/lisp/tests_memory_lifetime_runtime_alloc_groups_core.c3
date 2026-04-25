module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_runtime_alloc_groups_core_tests(Interp* interp, int* pass, int* fail) {
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
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* oom_scope = main::scope_create(saved_scope);
        bool setup_ok = oom_scope != null;
        Value* int_fail = null;
        if (setup_ok) {
            interp.current_scope = oom_scope;
            void* filled = oom_scope.alloc(main::SCOPE_CHUNK_INITIAL);
            setup_ok = filled != null;
            bool saved_chunk_oom = main::g_scope_force_chunk_alloc_oom;
            main::g_scope_force_chunk_alloc_oom = true;
            int_fail = setup_ok ? make_int(interp, 42) : null;
            main::g_scope_force_chunk_alloc_oom = saved_chunk_oom;
            interp.current_scope = saved_scope;
            main::scope_release(oom_scope);
        }

        bool ok = setup_ok &&
            runtime_alloc_error_matches(int_fail, "runtime/out-of-memory");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: core scalar value allocation OOM fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: core scalar value allocation OOM fails closed (setup_ok=%s result=%s)",
                setup_ok ? "yes" : "no",
                runtime_alloc_error_text(int_fail));
            (*fail)++;
            if (oom_scope != null && interp.current_scope == oom_scope) {
                interp.current_scope = saved_scope;
                main::scope_release(oom_scope);
            }
        }
    }

    {
        bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
        main::g_scope_force_dtor_alloc_oom = true;
        Value* string_fail = make_string(interp, "dtor registration oom");
        Value* error_fail = make_error(interp, "error dtor registration oom");
        Env* env_fail = make_env(interp, null);
        main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;

        bool ok =
            runtime_alloc_error_matches(string_fail, "out of memory while allocating string") &&
            runtime_alloc_error_matches(error_fail, "runtime/out-of-memory") &&
            env_fail == null;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: core destructor registration OOM fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: core destructor registration OOM fails closed (string=%s error=%s env=%s)",
                runtime_alloc_error_text(string_fail),
                runtime_alloc_error_text(error_fail),
                env_fail == null ? "null" : "value");
            (*fail)++;
        }
    }

    {
        bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
        main::g_scope_force_dtor_alloc_oom = true;
        Value* big_integer_fail = make_big_integer_from_i64(interp, 42);
        Value* big_float_fail = make_big_float_from_i64(interp, 42);
        Value* big_complex_fail = make_big_complex_from_i64(interp, 42);
        main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;

        bool ok =
            (runtime_alloc_error_matches(big_integer_fail, "BigInteger: failed to register value destructor") ||
             runtime_alloc_error_matches(big_integer_fail, "runtime/out-of-memory")) &&
            (runtime_alloc_error_matches(big_float_fail, "BigFloat: failed to register value destructor") ||
             runtime_alloc_error_matches(big_float_fail, "runtime/out-of-memory")) &&
            (runtime_alloc_error_matches(big_complex_fail, "BigComplex: failed to register value destructor") ||
             runtime_alloc_error_matches(big_complex_fail, "runtime/out-of-memory"));
        if (ok) {
            emit_pass_literal("[PASS] lifetime: heap numeric destructor registration OOM fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: heap numeric destructor registration OOM fails closed (int=%s float=%s complex=%s)",
                runtime_alloc_error_text(big_integer_fail),
                runtime_alloc_error_text(big_float_fail),
                runtime_alloc_error_text(big_complex_fail));
            (*fail)++;
        }
    }

    {
        main::ScopeRegion* saved_scope = interp.current_scope;
        main::ScopeRegion* oom_scope = main::scope_create(saved_scope);
        bool setup_ok = oom_scope != null;
        Env* env_alloc_fail = null;
        if (setup_ok) {
            interp.current_scope = oom_scope;
            void* filled = oom_scope.alloc(main::SCOPE_CHUNK_INITIAL);
            setup_ok = filled != null;
            bool saved_chunk_oom = main::g_scope_force_chunk_alloc_oom;
            main::g_scope_force_chunk_alloc_oom = true;
            env_alloc_fail = setup_ok ? make_env(interp, null) : null;
            main::g_scope_force_chunk_alloc_oom = saved_chunk_oom;
            interp.current_scope = saved_scope;
            main::scope_release(oom_scope);
        }

        bool ok = setup_ok && env_alloc_fail == null;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: env allocation OOM fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: env allocation OOM fails closed (setup_ok=%s result=%s)",
                setup_ok ? "yes" : "no",
                env_alloc_fail == null ? "null" : "value");
            (*fail)++;
            if (oom_scope != null && interp.current_scope == oom_scope) {
                interp.current_scope = saved_scope;
                main::scope_release(oom_scope);
            }
        }
    }

    {
        bool saved_escape_dtor_oom = main::g_scope_force_escape_dtor_alloc_oom;
        bool saved_escape_mode = interp.escape_env_mode;
        interp.escape_env_mode = true;
        main::g_scope_force_escape_dtor_alloc_oom = true;
        Env* env_escape_fail = make_env(interp, null);
        main::g_scope_force_escape_dtor_alloc_oom = saved_escape_dtor_oom;
        interp.escape_env_mode = saved_escape_mode;

        bool ok = env_escape_fail == null;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: escape env destructor registration OOM fails closed");
            (*pass)++;
        } else {
            io::printn("[FAIL] lifetime: escape env destructor registration OOM fails closed");
            (*fail)++;
        }
    }

    {
        bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
        main::g_scope_force_dtor_alloc_oom = true;
        Value* closure_fail = make_closure_no_param(interp, null, interp.global_env);
        Value* closure_param_fail = make_closure(
            interp,
            interp.symbols.intern("__closure_param_dtor_oom"),
            null,
            interp.global_env);
        main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;

        bool ok =
            (runtime_alloc_error_matches(closure_fail, "closure: failed to register destructor") ||
             runtime_alloc_error_matches(closure_fail, "runtime/out-of-memory")) &&
            (runtime_alloc_error_matches(closure_param_fail, "closure: failed to register destructor") ||
             runtime_alloc_error_matches(closure_param_fail, "runtime/out-of-memory"));
        if (ok) {
            emit_pass_literal("[PASS] lifetime: closure destructor registration OOM fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: closure destructor registration OOM fails closed (no_param=%s param=%s)",
                runtime_alloc_error_text(closure_fail),
                runtime_alloc_error_text(closure_param_fail));
            (*fail)++;
        }
    }

    {
        Continuation* k = interp.alloc_lisp_continuation();
        bool saved_wrapper_oom = g_value_force_make_continuation_wrapper_alloc_fail;
        g_value_force_make_continuation_wrapper_alloc_fail = true;
        Value* cont_fail = k != null ? make_continuation(interp, k) : null;
        g_value_force_make_continuation_wrapper_alloc_fail = saved_wrapper_oom;
        interp_discard_lisp_continuation(interp, k);

        bool ok = k != null &&
            runtime_alloc_error_matches(cont_fail, "continuation: failed to allocate wrapper");
        if (ok) {
            emit_pass_literal("[PASS] lifetime: continuation wrapper allocation OOM fails closed");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: continuation wrapper allocation OOM fails closed (k=%s result=%s)",
                k == null ? "null" : "value",
                runtime_alloc_error_text(cont_fail));
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
