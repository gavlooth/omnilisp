module lisp;

import std::io;
import std::core::mem;
import main;

fn void run_memory_lifetime_runtime_alloc_groups_core_constructor_tests(Interp* interp, int* pass, int* fail) {
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
        main::ScopeRegion* saved = interp.current_scope;
        main::ScopeRegion* child = main::scope_create(saved);
        bool setup_ok = child != null;
        Value* big_integer = null;
        Value* big_float = null;
        Value* big_complex = null;
        Value* copied_integer = null;
        Value* copied_float = null;
        Value* copied_complex = null;
        BoundaryCopyFault integer_fault = BOUNDARY_COPY_FAULT_NONE;
        BoundaryCopyFault float_fault = BOUNDARY_COPY_FAULT_NONE;
        BoundaryCopyFault complex_fault = BOUNDARY_COPY_FAULT_NONE;

        if (setup_ok) {
            interp.current_scope = child;
            big_integer = make_big_integer_from_i64(interp, 42);
            big_float = make_big_float_from_i64(interp, 42);
            big_complex = make_big_complex_from_i64(interp, 42);
            setup_ok =
                big_integer != null && big_integer.tag == BIG_INTEGER &&
                big_float != null && big_float.tag == BIG_FLOAT &&
                big_complex != null && big_complex.tag == BIG_COMPLEX;

            interp.current_scope = saved;
            interp.releasing_scope = child;
            bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
            main::g_scope_force_dtor_alloc_oom = true;
            copied_integer = setup_ok ? copy_to_parent_with_fault(big_integer, interp, &integer_fault) : null;
            copied_float = setup_ok ? copy_to_parent_with_fault(big_float, interp, &float_fault) : null;
            copied_complex = setup_ok ? copy_to_parent_with_fault(big_complex, interp, &complex_fault) : null;
            main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
            interp.releasing_scope = null;
        }
        interp.current_scope = saved;
        interp.releasing_scope = null;

        bool ok = setup_ok &&
            copied_integer == null &&
            copied_float == null &&
            copied_complex == null &&
            integer_fault == BOUNDARY_COPY_FAULT_DTOR_REGISTRATION &&
            float_fault == BOUNDARY_COPY_FAULT_DTOR_REGISTRATION &&
            complex_fault == BOUNDARY_COPY_FAULT_DTOR_REGISTRATION;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: heap numeric parent-copy dtor OOM reports dtor fault");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: heap numeric parent-copy dtor OOM reports dtor fault (setup=%s int=%s float=%s complex=%s)",
                setup_ok ? "yes" : "no",
                (ZString)boundary_copy_fault_name(integer_fault),
                (ZString)boundary_copy_fault_name(float_fault),
                (ZString)boundary_copy_fault_name(complex_fault));
            (*fail)++;
        }

        if (child != null) main::scope_release(child);
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
        usz values_before = runtime_alloc_active_value_dtor_count(interp.current_scope);
        int* payload = (int*)mem::malloc(int.sizeof);
        bool payload_ok = payload != null;
        if (payload_ok) {
            *payload = 0x7BEEF;
        }

        g_runtime_alloc_ffi_finalizer_calls = 0;
        bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
        main::g_scope_force_dtor_alloc_oom = true;
        Value* failed = payload_ok
            ? make_ffi_handle_ex(interp, payload, "oom-ffi-finalizer-dtor", &runtime_alloc_ffi_finalizer, false, false)
            : null;
        main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
        usz values_after = runtime_alloc_active_value_dtor_count(interp.current_scope);

        bool ok = payload_ok &&
            runtime_alloc_error_matches(failed, "runtime/out-of-memory") &&
            g_runtime_alloc_ffi_finalizer_calls == 1 &&
            *payload == 0 &&
            values_after == values_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: ffi finalizer-owned payload release survives dtor registration OOM");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: ffi finalizer-owned payload release survives dtor registration OOM (payload_ok=%s result=%s calls=%d payload=%d before_dtors=%d after_dtors=%d)",
                payload_ok ? "yes" : "no",
                runtime_alloc_error_text(failed),
                g_runtime_alloc_ffi_finalizer_calls,
                payload_ok ? *payload : -1,
                (long)values_before,
                (long)values_after);
            (*fail)++;
        }

        if (payload_ok) {
            mem::free(payload);
        }
    }

    {
        usz values_before = runtime_alloc_active_value_dtor_count(interp.current_scope);
        void* payload = mem::malloc(8);
        bool payload_ok = payload != null;

        bool saved_dtor_oom = main::g_scope_force_dtor_alloc_oom;
        main::g_scope_force_dtor_alloc_oom = true;
        Value* failed = payload_ok
            ? make_ffi_handle_ex(interp, payload, "oom-ffi-free-only-dtor", null, true, false)
            : null;
        main::g_scope_force_dtor_alloc_oom = saved_dtor_oom;
        usz values_after = runtime_alloc_active_value_dtor_count(interp.current_scope);

        bool ok = payload_ok &&
            runtime_alloc_error_matches(failed, "runtime/out-of-memory") &&
            values_after == values_before;
        if (ok) {
            emit_pass_literal("[PASS] lifetime: ffi free-owned payload release survives dtor registration OOM");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] lifetime: ffi free-owned payload release survives dtor registration OOM (payload_ok=%s result=%s before_dtors=%d after_dtors=%d)",
                payload_ok ? "yes" : "no",
                runtime_alloc_error_text(failed),
                (long)values_before,
                (long)values_after);
            (*fail)++;
        }
    }
}
