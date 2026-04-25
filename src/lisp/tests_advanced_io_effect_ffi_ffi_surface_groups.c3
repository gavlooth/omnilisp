module lisp;

import std::io;
import std::core::mem;

extern fn uint omni_ffi_test_echo_u32(uint value) @extern("omni_ffi_test_echo_u32");
extern fn ulong omni_ffi_test_echo_u64(ulong value) @extern("omni_ffi_test_echo_u64");
extern fn uint omni_ffi_test_return_u32_max() @extern("omni_ffi_test_return_u32_max");
extern fn ulong omni_ffi_test_return_u64_long_max() @extern("omni_ffi_test_return_u64_long_max");
extern fn ulong omni_ffi_test_return_u64_above_long_max() @extern("omni_ffi_test_return_u64_above_long_max");
extern fn int omni_ffi_call_rejects_unsupported_type_tags_for_tests() @extern("omni_ffi_call_rejects_unsupported_type_tags_for_tests");
extern fn int omni_ffi_closure_alloc_rejects_unsupported_type_tags_for_tests() @extern("omni_ffi_closure_alloc_rejects_unsupported_type_tags_for_tests");

fn bool ffi_unsigned_pack_contract_for_tests(Interp* interp) {
    long int_slot = 0;
    double dbl_slot = 0.0;
    void* ptr_slot = null;
    int arg_type = 0;
    void* arg_value = null;

    Value* neg_u32 = ffi_pack_call_arg(make_int(interp, -1), FfiTypeTag.FFI_TYPE_UINT32, 0, null,
        &int_slot, &dbl_slot, &ptr_slot, &arg_type, &arg_value, interp);
    Value* high_u32 = ffi_pack_call_arg(make_int(interp, (long)uint.max + 1), FfiTypeTag.FFI_TYPE_UINT32, 0, null,
        &int_slot, &dbl_slot, &ptr_slot, &arg_type, &arg_value, interp);
    Value* max_u32 = ffi_pack_call_arg(make_int(interp, (long)uint.max), FfiTypeTag.FFI_TYPE_UINT32, 0, null,
        &int_slot, &dbl_slot, &ptr_slot, &arg_type, &arg_value, interp);
    bool u32_ok = ffi_test_error_contains(neg_u32, "out of range for UInt32") &&
        ffi_test_error_contains(high_u32, "out of range for UInt32") &&
        max_u32 == null &&
        arg_type == (int)FfiTypeTag.FFI_TYPE_UINT32 &&
        arg_value == (void*)&int_slot &&
        int_slot == (long)uint.max;

    Value* neg_u64 = ffi_pack_call_arg(make_int(interp, -1), FfiTypeTag.FFI_TYPE_UINT64, 0, null,
        &int_slot, &dbl_slot, &ptr_slot, &arg_type, &arg_value, interp);
    Value* max_signed_u64 = ffi_pack_call_arg(make_int(interp, long.max), FfiTypeTag.FFI_TYPE_UINT64, 0, null,
        &int_slot, &dbl_slot, &ptr_slot, &arg_type, &arg_value, interp);
    bool u64_ok = ffi_test_error_contains(neg_u64, "out of range for UInt64") &&
        max_signed_u64 == null &&
        arg_type == (int)FfiTypeTag.FFI_TYPE_UINT64 &&
        arg_value == (void*)&int_slot &&
        int_slot == long.max;

    return u32_ok && u64_ok;
}

fn bool ffi_unsigned_return_contract_for_tests(Interp* interp) {
    Value* u32 = ffi_return_value_for(null, FfiTypeTag.FFI_TYPE_UINT32, null, 0, uint.max, 0, 0.0f, 0.0, null, interp);
    Value* u64_max_signed = ffi_return_value_for(null, FfiTypeTag.FFI_TYPE_UINT64, null, 0, 0, (ulong)long.max, 0.0f, 0.0, null, interp);
    Value* u64_high = ffi_return_value_for(null, FfiTypeTag.FFI_TYPE_UINT64, null, 0, 0, (ulong)long.max + (ulong)1, 0.0f, 0.0, null, interp);
    return u32 != null && u32.tag == INT && u32.int_val == (long)uint.max &&
        u64_max_signed != null && u64_max_signed.tag == INT && u64_max_signed.int_val == long.max &&
        ffi_test_error_contains(u64_high, "UInt64 return exceeds Integer range");
}

fn bool ffi_unsigned_libffi_contract_for_tests(Interp* interp) {
    long int_storage = (long)uint.max;
    int arg_type = (int)FfiTypeTag.FFI_TYPE_UINT32;
    void* arg_value = (void*)&int_storage;
    uint ret_u32 = 0;
    if (omni_ffi_call((void*)&omni_ffi_test_echo_u32, 1, &arg_type, &arg_value,
        (int)FfiTypeTag.FFI_TYPE_UINT32, (void*)&ret_u32) != 0 || ret_u32 != uint.max) {
        return false;
    }

    int_storage = long.max;
    arg_type = (int)FfiTypeTag.FFI_TYPE_UINT64;
    arg_value = (void*)&int_storage;
    ulong ret_u64 = 0;
    if (omni_ffi_call((void*)&omni_ffi_test_echo_u64, 1, &arg_type, &arg_value,
        (int)FfiTypeTag.FFI_TYPE_UINT64, (void*)&ret_u64) != 0 || ret_u64 != (ulong)long.max) {
        return false;
    }

    uint ret_u32_max = 0;
    if (omni_ffi_call((void*)&omni_ffi_test_return_u32_max, 0, null, null,
        (int)FfiTypeTag.FFI_TYPE_UINT32, (void*)&ret_u32_max) != 0 || ret_u32_max != uint.max) {
        return false;
    }

    ulong ret_u64_high = 0;
    return omni_ffi_call((void*)&omni_ffi_test_return_u64_above_long_max, 0, null, null,
        (int)FfiTypeTag.FFI_TYPE_UINT64, (void*)&ret_u64_high) == 0 &&
        ret_u64_high == (ulong)long.max + (ulong)1;
}

fn bool ffi_buffer_return_contract_for_tests(Interp* interp) {
    char[4] buffer = { 'o', 'm', 'n', 0 };
    Value* returned = ffi_return_value_for(null, FfiTypeTag.FFI_TYPE_BUFFER, null,
        0, 0, 0, 0.0f, 0.0, (void*)&buffer[0], interp);
    if (returned == null || returned.tag != FFI_HANDLE || returned.ffi_val == null) return false;

    Value* desc = foreign_runtime_describe_handle(returned, interp);
    if (desc == null || desc.tag != HASHMAP) return false;

    Value* runtime = ffi_metadata_dict_lookup(desc, interp, "runtime");
    Value* kind = ffi_metadata_dict_lookup(desc, interp, "kind");
    Value* name = ffi_metadata_dict_lookup(desc, interp, "name");
    Value* caps = ffi_metadata_dict_lookup(desc, interp, "capabilities");
    bool describe_ok = runtime != null && runtime.tag == SYMBOL && runtime.sym_val == interp.symbols.intern("native") &&
        kind != null && kind.tag == SYMBOL && kind.sym_val == interp.symbols.intern("buffer") &&
        name != null && name.tag == SYMBOL && name.sym_val == interp.symbols.intern("ffi-buffer") &&
        caps != null && caps.tag == ARRAY && caps.array_val != null &&
        caps.array_val.length == 0;

    Value* released = foreign_runtime_release_handle(returned, interp);
    bool release_rejected = released != null && released.tag == ERROR &&
        ffi_test_error_contains(released, "handle is not releasable");
    return describe_ok && release_rejected;
}

fn bool ffi_invalid_libffi_type_contract_for_tests() {
    return omni_ffi_call_rejects_unsupported_type_tags_for_tests() == 0 &&
        omni_ffi_closure_alloc_rejects_unsupported_type_tags_for_tests() == 0;
}

fn bool ffi_async_unsigned_return_contract_for_tests() {
    FfiAsyncCallCtx* ctx = (FfiAsyncCallCtx*)mem::malloc(FfiAsyncCallCtx.sizeof);
    if (ctx == null) return false;
    *ctx = {};
    ctx.fn_ptr = (void*)&omni_ffi_test_return_u64_above_long_max;
    ctx.nargs = 0;
    ctx.ret_type = (int)FfiTypeTag.FFI_TYPE_UINT64;

    OffloadCompletion* completion = ffi_async_offload_callback({ .custom_ctx = (void*)ctx });
    bool high_rejected = completion != null &&
        completion.kind == OFFLOAD_RES_ERROR &&
        completion.error_msg != null &&
        completion.error_len > 0;
    if (completion != null) scheduler_free_offload_completion(completion);

    ctx.fn_ptr = (void*)&omni_ffi_test_return_u32_max;
    ctx.ret_type = (int)FfiTypeTag.FFI_TYPE_UINT32;
    completion = ffi_async_offload_callback({ .custom_ctx = (void*)ctx });
    bool u32_ok = completion != null &&
        completion.kind == OFFLOAD_RES_INT &&
        completion.int_result == (long)uint.max;
    if (completion != null) scheduler_free_offload_completion(completion);

    ffi_async_call_ctx_free((void*)ctx);
    return high_rejected && u32_ok;
}

fn bool ffi_callback_finalization_guard_contract_for_tests(Interp* interp) {
    EvalResult ri = run("(ffi-callback (lambda (a b) (- a b)) ['Integer 'Integer] 'Integer)", interp);
    if (ri.error.has_error || ri.value == null || ri.value.tag != FFI_HANDLE || ri.value.ffi_val == null) return false;
    FfiCallbackContext* ctx = (FfiCallbackContext*)ri.value.ffi_val.user_data;
    if (ctx == null || ffi_callback_try_enter((void*)ctx) == 0) return false;
    void* code_ptr = ri.value.ffi_val.lib_handle;

    Value*[1] release_args = { ri.value };
    Value* released = prim_foreign_release(release_args[..], null, interp);
    bool release_ok = released != null && released.tag == VOID;
    bool held_active = ctx.open.load() == 0 &&
        ctx.finalize_requested.load() != 0 &&
        ctx.resources_released.load() == 0 &&
        ctx.active_calls.load() == 1;
    bool closed_rejects_enter = ffi_callback_try_enter((void*)ctx) == 0 &&
        ctx.active_calls.load() == 1;
    int rejected_result = omni_ffi_test_callback_int_int(code_ptr, 10, 3);
    bool closed_zeroes_return_slot = rejected_result == 0;

    ffi_callback_leave((void*)ctx);
    bool released_after_leave = ctx.resources_released.load() != 0 &&
        ctx.active_calls.load() == 0 &&
        ctx.callback == null &&
        ctx.param_types == null &&
        ctx.closure_handle == null;
    bool tombstone_rejects_enter = ffi_callback_try_enter((void*)ctx) == 0 &&
        ctx.active_calls.load() == 0;

    return release_ok && held_active && closed_rejects_enter && closed_zeroes_return_slot &&
        released_after_leave && tombstone_rejects_enter;
}


fn void run_advanced_ffi_system_surface_tests(Interp* interp, int* pass, int* fail) {
    run_advanced_c_api_string_boundary_tests(interp, pass, fail);

    // === FFI TESTS (define [ffi lib] / define [ffi λ]) ===
    // Library declaration
    setup(interp, "(define [ffi lib] libc \"libc.so.6\")");
    test_tag(interp, "ffi lib handle", "libc", FFI_HANDLE, pass, fail);
    test_truthy(interp, "foreign-describe uses array capabilities metadata",
        "(let (m (foreign-describe libc)) "
        "  (and (= (ref m 'type) 'ForeignHandle) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'library) "
        "       (= (ref m 'name) \"libc.so.6\") "
        "       (array? (ref m 'capabilities))"
        "       (= (length (ref m 'capabilities)) 3)"
        "       (= (ref (ref m 'capabilities) 0) 'load)"
        "       (= (ref (ref m 'capabilities) 1) 'resolve)"
        "       (= (ref (ref m 'capabilities) 2) 'reflect)))",
        pass, fail);
    test_error_contains(interp, "foreign-release rejects non-releasable library handle",
        "(foreign-release libc)",
        "handle is not releasable",
        pass, fail);
    test_error_contains(interp, "ffi lib rejects empty path",
        "(define [ffi lib] ffi-empty-path \"\")",
        "path must be a non-empty string shorter than 256 bytes",
        pass, fail);
    {
        bool saved_force = g_env_force_barrier_promote_failure;
        g_env_force_barrier_promote_failure = true;
        test_error_contains(interp, "ffi lib registration failure releases constructed wrapper",
            "(define [ffi lib] ffi-lib-fail \"libc.so.6\")",
            "ffi lib: out of memory",
            pass, fail);
        g_env_force_barrier_promote_failure = saved_force;
    }
    setup(interp, "(define [ffi lib] libc-recover \"libc.so.6\")");
    test_tag(interp, "ffi lib registration still works after failed wrapper release",
        "libc-recover", FFI_HANDLE, pass, fail);

    // Shared dlopen registry: two handles for the same path share the
    // same underlying dlopen handle.
    setup(interp, "(define [ffi lib] libc-shared-a \"libc.so.6\")");
    setup(interp, "(define [ffi lib] libc-shared-b \"libc.so.6\")");
    {
        Value* a_val = interp.global_env.lookup(interp.symbols.intern("libc-shared-a"));
        Value* b_val = interp.global_env.lookup(interp.symbols.intern("libc-shared-b"));
        if (a_val != null && a_val.tag == FFI_HANDLE &&
            b_val != null && b_val.tag == FFI_HANDLE &&
            a_val.ffi_val != null && b_val.ffi_val != null &&
            a_val.ffi_val.lib_handle == b_val.ffi_val.lib_handle) {
            io::printn("[PASS] shared dlopen registry");
            (*pass)++;
        } else {
            io::printn("[FAIL] shared dlopen registry: libc-shared-a and libc-shared-b do not share handle");
            (*fail)++;
        }
    }
    {
        char[256] too_long_path;
        for (usz i = 0; i < too_long_path.len; i++) too_long_path[i] = 'a';
        void* rejected = ffi_dlopen_registry_acquire(too_long_path[0:too_long_path.len]);
        if (rejected == null) {
            io::printn("[PASS] shared dlopen registry rejects overlong non-NUL path");
            (*pass)++;
        } else {
            ffi_dlopen_registry_release(too_long_path[0:too_long_path.len]);
            io::printn("[FAIL] shared dlopen registry rejects overlong non-NUL path");
            (*fail)++;
        }
    }
    {
        char[12] nul_path = { 'l', 'i', 'b', 'c', (char)0, '.', 's', 'o', '.', '6', 0, 0 };
        void* rejected = ffi_dlopen_registry_acquire(nul_path[0:10]);
        if (rejected == null) {
            io::printn("[PASS] shared dlopen registry rejects embedded-NUL path");
            (*pass)++;
        } else {
            ffi_dlopen_registry_release(nul_path[0:10]);
            io::printn("[FAIL] shared dlopen registry rejects embedded-NUL path");
            (*fail)++;
        }
    }

    // Function bindings — integer functions
    setup(interp, "(define [ffi λ libc] (strlen (^String s)) ^Integer)");
    test_eq(interp, "ffi λ strlen", "(strlen \"hello\")", 5, pass, fail);
    test_eq(interp, "ffi λ strlen empty", "(strlen \"\")", 0, pass, fail);
    test_truthy(interp, "foreign-describe FFI function signature metadata",
        "(let (m (foreign-describe strlen)) "
        "  (and (= (ref m 'type) 'ForeignCallable) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'function) "
        "       (= (ref m 'name) \"strlen\") "
        "       (= (ref m 'c-name) \"strlen\") "
        "       (array? (ref m 'parameters)) "
        "       (= (length (ref m 'parameters)) 1) "
        "       (= (ref (ref (ref m 'parameters) 0) 'type) 'String) "
        "       (= (ref (ref (ref m 'parameters) 0) 'abi-type) 'string) "
        "       (= (ref (ref m 'returns) 'type) 'Integer) "
        "       (= (ref (ref m 'returns) 'abi-type) 'int) "
        "       (array? (ref m 'capabilities)) "
        "       (= (length (ref m 'capabilities)) 2) "
        "       (= (ref (ref m 'capabilities) 0) 'call) "
        "       (= (ref (ref m 'capabilities) 1) 'reflect)))",
        pass, fail);
    test_truthy(interp, "foreign-describe FFI function signature metadata after call",
        "(block "
        "(strlen \"hello\") "
        "(let (m (foreign-describe strlen)) "
        "  (and (= (ref m 'type) 'ForeignCallable) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'function) "
        "       (= (ref m 'name) \"strlen\") "
        "       (= (ref m 'c-name) \"strlen\") "
        "       (array? (ref m 'parameters)) "
        "       (= (length (ref m 'parameters)) 1) "
        "       (= (ref (ref (ref m 'parameters) 0) 'type) 'String) "
        "       (= (ref (ref (ref m 'parameters) 0) 'abi-type) 'string) "
        "       (= (ref (ref m 'returns) 'type) 'Integer) "
        "       (= (ref (ref m 'returns) 'abi-type) 'int) "
        "       (array? (ref m 'capabilities)) "
        "       (= (length (ref m 'capabilities)) 2) "
        "       (= (ref (ref m 'capabilities) 0) 'call) "
        "       (= (ref (ref m 'capabilities) 1) 'reflect))))",
        pass, fail);

    setup(interp,
        "(define [ffi module] libc-grouped \"libc.so.6\" "
        "  (strlen (^String s)) ^Integer "
        "  (abs (^Integer n)) ^Integer "
        "  (fopen (^String path) (^String mode)) ^{'name File 'ownership owned 'finalizer fclose} "
        "  (fclose (^{'name File 'ownership borrowed 'nullability non-null} file)) ^Integer)");
    test_eq(interp, "ffi module grouped strlen", "(strlen \"hello\")", 5, pass, fail);
    test_eq(interp, "ffi module grouped abs", "(abs -7)", 7, pass, fail);
    test_error_contains(interp, "ffi module grouped fclose rejects nil for non-null handle",
        "(fclose nil)",
        "expected non-null ForeignHandle",
        pass, fail);
    test_truthy(interp, "ffi module grouped foreign-describe callable metadata",
        "(let (m (foreign-describe strlen)) "
        "  (and (= (ref m 'type) 'ForeignCallable) "
        "       (= (ref m 'runtime) 'c-abi) "
        "       (= (ref m 'kind) 'function) "
        "       (= (ref m 'name) \"strlen\") "
        "       (= (ref m 'c-name) \"strlen\") "
        "       (array? (ref m 'parameters)) "
        "       (= (ref (ref m 'returns) 'type) 'Integer) "
        "       (array? (ref m 'capabilities)) "
        "       (= (length (ref m 'capabilities)) 2) "
        "       (= (ref (ref m 'capabilities) 0) 'call) "
        "       (= (ref (ref m 'capabilities) 1) 'reflect)))",
        pass, fail);
    test_truthy(interp, "ffi module grouped ForeignHandle return metadata",
        "(let (r (ref (foreign-describe fopen) 'returns)) "
        "  (and (= (ref r 'type) 'ForeignHandle) "
        "       (= (ref r 'name) 'File) "
        "       (= (ref r 'ownership) 'owned) "
        "       (= (ref r 'finalizer) 'fclose)))",
        pass, fail);

    setup(interp, "(define [ffi λ libc] (abs (^Integer n)) ^Integer)");
    test_eq(interp, "ffi λ abs", "(abs -42)", 42, pass, fail);
    test_eq(interp, "ffi λ abs zero", "(abs 0)", 0, pass, fail);

    setup(interp, "(define [ffi λ libc] (atoi (^String s)) ^Integer)");
    test_eq(interp, "ffi λ atoi", "(atoi \"12345\")", 12345, pass, fail);
    test_eq(interp, "ffi λ atoi neg", "(atoi \"-99\")", -99, pass, fail);

    // Zero-arg function
    setup(interp, "(define [ffi λ libc] (getpid) ^Integer)");
    test_gt(interp, "ffi λ getpid", "(getpid)", 0, pass, fail);

    // Math library — Float64 functions
    setup(interp, "(define [ffi lib] libm \"libm.so.6\")");
    setup(interp, "(define [ffi λ libm] (sqrt (^Float64 x)) ^Float64)");
    test_eq_double(interp, "ffi λ sqrt", "(sqrt 4.0)", 2.0, pass, fail);
    test_eq_double(interp, "ffi λ sqrt 9", "(sqrt 9.0)", 3.0, pass, fail);

    setup(interp, "(define [ffi λ libm] (sinf (^Float32 x)) ^Float32)");
    test_truthy(interp, "ffi λ sinf returns Float32 payload",
        "(let (r (sinf (Float32 1.5707964))) (and (= (type-of r) 'Float32) (> r (Float32 0.999)) (< r (Float32 1.001))))",
        pass, fail);

    setup(interp, "(define [ffi λ libm] (pow (^Float64 x) (^Float64 y)) ^Float64)");
    test_eq_double(interp, "ffi λ pow", "(pow 2.0 10.0)", 1024.0, pass, fail);

    // Mixed int/Float64 (libffi enables this — old FFI couldn't)
    setup(interp, "(define [ffi λ libm] (ldexp (^Float64 x) (^Integer exp)) ^Float64)");
    test_eq_double(interp, "ffi λ ldexp", "(ldexp 1.0 10)", 1024.0, pass, fail);

    setup(interp, "(define [ffi λ libc] (strtol (^String s) (^ForeignHandle endptr) (^Integer base)) ^Integer)");
    test_eq(interp, "ffi λ strtol mixed scalar foreign handle", "(strtol \"12345\" nil 10)", 12345, pass, fail);

    setup(interp, "(define [ffi λ libc] (strchr (^String s) (^Integer c)) ^String)");
    test_str_val(interp, "ffi λ string return copies C string", "(strchr \"hello\" 101)", "ello", pass, fail);
    test_nil(interp, "ffi λ string return null maps to nil", "(strchr \"hello\" 120)", pass, fail);

    // Void return (canonical ForeignHandle annotation)
    setup(interp, "(define [ffi λ libc] (free (^ForeignHandle p)))");
    test_truthy(interp, "ffi λ free null (^ForeignHandle)", "(= (type-of (free nil)) 'Void)", pass, fail);
    test_error_contains(interp, "ffi λ free rejects integer address (^ForeignHandle)",
        "(free 0)",
        "argument 1 expected ForeignHandle, got Integer", pass, fail);

    // Error cases
    test_error_contains(interp, "ffi λ strlen rejects non-string arg",
        "(strlen 123)",
        "argument 1 expected String, got Integer", pass, fail);
    test_error_contains(interp, "ffi λ abs rejects boolean arg",
        "(abs false)",
        "argument 1 expected Integer, got Boolean", pass, fail);
    test_error_contains(interp, "ffi λ sqrt rejects boolean arg",
        "(sqrt false)",
        "argument 1 expected Float64, got Boolean", pass, fail);
    test_error_contains(interp, "ffi λ free rejects string arg",
        "(free \"hello\")",
        "argument 1 expected ForeignHandle, got String", pass, fail);
    test_error_contains(interp, "ffi λ exact arity rejects extra args",
        "(getpid 1)",
        "arity mismatch: expected 0 argument(s), got 1", pass, fail);
    test_error_contains(interp, "ffi lib bad",
        "(define [ffi lib] bad-lib \"nonexistent_xyz.so\")",
        "dlopen failed for", pass, fail);
    test_error_contains(interp, "ffi lib bad preserves path cause",
        "(define [ffi lib] bad-lib2 \"nonexistent_xyz.so\")",
        "nonexistent_xyz.so", pass, fail);
    setup(interp, "(define [ffi λ libc] (definitely_missing_ffi_symbol (^Integer n)) ^Integer)");
    test_error_contains(interp, "ffi λ missing symbol cause",
        "(definitely_missing_ffi_symbol 1)",
        "dlsym failed for", pass, fail);
    test_error_contains(interp, "ffi λ missing symbol preserves loader cause",
        "(definitely_missing_ffi_symbol 1)",
        "undefined symbol", pass, fail);
    test_error_contains(interp, "ffi λ unsupported parameter annotation",
        "(block (define [ffi lib] bad-ffi-lib \"libc.so.6\") (define [ffi λ bad-ffi-lib] (strlen (^Symbol s)) ^Integer))",
        "unsupported parameter type annotation '^Symbol'", pass, fail);
    test_error_contains(interp, "ffi λ legacy Pointer annotation is rejected",
        "(block (define [ffi lib] bad-ffi-lib3 \"libc.so.6\") (define [ffi λ bad-ffi-lib3] (free (^Pointer p))))",
        "unsupported parameter type annotation '^Pointer'", pass, fail);
    test_error_contains(interp, "ffi λ unsupported return annotation",
        "(block (define [ffi lib] bad-ffi-lib2 \"libc.so.6\") (define [ffi λ bad-ffi-lib2] (strlen (^String s)) ^Symbol))",
        "unsupported return type annotation '^Symbol'", pass, fail);
    setup(interp, "(define [ffi λ libc] (malloc (^Integer size)) ^ForeignHandle)");
    setup(interp, "(define [ffi λ libc variadic] (snprintf (^ForeignHandle buf) (^Integer size) (^String fmt)) ^Integer)");
    test_gt(interp, "ffi λ variadic snprintf",
        "(let (buf (malloc 64)) (snprintf buf 64 \"hello %s %d\" \"world\" 42))",
        0, pass, fail);
    test_gt(interp, "ffi-async-call variadic snprintf",
        "(let (buf (malloc 64)) (ffi-async-call snprintf buf 64 \"hello %s %d\" \"world\" 42))",
        0, pass, fail);
    test_error_contains(interp, "ffi λ variadic snprintf rejects unsupported variadic Omni value",
        "(let (buf (malloc 64)) (snprintf buf 64 \"hello %s %d\" nil 42))",
        "unsupported variadic argument type", pass, fail);
    {
        Value* bad_ffi_string = advanced_io_nul_string(interp, "hello", "world");
        long int_slot = 0;
        double dbl_slot = 0.0;
        void* ptr_slot = null;
        int arg_type = 0;
        void* arg_value = null;
        Value* fixed_err = null;
        Value* variadic_err = null;
        Value* variadic_f32_err = null;
        if (bad_ffi_string != null && bad_ffi_string.tag == STRING) {
            fixed_err = ffi_pack_call_arg(
                bad_ffi_string,
                FfiTypeTag.FFI_TYPE_STRING,
                0,
                null,
                &int_slot,
                &dbl_slot,
                &ptr_slot,
                &arg_type,
                &arg_value,
                interp
            );
            variadic_err = ffi_pack_variadic_call_arg(
                bad_ffi_string,
                3,
                &int_slot,
                &dbl_slot,
                &ptr_slot,
                &arg_type,
                &arg_value,
                interp
            );
        }
        variadic_f32_err = ffi_pack_variadic_call_arg(
            make_float32(interp, 1.25f),
            3,
            &int_slot,
            &dbl_slot,
            &ptr_slot,
            &arg_type,
            &arg_value,
            interp
        );
        ftxui_record_result(
            "ffi fixed string packing rejects embedded NUL string arg",
            ffi_test_error_contains(fixed_err, "string argument contains embedded NUL byte"),
            pass,
            fail);
        ftxui_record_result(
            "ffi variadic string packing rejects embedded NUL string arg",
            ffi_test_error_contains(variadic_err, "variadic string argument contains embedded NUL byte"),
            pass,
            fail);
        ftxui_record_result(
            "ffi variadic Float32 arguments promote to Float64 ABI",
            variadic_f32_err == null && arg_type == (int)FfiTypeTag.FFI_TYPE_DOUBLE &&
                arg_value == (void*)&dbl_slot && dbl_slot > 1.24 && dbl_slot < 1.26,
            pass,
            fail);
    }
    test_error_contains(interp, "ffi λ Boolean requires real booleans",
        "(block (define [ffi λ libc] (abs (^Boolean flag)) ^Integer) (abs 1))",
        "argument 1 expected Boolean, got Integer", pass, fail);
    test_error_contains(interp, "ffi module grouped rejects missing return annotation",
        "(define [ffi module] bad-grouped \"libc.so.6\" (strlen (^String s)))",
        "expected return type annotation after [ffi module] function signature", pass, fail);

    if (omni_ffi_call_null_arg_vectors_guard_for_tests() == -1) {
        io::printn("[PASS] ffi native call rejects null arg vectors");
        (*pass)++;
    } else {
        io::printn("[FAIL] ffi native call rejects null arg vectors");
        (*fail)++;
    }
    if (omni_ffi_call_var_null_arg_vectors_guard_for_tests() == -1) {
        io::printn("[PASS] ffi native variadic call rejects null arg vectors");
        (*pass)++;
    } else {
        io::printn("[FAIL] ffi native variadic call rejects null arg vectors");
        (*fail)++;
    }
    if (omni_ffi_closure_alloc_failure_clears_outputs_for_tests() == 0) {
        io::printn("[PASS] ffi native closure failure clears outputs");
        (*pass)++;
    } else {
        io::printn("[FAIL] ffi native closure failure clears outputs");
        (*fail)++;
    }
    ftxui_record_result(
        "ffi unsigned parameter packing rejects wrapping values",
        ffi_unsigned_pack_contract_for_tests(interp),
        pass,
        fail);
    ftxui_record_result(
        "ffi unsigned return conversion rejects unrepresentable UInt64",
        ffi_unsigned_return_contract_for_tests(interp),
        pass,
        fail);
    ftxui_record_result(
        "ffi native unsigned libffi storage preserves unsigned ABI bits",
        ffi_unsigned_libffi_contract_for_tests(interp),
        pass,
        fail);
    ftxui_record_result(
        "ffi native buffer return conversion exports opaque handles",
        ffi_buffer_return_contract_for_tests(interp),
        pass,
        fail);
    ftxui_record_result(
        "ffi native unsupported type tags fail closed before libffi prep",
        ffi_invalid_libffi_type_contract_for_tests(),
        pass,
        fail);
    {
        int saved_cons_fail = g_value_force_make_cons_or_error_alloc_fail_nth;
        g_value_force_make_cons_or_error_alloc_fail_nth = 1;
        Value* forced_cons_failure = ffi_callback_make_arg_cons(interp, make_int(interp, 1), make_nil(interp));
        g_value_force_make_cons_or_error_alloc_fail_nth = saved_cons_fail;
        if (forced_cons_failure != null && forced_cons_failure.tag == ERROR) {
            io::printn("[PASS] ffi-callback argument list cons failure fails closed");
            (*pass)++;
        } else {
            io::printn("[FAIL] ffi-callback argument list cons failure fails closed");
            (*fail)++;
        }
    }

    // === FFI CALLBACK TESTS ===
    setup(interp, "(define cb (ffi-callback (lambda (a b) (- a b)) ['Integer 'Integer] 'Integer))");
    test_tag(interp, "ffi-callback list form creates ForeignHandle", "cb", FFI_HANDLE, pass, fail);

    setup(interp, "(define cb2 (ffi-callback (lambda (a b) (+ a b)) 'Integer 'Integer 'Integer))");
    test_tag(interp, "ffi-callback variadic form creates ForeignHandle", "cb2", FFI_HANDLE, pass, fail);

    test_error_contains(interp, "ffi-callback rejects unsupported parameter type symbol",
        "(ffi-callback (lambda (x) x) ['UnknownType] 'Integer)",
        "unsupported parameter type", pass, fail);

    test_error_contains(interp, "ffi-callback rejects unsupported return type symbol",
        "(ffi-callback (lambda (x) x) ['Integer] 'UnknownType)",
        "unsupported return type", pass, fail);

    test_error_contains(interp, "ffi-callback rejects String return without ownership contract",
        "(ffi-callback (lambda () \"owned-by-omni\") [] 'String)",
        "String returns are unsupported", pass, fail);

    test_error_contains(interp, "ffi-callback rejects non-callable first arg",
        "(ffi-callback 123 ['Integer] 'Integer)",
        "first arg must be a callable", pass, fail);

    // End-to-end: invoke the callback through C test helper
    {
        EvalResult ri = run("(ffi-callback (lambda (a b) (- a b)) ['Integer 'Integer] 'Integer)", interp);
        if (ri.error.has_error || ri.value == null || ri.value.tag != FFI_HANDLE || ri.value.ffi_val == null) {
            io::printfn("[FAIL] ffi-callback end-to-end: failed to create callback");
            (*fail)++;
        } else {
            void* code_ptr = ri.value.ffi_val.lib_handle;
            int result = omni_ffi_test_callback_int_int(code_ptr, 10, 3);
            if (result == 7) {
                io::printfn("[PASS] ffi-callback end-to-end invocation");
                (*pass)++;
            } else {
                io::printfn("[FAIL] ffi-callback end-to-end invocation: expected 7, got %d", result);
                (*fail)++;
            }
        }
    }

    {
        EvalResult ri = run("(ffi-callback (lambda (a b) true) ['Integer 'Integer] 'Boolean)", interp);
        if (ri.error.has_error || ri.value == null || ri.value.tag != FFI_HANDLE || ri.value.ffi_val == null) {
            io::printfn("[FAIL] ffi-callback Boolean return end-to-end: failed to create callback");
            (*fail)++;
        } else {
            int result = omni_ffi_test_callback_int_int(ri.value.ffi_val.lib_handle, 1, 2);
            if (result == 1) {
                io::printfn("[PASS] ffi-callback Boolean return converts true to C true");
                (*pass)++;
            } else {
                io::printfn("[FAIL] ffi-callback Boolean return converts true to C true: expected 1, got %d", result);
                (*fail)++;
            }
        }
    }

    {
        EvalResult ri = run("(ffi-callback (lambda (a b) (car nil)) ['Integer 'Integer] 'Integer)", interp);
        if (ri.error.has_error || ri.value == null || ri.value.tag != FFI_HANDLE || ri.value.ffi_val == null) {
            io::printfn("[FAIL] ffi-callback error return slot zeroing: failed to create callback");
            (*fail)++;
        } else {
            int result = omni_ffi_test_callback_int_int(ri.value.ffi_val.lib_handle, 1, 2);
            if (result == 0) {
                io::printfn("[PASS] ffi-callback error return slot is deterministic zero");
                (*pass)++;
            } else {
                io::printfn("[FAIL] ffi-callback error return slot is deterministic zero: expected 0, got %d", result);
                (*fail)++;
            }
        }
    }

    {
        EvalResult ri = run("(ffi-callback (lambda (s) (if (string? s) (string-length s) -1)) ['String] 'Integer)", interp);
        if (ri.error.has_error || ri.value == null || ri.value.tag != FFI_HANDLE || ri.value.ffi_val == null) {
            io::printfn("[FAIL] ffi-callback String parameter end-to-end: failed to create callback");
            (*fail)++;
        } else {
            char[14] text = { 'c', 'a', 'l', 'l', 'b', 'a', 'c', 'k', '-', 't', 'e', 'x', 't', 0 };
            int result = omni_ffi_test_callback_string_len(ri.value.ffi_val.lib_handle, &text[0]);
            if (result == 13) {
                io::printfn("[PASS] ffi-callback String parameter arrives as Omni String");
                (*pass)++;
            } else {
                io::printfn("[FAIL] ffi-callback String parameter arrives as Omni String: expected 13, got %d", result);
                (*fail)++;
            }
        }
    }

    // Regression: foreign-release on ffi-callback must not segfault
    {
        EvalResult ri = run("(let (cb (ffi-callback (lambda (a b) (- a b)) ['Integer 'Integer] 'Integer)) (foreign-release cb))", interp);
        if (ri.error.has_error) {
            io::printfn("[FAIL] ffi-callback foreign-release: got error: %s", ri.error.message);
            (*fail)++;
        } else {
            io::printfn("[PASS] ffi-callback foreign-release");
            (*pass)++;
        }
    }
    ftxui_record_result(
        "ffi-callback finalization waits for active dispatch and leaves tombstone",
        ffi_callback_finalization_guard_contract_for_tests(interp),
        pass,
        fail);

    // === ASYNC FFI CALL TESTS ===
    test_eq(interp, "ffi-async-call strlen",
        "(ffi-async-call strlen \"hello\")", 5, pass, fail);
    test_eq(interp, "ffi-async-call strlen empty",
        "(ffi-async-call strlen \"\")", 0, pass, fail);
    ftxui_record_result(
        "ffi-async-call owns copied String arguments before worker handoff",
        ffi_async_string_arg_copy_contract_for_tests(interp),
        pass,
        fail);
    ftxui_record_result(
        "ffi-async-call unsigned returns reject unrepresentable UInt64",
        ffi_async_unsigned_return_contract_for_tests(),
        pass,
        fail);
    {
        Value* bad_async_string = advanced_io_nul_string(interp, "bad", "string");
        Value* strlen_v = interp.global_env.lookup(interp.symbols.intern("strlen"));
        if (bad_async_string == null || bad_async_string.tag == ERROR || strlen_v == null) {
            ftxui_record_result("ffi-async-call embedded-NUL string test setup", false, pass, fail);
        } else {
            Value*[2] async_args = { strlen_v, bad_async_string };
            ftxui_record_result(
                "ffi-async-call rejects embedded-NUL String before worker handoff",
                ffi_test_error_contains(prim_ffi_async_call(async_args[..], null, interp), "embedded NUL byte"),
                pass,
                fail);
        }
    }
    test_eq(interp, "ffi-async-call atoi neg",
        "(ffi-async-call atoi \"-42\")", -42, pass, fail);
    test_eq_double(interp, "ffi-async-call sqrt",
        "(ffi-async-call sqrt 4.0)", 2.0, pass, fail);
    test_truthy(interp, "ffi-async-call sinf Float32 payload",
        "(let (r (ffi-async-call sinf (Float32 1.5707964))) (and (> r 0.999) (< r 1.001)))",
        pass, fail);
    test_eq_double(interp, "ffi-async-call pow",
        "(ffi-async-call pow 2.0 10.0)", 1024.0, pass, fail);
    test_eq(interp, "ffi-async-call atoi",
        "(ffi-async-call atoi \"9876\")", 9876, pass, fail);
    test_str_val(interp, "ffi-async-call string return copies C string",
        "(ffi-async-call strchr \"hello\" 101)", "ello", pass, fail);
    test_nil(interp, "ffi-async-call string return null maps to nil",
        "(ffi-async-call strchr \"hello\" 120)", pass, fail);
    test_error_contains(interp, "ffi-async-call rejects pointer-like return",
        "(ffi-async-call malloc 16)",
        "pointer-like return types are not supported", pass, fail);
    test_error_contains(interp, "ffi-async-call rejects non-bound first arg",
        "(ffi-async-call 123 456)",
        "expected an FFI-bound function", pass, fail);
    test_error_contains(interp, "ffi-async-call rejects string first arg",
        "(ffi-async-call \"hello\" 1)",
        "expected an FFI-bound function", pass, fail);
    test_error_contains(interp, "ffi-async-call arity mismatch propagates",
        "(ffi-async-call strlen \"hello\" \"extra\")",
        "ffi: expected 1 args, got 2", pass, fail);
    test_error_contains(interp, "ffi-async-call type mismatch propagates",
        "(ffi-async-call strlen 123)",
        "expected String, got Integer", pass, fail);
    setup(interp, "(define [ffi λ libc] (memchr (^Buffer buf) (^Integer c) (^Integer n)) ^ForeignHandle)");
    test_error_contains(interp, "ffi-async-call rejects Buffer args before worker handoff",
        "(ffi-async-call memchr \"abc\" 97 3)",
        "Buffer arguments are not supported across worker handoff", pass, fail);

    // === SYSTEM PRIMITIVES TESTS ===
    // random — returns Float64 in [0,1)
    test_truthy(interp, "random range", "(let (r (random)) (and (>= r 0.0) (< r 1.0)))", pass, fail);
    test_truthy(interp, "random is Float64", "(float64? (random))", pass, fail);
    // random-int — returns int in [0,n)
    test_truthy(interp, "random-int range", "(let (r (random-int 10)) (and (>= r 0) (< r 10)))", pass, fail);
    test_truthy(interp, "random-int is int", "(int? (random-int 100))", pass, fail);
    test_truthy(interp, "random-int large bound range",
        "(let (r (random-int 9223372036854775807)) (and (>= r 0) (< r 9223372036854775807)))",
        pass, fail);
    test_error_contains(interp, "random-int rejects zero bound",
        "(random-int 0)",
        "random-int: bound must be positive", pass, fail);
    test_error_contains(interp, "random-int rejects negative bound",
        "(random-int -4)",
        "random-int: bound must be positive", pass, fail);
    // time — returns epoch seconds (should be > 2020-01-01)
    test_gt(interp, "time epoch", "(time)", 1577836800, pass, fail);
    // time-ms — returns epoch milliseconds (> time * 1000)
    test_gt(interp, "time-ms epoch", "(time-ms)", 1577836800000, pass, fail);
    // getenv — HOME should exist, nonexistent returns nil
    test_str(interp, "getenv HOME", "(getenv \"HOME\")", pass, fail);
    test_nil(interp, "getenv missing", "(getenv \"OMNI_NONEXISTENT_VAR_XYZ\")", pass, fail);
    {
        Value* bad_system_string = advanced_io_nul_string(interp, "OMNI_BAD_", "NAME");
        if (bad_system_string == null || bad_system_string.tag == ERROR) {
            ftxui_record_result("system C API string boundary test setup", false, pass, fail);
        } else {
            Value*[1] sys_args = { bad_system_string };
            ftxui_record_result(
                "getenv rejects embedded NUL name before native call",
                advanced_io_error_contains(prim_getenv(sys_args[..], null, interp), "embedded NUL byte"),
                pass, fail);
            ftxui_record_result(
                "shell rejects embedded NUL command before native call",
                advanced_io_error_contains(prim_shell(sys_args[..], null, interp), "embedded NUL byte"),
                pass, fail);
        }
    }
    // shell — captures stdout
    test_str_val(interp, "shell echo", "(shell \"echo hello\")", "hello", pass, fail);
    test_eq(interp, "shell captures stdout beyond 8192 bytes",
        "(string-byte-length (shell \"printf '%9000s' | tr ' ' x\"))", 9000, pass, fail);
    test_truthy(interp, "shell status tuple reports success",
        "(let (r (shell \"true\" true)) (and (list? r) (= (car r) \"\") (= (car (cdr r)) 0)))",
        pass, fail);
    test_truthy(interp, "shell status tuple reports command failure",
        "(let (r (shell \"false\" true)) (and (list? r) (= (car r) \"\") (= (car (cdr r)) 1)))",
        pass, fail);
    test_error_contains(interp, "shell rejects trailing arg",
        "(shell \"true\" true false)",
        "shell: arity mismatch: expected 1 or 2 arguments", pass, fail);
    test_error_contains(interp, "exit rejects trailing arg without exiting",
        "(exit 0 1)",
        "exit: arity mismatch: expected 0 or 1 arguments", pass, fail);
    // sleep — returns void (tiny sleep to not slow tests)
    test_tag_interp(interp, "sleep zero", "(sleep 0)", VOID, pass, fail);
    test_tag_interp(interp, "sleep tiny", "(sleep 0.001)", VOID, pass, fail);
    test_error_contains(interp, "sleep rejects negative seconds",
        "(sleep -1)",
        "sleep: seconds must be non-negative finite and within range", pass, fail);
    test_error_contains(interp, "sleep rejects overflow seconds",
        "(sleep 4294968)",
        "sleep: seconds must be non-negative finite and within range", pass, fail);

    run_advanced_uv_timer_callback_register_invoke_test(interp, pass, fail);
    run_advanced_uv_timer_callback_unhandle_idempotence_test(interp, pass, fail);
    run_advanced_uv_timer_callback_scope_release_test(interp, pass, fail);
    run_advanced_uv_timer_callback_wrapper_failure_scope_release_test(interp, pass, fail);
    run_advanced_uv_timer_callback_finalizer_scope_release_test(interp, pass, fail);
    run_advanced_uv_timer_callback_handle_isolation_test(interp, pass, fail);
    run_advanced_ftxui_context_screen_lifecycle_test(pass, fail);
    run_advanced_ftxui_table_canvas_conversion_test(pass, fail);
    run_advanced_ftxui_component_callback_delivery_test(pass, fail);
    run_advanced_ftxui_event_read_result_clear_guard_test(pass, fail);
}
