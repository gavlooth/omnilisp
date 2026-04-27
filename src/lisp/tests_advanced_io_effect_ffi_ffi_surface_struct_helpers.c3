module lisp;

import std::core::mem;

extern fn void* omni_ffi_test_return_static_ptr() @extern("omni_ffi_test_return_static_ptr");

fn bool ffi_struct_return_storage_contract_for_tests(Interp* interp) {
    long ret_int = 0;
    uint ret_u32 = 0;
    ulong ret_u64 = 0;
    float ret_f32 = 0.0f;
    double ret_dbl = 0.0;
    void* ret_ptr = null;
    void* storage = ffi_return_storage_for(
        FfiTypeTag.FFI_TYPE_STRUCT,
        &ret_int,
        &ret_u32,
        &ret_u64,
        &ret_f32,
        &ret_dbl,
        &ret_ptr
    );

    FfiBoundFn bound = {};
    bound.fn_ptr = (void*)&omni_ffi_test_return_static_ptr;
    bound.lib_handle = (void*)1;
    bound.return_type = FfiTypeTag.FFI_TYPE_STRUCT;
    Value*[] no_args = {};
    Value* called = foreign_runtime_call_c_abi_bound(&bound, no_args, null, interp);
    bool sync_ok = called != null &&
        called.tag == FFI_HANDLE &&
        called.ffi_val != null &&
        called.ffi_val.lib_handle != null;

    FfiAsyncCallCtx* ctx = (FfiAsyncCallCtx*)mem::malloc(FfiAsyncCallCtx.sizeof);
    if (ctx == null) return false;
    *ctx = {};
    ctx.fn_ptr = (void*)&omni_ffi_test_return_static_ptr;
    ctx.nargs = 0;
    ctx.ret_type = (int)FfiTypeTag.FFI_TYPE_STRUCT;
    OffloadCompletion* completion = ffi_async_offload_callback({ .custom_ctx = (void*)ctx });
    bool async_reaches_pointer_boundary = completion != null &&
        completion.kind == OFFLOAD_RES_ERROR &&
        completion.error_msg != null &&
        str_contains(completion.error_msg[:completion.error_len], "pointer-like return types are not supported");
    if (completion != null) scheduler_free_offload_completion(completion);
    ffi_async_call_ctx_free((void*)ctx);

    return storage == (void*)&ret_ptr && sync_ok && async_reaches_pointer_boundary;
}
