module lisp;

import std::io;
import std::core::mem;
import main;

const usz FFI_ASYNC_MAX_ARGS = 8;

struct FfiAsyncCallCtx {
    void* fn_ptr;
    int nargs;
    int[FFI_ASYNC_MAX_ARGS] arg_types;
    void*[FFI_ASYNC_MAX_ARGS] arg_values;
    long[FFI_ASYNC_MAX_ARGS] int_storage;
    double[FFI_ASYNC_MAX_ARGS] dbl_storage;
    void*[FFI_ASYNC_MAX_ARGS] ptr_storage;
    int ret_type;
}

fn void ffi_async_call_ctx_free(void* raw_ctx) {
    if (raw_ctx == null) return;
    mem::free(raw_ctx);
}

fn bool ffi_async_set_string_result(OffloadCompletion* completion, char* text) {
    if (text == null) {
        completion.kind = OFFLOAD_RES_NIL;
        return true;
    }

    usz len = ffi_c_string_len(text);
    if (len == usz.max) {
        scheduler_set_offload_error(completion, "ffi-async: string return is too large");
        return false;
    }
    char* copy = (char*)mem::malloc(len + 1);
    if (copy == null) {
        scheduler_set_offload_error(completion, "ffi-async: failed to copy string return");
        return false;
    }
    for (usz i = 0; i < len; i++) copy[i] = text[i];
    copy[len] = 0;
    return scheduler_set_offload_bytes_owned(
        completion,
        copy,
        len,
        "ffi-async: failed to publish string return"
    );
}

fn OffloadCompletion* ffi_async_offload_callback(OffloadWork work) {
    FfiAsyncCallCtx* ctx = (FfiAsyncCallCtx*)work.custom_ctx;
    if (ctx == null || ctx.fn_ptr == null) {
        OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
        if (completion == null) return null;
        *completion = {};
        scheduler_set_offload_error(completion, "ffi-async: invalid call context");
        return completion;
    }

    long ret_int = 0;
    float ret_f32 = 0.0f;
    double ret_dbl = 0.0;
    void* ret_ptr = null;
    void* ret_storage;
    switch ((FfiTypeTag)ctx.ret_type) {
        case FFI_TYPE_INT:
        case FFI_TYPE_BOOL:
        case FFI_TYPE_INT32:
        case FFI_TYPE_INT64:
        case FFI_TYPE_UINT32:
        case FFI_TYPE_UINT64:
            ret_storage = (void*)&ret_int;
        case FFI_TYPE_FLOAT32:
            ret_storage = (void*)&ret_f32;
        case FFI_TYPE_DOUBLE:
            ret_storage = (void*)&ret_dbl;
        case FFI_TYPE_PTR:
        case FFI_TYPE_STRING:
        case FFI_TYPE_BUFFER:
            ret_storage = (void*)&ret_ptr;
        default:
            ret_storage = (void*)&ret_int;
    }

    int rc = omni_ffi_call(ctx.fn_ptr, ctx.nargs, &ctx.arg_types[0], &ctx.arg_values[0],
                           ctx.ret_type, ret_storage);

    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};

    if (rc != 0) {
        scheduler_set_offload_error(completion, "ffi-async: libffi call failed");
        return completion;
    }

    switch ((FfiTypeTag)ctx.ret_type) {
        case FFI_TYPE_VOID:
            completion.kind = OFFLOAD_RES_NIL;
        case FFI_TYPE_INT:
        case FFI_TYPE_INT32:
        case FFI_TYPE_INT64:
        case FFI_TYPE_UINT32:
        case FFI_TYPE_UINT64:
            completion.kind = OFFLOAD_RES_INT;
            completion.int_result = ret_int;
        case FFI_TYPE_BOOL:
            completion.kind = OFFLOAD_RES_INT;
            completion.int_result = ret_int != 0 ? 1 : 0;
        case FFI_TYPE_FLOAT32:
            completion.kind = OFFLOAD_RES_DOUBLE;
            completion.double_result = (double)ret_f32;
        case FFI_TYPE_DOUBLE:
            completion.kind = OFFLOAD_RES_DOUBLE;
            completion.double_result = ret_dbl;
        case FFI_TYPE_STRING:
            (void)ffi_async_set_string_result(completion, (char*)ret_ptr);
        case FFI_TYPE_PTR:
        case FFI_TYPE_BUFFER:
        case FFI_TYPE_STRUCT:
            scheduler_set_offload_error(completion, "ffi-async: pointer-like return types are not supported");
        default:
            completion.kind = OFFLOAD_RES_NIL;
    }

    return completion;
}

fn Value* prim_ffi_async_call(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len < 1) {
        return ffi_raise(interp, "type/arg-mismatch", "ffi-async-call: expected at least a bound function");
    }

    Value* bound_prim = args[0];
    if (bound_prim == null || bound_prim.tag != PRIMITIVE || bound_prim.prim_val == null
        || bound_prim.prim_val.func != &prim_ffi_bound_call) {
        return ffi_raise(interp, "type/arg-mismatch", "ffi-async-call: expected an FFI-bound function as first argument");
    }

    FfiBoundFn* bound = (FfiBoundFn*)bound_prim.prim_val.user_data;
    if (bound == null || bound.is_unavailable) {
        return ffi_raise(interp, "ffi/unavailable", "ffi-async-call: bound function is unavailable");
    }

    if (Value* err = ffi_resolve_bound_fn(bound, interp)) return err;
    if (bound.fn_ptr == null) {
        return ffi_raise(interp, "ffi/unavailable", "ffi-async-call: could not resolve symbol");
    }

    usz nargs = args.len - 1;
    if (nargs > FFI_ASYNC_MAX_ARGS) {
        return ffi_raise(interp, "type/arity", "ffi-async-call: too many arguments (max 8)");
    }

    // Validate arg count (same logic as prim_ffi_bound_call)
    usz expected_min = bound.is_variadic ? bound.fixed_param_count : bound.param_count;
    if (!bound.is_variadic && nargs != bound.param_count) {
        char[256] ebuf;
        char[] msg;
        if (try formatted = io::bprintf(&ebuf, "ffi: expected %d args, got %d",
            (int)bound.param_count, (int)nargs)) {
            msg = formatted;
        } else {
            msg = "ffi: argument count mismatch";
        }
        return ffi_raise(interp, "ffi/invalid-state", msg);
    }
    if (bound.is_variadic && nargs < expected_min) {
        char[256] ebuf;
        char[] msg;
        if (try formatted = io::bprintf(&ebuf, "ffi: variadic expected at least %d args, got %d",
            (int)expected_min, (int)nargs)) {
            msg = formatted;
        } else {
            msg = "ffi: variadic argument count mismatch";
        }
        return ffi_raise(interp, "ffi/invalid-state", msg);
    }

    FfiAsyncCallCtx* ctx = (FfiAsyncCallCtx*)mem::malloc(FfiAsyncCallCtx.sizeof);
    if (ctx == null) {
        return ffi_raise(interp, "ffi/out-of-memory", "ffi-async-call: out of memory");
    }
    *ctx = {};
    ctx.fn_ptr = bound.fn_ptr;
    ctx.nargs = (int)nargs;
    ctx.ret_type = (int)bound.return_type;

    for (usz i = 0; i < nargs; i++) {
        Value* arg = args[i + 1];
        if (i < bound.param_count) {
            FfiHandlePolicy* policy = bound.param_handle_policies != null
                ? &bound.param_handle_policies[i]
                : null;
            if (Value* err = ffi_pack_call_arg(arg, bound.param_types[i], i, policy,
                &ctx.int_storage[i], &ctx.dbl_storage[i], &ctx.ptr_storage[i],
                &ctx.arg_types[i], &ctx.arg_values[i], interp)) {
                mem::free(ctx);
                return err;
            }
        } else {
            Value* err = ffi_pack_variadic_call_arg(
                arg,
                i,
                &ctx.int_storage[i],
                &ctx.dbl_storage[i],
                &ctx.ptr_storage[i],
                &ctx.arg_types[i],
                &ctx.arg_values[i],
                interp
            );
            if (err != null) {
                mem::free(ctx);
                return err;
            }
        }
    }

    return scheduler_execute_custom_offload_job_owned(
        &ffi_async_offload_callback,
        (void*)ctx,
        &ffi_async_call_ctx_free,
        interp
    );
}
