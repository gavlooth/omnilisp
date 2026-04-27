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
    char*[FFI_ASYNC_MAX_ARGS] owned_strings;
    int ret_type;
}

fn void ffi_async_call_ctx_free(void* raw_ctx) {
    if (raw_ctx == null) return;
    FfiAsyncCallCtx* ctx = (FfiAsyncCallCtx*)raw_ctx;
    for (usz i = 0; i < FFI_ASYNC_MAX_ARGS; i++) {
        if (ctx.owned_strings[i] != null) {
            mem::free(ctx.owned_strings[i]);
            ctx.owned_strings[i] = null;
        }
    }
    mem::free(raw_ctx);
}

fn Value* ffi_async_own_string_arg(FfiAsyncCallCtx* ctx, Value* arg, usz arg_index, Interp* interp) {
    if (ctx == null || arg_index >= FFI_ASYNC_MAX_ARGS) {
        return ffi_raise(interp, "ffi/invalid-state", "ffi-async-call: internal argument ownership error");
    }
    int raw_arg_type = ctx.arg_types[arg_index];
    if (!ffi_abi_arg_type_tag_int_supported(raw_arg_type)) {
        return ffi_raise(interp, "ffi/invalid-state", "ffi-async-call: unsupported argument ABI type");
    }
    if (raw_arg_type != (int)FfiTypeTag.FFI_TYPE_STRING) return null;
    if (arg == null || arg.tag == NIL) {
        ctx.ptr_storage[arg_index] = null;
        ctx.arg_values[arg_index] = (void*)&ctx.ptr_storage[arg_index];
        return null;
    }
    if (arg.tag != STRING) {
        return ffi_arg_type_error(interp, arg_index, interp.sym_String, arg);
    }

    CApiStringResult owned = c_api_string_dup_no_nul(arg.str_chars[:arg.str_len]);
    if (owned.contains_nul) {
        return ffi_raise(interp, "ffi/invalid-argument", "ffi-async-call: string argument contains embedded NUL byte");
    }
    if (owned.too_long) {
        return ffi_raise(interp, "ffi/invalid-argument", "ffi-async-call: string argument is too large");
    }
    if (!owned.ok || owned.ptr == null) {
        return ffi_raise(interp, "ffi/out-of-memory", "ffi-async-call: failed to copy string argument");
    }

    ctx.owned_strings[arg_index] = owned.ptr;
    ctx.ptr_storage[arg_index] = (void*)owned.ptr;
    ctx.arg_values[arg_index] = (void*)&ctx.ptr_storage[arg_index];
    return null;
}

fn bool ffi_async_string_arg_copy_contract_for_tests(Interp* interp) {
    if (interp == null) return false;
    Value* arg = make_string(interp, "async-copy");
    if (arg == null || arg.tag != STRING) return false;

    FfiAsyncCallCtx* ctx = (FfiAsyncCallCtx*)mem::malloc(FfiAsyncCallCtx.sizeof);
    if (ctx == null) return false;
    *ctx = {};
    ctx.arg_types[0] = (int)FfiTypeTag.FFI_TYPE_STRING;
    ctx.ptr_storage[0] = (void*)arg.str_chars;
    ctx.arg_values[0] = (void*)&ctx.ptr_storage[0];

    Value* err = ffi_async_own_string_arg(ctx, arg, 0, interp);
    bool ok = err == null &&
        ctx.owned_strings[0] != null &&
        ctx.ptr_storage[0] == (void*)ctx.owned_strings[0] &&
        ctx.ptr_storage[0] != (void*)arg.str_chars &&
        ffi_c_string_len(ctx.owned_strings[0]) == arg.str_len;
    if (ok) {
        for (usz i = 0; i < arg.str_len; i++) {
            if (ctx.owned_strings[0][i] != arg.str_chars[i]) {
                ok = false;
                break;
            }
        }
    }
    ffi_async_call_ctx_free((void*)ctx);
    return ok;
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

fn OffloadCompletion* ffi_async_error_completion(char[] message) {
    OffloadCompletion* completion = (OffloadCompletion*)mem::malloc(OffloadCompletion.sizeof);
    if (completion == null) return null;
    *completion = {};
    scheduler_set_offload_error(completion, message);
    return completion;
}

fn OffloadCompletion* ffi_async_offload_callback(OffloadWork work) {
    FfiAsyncCallCtx* ctx = (FfiAsyncCallCtx*)work.custom_ctx;
    if (ctx == null || ctx.fn_ptr == null) {
        return ffi_async_error_completion("ffi-async: invalid call context");
    }
    if (ctx.nargs < 0 || (usz)ctx.nargs > FFI_ASYNC_MAX_ARGS) {
        return ffi_async_error_completion("ffi-async: invalid argument count");
    }
    for (usz i = 0; i < (usz)ctx.nargs; i++) {
        if (!ffi_abi_arg_type_tag_int_supported(ctx.arg_types[i])) {
            return ffi_async_error_completion("ffi-async: unsupported argument ABI type");
        }
        if (ctx.arg_values[i] == null) {
            return ffi_async_error_completion("ffi-async: invalid argument storage");
        }
    }
    if (!ffi_abi_type_tag_int_supported(ctx.ret_type)) {
        return ffi_async_error_completion("ffi-async: unsupported return ABI type");
    }

    long ret_int = 0;
    uint ret_u32 = 0;
    ulong ret_u64 = 0;
    float ret_f32 = 0.0f;
    double ret_dbl = 0.0;
    void* ret_ptr = null;
    void* ret_storage;
    FfiTypeTag ret_tag = (FfiTypeTag)ctx.ret_type;
    switch (ret_tag) {
        case FFI_TYPE_VOID:
            ret_storage = (void*)&ret_int;
            break;
        case FFI_TYPE_INT:
        case FFI_TYPE_BOOL:
        case FFI_TYPE_INT32:
        case FFI_TYPE_INT64:
            ret_storage = (void*)&ret_int;
            break;
        case FFI_TYPE_UINT32:
            ret_storage = (void*)&ret_u32;
            break;
        case FFI_TYPE_UINT64:
            ret_storage = (void*)&ret_u64;
            break;
        case FFI_TYPE_FLOAT32:
            ret_storage = (void*)&ret_f32;
            break;
        case FFI_TYPE_DOUBLE:
            ret_storage = (void*)&ret_dbl;
            break;
        case FFI_TYPE_PTR:
        case FFI_TYPE_STRING:
        case FFI_TYPE_BUFFER:
        case FFI_TYPE_STRUCT:
            ret_storage = (void*)&ret_ptr;
            break;
        default:
            ret_storage = null;
    }
    if (ret_storage == null) {
        return ffi_async_error_completion("ffi-async: unsupported return ABI type");
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

    switch (ret_tag) {
        case FFI_TYPE_VOID:
            completion.kind = OFFLOAD_RES_VOID;
            break;
        case FFI_TYPE_INT:
        case FFI_TYPE_INT32:
        case FFI_TYPE_INT64:
            completion.kind = OFFLOAD_RES_INT;
            completion.int_result = ret_int;
            break;
        case FFI_TYPE_UINT32:
            completion.kind = OFFLOAD_RES_INT;
            completion.int_result = (long)ret_u32;
            break;
        case FFI_TYPE_UINT64:
            if (ret_u64 > (ulong)long.max) {
                scheduler_set_offload_error(completion, "ffi-async: UInt64 return exceeds Integer range");
            } else {
                completion.kind = OFFLOAD_RES_INT;
                completion.int_result = (long)ret_u64;
            }
            break;
        case FFI_TYPE_BOOL:
            completion.kind = OFFLOAD_RES_BOOL;
            completion.int_result = ret_int != 0 ? 1 : 0;
            break;
        case FFI_TYPE_FLOAT32:
            completion.kind = OFFLOAD_RES_FLOAT32;
            completion.double_result = (double)ret_f32;
            break;
        case FFI_TYPE_DOUBLE:
            completion.kind = OFFLOAD_RES_DOUBLE;
            completion.double_result = ret_dbl;
            break;
        case FFI_TYPE_STRING:
            (void)ffi_async_set_string_result(completion, (char*)ret_ptr);
            break;
        case FFI_TYPE_PTR:
        case FFI_TYPE_BUFFER:
        case FFI_TYPE_STRUCT:
            scheduler_set_offload_error(completion, "ffi-async: pointer-like return types are not supported");
            break;
        default:
            scheduler_set_offload_error(completion, "ffi-async: unsupported return ABI type");
    }

    return completion;
}

<*
@require interp != null
*>
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
            if (bound.param_types[i] == FFI_TYPE_BUFFER) {
                ffi_async_call_ctx_free((void*)ctx);
                return ffi_raise(interp, "ffi/unsupported",
                    "ffi-async-call: Buffer arguments are not supported across worker handoff");
            }
            FfiHandlePolicy* policy = bound.param_handle_policies != null
                ? &bound.param_handle_policies[i]
                : null;
            if (Value* err = ffi_pack_call_arg(arg, bound.param_types[i], i, policy,
                &ctx.int_storage[i], &ctx.dbl_storage[i], &ctx.ptr_storage[i],
                &ctx.arg_types[i], &ctx.arg_values[i], interp)) {
                ffi_async_call_ctx_free((void*)ctx);
                return err;
            }
            if (Value* err = ffi_async_own_string_arg(ctx, arg, i, interp)) {
                ffi_async_call_ctx_free((void*)ctx);
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
                ffi_async_call_ctx_free((void*)ctx);
                return err;
            }
            if (Value* own_err = ffi_async_own_string_arg(ctx, arg, i, interp)) {
                ffi_async_call_ctx_free((void*)ctx);
                return own_err;
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
