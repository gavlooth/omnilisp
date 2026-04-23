module lisp;

import std::io;

fn Value* ffi_unsupported_type_annotation_error(Interp* interp, char[] role, TypeAnnotation* ann) {
    char[256] ebuf;
    char[] type_name = "metadata dictionary";
    if (ann != null && !ann.is_dict) type_name = interp.symbols.get_name(ann.base_type);
    char[] msg;
    if (ann != null && ann.is_dict) {
        if (try formatted = io::bprintf(&ebuf, "ffi λ: unsupported %s type annotation metadata dictionary",
            (ZString)role)) {
            msg = formatted;
        } else {
            msg = "ffi λ: unsupported type annotation";
        }
    } else if (try formatted = io::bprintf(&ebuf, "ffi λ: unsupported %s type annotation '^%s'",
        (ZString)role, (ZString)type_name)) {
        msg = formatted;
    } else {
        msg = "ffi λ: unsupported type annotation";
    }
    return ffi_raise(interp, "ffi/type-mismatch", msg);
}

fn Value* foreign_runtime_load_c_abi_bound(FfiBoundFn* bound, Interp* interp) {
    if (bound == null) return ffi_raise(interp, "ffi/invalid-state", "ffi: internal error — no bound function");
    if (bound.is_unavailable) return ffi_raise(interp, "ffi/invalid-state", "ffi: bound symbol is unavailable");
    if (bound.lib_handle != null) return null;
    if (bound.lib_name == null || bound.lib_name_len == 0) {
        if (!bound.required_library) {
            bound.is_unavailable = true;
            return null;
        }
        return ffi_raise(interp, "ffi/invalid-state", "ffi: internal error — missing library name");
    }
    (void)dlerror();
    bound.lib_handle = ffi_dlopen_registry_acquire(bound.lib_name[:bound.lib_name_len]);
    if (bound.lib_handle == null) {
        int saved_errno = *c_errno_location();
        char[256] ebuf;
        char* dl_err = dlerror();
        if (!bound.required_library) {
            bound.is_unavailable = true;
            return null;
        }
        if (dl_err != null && dl_err[0] != 0) {
            if (try msg = io::bprintf(&ebuf, "ffi: dlopen failed for '%s': %s (errno=%d)",
                (ZString)bound.lib_name, (ZString)dl_err, saved_errno)) {
                return ffi_raise(interp, "ffi/library-load-failed", msg);
            }
            return ffi_raise(interp, "ffi/library-load-failed", "ffi: dlopen failed");
        }
        if (try msg = io::bprintf(&ebuf, "ffi: dlopen failed for '%s' (errno=%d)",
            (ZString)bound.lib_name, saved_errno)) {
            return ffi_raise(interp, "ffi/library-load-failed", msg);
        }
        return ffi_raise(interp, "ffi/library-load-failed", "ffi: dlopen failed");
    }
    return null;
}

fn Value* foreign_runtime_resolve_c_abi_bound(FfiBoundFn* bound, Interp* interp) {
    if (bound == null) return ffi_raise(interp, "ffi/invalid-state", "ffi: internal error — no bound function");
    if (bound.is_unavailable) return ffi_raise(interp, "ffi/invalid-state", "ffi: bound symbol is unavailable");
    if (bound.fn_ptr != null) return null;
    if (Value* err = foreign_runtime_load_c_abi_bound(bound, interp)) return err;
    if (bound.is_unavailable) return null;
    if (bound.c_name == null) return ffi_raise(interp, "ffi/invalid-state", "ffi: internal error — missing symbol name");
    (void)dlerror();
    bound.fn_ptr = dlsym(bound.lib_handle, (ZString)bound.c_name);
    if (bound.fn_ptr != null) return null;

    int saved_errno = *c_errno_location();
    char[256] ebuf;
    char* dl_err = dlerror();
    if (!bound.required_symbol) {
        bound.is_unavailable = true;
        return null;
    }
    if (dl_err != null && dl_err[0] != 0) {
        if (try msg = io::bprintf(&ebuf, "ffi: dlsym failed for '%s': %s (errno=%d)",
            (ZString)bound.c_name, (ZString)dl_err, saved_errno)) {
            return ffi_raise(interp, "ffi/symbol-not-found", msg);
        }
        return ffi_raise(interp, "ffi/symbol-not-found", "ffi: dlsym failed");
    }
    if (try msg = io::bprintf(&ebuf, "ffi: dlsym failed for '%s' (errno=%d)",
        (ZString)bound.c_name, saved_errno)) {
        return ffi_raise(interp, "ffi/symbol-not-found", msg);
    }
    return ffi_raise(interp, "ffi/symbol-not-found", "ffi: dlsym failed");
}

fn Value* ffi_resolve_bound_fn(FfiBoundFn* bound, Interp* interp) {
    ForeignRuntimeAdapter adapter;
    if (!foreign_runtime_adapter_for_bound(bound, &adapter) ||
        !foreign_runtime_adapter_supports(&adapter, FOREIGN_CAP_LOAD | FOREIGN_CAP_RESOLVE) ||
        adapter.load_bound == null ||
        adapter.resolve_bound == null) {
        return ffi_raise(interp, "ffi/unsupported", "ffi: runtime adapter does not support resolution");
    }
    if (Value* err = adapter.load_bound(bound, interp)) return err;
    return adapter.resolve_bound(bound, interp);
}

fn Value* ffi_arg_type_error(Interp* interp, usz arg_index, SymbolId expected_type, Value* actual) {
    char[256] ebuf;
    char[] expected_name = interp.symbols.get_name(expected_type);
    char[] actual_name = interp.symbols.get_name(value_type_name(actual, interp));
    if (try char[] msg = io::bprintf(&ebuf, "ffi: argument %d expected %s, got %s",
        (int)(arg_index + 1), (ZString)expected_name, (ZString)actual_name)) {
        return ffi_raise(interp, "ffi/type-mismatch", msg);
    }
    return ffi_raise(interp, "ffi/type-mismatch", "ffi: argument type mismatch");
}

fn Value* ffi_null_handle_error(Interp* interp, usz arg_index) {
    char[256] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "ffi: argument %d expected live ForeignHandle",
        (int)(arg_index + 1))) {
        return ffi_raise(interp, "ffi/type-mismatch", msg);
    }
    return ffi_raise(interp, "ffi/type-mismatch", "ffi: expected live ForeignHandle");
}

fn Value* ffi_non_null_handle_error(Interp* interp, usz arg_index) {
    char[256] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "ffi: argument %d expected non-null ForeignHandle",
        (int)(arg_index + 1))) {
        return ffi_raise(interp, "ffi/type-mismatch", msg);
    }
    return ffi_raise(interp, "ffi/type-mismatch", "ffi: expected non-null ForeignHandle");
}

fn Value* ffi_handle_name_error(Interp* interp, usz arg_index, SymbolId expected_name, FfiHandle* actual) {
    char[256] ebuf;
    char[] expected = interp.symbols.get_name(expected_name);
    char[] actual_name = actual != null ? actual.lib_name[:actual.name_len] : "closed";
    if (try char[] msg = io::bprintf(&ebuf,
        "ffi: argument %d expected ForeignHandle %s, got %s",
        (int)(arg_index + 1), (ZString)expected, (ZString)actual_name)) {
        return ffi_raise(interp, "ffi/type-mismatch", msg);
    }
    return ffi_raise(interp, "ffi/type-mismatch", "ffi: ForeignHandle name mismatch");
}

fn bool ffi_handle_name_matches(FfiHandle* handle, SymbolId expected_name, Interp* interp) {
    if (expected_name == INVALID_SYMBOL_ID) return true;
    if (handle == null) return false;
    char[] expected = interp.symbols.get_name(expected_name);
    return ffi_char_slice_equals(handle.lib_name[:handle.name_len], expected);
}

fn Value* ffi_pack_call_arg(Value* arg, FfiTypeTag param_type, usz arg_index,
                            FfiHandlePolicy* policy,
                            long* int_slot, double* dbl_slot, void** ptr_slot,
                            int* out_arg_type, void** out_arg_value,
                            Interp* interp) {
    switch (param_type) {
        case FFI_TYPE_INT:
            if (arg == null || arg.tag != INT) {
                return ffi_arg_type_error(interp, arg_index, interp.sym_Integer, arg);
            }
            *int_slot = arg.int_val;
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)int_slot;
            return null;
        case FFI_TYPE_BOOL:
            if (arg == null || arg.tag != SYMBOL ||
                (arg.sym_val != interp.sym_true && arg.sym_val != interp.sym_false)) {
                return ffi_arg_type_error(interp, arg_index, interp.sym_Boolean, arg);
            }
            *int_slot = arg.sym_val == interp.sym_true ? 1 : 0;
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)int_slot;
            return null;
        case FFI_TYPE_DOUBLE:
            if (arg != null && arg.tag == DOUBLE) {
                *dbl_slot = arg.double_val;
            } else if (arg != null && arg.tag == FLOAT32) {
                *dbl_slot = (double)arg.float32_val;
            } else if (arg != null && arg.tag == INT) {
                *dbl_slot = (double)arg.int_val;
            } else {
                return ffi_arg_type_error(interp, arg_index, interp.sym_Double, arg);
            }
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)dbl_slot;
            return null;
        case FFI_TYPE_PTR:
            if (arg == null || arg.tag == NIL) {
                if (policy != null && policy.is_foreign_handle
                    && policy.nullability == FFI_HANDLE_NON_NULL) {
                    return ffi_non_null_handle_error(interp, arg_index);
                }
                *ptr_slot = null;
            } else if (arg.tag == FFI_HANDLE) {
                if (arg.ffi_val == null || arg.ffi_val.lib_handle == null) {
                    return ffi_null_handle_error(interp, arg_index);
                }
                if (policy != null && policy.is_foreign_handle
                    && !ffi_handle_name_matches(arg.ffi_val, policy.name, interp)) {
                    return ffi_handle_name_error(interp, arg_index, policy.name, arg.ffi_val);
                }
                *ptr_slot = arg.ffi_val.lib_handle;
            } else {
                return ffi_arg_type_error(interp, arg_index, interp.sym_ForeignHandle, arg);
            }
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)ptr_slot;
            return null;
        case FFI_TYPE_STRING:
            if (arg == null || arg.tag == NIL) {
                *ptr_slot = null;
            } else if (arg.tag == STRING) {
                *ptr_slot = (void*)arg.str_chars;
            } else {
                return ffi_arg_type_error(interp, arg_index, interp.sym_String, arg);
            }
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)ptr_slot;
            return null;
        case FFI_TYPE_FLOAT32:
            if (arg != null && arg.tag == FLOAT32) {
                *(float*)int_slot = arg.float32_val;
            } else if (arg != null && arg.tag == DOUBLE) {
                *(float*)int_slot = (float)arg.double_val;
            } else if (arg != null && arg.tag == INT) {
                *(float*)int_slot = (float)arg.int_val;
            } else {
                return ffi_arg_type_error(interp, arg_index, interp.sym_Float32, arg);
            }
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)int_slot;
            return null;
        case FFI_TYPE_INT32:
            if (arg == null || arg.tag != INT) {
                return ffi_arg_type_error(interp, arg_index, interp.sym_Integer, arg);
            }
            *int_slot = (int)arg.int_val;
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)int_slot;
            return null;
        case FFI_TYPE_INT64:
            if (arg == null || arg.tag != INT) {
                return ffi_arg_type_error(interp, arg_index, interp.sym_Integer, arg);
            }
            *int_slot = arg.int_val;
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)int_slot;
            return null;
        case FFI_TYPE_UINT32:
            if (arg == null || arg.tag != INT) {
                return ffi_arg_type_error(interp, arg_index, interp.sym_Integer, arg);
            }
            *int_slot = (uint)arg.int_val;
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)int_slot;
            return null;
        case FFI_TYPE_UINT64:
            if (arg == null || arg.tag != INT) {
                return ffi_arg_type_error(interp, arg_index, interp.sym_Integer, arg);
            }
            *int_slot = (long)(ulong)arg.int_val;
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)int_slot;
            return null;
        case FFI_TYPE_BUFFER:
            if (arg == null || arg.tag == NIL) {
                *ptr_slot = null;
            } else if (arg.tag == STRING) {
                *ptr_slot = (void*)arg.str_chars;
            } else if (arg.tag == ARRAY) {
                if (arg.array_val != null && arg.array_val.length > 0) {
                    *ptr_slot = (void*)arg.array_val.items;
                } else {
                    *ptr_slot = null;
                }
            } else {
                return ffi_arg_type_error(interp, arg_index, interp.sym_Array, arg);
            }
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)ptr_slot;
            return null;
        case FFI_TYPE_STRUCT:
            return ffi_raise(interp, "ffi/unsupported", "ffi: struct by-value parameters are not supported yet");
        default:
            return ffi_raise(interp, "ffi/invalid-state", "ffi: internal error — unsupported parameter ABI type");
    }
}

fn Value* ffi_pack_variadic_call_arg(
    Value* arg,
    usz arg_index,
    long* int_slot,
    double* dbl_slot,
    void** ptr_slot,
    int* out_arg_type,
    void** out_arg_value,
    Interp* interp
) {
    if (arg != null && arg.tag == INT) {
        *int_slot = arg.int_val;
        *out_arg_type = (int)FfiTypeTag.FFI_TYPE_INT;
        *out_arg_value = (void*)int_slot;
    } else if (arg != null && arg.tag == DOUBLE) {
        *dbl_slot = arg.double_val;
        *out_arg_type = (int)FfiTypeTag.FFI_TYPE_DOUBLE;
        *out_arg_value = (void*)dbl_slot;
    } else if (arg != null && arg.tag == FLOAT32) {
        *(float*)int_slot = arg.float32_val;
        *out_arg_type = (int)FfiTypeTag.FFI_TYPE_FLOAT32;
        *out_arg_value = (void*)int_slot;
    } else if (arg != null && arg.tag == STRING) {
        *ptr_slot = (void*)arg.str_chars;
        *out_arg_type = (int)FfiTypeTag.FFI_TYPE_STRING;
        *out_arg_value = (void*)ptr_slot;
    } else if (arg != null && arg.tag == FFI_HANDLE) {
        if (arg.ffi_val == null || arg.ffi_val.lib_handle == null) {
            return ffi_null_handle_error(interp, arg_index);
        }
        *ptr_slot = arg.ffi_val.lib_handle;
        *out_arg_type = (int)FfiTypeTag.FFI_TYPE_PTR;
        *out_arg_value = (void*)ptr_slot;
    } else if (arg != null && arg.tag == SYMBOL) {
        *int_slot = (arg.sym_val == interp.sym_true) ? 1 : 0;
        *out_arg_type = (int)FfiTypeTag.FFI_TYPE_BOOL;
        *out_arg_value = (void*)int_slot;
    } else {
        *ptr_slot = null;
        *out_arg_type = (int)FfiTypeTag.FFI_TYPE_PTR;
        *out_arg_value = (void*)ptr_slot;
    }
    return null;
}

fn void* ffi_return_storage_for(FfiTypeTag return_type, long* ret_int, float* ret_f32, double* ret_dbl, void** ret_ptr) {
    switch (return_type) {
        case FFI_TYPE_INT:
        case FFI_TYPE_BOOL:
        case FFI_TYPE_INT32:
        case FFI_TYPE_INT64:
        case FFI_TYPE_UINT32:
        case FFI_TYPE_UINT64:
            return (void*)ret_int;
        case FFI_TYPE_FLOAT32:
            return (void*)ret_f32;
        case FFI_TYPE_DOUBLE:
            return (void*)ret_dbl;
        case FFI_TYPE_PTR:
        case FFI_TYPE_STRING:
        case FFI_TYPE_BUFFER:
            return (void*)ret_ptr;
        case FFI_TYPE_VOID:
            return (void*)ret_int;  // unused but must be valid
        default:
            return (void*)ret_int;
    }
}

fn usz ffi_c_string_len(char* text) @inline {
    if (text == null) return 0;
    usz len = 0;
    while (text[len] != 0) len++;
    return len;
}

fn Value* ffi_return_value_for(FfiBoundFn* bound, FfiTypeTag return_type, FfiHandlePolicy* policy,
                               long ret_int, float ret_f32, double ret_dbl, void* ret_ptr, Interp* interp) {
    switch (return_type) {
        case FFI_TYPE_INT:
            return make_int(interp, ret_int);
        case FFI_TYPE_BOOL:
            if (ret_int != 0) return make_symbol(interp, interp.sym_true);
            return make_symbol(interp, interp.sym_false);
        case FFI_TYPE_DOUBLE:
            return make_double(interp, ret_dbl);
        case FFI_TYPE_PTR:
            if (ret_ptr == null) {
                if (policy != null && policy.is_foreign_handle
                    && policy.nullability == FFI_HANDLE_NON_NULL) {
                    return ffi_raise(interp, "ffi/type-mismatch", "ffi: return expected non-null ForeignHandle");
                }
                return make_nil(interp);
            }
            char[] handle_name = "ffi-foreign-handle";
            FfiHandleFinalizer finalizer = null;
            FfiHandleOwnership ownership = FFI_HANDLE_BORROWED;
            if (policy != null && policy.is_foreign_handle) {
                if (policy.name != INVALID_SYMBOL_ID) handle_name = interp.symbols.get_name(policy.name);
                ownership = policy.ownership;
                if (ownership == FFI_HANDLE_OWNED) finalizer = policy.finalizer;
            }
            uint capabilities = ownership == FFI_HANDLE_OWNED ? FOREIGN_CAP_RELEASE : 0;
            ForeignRuntimeKind return_runtime = foreign_runtime_bound_kind_for_runtime_call(bound);
            Value* handle_v = make_ffi_handle_ex_with_descriptor(
                interp,
                ret_ptr,
                handle_name,
                finalizer,
                false,
                return_runtime,
                FOREIGN_HANDLE_OPAQUE,
                capabilities,
                false,
                ownership);
            if (handle_v == null) {
                return ffi_raise(interp, "ffi/out-of-memory", "ffi: out of memory while wrapping return ForeignHandle");
            }
            return handle_v;
        case FFI_TYPE_STRING:
            if (ret_ptr == null) return make_nil(interp);
            char* ret_text = (char*)ret_ptr;
            return make_string(interp, ret_text[:ffi_c_string_len(ret_text)]);
        case FFI_TYPE_FLOAT32:
            return make_float32(interp, ret_f32);
        case FFI_TYPE_INT32:
        case FFI_TYPE_INT64:
        case FFI_TYPE_UINT32:
        case FFI_TYPE_UINT64:
            return make_int(interp, ret_int);
        case FFI_TYPE_BUFFER:
            if (ret_ptr == null) return make_nil(interp);
            return make_ffi_handle_ex(interp, ret_ptr, "ffi-buffer", null, false, false, null);
        case FFI_TYPE_STRUCT:
            if (ret_ptr == null) return make_nil(interp);
            return make_ffi_handle_ex(interp, ret_ptr, "ffi-struct", null, false, false, null);
        case FFI_TYPE_VOID:
            return make_void(interp);
        default:
            return make_nil(interp);
    }
}

fn Value* foreign_runtime_call_c_abi_bound(FfiBoundFn* bound, Value*[] args, Env* env, Interp* interp) {
    if (bound == null) return ffi_raise(interp, "ffi/invalid-state", "ffi: internal error — no bound function");
    if (bound.is_unavailable) return ffi_raise(interp, "ffi/invalid-state", "ffi: symbol unavailable");

    // Lazy dlsym: resolve on first call
    if (Value* err = ffi_resolve_bound_fn(bound, interp)) return err;
    if (bound.is_unavailable || bound.fn_ptr == null) return ffi_raise(interp, "ffi/invalid-state", "ffi: symbol unavailable");

    if (bound.param_count > (usz)int.max || args.len > (usz)int.max) {
        return ffi_raise(interp, "ffi/arity", "ffi: argument count exceeds supported arity");
    }

    // Validate arg count
    usz expected_min = bound.is_variadic ? bound.fixed_param_count : bound.param_count;
    if (!bound.is_variadic && args.len != bound.param_count) {
        char[256] ebuf;
        char[] msg;
        if (try formatted = io::bprintf(&ebuf, "ffi: expected %d args, got %d",
            (int)bound.param_count, (int)args.len)) {
            msg = formatted;
        } else {
            msg = "ffi: argument count mismatch";
        }
        return ffi_raise(interp, "ffi/arity", msg);
    }
    if (bound.is_variadic && args.len < expected_min) {
        char[256] ebuf;
        char[] msg;
        if (try formatted = io::bprintf(&ebuf, "ffi: variadic expected at least %d args, got %d",
            (int)expected_min, (int)args.len)) {
            msg = formatted;
        } else {
            msg = "ffi: variadic argument count mismatch";
        }
        return ffi_raise(interp, "ffi/arity", msg);
    }

    usz total_args = args.len;
    usz fixed_count = bound.is_variadic ? bound.fixed_param_count : total_args;

    // Prepare C argument storage
    long* int_store = null;
    double* dbl_store = null;
    void** ptr_store = null;
    int* arg_types = null;
    void** arg_values = null;

    if (total_args > 0) {
        if (total_args > usz.max / long.sizeof
            || total_args > usz.max / double.sizeof
            || total_args > usz.max / uptr.sizeof
            || total_args > usz.max / int.sizeof) {
            return ffi_raise(interp, "ffi/invalid-argument", "ffi: argument storage size overflow");
        }
        int_store = (long*)interp.current_scope.alloc(long.sizeof * total_args);
        dbl_store = (double*)interp.current_scope.alloc(double.sizeof * total_args);
        ptr_store = (void**)interp.current_scope.alloc(uptr.sizeof * total_args);
        arg_types = (int*)interp.current_scope.alloc(int.sizeof * total_args);
        arg_values = (void**)interp.current_scope.alloc(uptr.sizeof * total_args);
        if (int_store == null || dbl_store == null || ptr_store == null || arg_types == null || arg_values == null) {
            return ffi_raise(interp, "ffi/out-of-memory", "ffi: out of memory while preparing call arguments");
        }
    }

    for (usz i = 0; i < total_args; i++) {
        if (i < bound.param_count) {
            FfiHandlePolicy* policy = null;
            if (bound.param_handle_policies != null) policy = &bound.param_handle_policies[i];
            if (Value* err = ffi_pack_call_arg(args[i], bound.param_types[i], i,
                policy,
                &int_store[i], &dbl_store[i], &ptr_store[i],
                &arg_types[i], &arg_values[i], interp)) {
                return err;
            }
        } else {
            if (Value* err = ffi_pack_variadic_call_arg(
                args[i],
                i,
                &int_store[i],
                &dbl_store[i],
                &ptr_store[i],
                &arg_types[i],
                &arg_values[i],
                interp
            )) return err;
        }
    }

    // Call via libffi
    int ret_type = (int)bound.return_type;
    long ret_int = 0;
    float ret_f32 = 0.0f;
    double ret_dbl = 0.0;
    void* ret_ptr = null;
    void* ret_storage = ffi_return_storage_for(bound.return_type, &ret_int, &ret_f32, &ret_dbl, &ret_ptr);

    int rc;
    if (bound.is_variadic) {
        rc = omni_ffi_call_var(bound.fn_ptr, (int)total_args, (int)fixed_count,
                               arg_types, arg_values, ret_type, ret_storage);
    } else {
        rc = omni_ffi_call(bound.fn_ptr, (int)total_args,
                           arg_types, arg_values, ret_type, ret_storage);
    }
    if (rc != 0) {
        int saved_errno = *c_errno_location();
        char[256] ebuf;
        char[] msg;
        if (try formatted = io::bprintf(&ebuf, "ffi: libffi call failed (errno=%d)", saved_errno)) {
            msg = formatted;
        } else {
            msg = "ffi: libffi call failed";
        }
        return ffi_raise(interp, "ffi/call-failed", msg);
    }

    return ffi_return_value_for(bound, bound.return_type, &bound.return_handle_policy, ret_int, ret_f32, ret_dbl, ret_ptr, interp);
}

// prim_ffi_bound_call — primitive handler for bound FFI functions.
// user_data points to a heap-allocated FfiBoundFn.
<*
@require interp != null
*>
fn Value* prim_ffi_bound_call(Value*[] args, Env* env, Interp* interp) {
    FfiBoundFn* bound = (FfiBoundFn*)interp.prim_user_data;
    ForeignRuntimeAdapter adapter;
    if (!foreign_runtime_adapter_for_bound(bound, &adapter) ||
        !foreign_runtime_adapter_supports(&adapter, FOREIGN_CAP_CALL) ||
        adapter.call_bound == null) {
        return ffi_raise(interp, "ffi/unsupported", "ffi: runtime adapter does not support calls");
    }
    return adapter.call_bound(bound, args, env, interp);
}
