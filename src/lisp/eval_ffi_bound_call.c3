module lisp;

import std::io;

fn Value* ffi_unsupported_type_annotation_error(Interp* interp, char[] role, SymbolId base_type) {
    char[256] ebuf;
    char[] type_name = interp.symbols.get_name(base_type);
    char[] msg;
    if (try formatted = io::bprintf(&ebuf, "ffi λ: unsupported %s type annotation '^%s'",
        (ZString)role, (ZString)type_name)) {
        msg = formatted;
    } else {
        msg = "ffi λ: unsupported type annotation";
    }
    return raise_error(interp, msg);
}

fn Value* ffi_resolve_bound_fn(FfiBoundFn* bound, Interp* interp) {
    if (bound.is_unavailable) return raise_error(interp, "ffi: bound symbol is unavailable");
    if (bound.fn_ptr != null) return null;
    if (bound.lib_handle == null) {
        if (bound.lib_name == null || bound.lib_name_len == 0) {
            if (!bound.required_library) {
                bound.is_unavailable = true;
                return null;
            }
            return raise_error(interp, "ffi: internal error — missing library name");
        }
        (void)dlerror();
        bound.lib_handle = dlopen((ZString)bound.lib_name, RTLD_LAZY);
        if (bound.lib_handle == null) {
            char[256] ebuf;
            char* dl_err = dlerror();
            if (!bound.required_library) {
                bound.is_unavailable = true;
                return null;
            }
            if (dl_err != null && dl_err[0] != 0) {
                if (try msg = io::bprintf(&ebuf, "ffi: dlopen failed for '%s': %s",
                    (ZString)bound.lib_name, (ZString)dl_err)) {
                    return raise_error(interp, msg);
                }
                return raise_error(interp, "ffi: dlopen failed");
            }
            if (try msg = io::bprintf(&ebuf, "ffi: dlopen failed for '%s'",
                (ZString)bound.lib_name)) {
                return raise_error(interp, msg);
            }
            return raise_error(interp, "ffi: dlopen failed");
        }
    }
    if (bound.c_name == null) return raise_error(interp, "ffi: internal error — missing symbol name");
    (void)dlerror();
    bound.fn_ptr = dlsym(bound.lib_handle, (ZString)bound.c_name);
    if (bound.fn_ptr != null) return null;

    char[256] ebuf;
    char* dl_err = dlerror();
    if (!bound.required_symbol) {
        bound.is_unavailable = true;
        return null;
    }
    if (dl_err != null && dl_err[0] != 0) {
        if (try msg = io::bprintf(&ebuf, "ffi: dlsym failed for '%s': %s",
            (ZString)bound.c_name, (ZString)dl_err)) {
            return raise_error(interp, msg);
        }
        return raise_error(interp, "ffi: dlsym failed");
    }
    if (try msg = io::bprintf(&ebuf, "ffi: dlsym failed for '%s'",
        (ZString)bound.c_name)) {
        return raise_error(interp, msg);
    }
    return raise_error(interp, "ffi: dlsym failed");
}

fn Value* ffi_arg_type_error(Interp* interp, usz arg_index, SymbolId expected_type, Value* actual) {
    char[256] ebuf;
    char[] expected_name = interp.symbols.get_name(expected_type);
    char[] actual_name = interp.symbols.get_name(value_type_name(actual, interp));
    if (try char[] msg = io::bprintf(&ebuf, "ffi: argument %d expected %s, got %s",
        (int)(arg_index + 1), (ZString)expected_name, (ZString)actual_name)) {
        return raise_error(interp, msg);
    }
    return raise_error(interp, "ffi: argument type mismatch");
}

fn Value* ffi_null_handle_error(Interp* interp, usz arg_index) {
    char[256] ebuf;
    if (try char[] msg = io::bprintf(&ebuf, "ffi: argument %d expected live Pointer handle",
        (int)(arg_index + 1))) {
        return raise_error(interp, msg);
    }
    return raise_error(interp, "ffi: expected live Pointer handle");
}

fn Value* ffi_pack_call_arg(Value* arg, FfiTypeTag param_type, usz arg_index,
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
                *ptr_slot = null;
            } else if (arg.tag == INT) {
                *ptr_slot = (void*)(uptr)arg.int_val;
            } else if (arg.tag == FFI_HANDLE) {
                if (arg.ffi_val == null || arg.ffi_val.lib_handle == null) {
                    return ffi_null_handle_error(interp, arg_index);
                }
                *ptr_slot = arg.ffi_val.lib_handle;
            } else {
                return ffi_arg_type_error(interp, arg_index, interp.sym_Pointer, arg);
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
        default:
            return raise_error(interp, "ffi: internal error — unsupported parameter ABI type");
    }
}

fn void* ffi_return_storage_for(FfiTypeTag return_type, long* ret_int, double* ret_dbl, void** ret_ptr) {
    switch (return_type) {
        case FFI_TYPE_INT:
        case FFI_TYPE_BOOL:
            return (void*)ret_int;
        case FFI_TYPE_DOUBLE:
            return (void*)ret_dbl;
        case FFI_TYPE_PTR:
        case FFI_TYPE_STRING:
            return (void*)ret_ptr;
        case FFI_TYPE_VOID:
            return (void*)ret_int;  // unused but must be valid
        default:
            return (void*)ret_int;
    }
}

fn Value* ffi_return_value_for(FfiTypeTag return_type, long ret_int, double ret_dbl, void* ret_ptr, Interp* interp) {
    switch (return_type) {
        case FFI_TYPE_INT:
            return make_int(interp, ret_int);
        case FFI_TYPE_BOOL:
            if (ret_int != 0) return make_symbol(interp, interp.sym_true);
            return make_symbol(interp, interp.sym_false);
        case FFI_TYPE_DOUBLE:
            return make_double(interp, ret_dbl);
        case FFI_TYPE_PTR:
        case FFI_TYPE_STRING:
            if (ret_ptr == null) return make_nil(interp);
            return make_int(interp, (long)(uptr)ret_ptr);
        case FFI_TYPE_VOID:
            return make_void(interp);
        default:
            return make_nil(interp);
    }
}

// prim_ffi_bound_call — primitive handler for bound FFI functions.
// user_data points to a heap-allocated FfiBoundFn.
fn Value* prim_ffi_bound_call(Value*[] args, Env* env, Interp* interp) {
    FfiBoundFn* bound = (FfiBoundFn*)interp.prim_user_data;
    if (bound == null) return raise_error(interp, "ffi: internal error — no bound function");
    if (bound.is_variadic) return raise_error(interp, "ffi: internal error — variadic FFI binding should have been rejected");
    if (bound.is_unavailable) return raise_error(interp, "ffi: symbol unavailable");

    // Lazy dlsym: resolve on first call
    if (Value* err = ffi_resolve_bound_fn(bound, interp)) return err;
    if (bound.is_unavailable || bound.fn_ptr == null) return raise_error(interp, "ffi: symbol unavailable");

    if (bound.param_count > (usz)int.max || args.len > (usz)int.max) {
        return raise_error(interp, "ffi: argument count exceeds supported arity");
    }

    // Validate arg count
    if (args.len != bound.param_count) {
        char[256] ebuf;
        char[] msg;
        if (try formatted = io::bprintf(&ebuf, "ffi: expected %d args, got %d",
            (int)bound.param_count, (int)args.len)) {
            msg = formatted;
        } else {
            msg = "ffi: argument count mismatch";
        }
        return raise_error(interp, msg);
    }

    // Prepare C argument storage
    long* int_store = null;
    double* dbl_store = null;
    void** ptr_store = null;
    int* arg_types = null;
    void** arg_values = null;

    if (bound.param_count > 0) {
        if (bound.param_count > usz.max / long.sizeof
            || bound.param_count > usz.max / double.sizeof
            || bound.param_count > usz.max / uptr.sizeof
            || bound.param_count > usz.max / int.sizeof) {
            return raise_error(interp, "ffi: argument storage size overflow");
        }
        int_store = (long*)interp.current_scope.alloc(long.sizeof * bound.param_count);
        dbl_store = (double*)interp.current_scope.alloc(double.sizeof * bound.param_count);
        ptr_store = (void**)interp.current_scope.alloc(uptr.sizeof * bound.param_count);
        arg_types = (int*)interp.current_scope.alloc(int.sizeof * bound.param_count);
        arg_values = (void**)interp.current_scope.alloc(uptr.sizeof * bound.param_count);
        if (int_store == null || dbl_store == null || ptr_store == null || arg_types == null || arg_values == null) {
            return raise_error(interp, "ffi: out of memory while preparing call arguments");
        }
    }

    for (usz i = 0; i < bound.param_count; i++) {
        if (Value* err = ffi_pack_call_arg(args[i], bound.param_types[i], i,
            &int_store[i], &dbl_store[i], &ptr_store[i],
            &arg_types[i], &arg_values[i], interp)) {
            return err;
        }
    }

    // Call via libffi
    int ret_type = (int)bound.return_type;
    long ret_int = 0;
    double ret_dbl = 0.0;
    void* ret_ptr = null;
    void* ret_storage = ffi_return_storage_for(bound.return_type, &ret_int, &ret_dbl, &ret_ptr);

    int rc = omni_ffi_call(bound.fn_ptr, (int)bound.param_count,
                           arg_types, arg_values, ret_type, ret_storage);
    if (rc != 0) return raise_error(interp, "ffi: libffi call failed");

    return ffi_return_value_for(bound.return_type, ret_int, ret_dbl, ret_ptr, interp);
}
