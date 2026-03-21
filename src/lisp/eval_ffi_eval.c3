module lisp;

import std::io;

// =============================================================================
// LIBDL DECLARATIONS (used by declarative FFI: define [ffi lib] / define [ffi λ])
// =============================================================================

extern fn void* dlopen(char* filename, int flags) @extern("dlopen");
extern fn void* dlsym(void* handle, char* symbol) @extern("dlsym");
extern fn int dlclose(void* handle) @extern("dlclose");
extern fn char* dlerror() @extern("dlerror");
const int RTLD_LAZY = 1;

// =============================================================================
// SECTION 2.3.1: FFI EVALUATION (define [ffi lib], define [ffi λ])
// =============================================================================

// C helper from csrc/ffi_helpers.c
extern fn int omni_ffi_call(void* fn_ptr, int nargs, int* arg_types, void** arg_values,
                            int ret_type, void* ret_value) @extern("omni_ffi_call");

// eval_ffi_lib — (define [ffi lib] name "path.so")
fn Value* eval_ffi_lib(Expr* expr, Env* env, Interp* interp) {
    ExprFfiLib* fl = &expr.ffi_lib;

    // Evaluate path expression
    Value* path_val = runtime_eval_expr(fl.path_expr, env, interp);
    if (path_val == null || path_val.tag == ERROR) return path_val;
    if (path_val.tag != STRING) return raise_error(interp, "ffi lib: path must be a string");

    // dlopen
    void* handle = dlopen((ZString)path_val.str_chars, RTLD_LAZY);
    if (handle == null) {
        char[256] ebuf;
        char[] msg = io::bprintf(&ebuf, "ffi lib: dlopen failed for '%s'",
            (ZString)path_val.str_chars)!!;
        return raise_error(interp, msg);
    }

    // Create FFI handle value and bind in global env
    Value* ffi_handle = make_ffi_handle(interp, handle, path_val.str_chars[:path_val.str_len]);
    env_define_with_barrier(interp.global_env, fl.name, ffi_handle, interp);
    return ffi_handle;
}

fn Value* ffi_resolve_bound_fn(FfiBoundFn* bound, Interp* interp) {
    if (bound.fn_ptr != null) return null;
    if (bound.c_name == null) return raise_error(interp, "ffi: internal error — missing symbol name");
    bound.fn_ptr = dlsym(bound.lib_handle, (ZString)bound.c_name);
    if (bound.fn_ptr != null) return null;

    char[256] ebuf;
    char[] msg = io::bprintf(&ebuf, "ffi: dlsym failed for '%s'",
        (ZString)bound.c_name)!!;
    return raise_error(interp, msg);
}

fn void ffi_pack_call_arg(Value* arg, FfiTypeTag param_type,
                          long* int_slot, double* dbl_slot, void** ptr_slot,
                          int* out_arg_type, void** out_arg_value,
                          Interp* interp) {
    switch (param_type) {
        case FFI_TYPE_INT:
        case FFI_TYPE_BOOL:
            if (arg.tag == INT) {
                *int_slot = arg.int_val;
            } else if (arg.tag == DOUBLE) {
                *int_slot = (long)arg.double_val;
            } else if (!is_falsy(arg, interp)) {
                *int_slot = 1;
            } else {
                *int_slot = 0;
            }
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)int_slot;
            return;
        case FFI_TYPE_DOUBLE:
            if (arg.tag == DOUBLE) {
                *dbl_slot = arg.double_val;
            } else if (arg.tag == INT) {
                *dbl_slot = (double)arg.int_val;
            } else {
                *dbl_slot = 0.0;
            }
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)dbl_slot;
            return;
        case FFI_TYPE_PTR:
            if (arg.tag == STRING) {
                *ptr_slot = (void*)arg.str_chars;
            } else if (arg.tag == INT) {
                *ptr_slot = (void*)(uptr)arg.int_val;
            } else if (arg.tag == NIL) {
                *ptr_slot = null;
            } else if (arg.tag == FFI_HANDLE) {
                *ptr_slot = arg.ffi_val.lib_handle;
            } else {
                *ptr_slot = (void*)(uptr)arg.int_val;
            }
            *out_arg_type = (int)param_type;
            *out_arg_value = (void*)ptr_slot;
            return;
        default:
            *out_arg_type = (int)FfiTypeTag.FFI_TYPE_PTR;
            *ptr_slot = null;
            *out_arg_value = (void*)ptr_slot;
            return;
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

    // Lazy dlsym: resolve on first call
    if (Value* err = ffi_resolve_bound_fn(bound, interp)) return err;

    // Validate arg count
    if (args.len < bound.param_count) {
        char[256] ebuf;
        char[] msg = io::bprintf(&ebuf, "ffi: expected %d args, got %d",
            (int)bound.param_count, (int)args.len)!!;
        return raise_error(interp, msg);
    }

    // Prepare C argument storage
    long* int_store = null;
    double* dbl_store = null;
    void** ptr_store = null;
    int* arg_types = null;
    void** arg_values = null;

    if (bound.param_count > 0) {
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
        ffi_pack_call_arg(args[i], bound.param_types[i],
            &int_store[i], &dbl_store[i], &ptr_store[i],
            &arg_types[i], &arg_values[i], interp);
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

// eval_ffi_fn — (define [ffi λ libname] (fname (^T arg)...) ^RetT)
fn Value* eval_ffi_fn(Expr* expr, Env* env, Interp* interp) {
    ExprFfiFn* ff = expr.ffi_fn;

    // Look up library handle
    Value* lib_val = env.lookup(ff.lib_name);
    if (lib_val == null) lib_val = interp.global_env.lookup(ff.lib_name);
    if (lib_val == null || lib_val.tag != FFI_HANDLE) {
        char[256] ebuf;
        char[] msg = io::bprintf(&ebuf, "ffi λ: library '%s' not found",
            (ZString)interp.symbols.get_name(ff.lib_name))!!;
        return raise_error(interp, msg);
    }
    if (lib_val.ffi_val.lib_handle == null) {
        return raise_error(interp, "ffi λ: library handle is closed");
    }

    // Build FfiBoundFn — dlsym is deferred to first call (lazy resolution)
    FfiBoundFn* bound = (FfiBoundFn*)mem::malloc(FfiBoundFn.sizeof);
    if (bound == null) {
        return raise_error(interp, "ffi λ: out of memory for bound function");
    }
    bound.fn_ptr = null;  // resolved lazily on first call
    bound.lib_handle = lib_val.ffi_val.lib_handle;
    if (ff.c_name == null) {
        mem::free(bound);
        return raise_error(interp, "ffi λ: missing C symbol name");
    }
    if (ff.c_name_len == usz.max) {
        mem::free(bound);
        return raise_error(interp, "ffi λ: C symbol name is too long");
    }
    bound.c_name = (char*)mem::malloc(ff.c_name_len + 1);
    if (bound.c_name == null) {
        mem::free(bound);
        return raise_error(interp, "ffi λ: out of memory for symbol name");
    }
    for (usz i = 0; i < ff.c_name_len; i++) {
        bound.c_name[i] = ff.c_name[i];
    }
    bound.c_name[ff.c_name_len] = 0;
    bound.c_name_len = ff.c_name_len;
    bound.param_count = ff.param_count;
    bound.param_types = null;
    bound.is_variadic = ff.is_variadic;

    if (ff.param_count > 0) {
        bound.param_types = (FfiTypeTag*)mem::malloc(FfiTypeTag.sizeof * ff.param_count);
        if (bound.param_types == null) {
            mem::free(bound.c_name);
            mem::free(bound);
            return raise_error(interp, "ffi λ: out of memory for parameter metadata");
        }
        for (usz i = 0; i < ff.param_count; i++) {
            bound.param_types[i] = type_ann_to_ffi_tag(ff.param_types[i].base_type, interp);
        }
    }

    if (ff.has_return_type) {
        bound.return_type = type_ann_to_ffi_tag(ff.return_type.base_type, interp);
        bound.has_return = true;
    } else {
        bound.return_type = FFI_TYPE_VOID;
        bound.has_return = false;
    }

    // Create a primitive value with user_data pointing to bound
    Value* prim = make_primitive(interp,
        interp.symbols.get_name(ff.fn_name),
        &prim_ffi_bound_call,
        (int)ff.param_count);
    prim.prim_val.user_data = (void*)bound;

    // Bind in global env
    env_define_with_barrier(interp.global_env, ff.fn_name, prim, interp);
    return prim;
}
