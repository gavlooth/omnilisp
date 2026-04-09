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
    (void)dlerror();
    void* handle = dlopen((ZString)path_val.str_chars, RTLD_LAZY);
    if (handle == null) {
        char[256] ebuf;
        char* dl_err = dlerror();
        char[] msg;
        if (dl_err != null && dl_err[0] != 0) {
            if (try formatted = io::bprintf(&ebuf, "ffi lib: dlopen failed for '%s': %s",
                    (ZString)path_val.str_chars, (ZString)dl_err)) {
                msg = formatted;
            } else {
                msg = "ffi lib: dlopen failed (error formatting failed)";
            }
        } else if (try formatted = io::bprintf(&ebuf, "ffi lib: dlopen failed for '%s'",
                (ZString)path_val.str_chars)) {
            msg = formatted;
        } else {
            msg = "ffi lib: dlopen failed";
        }
        return raise_error(interp, msg);
    }

    // Create FFI handle value and bind in global env
    Value* ffi_handle = make_ffi_handle(interp, handle, path_val.str_chars[:path_val.str_len]);
    if (ffi_handle == null) {
        dlclose(handle);
        return raise_error(interp, "ffi lib: out of memory");
    }
    if (!env_define_with_barrier_checked(interp.global_env, fl.name, ffi_handle, interp)) {
        dlclose(handle);
        return raise_error(interp, "ffi lib: out of memory");
    }
    return ffi_handle;
}


// eval_ffi_fn — (define [ffi λ libname] (fname (^T arg)...) ^RetT)
fn Value* eval_ffi_fn(Expr* expr, Env* env, Interp* interp) {
    ExprFfiFn* ff = expr.ffi_fn;
    if (ff.is_variadic) {
        return raise_error(interp, "ffi λ: variadic bindings are not supported yet");
    }

    // Look up library handle
    Value* lib_val = env.lookup(ff.lib_name);
    if (lib_val == null) lib_val = interp.global_env.lookup(ff.lib_name);
    if (lib_val == null || lib_val.tag != FFI_HANDLE) {
        char[256] ebuf;
        char[] msg;
        if (try formatted = io::bprintf(&ebuf, "ffi λ: library '%s' not found",
                (ZString)interp.symbols.get_name(ff.lib_name))) {
            msg = formatted;
        } else {
            msg = "ffi λ: library not found";
        }
        return raise_error(interp, msg);
    }
    if (lib_val.ffi_val.lib_handle == null) {
        return raise_error(interp, "ffi λ: library handle is closed");
    }

    // Build FfiBoundFn — dlsym is deferred to first call (lazy resolution)
    FfiBoundFn* bound = (FfiBoundFn*)interp.root_scope.alloc(FfiBoundFn.sizeof);
    if (bound == null) {
        return raise_error(interp, "ffi λ: out of memory for bound function");
    }
    bound.fn_ptr = null;  // resolved lazily on first call
    bound.lib_handle = lib_val.ffi_val.lib_handle;
    bound.required_library = true;
    bound.required_symbol = true;
    bound.is_unavailable = false;
    if (lib_val.ffi_val.name_len > 0) {
        if (lib_val.ffi_val.name_len == usz.max) {
            return raise_error(interp, "ffi λ: library name is too long");
        }
        bound.lib_name = (char*)interp.root_scope.alloc(lib_val.ffi_val.name_len + 1);
        if (bound.lib_name == null) {
            return raise_error(interp, "ffi λ: out of memory for library name");
        }
        for (usz i = 0; i < lib_val.ffi_val.name_len; i++) {
            bound.lib_name[i] = lib_val.ffi_val.lib_name[i];
        }
        bound.lib_name[lib_val.ffi_val.name_len] = 0;
        bound.lib_name_len = lib_val.ffi_val.name_len;
    } else {
        bound.lib_name = null;
        bound.lib_name_len = 0;
    }
    if (ff.c_name == null) {
        return raise_error(interp, "ffi λ: missing C symbol name");
    }
    if (ff.c_name_len == usz.max) {
        return raise_error(interp, "ffi λ: C symbol name is too long");
    }
    bound.c_name = (char*)interp.root_scope.alloc(ff.c_name_len + 1);
    if (bound.c_name == null) {
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
        bound.param_types = (FfiTypeTag*)interp.root_scope.alloc(FfiTypeTag.sizeof * ff.param_count);
        if (bound.param_types == null) {
            return raise_error(interp, "ffi λ: out of memory for parameter metadata");
        }
        for (usz i = 0; i < ff.param_count; i++) {
            if (!type_ann_to_ffi_tag(ff.param_types[i].base_type, interp, &bound.param_types[i])) {
                return ffi_unsupported_type_annotation_error(interp, "parameter", ff.param_types[i].base_type);
            }
        }
    }

    if (ff.has_return_type) {
        if (!type_ann_to_ffi_tag(ff.return_type.base_type, interp, &bound.return_type)) {
            return ffi_unsupported_type_annotation_error(interp, "return", ff.return_type.base_type);
        }
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
    if (prim == null) {
        return raise_error(interp, "ffi λ: out of memory for bound function primitive");
    }
    prim.prim_val.user_data = (void*)bound;

    // Bind in global env
    if (!env_define_with_barrier_checked(interp.global_env, ff.fn_name, prim, interp)) {
        return raise_error(interp, "ffi λ: out of memory while registering bound function");
    }
    return prim;
}
