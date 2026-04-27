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
extern fn int omni_ffi_call_var(void* fn_ptr, int nargs, int fixed_count,
                                int* arg_types, void** arg_values,
                                int ret_type, void* ret_value) @extern("omni_ffi_call_var");

// eval_ffi_lib — (define [ffi lib] name "path.so")
fn Value* eval_ffi_lib(Expr* expr, Env* env, Interp* interp) {
    if (interp == null) return null;
    if (interp.global_env == null) {
        return ffi_raise(interp, "ffi/invalid-state", "ffi lib: global environment is unavailable");
    }

    ExprFfiLib* fl = &expr.ffi_lib;

    // Evaluate path expression
    Value* path_val = runtime_eval_expr(fl.path_expr, env, interp);
    if (path_val == null || path_val.tag == ERROR) return path_val;
    if (path_val.tag != STRING) return ffi_raise(interp, "ffi/invalid-argument", "ffi lib: path must be a string");
    if (path_val.str_len == 0 || path_val.str_len >= FFI_DLOPEN_REGISTRY_PATH_CAP) {
        return ffi_raise(interp, "ffi/invalid-argument", "ffi lib: path must be a non-empty string shorter than 256 bytes");
    }
    CApiStringResult c_path = c_api_string_dup_no_nul(path_val.str_chars[:path_val.str_len], FFI_DLOPEN_REGISTRY_PATH_CAP - 1);
    if (c_path.contains_nul) {
        return ffi_raise(interp, "ffi/invalid-argument", "ffi lib: path contains embedded NUL byte");
    }
    if (!c_path.ok) {
        return ffi_raise(interp, "ffi/out-of-memory", "ffi lib: out of memory");
    }
    defer c_api_string_free(&c_path);

    // Acquire shared dlopen handle via registry
    (void)dlerror();
    void* handle = ffi_dlopen_registry_acquire(path_val.str_chars[:path_val.str_len]);
    if (handle == null) {
        int saved_errno = *c_errno_location();
        char[256] ebuf;
        char* dl_err = dlerror();
        char[] msg;
        if (dl_err != null && dl_err[0] != 0) {
            if (try formatted = io::bprintf(&ebuf, "ffi lib: dlopen failed for '%s': %s (errno=%d)",
                    c_path.ptr, (ZString)dl_err, saved_errno)) {
                msg = formatted;
            } else {
                msg = "ffi lib: dlopen failed (error formatting failed)";
            }
        } else if (try formatted = io::bprintf(&ebuf, "ffi lib: dlopen failed for '%s' (errno=%d)",
                c_path.ptr, saved_errno)) {
            msg = formatted;
        } else {
            msg = "ffi lib: dlopen failed";
        }
        return ffi_raise(interp, "ffi/library-load-failed", msg);
    }

    // Create FFI handle value and bind in global env
    Value* ffi_handle = make_ffi_handle(interp, handle, path_val.str_chars[:path_val.str_len]);
    if (ffi_handle == null) {
        // make_ffi_handle may have already released the box (and thus registry ref)
        // via ffi_handle_release; do not call registry_release again to avoid
        // double-decrement. Any remaining refcount is cleaned up at teardown.
        return ffi_raise(interp, "ffi/out-of-memory", "ffi lib: out of memory");
    }
    if (!env_define_with_barrier_checked(interp.global_env, fl.name, ffi_handle, interp)) {
        FfiHandle* box = ffi_handle.ffi_val;
        ffi_handle.ffi_val = null;
        ffi_handle_release(box);  // releases registry ref via ffi_handle_release_payload
        return ffi_raise(interp, "ffi/out-of-memory", "ffi lib: out of memory");
    }
    return ffi_handle;
}

fn bool ffi_symbol_to_c_symbol_name(Interp* interp, SymbolId sym, char* out_name, usz out_cap, usz* out_len) {
    if (sym == INVALID_SYMBOL_ID || out_name == null || out_cap == 0) return false;
    char[] symbol_name = interp.symbols.get_name(sym);
    if (symbol_name.len == 0 || symbol_name.len >= out_cap) return false;
    for (usz i = 0; i < symbol_name.len; i++) {
        out_name[i] = symbol_name[i] == '-' ? '_' : symbol_name[i];
    }
    out_name[symbol_name.len] = 0;
    *out_len = symbol_name.len;
    return true;
}

fn Value* ffi_resolve_return_handle_finalizer(FfiBoundFn* bound, FfiHandlePolicy* policy, Interp* interp) {
    if (policy == null || !policy.is_foreign_handle || policy.ownership != FFI_HANDLE_OWNED) return null;
    if (policy.finalizer_symbol == INVALID_SYMBOL_ID) {
        return ffi_raise(interp, "ffi/invalid-argument", "ffi λ: owned ForeignHandle return requires a finalizer");
    }
    if (bound == null || bound.lib_handle == null) {
        return ffi_raise(interp, "ffi/invalid-state", "ffi λ: cannot resolve ForeignHandle finalizer without a library handle");
    }

    char[256] finalizer_name;
    usz finalizer_len = 0;
    if (!ffi_symbol_to_c_symbol_name(interp, policy.finalizer_symbol, &finalizer_name, 256, &finalizer_len)) {
        return ffi_raise(interp, "ffi/invalid-argument", "ffi λ: invalid ForeignHandle finalizer symbol");
    }

    (void)dlerror();
    void* finalizer_ptr = dlsym(bound.lib_handle, (ZString)&finalizer_name);
    if (finalizer_ptr == null) {
        int saved_errno = *c_errno_location();
        char[256] ebuf;
        char* dl_err = dlerror();
        char[] finalizer_source_name = interp.symbols.get_name(policy.finalizer_symbol);
        if (dl_err != null && dl_err[0] != 0) {
            if (try msg = io::bprintf(&ebuf, "ffi λ: dlsym failed for ForeignHandle finalizer '%s': %s (errno=%d)",
                (ZString)finalizer_source_name, (ZString)dl_err, saved_errno)) {
                return ffi_raise(interp, "ffi/symbol-not-found", msg);
            }
            return ffi_raise(interp, "ffi/symbol-not-found", "ffi λ: dlsym failed for ForeignHandle finalizer");
        }
        if (try msg = io::bprintf(&ebuf, "ffi λ: dlsym failed for ForeignHandle finalizer '%s' (errno=%d)",
            (ZString)finalizer_source_name, saved_errno)) {
            return ffi_raise(interp, "ffi/symbol-not-found", msg);
        }
        return ffi_raise(interp, "ffi/symbol-not-found", "ffi λ: dlsym failed for ForeignHandle finalizer");
    }

    policy.finalizer = (FfiHandleFinalizer)finalizer_ptr;
    return null;
}


// eval_ffi_fn — (define [ffi λ libname] (fname (^T arg)...) ^RetT)
fn Value* eval_ffi_fn(Expr* expr, Env* env, Interp* interp) {
    ExprFfiFn* ff = expr.ffi_fn;

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
        return ffi_raise(interp, "ffi/library-not-found", msg);
    }
    if (lib_val.ffi_val.lib_handle == null) {
        return ffi_raise(interp, "ffi/invalid-state", "ffi λ: library handle is closed");
    }

    // Build FfiBoundFn — dlsym is deferred to first call (lazy resolution)
    FfiBoundFn* bound = (FfiBoundFn*)interp.root_scope.alloc(FfiBoundFn.sizeof);
    if (bound == null) {
        return ffi_raise(interp, "ffi/out-of-memory", "ffi λ: out of memory for bound function");
    }
    bound.fn_ptr = null;  // resolved lazily on first call
    bound.lib_handle = lib_val.ffi_val.lib_handle;
    bound.required_library = true;
    bound.required_symbol = true;
    bound.is_unavailable = false;
    if (lib_val.ffi_val.name_len > 0) {
        if (lib_val.ffi_val.name_len == usz.max) {
            return ffi_raise(interp, "ffi/invalid-argument", "ffi λ: library name is too long");
        }
        bound.lib_name = (char*)interp.root_scope.alloc(lib_val.ffi_val.name_len + 1);
        if (bound.lib_name == null) {
            return ffi_raise(interp, "ffi/out-of-memory", "ffi λ: out of memory for library name");
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
        return ffi_raise(interp, "ffi/invalid-argument", "ffi λ: missing C symbol name");
    }
    if (ff.c_name_len == usz.max) {
        return ffi_raise(interp, "ffi/invalid-argument", "ffi λ: C symbol name is too long");
    }
    bound.c_name = (char*)interp.root_scope.alloc(ff.c_name_len + 1);
    if (bound.c_name == null) {
        return ffi_raise(interp, "ffi/out-of-memory", "ffi λ: out of memory for symbol name");
    }
    for (usz i = 0; i < ff.c_name_len; i++) {
        bound.c_name[i] = ff.c_name[i];
    }
    bound.c_name[ff.c_name_len] = 0;
    bound.c_name_len = ff.c_name_len;
    bound.param_count = ff.param_count;
    bound.fixed_param_count = ff.param_count;
    bound.param_types = null;
    bound.param_handle_policies = null;
    bound.return_handle_policy = ffi_handle_policy_default();
    bound.is_variadic = ff.is_variadic;
    bound.runtime_kind = lib_val.ffi_val.runtime_kind;

    if (ff.param_count > (usz)int.max) {
        return ffi_raise(interp, "ffi/arity", "ffi λ: parameter count exceeds supported arity");
    }
    if (ff.param_count > 0) {
        if (ff.param_count > usz.max / FfiTypeTag.sizeof ||
            ff.param_count > usz.max / FfiHandlePolicy.sizeof) {
            return ffi_raise(interp, "ffi/invalid-argument", "ffi λ: parameter metadata size overflow");
        }
        bound.param_types = (FfiTypeTag*)interp.root_scope.alloc(FfiTypeTag.sizeof * ff.param_count);
        bound.param_handle_policies = (FfiHandlePolicy*)interp.root_scope.alloc(FfiHandlePolicy.sizeof * ff.param_count);
        if (bound.param_types == null || bound.param_handle_policies == null) {
            return ffi_raise(interp, "ffi/out-of-memory", "ffi λ: out of memory for parameter metadata");
        }
        for (usz i = 0; i < ff.param_count; i++) {
            if (!ffi_type_annotation_to_tag(&ff.param_types[i], interp,
                &bound.param_types[i], &bound.param_handle_policies[i])) {
                return ffi_unsupported_type_annotation_error(interp, "parameter", &ff.param_types[i]);
            }
            if (bound.param_types[i] == FFI_TYPE_VOID) {
                return ffi_raise(interp, "ffi/unsupported", "ffi λ: unsupported parameter ABI type '^Void'");
            }
            if (bound.param_handle_policies[i].ownership == FFI_HANDLE_OWNED) {
                return ffi_raise(interp, "ffi/unsupported", "ffi λ: owned ForeignHandle parameter policy is not supported");
            }
        }
    }

    if (ff.has_return_type) {
        if (!ffi_type_annotation_to_tag(&ff.return_type, interp, &bound.return_type, &bound.return_handle_policy)) {
            return ffi_unsupported_type_annotation_error(interp, "return", &ff.return_type);
        }
        if (Value* err = ffi_resolve_return_handle_finalizer(bound, &bound.return_handle_policy, interp)) return err;
        bound.has_return = true;
    } else {
        bound.return_type = FFI_TYPE_VOID;
        bound.has_return = false;
    }

    // Create a primitive value with user_data pointing to bound
    int prim_arity = ff.is_variadic ? -1 : (int)ff.param_count;
    Value* prim = make_primitive(interp,
        interp.symbols.get_name(ff.fn_name),
        &prim_ffi_bound_call,
        prim_arity);
    if (prim == null) {
        return ffi_raise(interp, "ffi/out-of-memory", "ffi λ: out of memory for bound function primitive");
    }
    prim.prim_val.user_data = (void*)bound;

    // Bind in global env
    if (!env_define_with_barrier_checked(interp.global_env, ff.fn_name, prim, interp)) {
        return ffi_raise(interp, "ffi/out-of-memory", "ffi λ: out of memory while registering bound function");
    }
    return prim;
}
