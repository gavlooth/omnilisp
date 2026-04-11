module lisp::aot;

import std::io;
import lisp;

fn lisp::Value* ffi_declare_fn(
    char[] lib_name,
    char[] omni_fn_name,
    char[] c_symbol,
    int* param_type_tags,
    usz param_count,
    bool has_return_type,
    int return_type_tag,
    bool eager_resolve = true,
    bool required_symbol = true
) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot ffi λ: runtime interpreter unavailable");
    if (lib_name.len == 0) return aot_make_error_best_effort(interp, "aot ffi λ: library binding name must not be empty");
    if (omni_fn_name.len == 0) return aot_make_error_best_effort(interp, "aot ffi λ: function binding name must not be empty");
    if (c_symbol.len == 0) return aot_make_error_best_effort(interp, "aot ffi λ: C symbol name must not be empty");
    if (param_count > 0 && param_type_tags == null) return aot_make_error_best_effort(interp, "aot ffi λ: missing parameter ABI tags");
    if (param_count > (usz)int.max) return aot_make_error_best_effort(interp, "aot ffi λ: parameter count exceeds supported arity");

    lisp::SymbolId lib_sid = (lisp::SymbolId)0;
    if (!aot_intern_checked(interp, lib_name, &lib_sid)) {
        return aot_make_error_best_effort(interp, "aot ffi λ: invalid or uninternable library binding name");
    }
    lisp::Value* lib_val = interp.global_env.lookup(lib_sid);
    if (lib_val == null || lib_val.tag != lisp::ValueTag.FFI_HANDLE || lib_val.ffi_val == null) {
        if (required_symbol) {
            char[256] ebuf;
            if (try char[] msg = io::bprintf(&ebuf, "aot ffi λ: library '%s' not found", (ZString)lib_name)) {
                return aot_make_error_best_effort(interp, msg);
            }
            return aot_make_error_best_effort(interp, "aot ffi λ: library not found");
        }
        lisp::Value* prim = lisp::make_primitive(interp, omni_fn_name, &lisp::prim_ffi_bound_call, (int)param_count);
        if (prim == null || prim.tag != lisp::ValueTag.PRIMITIVE || prim.prim_val == null) {
            return aot_make_error_best_effort(interp, "aot ffi λ: out of memory while creating unavailable primitive");
        }
        lisp::FfiBoundFn* unavailable = (lisp::FfiBoundFn*)interp.root_scope.alloc(lisp::FfiBoundFn.sizeof);
        if (unavailable == null) {
            return aot_make_error_best_effort(interp, "aot ffi λ: out of memory while allocating unavailable binding");
        }
        unavailable.fn_ptr = null;
        unavailable.lib_handle = null;
        unavailable.lib_name = null;
        unavailable.lib_name_len = 0;
        unavailable.param_count = param_count;
        unavailable.param_types = null;
        unavailable.return_type = (lisp::FfiTypeTag)0;
        unavailable.has_return = false;
        unavailable.is_variadic = false;
        unavailable.required_library = false;
        unavailable.required_symbol = false;
        unavailable.is_unavailable = true;
        unavailable.c_name = null;
        unavailable.c_name_len = 0;
        prim.prim_val.user_data = (void*)unavailable;
        lisp::SymbolId fn_sid = (lisp::SymbolId)0;
        if (!aot_intern_checked(interp, omni_fn_name, &fn_sid)) {
            aot_ffi_free_bound(unavailable);
            return aot_make_error_best_effort(interp, "aot ffi λ: invalid or uninternable function binding name");
        }
        if (!lisp::env_define_with_barrier_checked(interp.global_env, fn_sid, prim, interp)) {
            aot_ffi_free_bound(unavailable);
            return aot_make_error_best_effort(interp, "aot ffi λ: out of memory while registering unavailable primitive");
        }
        return prim;
    }

    lisp::FfiBoundFn* bound = (lisp::FfiBoundFn*)interp.root_scope.alloc(lisp::FfiBoundFn.sizeof);
    if (bound == null) {
        return aot_make_error_best_effort(interp, "aot ffi λ: out of memory while allocating bound function");
    }
    bound.fn_ptr = null;
    bound.lib_handle = lib_val.ffi_val.lib_handle;
    bound.required_library = true;
    bound.required_symbol = required_symbol;
    bound.is_unavailable = false;
    if (lib_val.ffi_val.name_len > 0) {
        if (!aot_copy_cstring(interp, lib_val.ffi_val.lib_name[:lib_val.ffi_val.name_len], &bound.lib_name, &bound.lib_name_len)) {
            aot_ffi_free_bound(bound);
            return aot_make_error_best_effort(interp, "aot ffi λ: out of memory while copying library soname");
        }
    } else {
        bound.lib_name = null;
        bound.lib_name_len = 0;
    }
    bound.param_count = param_count;
    bound.param_types = null;
    bound.has_return = false;
    bound.return_type = (lisp::FfiTypeTag)0;
    bound.is_variadic = false;
    bound.c_name = null;
    bound.c_name_len = c_symbol.len;

    if (c_symbol.len == usz.max) {
        aot_ffi_free_bound(bound);
        return aot_make_error_best_effort(interp, "aot ffi λ: C symbol name is too long");
    }
    bound.c_name = (char*)interp.root_scope.alloc(c_symbol.len + 1);
    if (bound.c_name == null) {
        aot_ffi_free_bound(bound);
        return aot_make_error_best_effort(interp, "aot ffi λ: out of memory while copying symbol name");
    }
    for (usz i = 0; i < c_symbol.len; i++) {
        bound.c_name[i] = c_symbol[i];
    }
    bound.c_name[c_symbol.len] = 0;

    if (param_count > 0) {
        if (param_count > usz.max / lisp::FfiTypeTag.sizeof) {
            aot_ffi_free_bound(bound);
            return aot_make_error_best_effort(interp, "aot ffi λ: parameter ABI table size overflow");
        }
        bound.param_types = (lisp::FfiTypeTag*)interp.root_scope.alloc(lisp::FfiTypeTag.sizeof * param_count);
        if (bound.param_types == null) {
            aot_ffi_free_bound(bound);
            return aot_make_error_best_effort(interp, "aot ffi λ: out of memory while allocating parameter ABI tags");
        }
        for (usz i = 0; i < param_count; i++) {
            lisp::FfiTypeTag tag;
            if (!aot_ffi_type_tag_from_int(param_type_tags[i], &tag)) {
                char[256] ebuf;
                if (try char[] msg = io::bprintf(&ebuf, "aot ffi λ: unsupported parameter ABI tag %d at index %d",
                    param_type_tags[i], (int)i)) {
                    aot_ffi_free_bound(bound);
                    return aot_make_error_best_effort(interp, msg);
                }
                aot_ffi_free_bound(bound);
                return aot_make_error_best_effort(interp, "aot ffi λ: unsupported parameter ABI tag");
            }
            bound.param_types[i] = tag;
        }
    }

    if (has_return_type) {
        lisp::FfiTypeTag ret_tag;
        if (!aot_ffi_type_tag_from_int(return_type_tag, &ret_tag)) {
            char[256] ebuf;
            if (try char[] msg = io::bprintf(&ebuf, "aot ffi λ: unsupported return ABI tag %d",
                return_type_tag)) {
                aot_ffi_free_bound(bound);
                return aot_make_error_best_effort(interp, msg);
            }
            aot_ffi_free_bound(bound);
            return aot_make_error_best_effort(interp, "aot ffi λ: unsupported return ABI tag");
        }
        bound.return_type = ret_tag;
        bound.has_return = true;
    }

    if (eager_resolve) {
        if (lisp::Value* resolve_error = lisp::ffi_resolve_bound_fn(bound, interp)) {
            aot_ffi_free_bound(bound);
            return resolve_error;
        }
    }

    lisp::Value* prim = lisp::make_primitive(interp, omni_fn_name, &lisp::prim_ffi_bound_call, (int)param_count);
    if (prim == null || prim.tag != lisp::ValueTag.PRIMITIVE || prim.prim_val == null) {
        aot_ffi_free_bound(bound);
        return aot_make_error_best_effort(interp, "aot ffi λ: out of memory while creating bound primitive");
    }
    prim.prim_val.user_data = (void*)bound;

    lisp::SymbolId fn_sid = (lisp::SymbolId)0;
    if (!aot_intern_checked(interp, omni_fn_name, &fn_sid)) {
        aot_ffi_free_bound(bound);
        return aot_make_error_best_effort(interp, "aot ffi λ: invalid or uninternable function binding name");
    }
    if (!lisp::env_define_with_barrier_checked(interp.global_env, fn_sid, prim, interp)) {
        aot_ffi_free_bound(bound);
        return aot_make_error_best_effort(interp, "aot ffi λ: out of memory while registering bound function");
    }

    return prim;
}
