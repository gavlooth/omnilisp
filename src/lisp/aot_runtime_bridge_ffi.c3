module lisp::aot;

import std::io;
import lisp;

extern fn void* dlopen(char* filename, int flags) @extern("dlopen");
extern fn int dlclose(void* handle) @extern("dlclose");
extern fn char* dlerror() @extern("dlerror");
const int RTLD_LAZY = 1;

fn bool aot_copy_cstring(char[] text, char** out_text, usz* out_len) @inline {
    *out_text = null;
    *out_len = 0;
    if (text.len == 0) return true;
    if (text.len == usz.max) return false;
    char* buf = (char*)mem::malloc(text.len + 1);
    if (buf == null) return false;
    mem::copy(buf, text.ptr, text.len);
    buf[text.len] = 0;
    *out_text = buf;
    *out_len = text.len;
    return true;
}

fn bool aot_ffi_type_tag_from_int(int raw_tag, lisp::FfiTypeTag* out_tag) @inline {
    switch (raw_tag) {
        case 0:
            *out_tag = (lisp::FfiTypeTag)0;
            return true;
        case 1:
            *out_tag = (lisp::FfiTypeTag)1;
            return true;
        case 2:
            *out_tag = (lisp::FfiTypeTag)2;
            return true;
        case 3:
            *out_tag = (lisp::FfiTypeTag)3;
            return true;
        case 4:
            *out_tag = (lisp::FfiTypeTag)4;
            return true;
        case 5:
            *out_tag = (lisp::FfiTypeTag)5;
            return true;
        default:
            return false;
    }
}

fn void aot_ffi_free_bound(lisp::FfiBoundFn* bound) @inline {
    if (bound == null) return;
    if (bound.param_types != null) mem::free(bound.param_types);
    if (bound.lib_name != null) mem::free(bound.lib_name);
    if (bound.c_name != null) mem::free(bound.c_name);
    mem::free(bound);
}

fn lisp::Value* ffi_declare_lib(char[] binding_name, char[] soname, bool eager_load = true, bool required = true) {
    lisp::Interp* interp = aot_bridge_interp();
    if (interp == null) return aot_make_error_best_effort(null, "aot ffi lib: runtime interpreter unavailable");
    if (binding_name.len == 0) return aot_make_error_best_effort(interp, "aot ffi lib: binding name must not be empty");
    if (soname.len == 0) return aot_make_error_best_effort(interp, "aot ffi lib: soname must not be empty");

    lisp::SymbolId binding_sid = interp.symbols.intern(binding_name);
    lisp::Value* existing = interp.global_env.lookup(binding_sid);
    if (existing != null) {
        if (existing.tag != lisp::ValueTag.FFI_HANDLE || existing.ffi_val == null) {
            return aot_make_error_best_effort(interp, "aot ffi lib: existing binding is not an FFI handle");
        }
        if (existing.ffi_val.lib_handle != null) {
            return existing;
        }
        if (!eager_load) {
            return existing;
        }
        soname = existing.ffi_val.lib_name[:existing.ffi_val.name_len];
    }

    void* handle = null;
    if (eager_load) {
        (void)dlerror();
        handle = dlopen((ZString)soname, RTLD_LAZY);
        if (handle == null) {
            char[256] ebuf;
            char* dl_err = dlerror();
            if (required) {
                if (dl_err != null && dl_err[0] != 0) {
                    if (try char[] msg = io::bprintf(&ebuf, "aot ffi lib: dlopen failed for '%s': %s",
                        (ZString)soname, (ZString)dl_err)) {
                        return aot_make_error_best_effort(interp, msg);
                    }
                } else {
                    if (try char[] msg = io::bprintf(&ebuf, "aot ffi lib: dlopen failed for '%s'",
                        (ZString)soname)) {
                        return aot_make_error_best_effort(interp, msg);
                    }
                }
                return aot_make_error_best_effort(interp, "aot ffi lib: dlopen failed");
            }
        }
    }

    lisp::Value* ffi_handle = existing;
    if (ffi_handle == null) {
        ffi_handle = lisp::make_ffi_handle_ex(interp, handle, soname, null, false, true);
        if (ffi_handle == null) {
            if (handle != null) dlclose(handle);
            return aot_make_error_best_effort(interp, "aot ffi lib: out of memory while creating handle");
        }
        if (!lisp::env_define_with_barrier_checked(interp.global_env, binding_sid, ffi_handle, interp)) {
            if (handle != null) dlclose(handle);
            return aot_make_error_best_effort(interp, "aot ffi lib: out of memory while registering library binding");
        }
    } else if (handle != null) {
        ffi_handle.ffi_val.lib_handle = handle;
    }
    return ffi_handle;
}

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

    lisp::SymbolId lib_sid = interp.symbols.intern(lib_name);
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
        lisp::FfiBoundFn* unavailable = (lisp::FfiBoundFn*)mem::malloc(lisp::FfiBoundFn.sizeof);
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
        lisp::SymbolId fn_sid = interp.symbols.intern(omni_fn_name);
        if (!lisp::env_define_with_barrier_checked(interp.global_env, fn_sid, prim, interp)) {
            aot_ffi_free_bound(unavailable);
            return aot_make_error_best_effort(interp, "aot ffi λ: out of memory while registering unavailable primitive");
        }
        return prim;
    }

    lisp::FfiBoundFn* bound = (lisp::FfiBoundFn*)mem::malloc(lisp::FfiBoundFn.sizeof);
    if (bound == null) {
        return aot_make_error_best_effort(interp, "aot ffi λ: out of memory while allocating bound function");
    }
    bound.fn_ptr = null;
    bound.lib_handle = lib_val.ffi_val.lib_handle;
    bound.required_library = true;
    bound.required_symbol = required_symbol;
    bound.is_unavailable = false;
    if (lib_val.ffi_val.name_len > 0) {
        if (!aot_copy_cstring(lib_val.ffi_val.lib_name[:lib_val.ffi_val.name_len], &bound.lib_name, &bound.lib_name_len)) {
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
    bound.c_name = (char*)mem::malloc(c_symbol.len + 1);
    if (bound.c_name == null) {
        aot_ffi_free_bound(bound);
        return aot_make_error_best_effort(interp, "aot ffi λ: out of memory while copying symbol name");
    }
    mem::copy(bound.c_name, c_symbol.ptr, c_symbol.len);
    bound.c_name[c_symbol.len] = 0;

    if (param_count > 0) {
        if (param_count > usz.max / lisp::FfiTypeTag.sizeof) {
            aot_ffi_free_bound(bound);
            return aot_make_error_best_effort(interp, "aot ffi λ: parameter ABI table size overflow");
        }
        bound.param_types = (lisp::FfiTypeTag*)mem::malloc(lisp::FfiTypeTag.sizeof * param_count);
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

    lisp::SymbolId fn_sid = interp.symbols.intern(omni_fn_name);
    if (!lisp::env_define_with_barrier_checked(interp.global_env, fn_sid, prim, interp)) {
        aot_ffi_free_bound(bound);
        return aot_make_error_best_effort(interp, "aot ffi λ: out of memory while registering bound function");
    }

    return prim;
}
