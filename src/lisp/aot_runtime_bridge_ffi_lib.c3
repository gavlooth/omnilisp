module lisp::aot;

import std::io;
import lisp;

extern fn void* dlopen(char* filename, int flags) @extern("dlopen");
extern fn int dlclose(void* handle) @extern("dlclose");
extern fn char* dlerror() @extern("dlerror");
const int RTLD_LAZY = 1;

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
                    if (try char[] msg = io::bprintf(
                        &ebuf,
                        "aot ffi lib: dlopen failed for '%s': %s",
                        (ZString)soname,
                        (ZString)dl_err
                    )) {
                        return aot_make_error_best_effort(interp, msg);
                    }
                } else {
                    if (try char[] msg = io::bprintf(
                        &ebuf,
                        "aot ffi lib: dlopen failed for '%s'",
                        (ZString)soname
                    )) {
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
