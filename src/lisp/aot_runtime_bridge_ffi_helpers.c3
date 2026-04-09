module lisp::aot;

import std::core::mem;
import lisp;

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
