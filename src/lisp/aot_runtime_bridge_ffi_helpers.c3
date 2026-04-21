module lisp::aot;

import lisp;

fn bool aot_copy_cstring(lisp::Interp* interp, char[] text, char** out_text, usz* out_len) @inline {
    *out_text = null;
    *out_len = 0;
    if (text.len == 0) return true;
    if (interp == null || interp.root_scope == null || text.len == usz.max) return false;
    char* buf = (char*)interp.root_scope.alloc(text.len + 1);
    if (buf == null) return false;
    for (usz i = 0; i < text.len; i++) {
        buf[i] = text[i];
    }
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
        case 6:
            *out_tag = (lisp::FfiTypeTag)6;
            return true;
        case 7:
            *out_tag = (lisp::FfiTypeTag)7;
            return true;
        case 8:
            *out_tag = (lisp::FfiTypeTag)8;
            return true;
        case 9:
            *out_tag = (lisp::FfiTypeTag)9;
            return true;
        case 10:
            *out_tag = (lisp::FfiTypeTag)10;
            return true;
        case 11:
            *out_tag = (lisp::FfiTypeTag)11;
            return true;
        case 12:
            *out_tag = (lisp::FfiTypeTag)12;
            return true;
        default:
            return false;
    }
}

fn void aot_ffi_free_bound(lisp::FfiBoundFn* bound) @inline {
    // AOT FFI bindings are root-scope-owned and are reclaimed when the
    // interpreter root scope is released.
    (void)bound;
}
