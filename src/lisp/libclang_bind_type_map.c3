module lisp;

fn void set_unsupported_c_type_name(LibclangHandle* h, CXType ty, char* out_buf, usz out_cap) {
    if (out_buf == null || out_cap == 0) return;
    out_buf[0] = 0;

    CXType canon = h.get_canonical_type(ty);
    CXString spelling = h.get_type_spelling(canon);
    char* cstr = h.get_cstring(spelling);
    if (cstr != null && cstr[0] != 0) {
        cstr_copy(cstr, out_buf, out_cap - 1);
    } else {
        copy_str("unknown", out_buf, out_cap - 1);
    }
    h.dispose_string(spelling);
}

fn bool map_c_type(LibclangHandle* h, CXType ty, bool is_return, char* ffi_out, char* omni_out, usz buf_size,
    char* unsupported_type_out = null, usz unsupported_type_cap = 0) {
    CXType canon = h.get_canonical_type(ty);
    int kind = canon.kind;

    if (kind == CXTYPE_VOID) {
        copy_str("'void", ffi_out, buf_size);
        copy_str("Void", omni_out, buf_size);
        return true;
    }
    if (kind == CXTYPE_INT || kind == CXTYPE_UINT ||
        kind == CXTYPE_LONG || kind == CXTYPE_ULONG ||
        kind == CXTYPE_ENUM) {
        copy_str("'int", ffi_out, buf_size);
        copy_str("Integer", omni_out, buf_size);
        return true;
    }
    if (kind == CXTYPE_FLOAT || kind == CXTYPE_DOUBLE) {
        copy_str("'double", ffi_out, buf_size);
        copy_str("Double", omni_out, buf_size);
        return true;
    }
    if (kind == CXTYPE_POINTER) {
        CXString spelling = h.get_type_spelling(canon);
        char* cstr = h.get_cstring(spelling);
        bool is_char_ptr = false;
        bool is_const_char_ptr = false;
        if (cstr != null) {
            is_char_ptr = bindgen_type_is_char_pointer(cstr);
            is_const_char_ptr = bindgen_type_is_pointee_const_qualified(cstr);
        }
        h.dispose_string(spelling);

        if (is_char_ptr) {
            if (!is_return && !is_const_char_ptr) {
                copy_str("'ptr", ffi_out, buf_size);
                copy_str("ForeignHandle", omni_out, buf_size);
            } else {
                copy_str("'string", ffi_out, buf_size);
                copy_str("String", omni_out, buf_size);
            }
        } else {
            copy_str("'ptr", ffi_out, buf_size);
            copy_str("ForeignHandle", omni_out, buf_size);
        }
        return true;
    }

    set_unsupported_c_type_name(h, ty, unsupported_type_out, unsupported_type_cap);
    return false;
}
