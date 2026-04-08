module lisp;

import std::core::mem;

fn bool bindgen_type_is_callback(char* c_type) {
    return cstr_contains(c_type, "(*");
}

fn bool bindgen_type_is_const_qualified(char* c_type) {
    return cstr_contains(c_type, "const");
}

fn bool bindgen_type_is_char_pointer(char* c_type) {
    return cstr_contains(c_type, "char");
}

fn void bindgen_set_scalar_metadata(BindgenOwnershipTag* ownership, BindgenNullabilityTag* nullability,
    BindgenRoleTag* role, BindgenTeardownTag* teardown) {
    *ownership = BINDGEN_OWNERSHIP_NONE;
    *nullability = BINDGEN_NULLABILITY_NA;
    *role = BINDGEN_ROLE_VALUE;
    *teardown = BINDGEN_TEARDOWN_NONE;
}

fn void bindgen_infer_metadata_for_type(char* c_type, char* omni_type, bool is_return,
    BindgenOwnershipTag* ownership, BindgenNullabilityTag* nullability,
    BindgenRoleTag* role, BindgenTeardownTag* teardown) {
    bindgen_set_scalar_metadata(ownership, nullability, role, teardown);

    if (!is_return && bindgen_type_is_char_pointer(c_type) && !bindgen_type_is_const_qualified(c_type)) {
        *ownership = BINDGEN_OWNERSHIP_BORROWED;
        *nullability = BINDGEN_NULLABILITY_UNKNOWN;
        *role = BINDGEN_ROLE_STRING_BUFFER;
        *teardown = BINDGEN_TEARDOWN_MANUAL_REVIEW;
        return;
    }

    if (cstr_eq(omni_type, "Pointer")) {
        *ownership = is_return ? BINDGEN_OWNERSHIP_UNKNOWN : BINDGEN_OWNERSHIP_BORROWED;
        *nullability = BINDGEN_NULLABILITY_UNKNOWN;
        *role = bindgen_type_is_callback(c_type) ? BINDGEN_ROLE_CALLBACK : BINDGEN_ROLE_OPAQUE_POINTER;
        *teardown = bindgen_type_is_callback(c_type)
            ? BINDGEN_TEARDOWN_CALLBACK_UNREGISTER
            : (is_return ? BINDGEN_TEARDOWN_MANUAL_REVIEW : BINDGEN_TEARDOWN_NONE);
        return;
    }

    if (cstr_eq(omni_type, "String")) {
        *nullability = BINDGEN_NULLABILITY_UNKNOWN;
        if (is_return) {
            *ownership = bindgen_type_is_const_qualified(c_type) ? BINDGEN_OWNERSHIP_BORROWED : BINDGEN_OWNERSHIP_UNKNOWN;
            *role = BINDGEN_ROLE_STRING_RETURN;
            *teardown = BINDGEN_TEARDOWN_MANUAL_REVIEW;
            return;
        }

        *ownership = BINDGEN_OWNERSHIP_BORROWED;
        *role = bindgen_type_is_const_qualified(c_type) ? BINDGEN_ROLE_STRING_INPUT : BINDGEN_ROLE_STRING_BUFFER;
        *teardown = *role == BINDGEN_ROLE_STRING_BUFFER ? BINDGEN_TEARDOWN_MANUAL_REVIEW : BINDGEN_TEARDOWN_NONE;
    }
}

fn void bindgen_set_return_metadata(ParsedFunc* func) {
    bindgen_infer_metadata_for_type(
        &func.return_c_type,
        &func.return_omni_type,
        true,
        &func.return_ownership,
        &func.return_nullability,
        &func.return_role,
        &func.return_teardown
    );
}

fn bool cstr_contains_cstr(char* haystack, char* needle) {
    if (needle == null || needle[0] == 0) return false;
    usz i = 0;
    while (haystack[i] != 0) {
        usz j = 0;
        while (needle[j] != 0 && haystack[i + j] != 0 && haystack[i + j] == needle[j]) j++;
        if (needle[j] == 0) return true;
        i++;
    }
    return false;
}

fn bool bindgen_param_is_integer_size_candidate(ParsedParam* p) {
    return cstr_eq(&p.omni_type, "Integer");
}

fn bool bindgen_param_name_has_size_hint(char* name) {
    return cstr_contains(name, "len") ||
        cstr_contains(name, "size") ||
        cstr_contains(name, "count") ||
        cstr_contains(name, "capacity") ||
        cstr_contains(name, "cap") ||
        cstr_contains(name, "bytes");
}

fn BindgenBufferDirectionTag bindgen_infer_string_buffer_direction(char* name) {
    if (cstr_contains(name, "out") || cstr_contains(name, "output") || cstr_contains(name, "dst")) {
        return BINDGEN_BUFFER_DIRECTION_OUT;
    }
    if (cstr_contains(name, "inout")) {
        return BINDGEN_BUFFER_DIRECTION_INOUT;
    }
    if (cstr_contains(name, "input") || cstr_contains(name, "src")) {
        return BINDGEN_BUFFER_DIRECTION_IN;
    }
    // Mutable char* defaults to inout until explicit ownership policy is supplied.
    return BINDGEN_BUFFER_DIRECTION_INOUT;
}

fn int bindgen_find_string_buffer_size_param_index(ParsedFunc* func, int buffer_index) {
    if (func == null || buffer_index < 0 || (usz)buffer_index >= func.param_count) return -1;
    char* buffer_name = &func.params[(usz)buffer_index].name;
    int best_index = -1;
    int best_score = -1;
    for (usz i = 0; i < func.param_count; i++) {
        if ((int)i == buffer_index) continue;
        ParsedParam* candidate = &func.params[i];
        if (!bindgen_param_is_integer_size_candidate(candidate)) continue;
        if (!bindgen_param_name_has_size_hint(&candidate.name)) continue;

        int score = 0;
        if (cstr_contains_cstr(&candidate.name, buffer_name)) score += 10;
        if (cstr_contains(&candidate.name, "size")) score += 4;
        if (cstr_contains(&candidate.name, "len")) score += 4;
        if (cstr_contains(&candidate.name, "capacity")) score += 3;
        if (cstr_contains(&candidate.name, "count")) score += 2;

        int distance = (int)i - buffer_index;
        if (distance < 0) distance = -distance;
        if (distance <= 3) score += 4 - distance;

        if (score > best_score) {
            best_score = score;
            best_index = (int)i;
        }
    }
    return best_index;
}

fn void bindgen_finalize_param_metadata(ParsedFunc* func) {
    if (func == null || func.param_count == 0) return;
    for (usz i = 0; i < func.param_count; i++) {
        ParsedParam* p = &func.params[i];
        p.buffer_direction = BINDGEN_BUFFER_DIRECTION_NA;
        p.buffer_size_param_index = -1;
        if (p.role != BINDGEN_ROLE_STRING_BUFFER) continue;
        p.buffer_direction = bindgen_infer_string_buffer_direction(&p.name);
        p.buffer_size_param_index = bindgen_find_string_buffer_size_param_index(func, (int)i);
    }
}

/**
 * Map a CXType to FFI type string and Omni type annotation.
 */
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
    // Get canonical type to resolve typedefs
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
        // Check if char* / const char*
        CXString spelling = h.get_type_spelling(canon);
        char* cstr = h.get_cstring(spelling);
        bool is_char_ptr = false;
        bool is_const_char_ptr = false;
        if (cstr != null) {
            is_char_ptr = bindgen_type_is_char_pointer(cstr);
            is_const_char_ptr = bindgen_type_is_const_qualified(cstr);
        }
        h.dispose_string(spelling);

        if (is_char_ptr) {
            if (!is_return && !is_const_char_ptr) {
                copy_str("'ptr", ffi_out, buf_size);
                copy_str("Pointer", omni_out, buf_size);
            } else {
                copy_str("'string", ffi_out, buf_size);
                copy_str("String", omni_out, buf_size);
            }
        } else {
            copy_str("'ptr", ffi_out, buf_size);
            copy_str("Pointer", omni_out, buf_size);
        }
        return true;
    }

    set_unsupported_c_type_name(h, ty, unsupported_type_out, unsupported_type_cap);
    return false;
}

fn bool visitor_matches_filter(VisitorState* state, char* name_cstr) {
    if (!state.use_filter) return true;
    for (usz i = 0; i < state.filter_count; i++) {
        if (cstr_eq(name_cstr, &state.filter[i])) return true;
    }
    return false;
}

fn void visitor_set_param_name(ParsedParam* param, int index) {
    // Generate param name (arg0, arg1, ...)
    param.name[0] = 'a'; param.name[1] = 'r'; param.name[2] = 'g';
    if (index < 10) {
        param.name[3] = (char)('0' + index);
        param.name[4] = 0;
        return;
    }
    param.name[3] = (char)('0' + index / 10);
    param.name[4] = (char)('0' + index % 10);
    param.name[5] = 0;
}

fn void visitor_copy_param_name(LibclangHandle* h, CXCursor fn_cursor, ParsedParam* param, int index) {
    if (h.get_arg_cursor == null) {
        visitor_set_param_name(param, index);
        return;
    }

    CXCursor arg_cursor = h.get_arg_cursor(fn_cursor, (uint)index);
    CXString name_str = h.get_cursor_spelling(arg_cursor);
    char* name_cstr = h.get_cstring(name_str);
    if (name_cstr != null && name_cstr[0] != 0) {
        cstr_copy(name_cstr, &param.name, param.name.len - 1);
        h.dispose_string(name_str);
        return;
    }

    h.dispose_string(name_str);
    visitor_set_param_name(param, index);
}

fn bool visitor_set_param_types(LibclangHandle* h, ParsedParam* param, CXType arg_type,
    char* unsupported_type_out, usz unsupported_type_cap) {
    bool mapped = map_c_type(h, arg_type, false, &param.ffi_type, &param.omni_type, 16,
        unsupported_type_out, unsupported_type_cap);

    // Get type spelling for c_type field
    CXString type_str = h.get_type_spelling(arg_type);
    char* type_cstr = h.get_cstring(type_str);
    if (type_cstr != null) {
        cstr_copy(type_cstr, &param.c_type, 63);
    } else {
        param.c_type[0] = 0;
    }
    h.dispose_string(type_str);
    bindgen_infer_metadata_for_type(
        &param.c_type,
        &param.omni_type,
        false,
        &param.ownership,
        &param.nullability,
        &param.role,
        &param.teardown
    );
    param.buffer_direction = BINDGEN_BUFFER_DIRECTION_NA;
    param.buffer_size_param_index = -1;
    return mapped;
}

fn bool parsed_func_ensure_param_capacity(ParsedFunc* func, usz needed) {
    if (needed <= func.param_capacity) return true;
    if (g_bind_force_param_alloc_fail) return false;
    usz new_cap = func.param_capacity > 0 ? func.param_capacity : 8;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }
    ParsedParam* new_params = (ParsedParam*)mem::realloc(func.params, ParsedParam.sizeof * new_cap);
    if (new_params == null) return false;
    func.params = new_params;
    func.param_capacity = new_cap;
    return true;
}

fn bool visitor_set_params(LibclangHandle* h, CXCursor fn_cursor, CXType fn_type, ParsedFunc* func, VisitorState* state) {
    // Parameters
    int num_args = h.get_num_arg_types(fn_type);
    if (num_args < 0) num_args = 0;
    if (!parsed_func_ensure_param_capacity(func, (usz)num_args)) {
        state.param_allocation_failed = true;
        func.param_count = 0;
        return false;
    }
    func.param_count = (usz)num_args;

    for (int i = 0; i < num_args; i++) {
        ParsedParam* param = &func.params[i];
        CXType arg_type = h.get_arg_type(fn_type, (uint)i);
        if (!visitor_set_param_types(h, param, arg_type, &state.unsupported_type_name, state.unsupported_type_name.len)) {
            state.unsupported_type = true;
            func.param_count = 0;
            return false;
        }
        visitor_copy_param_name(h, fn_cursor, param, i);
    }
    bindgen_finalize_param_metadata(func);
    return true;
}
