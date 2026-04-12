module lisp;

fn bool bindgen_metadata_is_trivial(BindgenRoleTag role, BindgenOwnershipTag ownership,
    BindgenNullabilityTag nullability, BindgenTeardownTag teardown) {
    return role == BINDGEN_ROLE_VALUE &&
        ownership == BINDGEN_OWNERSHIP_NONE &&
        nullability == BINDGEN_NULLABILITY_NA &&
        teardown == BINDGEN_TEARDOWN_NONE;
}

fn bool bindgen_function_has_nontrivial_metadata(ParsedFunc* f) {
    if (f == null || f.filtered_out) return false;
    if (!bindgen_metadata_is_trivial(f.return_role, f.return_ownership, f.return_nullability, f.return_teardown)) {
        return true;
    }
    for (usz i = 0; i < f.param_count; i++) {
        ParsedParam* p = &f.params[i];
        if (!bindgen_metadata_is_trivial(p.role, p.ownership, p.nullability, p.teardown)) return true;
    }
    return false;
}

fn bool bindgen_function_needs_facade_scaffold(ParsedFunc* f) {
    if (f == null || f.filtered_out) return false;
    if (f.return_role == BINDGEN_ROLE_STRING_RETURN || f.return_role == BINDGEN_ROLE_OPAQUE_POINTER) {
        return true;
    }
    for (usz i = 0; i < f.param_count; i++) {
        BindgenRoleTag role = f.params[i].role;
        if (role == BINDGEN_ROLE_STRING_BUFFER || role == BINDGEN_ROLE_OPAQUE_POINTER || role == BINDGEN_ROLE_CALLBACK) {
            return true;
        }
    }
    return false;
}

fn usz bindgen_append_ownership_name(char[] buf, usz pos, BindgenOwnershipTag ownership, bool* ok) {
    switch (ownership) {
        case BINDGEN_OWNERSHIP_NONE: return buf_append(buf, pos, "none", ok);
        case BINDGEN_OWNERSHIP_BORROWED: return buf_append(buf, pos, "borrowed", ok);
        case BINDGEN_OWNERSHIP_UNKNOWN: return buf_append(buf, pos, "unknown", ok);
    }
}

fn usz bindgen_append_nullability_name(char[] buf, usz pos, BindgenNullabilityTag nullability, bool* ok) {
    switch (nullability) {
        case BINDGEN_NULLABILITY_NA: return buf_append(buf, pos, "n/a", ok);
        case BINDGEN_NULLABILITY_UNKNOWN: return buf_append(buf, pos, "unknown", ok);
    }
}

fn usz bindgen_append_role_name(char[] buf, usz pos, BindgenRoleTag role, bool* ok) {
    switch (role) {
        case BINDGEN_ROLE_VALUE: return buf_append(buf, pos, "value", ok);
        case BINDGEN_ROLE_STRING_INPUT: return buf_append(buf, pos, "string-input", ok);
        case BINDGEN_ROLE_STRING_BUFFER: return buf_append(buf, pos, "string-buffer", ok);
        case BINDGEN_ROLE_STRING_RETURN: return buf_append(buf, pos, "string-return", ok);
        case BINDGEN_ROLE_OPAQUE_POINTER: return buf_append(buf, pos, "opaque-pointer", ok);
        case BINDGEN_ROLE_CALLBACK: return buf_append(buf, pos, "callback", ok);
    }
}

fn usz bindgen_append_buffer_direction_name(char[] buf, usz pos, BindgenBufferDirectionTag direction, bool* ok) {
    switch (direction) {
        case BINDGEN_BUFFER_DIRECTION_NA: return buf_append(buf, pos, "n/a", ok);
        case BINDGEN_BUFFER_DIRECTION_IN: return buf_append(buf, pos, "in", ok);
        case BINDGEN_BUFFER_DIRECTION_OUT: return buf_append(buf, pos, "out", ok);
        case BINDGEN_BUFFER_DIRECTION_INOUT: return buf_append(buf, pos, "inout", ok);
    }
}

fn usz bindgen_append_teardown_name(char[] buf, usz pos, BindgenTeardownTag teardown, bool* ok) {
    switch (teardown) {
        case BINDGEN_TEARDOWN_NONE: return buf_append(buf, pos, "none", ok);
        case BINDGEN_TEARDOWN_MANUAL_REVIEW: return buf_append(buf, pos, "manual-review", ok);
        case BINDGEN_TEARDOWN_CALLBACK_UNREGISTER: return buf_append(buf, pos, "callback-unregister", ok);
    }
}

fn usz bindgen_append_metadata_fields(char[] buf, usz pos, BindgenRoleTag role,
    BindgenOwnershipTag ownership, BindgenNullabilityTag nullability, BindgenTeardownTag teardown, bool* ok) {
    pos = buf_append(buf, pos, " role=", ok);
    pos = bindgen_append_role_name(buf, pos, role, ok);
    pos = buf_append(buf, pos, " ownership=", ok);
    pos = bindgen_append_ownership_name(buf, pos, ownership, ok);
    pos = buf_append(buf, pos, " nullability=", ok);
    pos = bindgen_append_nullability_name(buf, pos, nullability, ok);
    pos = buf_append(buf, pos, " teardown=", ok);
    pos = bindgen_append_teardown_name(buf, pos, teardown, ok);
    return pos;
}

fn usz bindgen_append_quoted_role_name(char[] buf, usz pos, BindgenRoleTag role, bool* ok) {
    pos = buf_append_char(buf, pos, '"', ok);
    pos = bindgen_append_role_name(buf, pos, role, ok);
    return buf_append_char(buf, pos, '"', ok);
}

fn usz bindgen_append_quoted_ownership_name(char[] buf, usz pos, BindgenOwnershipTag ownership, bool* ok) {
    pos = buf_append_char(buf, pos, '"', ok);
    pos = bindgen_append_ownership_name(buf, pos, ownership, ok);
    return buf_append_char(buf, pos, '"', ok);
}

fn usz bindgen_append_quoted_nullability_name(char[] buf, usz pos, BindgenNullabilityTag nullability, bool* ok) {
    pos = buf_append_char(buf, pos, '"', ok);
    pos = bindgen_append_nullability_name(buf, pos, nullability, ok);
    return buf_append_char(buf, pos, '"', ok);
}

fn usz bindgen_append_quoted_teardown_name(char[] buf, usz pos, BindgenTeardownTag teardown, bool* ok) {
    pos = buf_append_char(buf, pos, '"', ok);
    pos = bindgen_append_teardown_name(buf, pos, teardown, ok);
    return buf_append_char(buf, pos, '"', ok);
}

fn usz bindgen_append_quoted_buffer_direction_name(char[] buf, usz pos, BindgenBufferDirectionTag direction, bool* ok) {
    pos = buf_append_char(buf, pos, '"', ok);
    pos = bindgen_append_buffer_direction_name(buf, pos, direction, ok);
    return buf_append_char(buf, pos, '"', ok);
}

fn usz bindgen_append_metadata_literal_args(char[] buf, usz pos, BindgenRoleTag role, BindgenOwnershipTag ownership,
    BindgenNullabilityTag nullability, BindgenTeardownTag teardown, bool* ok) {
    pos = buf_append_char(buf, pos, ' ', ok);
    pos = bindgen_append_quoted_role_name(buf, pos, role, ok);
    pos = buf_append_char(buf, pos, ' ', ok);
    pos = bindgen_append_quoted_ownership_name(buf, pos, ownership, ok);
    pos = buf_append_char(buf, pos, ' ', ok);
    pos = bindgen_append_quoted_nullability_name(buf, pos, nullability, ok);
    pos = buf_append_char(buf, pos, ' ', ok);
    return bindgen_append_quoted_teardown_name(buf, pos, teardown, ok);
}

fn usz bindgen_append_string_buffer_metadata_literal_args(char[] buf, usz pos, ParsedParam* p, bool* ok) {
    pos = bindgen_append_metadata_literal_args(buf, pos, p.role, p.ownership, p.nullability, p.teardown, ok);
    pos = buf_append_char(buf, pos, ' ', ok);
    return bindgen_append_quoted_buffer_direction_name(buf, pos, p.buffer_direction, ok);
}

fn usz bindgen_append_buffer_size_param_name(char[] buf, usz pos, ParsedFunc* f, ParsedParam* p, bool* ok) {
    if (p.buffer_size_param_index < 0 || (usz)p.buffer_size_param_index >= f.param_count) {
        return buf_append(buf, pos, "none", ok);
    }
    if (!bindgen_emitted_name_is_safe_for_codegen(&f.params[(usz)p.buffer_size_param_index].name[0])) {
        *ok = false;
        return pos;
    }
    return buf_append_cstr(buf, pos, &f.params[(usz)p.buffer_size_param_index].name, ok);
}

fn usz bindgen_append_metadata_descriptor_fields(char[] buf, usz pos, char* c_type, char* abi_type, char* omni_type,
    BindgenRoleTag role, BindgenOwnershipTag ownership, BindgenNullabilityTag nullability, BindgenTeardownTag teardown, bool* ok) {
    if (!bindgen_metadata_text_is_safe(c_type) || !bindgen_metadata_text_is_safe(abi_type) || !bindgen_metadata_text_is_safe(omni_type)) {
        *ok = false;
        return pos;
    }

    pos = buf_append(buf, pos, " c-type=", ok);
    pos = buf_append_cstr(buf, pos, c_type, ok);
    pos = buf_append(buf, pos, " abi-type=", ok);
    pos = buf_append_cstr(buf, pos, abi_type, ok);
    pos = buf_append(buf, pos, " type=", ok);
    pos = buf_append_cstr(buf, pos, omni_type, ok);
    return bindgen_append_metadata_fields(buf, pos, role, ownership, nullability, teardown, ok);
}

fn usz bindgen_append_metadata_shape_record(char[] buf, usz pos, ParsedFunc* f, bool* ok) {
    if (!bindgen_function_has_nontrivial_metadata(f)) return pos;
    if (!bindgen_metadata_text_is_safe(&f.name[0]) || !bindgen_emitted_name_is_safe_for_codegen(&f.name[0])) {
        *ok = false;
        return pos;
    }

    pos = buf_append(buf, pos, "  ;; bindgen-descriptor: callable type=ForeignCallable runtime=c-abi kind=function c-name=", ok);
    pos = buf_append_cstr(buf, pos, &f.name, ok);
    if (!bindgen_metadata_text_is_safe(&f.return_omni_type)) {
        *ok = false;
        return pos;
    }
    pos = buf_append(buf, pos, " name=", ok);
    pos = buf_append_kebab(buf, pos, &f.name, ok);
    pos = buf_append_char(buf, pos, '\n', ok);

    pos = buf_append(buf, pos, "  ;; bindgen-descriptor: returns", ok);
    pos = bindgen_append_metadata_descriptor_fields(
        buf,
        pos,
        &f.return_c_type,
        &f.return_ffi_type,
        &f.return_omni_type,
        f.return_role,
        f.return_ownership,
        f.return_nullability,
        f.return_teardown,
        ok
    );
    pos = buf_append_char(buf, pos, '\n', ok);

    for (usz i = 0; i < f.param_count; i++) {
        ParsedParam* p = &f.params[i];
        if (!bindgen_metadata_text_is_safe(&p.name[0]) || !bindgen_emitted_name_is_safe_for_codegen(&p.name[0])) {
            *ok = false;
            return pos;
        }
        if (!bindgen_metadata_text_is_safe(&p.omni_type) || !bindgen_metadata_text_is_safe(&p.c_type) ||
            !bindgen_metadata_text_is_safe(&p.ffi_type)) {
            *ok = false;
            return pos;
        }
        pos = buf_append(buf, pos, "  ;; bindgen-descriptor: parameter name=", ok);
        pos = buf_append_cstr(buf, pos, &p.name, ok);
        pos = bindgen_append_metadata_descriptor_fields(
            buf,
            pos,
            &p.c_type,
            &p.ffi_type,
            &p.omni_type,
            p.role,
            p.ownership,
            p.nullability,
            p.teardown,
            ok
        );
        if (p.role == BINDGEN_ROLE_STRING_BUFFER) {
            pos = buf_append(buf, pos, " buffer-direction=", ok);
            pos = bindgen_append_buffer_direction_name(buf, pos, p.buffer_direction, ok);
            pos = buf_append(buf, pos, " size-param=", ok);
            pos = bindgen_append_buffer_size_param_name(buf, pos, f, p, ok);
        }
        pos = buf_append_char(buf, pos, '\n', ok);
    }
    return pos;
}

fn usz bindgen_append_function_metadata(char[] buf, usz pos, ParsedFunc* f, bool* ok) {
    if (!bindgen_function_has_nontrivial_metadata(f)) return pos;
    if (!bindgen_metadata_text_is_safe(&f.return_omni_type) ||
        !bindgen_metadata_text_is_safe(&f.return_c_type) ||
        !bindgen_metadata_text_is_safe(&f.return_ffi_type) ||
        !bindgen_emitted_name_is_safe_for_codegen(&f.name[0])) {
        *ok = false;
        return pos;
    }

    if (!bindgen_metadata_is_trivial(f.return_role, f.return_ownership, f.return_nullability, f.return_teardown)) {
        pos = buf_append(buf, pos, "  ;; bindgen-policy: returns c-type=", ok);
        pos = buf_append_cstr(buf, pos, &f.return_c_type, ok);
        pos = bindgen_append_metadata_fields(buf, pos, f.return_role, f.return_ownership, f.return_nullability, f.return_teardown, ok);
        pos = buf_append_char(buf, pos, '\n', ok);
    }

    for (usz i = 0; i < f.param_count; i++) {
        ParsedParam* p = &f.params[i];
        if (!bindgen_metadata_text_is_safe(&p.name[0]) || !bindgen_emitted_name_is_safe_for_codegen(&p.name[0])) {
            *ok = false;
            return pos;
        }
        if (!bindgen_metadata_text_is_safe(&p.c_type) || !bindgen_metadata_text_is_safe(&p.ffi_type)) {
            *ok = false;
            return pos;
        }
        if (bindgen_metadata_is_trivial(p.role, p.ownership, p.nullability, p.teardown)) continue;
        pos = buf_append(buf, pos, "  ;; bindgen-policy: parameter name=", ok);
        pos = buf_append_cstr(buf, pos, &p.name, ok);
        pos = buf_append(buf, pos, " c-type=", ok);
        pos = buf_append_cstr(buf, pos, &p.c_type, ok);
        pos = bindgen_append_metadata_fields(buf, pos, p.role, p.ownership, p.nullability, p.teardown, ok);
        if (p.role == BINDGEN_ROLE_STRING_BUFFER) {
            pos = buf_append(buf, pos, " buffer-direction=", ok);
            pos = bindgen_append_buffer_direction_name(buf, pos, p.buffer_direction, ok);
            pos = buf_append(buf, pos, " size-param=", ok);
            pos = bindgen_append_buffer_size_param_name(buf, pos, f, p, ok);
        }
        pos = buf_append_char(buf, pos, '\n', ok);
    }

    pos = bindgen_append_metadata_shape_record(buf, pos, f, ok);
    return pos;
}
