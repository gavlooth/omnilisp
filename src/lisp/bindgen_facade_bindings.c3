module lisp;

fn usz bindgen_append_string_buffer_size_arg(char[] buf, usz pos, ParsedFunc* f, ParsedParam* p, bool* ok) {
    if (p.buffer_size_param_index < 0 || (usz)p.buffer_size_param_index >= f.param_count) {
        return buf_append(buf, pos, "nil", ok);
    }
    return buf_append_cstr(buf, pos, &f.params[(usz)p.buffer_size_param_index].name, ok);
}

fn usz bindgen_append_facade_arg_binding_value(char[] buf, usz pos, ParsedFunc* f, ParsedParam* p, bool* ok) {
    switch (p.role) {
        case BINDGEN_ROLE_STRING_BUFFER:
            pos = buf_append(buf, pos, "(bindgen-prepare-string-buffer ", ok);
            pos = buf_append_cstr(buf, pos, &p.name, ok);
            pos = buf_append_char(buf, pos, ' ', ok);
            pos = bindgen_append_string_buffer_size_arg(buf, pos, f, p, ok);
            pos = bindgen_append_string_buffer_metadata_literal_args(buf, pos, p, ok);
            return buf_append_char(buf, pos, ')', ok);
        case BINDGEN_ROLE_CALLBACK:
            pos = buf_append(buf, pos, "(bindgen-register-callback ", ok);
            pos = buf_append_cstr(buf, pos, &p.name, ok);
            pos = bindgen_append_metadata_literal_args(buf, pos, p.role, p.ownership, p.nullability, p.teardown, ok);
            return buf_append_char(buf, pos, ')', ok);
        case BINDGEN_ROLE_OPAQUE_POINTER:
            pos = buf_append(buf, pos, "(bindgen-wrap-opaque-arg ", ok);
            pos = buf_append_cstr(buf, pos, &p.name, ok);
            pos = bindgen_append_metadata_literal_args(buf, pos, p.role, p.ownership, p.nullability, p.teardown, ok);
            return buf_append_char(buf, pos, ')', ok);
        case BINDGEN_ROLE_VALUE:
        case BINDGEN_ROLE_STRING_INPUT:
        case BINDGEN_ROLE_STRING_RETURN:
            return buf_append_cstr(buf, pos, &p.name, ok);
    }
}

fn usz bindgen_append_facade_return_expr(char[] buf, usz pos, ParsedFunc* f, bool* ok) {
    switch (f.return_role) {
        case BINDGEN_ROLE_STRING_RETURN:
            pos = buf_append(buf, pos, "(", ok);
            if (f.return_ownership == BINDGEN_OWNERSHIP_BORROWED) {
                pos = buf_append(buf, pos, "bindgen-wrap-string-return-borrowed ", ok);
            } else {
                pos = buf_append(buf, pos, "bindgen-wrap-string-return-unknown ", ok);
            }
            pos = bindgen_append_facade_return_name(buf, pos, f, ok);
            pos = bindgen_append_metadata_literal_args(
                buf,
                pos,
                f.return_role,
                f.return_ownership,
                f.return_nullability,
                f.return_teardown,
                ok
            );
            return buf_append_char(buf, pos, ')', ok);
        case BINDGEN_ROLE_OPAQUE_POINTER:
            pos = buf_append(buf, pos, "(", ok);
            if (f.return_teardown == BINDGEN_TEARDOWN_MANUAL_REVIEW) {
                pos = buf_append(buf, pos, "bindgen-wrap-opaque-return-manual-review ", ok);
            } else {
                pos = buf_append(buf, pos, "bindgen-wrap-opaque-return-none ", ok);
            }
            pos = bindgen_append_facade_return_name(buf, pos, f, ok);
            pos = bindgen_append_metadata_literal_args(
                buf,
                pos,
                f.return_role,
                f.return_ownership,
                f.return_nullability,
                f.return_teardown,
                ok
            );
            return buf_append_char(buf, pos, ')', ok);
        case BINDGEN_ROLE_VALUE:
        case BINDGEN_ROLE_STRING_INPUT:
        case BINDGEN_ROLE_STRING_BUFFER:
        case BINDGEN_ROLE_CALLBACK:
            return bindgen_append_facade_return_name(buf, pos, f, ok);
    }
}
