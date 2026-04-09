module lisp;

fn usz bindgen_append_facade_wrappers(char[] buf, usz pos, ParsedFunc* funcs, usz func_count, bool* ok) {
    pos = bindgen_append_facade_helper_stubs(buf, pos, funcs, func_count, ok);
    usz wrapper_count = 0;
    for (usz i = 0; i < func_count; i++) {
        ParsedFunc* f = &funcs[i];
        if (f.is_variadic) continue;
        if (bindgen_function_needs_facade_scaffold(f)) {
            pos = bindgen_append_facade_wrapper_scaffold(buf, pos, f, ok);
        } else {
            pos = bindgen_append_facade_alias_wrapper(buf, pos, f, ok);
        }
        wrapper_count++;
    }
    if (wrapper_count > 0) pos = buf_append_char(buf, pos, '\n', ok);
    return pos;
}

fn usz bindgen_append_facade_alias_wrapper(char[] buf, usz pos, ParsedFunc* f, bool* ok) {
    pos = buf_append(buf, pos, "  (define ", ok);
    pos = buf_append_kebab(buf, pos, &f.name, ok);
    pos = buf_append(buf, pos, " raw-", ok);
    pos = buf_append_kebab(buf, pos, &f.name, ok);
    pos = buf_append(buf, pos, ")\n", ok);
    return pos;
}

fn usz bindgen_append_facade_param_list(char[] buf, usz pos, ParsedFunc* f, bool* ok) {
    for (usz i = 0; i < f.param_count; i++) {
        ParsedParam* p = &f.params[i];
        pos = buf_append(buf, pos, " (^", ok);
        pos = buf_append_cstr(buf, pos, &p.omni_type, ok);
        pos = buf_append_char(buf, pos, ' ', ok);
        pos = buf_append_cstr(buf, pos, &p.name, ok);
        pos = buf_append_char(buf, pos, ')', ok);
    }
    return pos;
}

fn usz bindgen_append_facade_raw_call(char[] buf, usz pos, char[] indent, ParsedFunc* f, bool* ok) {
    pos = buf_append(buf, pos, indent, ok);
    pos = buf_append(buf, pos, "(raw-", ok);
    pos = buf_append_kebab(buf, pos, &f.name, ok);
    for (usz i = 0; i < f.param_count; i++) {
        pos = buf_append_char(buf, pos, ' ', ok);
        pos = bindgen_append_facade_arg_name(buf, pos, &f.params[i], ok);
    }
    pos = buf_append(buf, pos, ")\n", ok);
    return pos;
}

fn usz bindgen_append_facade_arg_name(char[] buf, usz pos, ParsedParam* p, bool* ok) {
    switch (p.role) {
        case BINDGEN_ROLE_STRING_BUFFER:
            pos = buf_append_cstr(buf, pos, &p.name, ok);
            return buf_append(buf, pos, "_input", ok);
        case BINDGEN_ROLE_OPAQUE_POINTER:
        case BINDGEN_ROLE_CALLBACK:
            pos = buf_append_cstr(buf, pos, &p.name, ok);
            return buf_append(buf, pos, "_handle", ok);
        case BINDGEN_ROLE_VALUE:
        case BINDGEN_ROLE_STRING_INPUT:
        case BINDGEN_ROLE_STRING_RETURN:
            return buf_append_cstr(buf, pos, &p.name, ok);
    }
}

fn bool bindgen_function_has_meaningful_return(ParsedFunc* f) {
    return !cstr_eq(&f.return_omni_type, "Void");
}

fn usz bindgen_append_facade_return_name(char[] buf, usz pos, ParsedFunc* f, bool* ok) {
    switch (f.return_role) {
        case BINDGEN_ROLE_STRING_RETURN:
            return buf_append(buf, pos, "result_text", ok);
        case BINDGEN_ROLE_OPAQUE_POINTER:
            return buf_append(buf, pos, "result_handle", ok);
        case BINDGEN_ROLE_VALUE:
        case BINDGEN_ROLE_STRING_INPUT:
        case BINDGEN_ROLE_STRING_BUFFER:
        case BINDGEN_ROLE_CALLBACK:
            return buf_append(buf, pos, "result", ok);
    }
}

fn usz bindgen_append_facade_param_bindings(char[] buf, usz pos, ParsedFunc* f, bool* ok) {
    bool wrote_binding = false;
    for (usz i = 0; i < f.param_count; i++) {
        ParsedParam* p = &f.params[i];
        switch (p.role) {
            case BINDGEN_ROLE_STRING_BUFFER:
            case BINDGEN_ROLE_OPAQUE_POINTER:
            case BINDGEN_ROLE_CALLBACK:
                if (wrote_binding) pos = buf_append_char(buf, pos, ' ', ok);
                pos = bindgen_append_facade_arg_name(buf, pos, p, ok);
                pos = buf_append_char(buf, pos, ' ', ok);
                pos = bindgen_append_facade_arg_binding_value(buf, pos, f, p, ok);
                wrote_binding = true;
                break;
            case BINDGEN_ROLE_VALUE:
            case BINDGEN_ROLE_STRING_INPUT:
            case BINDGEN_ROLE_STRING_RETURN:
                break;
        }
    }
    return pos;
}

fn bool bindgen_function_needs_arg_bindings(ParsedFunc* f) {
    for (usz i = 0; i < f.param_count; i++) {
        BindgenRoleTag role = f.params[i].role;
        if (role == BINDGEN_ROLE_STRING_BUFFER || role == BINDGEN_ROLE_OPAQUE_POINTER || role == BINDGEN_ROLE_CALLBACK) {
            return true;
        }
    }
    return false;
}

fn bool bindgen_function_needs_return_alias(ParsedFunc* f) {
    return f.return_role == BINDGEN_ROLE_STRING_RETURN || f.return_role == BINDGEN_ROLE_OPAQUE_POINTER;
}

fn usz bindgen_append_facade_callback_cleanup(char[] buf, usz pos, char[] indent, ParsedFunc* f, bool* ok) {
    for (usz i = 0; i < f.param_count; i++) {
        ParsedParam* p = &f.params[i];
        if (!bindgen_param_needs_callback_unregister(p)) continue;
        pos = buf_append(buf, pos, indent, ok);
        pos = buf_append(buf, pos, "(bindgen-unregister-callback ", ok);
        pos = bindgen_append_facade_arg_name(buf, pos, p, ok);
        pos = bindgen_append_metadata_literal_args(
            buf,
            pos,
            p.role,
            p.ownership,
            p.nullability,
            p.teardown,
            ok
        );
        pos = buf_append(buf, pos, ")\n", ok);
    }
    return pos;
}

fn usz bindgen_append_facade_wrapper_scaffold(char[] buf, usz pos, ParsedFunc* f, bool* ok) {
    pos = bindgen_append_facade_review_note(buf, pos, f, ok);
    pos = buf_append(buf, pos, "  (define (", ok);
    pos = buf_append_kebab(buf, pos, &f.name, ok);
    pos = bindgen_append_facade_param_list(buf, pos, f, ok);
    pos = buf_append(buf, pos, ")\n", ok);
    if (bindgen_function_has_meaningful_return(f)) {
        bool needs_arg_bindings = bindgen_function_needs_arg_bindings(f);
        bool needs_return_alias = bindgen_function_needs_return_alias(f);
        pos = buf_append(buf, pos, "    (let (", ok);
        if (needs_arg_bindings) {
            bool wrote_binding = false;
            for (usz i = 0; i < f.param_count; i++) {
                ParsedParam* p = &f.params[i];
                switch (p.role) {
                    case BINDGEN_ROLE_STRING_BUFFER:
                    case BINDGEN_ROLE_OPAQUE_POINTER:
                    case BINDGEN_ROLE_CALLBACK:
                        if (wrote_binding) pos = buf_append(buf, pos, "\n          ", ok);
                        pos = bindgen_append_facade_arg_name(buf, pos, p, ok);
                        pos = buf_append_char(buf, pos, ' ', ok);
                        pos = bindgen_append_facade_arg_binding_value(buf, pos, f, p, ok);
                        wrote_binding = true;
                        break;
                    case BINDGEN_ROLE_VALUE:
                    case BINDGEN_ROLE_STRING_INPUT:
                    case BINDGEN_ROLE_STRING_RETURN:
                        break;
                }
            }
            pos = buf_append(buf, pos, "\n          ", ok);
        }
        pos = buf_append(buf, pos, "result ", ok);
        pos = buf_append(buf, pos, "(raw-", ok);
        pos = buf_append_kebab(buf, pos, &f.name, ok);
        for (usz i = 0; i < f.param_count; i++) {
            pos = buf_append_char(buf, pos, ' ', ok);
            pos = bindgen_append_facade_arg_name(buf, pos, &f.params[i], ok);
        }
        pos = buf_append_char(buf, pos, ')', ok);
        if (needs_return_alias) {
            pos = buf_append(buf, pos, "\n          ", ok);
            pos = bindgen_append_facade_return_name(buf, pos, f, ok);
            pos = buf_append(buf, pos, " result", ok);
        }
        pos = buf_append(buf, pos, ")\n", ok);
        pos = bindgen_append_facade_wrapper_body_notes(buf, pos, "      ", f, ok);
        pos = bindgen_append_facade_callback_cleanup(buf, pos, "      ", f, ok);
        pos = buf_append(buf, pos, "      ", ok);
        pos = bindgen_append_facade_return_expr(buf, pos, f, ok);
        pos = buf_append(buf, pos, ")\n", ok);
    } else {
        bool needs_arg_bindings = bindgen_function_needs_arg_bindings(f);
        if (needs_arg_bindings) {
            pos = buf_append(buf, pos, "    (let (", ok);
            pos = bindgen_append_facade_param_bindings(buf, pos, f, ok);
            pos = buf_append(buf, pos, ")\n", ok);
            pos = bindgen_append_facade_wrapper_body_notes(buf, pos, "      ", f, ok);
            pos = bindgen_append_facade_raw_call(buf, pos, "      ", f, ok);
            pos = bindgen_append_facade_callback_cleanup(buf, pos, "      ", f, ok);
            pos = buf_append(buf, pos, "    )\n", ok);
        } else {
            pos = bindgen_append_facade_wrapper_body_notes(buf, pos, "    ", f, ok);
            pos = bindgen_append_facade_raw_call(buf, pos, "    ", f, ok);
        }
    }
    pos = buf_append(buf, pos, "  )\n", ok);
    return pos;
}
