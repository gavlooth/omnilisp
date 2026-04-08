module lisp;

import std::io;

// =============================================================================
// FFI Binding Generator — takes ParsedFunc[] + library info, writes Omni module.
// Generates (define [ffi lib])/(define [ffi λ]) declarations.
// =============================================================================

const usz BINDGEN_BUF_SIZE = 32768;

/**
 * Generate an Omni raw FFI module file from parsed C function declarations.
 *
 * Output format:
 *   (module ffi-{name}-raw (export fn1 fn2 ...)
 *     (define [ffi lib] _lib "lib{library}.so")
 *     (define [ffi λ _lib] (fn1 (^Type arg0) ...) ^RetType)
 *   )
 */
fn bool generate_ffi_module(
    char[] module_name,    // e.g., "math"
    char[] so_name,        // e.g., "m" (without lib prefix/.so suffix)
    ParsedFunc* funcs,
    usz func_count,
    char[] output_path
) {
    char[BINDGEN_BUF_SIZE] buf;
    usz pos = 0;
    bool ok = true;

    pos = bindgen_append_raw_header(buf[..], pos, so_name, &ok);
    pos = bindgen_append_module_open(buf[..], pos, module_name, "-raw", funcs, func_count, &ok);
    pos = bindgen_append_library_handle(buf[..], pos, so_name, &ok);
    pos = bindgen_append_functions(buf[..], pos, funcs, func_count, &ok);
    pos = buf_append(buf[..], pos, ")\n", &ok); // close module
    if (!ok) return false;
    return bindgen_write_text_file(output_path, buf[:pos]);
}

fn bool generate_ffi_facade_module_if_missing(
    char[] module_name,
    char[] so_name,
    ParsedFunc* funcs,
    usz func_count,
    char[] output_path
) {
    if (bindgen_output_file_exists(output_path)) return true;

    char[BINDGEN_BUF_SIZE] buf;
    usz pos = 0;
    bool ok = true;

    pos = bindgen_append_facade_header(buf[..], pos, so_name, &ok);
    pos = bindgen_append_module_open(buf[..], pos, module_name, "", funcs, func_count, &ok);
    pos = bindgen_append_facade_import(buf[..], pos, module_name, &ok);
    pos = bindgen_append_facade_alias_import(buf[..], pos, module_name, funcs, func_count, &ok);
    pos = buf_append(buf[..], pos, "  ;; Edit this facade to add Omni-facing naming, ownership, or buffer cleanup.\n", &ok);
    pos = bindgen_append_facade_wrappers(buf[..], pos, funcs, func_count, &ok);
    pos = buf_append(buf[..], pos, ")\n", &ok);
    if (!ok) return false;
    return bindgen_write_text_file(output_path, buf[:pos]);
}

fn bool generate_ffi_module_pair(
    char[] module_name,
    char[] so_name,
    ParsedFunc* funcs,
    usz func_count,
    char[] raw_output_path,
    char[] facade_output_path
) {
    if (!generate_ffi_module(module_name, so_name, funcs, func_count, raw_output_path)) return false;
    return generate_ffi_facade_module_if_missing(module_name, so_name, funcs, func_count, facade_output_path);
}

fn bool bindgen_write_text_file(char[] output_path, char[] text) {
    if (try file = io::file::open((String)output_path, "w")) {
        char[] remaining = text;
        bool write_ok = true;
        while (write_ok && remaining.len > 0) {
            usz? written_opt = file.write(remaining);
            if (catch err = written_opt) {
                write_ok = false;
                break;
            }
            if (written_opt == 0) {
                write_ok = false;
                break;
            }
            remaining = remaining[written_opt..];
        }
        if (catch err = file.close()) write_ok = false;
        return write_ok;
    }
    return false;
}

fn bool bindgen_output_file_exists(char[] output_path) {
    if (try file = io::file::open((String)output_path, "r")) {
        (void)file.close();
        return true;
    }
    return false;
}

fn usz bindgen_append_raw_header(char[] buf, usz pos, char[] so_name, bool* ok) {
    pos = buf_append(buf, pos, ";; Auto-generated raw FFI bindings for lib", ok);
    pos = buf_append(buf, pos, so_name, ok);
    pos = buf_append(buf, pos, "\n;; Regenerate with: omni --bind\n", ok);
    pos = buf_append(buf, pos, ";; This file is regenerated on every bind run.\n\n", ok);
    return pos;
}

fn usz bindgen_append_facade_header(char[] buf, usz pos, char[] so_name, bool* ok) {
    pos = buf_append(buf, pos, ";; Auto-generated FFI facade stub for lib", ok);
    pos = buf_append(buf, pos, so_name, ok);
    pos = buf_append(buf, pos, "\n;; Raw bindings live in the sibling *_raw.omni file.\n", ok);
    pos = buf_append(buf, pos, ";; This facade is created only when missing so local edits survive reruns.\n\n", ok);
    return pos;
}

fn usz bindgen_append_module_open(char[] buf, usz pos, char[] module_name, char[] module_suffix, ParsedFunc* funcs, usz func_count, bool* ok) {
    pos = buf_append(buf, pos, "(module ffi-", ok);
    pos = buf_append(buf, pos, module_name, ok);
    pos = buf_append(buf, pos, module_suffix, ok);
    pos = buf_append(buf, pos, " (export", ok);

    // Export list — only non-variadic functions.
    for (usz i = 0; i < func_count; i++) {
        if (funcs[i].is_variadic) continue;
        pos = buf_append_char(buf, pos, ' ', ok);
        pos = buf_append_kebab(buf, pos, &funcs[i].name, ok);
    }
    pos = buf_append(buf, pos, ")\n\n", ok);
    return pos;
}

fn usz bindgen_append_facade_import(char[] buf, usz pos, char[] module_name, bool* ok) {
    pos = buf_append(buf, pos, "  (import \"", ok);
    pos = buf_append(buf, pos, module_name, ok);
    pos = buf_append(buf, pos, "_raw.omni\")\n", ok);
    return pos;
}

fn usz bindgen_append_facade_alias_import(char[] buf, usz pos, char[] module_name, ParsedFunc* funcs, usz func_count, bool* ok) {
    usz export_count = 0;
    for (usz i = 0; i < func_count; i++) {
        if (!funcs[i].is_variadic) export_count++;
    }
    if (export_count == 0) return pos;

    pos = buf_append(buf, pos, "  (import ffi-", ok);
    pos = buf_append(buf, pos, module_name, ok);
    pos = buf_append(buf, pos, "-raw (", ok);
    usz written = 0;
    for (usz i = 0; i < func_count; i++) {
        ParsedFunc* f = &funcs[i];
        if (f.is_variadic) continue;
        pos = buf_append(buf, pos, "(", ok);
        pos = buf_append_kebab(buf, pos, &f.name, ok);
        pos = buf_append(buf, pos, " 'as raw-", ok);
        pos = buf_append_kebab(buf, pos, &f.name, ok);
        pos = buf_append(buf, pos, ")", ok);
        written++;
        if (written < export_count) pos = buf_append_char(buf, pos, ' ', ok);
    }
    pos = buf_append(buf, pos, "))\n\n", ok);
    return pos;
}

fn usz bindgen_append_library_handle(char[] buf, usz pos, char[] so_name, bool* ok) {
    pos = buf_append(buf, pos, "  (define [ffi lib] _lib \"lib", ok);
    pos = buf_append(buf, pos, so_name, ok);
    pos = buf_append(buf, pos, ".so\")\n\n", ok);
    return pos;
}

fn usz bindgen_append_variadic_skip(char[] buf, usz pos, ParsedFunc* f, bool* ok) {
    pos = buf_append(buf, pos, "  ;; skipped: ", ok);
    pos = buf_append_cstr(buf, pos, &f.name, ok);
    pos = buf_append(buf, pos, " (variadic)\n", ok);
    return pos;
}

fn bool bindgen_metadata_is_trivial(BindgenRoleTag role, BindgenOwnershipTag ownership,
    BindgenNullabilityTag nullability, BindgenTeardownTag teardown) {
    return role == BINDGEN_ROLE_VALUE &&
        ownership == BINDGEN_OWNERSHIP_NONE &&
        nullability == BINDGEN_NULLABILITY_NA &&
        teardown == BINDGEN_TEARDOWN_NONE;
}

fn bool bindgen_function_has_nontrivial_metadata(ParsedFunc* f) {
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
    return buf_append_cstr(buf, pos, &f.params[(usz)p.buffer_size_param_index].name, ok);
}

fn usz bindgen_append_metadata_shape_fields(char[] buf, usz pos, char* c_type, char* ffi_type, char* omni_type,
    BindgenRoleTag role, BindgenOwnershipTag ownership, BindgenNullabilityTag nullability, BindgenTeardownTag teardown, bool* ok) {
    pos = buf_append(buf, pos, " c-type=", ok);
    pos = buf_append_cstr(buf, pos, c_type, ok);
    pos = buf_append(buf, pos, " ffi-type=", ok);
    pos = buf_append_cstr(buf, pos, ffi_type, ok);
    pos = buf_append(buf, pos, " omni-type=", ok);
    pos = buf_append_cstr(buf, pos, omni_type, ok);
    return bindgen_append_metadata_fields(buf, pos, role, ownership, nullability, teardown, ok);
}

fn usz bindgen_append_metadata_shape_record(char[] buf, usz pos, ParsedFunc* f, bool* ok) {
    if (!bindgen_function_has_nontrivial_metadata(f)) return pos;

    pos = buf_append(buf, pos, "  ;; bindgen-meta-record: function c-symbol=", ok);
    pos = buf_append_cstr(buf, pos, &f.name, ok);
    pos = buf_append(buf, pos, " omni-name=", ok);
    pos = buf_append_kebab(buf, pos, &f.name, ok);
    pos = buf_append_char(buf, pos, '\n', ok);

    pos = buf_append(buf, pos, "  ;; bindgen-meta-record: return", ok);
    pos = bindgen_append_metadata_shape_fields(
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
        pos = buf_append(buf, pos, "  ;; bindgen-meta-record: param name=", ok);
        pos = buf_append_cstr(buf, pos, &p.name, ok);
        pos = bindgen_append_metadata_shape_fields(
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

    if (!bindgen_metadata_is_trivial(f.return_role, f.return_ownership, f.return_nullability, f.return_teardown)) {
        pos = buf_append(buf, pos, "  ;; bindgen-meta: return c-type=", ok);
        pos = buf_append_cstr(buf, pos, &f.return_c_type, ok);
        pos = bindgen_append_metadata_fields(buf, pos, f.return_role, f.return_ownership, f.return_nullability, f.return_teardown, ok);
        pos = buf_append_char(buf, pos, '\n', ok);
    }

    for (usz i = 0; i < f.param_count; i++) {
        ParsedParam* p = &f.params[i];
        if (bindgen_metadata_is_trivial(p.role, p.ownership, p.nullability, p.teardown)) continue;
        pos = buf_append(buf, pos, "  ;; bindgen-meta: param ", ok);
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

fn usz bindgen_append_function_decl(char[] buf, usz pos, ParsedFunc* f, bool* ok) {
    // (define [ffi λ _lib] (kebab-name (^Type arg0) ...) ^RetType)
    pos = bindgen_append_function_metadata(buf, pos, f, ok);
    pos = buf_append(buf, pos, "  (define [ffi lambda _lib] (", ok);
    pos = buf_append_kebab(buf, pos, &f.name, ok);

    for (usz j = 0; j < f.param_count; j++) {
        ParsedParam* p = &f.params[j];
        pos = buf_append(buf, pos, " (^", ok);
        pos = buf_append_cstr(buf, pos, &p.omni_type, ok);
        pos = buf_append_char(buf, pos, ' ', ok);
        pos = buf_append_cstr(buf, pos, &p.name, ok);
        pos = buf_append_char(buf, pos, ')', ok);
    }

    pos = buf_append(buf, pos, ")", ok);
    pos = buf_append(buf, pos, " ^", ok);
    pos = buf_append_cstr(buf, pos, &f.return_omni_type, ok);
    pos = buf_append(buf, pos, ")\n\n", ok);
    return pos;
}

fn usz bindgen_append_functions(char[] buf, usz pos, ParsedFunc* funcs, usz func_count, bool* ok) {
    for (usz i = 0; i < func_count; i++) {
        ParsedFunc* f = &funcs[i];
        if (f.is_variadic) {
            pos = bindgen_append_variadic_skip(buf, pos, f, ok);
            continue;
        }
        pos = bindgen_append_function_decl(buf, pos, f, ok);
    }
    return pos;
}

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

fn bool bindgen_param_needs_callback_unregister(ParsedParam* p) {
    return p.role == BINDGEN_ROLE_CALLBACK && p.teardown == BINDGEN_TEARDOWN_CALLBACK_UNREGISTER;
}

fn bool bindgen_param_needs_buffer_manual_review(ParsedParam* p) {
    return p.role == BINDGEN_ROLE_STRING_BUFFER && p.teardown == BINDGEN_TEARDOWN_MANUAL_REVIEW;
}

fn bool bindgen_param_needs_buffer_none(ParsedParam* p) {
    return p.role == BINDGEN_ROLE_STRING_BUFFER && p.teardown == BINDGEN_TEARDOWN_NONE;
}

fn void bindgen_collect_facade_helper_flags(ParsedFunc* funcs, usz func_count,
    bool* need_buffer_helper, bool* need_buffer_manual_review_helper, bool* need_buffer_none_helper,
    bool* need_callback_helper, bool* need_opaque_arg_helper, bool* need_string_return_helper,
    bool* need_opaque_return_helper, bool* need_callback_cleanup_helper) {
    *need_buffer_helper = false;
    *need_buffer_manual_review_helper = false;
    *need_buffer_none_helper = false;
    *need_callback_helper = false;
    *need_opaque_arg_helper = false;
    *need_string_return_helper = false;
    *need_opaque_return_helper = false;
    *need_callback_cleanup_helper = false;

    for (usz i = 0; i < func_count; i++) {
        ParsedFunc* f = &funcs[i];
        if (f.is_variadic || !bindgen_function_needs_facade_scaffold(f)) continue;

        if (f.return_role == BINDGEN_ROLE_STRING_RETURN) *need_string_return_helper = true;
        if (f.return_role == BINDGEN_ROLE_OPAQUE_POINTER) *need_opaque_return_helper = true;

        for (usz j = 0; j < f.param_count; j++) {
            switch (f.params[j].role) {
                case BINDGEN_ROLE_STRING_BUFFER:
                    *need_buffer_helper = true;
                    if (bindgen_param_needs_buffer_manual_review(&f.params[j])) {
                        *need_buffer_manual_review_helper = true;
                    } else if (bindgen_param_needs_buffer_none(&f.params[j])) {
                        *need_buffer_none_helper = true;
                    }
                    break;
                case BINDGEN_ROLE_CALLBACK:
                    *need_callback_helper = true;
                    if (f.params[j].teardown == BINDGEN_TEARDOWN_CALLBACK_UNREGISTER) {
                        *need_callback_cleanup_helper = true;
                    }
                    break;
                case BINDGEN_ROLE_OPAQUE_POINTER:
                    *need_opaque_arg_helper = true;
                    break;
                case BINDGEN_ROLE_VALUE:
                case BINDGEN_ROLE_STRING_INPUT:
                case BINDGEN_ROLE_STRING_RETURN:
                    break;
            }
        }
    }
}

fn usz bindgen_append_facade_helper_stubs(char[] buf, usz pos, ParsedFunc* funcs, usz func_count, bool* ok) {
    bool need_buffer_helper;
    bool need_buffer_manual_review_helper;
    bool need_buffer_none_helper;
    bool need_callback_helper;
    bool need_opaque_arg_helper;
    bool need_string_return_helper;
    bool need_opaque_return_helper;
    bool need_callback_cleanup_helper;
    bindgen_collect_facade_helper_flags(funcs, func_count, &need_buffer_helper, &need_buffer_manual_review_helper,
        &need_buffer_none_helper, &need_callback_helper, &need_opaque_arg_helper, &need_string_return_helper,
        &need_opaque_return_helper, &need_callback_cleanup_helper);

    if (!need_buffer_helper && !need_callback_helper && !need_opaque_arg_helper &&
        !need_string_return_helper && !need_opaque_return_helper && !need_callback_cleanup_helper) {
        return pos;
    }

    pos = buf_append(buf, pos, "  ;; Generated helper templates for non-trivial FFI contracts.\n", ok);
    if (need_buffer_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-validate-string-buffer-size buffer-size direction)\n", ok);
        pos = buf_append(buf, pos, "    ;; Concrete policy: mutable out/inout buffers require a positive integer size input.\n", ok);
        pos = buf_append(buf, pos, "    (if (== direction \"in\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (if (== direction \"out\")\n", ok);
        pos = buf_append(buf, pos, "            nil\n", ok);
        pos = buf_append(buf, pos, "            (if (== direction \"inout\")\n", ok);
        pos = buf_append(buf, pos, "                nil\n", ok);
        pos = buf_append(buf, pos, "                (raise \"bindgen/string-buffer-direction-mismatch\"))))\n", ok);
        pos = buf_append(buf, pos, "    (if (== direction \"in\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (if (nil? buffer-size)\n", ok);
        pos = buf_append(buf, pos, "            (raise \"bindgen/string-buffer-size-required\")\n", ok);
        pos = buf_append(buf, pos, "            nil))\n", ok);
        pos = buf_append(buf, pos, "    (if (== direction \"in\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (if (int? buffer-size)\n", ok);
        pos = buf_append(buf, pos, "            nil\n", ok);
        pos = buf_append(buf, pos, "            (raise \"bindgen/string-buffer-size-type-mismatch\")))\n", ok);
        pos = buf_append(buf, pos, "    (if (== direction \"in\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (if (> buffer-size 0)\n", ok);
        pos = buf_append(buf, pos, "            nil\n", ok);
        pos = buf_append(buf, pos, "            (raise \"bindgen/string-buffer-size-nonpositive\")))\n", ok);
        pos = buf_append(buf, pos, "    nil\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);

        pos = buf_append(buf, pos, "  (define (bindgen-prepare-string-buffer buffer buffer-size role ownership nullability teardown direction)\n", ok);
        pos = buf_append(buf, pos, "    ;; Fail closed on nil buffers so wrapper edits cannot silently pass invalid pointer state.\n", ok);
        pos = buf_append(buf, pos, "    (if (nil? buffer)\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-buffer-nil\")\n", ok);
        pos = buf_append(buf, pos, "        nil)\n", ok);
        pos = buf_append(buf, pos, "    ;; metadata arguments keep role/ownership/nullability/teardown and direction explicit at the wrapper boundary.\n", ok);
        pos = buf_append(buf, pos, "    (bindgen-validate-string-buffer-size buffer-size direction)\n", ok);
        if (need_buffer_manual_review_helper && need_buffer_none_helper) {
            pos = buf_append(buf, pos, "    (if (== teardown \"manual-review\")\n", ok);
            pos = buf_append(buf, pos, "        (bindgen-prepare-string-buffer-manual-review buffer buffer-size role ownership nullability teardown direction)\n", ok);
            pos = buf_append(buf, pos, "        (if (== teardown \"none\")\n", ok);
            pos = buf_append(buf, pos, "            (bindgen-prepare-string-buffer-none buffer buffer-size role ownership nullability teardown direction)\n", ok);
            pos = buf_append(buf, pos, "            (raise \"bindgen/string-buffer-unsupported-teardown\")))\n", ok);
        } else if (need_buffer_manual_review_helper) {
            pos = buf_append(buf, pos, "    (if (== teardown \"manual-review\")\n", ok);
            pos = buf_append(buf, pos, "        (bindgen-prepare-string-buffer-manual-review buffer buffer-size role ownership nullability teardown direction)\n", ok);
            pos = buf_append(buf, pos, "        (raise \"bindgen/string-buffer-unsupported-teardown\"))\n", ok);
        } else if (need_buffer_none_helper) {
            pos = buf_append(buf, pos, "    (if (== teardown \"none\")\n", ok);
            pos = buf_append(buf, pos, "        (bindgen-prepare-string-buffer-none buffer buffer-size role ownership nullability teardown direction)\n", ok);
            pos = buf_append(buf, pos, "        (raise \"bindgen/string-buffer-unsupported-teardown\"))\n", ok);
        } else {
            pos = buf_append(buf, pos, "    (raise \"bindgen/string-buffer-policy-missing\")\n", ok);
        }
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_buffer_manual_review_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-prepare-string-buffer-manual-review buffer buffer-size role ownership nullability teardown direction)\n", ok);
        pos = buf_append(buf, pos, "    ;; Fail closed if metadata does not match the expected mutable string-buffer contract.\n", ok);
        pos = buf_append(buf, pos, "    (if (== role \"string-buffer\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-buffer-role-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== ownership \"borrowed\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/string-buffer-ownership-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    ;; REVIEW(bindgen): manual-review teardown still requires explicit allocation/writeback policy edits.\n", ok);
        pos = buf_append(buf, pos, "    buffer\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_buffer_none_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-prepare-string-buffer-none buffer buffer-size role ownership nullability teardown direction)\n", ok);
        pos = buf_append(buf, pos, "    ;; none teardown keeps pointer ownership with the caller after size/direction validation.\n", ok);
        pos = buf_append(buf, pos, "    buffer\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_callback_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-register-callback callback role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; Fail closed when callback metadata does not match the shipped shim-only handle contract.\n", ok);
        pos = buf_append(buf, pos, "    (if (nil? callback)\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/callback-nil\")\n", ok);
        pos = buf_append(buf, pos, "        nil)\n", ok);
        pos = buf_append(buf, pos, "    (if (== role \"callback\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/callback-role-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== ownership \"borrowed\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/callback-ownership-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== teardown \"callback-unregister\")\n", ok);
        pos = buf_append(buf, pos, "        (__raw-uv-timer-callback-handle callback)\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/callback-unsupported-teardown\"))\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_callback_cleanup_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-unregister-callback callback-handle role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; Fail closed for unknown callback teardown contracts; tolerate nil so wrapper cleanup can be idempotent.\n", ok);
        pos = buf_append(buf, pos, "    (if (nil? callback-handle)\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (if (== role \"callback\")\n", ok);
        pos = buf_append(buf, pos, "            nil\n", ok);
        pos = buf_append(buf, pos, "            (raise \"bindgen/callback-role-mismatch\")))\n", ok);
        pos = buf_append(buf, pos, "    (if (== ownership \"borrowed\")\n", ok);
        pos = buf_append(buf, pos, "        nil\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/callback-ownership-mismatch\"))\n", ok);
        pos = buf_append(buf, pos, "    (if (== teardown \"callback-unregister\")\n", ok);
        pos = buf_append(buf, pos, "        (__raw-uv-timer-callback-unhandle callback-handle)\n", ok);
        pos = buf_append(buf, pos, "        (raise \"bindgen/callback-unsupported-teardown\"))\n", ok);
        pos = buf_append(buf, pos, "    nil\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_opaque_arg_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-wrap-opaque-arg handle role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; TODO(bindgen): normalize opaque argument ownership/lifetime policy at this boundary.\n", ok);
        pos = buf_append(buf, pos, "    ;; metadata arguments keep pointer policy explicit for wrapper hardening.\n", ok);
        pos = buf_append(buf, pos, "    handle\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_string_return_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-wrap-string-return-borrowed text role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; TODO(bindgen): borrowed string policy path; decide copy-vs-borrow lifetime boundary behavior.\n", ok);
        pos = buf_append(buf, pos, "    ;; metadata arguments keep nullability/teardown visible while this is still staged.\n", ok);
        pos = buf_append(buf, pos, "    text\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
        pos = buf_append(buf, pos, "  (define (bindgen-wrap-string-return-unknown text role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; TODO(bindgen): unknown string ownership path; require explicit policy before broad API exposure.\n", ok);
        pos = buf_append(buf, pos, "    ;; metadata arguments keep contract ambiguity explicit instead of silently coercing.\n", ok);
        pos = buf_append(buf, pos, "    text\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    if (need_opaque_return_helper) {
        pos = buf_append(buf, pos, "  (define (bindgen-wrap-opaque-return-manual-review handle role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; TODO(bindgen): manual-review opaque return path; attach handle/finalizer policy here.\n", ok);
        pos = buf_append(buf, pos, "    ;; metadata arguments keep ownership/nullability context explicit at this boundary.\n", ok);
        pos = buf_append(buf, pos, "    handle\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
        pos = buf_append(buf, pos, "  (define (bindgen-wrap-opaque-return-none handle role ownership nullability teardown)\n", ok);
        pos = buf_append(buf, pos, "    ;; TODO(bindgen): no-teardown opaque return path; confirm handle contract before broad exposure.\n", ok);
        pos = buf_append(buf, pos, "    ;; metadata arguments keep return policy explicit for staged hardening.\n", ok);
        pos = buf_append(buf, pos, "    handle\n", ok);
        pos = buf_append(buf, pos, "  )\n\n", ok);
    }
    return pos;
}

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

fn usz bindgen_append_facade_review_issue(char[] buf, usz pos, char[] text, bool* wrote_issue, bool* ok) {
    if (*wrote_issue) pos = buf_append(buf, pos, "; ", ok);
    pos = buf_append(buf, pos, text, ok);
    *wrote_issue = true;
    return pos;
}

fn usz bindgen_append_facade_review_note(char[] buf, usz pos, ParsedFunc* f, bool* ok) {
    pos = buf_append(buf, pos, "  ;; TODO(bindgen): review ", ok);
    pos = buf_append_kebab(buf, pos, &f.name, ok);
    pos = buf_append(buf, pos, " — ", ok);
    bool wrote_issue = false;

    if (f.return_role == BINDGEN_ROLE_STRING_RETURN) {
        pos = bindgen_append_facade_review_issue(
            buf, pos, "string return ownership/nullability is not automatic", &wrote_issue, ok);
    }
    if (f.return_role == BINDGEN_ROLE_OPAQUE_POINTER) {
        pos = bindgen_append_facade_review_issue(
            buf, pos, "opaque return ownership/finalizer policy needs an Omni wrapper", &wrote_issue, ok);
    }
    for (usz i = 0; i < f.param_count; i++) {
        ParsedParam* p = &f.params[i];
        switch (p.role) {
            case BINDGEN_ROLE_STRING_BUFFER:
                pos = bindgen_append_facade_review_issue(
                    buf, pos, "mutable char* buffer direction/size must be wrapped explicitly", &wrote_issue, ok);
                break;
            case BINDGEN_ROLE_CALLBACK:
                pos = bindgen_append_facade_review_issue(
                    buf, pos, "callback registration and teardown must be wrapped explicitly", &wrote_issue, ok);
                break;
            case BINDGEN_ROLE_OPAQUE_POINTER:
                pos = bindgen_append_facade_review_issue(
                    buf, pos, "opaque pointer ownership/lifetime must be reviewed", &wrote_issue, ok);
                break;
            case BINDGEN_ROLE_STRING_INPUT:
                // keep looking for a stronger review surface first
                break;
            case BINDGEN_ROLE_STRING_RETURN:
                break;
            case BINDGEN_ROLE_VALUE:
                break;
        }
    }
    if (!wrote_issue) {
        pos = buf_append(buf, pos, "non-trivial FFI ownership/nullability metadata needs review before exposing it as stable Omni API.", ok);
    } else {
        pos = buf_append_char(buf, pos, '.', ok);
    }
    return buf_append_char(buf, pos, '\n', ok);
}

fn usz bindgen_append_facade_wrapper_body_notes(char[] buf, usz pos, char[] indent, ParsedFunc* f, bool* ok) {
    if (f.return_role == BINDGEN_ROLE_STRING_RETURN) {
        pos = buf_append(buf, pos, indent, ok);
        pos = buf_append(buf, pos, ";; REVIEW: decide whether to copy, borrow, or null-check the returned string.\n", ok);
    }
    if (f.return_role == BINDGEN_ROLE_OPAQUE_POINTER) {
        pos = buf_append(buf, pos, indent, ok);
        pos = buf_append(buf, pos, ";; REVIEW: wrap opaque returns in a handle/finalizer policy before exposing them broadly.\n", ok);
    }
    for (usz i = 0; i < f.param_count; i++) {
        ParsedParam* p = &f.params[i];
        switch (p.role) {
            case BINDGEN_ROLE_STRING_INPUT:
                break;
            case BINDGEN_ROLE_STRING_BUFFER:
                pos = buf_append(buf, pos, indent, ok);
                pos = buf_append(buf, pos, ";; REVIEW: mutable char* buffers need explicit allocation, size, and output semantics.\n", ok);
                break;
            case BINDGEN_ROLE_STRING_RETURN:
                break;
            case BINDGEN_ROLE_OPAQUE_POINTER:
                pos = buf_append(buf, pos, indent, ok);
                pos = buf_append(buf, pos, ";; REVIEW: confirm opaque pointer ownership/lifetime at the Omni boundary.\n", ok);
                break;
            case BINDGEN_ROLE_CALLBACK:
                pos = buf_append(buf, pos, indent, ok);
                pos = buf_append(buf, pos, ";; REVIEW: confirm callback-handle registration/unregister policy matches the target subsystem contract.\n", ok);
                break;
            case BINDGEN_ROLE_VALUE:
                break;
        }
    }
    return pos;
}

// --- Buffer helpers ---

fn usz buf_append(char[] buf, usz pos, char[] str, bool* ok) {
    usz i = 0;
    for (; i < str.len && pos < buf.len - 1; i++) {
        buf[pos] = str[i];
        pos++;
    }
    if (i < str.len) *ok = false;
    return pos;
}

fn usz buf_append_char(char[] buf, usz pos, char c, bool* ok) {
    if (pos < buf.len - 1) {
        buf[pos] = c;
        pos++;
    } else {
        *ok = false;
    }
    return pos;
}

fn usz buf_append_cstr(char[] buf, usz pos, void* cstr_ptr, bool* ok) {
    char* s = (char*)cstr_ptr;
    usz i = 0;
    while (s[i] != 0 && pos < buf.len - 1) {
        buf[pos] = s[i];
        pos++;
        i++;
    }
    if (s[i] != 0) *ok = false;
    return pos;
}

/**
 * Append a C identifier converted to kebab-case.
 * snake_case → kebab-case: replace '_' with '-'
 */
fn usz buf_append_kebab(char[] buf, usz pos, void* cstr_ptr, bool* ok) {
    char* s = (char*)cstr_ptr;
    usz i = 0;
    while (s[i] != 0 && pos < buf.len - 1) {
        if (s[i] == '_') {
            buf[pos] = '-';
        } else {
            buf[pos] = s[i];
        }
        pos++;
        i++;
    }
    if (s[i] != 0) *ok = false;
    return pos;
}
