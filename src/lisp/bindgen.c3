module lisp;

// =============================================================================
// FFI Binding Generator — takes ParsedFunc[] + library info, writes Omni module.
// Generates grouped or legacy declarative FFI declarations.
// =============================================================================

const usz BINDGEN_BUF_SIZE = 32768;

enum BindgenRawSyntax : char {
    BINDGEN_RAW_SYNTAX_LEGACY,
    BINDGEN_RAW_SYNTAX_GROUPED,
}

fn bool bindgen_cstr_eq(char* text, char[] expected) {
    if (text == null) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (text[i] != expected[i]) return false;
    }
    return text[expected.len] == 0;
}

fn bool bindgen_library_name_char_is_safe(char ch) @inline {
    return ch > ' ' && ch != '"' && ch != '/' && ch != '\\';
}

fn bool bindgen_library_name_is_safe(char* text) {
    if (text == null || text[0] == 0) return false;
    for (usz i = 0; text[i] != 0; i++) {
        if (!bindgen_library_name_char_is_safe(text[i])) return false;
    }
    return true;
}

fn bool bindgen_library_name_slice_is_safe(char[] text) {
    if (text.len == 0) return false;
    for (usz i = 0; i < text.len; i++) {
        if (!bindgen_library_name_char_is_safe(text[i])) return false;
    }
    return true;
}

fn bool bindgen_raw_syntax_from_dep(TomlFfiDep* dep, BindgenRawSyntax* out) {
    if (dep == null || out == null) return false;
    char* raw_syntax = &dep.raw_syntax;
    if (raw_syntax[0] == 0 || bindgen_cstr_eq(raw_syntax, "legacy")) {
        *out = BINDGEN_RAW_SYNTAX_LEGACY;
        return true;
    }
    if (bindgen_cstr_eq(raw_syntax, "grouped")) {
        *out = BINDGEN_RAW_SYNTAX_GROUPED;
        return true;
    }
    return false;
}

fn char[] bindgen_raw_syntax_name(BindgenRawSyntax raw_syntax) {
    switch (raw_syntax) {
        case BINDGEN_RAW_SYNTAX_LEGACY:
            return "legacy";
        case BINDGEN_RAW_SYNTAX_GROUPED:
            return "grouped";
    }
}

fn usz bindgen_append_toml_escaped_char(char[] buf, usz pos, char ch, bool* ok) {
    switch (ch) {
        case '"':
            return buf_append(buf, pos, "\\\"", ok);
        case '\\':
            return buf_append(buf, pos, "\\\\", ok);
        case '\n':
            return buf_append(buf, pos, "\\n", ok);
        case '\r':
            return buf_append(buf, pos, "\\r", ok);
        case '\t':
            return buf_append(buf, pos, "\\t", ok);
        case (char)8:
            return buf_append(buf, pos, "\\b", ok);
        case (char)12:
            return buf_append(buf, pos, "\\f", ok);
        default:
            uint byte = (uint)ch & 0xFF;
            if (byte < 0x20) {
                char[] hex = "0123456789abcdef";
                pos = buf_append(buf, pos, "\\u00", ok);
                pos = buf_append_char(buf, pos, hex[(usz)((byte >> 4) & 0xF)], ok);
                return buf_append_char(buf, pos, hex[(usz)(byte & 0xF)], ok);
            }
            return buf_append_char(buf, pos, ch, ok);
    }
}

fn usz bindgen_append_toml_quoted(char[] buf, usz pos, char[] text, bool* ok) {
    pos = buf_append_char(buf, pos, '"', ok);
    for (usz i = 0; i < text.len; i++) {
        pos = bindgen_append_toml_escaped_char(buf, pos, text[i], ok);
    }
    return buf_append_char(buf, pos, '"', ok);
}

fn usz bindgen_append_toml_quoted_cstr(char[] buf, usz pos, char* text, bool* ok) {
    if (text == null) {
        *ok = false;
        return pos;
    }
    pos = buf_append_char(buf, pos, '"', ok);
    for (usz i = 0; text[i] != 0; i++) {
        pos = bindgen_append_toml_escaped_char(buf, pos, text[i], ok);
    }
    return buf_append_char(buf, pos, '"', ok);
}

fn bool generate_ffi_bindgen_manifest(
    char[] module_name,
    char[] so_name,
    char[] raw_output_path,
    char[] facade_output_path,
    char[] manifest_output_path,
    BindgenRawSyntax raw_syntax,
    char[64]* exclude_functions,
    usz exclude_function_count,
    char[64]* strip_prefixes,
    usz strip_prefix_count
) {
    if (exclude_function_count > 0 && exclude_functions == null) return false;
    if (strip_prefix_count > 0 && strip_prefixes == null) return false;
    if (!bindgen_library_name_slice_is_safe(so_name)) return false;

    char[BINDGEN_BUF_SIZE] buf;
    usz pos = 0;
    bool ok = true;

    pos = buf_append(buf[..], pos, "# Auto-generated FFI bindgen manifest.\n", &ok);
    pos = buf_append(buf[..], pos, "# Regenerate with: omni --bind\n", &ok);
    pos = buf_append(buf[..], pos, "format-version = \"1\"\n", &ok);
    pos = buf_append(buf[..], pos, "dependency = ", &ok);
    pos = bindgen_append_toml_quoted(buf[..], pos, module_name, &ok);
    pos = buf_append(buf[..], pos, "\n", &ok);
    pos = buf_append(buf[..], pos, "library = ", &ok);
    pos = bindgen_append_toml_quoted(buf[..], pos, so_name, &ok);
    pos = buf_append(buf[..], pos, "\n", &ok);
    pos = buf_append(buf[..], pos, "raw-syntax = ", &ok);
    pos = bindgen_append_toml_quoted(buf[..], pos, bindgen_raw_syntax_name(raw_syntax), &ok);
    pos = buf_append(buf[..], pos, "\n", &ok);
    pos = buf_append(buf[..], pos, "raw-output = ", &ok);
    pos = bindgen_append_toml_quoted(buf[..], pos, raw_output_path, &ok);
    pos = buf_append(buf[..], pos, "\n", &ok);
    pos = buf_append(buf[..], pos, "facade-output = ", &ok);
    pos = bindgen_append_toml_quoted(buf[..], pos, facade_output_path, &ok);
    pos = buf_append(buf[..], pos, "\n", &ok);
    pos = buf_append(buf[..], pos, "exclude-functions = [", &ok);
    for (usz i = 0; i < exclude_function_count; i++) {
        if (i > 0) pos = buf_append(buf[..], pos, ", ", &ok);
        pos = bindgen_append_toml_quoted_cstr(buf[..], pos, &exclude_functions[i], &ok);
    }
    pos = buf_append(buf[..], pos, "]\n", &ok);
    pos = buf_append(buf[..], pos, "strip-prefixes = [", &ok);
    for (usz i = 0; i < strip_prefix_count; i++) {
        if (i > 0) pos = buf_append(buf[..], pos, ", ", &ok);
        pos = bindgen_append_toml_quoted_cstr(buf[..], pos, &strip_prefixes[i], &ok);
    }
    pos = buf_append(buf[..], pos, "]\n", &ok);

    if (!ok) return false;
    return bindgen_write_text_file(manifest_output_path, buf[:pos]);
}

/**
 * Generate an Omni raw FFI module file from parsed C function declarations.
 *
 * Legacy output format:
 *   (module ffi-{name}-raw (export fn1 fn2 ...)
 *     (define [ffi lib] _lib "lib{library}.so")
 *     (define [ffi λ _lib] (fn1 (^Type arg0) ...) ^RetType)
 *   )
 *
 * Grouped output format:
 *   (module ffi-{name}-raw (export fn1 fn2 ...)
 *     (define [ffi module] _lib "lib{library}.so"
 *       (fn1 (^Type arg0) ...) ^RetType)
 *   )
 */
fn bool generate_ffi_module_with_syntax(
    char[] module_name,    // e.g., "math"
    char[] so_name,        // e.g., "m" (without lib prefix/.so suffix)
    ParsedFunc* funcs,
    usz func_count,
    char[] output_path,
    BindgenRawSyntax raw_syntax
) {
    return generate_ffi_module_with_options(
        module_name,
        so_name,
        funcs,
        func_count,
        output_path,
        raw_syntax,
        null,
        0
    );
}

fn bool generate_ffi_module_with_options(
    char[] module_name,
    char[] so_name,
    ParsedFunc* funcs,
    usz func_count,
    char[] output_path,
    BindgenRawSyntax raw_syntax,
    char[64]* strip_prefixes,
    usz strip_prefix_count
) {
    if (!bindgen_validate_emitted_function_names(funcs, func_count, null, 0)) return false;
    if (!bindgen_library_name_slice_is_safe(so_name)) return false;

    char[BINDGEN_BUF_SIZE] buf;
    usz pos = 0;
    bool ok = true;

    char[] raw_syntax_note = "";
    switch (raw_syntax) {
        case BINDGEN_RAW_SYNTAX_LEGACY:
            raw_syntax_note = "legacy";
        case BINDGEN_RAW_SYNTAX_GROUPED:
            raw_syntax_note = "grouped";
    }
    pos = bindgen_append_raw_header(buf[..], pos, so_name, raw_syntax_note, &ok);
    pos = bindgen_append_module_open(buf[..], pos, module_name, "-raw", funcs, func_count,
        null, 0, &ok);
    switch (raw_syntax) {
        case BINDGEN_RAW_SYNTAX_LEGACY:
            pos = bindgen_append_library_handle(buf[..], pos, so_name, &ok);
            pos = bindgen_append_functions(buf[..], pos, funcs, func_count,
                null, 0, &ok);
        case BINDGEN_RAW_SYNTAX_GROUPED:
            pos = bindgen_append_grouped_module(buf[..], pos, so_name, funcs, func_count,
                null, 0, &ok);
    }
    pos = buf_append(buf[..], pos, ")\n", &ok); // close module
    if (!ok) return false;
    return bindgen_write_text_file(output_path, buf[:pos]);
}

fn bool generate_ffi_module(
    char[] module_name,
    char[] so_name,
    ParsedFunc* funcs,
    usz func_count,
    char[] output_path
) {
    return generate_ffi_module_with_syntax(
        module_name,
        so_name,
        funcs,
        func_count,
        output_path,
        BINDGEN_RAW_SYNTAX_LEGACY
    );
}

fn bool generate_ffi_facade_module_if_missing(
    char[] module_name,
    char[] so_name,
    ParsedFunc* funcs,
    usz func_count,
    char[] output_path
) {
    return generate_ffi_facade_module_if_missing_with_prefixes(
        module_name,
        so_name,
        funcs,
        func_count,
        output_path,
        null,
        0
    );
}

fn bool generate_ffi_facade_module_if_missing_with_prefixes(
    char[] module_name,
    char[] so_name,
    ParsedFunc* funcs,
    usz func_count,
    char[] output_path,
    char[64]* strip_prefixes,
    usz strip_prefix_count
) {
    if (!bindgen_validate_emitted_function_names(funcs, func_count,
        strip_prefixes, strip_prefix_count)) return false;
    if (!bindgen_library_name_slice_is_safe(so_name)) return false;
    if (bindgen_output_file_exists(output_path)) return true;

    char[BINDGEN_BUF_SIZE] buf;
    usz pos = 0;
    bool ok = true;

    pos = bindgen_append_facade_header(buf[..], pos, so_name, &ok);
    pos = bindgen_append_module_open(buf[..], pos, module_name, "", funcs, func_count,
        strip_prefixes, strip_prefix_count, &ok);
    pos = bindgen_append_facade_import(buf[..], pos, module_name, &ok);
    pos = bindgen_append_facade_alias_import(buf[..], pos, module_name, funcs, func_count,
        strip_prefixes, strip_prefix_count, &ok);
    pos = buf_append(buf[..], pos, "  ;; Edit this facade to add Omni-facing naming, ownership, or buffer cleanup.\n", &ok);
    pos = bindgen_append_facade_wrappers(buf[..], pos, funcs, func_count,
        strip_prefixes, strip_prefix_count, &ok);
    pos = buf_append(buf[..], pos, ")\n", &ok);
    if (!ok) return false;
    return bindgen_write_text_file(output_path, buf[:pos]);
}

fn bool generate_ffi_module_pair_with_syntax(
    char[] module_name,
    char[] so_name,
    ParsedFunc* funcs,
    usz func_count,
    char[] raw_output_path,
    char[] facade_output_path,
    BindgenRawSyntax raw_syntax
) {
    return generate_ffi_module_pair_with_options(
        module_name,
        so_name,
        funcs,
        func_count,
        raw_output_path,
        facade_output_path,
        raw_syntax,
        null,
        0
    );
}

fn bool generate_ffi_module_pair_with_options(
    char[] module_name,
    char[] so_name,
    ParsedFunc* funcs,
    usz func_count,
    char[] raw_output_path,
    char[] facade_output_path,
    BindgenRawSyntax raw_syntax,
    char[64]* strip_prefixes,
    usz strip_prefix_count
) {
    if (!bindgen_validate_emitted_function_names(funcs, func_count,
        strip_prefixes, strip_prefix_count)) return false;
    bool raw_existed = bindgen_output_file_exists(raw_output_path);
    if (!generate_ffi_module_with_options(module_name, so_name, funcs, func_count,
        raw_output_path, raw_syntax, strip_prefixes, strip_prefix_count)) return false;
    if (generate_ffi_facade_module_if_missing_with_prefixes(module_name, so_name, funcs,
        func_count, facade_output_path, strip_prefixes, strip_prefix_count)) return true;
    if (!raw_existed && !bindgen_delete_output_file_if_exists(raw_output_path)) return false;
    return false;
}

fn bool generate_ffi_module_pair(
    char[] module_name,
    char[] so_name,
    ParsedFunc* funcs,
    usz func_count,
    char[] raw_output_path,
    char[] facade_output_path
) {
    return generate_ffi_module_pair_with_syntax(
        module_name,
        so_name,
        funcs,
        func_count,
        raw_output_path,
        facade_output_path,
        BINDGEN_RAW_SYNTAX_LEGACY
    );
}
