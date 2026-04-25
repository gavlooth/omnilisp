module lisp;

fn bool bindgen_func_is_hidden(ParsedFunc* f) {
    return f == null || f.filtered_out;
}

fn usz bindgen_append_raw_header(
    char[] buf,
    usz pos,
    char[] so_name,
    char[] raw_syntax_note,
    BindgenMode mode,
    BindgenGenerator generator,
    bool* ok
) {
    pos = buf_append(buf, pos, ";; Auto-generated raw FFI bindings for lib", ok);
    pos = buf_append(buf, pos, so_name, ok);
    pos = buf_append(buf, pos, "\n;; Regenerate with: omni --bind\n", ok);
    if (raw_syntax_note.len > 0) {
        pos = buf_append(buf, pos, ";; Raw syntax: ", ok);
        pos = buf_append(buf, pos, raw_syntax_note, ok);
        pos = buf_append(buf, pos, "\n", ok);
    }
    if (mode == BINDGEN_MODE_API) {
        pos = buf_append(buf, pos, ";; mode = ", ok);
        pos = buf_append(buf, pos, bindgen_mode_name(mode), ok);
        pos = buf_append(buf, pos, "\n", ok);
    }
    if (generator == BINDGEN_GENERATOR_CPPINTEROP) {
        pos = buf_append(buf, pos, ";; generator = ", ok);
        pos = buf_append(buf, pos, bindgen_generator_name(generator), ok);
        pos = buf_append(buf, pos, "\n", ok);
    }
    pos = buf_append(buf, pos, ";; This file is regenerated on every bind run.\n\n", ok);
    return pos;
}

fn bool bindgen_is_safe_emitted_text_char(char ch) @inline {
    return ch >= 0x20;
}

fn bool bindgen_emitted_name_is_safe_for_codegen(char* text) {
    if (text == null || text[0] == 0) return false;
    if (!bindgen_emitted_name_has_safe_symbol_start(text)) return false;

    usz i = 0;
    while (text[i] != 0) {
        if (!is_symbol_char(text[i])) return false;
        if (!bindgen_is_safe_emitted_text_char(text[i])) return false;
        i++;
    }
    return true;
}

fn bool bindgen_metadata_text_is_safe(char* text) @inline {
    if (text == null) return false;
    usz i = 0;
    while (text[i] != 0) {
        if ((uint)text[i] < 0x20) return false;
        i++;
    }
    return true;
}

fn usz bindgen_append_facade_header(
    char[] buf,
    usz pos,
    char[] so_name,
    BindgenMode mode,
    BindgenGenerator generator,
    bool* ok
) {
    pos = buf_append(buf, pos, ";; Auto-generated FFI facade stub for lib", ok);
    pos = buf_append(buf, pos, so_name, ok);
    pos = buf_append(buf, pos, "\n;; Raw bindings live in the sibling *_raw.omni file.\n", ok);
    if (mode == BINDGEN_MODE_API) {
        pos = buf_append(buf, pos, ";; mode = ", ok);
        pos = buf_append(buf, pos, bindgen_mode_name(mode), ok);
        pos = buf_append(buf, pos, "\n", ok);
    }
    if (generator == BINDGEN_GENERATOR_CPPINTEROP) {
        pos = buf_append(buf, pos, ";; generator = ", ok);
        pos = buf_append(buf, pos, bindgen_generator_name(generator), ok);
        pos = buf_append(buf, pos, "\n", ok);
    }
    pos = buf_append(buf, pos, ";; This facade is created only when missing so local edits survive reruns.\n\n", ok);
    return pos;
}

fn bool bindgen_name_starts_with_prefix(char* text, char* prefix) @inline {
    for (usz i = 0; ; i++) {
        if (prefix[i] == 0) return true;
        if (text[i] != prefix[i] || text[i] == 0) return false;
    }
}

fn void bindgen_apply_prefix_strips(
    char* source_name,
    char[64]* strip_prefixes,
    usz strip_prefix_count,
    char[128]* out_name,
    bool* ok
) {
    usz source_len = 0;
    while (source_name[source_len] != 0) source_len++;

    usz strip_len = 0;
    for (usz i = 0; i < strip_prefix_count; i++) {
        char* prefix = &strip_prefixes[i];
        if (prefix[0] == 0) continue;
        if (bindgen_name_starts_with_prefix(source_name, prefix)) {
            usz p_len = 0;
            while (prefix[p_len] != 0) p_len++;
            strip_len = p_len;
            break;
        }
    }

    if (strip_len >= source_len) {
        (*out_name)[0] = 0;
        *ok = false;
        return;
    }

    usz start = strip_len;
    usz copied = 0;
    while (start + copied < source_len && copied < 127) {
        (*out_name)[copied] = source_name[start + copied];
        copied++;
    }
    if (start + copied < source_len) {
        (*out_name)[0] = 0;
        *ok = false;
        return;
    }
    (*out_name)[copied] = 0;
}

fn bool bindgen_emitted_name_has_safe_symbol_start(char* name) @inline {
    if (name == null || name[0] == 0) return false;
    char first = name[0] == '_' ? '-' : name[0];
    char second = name[1] == '_' ? '-' : name[1];
    if (first >= '0' && first <= '9') return false;
    if (first == '-' && second >= '0' && second <= '9') return false;
    if (first == '.') return false;
    return is_symbol_char(first);
}

fn bool bindgen_resolve_emitted_function_name(
    ParsedFunc* f,
    char[64]* strip_prefixes,
    usz strip_prefix_count,
    char[128]* out_name
) {
    for (usz i = 0; i < (*out_name).len; i++) (*out_name)[i] = 0;
    if (f == null) return false;

    if (strip_prefix_count > 0) {
        bool ok = true;
        bindgen_apply_prefix_strips(&f.name, strip_prefixes, strip_prefix_count, out_name, &ok);
        return ok;
    }

    usz copied = 0;
    while (f.name[copied] != 0 && copied < 127) {
        (*out_name)[copied] = f.name[copied];
        copied++;
    }
    if (f.name[copied] != 0) {
        (*out_name)[0] = 0;
        return false;
    }
    (*out_name)[copied] = 0;
    return true;
}

fn char bindgen_emitted_name_char(char ch) @inline {
    return ch == '_' ? '-' : ch;
}

fn bool bindgen_emitted_names_equal(char* left, char* right) {
    if (left == null || right == null) return false;
    usz i = 0;
    while (left[i] != 0 && right[i] != 0) {
        if (bindgen_emitted_name_char(left[i]) != bindgen_emitted_name_char(right[i])) return false;
        i++;
    }
    return left[i] == 0 && right[i] == 0;
}

fn bool bindgen_validate_emitted_function_names(
    ParsedFunc* funcs,
    usz func_count,
    char[64]* strip_prefixes,
    usz strip_prefix_count
) {
    if (strip_prefix_count > 0 && strip_prefixes == null) return false;
    for (usz i = 0; i < func_count; i++) {
        ParsedFunc* f = &funcs[i];
        if (f.is_variadic || bindgen_func_is_hidden(f)) continue;

        char[128] emitted_name;
        if (!bindgen_resolve_emitted_function_name(f, strip_prefixes, strip_prefix_count, &emitted_name) ||
            !bindgen_emitted_name_is_safe_for_codegen(&emitted_name[0])) return false;

        for (usz j = 0; j < i; j++) {
            ParsedFunc* prev = &funcs[j];
            if (prev.is_variadic || bindgen_func_is_hidden(prev)) continue;
            char[128] prev_name;
            if (!bindgen_resolve_emitted_function_name(prev, strip_prefixes, strip_prefix_count, &prev_name)) return false;
            if (bindgen_emitted_names_equal(&emitted_name[0], &prev_name[0])) return false;
        }
    }
    return true;
}

fn usz bindgen_append_function_name(
    char[] buf,
    usz pos,
    char* source_name,
    char[64]* strip_prefixes,
    usz strip_prefix_count,
    bool* ok
) {
    char[128] emitted_name;
    for (usz i = 0; i < emitted_name.len; i++) emitted_name[i] = 0;

    if (strip_prefix_count > 0) {
        bindgen_apply_prefix_strips(source_name, strip_prefixes, strip_prefix_count, &emitted_name, ok);
        if (!*ok) return pos;
        if (!bindgen_emitted_name_is_safe_for_codegen(&emitted_name[0])) {
            *ok = false;
            return pos;
        }
        pos = buf_append_kebab(buf, pos, &emitted_name[0], ok);
        return pos;
    }

    if (!bindgen_emitted_name_is_safe_for_codegen(source_name)) {
        *ok = false;
        return pos;
    }
    pos = buf_append_kebab(buf, pos, source_name, ok);
    return pos;
}

fn usz bindgen_append_module_open(
    char[] buf,
    usz pos,
    char[] module_name,
    char[] module_suffix,
    ParsedFunc* funcs,
    usz func_count,
    char[64]* strip_prefixes,
    usz strip_prefix_count,
    bool* ok
) {
    if (!bindgen_emitted_name_is_safe_for_codegen(&module_name[0])) {
        *ok = false;
        return pos;
    }

    pos = buf_append(buf, pos, "(module ffi-", ok);
    pos = buf_append(buf, pos, module_name, ok);
    pos = buf_append(buf, pos, module_suffix, ok);
    pos = buf_append(buf, pos, " (export", ok);

    // Export list — only non-variadic functions.
    for (usz i = 0; i < func_count; i++) {
        if (bindgen_func_is_hidden(&funcs[i]) || funcs[i].is_variadic) continue;
        pos = buf_append_char(buf, pos, ' ', ok);
        pos = bindgen_append_function_name(buf, pos, &funcs[i].name, strip_prefixes, strip_prefix_count, ok);
    }
    pos = buf_append(buf, pos, ")\n\n", ok);
    return pos;
}

fn usz bindgen_append_facade_import(char[] buf, usz pos, char[] module_name, bool* ok) {
    if (!bindgen_emitted_name_is_safe_for_codegen(&module_name[0])) {
        *ok = false;
        return pos;
    }

    pos = buf_append(buf, pos, "  (import \"", ok);
    pos = buf_append(buf, pos, module_name, ok);
    pos = buf_append(buf, pos, "_raw.omni\")\n", ok);
    return pos;
}

fn usz bindgen_append_facade_alias_import(
    char[] buf,
    usz pos,
    char[] module_name,
    ParsedFunc* funcs,
    usz func_count,
    char[64]* strip_prefixes,
    usz strip_prefix_count,
    bool* ok
) {
    if (!bindgen_emitted_name_is_safe_for_codegen(&module_name[0])) {
        *ok = false;
        return pos;
    }

    usz export_count = 0;
    for (usz i = 0; i < func_count; i++) {
        if (!bindgen_func_is_hidden(&funcs[i]) && !funcs[i].is_variadic) export_count++;
    }
    if (export_count == 0) return pos;

    pos = buf_append(buf, pos, "  (import ffi-", ok);
    pos = buf_append(buf, pos, module_name, ok);
    pos = buf_append(buf, pos, "-raw (", ok);
    usz written = 0;
    for (usz i = 0; i < func_count; i++) {
        ParsedFunc* f = &funcs[i];
        if (bindgen_func_is_hidden(f) || f.is_variadic) continue;
        pos = buf_append(buf, pos, "(", ok);
        pos = bindgen_append_function_name(buf, pos, &f.name, null, 0, ok);
        pos = buf_append(buf, pos, " 'as raw-", ok);
        pos = bindgen_append_function_name(buf, pos, &f.name, strip_prefixes, strip_prefix_count, ok);
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
    if (!bindgen_emitted_name_is_safe_for_codegen(&f.name[0]) || !bindgen_metadata_text_is_safe(&f.name[0])) {
        *ok = false;
        return pos;
    }
    pos = buf_append(buf, pos, "  ;; skipped: ", ok);
    pos = buf_append_cstr(buf, pos, &f.name, ok);
    pos = buf_append(buf, pos, " (variadic)\n", ok);
    return pos;
}

fn usz bindgen_append_function_decl(
    char[] buf,
    usz pos,
    ParsedFunc* f,
    char[64]* strip_prefixes,
    usz strip_prefix_count,
    bool* ok
) {
    if (!bindgen_metadata_text_is_safe(&f.return_omni_type)) {
        *ok = false;
        return pos;
    }

    // (define [ffi λ _lib] (kebab-name (^Type arg0) ...) ^RetType)
    pos = bindgen_append_function_metadata(buf, pos, f, ok);
    pos = buf_append(buf, pos, "  (define [ffi lambda _lib] (", ok);
    pos = bindgen_append_function_name(buf, pos, &f.name, strip_prefixes, strip_prefix_count, ok);

    for (usz j = 0; j < f.param_count; j++) {
        ParsedParam* p = &f.params[j];
        if (!bindgen_emitted_name_is_safe_for_codegen(&p.name[0])) {
            *ok = false;
            return pos;
        }
        if (!bindgen_metadata_text_is_safe(&p.omni_type) || !bindgen_metadata_text_is_safe(&p.name[0])) {
            *ok = false;
            return pos;
        }

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

fn usz bindgen_append_grouped_function_decl(
    char[] buf,
    usz pos,
    ParsedFunc* f,
    char[64]* strip_prefixes,
    usz strip_prefix_count,
    bool* ok
) {
    if (!bindgen_metadata_text_is_safe(&f.return_omni_type)) {
        *ok = false;
        return pos;
    }

    pos = bindgen_append_function_metadata(buf, pos, f, ok);
    pos = buf_append(buf, pos, "    (", ok);
    pos = bindgen_append_function_name(buf, pos, &f.name, strip_prefixes, strip_prefix_count, ok);

    for (usz j = 0; j < f.param_count; j++) {
        ParsedParam* p = &f.params[j];
        if (!bindgen_emitted_name_is_safe_for_codegen(&p.name[0])) {
            *ok = false;
            return pos;
        }
        if (!bindgen_metadata_text_is_safe(&p.omni_type) || !bindgen_metadata_text_is_safe(&p.name[0])) {
            *ok = false;
            return pos;
        }

        pos = buf_append(buf, pos, " (^", ok);
        pos = buf_append_cstr(buf, pos, &p.omni_type, ok);
        pos = buf_append_char(buf, pos, ' ', ok);
        pos = buf_append_cstr(buf, pos, &p.name, ok);
        pos = buf_append_char(buf, pos, ')', ok);
    }

    pos = buf_append(buf, pos, ") ^", ok);
    pos = buf_append_cstr(buf, pos, &f.return_omni_type, ok);
    pos = buf_append(buf, pos, "\n", ok);
    return pos;
}

fn usz bindgen_append_functions(
    char[] buf,
    usz pos,
    ParsedFunc* funcs,
    usz func_count,
    char[64]* strip_prefixes,
    usz strip_prefix_count,
    bool* ok
) {
    for (usz i = 0; i < func_count; i++) {
        ParsedFunc* f = &funcs[i];
        if (bindgen_func_is_hidden(f)) continue;
        if (f.is_variadic) {
            pos = bindgen_append_variadic_skip(buf, pos, f, ok);
            continue;
        }
        pos = bindgen_append_function_decl(buf, pos, f, strip_prefixes, strip_prefix_count, ok);
    }
    return pos;
}

fn usz bindgen_append_grouped_module(
    char[] buf,
    usz pos,
    char[] so_name,
    ParsedFunc* funcs,
    usz func_count,
    char[64]* strip_prefixes,
    usz strip_prefix_count,
    bool* ok
) {
    pos = buf_append(buf, pos, "  (define [ffi module] _lib \"lib", ok);
    pos = buf_append(buf, pos, so_name, ok);
    pos = buf_append(buf, pos, ".so\"\n", ok);

    for (usz i = 0; i < func_count; i++) {
        ParsedFunc* f = &funcs[i];
        if (bindgen_func_is_hidden(f)) continue;
        if (f.is_variadic) {
            pos = bindgen_append_variadic_skip(buf, pos, f, ok);
            continue;
        }
        pos = bindgen_append_grouped_function_decl(buf, pos, f, strip_prefixes, strip_prefix_count, ok);
    }

    pos = buf_append(buf, pos, "  )\n\n", ok);
    return pos;
}
