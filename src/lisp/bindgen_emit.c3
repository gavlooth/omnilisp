module lisp;

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
