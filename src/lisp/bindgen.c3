module lisp;

import std::io;

// =============================================================================
// FFI Binding Generator — takes ParsedFunc[] + library info, writes Omni module.
// Generates (define [ffi lib])/(define [ffi λ]) declarations.
// =============================================================================

const usz BINDGEN_BUF_SIZE = 32768;

/**
 * Generate an Omni FFI module file from parsed C function declarations.
 *
 * Output format:
 *   (module ffi-{name} (export fn1 fn2 ...)
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

    pos = bindgen_append_header(buf[..], pos, so_name, &ok);
    pos = bindgen_append_module_open(buf[..], pos, module_name, funcs, func_count, &ok);
    pos = bindgen_append_library_handle(buf[..], pos, so_name, &ok);
    pos = bindgen_append_functions(buf[..], pos, funcs, func_count, &ok);
    pos = buf_append(buf[..], pos, ")\n", &ok); // close module
    if (!ok) return false;

    // Write to file
    if (try file = io::file::open((String)output_path, "w")) {
        char[] remaining = buf[:pos];
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

fn usz bindgen_append_header(char[] buf, usz pos, char[] so_name, bool* ok) {
    pos = buf_append(buf, pos, ";; Auto-generated FFI bindings for lib", ok);
    pos = buf_append(buf, pos, so_name, ok);
    pos = buf_append(buf, pos, "\n;; Regenerate with: omni --bind\n\n", ok);
    return pos;
}

fn usz bindgen_append_module_open(char[] buf, usz pos, char[] module_name, ParsedFunc* funcs, usz func_count, bool* ok) {
    pos = buf_append(buf, pos, "(module ffi-", ok);
    pos = buf_append(buf, pos, module_name, ok);
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
