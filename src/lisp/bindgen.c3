module lisp;

import std::io;

// =============================================================================
// FFI Binding Generator — takes ParsedFunc[] + library info, writes Omni module.
// Generates (ffi-open)/(ffi-call) wrappers with typed parameters.
// =============================================================================

const usz BINDGEN_BUF_SIZE = 32768;

/**
 * Generate an Omni FFI module file from parsed C function declarations.
 *
 * Output format:
 *   (module ffi-{name} (export fn1 fn2 ...)
 *     (define _lib (ffi-open "lib{library}.so"))
 *     (define (fn1 (^Type arg0) ...) (ffi-call _lib "fn1" 'ret arg0 'type ...))
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

    // Header comment
    pos = buf_append(buf[..], pos, ";; Auto-generated FFI bindings for lib");
    pos = buf_append(buf[..], pos, so_name);
    pos = buf_append(buf[..], pos, "\n;; Regenerate with: omni --bind\n\n");

    // Module declaration: (module ffi-{name} (export ...)
    pos = buf_append(buf[..], pos, "(module ffi-");
    pos = buf_append(buf[..], pos, module_name);
    pos = buf_append(buf[..], pos, " (export");

    // Export list — only non-variadic functions
    for (usz i = 0; i < func_count; i++) {
        if (funcs[i].is_variadic) continue;
        pos = buf_append_char(buf[..], pos, ' ');
        pos = buf_append_kebab(buf[..], pos, &funcs[i].name);
    }
    pos = buf_append(buf[..], pos, ")\n\n");

    // Library handle
    pos = buf_append(buf[..], pos, "  (define _lib (ffi-open \"lib");
    pos = buf_append(buf[..], pos, so_name);
    pos = buf_append(buf[..], pos, ".so\"))\n\n");

    // Function definitions
    for (usz i = 0; i < func_count; i++) {
        ParsedFunc* f = &funcs[i];

        if (f.is_variadic) {
            // Skip variadic with comment
            pos = buf_append(buf[..], pos, "  ;; skipped: ");
            pos = buf_append_cstr(buf[..], pos, &f.name);
            pos = buf_append(buf[..], pos, " (variadic)\n");
            continue;
        }

        // (define (kebab-name (^Type arg0) (^Type arg1) ...)
        pos = buf_append(buf[..], pos, "  (define (");
        pos = buf_append_kebab(buf[..], pos, &f.name);

        for (usz j = 0; j < f.param_count; j++) {
            ParsedParam* p = &f.params[j];
            pos = buf_append(buf[..], pos, " (^");
            pos = buf_append_cstr(buf[..], pos, &p.omni_type);
            pos = buf_append_char(buf[..], pos, ' ');
            pos = buf_append_cstr(buf[..], pos, &p.name);
            pos = buf_append_char(buf[..], pos, ')');
        }

        pos = buf_append(buf[..], pos, ")\n");

        // (ffi-call _lib "c_name" 'ret_type arg0 'type0 arg1 'type1 ...)
        pos = buf_append(buf[..], pos, "    (ffi-call _lib \"");
        pos = buf_append_cstr(buf[..], pos, &f.name);
        pos = buf_append(buf[..], pos, "\" ");
        pos = buf_append_cstr(buf[..], pos, &f.return_ffi_type);

        for (usz j = 0; j < f.param_count; j++) {
            ParsedParam* p = &f.params[j];
            pos = buf_append_char(buf[..], pos, ' ');
            pos = buf_append_cstr(buf[..], pos, &p.name);
            pos = buf_append_char(buf[..], pos, ' ');
            pos = buf_append_cstr(buf[..], pos, &p.ffi_type);
        }

        pos = buf_append(buf[..], pos, "))\n\n");
    }

    // Close module
    pos = buf_append(buf[..], pos, ")\n");

    // Write to file
    if (try file = io::file::open((String)output_path, "w")) {
        defer (void)file.close();
        file.write(buf[:pos])!!;
        return true;
    }
    return false;
}

// --- Buffer helpers ---

fn usz buf_append(char[] buf, usz pos, char[] str) {
    for (usz i = 0; i < str.len && pos < buf.len - 1; i++) {
        buf[pos] = str[i];
        pos++;
    }
    return pos;
}

fn usz buf_append_char(char[] buf, usz pos, char c) {
    if (pos < buf.len - 1) {
        buf[pos] = c;
        pos++;
    }
    return pos;
}

fn usz buf_append_cstr(char[] buf, usz pos, void* cstr_ptr) {
    char* s = (char*)cstr_ptr;
    usz i = 0;
    while (s[i] != 0 && pos < buf.len - 1) {
        buf[pos] = s[i];
        pos++;
        i++;
    }
    return pos;
}

/**
 * Append a C identifier converted to kebab-case.
 * snake_case → kebab-case: replace '_' with '-'
 */
fn usz buf_append_kebab(char[] buf, usz pos, void* cstr_ptr) {
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
    return pos;
}
