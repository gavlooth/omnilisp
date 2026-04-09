module lisp;

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
