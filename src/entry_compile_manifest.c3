module main;

import std::io;
import std::collections::list;
import lisp;

extern fn int compile_manifest_getpid() @extern("getpid");
extern fn int compile_manifest_uv_rename(char* src, char* dst) @extern("omni_uv_fs_rename");
extern fn int compile_manifest_uv_unlink(char* path) @extern("omni_uv_fs_unlink");

bool g_compile_manifest_force_fail_before_rename = false;

fn void compile_manifest_append_json_hex4(DString* out, int value) {
    char[4] digits;
    for (usz i = 0; i < 4; i++) {
        int shift = 12 - (int)(i * 4);
        int nibble = (value >> shift) & 0xF;
        digits[i] = (char)(nibble < 10 ? ('0' + nibble) : ('a' + (nibble - 10)));
    }
    out.append_string("\\u");
    out.append_char(digits[0]);
    out.append_char(digits[1]);
    out.append_char(digits[2]);
    out.append_char(digits[3]);
}

fn void compile_manifest_append_json_escaped(DString* out, char[] text) {
    for (usz i = 0; i < text.len; i++) {
        char c = text[i];
        switch (c) {
            case '\\':
                out.append_string("\\\\");
            case '"':
                out.append_string("\\\"");
            case '\n':
                out.append_string("\\n");
            case '\r':
                out.append_string("\\r");
            case '\t':
                out.append_string("\\t");
            case '\b':
                out.append_string("\\b");
            case '\f':
                out.append_string("\\f");
            default:
                int code = (int)c;
                if (code < 0) code += 256;
                if (code < 0x20) {
                    compile_manifest_append_json_hex4(out, code);
                } else {
                    out.append_char(c);
                }
        }
    }
}

fn void compile_manifest_append_json_string(DString* out, char[] text) {
    out.append_char('"');
    compile_manifest_append_json_escaped(out, text);
    out.append_char('"');
}

fn void compile_manifest_emit_ffi_libraries(
    DString* out,
    lisp::Expr* expr,
    bool* first_entry,
    lisp::Interp* interp
) {
    if (expr == null) return;

    if (expr.tag == lisp::ExprTag.E_MODULE && expr.module_expr != null) {
        for (usz i = 0; i < expr.module_expr.body_count; i++) {
            compile_manifest_emit_ffi_libraries(out, expr.module_expr.body[i], first_entry, interp);
        }
        return;
    }
    if (expr.tag == lisp::ExprTag.E_BLOCK && expr.block != null) {
        for (usz i = 0; i < expr.block.expr_count; i++) {
            compile_manifest_emit_ffi_libraries(out, expr.block.exprs[i], first_entry, interp);
        }
        return;
    }

    if (expr.tag != lisp::ExprTag.E_FFI_LIB) return;
    lisp::ExprFfiLib* lib = &expr.ffi_lib;
    if (lib.path_expr == null
        || lib.path_expr.tag != lisp::ExprTag.E_LIT
        || lib.path_expr.lit.value.tag != lisp::ValueTag.STRING) {
        return;
    }

    if (!*first_entry) out.append_string(",\n");
    *first_entry = false;

    char[] binding = interp.symbols.get_name(lib.name);
    char[] soname = lib.path_expr.lit.value.str_chars[:lib.path_expr.lit.value.str_len];

    out.append_string("    {\"binding\": ");
    compile_manifest_append_json_string(out, binding);
    out.append_string(", \"soname\": ");
    compile_manifest_append_json_string(out, soname);
    out.append_string(", \"eager_load\": true, \"required\": true}");
}

fn void compile_manifest_emit_ffi_functions(
    DString* out,
    lisp::Expr* expr,
    bool* first_entry,
    lisp::Interp* interp
) {
    if (expr == null) return;

    if (expr.tag == lisp::ExprTag.E_MODULE && expr.module_expr != null) {
        for (usz i = 0; i < expr.module_expr.body_count; i++) {
            compile_manifest_emit_ffi_functions(out, expr.module_expr.body[i], first_entry, interp);
        }
        return;
    }
    if (expr.tag == lisp::ExprTag.E_BLOCK && expr.block != null) {
        for (usz i = 0; i < expr.block.expr_count; i++) {
            compile_manifest_emit_ffi_functions(out, expr.block.exprs[i], first_entry, interp);
        }
        return;
    }

    if (expr.tag != lisp::ExprTag.E_FFI_FN || expr.ffi_fn == null || expr.ffi_fn.is_variadic) return;
    lisp::ExprFfiFn* fn_expr = expr.ffi_fn;

    if (!*first_entry) out.append_string(",\n");
    *first_entry = false;

    char[] library_binding = interp.symbols.get_name(fn_expr.lib_name);
    char[] binding = interp.symbols.get_name(fn_expr.fn_name);
    char[] symbol_name = fn_expr.c_name != null ? fn_expr.c_name[:fn_expr.c_name_len] : "";

    out.append_string("    {\"binding\": ");
    compile_manifest_append_json_string(out, binding);
    out.append_string(", \"library_binding\": ");
    compile_manifest_append_json_string(out, library_binding);
    out.append_string(", \"symbol\": ");
    compile_manifest_append_json_string(out, symbol_name);
    out.append_string(", \"params\": [");
    for (usz i = 0; i < fn_expr.param_count; i++) {
        if (i > 0) out.append_string(", ");
        char[] param_type = interp.symbols.get_name(fn_expr.param_types[i].base_type);
        compile_manifest_append_json_string(out, param_type);
    }
    out.append_string("], \"return\": ");
    char[] ret_type = fn_expr.has_return_type
        ? interp.symbols.get_name(fn_expr.return_type.base_type)
        : "Void";
    compile_manifest_append_json_string(out, ret_type);
    out.append_string(", \"eager_resolve\": true, \"required_symbol\": true}");
}

fn bool compile_manifest_has_ffi_decl(lisp::Expr* expr) {
    if (expr == null) return false;
    if (expr.tag == lisp::ExprTag.E_FFI_LIB) return true;
    if (expr.tag == lisp::ExprTag.E_FFI_FN && expr.ffi_fn != null && !expr.ffi_fn.is_variadic) return true;
    if (expr.tag == lisp::ExprTag.E_MODULE && expr.module_expr != null) {
        for (usz i = 0; i < expr.module_expr.body_count; i++) {
            if (compile_manifest_has_ffi_decl(expr.module_expr.body[i])) return true;
        }
    }
    if (expr.tag == lisp::ExprTag.E_BLOCK && expr.block != null) {
        for (usz i = 0; i < expr.block.expr_count; i++) {
            if (compile_manifest_has_ffi_decl(expr.block.exprs[i])) return true;
        }
    }
    return false;
}

fn bool compile_manifest_copy_path_cstr(char[] path, char[] path_buf, char[]* out_path) {
    if (out_path != null) *out_path = "";
    if (path.len == 0 || path.len >= path_buf.len) return false;
    for (usz i = 0; i < path.len; i++) {
        path_buf[i] = path[i];
    }
    path_buf[path.len] = 0;
    if (out_path != null) *out_path = path_buf[:path.len];
    return true;
}

fn bool compile_manifest_build_atomic_temp_path(char[] final_path, char[] temp_buf, char[]* out_temp_path) {
    if (out_temp_path != null) *out_temp_path = "";

    char[32] suffix_buf;
    char[] suffix = "";
    if (try rendered = io::bprintf(&suffix_buf, ".tmp.%d", compile_manifest_getpid())) {
        suffix = rendered;
    } else {
        return false;
    }

    if (final_path.len == 0 || final_path.len > temp_buf.len || suffix.len > temp_buf.len - final_path.len) {
        return false;
    }
    usz temp_len = final_path.len + suffix.len;
    if (temp_len >= temp_buf.len) return false;
    for (usz i = 0; i < final_path.len; i++) {
        temp_buf[i] = final_path[i];
    }
    for (usz i = 0; i < suffix.len; i++) {
        temp_buf[final_path.len + i] = suffix[i];
    }
    temp_buf[temp_len] = 0;
    if (out_temp_path != null) *out_temp_path = temp_buf[:temp_len];
    return true;
}

fn bool compile_manifest_write_atomic_text(char[] final_path, char[] text) {
    char[4096] final_path_buf;
    char[4160] temp_path_buf;
    char[] final_path_cstr = "";
    char[] temp_path = "";
    if (!compile_manifest_copy_path_cstr(final_path, final_path_buf[..], &final_path_cstr)) return false;
    if (!compile_manifest_build_atomic_temp_path(final_path_cstr, temp_path_buf[..], &temp_path)) return false;

    (void)compile_manifest_uv_unlink((ZString)temp_path.ptr);

    bool committed = false;
    if (try file = io::file::open((String)temp_path, "w")) {
        defer if (!committed) (void)compile_manifest_uv_unlink((ZString)temp_path.ptr);
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
        if (!write_ok) return false;
        if (g_compile_manifest_force_fail_before_rename) return false;
        if (compile_manifest_uv_rename((ZString)temp_path.ptr, (ZString)final_path_cstr.ptr) != 0) return false;
        committed = true;
        return true;
    }
    return false;
}

fn bool write_compile_ffi_manifest(
    char[] source,
    char[] input_path,
    char[] output_path,
    lisp::Interp* interp,
    char[]* manifest_path_out,
    EntryFileWriteKind* write_kind_out
) {
    *manifest_path_out = "";
    *write_kind_out = ENTRY_FILE_WRITE_GENERIC_FAILURE;

    List{lisp::Expr*} exprs = lisp::parse_program(source, interp);
    if (exprs.len() == 0) return true;

    bool has_ffi = false;
    for (usz i = 0; i < exprs.len(); i++) {
        if (compile_manifest_has_ffi_decl(exprs[i])) {
            has_ffi = true;
            break;
        }
    }
    if (!has_ffi) return true;

    DString manifest_path_builder;
    manifest_path_builder.init(mem, output_path.len + 32);
    defer manifest_path_builder.free();
    manifest_path_builder.append_string((String)output_path);
    manifest_path_builder.append_string(".ffi-manifest.json");
    char[] manifest_path = manifest_path_builder.str_view().copy(mem);
    if (manifest_path.len == 0) return false;
    *manifest_path_out = manifest_path;

    DString json;
    json.init(mem, 4096);
    defer json.free();

    json.append_string("{\n");
    json.append_string("  \"schema\": \"omni-ffi-manifest-v1\",\n");
    json.append_string("  \"source\": ");
    compile_manifest_append_json_string(&json, input_path);
    json.append_string(",\n");
    json.append_string("  \"generated_for\": ");
    compile_manifest_append_json_string(&json, output_path);
    json.append_string(",\n");
    json.append_string("  \"startup_preload\": true,\n");
    json.append_string("  \"default_library_policy\": {\"eager_load\": true, \"required\": true},\n");
    json.append_string("  \"default_symbol_policy\": {\"eager_resolve\": true, \"required\": true},\n");
    json.append_string("  \"libraries\": [\n");
    bool first_lib = true;
    for (usz i = 0; i < exprs.len(); i++) {
        compile_manifest_emit_ffi_libraries(&json, exprs[i], &first_lib, interp);
    }
    json.append_string("\n  ],\n");
    json.append_string("  \"functions\": [\n");
    bool first_fn = true;
    for (usz i = 0; i < exprs.len(); i++) {
        compile_manifest_emit_ffi_functions(&json, exprs[i], &first_fn, interp);
    }
    json.append_string("\n  ]\n");
    json.append_string("}\n");
    bool write_ok = compile_manifest_write_atomic_text(manifest_path, json.str_view());
    if (!write_ok) *write_kind_out = classify_file_write_errno();
    return write_ok;
}
