module main;

import std::io;
import std::collections::list;
import lisp;

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

    switch (expr.tag) {
        case lisp::ExprTag.E_LAMBDA:
            if (expr.lambda != null) compile_manifest_emit_ffi_libraries(out, expr.lambda.body, first_entry, interp);
            return;
        case lisp::ExprTag.E_APP:
            compile_manifest_emit_ffi_libraries(out, expr.app.func, first_entry, interp);
            compile_manifest_emit_ffi_libraries(out, expr.app.arg, first_entry, interp);
            return;
        case lisp::ExprTag.E_IF:
            compile_manifest_emit_ffi_libraries(out, expr.if_expr.test, first_entry, interp);
            compile_manifest_emit_ffi_libraries(out, expr.if_expr.then_branch, first_entry, interp);
            compile_manifest_emit_ffi_libraries(out, expr.if_expr.else_branch, first_entry, interp);
            return;
        case lisp::ExprTag.E_LET:
            compile_manifest_emit_ffi_libraries(out, expr.let_expr.init, first_entry, interp);
            compile_manifest_emit_ffi_libraries(out, expr.let_expr.body, first_entry, interp);
            return;
        case lisp::ExprTag.E_RESET:
            compile_manifest_emit_ffi_libraries(out, expr.reset.body, first_entry, interp);
            return;
        case lisp::ExprTag.E_SHIFT:
            compile_manifest_emit_ffi_libraries(out, expr.shift.body, first_entry, interp);
            return;
        case lisp::ExprTag.E_PERFORM:
            if (expr.perform.has_tag_expr) compile_manifest_emit_ffi_libraries(out, expr.perform.tag_expr, first_entry, interp);
            compile_manifest_emit_ffi_libraries(out, expr.perform.arg, first_entry, interp);
            return;
        case lisp::ExprTag.E_RESOLVE:
            compile_manifest_emit_ffi_libraries(out, expr.resolve.value, first_entry, interp);
            return;
        case lisp::ExprTag.E_HANDLE:
            if (expr.handle != null) {
                compile_manifest_emit_ffi_libraries(out, expr.handle.body, first_entry, interp);
                for (usz i = 0; i < expr.handle.clause_count; i++) {
                    if (expr.handle.clauses[i].has_effect_tag_expr) {
                        compile_manifest_emit_ffi_libraries(out, expr.handle.clauses[i].effect_tag_expr, first_entry, interp);
                    }
                    compile_manifest_emit_ffi_libraries(out, expr.handle.clauses[i].handler_body, first_entry, interp);
                }
            }
            return;
        case lisp::ExprTag.E_INDEX:
            compile_manifest_emit_ffi_libraries(out, expr.index.collection, first_entry, interp);
            compile_manifest_emit_ffi_libraries(out, expr.index.index, first_entry, interp);
            return;
        case lisp::ExprTag.E_MATCH:
            if (expr.match != null) {
                compile_manifest_emit_ffi_libraries(out, expr.match.scrutinee, first_entry, interp);
                for (usz i = 0; i < expr.match.clause_count; i++) {
                    compile_manifest_emit_ffi_libraries(out, expr.match.clauses[i].result, first_entry, interp);
                }
            }
            return;
        case lisp::ExprTag.E_AND:
            compile_manifest_emit_ffi_libraries(out, expr.and_expr.left, first_entry, interp);
            compile_manifest_emit_ffi_libraries(out, expr.and_expr.right, first_entry, interp);
            return;
        case lisp::ExprTag.E_OR:
            compile_manifest_emit_ffi_libraries(out, expr.or_expr.left, first_entry, interp);
            compile_manifest_emit_ffi_libraries(out, expr.or_expr.right, first_entry, interp);
            return;
        case lisp::ExprTag.E_CALL:
            if (expr.call != null) {
                compile_manifest_emit_ffi_libraries(out, expr.call.func, first_entry, interp);
                for (usz i = 0; i < expr.call.arg_count; i++) {
                    compile_manifest_emit_ffi_libraries(out, expr.call.args[i], first_entry, interp);
                }
            }
            return;
        case lisp::ExprTag.E_MODULE:
            if (expr.module_expr != null) {
                for (usz i = 0; i < expr.module_expr.body_count; i++) {
                    compile_manifest_emit_ffi_libraries(out, expr.module_expr.body[i], first_entry, interp);
                }
            }
            return;
        case lisp::ExprTag.E_WITH_MODULE:
            if (expr.with_module != null) {
                for (usz i = 0; i < expr.with_module.body_count; i++) {
                    compile_manifest_emit_ffi_libraries(out, expr.with_module.body[i], first_entry, interp);
                }
            }
            return;
        case lisp::ExprTag.E_BLOCK:
            if (expr.block != null) {
                for (usz i = 0; i < expr.block.expr_count; i++) {
                    compile_manifest_emit_ffi_libraries(out, expr.block.exprs[i], first_entry, interp);
                }
            }
            return;
        case lisp::ExprTag.E_SET:
            compile_manifest_emit_ffi_libraries(out, expr.set_expr.value, first_entry, interp);
            return;
        case lisp::ExprTag.E_DEFINE:
            compile_manifest_emit_ffi_libraries(out, expr.define.value, first_entry, interp);
            break;
        default:
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

    switch (expr.tag) {
        case lisp::ExprTag.E_LAMBDA:
            if (expr.lambda != null) compile_manifest_emit_ffi_functions(out, expr.lambda.body, first_entry, interp);
            return;
        case lisp::ExprTag.E_APP:
            compile_manifest_emit_ffi_functions(out, expr.app.func, first_entry, interp);
            compile_manifest_emit_ffi_functions(out, expr.app.arg, first_entry, interp);
            return;
        case lisp::ExprTag.E_IF:
            compile_manifest_emit_ffi_functions(out, expr.if_expr.test, first_entry, interp);
            compile_manifest_emit_ffi_functions(out, expr.if_expr.then_branch, first_entry, interp);
            compile_manifest_emit_ffi_functions(out, expr.if_expr.else_branch, first_entry, interp);
            return;
        case lisp::ExprTag.E_LET:
            compile_manifest_emit_ffi_functions(out, expr.let_expr.init, first_entry, interp);
            compile_manifest_emit_ffi_functions(out, expr.let_expr.body, first_entry, interp);
            return;
        case lisp::ExprTag.E_RESET:
            compile_manifest_emit_ffi_functions(out, expr.reset.body, first_entry, interp);
            return;
        case lisp::ExprTag.E_SHIFT:
            compile_manifest_emit_ffi_functions(out, expr.shift.body, first_entry, interp);
            return;
        case lisp::ExprTag.E_PERFORM:
            if (expr.perform.has_tag_expr) compile_manifest_emit_ffi_functions(out, expr.perform.tag_expr, first_entry, interp);
            compile_manifest_emit_ffi_functions(out, expr.perform.arg, first_entry, interp);
            return;
        case lisp::ExprTag.E_RESOLVE:
            compile_manifest_emit_ffi_functions(out, expr.resolve.value, first_entry, interp);
            return;
        case lisp::ExprTag.E_HANDLE:
            if (expr.handle != null) {
                compile_manifest_emit_ffi_functions(out, expr.handle.body, first_entry, interp);
                for (usz i = 0; i < expr.handle.clause_count; i++) {
                    if (expr.handle.clauses[i].has_effect_tag_expr) {
                        compile_manifest_emit_ffi_functions(out, expr.handle.clauses[i].effect_tag_expr, first_entry, interp);
                    }
                    compile_manifest_emit_ffi_functions(out, expr.handle.clauses[i].handler_body, first_entry, interp);
                }
            }
            return;
        case lisp::ExprTag.E_INDEX:
            compile_manifest_emit_ffi_functions(out, expr.index.collection, first_entry, interp);
            compile_manifest_emit_ffi_functions(out, expr.index.index, first_entry, interp);
            return;
        case lisp::ExprTag.E_MATCH:
            if (expr.match != null) {
                compile_manifest_emit_ffi_functions(out, expr.match.scrutinee, first_entry, interp);
                for (usz i = 0; i < expr.match.clause_count; i++) {
                    compile_manifest_emit_ffi_functions(out, expr.match.clauses[i].result, first_entry, interp);
                }
            }
            return;
        case lisp::ExprTag.E_AND:
            compile_manifest_emit_ffi_functions(out, expr.and_expr.left, first_entry, interp);
            compile_manifest_emit_ffi_functions(out, expr.and_expr.right, first_entry, interp);
            return;
        case lisp::ExprTag.E_OR:
            compile_manifest_emit_ffi_functions(out, expr.or_expr.left, first_entry, interp);
            compile_manifest_emit_ffi_functions(out, expr.or_expr.right, first_entry, interp);
            return;
        case lisp::ExprTag.E_CALL:
            if (expr.call != null) {
                compile_manifest_emit_ffi_functions(out, expr.call.func, first_entry, interp);
                for (usz i = 0; i < expr.call.arg_count; i++) {
                    compile_manifest_emit_ffi_functions(out, expr.call.args[i], first_entry, interp);
                }
            }
            return;
        case lisp::ExprTag.E_MODULE:
            if (expr.module_expr != null) {
                for (usz i = 0; i < expr.module_expr.body_count; i++) {
                    compile_manifest_emit_ffi_functions(out, expr.module_expr.body[i], first_entry, interp);
                }
            }
            return;
        case lisp::ExprTag.E_WITH_MODULE:
            if (expr.with_module != null) {
                for (usz i = 0; i < expr.with_module.body_count; i++) {
                    compile_manifest_emit_ffi_functions(out, expr.with_module.body[i], first_entry, interp);
                }
            }
            return;
        case lisp::ExprTag.E_BLOCK:
            if (expr.block != null) {
                for (usz i = 0; i < expr.block.expr_count; i++) {
                    compile_manifest_emit_ffi_functions(out, expr.block.exprs[i], first_entry, interp);
                }
            }
            return;
        case lisp::ExprTag.E_SET:
            compile_manifest_emit_ffi_functions(out, expr.set_expr.value, first_entry, interp);
            return;
        case lisp::ExprTag.E_DEFINE:
            compile_manifest_emit_ffi_functions(out, expr.define.value, first_entry, interp);
            break;
        default:
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
    if (expr.tag == lisp::ExprTag.E_LAMBDA) {
        return expr.lambda != null && compile_manifest_has_ffi_decl(expr.lambda.body);
    }
    if (expr.tag == lisp::ExprTag.E_APP) {
        return compile_manifest_has_ffi_decl(expr.app.func) || compile_manifest_has_ffi_decl(expr.app.arg);
    }
    if (expr.tag == lisp::ExprTag.E_IF) {
        return compile_manifest_has_ffi_decl(expr.if_expr.test)
            || compile_manifest_has_ffi_decl(expr.if_expr.then_branch)
            || compile_manifest_has_ffi_decl(expr.if_expr.else_branch);
    }
    if (expr.tag == lisp::ExprTag.E_LET) {
        return compile_manifest_has_ffi_decl(expr.let_expr.init)
            || compile_manifest_has_ffi_decl(expr.let_expr.body);
    }
    if (expr.tag == lisp::ExprTag.E_RESET) return compile_manifest_has_ffi_decl(expr.reset.body);
    if (expr.tag == lisp::ExprTag.E_SHIFT) return compile_manifest_has_ffi_decl(expr.shift.body);
    if (expr.tag == lisp::ExprTag.E_PERFORM) {
        return (expr.perform.has_tag_expr && compile_manifest_has_ffi_decl(expr.perform.tag_expr))
            || compile_manifest_has_ffi_decl(expr.perform.arg);
    }
    if (expr.tag == lisp::ExprTag.E_RESOLVE) return compile_manifest_has_ffi_decl(expr.resolve.value);
    if (expr.tag == lisp::ExprTag.E_HANDLE && expr.handle != null) {
        if (compile_manifest_has_ffi_decl(expr.handle.body)) return true;
        for (usz i = 0; i < expr.handle.clause_count; i++) {
            if (expr.handle.clauses[i].has_effect_tag_expr &&
                compile_manifest_has_ffi_decl(expr.handle.clauses[i].effect_tag_expr)) return true;
            if (compile_manifest_has_ffi_decl(expr.handle.clauses[i].handler_body)) return true;
        }
        return false;
    }
    if (expr.tag == lisp::ExprTag.E_INDEX) {
        return compile_manifest_has_ffi_decl(expr.index.collection)
            || compile_manifest_has_ffi_decl(expr.index.index);
    }
    if (expr.tag == lisp::ExprTag.E_MATCH && expr.match != null) {
        if (compile_manifest_has_ffi_decl(expr.match.scrutinee)) return true;
        for (usz i = 0; i < expr.match.clause_count; i++) {
            if (compile_manifest_has_ffi_decl(expr.match.clauses[i].result)) return true;
        }
        return false;
    }
    if (expr.tag == lisp::ExprTag.E_AND) {
        return compile_manifest_has_ffi_decl(expr.and_expr.left)
            || compile_manifest_has_ffi_decl(expr.and_expr.right);
    }
    if (expr.tag == lisp::ExprTag.E_OR) {
        return compile_manifest_has_ffi_decl(expr.or_expr.left)
            || compile_manifest_has_ffi_decl(expr.or_expr.right);
    }
    if (expr.tag == lisp::ExprTag.E_CALL && expr.call != null) {
        if (compile_manifest_has_ffi_decl(expr.call.func)) return true;
        for (usz i = 0; i < expr.call.arg_count; i++) {
            if (compile_manifest_has_ffi_decl(expr.call.args[i])) return true;
        }
        return false;
    }
    if (expr.tag == lisp::ExprTag.E_MODULE && expr.module_expr != null) {
        for (usz i = 0; i < expr.module_expr.body_count; i++) {
            if (compile_manifest_has_ffi_decl(expr.module_expr.body[i])) return true;
        }
    }
    if (expr.tag == lisp::ExprTag.E_WITH_MODULE && expr.with_module != null) {
        for (usz i = 0; i < expr.with_module.body_count; i++) {
            if (compile_manifest_has_ffi_decl(expr.with_module.body[i])) return true;
        }
    }
    if (expr.tag == lisp::ExprTag.E_BLOCK && expr.block != null) {
        for (usz i = 0; i < expr.block.expr_count; i++) {
            if (compile_manifest_has_ffi_decl(expr.block.exprs[i])) return true;
        }
    }
    if (expr.tag == lisp::ExprTag.E_SET) return compile_manifest_has_ffi_decl(expr.set_expr.value);
    if (expr.tag == lisp::ExprTag.E_DEFINE) return compile_manifest_has_ffi_decl(expr.define.value);
    return false;
}

fn EntryFileWriteKind compile_manifest_unlink_failure_kind(char[] path) {
    EntryFileReadKind read_kind = ENTRY_FILE_READ_GENERIC_FAILURE;
    if (classify_file_read_path(path, &read_kind)) return ENTRY_FILE_WRITE_GENERIC_FAILURE;
    switch (read_kind) {
        case ENTRY_FILE_READ_PERMISSION_DENIED:
            return ENTRY_FILE_WRITE_PERMISSION_DENIED;
        case ENTRY_FILE_READ_PATH_FAILURE:
            return ENTRY_FILE_WRITE_PATH_FAILURE;
        default:
            return ENTRY_FILE_WRITE_GENERIC_FAILURE;
    }
}

fn bool compile_manifest_remove_stale(
    DString* manifest_path_builder,
    char[]* manifest_path_out,
    EntryFileWriteKind* write_kind_out
) {
    char[] stale_manifest_path = manifest_path_builder.str_view().copy(mem);
    if (stale_manifest_path.len > 0) {
        int unlink_rc = entry_file_uv_unlink((ZString)stale_manifest_path.ptr);
        if (unlink_rc == 0) {
            mem::free(stale_manifest_path.ptr);
            return true;
        }
        EntryFileReadKind read_kind = ENTRY_FILE_READ_GENERIC_FAILURE;
        if (!classify_file_read_path(stale_manifest_path, &read_kind) &&
            read_kind == ENTRY_FILE_READ_NOT_FOUND) {
            mem::free(stale_manifest_path.ptr);
            return true;
        }
        *manifest_path_out = stale_manifest_path;
        *write_kind_out = compile_manifest_unlink_failure_kind(stale_manifest_path);
        return false;
    }
    return false;
}

fn bool compile_manifest_write_atomic_text(char[] final_path, char[] text) {
    char[4160] temp_path_buf;
    char[] temp_path = "";
    EntryFileWriteKind kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
    if (!entry_write_text_staged(final_path, text, temp_path_buf[..], &temp_path, &kind)) return false;
    bool committed = false;
    defer if (!committed) entry_cleanup_staged_text(temp_path);
    if (g_compile_manifest_force_fail_before_rename) return false;
    if (!entry_commit_staged_text(temp_path, final_path, &kind)) return false;
    committed = true;
    return true;
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

    DString manifest_path_builder;
    manifest_path_builder.init(mem, output_path.len + 32);
    defer manifest_path_builder.free();
    manifest_path_builder.append_string((String)output_path);
    manifest_path_builder.append_string(".ffi-manifest.json");

    List{lisp::Expr*} exprs = lisp::parse_program(source, interp);
    if (exprs.len() == 0) {
        return compile_manifest_remove_stale(&manifest_path_builder, manifest_path_out, write_kind_out);
    }
    defer exprs.free();

    lisp::Compiler compiler;
    compiler.init(interp);
    defer compiler.free();
    if (!compiler.expand_program_macros_for_analysis(&exprs)) return false;

    bool has_ffi = false;
    for (usz i = 0; i < exprs.len(); i++) {
        if (compile_manifest_has_ffi_decl(exprs[i])) {
            has_ffi = true;
            break;
        }
    }
    if (!has_ffi) {
        return compile_manifest_remove_stale(&manifest_path_builder, manifest_path_out, write_kind_out);
    }

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
    if (!write_ok) {
        char[] cleanup_manifest_path = "";
        (void)compile_manifest_remove_stale(&manifest_path_builder, &cleanup_manifest_path, write_kind_out);
        if (cleanup_manifest_path.len > 0) mem::free(cleanup_manifest_path.ptr);
        if (*write_kind_out == ENTRY_FILE_WRITE_GENERIC_FAILURE) *write_kind_out = classify_file_write_errno();
    }
    return write_ok;
}
