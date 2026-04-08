module main;

import std::io;
import std::collections::list;
import lisp;

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
            default:
                out.append_char(c);
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

    if (try file = io::file::open((String)manifest_path, "w")) {
        char[] remaining = json.str_view();
        bool write_ok = true;
        while (write_ok && remaining.len > 0) {
            usz? written_opt = file.write(remaining);
            if (catch err = written_opt) {
                *write_kind_out = classify_file_write_errno();
                write_ok = false;
                break;
            }
            if (written_opt == 0) {
                write_ok = false;
                break;
            }
            remaining = remaining[written_opt..];
        }
        if (catch err = file.close()) {
            *write_kind_out = classify_file_write_errno();
            write_ok = false;
        }
        return write_ok;
    }

    *write_kind_out = classify_file_write_errno();
    return false;
}

fn int run_compile_mode(char* input_file, char* output_file) {
    if (input_file == null || cli_arg_starts_with_dash(input_file)) {
        if (input_file != null) {
            io::printfn("Error: missing compile input file before option %s", (ZString)input_file);
        }
        return print_compile_usage();
    }
    if (output_file == null || cli_arg_starts_with_dash(output_file)) {
        if (output_file != null) {
            io::printfn("Error: missing compile output file before option %s", (ZString)output_file);
        }
        return print_compile_usage();
    }

    // Read input file
    char[] input_path = input_file[:cstr_len(input_file)];
    if (try source = io::file::load_temp((String)input_path)) {
        EntryFileWriteKind output_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
        if (!validate_output_target_path(output_file, &output_kind)) {
            print_compile_write_error(output_file[:cstr_len(output_file)], output_kind);
            return 1;
        }

        print_compile_start(input_file, output_file);

        // Initialize thread-local registry before interpreter
        thread_registry_init();

        // Initialize interpreter for symbol table (heap-allocated due to size)
        lisp::Interp* interp = create_plain_interp();
        if (interp == null) {
            print_compile_alloc_error();
            thread_registry_shutdown();
            return 1;
        }

        // Compile to C3
        char[] compile_error = "";
        char[] c3_code = lisp::compile_to_c3(source, interp, &compile_error);

        if (c3_code.len == 0) {
            if (compile_error.len > 0) print_compile_error_message(compile_error);
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 1;
        }

        // Write output file
        char[] output_path = output_file[:cstr_len(output_file)];
        if (try file = io::file::open((String)output_path, "w")) {
            char[] remaining = c3_code;
            bool write_ok = true;
            EntryFileWriteKind write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            while (write_ok && remaining.len > 0) {
                usz? written_opt = file.write(remaining);
                if (catch err = written_opt) {
                    write_kind = classify_file_write_errno();
                    write_ok = false;
                    break;
                }
                if (written_opt == 0) {
                    write_ok = false;
                    break;
                }
                remaining = remaining[written_opt..];
            }
            if (catch err = file.close()) {
                write_kind = classify_file_write_errno();
                write_ok = false;
            }
            if (!write_ok) {
                print_compile_write_error(output_path, write_kind);
                destroy_plain_interp(interp);
                thread_registry_shutdown();
                return 1;
            }
            char[] manifest_path = "";
            EntryFileWriteKind manifest_write_kind = ENTRY_FILE_WRITE_GENERIC_FAILURE;
            defer if (manifest_path.len > 0) mem::free(manifest_path.ptr);
            if (!write_compile_ffi_manifest(source, input_path, output_path, interp, &manifest_path, &manifest_write_kind)) {
                if (manifest_path.len > 0) {
                    print_compile_write_error(manifest_path, manifest_write_kind);
                } else {
                    print_compile_write_error(output_path, manifest_write_kind);
                }
                destroy_plain_interp(interp);
                thread_registry_shutdown();
                return 1;
            }

            print_compile_success(output_file);
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 0;
        } else {
            print_compile_write_error(output_path, classify_file_write_errno());
            destroy_plain_interp(interp);
            thread_registry_shutdown();
            return 1;
        }
    } else {
        EntryFileReadKind read_kind = ENTRY_FILE_READ_GENERIC_FAILURE;
        if (!classify_file_read_path(input_path, &read_kind)) {
            return print_compile_read_error(input_path, read_kind);
        }
        return print_compile_read_error(input_path, ENTRY_FILE_READ_GENERIC_FAILURE);
    }
}
