module lisp;

import std::collections::list;
import main;

fn void Compiler.add_global_if_missing(Compiler* self, SymbolId sym) {
    if (!self.is_global(sym)) {
        self.defined_globals.push(sym);
    }
}

fn bool compiler_has_inline_compiled_module(Compiler* self, SymbolId module_name) {
    for (usz i = 0; i < self.compiled_module_count; i++) {
        if (self.compiled_modules[i].name == module_name) return true;
    }
    return false;
}

fn void Compiler.collect_expr_generated_globals(Compiler* self, Expr* expr) {
    if (expr == null) return;

    switch (expr.tag) {
        case E_DEFINE:
            self.add_global_if_missing(expr.define.name);
        case E_FFI_LIB:
            self.add_global_if_missing(expr.ffi_lib.name);
        case E_FFI_FN:
            if (expr.ffi_fn != null) {
                self.add_global_if_missing(expr.ffi_fn.fn_name);
            }
        case E_DEFTYPE:
            if (expr.deftype != null) {
                self.add_global_if_missing(expr.deftype.name);
            }
        case E_DEFUNION:
            if (expr.defunion != null) {
                for (usz vi = 0; vi < expr.defunion.variant_count; vi++) {
                    self.add_global_if_missing(expr.defunion.variants[vi].name);
                }
            }
        default:
    }
}

fn void Compiler.collect_top_level_defines(Compiler* self, List{Expr*}* exprs) {
    foreach (expr : *exprs) {
        if (expr.tag == E_MODULE) {
            // Module body definitions become globals when compiled inline.
            for (usz mi = 0; mi < expr.module_expr.body_count; mi++) {
                self.collect_expr_generated_globals(expr.module_expr.body[mi]);
            }
        } else if (expr.tag == E_IMPORT) {
            if (!expr.import_expr.has_path) {
                self.add_global_if_missing(expr.import_expr.name);
            }
            for (usz ii = 0; ii < expr.import_expr.import_count; ii++) {
                SymbolId imp_name = expr.import_expr.imports[ii];
                SymbolId alias_name = expr.import_expr.aliases[ii];
                self.add_global_if_missing(imp_name);
                if ((uint)alias_name != 0) {
                    self.add_global_if_missing(alias_name);
                }
            }
        } else {
            self.collect_expr_generated_globals(expr);
        }
    }
}

fn void Compiler.emit_global_lookup_sync(Compiler* self, SymbolId sym) {
    self.emit_indent();
    self.emit_symbol_name(sym);
    self.emit(" = aot::lookup_var(\"");
    self.emit_escaped(self.interp.symbols.get_name(sym));
    self.emit("\");\n");
}

fn void Compiler.emit_type_form_global_sync(Compiler* self, Expr* expr) {
    if (expr == null) return;

    switch (expr.tag) {
        case E_DEFTYPE:
            if (expr.deftype != null && self.is_global(expr.deftype.name)) {
                self.emit_global_lookup_sync(expr.deftype.name);
            }
        case E_DEFUNION:
            if (expr.defunion != null) {
                for (usz vi = 0; vi < expr.defunion.variant_count; vi++) {
                    SymbolId variant_name = expr.defunion.variants[vi].name;
                    if (self.is_global(variant_name)) {
                        self.emit_global_lookup_sync(variant_name);
                    }
                }
            }
        default:
    }
}

fn bool Compiler.has_ffi_preload_expr(Compiler* self, Expr* expr) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_FFI_LIB:
        case E_FFI_FN:
            return true;
        case E_MODULE:
            if (expr.module_expr == null) return false;
            for (usz i = 0; i < expr.module_expr.body_count; i++) {
                if (self.has_ffi_preload_expr(expr.module_expr.body[i])) return true;
            }
            return false;
        default:
            return false;
    }
}

fn void Compiler.emit_ffi_preload_expr_phase(Compiler* self, Expr* expr, bool libs_phase) {
    if (expr == null) return;
    if (expr.tag == E_MODULE && expr.module_expr != null) {
        for (usz i = 0; i < expr.module_expr.body_count; i++) {
            self.emit_ffi_preload_expr_phase(expr.module_expr.body[i], libs_phase);
        }
        return;
    }

    if (libs_phase) {
        if (expr.tag != E_FFI_LIB || expr.ffi_lib.path_expr == null
            || expr.ffi_lib.path_expr.tag != E_LIT
            || expr.ffi_lib.path_expr.lit.value.tag != STRING) {
            return;
        }
        self.emit_indent();
        self.emit_ffi_lib_call(expr, true);
        self.emit(";\n");
        return;
    }

    if (expr.tag != E_FFI_FN || expr.ffi_fn == null || expr.ffi_fn.is_variadic
        || expr.ffi_fn.c_name == null || expr.ffi_fn.c_name_len == 0) {
        return;
    }

    int* param_tags = null;
    int return_tag = 0;
    char[] lib_name = "";
    char[] fn_name = "";
    char[] c_symbol = "";
    usz param_count = 0;
    if (!self.prepare_ffi_fn_call(expr, &param_tags, &return_tag, &lib_name, &fn_name, &c_symbol, &param_count)) {
        if (param_tags != null) mem::free(param_tags);
        return;
    }
    self.emit_indent();
    self.emit_ffi_fn_call_prepared(lib_name, fn_name, c_symbol, param_tags, param_count, expr.ffi_fn.has_return_type, return_tag, true);
    self.emit(";\n");
    if (param_tags != null) mem::free(param_tags);
}

fn void Compiler.emit_ffi_startup_preload(Compiler* self, List{Expr*}* exprs) {
    if (exprs == null || exprs.len() == 0) return;
    bool has_ffi = false;
    foreach (expr : *exprs) {
        if (self.has_ffi_preload_expr(expr)) {
            has_ffi = true;
            break;
        }
    }
    if (!has_ffi) return;

    self.emit_line("// Startup FFI preload");
    for (usz i = 0; i < exprs.len(); i++) {
        self.emit_ffi_preload_expr_phase((*exprs)[i], true);
    }
    for (usz i = 0; i < exprs.len(); i++) {
        self.emit_ffi_preload_expr_phase((*exprs)[i], false);
    }
    self.emit_newline();
}

fn char[] ffi_manifest_type_name(FfiTypeTag tag) {
    switch (tag) {
        case FFI_TYPE_VOID:
            return "Void";
        case FFI_TYPE_INT:
            return "Integer";
        case FFI_TYPE_DOUBLE:
            return "Double";
        case FFI_TYPE_PTR:
            return "Pointer";
        case FFI_TYPE_BOOL:
            return "Boolean";
        case FFI_TYPE_STRING:
            return "String";
        default:
            return "Void";
    }
}

fn void Compiler.emit_json_string(Compiler* self, char[] s) {
    self.emit_char('"');
    foreach (c : s) {
        switch (c) {
            case '"':
                self.emit("\\\"");
            case '\\':
                self.emit("\\\\");
            case '\n':
                self.emit("\\n");
            case '\r':
                self.emit("\\r");
            case '\t':
                self.emit("\\t");
            case '`':
                self.emit("\\u0060");
            default:
                self.emit_char(c);
        }
    }
    self.emit_char('"');
}

fn void Compiler.emit_ffi_manifest_signature_json(
    Compiler* self,
    int* param_tags,
    usz param_count,
    bool has_return_type,
    int return_tag
) {
    self.emit("{\n");
    self.emit("        \"params\": [");
    for (usz i = 0; i < param_count; i++) {
        if (i > 0) self.emit(", ");
        self.emit_json_string(ffi_manifest_type_name((FfiTypeTag)param_tags[i]));
    }
    self.emit("],\n");
    self.emit("        \"return\": ");
    self.emit_json_string(ffi_manifest_type_name((FfiTypeTag)(has_return_type ? return_tag : (int)FfiTypeTag.FFI_TYPE_VOID)));
    self.emit("\n      }");
}

fn void Compiler.emit_ffi_manifest_lib_entry(Compiler* self, Expr* expr, bool* first) {
    if (expr == null || expr.tag != E_FFI_LIB) return;
    if (expr.ffi_lib.path_expr == null
        || expr.ffi_lib.path_expr.tag != E_LIT
        || expr.ffi_lib.path_expr.lit.value.tag != STRING) {
        return;
    }

    if (!*first) self.emit(",\n");
    *first = false;

    ExprFfiLib* fl = &expr.ffi_lib;
    char[] lib_name = self.interp.symbols.get_name(fl.name);
    char[] soname = fl.path_expr.lit.value.str_chars[:fl.path_expr.lit.value.str_len];

    self.emit("      {\n");
    self.emit("        \"binding\": ");
    self.emit_json_string(lib_name);
    self.emit(",\n");
    self.emit("        \"soname\": ");
    self.emit_json_string(soname);
    self.emit(",\n");
    self.emit("        \"eager_load\": true,\n");
    self.emit("        \"required_library\": true\n");
    self.emit("      }");
}

fn void Compiler.emit_ffi_manifest_fn_entry(Compiler* self, Expr* expr, bool* first) {
    if (expr == null || expr.tag != E_FFI_FN || expr.ffi_fn == null || expr.ffi_fn.is_variadic
        || expr.ffi_fn.c_name == null || expr.ffi_fn.c_name_len == 0) {
        return;
    }

    int* param_tags = null;
    int return_tag = 0;
    char[] lib_name = "";
    char[] fn_name = "";
    char[] c_symbol = "";
    usz param_count = 0;
    if (!self.prepare_ffi_fn_call(expr, &param_tags, &return_tag, &lib_name, &fn_name, &c_symbol, &param_count)) {
        if (param_tags != null) mem::free(param_tags);
        return;
    }

    if (!*first) self.emit(",\n");
    *first = false;

    self.emit("      {\n");
    self.emit("        \"binding\": ");
    self.emit_json_string(fn_name);
    self.emit(",\n");
    self.emit("        \"library\": ");
    self.emit_json_string(lib_name);
    self.emit(",\n");
    self.emit("        \"symbol\": ");
    self.emit_json_string(c_symbol);
    self.emit(",\n");
    self.emit("        \"eager_resolve\": true,\n");
    self.emit("        \"required_library\": true,\n");
    self.emit("        \"required_symbol\": true,\n");
    self.emit("        \"signature\": ");
    self.emit_ffi_manifest_signature_json(param_tags, param_count, expr.ffi_fn.has_return_type, return_tag);
    self.emit("\n      }");

    if (param_tags != null) mem::free(param_tags);
}

fn void Compiler.emit_ffi_contract_manifest(Compiler* self, List{Expr*}* exprs) {
    if (exprs == null || exprs.len() == 0) return;
    bool has_ffi = false;
    foreach (expr : *exprs) {
        if (self.has_ffi_preload_expr(expr)) {
            has_ffi = true;
            break;
        }
    }
    if (!has_ffi) return;

    self.emit("const char[] __omni_ffi_contract_json = `{\n");
    self.emit("  \"format\": \"omni.ffi-contract/v1\",\n");
    self.emit("  \"libraries\": [\n");
    bool first_lib = true;
    for (usz i = 0; i < exprs.len(); i++) {
        Expr* expr = (*exprs)[i];
        if (expr.tag == E_MODULE && expr.module_expr != null) {
            for (usz mi = 0; mi < expr.module_expr.body_count; mi++) {
                self.emit_ffi_manifest_lib_entry(expr.module_expr.body[mi], &first_lib);
            }
        } else {
            self.emit_ffi_manifest_lib_entry(expr, &first_lib);
        }
    }
    self.emit("\n  ],\n");
    self.emit("  \"functions\": [\n");
    bool first_fn = true;
    for (usz i = 0; i < exprs.len(); i++) {
        Expr* expr = (*exprs)[i];
        if (expr.tag == E_MODULE && expr.module_expr != null) {
            for (usz mi = 0; mi < expr.module_expr.body_count; mi++) {
                self.emit_ffi_manifest_fn_entry(expr.module_expr.body[mi], &first_fn);
            }
        } else {
            self.emit_ffi_manifest_fn_entry(expr, &first_fn);
        }
    }
    self.emit("\n  ]\n");
    self.emit("}`;\n\n");
}
