module lisp;

import std::collections::list;
import main;

fn bool compiler_expr_is_define_like(Expr* expr) {
    if (expr == null) return false;
    switch (expr.tag) {
        case E_DEFINE:
        case E_FFI_LIB:
        case E_FFI_FN:
            return true;
        case E_BLOCK:
            if (expr.block == null) return false;
            for (usz i = 0; i < expr.block.expr_count; i++) {
                if (!compiler_expr_is_define_like(expr.block.exprs[i])) return false;
            }
            return true;
        default:
            return false;
    }
}

fn isz find_last_non_define(List{Expr*}* exprs) {
    isz last_non_define = -1;
    for (usz i = 0; i < exprs.len(); i++) {
        if (!compiler_expr_is_define_like((*exprs)[i])) {
            last_non_define = (isz)i;
        }
    }
    return last_non_define;
}

fn void Compiler.emit_top_level_exprs(Compiler* self, List{Expr*}* exprs) {
    isz last_non_define = -1;
    if (self.print_last) {
        last_non_define = find_last_non_define(exprs);
    }

    bool print_all_active = false;
    for (usz i = 0; i < exprs.len(); i++) {
        if ((*exprs)[i].tag == E_DEFINE) {
            char[] dname = self.interp.symbols.get_name((*exprs)[i].define.name);
            if (self.str_eq(dname, "__e2e_start__")) {
                print_all_active = true;
                usz init_r = self.compile_define_rhs_with_typed_bridge((*exprs)[i]);
                self.emit_indent();
                self.emit_symbol_name((*exprs)[i].define.name);
                self.emit(" = ");
                self.emit_temp_ref(init_r);
                self.emit(";\n");
                continue;
            }
        }
        if ((*exprs)[i].tag == E_FFI_LIB) {
            usz init_r = self.compile_to_temp((*exprs)[i]);
            self.emit_indent();
            self.emit_symbol_name((*exprs)[i].ffi_lib.name);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
            continue;
        }
        if ((*exprs)[i].tag == E_FFI_FN && (*exprs)[i].ffi_fn != null) {
            usz init_r = self.compile_to_temp((*exprs)[i]);
            self.emit_indent();
            self.emit_symbol_name((*exprs)[i].ffi_fn.fn_name);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
            continue;
        }

        if ((*exprs)[i].tag == E_IMPORT) {
            ExprImport imp = (*exprs)[i].import_expr;
            CompiledModule* inline_mod = imp.has_path ? null : self.find_inline_compiled_module(imp.name);
            if (inline_mod != null) {
                if (imp.import_all) {
                    for (usz ii = 0; ii < inline_mod.export_count; ii++) {
                        SymbolId backing = INVALID_SYMBOL_ID;
                        if (!self.find_inline_module_export_backing(imp.name, inline_mod.exports[ii], &backing)) {
                            self.set_compile_error("compiler: inline import all could not resolve export backing");
                            break;
                        }
                        self.emit_indent();
                        self.emit_symbol_name(inline_mod.exports[ii]);
                        self.emit(" = ");
                        self.emit_symbol_name(backing);
                        self.emit("; // import: inline module\n");
                    }
                    continue;
                }
                if (imp.import_count == 0) {
                    self.emit_indent();
                    self.emit("(void)aot::make_void(); // import: inline module\n");
                    continue;
                }
                for (usz ii = 0; ii < imp.import_count; ii++) {
                    SymbolId bind_name = (uint)imp.aliases[ii] != 0 ? imp.aliases[ii] : imp.imports[ii];
                    SymbolId backing = INVALID_SYMBOL_ID;
                    if (!self.find_inline_module_export_backing(imp.name, imp.imports[ii], &backing)) {
                        self.set_compile_error("compiler: inline selective import could not resolve export backing");
                        break;
                    }
                    self.emit_indent();
                    self.emit_symbol_name(bind_name);
                    self.emit(" = ");
                    self.emit_symbol_name(backing);
                    self.emit("; // import: inline module\n");
                }
                continue;
            }
            if (!imp.has_path
                && imp.import_count == 0
                && compiler_has_inline_compiled_module(self, imp.name)) {
                self.emit_indent();
                self.emit("(void)aot::make_void(); // import: inline module\n");
                continue;
            }
            if (imp.has_path) {
                self.emit_indent();
                self.emit("aot::import_module(\"");
                self.emit_escaped(self.interp.symbols.get_name(imp.name));
                self.emit("\", ");
                self.emit("\"");
                self.emit_escaped(imp.path[:imp.path_len]);
                self.emit("\", true, ");
                self.emit(imp.import_all ? "true" : "false");
                self.emit(");\n");
            } else {
                self.emit_indent();
                self.emit_symbol_name(imp.name);
                self.emit(" = aot::import_module(\"");
                self.emit_escaped(self.interp.symbols.get_name(imp.name));
                self.emit("\", \"\", false, ");
                self.emit(imp.import_all ? "true" : "false");
                self.emit(");\n");
            }

            for (usz ii = 0; ii < imp.import_count; ii++) {
                SymbolId bind_name = (uint)imp.aliases[ii] != 0 ? imp.aliases[ii] : imp.imports[ii];
                self.emit_indent();
                self.emit_symbol_name(bind_name);
                self.emit(" = aot::lookup_var(\"");
                self.emit_escaped(self.interp.symbols.get_name(imp.imports[ii]));
                self.emit("\");\n");
            }
            continue;
        }

        bool should_print = (self.print_all && print_all_active)
            || (self.print_last && (isz)i == last_non_define);

        if (should_print) {
            usz r = self.compile_to_temp((*exprs)[i]);
            self.emit_indent();
            self.emit("aot::print_value(");
            self.emit_temp_ref(r);
            self.emit(");\n");
            self.emit_type_form_global_sync((*exprs)[i]);
        } else if ((*exprs)[i].tag == E_DEFINE) {
            usz init_r = self.compile_define_rhs_with_typed_bridge((*exprs)[i]);
            self.emit_indent();
            self.emit_symbol_name((*exprs)[i].define.name);
            self.emit(" = ");
            self.emit_temp_ref(init_r);
            self.emit(";\n");
        } else {
            self.compile_to_temp((*exprs)[i]);
            self.emit_type_form_global_sync((*exprs)[i]);
        }
    }
}
