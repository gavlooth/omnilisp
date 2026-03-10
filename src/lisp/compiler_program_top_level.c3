module lisp;

import std::collections::list;
import main;

fn void Compiler.add_global_if_missing(Compiler* self, SymbolId sym) {
    if (!self.is_global(sym)) {
        self.defined_globals.push(sym);
    }
}

fn void Compiler.collect_expr_generated_globals(Compiler* self, Expr* expr) {
    if (expr == null) return;

    switch (expr.tag) {
        case E_DEFINE:
            self.add_global_if_missing(expr.define.name);
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

fn isz find_last_non_define(List{Expr*}* exprs) {
    isz last_non_define = -1;
    for (usz i = 0; i < exprs.len(); i++) {
        if ((*exprs)[i].tag != E_DEFINE) {
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
