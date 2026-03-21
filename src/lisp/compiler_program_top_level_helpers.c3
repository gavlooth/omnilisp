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
