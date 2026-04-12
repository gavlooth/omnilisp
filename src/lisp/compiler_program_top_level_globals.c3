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
        case E_BLOCK:
            if (expr.block != null) {
                for (usz bi = 0; bi < expr.block.expr_count; bi++) {
                    self.collect_expr_generated_globals(expr.block.exprs[bi]);
                }
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
