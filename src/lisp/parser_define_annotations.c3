module lisp;

import std::collections::list;

fn bool Parser.parse_define_attrs(Parser* self, List{SymbolId}* attrs) {
    while (self.lexer.current.type == T_SYMBOL) {
        attrs.push(self.get_current_symbol());
        self.lexer.advance();
    }
    if (attrs.len() == 0) {
        self.set_error("expected attribute after [");
        return false;
    }

    self.expect(T_RBRACKET, "]");
    if (self.has_error) return false;
    return true;
}

fn Expr* Parser.parse_define_with_annotation(Parser* self, Expr* e, List{SymbolId}* attrs) {
    SymbolId annotation = (*attrs)[0];

    // Type-related defines: [type], [struct], [abstract], [union], [alias]
    if (annotation == self.interp.sym_type ||
        annotation == self.interp.sym_struct ||
        annotation == self.interp.sym_abstract ||
        annotation == self.interp.sym_union ||
        annotation == self.interp.sym_alias) {
        return self.parse_define_type(e, annotation);
    }

    // FFI defines: [ffi lib], [ffi λ], [ffi module]
    if (annotation == self.interp.sym_ffi) {
        return self.parse_define_ffi(e, attrs);
    }

    // Special defines: [macro], [effect], [schema], [relation]
    return self.parse_define_special(e, annotation, attrs);
}

/**
 * Handle type-related bracket defines: [type], [struct], [abstract], [union], [alias]
 * Dispatches to the appropriate existing sub-parser.
 */
fn Expr* Parser.parse_define_type(Parser* self, Expr* e, SymbolId annotation) {
    if (annotation == self.interp.sym_type || annotation == self.interp.sym_struct) {
        return self.parse_deftype(e);
    }
    if (annotation == self.interp.sym_abstract) {
        return self.parse_defabstract(e);
    }
    if (annotation == self.interp.sym_union) {
        return self.parse_defunion(e);
    }
    if (annotation == self.interp.sym_alias) {
        return self.parse_defalias(e);
    }
    self.set_error("unknown type define attribute");
    return null;
}

/**
 * Handle FFI bracket defines: [ffi lib], [ffi λ], [ffi module]
 * Dispatches to parse_ffi_lib, parse_ffi_fn, or parse_ffi_module.
 */
fn Expr* Parser.parse_define_ffi(Parser* self, Expr* e, List{SymbolId}* attrs) {
    if (attrs.len() < 2) { self.set_error("expected 'lib', 'λ'/'lambda', or 'module' after [ffi"); return null; }
    if ((*attrs)[1] == self.interp.sym_lib) {
        return self.parse_ffi_lib(e);
    }
    if ((*attrs)[1] == self.interp.sym_module) {
        return self.parse_ffi_module(e, attrs);
    }
    if (symbol_is_lambda_spelling(&self.interp.symbols, (*attrs)[1], self.interp.sym_lambda)) {
        return self.parse_ffi_fn(e, attrs);
    }
    self.set_error("unknown [ffi ...] form: expected 'lib', 'λ', or 'module'");
    return null;
}
