module lisp;

import std::collections::list;

/**
 * Parse import: (import name), (import name (sym1 (sym2 'as alias))), (import name 'all), (import "path")
 */
fn bool Parser.parse_import_spec_tail(Parser* self, Expr* e) {
    // Check for optional specifiers before closing )
    // (import name 'all) or (import name (sym1 (sym2 'as alias)))
    if (self.lexer.current.type == T_QUOTE) {
        self.lexer.advance();  // consume '
        if (self.lexer.current.type != T_SYMBOL) {
            self.set_error("expected 'all or (specifiers...) after import module");
            return false;
        }
        SymbolId maybe_all = self.get_current_symbol();
        if ((uint)maybe_all == (uint)self.interp.sym_all) {
            // (import name 'all)
            e.import_expr.import_all = true;
            self.lexer.advance();
            return true;
        }
        self.set_error("expected 'all or (specifiers...) after import module");
        return false;
    }
    if (self.lexer.current.type == T_LPAREN) {
        return self.parse_import_selective_list(e);
    }
    if (self.lexer.current.type != T_RPAREN) {
        self.set_error("expected 'all or (specifiers...) after import module");
        return false;
    }
    return true;
}

fn Expr* Parser.parse_single_import_expr(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.init_import_expr(e);
    if (!self.parse_import_target(e)) return null;
    if (!self.parse_import_spec_tail(e)) return null;
    return e;
}

fn Expr* Parser.parse_grouped_import(Parser* self, Expr* wrapper) {
    List{Expr*} items;
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        self.lexer.advance();  // consume grouped spec '('
        Expr* item = self.parse_single_import_expr();
        if (item == null || self.has_error) { items.free(); return null; }
        self.expect(T_RPAREN, ")");  // close grouped spec
        if (self.has_error) { items.free(); return null; }
        items.push(item);
    }
    if (items.len() == 0) {
        items.free();
        self.set_error("expected grouped import spec after import");
        return null;
    }
    if (self.lexer.current.type != T_RPAREN) {
        items.free();
        self.set_error("expected grouped import spec or ) after import");
        return null;
    }
    self.lexer.advance();  // close outer import
    if (items.len() == 1) {
        Expr* only = items[0];
        items.free();
        return only;
    }
    wrapper.tag = E_BLOCK;
    wrapper.block = (ExprBlock*)self.alloc_ast_bytes(ExprBlock.sizeof, "out of memory while allocating grouped import block");
    if (wrapper.block == null) { items.free(); return null; }
    wrapper.block.expr_count = items.len();
    wrapper.block.exprs = (Expr**)self.alloc_ast_array_bytes(Expr*.sizeof, items.len(), "out of memory while allocating grouped import entries");
    if (wrapper.block.exprs == null && items.len() > 0) { items.free(); return null; }
    for (usz i = 0; i < items.len(); i++) wrapper.block.exprs[i] = items[i];
    items.free();
    return wrapper;
}

fn Expr* Parser.parse_import(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    self.lexer.advance();  // consume 'import'

    if (self.lexer.current.type == T_LPAREN) {
        return self.parse_grouped_import(e);
    }

    self.init_import_expr(e);
    if (!self.parse_import_target(e)) return null;

    if (!self.parse_import_spec_tail(e)) return null;

    self.expect(T_RPAREN, ")");  // close import
    return e;
}
