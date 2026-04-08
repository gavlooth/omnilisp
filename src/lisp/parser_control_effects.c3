module lisp;

import std::collections::list;

fn Expr* parser_make_bool_literal_expr(Parser* self, bool truthy, usz line, usz column) {
    Expr* e = self.alloc_expr();
    if (e == null) return null;
    e.loc_line = line;
    e.loc_column = column;
    e.tag = E_LIT;
    e.lit.value = truthy ? make_symbol(self.interp, self.interp.sym_true) : make_nil(self.interp);
    return e;
}

fn Expr* parser_make_logical_chain_expr(
    Parser* self,
    List{Expr*}* items,
    bool is_and,
    usz line,
    usz column
) {
    if (items == null || items.len() == 0) {
        return parser_make_bool_literal_expr(self, is_and, line, column);
    }
    if (items.len() == 1) return (*items)[0];

    Expr* nested = (*items)[items.len() - 1];
    for (isz i = (isz)items.len() - 2; i >= 0; i--) {
        Expr* e = self.alloc_expr();
        if (e == null) return null;
        e.loc_line = line;
        e.loc_column = column;
        e.tag = is_and ? E_AND : E_OR;
        if (is_and) {
            e.and_expr.left = (*items)[(usz)i];
            e.and_expr.right = nested;
        } else {
            e.or_expr.left = (*items)[(usz)i];
            e.or_expr.right = nested;
        }
        nested = e;
    }
    return nested;
}

fn Expr* Parser.parse_quote(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'quote' location
    if (e == null) return null;
    self.lexer.advance();  // consume 'quote'

    Value* datum = self.parse_datum();

    self.expect(T_RPAREN, ")");

    e.tag = E_QUOTE;
    e.quote.datum = datum;
    return e;
}

/**
 * Parse an 'and' expression (short-circuit boolean and).
 * (and left right) - returns left if falsy, otherwise right
 */
fn Expr* Parser.parse_and(Parser* self) {
    if (self.has_error) return null;
    usz line = self.lexer.current.line;
    usz column = self.lexer.current.column;
    self.lexer.advance();  // consume 'and'

    List{Expr*} items;
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        items.push(self.parse_expr());
    }
    if (self.has_error) {
        items.free();
        return null;
    }

    self.expect(T_RPAREN, ")");
    Expr* e = parser_make_logical_chain_expr(self, &items, true, line, column);
    items.free();
    return e;
}

/**
 * Parse an 'or' expression (short-circuit boolean or).
 * (or left right) - returns left if truthy, otherwise right
 */
fn Expr* Parser.parse_or(Parser* self) {
    if (self.has_error) return null;
    usz line = self.lexer.current.line;
    usz column = self.lexer.current.column;
    self.lexer.advance();  // consume 'or'

    List{Expr*} items;
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        items.push(self.parse_expr());
    }
    if (self.has_error) {
        items.free();
        return null;
    }

    self.expect(T_RPAREN, ")");
    Expr* e = parser_make_logical_chain_expr(self, &items, false, line, column);
    items.free();
    return e;
}

/**
 * Parse a block expression (sequencing).
 * (block e1 e2 ... en) - evaluates all, returns last
 */
fn Expr* Parser.parse_block(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'block' location
    if (e == null) return null;
    self.lexer.advance();  // consume 'block'

    List{Expr*} items;
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        items.push(self.parse_expr());
    }
    if (self.has_error) { items.free(); return null; }
    self.lexer.advance();  // consume ')'

    // (block) with no expressions evaluates to nil
    if (items.len() == 0) {
        items.free();
        e.tag = E_LIT;
        e.lit.value = self.interp.alloc_value_root();
        e.lit.value.tag = NIL;
    } else {
        e.tag = E_BLOCK;
        e.block = (ExprBlock*)self.alloc_ast_bytes(ExprBlock.sizeof, "out of memory while allocating block expression");
        if (e.block == null) { items.free(); return null; }
        e.block.expr_count = items.len();
        e.block.exprs = (Expr**)self.alloc_ast_bytes(Expr*.sizeof * items.len(), "out of memory while allocating block expressions");
        if (e.block.exprs == null && items.len() > 0) { items.free(); return null; }
        for (usz i = 0; i < items.len(); i++) { e.block.exprs[i] = items[i]; }
        items.free();
    }
    return e;
}
