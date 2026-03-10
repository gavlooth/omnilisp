module lisp;

import std::collections::list;

fn Expr* Parser.parse_quote(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'quote' location
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
    Expr* e = self.alloc_expr_here();  // Capture 'and' location
    self.lexer.advance();  // consume 'and'

    Expr* left = self.parse_expr();
    Expr* right = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_AND;
    e.and_expr.left = left;
    e.and_expr.right = right;
    return e;
}

/**
 * Parse an 'or' expression (short-circuit boolean or).
 * (or left right) - returns left if truthy, otherwise right
 */
fn Expr* Parser.parse_or(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'or' location
    self.lexer.advance();  // consume 'or'

    Expr* left = self.parse_expr();
    Expr* right = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_OR;
    e.or_expr.left = left;
    e.or_expr.right = right;
    return e;
}

/**
 * Parse a begin expression (sequencing).
 * (begin e1 e2 ... en) - evaluates all, returns last
 */
fn Expr* Parser.parse_begin(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'begin' location
    self.lexer.advance();  // consume 'begin'

    List{Expr*} items;
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        items.push(self.parse_expr());
    }
    if (self.has_error) { items.free(); return null; }
    self.lexer.advance();  // consume ')'

    // (begin) with no expressions evaluates to nil
    if (items.len() == 0) {
        items.free();
        e.tag = E_LIT;
        e.lit.value = self.interp.alloc_value_root();
        e.lit.value.tag = NIL;
    } else {
        e.tag = E_BEGIN;
        e.begin = mem::malloc(ExprBegin.sizeof);
        e.begin.expr_count = items.len();
        e.begin.exprs = (Expr**)mem::malloc(Expr*.sizeof * items.len());
        for (usz i = 0; i < items.len(); i++) { e.begin.exprs[i] = items[i]; }
        items.free();
    }
    return e;
}
