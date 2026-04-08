module lisp;

import std::collections::list;

fn Expr* Parser.parse_define(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'define' location
    if (e == null) return null;
    self.lexer.advance();  // consume 'define'

    // Check for bracket annotation: (define [attr...] ...)
    if (self.lexer.current.type == T_LBRACKET) {
        self.lexer.advance();  // consume '['
        List{SymbolId} attrs;
        defer attrs.free();
        if (!self.parse_define_attrs(&attrs)) return null;
        return self.parse_define_with_annotation(e, &attrs);
    }

    // Shorthand define: (define (name params...) body) => (define name (lambda (params...) body))
    // Also supports typed params: (define (name (^Integer x) (^String y)) body)
    if (self.lexer.current.type == T_LPAREN) {
        self.lexer.advance();  // consume '('
        return self.parse_shorthand_define(e);
    }

    return self.parse_normal_define(e);
}
