module lisp;

import std::collections::list;

/**
 * Parse set! forms:
 * - (set! name value) / (set! obj.field value): mutation special form
 * - (set! collection key value ...): desugared to regular call
 */
fn Expr* Parser.build_set_call_symbol(Parser* self, usz line, usz col) {
    Expr* fn_expr = self.alloc_expr_here();
    if (fn_expr == null) return null;
    fn_expr.tag = E_VAR;
    fn_expr.loc_line = line;
    fn_expr.loc_column = col;
    fn_expr.var_expr.name = self.interp.sym_set;
    return fn_expr;
}

fn Expr* Parser.parse_generic_set_call(Parser* self, Expr* lhs, Expr* rhs, usz line, usz col) {
    List{Expr*} args;
    args.push(lhs);
    args.push(rhs);
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        args.push(self.parse_expr());
    }
    if (self.has_error) {
        args.free();
        return null;
    }
    self.expect(T_RPAREN, ")");

    Expr* set_fn = self.build_set_call_symbol(line, col);
    if (set_fn == null) { args.free(); return null; }
    Expr* call_expr = self.build_call_expr(set_fn, &args, line, col);
    args.free();
    if (call_expr == null || self.has_error) return null;
    return self.parse_postfix_index(call_expr);
}

fn Expr* Parser.parse_set(Parser* self) {
    if (self.has_error) return null;
    usz set_line = self.lexer.current.line;
    usz set_col = self.lexer.current.column;
    self.lexer.advance();  // consume 'set!'

    Expr* lhs = self.parse_expr();
    if (lhs == null || self.has_error) return null;
    Expr* rhs = self.parse_expr();
    if (rhs == null || self.has_error) return null;

    // Generic call surface: (set! collection key value ...)
    if (self.lexer.current.type != T_RPAREN) {
        return self.parse_generic_set_call(lhs, rhs, set_line, set_col);
    }

    // Mutation special form surface: (set! name value) or (set! obj.field value)
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    e.loc_line = set_line;
    e.loc_column = set_col;
    e.tag = E_SET;
    e.set_expr.value = rhs;
    e.set_expr.path_segments = null;
    e.set_expr.path_segment_count = 0;
    e.set_expr.is_path = false;

    if (lhs.tag == E_VAR) {
        e.set_expr.name = lhs.var_expr.name;
    } else if (lhs.tag == E_PATH) {
        if (lhs.path.segment_count == 0 || lhs.path.segments == null) {
            self.set_error("empty path in set!");
            return null;
        }
        e.set_expr.is_path = true;
        e.set_expr.path_segment_count = lhs.path.segment_count;
        e.set_expr.path_segments = lhs.path.segments;
        e.set_expr.name = lhs.path.segments[0];
    } else {
        self.set_error("expected variable name or path after set!");
        return null;
    }

    self.expect(T_RPAREN, ")");
    return e;
}
