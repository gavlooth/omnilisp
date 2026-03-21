module lisp;

import std::collections::list;
import std::core::mem;

/**
 * Parse a dict literal after the opening '{' has been consumed.
 * Desugars {k1 v1 k2 v2 ...} to (Dictionary k1 v1 k2 v2 ...).
 */
fn Expr* Parser.parse_dict_literal(Parser* self) {
    Lexer* lex = self.lexer;
    Expr* e = self.alloc_expr_here();

    // Build E_CALL: func = E_VAR("Dictionary"), args = collected expressions
    Expr* func = self.interp.alloc_expr();
    func.tag = E_VAR;
    func.var_expr.name = self.interp.symbols.intern("Dictionary");

    List{Expr*} args;
    while (lex.current.type != T_RBRACE && !self.has_error) {
        Expr* arg = self.parse_expr();
        if (arg == null) { args.free(); return null; }
        args.push(arg);
    }

    e.tag = E_CALL;
    e.call = (ExprCall*)ast_arena_alloc(&self.interp.ast_arena, ExprCall.sizeof);
    e.call.func = func;
    e.call.arg_count = args.len();
    e.call.args = (Expr**)ast_arena_alloc(&self.interp.ast_arena, Expr*.sizeof * args.len());
    for (usz i = 0; i < args.len(); i++) { e.call.args[i] = args[i]; }
    args.free();

    if (e.call.arg_count % 2 != 0) {
        self.set_error("dict literal requires even number of elements");
        return null;
    }

    self.expect(T_RBRACE, "}");
    return self.parse_postfix_index(e);
}

/**
 * Parse an array literal after the opening '[' has been consumed.
 * Desugars [e1 e2 e3 ...] to (Array e1 e2 e3 ...).
 */
fn Expr* Parser.parse_array_literal(Parser* self) {
    Lexer* lex = self.lexer;
    Expr* e = self.alloc_expr_here();

    // Build E_CALL: func = E_VAR("Array"), args = collected expressions
    Expr* func = self.interp.alloc_expr();
    func.tag = E_VAR;
    func.var_expr.name = self.interp.symbols.intern("Array");

    List{Expr*} args;
    while (lex.current.type != T_RBRACKET && !self.has_error) {
        Expr* arg = self.parse_expr();
        if (arg == null) { args.free(); return null; }
        args.push(arg);
    }

    e.tag = E_CALL;
    e.call = (ExprCall*)ast_arena_alloc(&self.interp.ast_arena, ExprCall.sizeof);
    e.call.func = func;
    e.call.arg_count = args.len();
    e.call.args = (Expr**)ast_arena_alloc(&self.interp.ast_arena, Expr*.sizeof * args.len());
    for (usz i = 0; i < args.len(); i++) { e.call.args[i] = args[i]; }
    args.free();

    self.expect(T_RBRACKET, "]");
    return self.parse_postfix_index(e);
}
