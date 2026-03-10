module lisp;

import std::core::mem;
import std::collections::list;

/**
 * Check for and parse postfix index access: expr.[index]
 * Returns the expression wrapped in E_INDEX if followed by .[, otherwise returns unchanged.
 *
 * OmniLisp dot-bracket syntax:
 *   arr.[0]           - array/list access by integer index
 *   dict.['key]       - dictionary access by quoted symbol key
 *   matrix.[i].[j]    - chained indexing
 */
fn Expr* Parser.parse_postfix_index(Parser* self, Expr* base) {
    if (self.has_error) return base;
    // Keep parsing postfix .[index] as long as we see .[
    while (self.lexer.current.type == T_DOT_BRACKET && !self.has_error) {
        self.lexer.advance();  // consume '.['

        // Parse the index expression
        Expr* idx = self.parse_expr();

        self.expect(T_RBRACKET, "]");

        // Wrap in E_INDEX
        Expr* indexed = self.alloc_expr_here();
        indexed.tag = E_INDEX;
        indexed.index.collection = base;
        indexed.index.index = idx;

        base = indexed;
    }
    return base;
}

/**
 * Parse a dict literal after the opening '{' has been consumed.
 * Desugars {k1 v1 k2 v2 ...} to (dict k1 v1 k2 v2 ...).
 */
fn Expr* Parser.parse_dict_literal(Parser* self) {
    Lexer* lex = self.lexer;
    Expr* e = self.alloc_expr_here();

    // Build E_CALL: func = E_VAR("dict"), args = collected expressions
    Expr* func = self.interp.alloc_expr();
    func.tag = E_VAR;
    func.var_expr.name = self.interp.symbols.intern("dict");

    List{Expr*} args;
    while (lex.current.type != T_RBRACE && !self.has_error) {
        Expr* arg = self.parse_expr();
        if (arg == null) { args.free(); return null; }
        args.push(arg);
    }

    e.tag = E_CALL;
    e.call = mem::malloc(ExprCall.sizeof);
    e.call.func = func;
    e.call.arg_count = args.len();
    e.call.args = (Expr**)mem::malloc(Expr*.sizeof * args.len());
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
 * Desugars [e1 e2 e3 ...] to (array e1 e2 e3 ...).
 */
fn Expr* Parser.parse_array_literal(Parser* self) {
    Lexer* lex = self.lexer;
    Expr* e = self.alloc_expr_here();

    // Build E_CALL: func = E_VAR("array"), args = collected expressions
    Expr* func = self.interp.alloc_expr();
    func.tag = E_VAR;
    func.var_expr.name = self.interp.symbols.intern("array");

    List{Expr*} args;
    while (lex.current.type != T_RBRACKET && !self.has_error) {
        Expr* arg = self.parse_expr();
        if (arg == null) { args.free(); return null; }
        args.push(arg);
    }

    e.tag = E_CALL;
    e.call = mem::malloc(ExprCall.sizeof);
    e.call.func = func;
    e.call.arg_count = args.len();
    e.call.args = (Expr**)mem::malloc(Expr*.sizeof * args.len());
    for (usz i = 0; i < args.len(); i++) { e.call.args[i] = args[i]; }
    args.free();

    self.expect(T_RBRACKET, "]");
    return self.parse_postfix_index(e);
}
