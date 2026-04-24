module lisp;

import std::collections::list;
import std::core::mem;

fn Expr* Parser.parse_bare_dict_symbol_key(Parser* self) {
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    e.tag = E_QUOTE;
    e.quote.datum = self.interp.alloc_value_root();
    e.quote.datum.tag = SYMBOL;
    e.quote.datum.sym_val = self.get_current_symbol();
    self.lexer.advance();
    return e;
}

/**
 * Parse a dict literal after the opening '{' has been consumed.
 * Desugars {k1 v1 k2 v2 ...} to (Dictionary k1 v1 k2 v2 ...).
 */
fn Expr* Parser.parse_dict_literal(Parser* self) {
    Lexer* lex = self.lexer;
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;

    // Build E_CALL: func = E_VAR("Dictionary"), args = collected expressions
    Expr* func = self.alloc_expr();
    if (func == null) return null;
    func.tag = E_VAR;
    func.var_expr.name = self.intern_or_error("Dictionary", "failed to intern Dictionary constructor");
    if (func.var_expr.name == INVALID_SYMBOL_ID) return null;

    List{Expr*} args;
    while (lex.current.type != T_RBRACE && !self.has_error) {
        bool is_key_position = args.len() % 2 == 0;
        Expr* arg = is_key_position && lex.current.type == T_SYMBOL
            ? self.parse_bare_dict_symbol_key()
            : self.parse_expr();
        if (arg == null) { args.free(); return null; }
        args.push(arg);
    }

    e.tag = E_CALL;
    e.call = (ExprCall*)self.alloc_ast_bytes(ExprCall.sizeof, "out of memory while allocating dictionary literal");
    if (e.call == null) { args.free(); return null; }
    e.call.func = func;
    e.call.arg_count = args.len();
    e.call.args = null;
    if (args.len() > 0) {
        e.call.args = (Expr**)self.alloc_ast_array_bytes(Expr*.sizeof, args.len(), "out of memory while allocating dictionary literal arguments");
        if (e.call.args == null) { args.free(); return null; }
    }
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
    if (e == null) return null;

    // Build E_CALL: func = E_VAR("Array"), args = collected expressions
    Expr* func = self.alloc_expr();
    if (func == null) return null;
    func.tag = E_VAR;
    func.var_expr.name = self.intern_or_error("Array", "failed to intern Array constructor");
    if (func.var_expr.name == INVALID_SYMBOL_ID) return null;

    List{Expr*} args;
    while (lex.current.type != T_RBRACKET && !self.has_error) {
        Expr* arg = self.parse_expr();
        if (arg == null) { args.free(); return null; }
        args.push(arg);
    }

    e.tag = E_CALL;
    e.call = (ExprCall*)self.alloc_ast_bytes(ExprCall.sizeof, "out of memory while allocating array literal");
    if (e.call == null) { args.free(); return null; }
    e.call.func = func;
    e.call.arg_count = args.len();
    e.call.args = null;
    if (args.len() > 0) {
        e.call.args = (Expr**)self.alloc_ast_array_bytes(Expr*.sizeof, args.len(), "out of memory while allocating array literal arguments");
        if (e.call.args == null) { args.free(); return null; }
    }
    for (usz i = 0; i < args.len(); i++) { e.call.args[i] = args[i]; }
    args.free();

    self.expect(T_RBRACKET, "]");
    return self.parse_postfix_index(e);
}
