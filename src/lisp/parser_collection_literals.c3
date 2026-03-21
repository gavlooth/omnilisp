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
    // Keep parsing postfix index access as long as we see .[index]
    while (!self.has_error) {
        bool is_dot_bracket = self.lexer.current.type == T_DOT_BRACKET;
        bool is_dot_symbol = self.lexer.current.type == T_SYMBOL &&
            self.lexer.current.text_len == 1 && self.lexer.current.text[0] == '.';

        if (!is_dot_bracket && !is_dot_symbol) {
            break;
        }

        if (is_dot_bracket) {
            self.lexer.advance(); // consume '.['
        } else {
            self.lexer.advance(); // consume '.'
            if (self.lexer.current.type != T_LBRACKET) {
                self.set_error("expected '[' after '.' in index access");
                return base;
            }
            self.lexer.advance(); // consume '['
        }

        // Parse the index expression
        Expr* idx = self.parse_expr();
        if (self.has_error || idx == null) return base;

        self.expect(T_RBRACKET, "]");
        if (self.has_error) return base;

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
 * Build a single-argument lookup accessor lambda:
 *   (lambda (x) (ref x key_expr))
 */
fn Expr* Parser.build_lookup_accessor(Parser* self, Expr* key_expr, usz line, usz col) {
    SymbolId param = self.make_placeholder_param_sym();

    Expr* ref_fn = self.interp.alloc_expr();
    ref_fn.tag = E_VAR;
    ref_fn.var_expr.name = self.interp.symbols.intern("ref");
    ref_fn.loc_line = line;
    ref_fn.loc_column = col;

    Expr* coll_var = self.interp.alloc_expr();
    coll_var.tag = E_VAR;
    coll_var.var_expr.name = param;
    coll_var.loc_line = line;
    coll_var.loc_column = col;

    Expr* call = self.interp.alloc_expr();
    call.tag = E_CALL;
    call.call = (ExprCall*)ast_arena_alloc(&self.interp.ast_arena, ExprCall.sizeof);
    call.loc_line = line;
    call.loc_column = col;
    call.call.func = ref_fn;
    call.call.arg_count = 2;
    call.call.args = (Expr**)ast_arena_alloc(&self.interp.ast_arena, Expr*.sizeof * 2);
    call.call.args[0] = coll_var;
    call.call.args[1] = key_expr;

    Expr* lam = self.interp.alloc_expr();
    lam.tag = E_LAMBDA;
    lam.lambda = (ExprLambda*)ast_arena_alloc(&self.interp.ast_arena, ExprLambda.sizeof);
    lam.loc_line = line;
    lam.loc_column = col;
    lam.lambda.param = param;
    lam.lambda.param_count = 1;
    lam.lambda.params = (SymbolId*)ast_arena_alloc(&self.interp.ast_arena, SymbolId.sizeof);
    lam.lambda.params[0] = param;
    lam.lambda.has_rest = false;
    lam.lambda.rest_param = 0;
    lam.lambda.has_typed_params = false;
    lam.lambda.param_annotations = null;
    lam.lambda.body = call;
    lam.lambda.is_accessor_shorthand = true;
    return lam;
}
