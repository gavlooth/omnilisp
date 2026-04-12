module lisp;

import std::collections::list;
import main;

// Parse one or more body expressions before a closing ')'.
// If multiple expressions, wraps them in an implicit E_BLOCK (block).
// Does NOT consume the closing ')'.
fn Expr* Parser.parse_implicit_block(Parser* self) {
    if (self.has_error) return null;
    Expr* first = self.parse_expr();
    if (self.has_error) return null;
    if (self.lexer.current.type == T_RPAREN) {
        // Single body expression - return as-is
        return first;
    }
    // Multiple body expressions - wrap in block
    List{Expr*} items;
    items.push(first);
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        items.push(self.parse_expr());
    }
    Expr* block_e = self.alloc_expr();
    if (block_e == null) { items.free(); return null; }
    block_e.tag = E_BLOCK;
    block_e.block = (ExprBlock*)self.alloc_ast_bytes(ExprBlock.sizeof, "out of memory while allocating implicit block");
    if (block_e.block == null) { items.free(); return null; }
    block_e.block.expr_count = items.len();
    if (items.len() > 0) {
        usz expr_bytes;
        if (!self.alloc_ast_bytes_for_count(
            Expr*.sizeof,
            items.len(),
            "out of memory while allocating implicit block expressions",
            &expr_bytes
        )) {
            items.free();
            return null;
        }
        block_e.block.exprs = (Expr**)self.alloc_ast_bytes(expr_bytes, "out of memory while allocating implicit block expressions");
        if (block_e.block.exprs == null) {
            items.free();
            return null;
        }
    } else {
        block_e.block.exprs = null;
    }
    for (usz i = 0; i < items.len(); i++) { block_e.block.exprs[i] = items[i]; }
    items.free();
    return block_e;
}

fn Expr* Parser.wrap_body_with_destructuring(Parser* self, Expr* lambda_expr, Expr* body, List{Pattern*} patterns, List{SymbolId} names) {
    for (usz di = patterns.len(); di > 0; di--) {
        Expr* let_wrap = self.alloc_expr();
        if (let_wrap == null) return null;
        let_wrap.tag = E_LET;
        let_wrap.loc_line = lambda_expr.loc_line;
        let_wrap.loc_column = lambda_expr.loc_column;
        let_wrap.let_expr.name = (SymbolId)0;
        let_wrap.let_expr.pattern = patterns[di - 1];

        Expr* var_ref = self.alloc_expr();
        if (var_ref == null) return null;
        var_ref.tag = E_VAR;
        var_ref.var_expr.name = names[di - 1];

        let_wrap.let_expr.init = var_ref;
        let_wrap.let_expr.body = body;
        let_wrap.let_expr.is_recursive = false;
        body = let_wrap;
    }
    return body;
}

fn void Parser.finalize_lambda_expr(Parser* self, Expr* e, List{SymbolId} params, List{TypeAnnotation} param_anns, bool has_any_typed, bool has_rest, SymbolId rest_param, Expr* body) {
    usz param_count = params.len();
    e.tag = E_LAMBDA;
    e.lambda = (ExprLambda*)self.alloc_ast_bytes(ExprLambda.sizeof, "out of memory while allocating lambda");
    if (e.lambda == null) return;
    e.lambda.param = param_count > 0 ? params[0] : INVALID_SYMBOL_ID;
    e.lambda.param_count = param_count;
    e.lambda.params = null;
    if (param_count > 0) {
        usz lambda_params_bytes;
        if (!self.alloc_ast_bytes_for_count(
            SymbolId.sizeof,
            param_count,
            "out of memory while allocating lambda parameters",
            &lambda_params_bytes
        )) return;

        e.lambda.params = (SymbolId*)self.alloc_ast_bytes(lambda_params_bytes, "out of memory while allocating lambda parameters");
        if (e.lambda.params == null) return;
        for (usz i = 0; i < param_count; i++) {
            e.lambda.params[i] = params[i];
        }
    }
    e.lambda.has_rest = has_rest;
    e.lambda.rest_param = rest_param;
    e.lambda.has_typed_params = has_any_typed;
    e.lambda.param_annotations = null;
    if (has_any_typed && param_count > 0) {
        usz ann_bytes;
        if (!self.alloc_ast_bytes_for_count(
            TypeAnnotation.sizeof,
            param_count,
            "out of memory while allocating lambda parameter annotations",
            &ann_bytes
        )) return;

        e.lambda.param_annotations = (TypeAnnotation*)self.alloc_ast_bytes(ann_bytes, "out of memory while allocating lambda parameter annotations");
        if (e.lambda.param_annotations == null) return;
        for (usz i = 0; i < param_count; i++) {
            e.lambda.param_annotations[i] = param_anns[i];
        }
    }
    e.lambda.body = body;
}

fn bool Parser.parse_rest_param(Parser* self, SymbolId* rest_param) {
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected rest parameter name after ..");
        return false;
    }
    *rest_param = self.get_current_symbol();
    self.lexer.advance();
    return true;
}

fn Expr* Parser.finalize_lambda_no_fixed_params(Parser* self, Expr* e, bool has_rest, SymbolId rest_param) {
    Expr* body = self.parse_implicit_block();
    self.expect(T_RPAREN, ")");
    if (self.has_error) return null;
    List{SymbolId} no_params;
    List{TypeAnnotation} no_param_anns;
    self.finalize_lambda_expr(e, no_params, no_param_anns, false, has_rest, rest_param, body);
    return e;
}

fn Expr* Parser.finalize_lambda_with_params(Parser* self, Expr* e, List{SymbolId} params, List{TypeAnnotation} param_anns, bool has_any_typed, bool has_rest, SymbolId rest_param, List{Pattern*} destr_patterns, List{SymbolId} destr_param_names, bool require_fixed_param) {
    Expr* body = self.parse_implicit_block();
    self.expect(T_RPAREN, ")");
    if (self.has_error) return null;
    if (require_fixed_param && params.len() < 1) {
        self.set_error("expected at least one parameter");
        return null;
    }
    body = self.wrap_body_with_destructuring(e, body, destr_patterns, destr_param_names);
    if (body == null || self.has_error) return null;
    self.finalize_lambda_expr(e, params, param_anns, has_any_typed, has_rest, rest_param, body);
    if (self.has_error || e.lambda == null) return null;
    return e;
}
