module lisp;

import std::io;
import std::collections::list;

// Parse one or more body expressions before a closing ')'.
// If multiple expressions, wraps them in an implicit E_BEGIN.
// Does NOT consume the closing ')'.
fn Expr* Parser.parse_implicit_begin(Parser* self) {
    if (self.has_error) return null;
    Expr* first = self.parse_expr();
    if (self.has_error) return null;
    if (self.lexer.current.type == T_RPAREN) {
        // Single body expression — return as-is
        return first;
    }
    // Multiple body expressions — wrap in begin
    List{Expr*} items;
    items.push(first);
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        items.push(self.parse_expr());
    }
    Expr* begin_e = self.interp.alloc_expr();
    begin_e.tag = E_BEGIN;
    begin_e.begin = (ExprBegin*)mem::malloc(ExprBegin.sizeof);
    begin_e.begin.expr_count = items.len();
    begin_e.begin.exprs = (Expr**)mem::malloc(Expr*.sizeof * items.len());
    for (usz i = 0; i < items.len(); i++) { begin_e.begin.exprs[i] = items[i]; }
    items.free();
    return begin_e;
}

fn Expr* Parser.wrap_body_with_destructuring(Parser* self, Expr* lambda_expr, Expr* body, List{Pattern*} patterns, List{SymbolId} names) {
    for (usz di = patterns.len(); di > 0; di--) {
        Expr* let_wrap = self.interp.alloc_expr();
        let_wrap.tag = E_LET;
        let_wrap.loc_line = lambda_expr.loc_line;
        let_wrap.loc_column = lambda_expr.loc_column;
        let_wrap.let_expr.name = (SymbolId)0;
        let_wrap.let_expr.pattern = patterns[di - 1];

        Expr* var_ref = self.interp.alloc_expr();
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
    e.lambda = mem::malloc(ExprLambda.sizeof);
    e.lambda.param = param_count > 0 ? params[0] : INVALID_SYMBOL_ID;
    e.lambda.param_count = param_count;
    e.lambda.params = null;
    if (param_count > 0) {
        e.lambda.params = (SymbolId*)mem::malloc(SymbolId.sizeof * param_count);
        for (usz i = 0; i < param_count; i++) {
            e.lambda.params[i] = params[i];
        }
    }
    e.lambda.has_rest = has_rest;
    e.lambda.rest_param = rest_param;
    e.lambda.has_typed_params = has_any_typed;
    e.lambda.param_annotations = null;
    if (has_any_typed && param_count > 0) {
        e.lambda.param_annotations = (TypeAnnotation*)mem::malloc(TypeAnnotation.sizeof * param_count);
        for (usz i = 0; i < param_count; i++) {
            e.lambda.param_annotations[i] = param_anns[i];
        }
    }
    e.lambda.body = body;
}

fn bool Parser.collect_callable_params(Parser* self, List{SymbolId}* params, List{TypeAnnotation}* param_anns, bool* has_any_typed, List{Pattern*}* destr_patterns, List{SymbolId}* destr_param_names) {
    while (self.lexer.current.type == T_SYMBOL || self.lexer.current.type == T_LPAREN || self.lexer.current.type == T_LBRACE || self.lexer.current.type == T_LBRACKET) {
        if (self.lexer.current.type == T_LBRACE || self.lexer.current.type == T_LBRACKET) {
            Pattern* dp = self.parse_pattern();
            char[8] gensym_buf;
            usz glen = io::bprintf(&gensym_buf, "__d%d", (int)(*destr_patterns).len())!!.len;
            SymbolId gensym = self.interp.symbols.intern(gensym_buf[:glen]);
            params.push(gensym);
            TypeAnnotation empty_ann;
            empty_ann.has_annotation = false;
            param_anns.push(empty_ann);
            destr_patterns.push(dp);
            destr_param_names.push(gensym);
        } else if (self.lexer.current.type == T_LPAREN) {
            self.lexer.advance();  // consume '('
            TypeAnnotation ann = self.parse_type_annotation();
            if (!ann.has_annotation) {
                self.set_error("expected type annotation in typed parameter");
                return false;
            }
            if (self.lexer.current.type != T_SYMBOL) {
                self.set_error("expected parameter name after type annotation");
                return false;
            }
            params.push(self.get_current_symbol());
            param_anns.push(ann);
            *has_any_typed = true;
            self.lexer.advance();
            self.expect(T_RPAREN, ")");
            if (self.has_error) return false;
        } else {
            params.push(self.get_current_symbol());
            TypeAnnotation empty_ann;
            empty_ann.has_annotation = false;
            param_anns.push(empty_ann);
            self.lexer.advance();
        }

        if (self.lexer.current.type == T_DOTDOT || self.lexer.current.type == T_RPAREN) {
            break;
        }
    }
    return !self.has_error;
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
    Expr* body = self.parse_implicit_begin();
    self.expect(T_RPAREN, ")");
    if (self.has_error) return null;
    List{SymbolId} no_params;
    List{TypeAnnotation} no_param_anns;
    self.finalize_lambda_expr(e, no_params, no_param_anns, false, has_rest, rest_param, body);
    return e;
}

fn Expr* Parser.finalize_lambda_with_params(Parser* self, Expr* e, List{SymbolId} params, List{TypeAnnotation} param_anns, bool has_any_typed, bool has_rest, SymbolId rest_param, List{Pattern*} destr_patterns, List{SymbolId} destr_param_names, bool require_fixed_param) {
    Expr* body = self.parse_implicit_begin();
    self.expect(T_RPAREN, ")");
    if (self.has_error) return null;
    if (require_fixed_param && params.len() < 1) {
        self.set_error("expected at least one parameter");
        return null;
    }
    body = self.wrap_body_with_destructuring(e, body, destr_patterns, destr_param_names);
    self.finalize_lambda_expr(e, params, param_anns, has_any_typed, has_rest, rest_param, body);
    return e;
}

fn Expr* Parser.parse_lambda(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'lambda' location
    self.lexer.advance();  // consume 'lambda'

    self.expect(T_LPAREN, "(");

    if (self.lexer.current.type == T_RPAREN) {
        self.lexer.advance();  // consume ')'
        return self.finalize_lambda_no_fixed_params(e, false, 0);
    }

    if (self.lexer.current.type == T_DOTDOT) {
        self.lexer.advance();  // consume '..'
        SymbolId rest_param;
        if (!self.parse_rest_param(&rest_param)) return null;
        self.expect(T_RPAREN, ")");
        return self.finalize_lambda_no_fixed_params(e, true, rest_param);
    }

    List{SymbolId} params;
    defer params.free();
    List{TypeAnnotation} param_anns;
    defer param_anns.free();
    bool has_any_typed = false;
    List{Pattern*} lam_destr_patterns;
    defer lam_destr_patterns.free();
    List{SymbolId} lam_destr_param_names;
    defer lam_destr_param_names.free();

    if (!self.collect_callable_params(&params, &param_anns, &has_any_typed, &lam_destr_patterns, &lam_destr_param_names)) {
        return null;
    }

    if (self.lexer.current.type == T_DOTDOT) {
        self.lexer.advance();  // consume '..'
        SymbolId rest_param;
        if (!self.parse_rest_param(&rest_param)) return null;
        self.expect(T_RPAREN, ")");
        return self.finalize_lambda_with_params(e, params, param_anns, has_any_typed, true, rest_param, lam_destr_patterns, lam_destr_param_names, false);
    }

    self.expect(T_RPAREN, ")");
    return self.finalize_lambda_with_params(e, params, param_anns, has_any_typed, false, 0, lam_destr_patterns, lam_destr_param_names, true);
}

fn Expr* Parser.parse_if(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'if' location
    self.lexer.advance();  // consume 'if'

    Expr* test = self.parse_expr();
    Expr* then_branch = self.parse_expr();
    Expr* else_branch = self.parse_expr();

    self.expect(T_RPAREN, ")");

    e.tag = E_IF;
    e.if_expr.test = test;
    e.if_expr.then_branch = then_branch;
    e.if_expr.else_branch = else_branch;
    return e;
}
