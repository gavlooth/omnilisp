module lisp;

import std::collections::list;

fn Expr* Parser.parse_shorthand_define(Parser* self, Expr* e) {
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected function name in shorthand define");
        return null;
    }
    SymbolId name = self.get_current_symbol();
    self.lexer.advance();

    List{SymbolId} params;
    List{TypeAnnotation} define_param_anns;
    bool define_has_typed = false;
    bool define_has_rest = false;
    SymbolId define_rest_param = (SymbolId)0;
    List{Pattern*} destr_patterns;
    List{SymbolId} destr_param_names;

    if (!self.collect_callable_params(&params, &define_param_anns, &define_has_typed, &destr_patterns, &destr_param_names)) {
        params.free();
        define_param_anns.free();
        destr_patterns.free();
        destr_param_names.free();
        return null;
    }

    if (!self.parse_shorthand_rest_param(&define_has_rest, &define_rest_param)) {
        params.free();
        define_param_anns.free();
        destr_patterns.free();
        destr_param_names.free();
        return null;
    }

    Expr* body = self.parse_shorthand_define_body(e, destr_patterns, destr_param_names);
    if (body == null) {
        params.free();
        define_param_anns.free();
        destr_patterns.free();
        destr_param_names.free();
        return null;
    }

    Expr* lambda_expr = self.build_shorthand_define_lambda(
        e,
        params,
        define_param_anns,
        define_has_typed,
        define_has_rest,
        define_rest_param,
        body
    );
    params.free();
    define_param_anns.free();
    destr_patterns.free();
    destr_param_names.free();

    e.tag = E_DEFINE;
    e.define.name = name;
    e.define.value = lambda_expr;
    return e;
}

fn Expr* Parser.parse_normal_define(Parser* self, Expr* e) {
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected name after define");
        return null;
    }

    SymbolId name = self.get_current_symbol();
    self.lexer.advance();
    Expr* value = self.parse_expr();
    self.expect(T_RPAREN, ")");

    e.tag = E_DEFINE;
    e.define.name = name;
    e.define.value = value;
    return e;
}
