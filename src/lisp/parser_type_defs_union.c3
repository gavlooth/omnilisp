module lisp;

import std::collections::list;

/**
 * Parse union type: (define [union] (Name T...) Variant1 (Variant2 fields...) ...)
 * or simple: (define [union] Name Variant1 (Variant2 T) ...)
 */
fn void Parser.init_defunion_expr(Parser* self, Expr* e) {
    e.tag = E_DEFUNION;
    e.defunion = (ExprDefUnion*)ast_arena_alloc(&self.interp.ast_arena, ExprDefUnion.sizeof);
    e.defunion.type_params = null;
    e.defunion.type_param_count = 0;
    e.defunion.variants = null;
    e.defunion.variant_count = 0;
}

fn bool Parser.parse_defunion_name_compound(Parser* self, Expr* e) {
    List{SymbolId} params;
    defer params.free();
    if (!self.parse_compound_symbol_with_params(&e.defunion.name, &params, "expected union name")) {
        return false;
    }
    if (!self.validate_invariant_type_params(&params)) return false;
    if (!self.copy_symbol_list(&e.defunion.type_params, &params, "out of memory while parsing union type parameters")) {
        return false;
    }
    e.defunion.type_param_count = params.len();
    return true;
}

fn bool Parser.parse_defunion_name(Parser* self, Expr* e) {
    if (self.lexer.current.type == T_LPAREN) {
        return self.parse_defunion_name_compound(e);
    }
    if (self.lexer.current.type == T_SYMBOL) {
        e.defunion.name = self.get_current_symbol();
        self.lexer.advance();
        return true;
    }
    self.set_error("expected union name after [union]");
    return false;
}

fn bool Parser.parse_defunion_variant_compound(Parser* self, UnionVariant* v) {
    List{SymbolId} fields;
    defer fields.free();
    bool ok = self.parse_compound_symbol_with_params(
        &v.name,
        &fields,
        "expected variant name"
    );
    if (!ok) return false;
    if (!self.copy_symbol_list(&v.fields, &fields, "out of memory while parsing union variant fields")) return false;
    v.field_count = fields.len();
    return true;
}

fn bool Parser.parse_defunion_variant(Parser* self, List{UnionVariant}* variants) {
    UnionVariant v = {};
    if (self.lexer.current.type == T_LPAREN) {
        if (!self.parse_defunion_variant_compound(&v)) return false;
    } else {
        v.name = self.get_current_symbol();
        v.fields = null;
        v.field_count = 0;
        self.lexer.advance();
    }
    variants.push(v);
    return true;
}

fn Expr* Parser.parse_defunion(Parser* self, Expr* e) {
    self.init_defunion_expr(e);
    if (!self.parse_defunion_name(e)) return null;

    List{UnionVariant} variants;
    defer variants.free();
    while ((self.lexer.current.type == T_SYMBOL || self.lexer.current.type == T_LPAREN) && !self.has_error) {
        if (!self.parse_defunion_variant(&variants)) return null;
    }

    self.expect(T_RPAREN, ")");  // close define
    if (self.has_error) return null;
    if (!self.copy_union_variant_list(&e.defunion.variants, &variants)) return null;
    e.defunion.variant_count = variants.len();
    return e;
}
