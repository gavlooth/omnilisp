module lisp;

import std::core::mem;
import std::collections::list;

fn bool Parser.copy_symbol_list(Parser* self, SymbolId** out, List{SymbolId}* items, char[] oom_error) {
    if (items.len() == 0) {
        *out = null;
        return true;
    }
    SymbolId* buf = (SymbolId*)mem::malloc(SymbolId.sizeof * items.len());
    if (buf == null) {
        self.set_error(oom_error);
        return false;
    }
    for (usz i = 0; i < items.len(); i++) buf[i] = (*items)[i];
    *out = buf;
    return true;
}

fn bool Parser.copy_type_field_list(Parser* self, TypeField** out, List{TypeField}* items) {
    if (items.len() == 0) {
        *out = null;
        return true;
    }
    TypeField* buf = (TypeField*)mem::malloc(TypeField.sizeof * items.len());
    if (buf == null) {
        self.set_error("out of memory while parsing type fields");
        return false;
    }
    for (usz i = 0; i < items.len(); i++) buf[i] = (*items)[i];
    *out = buf;
    return true;
}

fn bool Parser.copy_union_variant_list(Parser* self, UnionVariant** out, List{UnionVariant}* items) {
    if (items.len() == 0) {
        *out = null;
        return true;
    }
    UnionVariant* buf = (UnionVariant*)mem::malloc(UnionVariant.sizeof * items.len());
    if (buf == null) {
        self.set_error("out of memory while parsing union variants");
        return false;
    }
    for (usz i = 0; i < items.len(); i++) buf[i] = (*items)[i];
    *out = buf;
    return true;
}

fn bool Parser.parse_compound_symbol_with_params(
    Parser* self,
    SymbolId* name_out,
    List{SymbolId}* params_out,
    char[] missing_name_error
) {
    self.lexer.advance();  // consume '('
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error(missing_name_error);
        return false;
    }
    *name_out = self.get_current_symbol();
    self.lexer.advance();

    while (self.lexer.current.type == T_SYMBOL) {
        params_out.push(self.get_current_symbol());
        self.lexer.advance();
    }

    self.expect(T_RPAREN, ")");
    return !self.has_error;
}

/**
 * Parse type definition: (define [type] Name (^Type field) ...)
 * or with parent: (define [type] (Name Parent) (^Type field) ...)
 */
fn void Parser.init_deftype_expr(Parser* self, Expr* e) {
    e.tag = E_DEFTYPE;
    e.deftype = mem::malloc(ExprDefType.sizeof);
    e.deftype.has_parent = false;
    e.deftype.fields = null;
    e.deftype.field_count = 0;
    e.deftype.type_params = null;
    e.deftype.type_param_count = 0;
    e.deftype.parent = 0;
}

fn bool Parser.parse_deftype_name_compound(Parser* self, Expr* e) {
    List{SymbolId} params;
    defer params.free();
    if (!self.parse_compound_symbol_with_params(&e.deftype.name, &params, "expected type name")) {
        return false;
    }
    if (!self.copy_symbol_list(&e.deftype.type_params, &params, "out of memory while parsing type parameters")) {
        return false;
    }
    e.deftype.type_param_count = params.len();
    return true;
}

fn bool Parser.parse_deftype_name(Parser* self, Expr* e) {
    // Name can be simple: Name or parenthesized: (Name Parent?) or (Name TypeParam...)
    // All extra symbols collected as type_params — disambiguate parent vs type-param at eval time.
    if (self.lexer.current.type == T_LPAREN) {
        return self.parse_deftype_name_compound(e);
    }
    if (self.lexer.current.type == T_SYMBOL) {
        e.deftype.name = self.get_current_symbol();
        self.lexer.advance();
        return true;
    }
    self.set_error("expected type name after [type]");
    return false;
}

fn bool Parser.parse_deftype_typed_field(Parser* self, List{TypeField}* fields) {
    self.lexer.advance();  // consume (
    TypeAnnotation ann = self.parse_type_annotation();
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected field name");
        return false;
    }
    TypeField field = { .name = self.get_current_symbol(), .type_ann = ann };
    fields.push(field);
    self.lexer.advance();
    self.expect(T_RPAREN, ")");
    return !self.has_error;
}

fn bool Parser.parse_deftype_bare_field(Parser* self, List{TypeField}* fields) {
    TypeField field = { .name = self.get_current_symbol(), .type_ann = parser_empty_type_annotation() };
    fields.push(field);
    self.lexer.advance();
    return true;
}

fn Expr* Parser.parse_deftype(Parser* self, Expr* e) {
    self.init_deftype_expr(e);

    if (!self.parse_deftype_name(e)) return null;

    List{TypeField} fields;
    defer fields.free();
    // Parse fields: (^Type field) or just field
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        if (!self.parse_deftype_typed_field(&fields)) return null;
    }

    // Also allow bare symbol fields (untyped)
    while (self.lexer.current.type == T_SYMBOL && !self.has_error) {
        if (!self.parse_deftype_bare_field(&fields)) return null;
    }

    self.expect(T_RPAREN, ")");  // close define
    if (self.has_error) return null;
    if (!self.copy_type_field_list(&e.deftype.fields, &fields)) return null;
    e.deftype.field_count = fields.len();
    return e;
}

/**
 * Parse abstract type: (define [abstract] Name) or (define [abstract] (Name Parent))
 */
fn Expr* Parser.parse_defabstract(Parser* self, Expr* e) {
    e.tag = E_DEFABSTRACT;
    e.defabstract.has_parent = false;
    e.defabstract.parent = 0;

    if (self.lexer.current.type == T_LPAREN) {
        self.lexer.advance();
        if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected type name"); return null; }
        e.defabstract.name = self.get_current_symbol();
        self.lexer.advance();
        if (self.lexer.current.type == T_SYMBOL) {
            e.defabstract.parent = self.get_current_symbol();
            e.defabstract.has_parent = true;
            self.lexer.advance();
        }
        self.expect(T_RPAREN, ")");
    } else if (self.lexer.current.type == T_SYMBOL) {
        e.defabstract.name = self.get_current_symbol();
        self.lexer.advance();
    } else {
        self.set_error("expected type name after [abstract]");
        return null;
    }

    self.expect(T_RPAREN, ")");  // close define
    return e;
}

/**
 * Parse union type: (define [union] (Name T...) Variant1 (Variant2 fields...) ...)
 * or simple: (define [union] Name Variant1 (Variant2 T) ...)
 */
fn void Parser.init_defunion_expr(Parser* self, Expr* e) {
    e.tag = E_DEFUNION;
    e.defunion = mem::malloc(ExprDefUnion.sizeof);
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

/**
 * Parse type alias: (define [alias] Name ^Target)
 */
fn Expr* Parser.parse_defalias(Parser* self, Expr* e) {
    e.tag = E_DEFALIAS;

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected alias name after [alias]"); return null; }
    e.defalias.name = self.get_current_symbol();
    self.lexer.advance();

    if (self.lexer.current.type == T_SYMBOL && self.lexer.current.text[0] == '^') {
        e.defalias.target = self.parse_type_annotation();
    } else if (self.lexer.current.type == T_SYMBOL) {
        e.defalias.target.has_annotation = true;
        e.defalias.target.is_compound = false;
        e.defalias.target.is_dict = false;
        e.defalias.target.base_type = self.get_current_symbol();
        e.defalias.target.param_count = 0;
        e.defalias.target.meta_count = 0;
        e.defalias.target.has_val_literal = false;
        self.lexer.advance();
    } else {
        self.set_error("expected target type after alias name");
        return null;
    }

    self.expect(T_RPAREN, ")");
    return e;
}

fn Expr* Parser.parse_defeffect(Parser* self, Expr* e) {
    e.tag = E_DEFEFFECT;
    e.defeffect.has_arg_type = false;

    if (self.lexer.current.type != T_LPAREN) {
        self.set_error("expected ( after [effect]");
        return null;
    }
    self.lexer.advance();  // consume '('

    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected effect tag name");
        return null;
    }
    e.defeffect.name = self.get_current_symbol();
    self.lexer.advance();

    if (self.lexer.current.type == T_LPAREN) {
        self.lexer.advance();  // consume '('
        if (self.lexer.current.type == T_SYMBOL && self.lexer.current.text[0] == '^') {
            e.defeffect.arg_type = self.parse_type_annotation();
            e.defeffect.has_arg_type = true;
            if (self.lexer.current.type == T_SYMBOL) {
                self.lexer.advance();
            }
        }
        self.expect(T_RPAREN, ")");  // close param
    }

    self.expect(T_RPAREN, ")");  // close effect form
    self.expect(T_RPAREN, ")");  // close define
    return e;
}
