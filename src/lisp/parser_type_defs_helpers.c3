module lisp;

import std::collections::list;

fn bool Parser.copy_symbol_list(Parser* self, SymbolId** out, List{SymbolId}* items, char[] oom_error) {
    if (items.len() == 0) {
        *out = null;
        return true;
    }
    SymbolId* buf = (SymbolId*)ast_arena_alloc(&self.interp.ast_arena, SymbolId.sizeof * items.len());
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
    TypeField* buf = (TypeField*)ast_arena_alloc(&self.interp.ast_arena, TypeField.sizeof * items.len());
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
    UnionVariant* buf = (UnionVariant*)ast_arena_alloc(&self.interp.ast_arena, UnionVariant.sizeof * items.len());
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

fn bool Parser.validate_invariant_type_params(Parser* self, List{SymbolId}* params) {
    for (usz i = 0; i < params.len(); i++) {
        char[] name = self.interp.symbols.get_name((*params)[i]);
        if (name.len <= 1) continue;
        if (name[0] != '+' && name[0] != '-') continue;

        self.set_error("variance markers (+T/-T) are not supported yet; type parameters are invariant");
        return false;
    }
    return true;
}
