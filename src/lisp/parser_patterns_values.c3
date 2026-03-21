module lisp;

import std::collections::list;

fn Pattern* Parser.parse_string_pattern_literal(Parser* self) {
    Pattern* p = self.interp.alloc_pattern();
    p.tag = PAT_LIT;
    p.lit_value = self.interp.alloc_value_root();
    p.lit_value.tag = STRING;
    usz len = self.lexer.current.text_len;
    StringVal* builder = strval_new(len + 1);
    for (usz i = 0; i < len; i++) {
        builder.chars[i] = self.lexer.current.text[i];
    }
    builder.chars[len] = 0;
    builder.len = len;
    strval_into_value(builder, p.lit_value);
    self.lexer.advance();
    return p;
}

fn Pattern* Parser.parse_seq_pattern(Parser* self) {
    self.lexer.advance();  // consume '['

    Pattern* p = self.interp.alloc_pattern();
    p.tag = PAT_SEQ;
    p.rest_pos = REST_NONE;
    p.rest_binding = 0;

    List{Pattern*} elems;

    if (self.lexer.current.type == T_DOTDOT) {
        self.lexer.advance();  // consume '..'
        p.rest_pos = REST_START;
        while (self.lexer.current.type != T_RBRACKET && !self.has_error) {
            elems.push(self.parse_pattern());
        }
        if (self.has_error) { elems.free(); return null; }

        p.elem_count = elems.len();
        p.elements = (Pattern**)ast_arena_alloc(&self.interp.ast_arena, Pattern*.sizeof * elems.len());
        for (usz i = 0; i < elems.len(); i++) { p.elements[i] = elems[i]; }
        elems.free();

        self.lexer.advance();  // consume ']'
        return p;
    }

    while (self.lexer.current.type != T_RBRACKET && self.lexer.current.type != T_DOTDOT && !self.has_error) {
        elems.push(self.parse_pattern());
    }

    p.elem_count = elems.len();
    p.elements = (Pattern**)ast_arena_alloc(&self.interp.ast_arena, Pattern*.sizeof * elems.len());
    for (usz i = 0; i < elems.len(); i++) { p.elements[i] = elems[i]; }
    elems.free();

    if (self.lexer.current.type == T_DOTDOT) {
        self.lexer.advance();  // consume '..'
        if (self.lexer.current.type == T_RBRACKET) {
            p.rest_pos = REST_END;
        } else {
            p.rest_pos = REST_MIDDLE;
            if (self.lexer.current.type != T_SYMBOL) {
                self.set_error("expected variable after ..");
                return null;
            }
            p.rest_binding = self.get_current_symbol();
            self.lexer.advance();
        }
    }

    self.expect(T_RBRACKET, "]");
    return p;
}

fn Pattern* Parser.parse_dict_pattern(Parser* self) {
    self.lexer.advance();  // consume '{'

    Pattern* p = self.interp.alloc_pattern();
    p.tag = PAT_DICT;

    List{SymbolId} keys;
    while (self.lexer.current.type == T_SYMBOL && !self.has_error) {
        keys.push(self.get_current_symbol());
        self.lexer.advance();
    }
    if (self.has_error) { keys.free(); return null; }

    p.dict_key_count = keys.len();
    p.dict_keys = (SymbolId*)ast_arena_alloc(&self.interp.ast_arena, SymbolId.sizeof * keys.len());
    for (usz i = 0; i < keys.len(); i++) { p.dict_keys[i] = keys[i]; }
    keys.free();

    self.expect(T_RBRACE, "}");
    return p;
}

fn Pattern* Parser.parse_symbol_pattern(Parser* self) {
    SymbolId sym = self.get_current_symbol();
    SymbolId sym_nil = self.interp.symbols.intern("nil");

    Pattern* p = self.interp.alloc_pattern();
    if ((uint)sym == (uint)sym_nil || (uint)sym == (uint)self.interp.sym_false) {
        // `nil` and `false` are literal falsy constants in pattern position.
        p.tag = PAT_LIT;
        p.lit_value = self.interp.alloc_value_root();
        p.lit_value.tag = NIL;
    } else if ((uint)sym == (uint)self.interp.sym_Void) {
        // `Void` is a literal singleton value in pattern position.
        p.tag = PAT_LIT;
        p.lit_value = self.interp.alloc_value_root();
        p.lit_value.tag = VOID;
    } else {
        p.tag = PAT_VAR;
        p.var_name = sym;
    }
    self.lexer.advance();
    return p;
}
