module lisp;

import std::collections::list;

fn Pattern* Parser.parse_string_pattern_literal(Parser* self) {
    Pattern* p = self.alloc_pattern();
    if (p == null) return null;
    p.tag = PAT_LIT;
    p.lit_value = self.alloc_root_value_or_error("out of memory while parsing pattern literal");
    if (p.lit_value == null) return null;
    p.lit_value.tag = STRING;
    StringVal* builder = strval_new_from_slice(self.lexer.current.text[:self.lexer.current.text_len]);
    if (builder == null) {
        self.set_error("out of memory while parsing string literal");
        return null;
    }
    strval_into_value(builder, p.lit_value);
    self.lexer.advance();
    return p;
}

fn Pattern* Parser.parse_seq_pattern(Parser* self) {
    self.lexer.advance();  // consume '['

    Pattern* p = self.alloc_pattern();
    if (p == null) return null;
    p.tag = PAT_SEQ;
    p.rest_pos = REST_NONE;
    p.rest_binding = (SymbolId)0;

    List{Pattern*} elems = {};

    if (self.lexer.current.type == T_DOTDOT) {
        self.lexer.advance();  // consume '..'
        p.rest_pos = REST_START;
        while (self.lexer.current.type != T_RBRACKET && !self.has_error) {
            elems.push(self.parse_pattern());
        }
        if (self.has_error) { elems.free(); return null; }

        p.elem_count = elems.len();
        p.elements = null;
        if (p.elem_count > 0) {
            p.elements = (Pattern**)self.alloc_ast_array_bytes(
                Pattern*.sizeof,
                p.elem_count,
                "out of memory while allocating sequence pattern"
            );
            if (p.elements == null) { elems.free(); return null; }
        }
        for (usz i = 0; i < elems.len(); i++) { p.elements[i] = elems[i]; }
        elems.free();

        self.lexer.advance();  // consume ']'
        return p;
    }

    while (self.lexer.current.type != T_RBRACKET && self.lexer.current.type != T_DOTDOT && !self.has_error) {
        elems.push(self.parse_pattern());
    }

    p.elem_count = elems.len();
    p.elements = null;
    if (p.elem_count > 0) {
        p.elements = (Pattern**)self.alloc_ast_array_bytes(
            Pattern*.sizeof,
            p.elem_count,
            "out of memory while allocating sequence pattern"
        );
        if (p.elements == null) { elems.free(); return null; }
    }
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

    Pattern* p = self.alloc_pattern();
    if (p == null) return null;
    p.tag = PAT_DICT;

    List{SymbolId} keys = {};
    while (self.lexer.current.type == T_SYMBOL && !self.has_error) {
        SymbolId key = self.get_current_symbol();
        for (usz i = 0; i < keys.len(); i++) {
            if ((uint)keys[i] == (uint)key) {
                self.set_error("duplicate key in dict pattern");
                keys.free();
                return null;
            }
        }
        keys.push(key);
        self.lexer.advance();
    }
    if (self.has_error) { keys.free(); return null; }

    p.dict_key_count = keys.len();
    p.dict_keys = null;
    if (p.dict_key_count > 0) {
        p.dict_keys = (SymbolId*)self.alloc_ast_array_bytes(
            SymbolId.sizeof,
            p.dict_key_count,
            "out of memory while allocating dict pattern keys"
        );
        if (p.dict_keys == null) { keys.free(); return null; }
    }
    for (usz i = 0; i < keys.len(); i++) { p.dict_keys[i] = keys[i]; }
    keys.free();

    self.expect(T_RBRACE, "}");
    return p;
}

fn Pattern* Parser.parse_symbol_pattern(Parser* self) {
    SymbolId sym = self.get_current_symbol();
    SymbolId sym_nil = self.intern_or_error("nil", "parser: failed to intern nil pattern symbol");
    if (sym_nil == INVALID_SYMBOL_ID) return null;

    Pattern* p = self.alloc_pattern();
    if (p == null) return null;
    if ((uint)sym == (uint)sym_nil) {
        p.tag = PAT_LIT;
        p.lit_value = self.alloc_root_value_or_error("out of memory while parsing pattern literal");
        if (p.lit_value == null) return null;
        p.lit_value.tag = NIL;
    } else if ((uint)sym == (uint)self.interp.sym_false) {
        p.tag = PAT_LIT;
        p.lit_value = self.alloc_root_value_or_error("out of memory while parsing pattern literal");
        if (p.lit_value == null) return null;
        p.lit_value.tag = SYMBOL;
        p.lit_value.sym_val = self.interp.sym_false;
    } else if ((uint)sym == (uint)self.interp.sym_Void) {
        // `Void` is a literal singleton value in pattern position.
        p.tag = PAT_LIT;
        p.lit_value = self.alloc_root_value_or_error("out of memory while parsing pattern literal");
        if (p.lit_value == null) return null;
        p.lit_value.tag = VOID;
    } else {
        p.tag = PAT_VAR;
        p.var_name = sym;
    }
    self.lexer.advance();
    return p;
}
