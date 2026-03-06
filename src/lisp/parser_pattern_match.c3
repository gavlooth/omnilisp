module lisp;

import std::collections::list;

/**
 * Parse a pattern for match expressions.
 * Patterns:
 *   _             - wildcard (matches anything, binds nothing)
 *   x             - variable (matches anything, binds to x)
 *   42, "hello"   - literal
 *   'sym          - quoted symbol
 *   [a b c]       - exact sequence
 *   [head .. tail] - first element + rest
 *   [x y ..]      - first N, ignore rest
 *   [.. last]     - ignore all but last
 */
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
        p.elements = (Pattern**)mem::malloc(Pattern*.sizeof * elems.len());
        for (usz i = 0; i < elems.len(); i++) { p.elements[i] = elems[i]; }
        elems.free();

        self.lexer.advance();  // consume ']'
        return p;
    }

    while (self.lexer.current.type != T_RBRACKET && self.lexer.current.type != T_DOTDOT && !self.has_error) {
        elems.push(self.parse_pattern());
    }

    p.elem_count = elems.len();
    p.elements = (Pattern**)mem::malloc(Pattern*.sizeof * elems.len());
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
    p.dict_keys = (SymbolId*)mem::malloc(SymbolId.sizeof * keys.len());
    for (usz i = 0; i < keys.len(); i++) { p.dict_keys[i] = keys[i]; }
    keys.free();

    self.expect(T_RBRACE, "}");
    return p;
}

fn bool Parser.parse_as_pattern_marker(Parser* self) {
    if (self.lexer.current.type != T_QUOTE) return false;
    self.lexer.advance();  // consume '
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("expected 'as in as-pattern");
        return false;
    }
    SymbolId maybe_as = self.get_current_symbol();
    if ((uint)maybe_as != (uint)self.interp.sym_as) {
        self.set_error("expected 'as in as-pattern");
        return false;
    }
    self.lexer.advance();
    return true;
}

fn Pattern* Parser.parse_paren_pattern(Parser* self) {
    self.lexer.advance();  // consume '('
    Pattern* first = self.parse_pattern();
    if (self.has_error) return null;

    if (self.lexer.current.type == T_QUOTE) {
        Pattern* p = self.interp.alloc_pattern();
        p.tag = PAT_AS;
        p.as_sub = first;
        if (!self.parse_as_pattern_marker()) return null;
        if (self.lexer.current.type != T_SYMBOL) {
            self.set_error("expected binding name after 'as");
            return null;
        }
        p.as_name = self.get_current_symbol();
        self.lexer.advance();
        self.expect(T_RPAREN, ")");
        return p;
    }

    if (self.lexer.current.type == T_SYMBOL &&
        (uint)self.get_current_symbol() == (uint)self.interp.sym_as) {
        self.set_error("expected 'as in as-pattern");
        return null;
    }

    if (first.tag != PAT_VAR) {
        self.set_error("expected constructor, guard, or (pattern 'as name)");
        return null;
    }

    SymbolId sym = first.var_name;
    if ((uint)sym == (uint)self.interp.sym_question) {
        Pattern* p = self.interp.alloc_pattern();
        p.tag = PAT_GUARD;
        p.guard_pred = self.parse_expr();
        if (self.lexer.current.type != T_RPAREN) {
            p.guard_sub = self.parse_pattern();
        } else {
            p.guard_sub = null;
        }
        self.expect(T_RPAREN, ")");
        return p;
    }

    Pattern* p = self.interp.alloc_pattern();
    p.tag = PAT_CONSTRUCTOR;
    p.constructor_name = sym;
    List{Pattern*} subs;
    while (self.lexer.current.type != T_RPAREN && !self.has_error) {
        subs.push(self.parse_pattern());
    }
    p.ctor_sub_count = subs.len();
    p.ctor_sub_patterns = (Pattern**)mem::malloc(Pattern*.sizeof * subs.len());
    for (usz i = 0; i < subs.len(); i++) { p.ctor_sub_patterns[i] = subs[i]; }
    subs.free();
    self.expect(T_RPAREN, ")");
    return p;
}

fn Pattern* Parser.parse_pattern(Parser* self) {
    if (self.has_error) return null;
    switch (self.lexer.current.type) {
        case T_UNDERSCORE: {
            Pattern* p = self.interp.alloc_pattern();
            p.tag = PAT_WILDCARD;
            self.lexer.advance();
            return p;
        }
        case T_QUOTE: {
            self.lexer.advance();
            Pattern* p = self.interp.alloc_pattern();
            p.tag = PAT_QUOTE;
            p.quote_datum = self.parse_datum();
            return p;
        }
        case T_INT: {
            Pattern* p = self.interp.alloc_pattern();
            p.tag = PAT_LIT;
            p.lit_value = self.interp.alloc_value_root();
            p.lit_value.tag = INT;
            p.lit_value.int_val = self.lexer.current.int_value;
            self.lexer.advance();
            return p;
        }
        case T_STRING:
            return self.parse_string_pattern_literal();
        case T_LBRACKET:
            return self.parse_seq_pattern();
        case T_LBRACE:
            return self.parse_dict_pattern();
        case T_LPAREN:
            return self.parse_paren_pattern();
        case T_SYMBOL: {
            Pattern* p = self.interp.alloc_pattern();
            p.tag = PAT_VAR;
            p.var_name = self.get_current_symbol();
            self.lexer.advance();
            return p;
        }
        default:
            self.set_error("invalid pattern syntax");
            return null;
    }
}

/**
 * Parse a match expression.
 * (match expr (pattern1 result1) (pattern2 result2) ...)
 */
fn Expr* Parser.parse_match(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'match' location
    self.lexer.advance();  // consume 'match'

    Expr* scrutinee = self.parse_expr();

    e.tag = E_MATCH;
    e.match = (ExprMatch*)mem::malloc(ExprMatch.sizeof);
    e.match.scrutinee = scrutinee;

    List{MatchClause} match_clauses;
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        self.lexer.advance();  // consume '('
        Pattern* pattern = self.parse_pattern();
        Expr* result = self.parse_expr();
        self.expect(T_RPAREN, ")");
        match_clauses.push({ .pattern = pattern, .result = result });
    }

    e.match.clause_count = match_clauses.len();
    e.match.clauses = (MatchClause*)mem::malloc(MatchClause.sizeof * match_clauses.len());
    for (usz i = 0; i < match_clauses.len(); i++) { e.match.clauses[i] = match_clauses[i]; }
    match_clauses.free();

    self.expect(T_RPAREN, ")");
    return e;
}
