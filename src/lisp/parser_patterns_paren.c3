module lisp;

import std::collections::list;

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

fn bool Parser.current_is_dot_symbol(Parser* self) @inline {
    return self.lexer.current.type == T_SYMBOL
        && self.lexer.current.text_len == 1
        && self.lexer.current.text[0] == '.';
}

fn Pattern* Parser.parse_paren_pattern(Parser* self) {
    self.lexer.advance();  // consume '('
    Pattern* first = self.parse_pattern();
    if (self.has_error) return null;

    if (self.current_is_dot_symbol()) {
        self.lexer.advance();  // consume '.'
        Pattern* p = self.interp.alloc_pattern();
        p.tag = PAT_CONS;
        p.car_pat = first;
        p.cdr_pat = self.parse_pattern();
        if (self.has_error) return null;
        self.expect(T_RPAREN, ")");
        return p;
    }

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
    p.ctor_sub_patterns = (Pattern**)ast_arena_alloc(&self.interp.ast_arena, Pattern*.sizeof * subs.len());
    for (usz i = 0; i < subs.len(); i++) { p.ctor_sub_patterns[i] = subs[i]; }
    subs.free();
    self.expect(T_RPAREN, ")");
    return p;
}
