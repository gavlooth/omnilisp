module lisp;

import std::collections::list;

fn void Parser.reject_legacy_let_binding_shape(Parser* self, bool named_let) {
    if (self.has_error) return;

    if (self.lexer.current.type == T_LBRACKET || self.lexer.current.type == T_LBRACE) {
        if (named_let) {
            self.set_error("named let bindings must use flat pairs: (let loop (x 0 y 1) ...)");
        } else {
            self.set_error("let bindings must use flat pairs: (let (x 1 y 2) ...)");
        }
        return;
    }

    if (self.lexer.current.type == T_LPAREN) {
        if (named_let) {
            self.set_error("Scheme-style named let bindings were removed; use flat pairs: (let loop (x 0 y 1) ...)");
        } else {
            self.set_error("Scheme-style let bindings were removed; use flat pairs: (let (x 1 y 2) ...)");
        }
    }
}

fn bool Parser.consume_let_rec_annotation(Parser* self) {
    if (self.lexer.current.type != T_SYMBOL) return false;
    if (self.lexer.current.text_len < 1 || self.lexer.current.text[0] != '^') return false;
    if (self.lexer.current.text_len != 4 ||
        self.lexer.current.text[1] != 'r' ||
        self.lexer.current.text[2] != 'e' ||
        self.lexer.current.text[3] != 'c') {
        return false;
    }
    self.lexer.advance();  // consume ^rec
    return true;
}

fn bool Parser.collect_let_bindings(Parser* self, List{SymbolId}* names, List{Pattern*}* patterns, List{Expr*}* inits) {
    while ((self.lexer.current.type == T_SYMBOL ||
            self.lexer.current.type == T_LBRACKET ||
            self.lexer.current.type == T_LBRACE) &&
           !self.has_error) {
        if (self.lexer.current.type == T_LBRACKET || self.lexer.current.type == T_LBRACE) {
            names.push((SymbolId)0);
            patterns.push(self.parse_pattern());
            inits.push(self.parse_expr());
        } else {
            names.push(self.get_current_symbol());
            patterns.push(null);
            self.lexer.advance();
            inits.push(self.parse_expr());
        }
    }
    return !self.has_error;
}

fn Expr* Parser.parse_let(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'let' location
    if (e == null) return null;
    self.lexer.advance();  // consume 'let'

    bool is_recursive = self.consume_let_rec_annotation();

    if (self.lexer.current.type == T_SYMBOL) {
        SymbolId loop_name = self.get_current_symbol();
        self.lexer.advance();  // consume loop name

        return self.parse_named_let(e, loop_name);
    }

    if (self.lexer.current.type != T_LPAREN) {
        self.reject_legacy_let_binding_shape(false);
        if (self.has_error) return null;
    }

    self.expect(T_LPAREN, "(");  // consume '(' of bindings list
    if (self.has_error) return null;
    if (self.lexer.current.type == T_LPAREN) {
        self.reject_legacy_let_binding_shape(false);
        if (self.has_error) return null;
    }

    // Collect flat-pair bindings: (x 1 y 2 z 3) or ([a b] [1 2] x 3)
    // Each binding is either a symbol name or a destructuring pattern.
    List{SymbolId} names;
    List{Pattern*} patterns;
    List{Expr*} inits;

    self.collect_let_bindings(&names, &patterns, &inits);
    if (self.has_error) { names.free(); patterns.free(); inits.free(); return null; }

    self.expect(T_RPAREN, ")");  // consume ')' of bindings list
    if (self.has_error) { names.free(); patterns.free(); inits.free(); return null; }

    usz binding_count = names.len();
    if (binding_count < 1) { names.free(); patterns.free(); inits.free(); self.set_error("expected at least one binding in let"); return null; }

    Expr* body = self.parse_implicit_block();

    self.expect(T_RPAREN, ")");  // consume outer ')' of let form

    Expr* result = self.build_let_expr_chain(e, body, names, patterns, inits, is_recursive);
    names.free();
    patterns.free();
    inits.free();
    return result;
}
