module lisp;

import std::collections::list;

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

fn Expr* Parser.build_let_expr_chain(Parser* self, Expr* outer, Expr* body, List{SymbolId} names, List{Pattern*} patterns, List{Expr*} inits, bool is_recursive) {
    usz binding_count = names.len();
    if (binding_count == 1) {
        outer.tag = E_LET;
        outer.let_expr.name = names[0];
        outer.let_expr.init = inits[0];
        outer.let_expr.body = body;
        outer.let_expr.is_recursive = is_recursive;
        outer.let_expr.pattern = patterns[0];
        return outer;
    }

    Expr* inner = body;
    for (usz i = binding_count; i > 1; i--) {
        Expr* wrapper = self.interp.alloc_expr();
        wrapper.tag = E_LET;
        wrapper.loc_line = outer.loc_line;
        wrapper.loc_column = outer.loc_column;
        wrapper.let_expr.name = names[i - 1];
        wrapper.let_expr.init = inits[i - 1];
        wrapper.let_expr.body = inner;
        wrapper.let_expr.is_recursive = false;
        wrapper.let_expr.pattern = patterns[i - 1];
        inner = wrapper;
    }

    outer.tag = E_LET;
    outer.let_expr.name = names[0];
    outer.let_expr.init = inits[0];
    outer.let_expr.body = inner;
    outer.let_expr.is_recursive = is_recursive;
    outer.let_expr.pattern = patterns[0];
    return outer;
}

fn Expr* Parser.parse_let(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'let' location
    self.lexer.advance();  // consume 'let'

    bool is_recursive = self.consume_let_rec_annotation();

    if (self.lexer.current.type == T_SYMBOL) {
        SymbolId loop_name = self.get_current_symbol();
        self.lexer.advance();  // consume loop name

        return self.parse_named_let(e, loop_name);
    }

    self.expect(T_LPAREN, "(");  // consume '(' of bindings list

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

    Expr* body = self.parse_implicit_begin();

    self.expect(T_RPAREN, ")");  // consume outer ')' of let form

    Expr* result = self.build_let_expr_chain(e, body, names, patterns, inits, is_recursive);
    names.free();
    patterns.free();
    inits.free();
    return result;
}
