module lisp;

/**
 * Parse abstract type: (define [abstract] Name) or (define [abstract] (Name Parent))
 */
fn Expr* Parser.parse_defabstract(Parser* self, Expr* e) {
    e.tag = E_DEFABSTRACT;
    e.defabstract.has_parent = false;
    e.defabstract.parent = (SymbolId)0;

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
        e.defalias.target.val_literal.tag = NIL;
        e.defalias.target.val_literal.int_value = 0;
        e.defalias.target.val_literal.sym_value = (SymbolId)0;
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
