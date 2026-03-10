module lisp;

import std::collections::list;

fn bool Parser.consume_handle_strict_annotation(Parser* self) {
    if (self.lexer.current.type != T_SYMBOL) return false;
    if (self.lexer.current.text_len != 7) return false;
    if (self.lexer.current.text[0] != '^') return false;
    if (self.lexer.current.text[1] != 's') return false;
    if (self.lexer.current.text[2] != 't') return false;
    if (self.lexer.current.text[3] != 'r') return false;
    if (self.lexer.current.text[4] != 'i') return false;
    if (self.lexer.current.text[5] != 'c') return false;
    if (self.lexer.current.text[6] != 't') return false;
    self.lexer.advance();  // consume ^strict
    return true;
}

fn bool Parser.parse_handle_old_clause(Parser* self, EffectClause* clause) {
    self.lexer.advance();  // consume inner (
    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected effect tag"); return false; }
    clause.effect_tag = self.get_current_symbol();
    self.lexer.advance();

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected continuation name"); return false; }
    clause.k_name = self.get_current_symbol();
    self.lexer.advance();

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected arg name"); return false; }
    clause.arg_name = self.get_current_symbol();
    self.lexer.advance();

    self.expect(T_RPAREN, ")");
    if (self.has_error) return false;
    clause.handler_body = self.parse_expr();
    return !self.has_error;
}

fn bool Parser.parse_handle_new_clause(Parser* self, EffectClause* clause) {
    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected effect tag"); return false; }
    clause.effect_tag = self.get_current_symbol();
    self.lexer.advance();

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected arg name"); return false; }
    clause.arg_name = self.get_current_symbol();
    self.lexer.advance();

    clause.k_name = self.interp.sym__k;
    clause.handler_body = self.parse_implicit_block();
    return !self.has_error;
}

fn bool Parser.parse_handle_clause(Parser* self, EffectClause* clause) {
    if (self.lexer.current.type == T_LPAREN) {
        return self.parse_handle_old_clause(clause);
    }
    if (self.lexer.current.type == T_SYMBOL) {
        return self.parse_handle_new_clause(clause);
    }
    self.set_error("expected effect tag or '('");
    return false;
}

fn Expr* Parser.parse_handle(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'handle' location
    self.lexer.advance();  // consume 'handle'

    bool strict = self.consume_handle_strict_annotation();

    Expr* body = self.parse_expr();

    e.tag = E_HANDLE;
    e.handle = (ExprHandle*)mem::malloc(ExprHandle.sizeof);
    e.handle.body = body;
    e.handle.strict_mode = strict;

    List{EffectClause} clause_list;
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        self.lexer.advance();  // consume outer (
        EffectClause clause;
        if (!self.parse_handle_clause(&clause)) return null;

        self.expect(T_RPAREN, ")");
        if (self.has_error) return null;

        clause_list.push(clause);
    }

    // Finalize clauses
    e.handle.clause_count = clause_list.len();
    e.handle.clauses = (EffectClause*)mem::malloc(EffectClause.sizeof * clause_list.len());
    for (usz i = 0; i < clause_list.len(); i++) { e.handle.clauses[i] = clause_list[i]; }
    clause_list.free();

    self.expect(T_RPAREN, ")");

    return e;
}
