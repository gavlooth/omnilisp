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

fn bool Parser.parse_handle_clause(Parser* self, EffectClause* clause) {
    if (self.lexer.current.type != T_SYMBOL) {
        self.set_error("legacy handle clause syntax was removed; use (tag arg body) and with-continuation when needed");
        return false;
    }

    clause.effect_tag = self.get_current_symbol();
    self.lexer.advance();

    if (self.lexer.current.type != T_SYMBOL) { self.set_error("expected arg name"); return false; }
    clause.arg_name = self.get_current_symbol();
    self.lexer.advance();

    clause.k_name = self.interp.sym__k;
    bool was_in_handle_clause = self.in_handle_clause;
    self.in_handle_clause = true;
    clause.handler_body = self.parse_implicit_block();
    self.in_handle_clause = was_in_handle_clause;
    return !self.has_error;
}

fn Expr* Parser.parse_handle(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'handle' location
    self.lexer.advance();  // consume 'handle'

    bool strict = self.consume_handle_strict_annotation();

    Expr* body = self.parse_expr();

    e.tag = E_HANDLE;
    e.handle = (ExprHandle*)ast_arena_alloc(&self.interp.ast_arena, ExprHandle.sizeof);
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
    e.handle.clauses = (EffectClause*)ast_arena_alloc(&self.interp.ast_arena, EffectClause.sizeof * clause_list.len());
    for (usz i = 0; i < clause_list.len(); i++) { e.handle.clauses[i] = clause_list[i]; }
    clause_list.free();

    self.expect(T_RPAREN, ")");

    return e;
}
