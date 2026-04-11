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
    clause.effect_tag = (SymbolId)0;
    clause.effect_tag_expr = null;
    clause.has_effect_tag_expr = false;

    if (self.lexer.current.type == T_LPAREN) {
        self.set_error("handle clause syntax was removed; handle clauses must be `(tag arg body)` and nested clause forms are not allowed");
        return false;
    }

    if (self.lexer.current.type != T_SYMBOL && self.lexer.current.type != T_PATH) {
        self.set_error("expected effect tag or effect name in handle clause");
        return false;
    }

    if (!self.parse_effect_tag_ref(&clause.effect_tag, &clause.effect_tag_expr, &clause.has_effect_tag_expr)) {
        return false;
    }

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
    if (e == null) return null;
    self.lexer.advance();  // consume 'handle'

    bool strict = self.consume_handle_strict_annotation();

    Expr* body = self.parse_expr();

    e.tag = E_HANDLE;

    List{EffectClause} clause_list;
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        self.lexer.advance();  // consume outer (
        EffectClause clause;
        if (!self.parse_handle_clause(&clause)) return null;

        self.expect(T_RPAREN, ")");
        if (self.has_error) return null;

        clause_list.push(clause);
    }

    if (clause_list.len() == 0) {
        clause_list.free();
        self.set_error("handle form requires at least one clause after the body");
        return null;
    }

    e.handle = (ExprHandle*)self.alloc_ast_bytes(ExprHandle.sizeof, "out of memory while allocating handle expression");
    if (e.handle == null) { clause_list.free(); return null; }
    e.handle.body = body;
    e.handle.strict_mode = strict;

    e.handle.clause_count = clause_list.len();
    e.handle.clauses = (EffectClause*)self.alloc_ast_array_bytes(EffectClause.sizeof, clause_list.len(), "out of memory while allocating handle clauses");
    if (e.handle.clauses == null && clause_list.len() > 0) { clause_list.free(); return null; }
    for (usz i = 0; i < clause_list.len(); i++) { e.handle.clauses[i] = clause_list[i]; }
    clause_list.free();

    self.expect(T_RPAREN, ")");

    return e;
}
