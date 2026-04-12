module lisp;

import std::collections::list;

/**
 * Parse a match expression.
 * (match expr (pattern1 result1) (pattern2 result2) ...)
 */
fn Expr* Parser.parse_match(Parser* self) {
    if (self.has_error) return null;
    Expr* e = self.alloc_expr_here();  // Capture 'match' location
    if (e == null) return null;
    self.lexer.advance();  // consume 'match'

    Expr* scrutinee = self.parse_expr();

    e.tag = E_MATCH;
    e.match = (ExprMatch*)self.alloc_ast_bytes(ExprMatch.sizeof, "out of memory while allocating match expression");
    if (e.match == null) return null;
    e.match.scrutinee = scrutinee;

    List{MatchClause} match_clauses;
    while (self.lexer.current.type == T_LPAREN && !self.has_error) {
        self.lexer.advance();  // consume '('
        Pattern* pattern = self.parse_pattern();
        if (self.has_error) {
            match_clauses.free();
            return null;
        }
        if (self.lexer.current.type == T_RPAREN) {
            match_clauses.free();
            self.set_error("match clauses must be (pattern result) pairs");
            return null;
        }
        Expr* result = self.parse_expr();
        if (self.has_error) {
            match_clauses.free();
            return null;
        }
        if (self.lexer.current.type != T_RPAREN) {
            match_clauses.free();
            self.set_error("match clauses must be (pattern result) pairs");
            return null;
        }
        self.expect(T_RPAREN, ")");
        match_clauses.push({ .pattern = pattern, .result = result });
    }

    e.match.clause_count = match_clauses.len();
    usz clause_count = e.match.clause_count;
    if (clause_count > 0) {
        if (clause_count > usz.max / MatchClause.sizeof) {
            self.set_error("match clauses is too large");
            match_clauses.free();
            return null;
        }
        e.match.clauses = (MatchClause*)self.alloc_ast_bytes(MatchClause.sizeof * clause_count, "out of memory while allocating match clauses");
    }
    if (e.match.clauses == null && clause_count > 0) { match_clauses.free(); return null; }
    for (usz i = 0; i < match_clauses.len(); i++) { e.match.clauses[i] = match_clauses[i]; }
    match_clauses.free();

    self.expect(T_RPAREN, ")");
    return e;
}
