module lisp;

import std::collections::list;

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
    e.match.clauses = (MatchClause*)mem::malloc(MatchClause.sizeof * match_clauses.len());
    for (usz i = 0; i < match_clauses.len(); i++) { e.match.clauses[i] = match_clauses[i]; }
    match_clauses.free();

    self.expect(T_RPAREN, ")");
    return e;
}
