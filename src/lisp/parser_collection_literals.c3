module lisp;

import std::core::mem;
import std::collections::list;

/**
 * Check for and parse postfix index access: expr.[index]
 * Returns the expression wrapped in E_INDEX if followed by .[, otherwise returns unchanged.
 *
 * OmniLisp dot-bracket syntax:
 *   arr.[0]           - array/list access by integer index
 *   dict.['key]       - dictionary access by quoted symbol key
 *   matrix.[i].[j]    - chained indexing
 */
fn Expr* Parser.parse_postfix_index(Parser* self, Expr* base) {
    if (self.has_error) return null;
    // Keep parsing postfix index access as long as we see .[index]
    while (!self.has_error) {
        bool is_dot_bracket = self.lexer.current.type == T_DOT_BRACKET;
        bool is_dot_symbol = self.lexer.current.type == T_SYMBOL &&
            self.lexer.current.text_len == 1 && self.lexer.current.text[0] == '.';

        if (!is_dot_bracket && !is_dot_symbol) {
            break;
        }

        if (is_dot_bracket) {
            self.lexer.advance(); // consume '.['
        } else {
            self.lexer.advance(); // consume '.'
            if (self.lexer.current.type != T_LBRACKET) {
                self.set_error("expected '[' after '.' in index access");
                return null;
            }
            self.lexer.advance(); // consume '['
        }

        // Parse the index expression
        Expr* idx = self.parse_expr();
        if (self.has_error || idx == null) return null;

        self.expect(T_RBRACKET, "]");
        if (self.has_error) return null;

        // Wrap in E_INDEX
        Expr* indexed = self.alloc_expr_here();
        if (indexed == null) return null;
        indexed.tag = E_INDEX;
        indexed.index.collection = base;
        indexed.index.index = idx;

        base = indexed;
    }
    return base;
}
