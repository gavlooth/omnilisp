module lisp;

/**
 * Parse a pattern for match expressions.
 * Patterns:
 *   _             - wildcard (matches anything, binds nothing)
 *   x             - variable (matches anything, binds to x)
 *   42, "hello"   - literal
 *   'sym          - quoted symbol
 *   [a b c]       - exact sequence
 *   [head .. tail] - first element + rest
 *   [x y ..]      - first N, ignore rest
 *   [.. last]     - ignore all but last
 */
fn Pattern* Parser.parse_pattern(Parser* self) {
    if (self.has_error) return null;
    switch (self.lexer.current.type) {
        case T_UNDERSCORE: {
            Pattern* p = self.interp.alloc_pattern();
            p.tag = PAT_WILDCARD;
            self.lexer.advance();
            return p;
        }
        case T_QUOTE: {
            self.lexer.advance();
            Pattern* p = self.interp.alloc_pattern();
            p.tag = PAT_QUOTE;
            p.quote_datum = self.parse_datum();
            return p;
        }
        case T_INT: {
            Pattern* p = self.interp.alloc_pattern();
            p.tag = PAT_LIT;
            p.lit_value = self.interp.alloc_value_root();
            p.lit_value.tag = INT;
            p.lit_value.int_val = self.lexer.current.int_value;
            self.lexer.advance();
            return p;
        }
        case T_STRING:
            return self.parse_string_pattern_literal();
        case T_LBRACKET:
            return self.parse_seq_pattern();
        case T_LBRACE:
            return self.parse_dict_pattern();
        case T_LPAREN:
            return self.parse_paren_pattern();
        case T_SYMBOL:
            return self.parse_symbol_pattern();
        default:
            self.set_error("invalid pattern syntax");
            return null;
    }
}
