module lisp;

import std::core::mem;
import std::collections::list;

fn Expr* Parser.postfix_symbol_key_expr(Parser* self, SymbolId sym) {
    if (sym == INVALID_SYMBOL_ID) {
        self.set_error("invalid postfix path segment");
        return null;
    }
    Expr* e = self.alloc_expr_here();
    if (e == null) return null;
    e.tag = E_LIT;
    e.lit.value = self.interp.alloc_value_root();
    e.lit.value.tag = SYMBOL;
    e.lit.value.sym_val = sym;
    return e;
}

fn Expr* Parser.postfix_index_expr(Parser* self, Expr* base, Expr* idx) {
    Expr* indexed = self.alloc_expr_here();
    if (indexed == null) return null;
    indexed.tag = E_INDEX;
    indexed.index.collection = base;
    indexed.index.index = idx;
    return indexed;
}

fn Expr* Parser.postfix_symbol_segments(Parser* self, Expr* base, char[] text, usz start) {
    for (usz i = start; i <= text.len; i++) {
        if (i != text.len && text[i] != '.') continue;
        if (i == start) {
            self.set_error("empty postfix path segment");
            return null;
        }
        SymbolId sym = self.intern_or_error(text[start:i - start], "failed to intern postfix path segment");
        Expr* idx = self.postfix_symbol_key_expr(sym);
        if (self.has_error || idx == null) return null;
        base = self.postfix_index_expr(base, idx);
        if (self.has_error || base == null) return null;
        start = i + 1;
    }
    return base;
}

fn Expr* Parser.parse_postfix_symbol_path(Parser* self, Expr* base) {
    char[] text = self.lexer.current.text[:self.lexer.current.text_len];
    if (text.len <= 1 || text[0] != '.') return base;
    self.lexer.advance();
    return self.postfix_symbol_segments(base, text, 1);
}

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
        bool is_dot_path_symbol = self.lexer.current.type == T_SYMBOL &&
            self.lexer.current.text_len > 1 && self.lexer.current.text[0] == '.';

        if (!is_dot_bracket && !is_dot_symbol && !is_dot_path_symbol) {
            break;
        }

        if (is_dot_path_symbol) {
            base = self.parse_postfix_symbol_path(base);
            continue;
        }

        if (is_dot_bracket) {
            self.lexer.advance(); // consume '.['
        } else {
            self.lexer.advance(); // consume '.'
            if (self.lexer.current.type == T_LBRACKET) {
                self.lexer.advance(); // consume '['
            } else {
                if (base.tag != E_INDEX) {
                    self.set_error("leading-dot accessors were removed; use ref or access syntax like expr.[key]");
                    return null;
                }
                if (self.lexer.current.type != T_SYMBOL && self.lexer.current.type != T_PATH) {
                    self.set_error("expected '[' or path segment after '.' in postfix access");
                    return null;
                }
                char[] text = self.lexer.current.text[:self.lexer.current.text_len];
                if (text.len == 0 || text[0] == '.') {
                    self.set_error("expected path segment after '.' in postfix access");
                    return null;
                }
                self.lexer.advance();
                base = self.postfix_symbol_segments(base, text, 0);
                if (self.has_error || base == null) return null;
                continue;
            }
        }

        // Fail closed with bracket-specific diagnostics when the postfix index
        // body is missing entirely (for example: `expr.[`).
        if (self.lexer.current.type == T_EOF) {
            self.set_error("expected ']'");
            return null;
        }

        // Parse the index expression
        Expr* idx = self.parse_expr();
        if (self.has_error || idx == null) return null;

        self.expect(T_RBRACKET, "]");
        if (self.has_error) return null;

        base = self.postfix_index_expr(base, idx);
        if (self.has_error || base == null) return null;
    }
    return base;
}
