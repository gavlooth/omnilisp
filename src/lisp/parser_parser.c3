module lisp;
// =============================================================================
// SECTION 2: PARSER
// =============================================================================

const int PARSER_MAX_NESTING = 1024;

bool g_parser_force_ast_expr_alloc_fail = false;
bool g_parser_force_ast_pattern_alloc_fail = false;
bool g_parser_force_ast_alloc_fail = false;

struct Parser {
    Lexer*    lexer;
    Interp*   interp;
    bool      has_error;
    bool      in_handle_clause;
    char[256] error_msg;
    usz       error_msg_len;
    usz       error_line;
    usz       error_col;
    int       depth;
    int       max_depth;
}

fn void Parser.init(Parser* self, Lexer* lexer, Interp* interp) {
    self.lexer = lexer;
    self.interp = interp;
    self.has_error = false;
    self.in_handle_clause = false;
    self.error_msg_len = 0;
    self.error_line = 0;
    self.error_col = 0;
    self.depth = 0;
    self.max_depth = PARSER_MAX_NESTING;
}

/**
 * Set a parse error (keeps first error only).
 */
fn void Parser.set_error(Parser* self, char[] msg) {
    if (self.has_error) return;  // keep first error
    self.has_error = true;
    self.error_line = self.lexer.current.line;
    self.error_col = self.lexer.current.column;
    usz len = msg.len;
    if (len > 255) len = 255;
    for (usz i = 0; i < len; i++) self.error_msg[i] = msg[i];
    self.error_msg[len] = 0;
    self.error_msg_len = len;
}

fn SymbolId Parser.intern_or_error(Parser* self, char[] name, char[] err_msg) {
    if (self.has_error) return INVALID_SYMBOL_ID;
    SymbolId sym = self.interp.symbols.intern(name);
    if (sym == INVALID_SYMBOL_ID) self.set_error(err_msg);
    return sym;
}

/**
 * Expect a token type, or set a parse error.
 */
fn void Parser.expect(Parser* self, TokenType t, char[] msg) {
    if (self.has_error) return;
    if (!self.lexer.match(t)) {
        if (msg.len == 1) {
            switch (msg[0]) {
                case '(': self.set_error("expected '('"); return;
                case ')': self.set_error("expected ')'"); return;
                case '[': self.set_error("expected '['"); return;
                case ']': self.set_error("expected ']'"); return;
                case '{': self.set_error("expected '{'"); return;
                case '}': self.set_error("expected '}'"); return;
                default: break;
            }
        }
        self.set_error(msg);
    }
}

fn void* Parser.alloc_ast_bytes(Parser* self, usz size, char[] err_msg) {
    if (self.has_error) return null;
    if (g_parser_force_ast_alloc_fail) {
        self.set_error(err_msg);
        return null;
    }
    void* ptr = ast_arena_alloc(&self.interp.ast_arena, size);
    if (ptr == null) {
        self.set_error(err_msg);
        return null;
    }
    return ptr;
}

fn void* Parser.alloc_ast_array_bytes(Parser* self, usz elem_size, usz count, char[] err_msg) {
    if (count != 0 && elem_size > usz.max / count) {
        self.set_error(err_msg);
        return null;
    }
    return self.alloc_ast_bytes(elem_size * count, err_msg);
}

fn Expr* Parser.alloc_expr(Parser* self) {
    if (self.has_error) return null;
    if (g_parser_force_ast_expr_alloc_fail) {
        self.set_error("parser: out of memory while allocating expression");
        return null;
    }
    return (Expr*)self.alloc_ast_bytes(Expr.sizeof, "parser: out of memory while allocating expression");
}

fn Pattern* Parser.alloc_pattern(Parser* self) {
    if (self.has_error) return null;
    if (g_parser_force_ast_pattern_alloc_fail) {
        self.set_error("parser: out of memory while allocating pattern");
        return null;
    }
    return (Pattern*)self.alloc_ast_bytes(Pattern.sizeof, "parser: out of memory while allocating pattern");
}

/**
 * Allocate an expression and capture current source location.
 */
fn Expr* Parser.alloc_expr_here(Parser* self) {
    Expr* e = self.alloc_expr();
    if (e == null) return null;
    e.loc_line = self.lexer.current.line;
    e.loc_column = self.lexer.current.column;
    return e;
}

/**
 * Get interned symbol from current token text.
 */
fn SymbolId Parser.get_current_symbol(Parser* self) {
    SymbolId sym = self.interp.symbols.intern(self.lexer.current.text[:self.lexer.current.text_len]);
    if (sym == INVALID_SYMBOL_ID) {
        self.set_error("parser: failed to intern symbol");
    }
    return sym;
}
