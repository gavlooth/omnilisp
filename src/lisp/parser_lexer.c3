module lisp;


// =============================================================================
// SECTION 1: LEXER
// =============================================================================

enum TokenType : char {
    T_EOF,
    T_LPAREN,
    T_RPAREN,
    T_LBRACKET,     // [ for array literals
    T_RBRACKET,     // ] for array literals
    T_DOT_BRACKET,  // .[ token for parser-time index parsing
    T_DOTDOT,       // .. for rest patterns in destructuring
    T_QUOTE,
    T_INT,
    T_FLOAT,        // Floating-point literal
    T_STRING,       // "..." string literal
    T_SYMBOL,
    T_PATH,         // dot-separated path: point.x, person.address.city
    T_UNDERSCORE,   // _ wildcard pattern
    T_BACKTICK,     // ` quasiquote
    T_COMMA,        // , unquote
    T_COMMA_AT,     // ,@ unquote-splicing
    T_LBRACE,       // { for metadata dictionaries
    T_RBRACE,       // } for metadata dictionaries
    T_HASH_UNDERSCORE, // #_ or #N_ form comment (int_value = N)
    T_REGEX,        // #r"pattern" compiled regex literal
    T_ERROR,
}

const usz LEXER_TEXT_INITIAL_CAP = 1024;

struct Token {
    TokenType type;
    char*     text;
    usz       text_len;
    usz       text_cap;
    long      int_value;    // For T_INT
    double    double_value; // For T_FLOAT
    usz       line;       // 1-indexed line number
    usz       column;     // 1-indexed column number
}

struct Lexer {
    char[]  source;
    usz     pos;
    usz     len;
    usz     line;     // Current line (1-indexed)
    usz     column;   // Current column (1-indexed)
    Token   current;
}

fn bool Lexer.is_number_start(Lexer* self, char c) @inline {
    if (c >= '0' && c <= '9') return true;
    if (c != '-' || self.pos + 1 >= self.len) return false;
    char next = self.source[self.pos + 1];
    return next >= '0' && next <= '9';
}

fn bool Lexer.scan_literal_or_symbol(Lexer* self, char c) {
    // String literal: "..."
    if (c == '"') {
        self.scan_string();
        return true;
    }

    // Integer or float (with optional leading minus)
    if (self.is_number_start(c)) {
        self.scan_number();
        return true;
    }

    if (self.scan_dot_tokens(c)) return true;
    if (self.scan_standalone_underscore(c)) return true;

    // Symbol or Path (dot-separated like point.x or person.address.city)
    // Stops before rest-pattern tokens (`..`)
    if (is_symbol_char(c)) {
        self.scan_symbol();
        return true;
    }
    return false;
}

fn void Lexer.advance(Lexer* self) {
    self.skip_whitespace();

    // Capture token start position
    self.current.line = self.line;
    self.current.column = self.column;

    if (self.pos >= self.len) {
        self.current.type = T_EOF;
        return;
    }

    char c = self.source[self.pos];

    if (self.scan_punctuation_token(c)) return;
    if (self.scan_logic_variable(c)) return;

    // Reader dispatch: # followed by {, r", _, N_
    if (c == '#') {
        if (self.scan_hash_dispatch()) return;
    }

    if (self.scan_literal_or_symbol(c)) return;

    // Unknown character
    self.set_error_token();
}
