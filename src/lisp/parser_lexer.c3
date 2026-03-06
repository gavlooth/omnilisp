module lisp;


import std::io;
// =============================================================================
// SECTION 1: LEXER
// =============================================================================

enum TokenType : char {
    T_EOF,
    T_LPAREN,
    T_RPAREN,
    T_LBRACKET,     // [ for array literals
    T_RBRACKET,     // ] for array literals
    T_DOT_BRACKET,  // .[ for index access
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
    T_HASH_LBRACE,  // #{ for set literals
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

fn bool Lexer.ensure_text_capacity(Lexer* self, usz needed_cap) {
    if (needed_cap <= self.current.text_cap) return true;

    usz new_cap = self.current.text_cap > 0 ? self.current.text_cap : LEXER_TEXT_INITIAL_CAP;
    while (new_cap < needed_cap) {
        if (new_cap > usz.max / 2) {
            new_cap = needed_cap;
            break;
        }
        new_cap *= 2;
    }

    char* new_text = (char*)mem::realloc(self.current.text, new_cap);
    if (new_text == null) return false;
    self.current.text = new_text;
    self.current.text_cap = new_cap;
    return true;
}

fn bool Lexer.push_current_text_char(Lexer* self, char c) @inline {
    usz next_len = self.current.text_len + 1;
    if (!self.ensure_text_capacity(next_len + 1)) return false;
    self.current.text[self.current.text_len] = c;
    self.current.text_len = next_len;
    self.current.text[self.current.text_len] = 0;
    return true;
}

fn bool Lexer.set_error_text(Lexer* self, char[] msg) {
    self.current.type = T_ERROR;
    if (!self.ensure_text_capacity(msg.len + 1)) {
        self.current.text_len = 0;
        return false;
    }
    for (usz i = 0; i < msg.len; i++) self.current.text[i] = msg[i];
    self.current.text[msg.len] = 0;
    self.current.text_len = msg.len;
    return true;
}

fn void Lexer.init(Lexer* self, char[] source) {
    self.source = source;
    self.pos = 0;
    self.len = source.len;
    self.line = 1;
    self.column = 1;
    self.current.text = (char*)mem::malloc(LEXER_TEXT_INITIAL_CAP);
    if (self.current.text == null) {
        self.current.text_cap = 0;
        self.current.text_len = 0;
        self.current.type = T_ERROR;
        return;
    }
    self.current.text_cap = LEXER_TEXT_INITIAL_CAP;
    self.current.text_len = 0;
    self.current.text[0] = 0;
    self.advance();  // Prime the first token
}

fn void Lexer.destroy(Lexer* self) {
    if (self.current.text != null) {
        mem::free(self.current.text);
        self.current.text = null;
    }
    self.current.text_cap = 0;
    self.current.text_len = 0;
}

fn char Lexer.peek(Lexer* self) {
    if (self.pos >= self.len) return 0;
    return self.source[self.pos];
}

fn char Lexer.next_char(Lexer* self) {
    if (self.pos >= self.len) return 0;
    char c = self.source[self.pos++];
    if (c == '\n') {
        self.line++;
        self.column = 1;
    } else {
        self.column++;
    }
    return c;
}

fn void Lexer.skip_whitespace(Lexer* self) {
    while (self.pos < self.len) {
        char c = self.source[self.pos];
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
            self.next_char();  // Track line/column
        } else if (c == ';') {
            // Line comment: skip to end of line
            while (self.pos < self.len && self.source[self.pos] != '\n') {
                self.next_char();
            }
        } else if (c == '#' && self.pos + 1 < self.len && self.source[self.pos + 1] == '|') {
            // Block comment: #| ... |# (nestable)
            self.next_char();  // consume #
            self.next_char();  // consume |
            int depth = 1;
            while (self.pos < self.len && depth > 0) {
                if (self.source[self.pos] == '#' && self.pos + 1 < self.len && self.source[self.pos + 1] == '|') {
                    depth++;
                    self.next_char();
                    self.next_char();
                } else if (self.source[self.pos] == '|' && self.pos + 1 < self.len && self.source[self.pos + 1] == '#') {
                    depth--;
                    self.next_char();
                    self.next_char();
                } else {
                    self.next_char();
                }
            }
        } else {
            break;
        }
    }
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
    // Stops when seeing .[ (index access) or .. (rest pattern)
    if (is_symbol_char(c)) {
        self.scan_symbol();
        return true;
    }
    return false;
}

fn void Lexer.set_error_token(Lexer* self) @inline {
    self.current.type = T_ERROR;
    self.current.text_len = 0;
    if (self.current.text != null && self.current.text_cap > 0) self.current.text[0] = 0;
    self.pos++;
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
        // Unrecognized # dispatch — fall through to symbol parsing
    }

    if (self.scan_literal_or_symbol(c)) return;

    // Unknown character
    self.set_error_token();
}

fn bool Lexer.at_end(Lexer* self) {
    return self.current.type == T_EOF;
}

fn bool Lexer.match(Lexer* self, TokenType t) {
    if (self.current.type == t) {
        self.advance();
        return true;
    }
    return false;
}

fn void Lexer.expect(Lexer* self, TokenType t, char[] msg) {
    if (!self.match(t)) {
        io::printfn("Parse error at line %d, col %d: expected '%s'", self.line, self.column, msg);
        unreachable("parse error: expected token not found");
    }
}
