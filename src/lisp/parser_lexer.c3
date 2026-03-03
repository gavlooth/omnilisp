module lisp;


import std::io;
import std::collections::list;
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

struct Token {
    TokenType type;
    char[64]  text;
    usz       text_len;
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

fn void Lexer.init(Lexer* self, char[] source) {
    self.source = source;
    self.pos = 0;
    self.len = source.len;
    self.line = 1;
    self.column = 1;
    self.advance();  // Prime the first token
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

fn bool is_symbol_char(char c) {
    if (c >= 'a' && c <= 'z') return true;
    if (c >= 'A' && c <= 'Z') return true;
    if (c >= '0' && c <= '9') return true;
    if (c == '_' || c == '-' || c == '+' || c == '*' || c == '/' ||
        c == '=' || c == '<' || c == '>' || c == '!' || c == '?' ||
        c == ':' || c == '.' || c == '@' || c == '#' || c == '$' ||
        c == '%' || c == '&' || c == '|' || c == '^' || c == '~') {
        return true;
    }
    // Accept high bytes (>= 0x80) for UTF-8 multibyte characters (e.g. λ)
    if ((uint)c >= 0x80) return true;
    return false;
}

fn void Lexer.scan_string(Lexer* self) {
    self.next_char();  // consume opening quote
    usz out_idx = 0;
    bool truncated = false;

    while (self.pos < self.len && self.source[self.pos] != '"') {
        char ch = self.source[self.pos];

        if (ch == '\\' && self.pos + 1 < self.len) {
            // Handle escape sequences
            self.next_char();
            char escaped = self.source[self.pos];
            switch (escaped) {
                case 'n':
                    if (out_idx < 63) { self.current.text[out_idx++] = '\n'; } else { truncated = true; }
                case 't':
                    if (out_idx < 63) { self.current.text[out_idx++] = '\t'; } else { truncated = true; }
                case '\\':
                    if (out_idx < 63) { self.current.text[out_idx++] = '\\'; } else { truncated = true; }
                case '"':
                    if (out_idx < 63) { self.current.text[out_idx++] = '"'; } else { truncated = true; }
                default:
                    // Unknown escape, just include the character
                    if (out_idx < 63) { self.current.text[out_idx++] = escaped; } else { truncated = true; }
            }
            self.next_char();
        } else {
            if (out_idx < 63) { self.current.text[out_idx++] = ch; } else { truncated = true; }
            self.next_char();
        }
    }

    if (self.pos < self.len && self.source[self.pos] == '"') {
        self.next_char();  // consume closing quote
    } else {
        // EOF without closing quote
        self.current.type = T_ERROR;
        char[] err = "unterminated string literal";
        for (usz i = 0; i < err.len; i++) self.current.text[i] = err[i];
        self.current.text[err.len] = 0;
        self.current.text_len = err.len;
        return;
    }

    if (truncated) {
        self.current.type = T_ERROR;
        char[] err = "string literal too long (max 63 bytes)";
        for (usz i = 0; i < err.len; i++) self.current.text[i] = err[i];
        self.current.text[err.len] = 0;
        self.current.text_len = err.len;
        return;
    }

    self.current.text[out_idx] = 0;
    self.current.text_len = out_idx;
    self.current.type = T_STRING;
}

// Returns true if a hash dispatch was matched, false to fall through to symbol parsing
fn bool Lexer.scan_hash_dispatch(Lexer* self) {
    if (self.pos + 1 < self.len) {
        char next = self.source[self.pos + 1];
        if (next == '{') {
            // #{} set literal
            self.next_char();  // consume #
            self.next_char();  // consume {
            self.current.type = T_HASH_LBRACE;
            return true;
        }
        if (next == '_') {
            // #_ or #N_ form comment — handled as T_HASH_UNDERSCORE
            self.next_char();  // consume #
            self.next_char();  // consume _
            self.current.type = T_HASH_UNDERSCORE;
            self.current.int_value = 1;  // default: skip 1 form
            return true;
        }
        if (next >= '1' && next <= '9') {
            // #N_ form comment (e.g., #3_)
            char digit = next;
            if (self.pos + 2 < self.len && self.source[self.pos + 2] == '_') {
                self.next_char();  // consume #
                self.next_char();  // consume N
                self.next_char();  // consume _
                self.current.type = T_HASH_UNDERSCORE;
                self.current.int_value = (long)(digit - '0');
                return true;
            }
        }
        if (next == 'r' && self.pos + 2 < self.len && self.source[self.pos + 2] == '"') {
            // #r"pattern" regex literal
            self.next_char();  // consume #
            self.next_char();  // consume r
            // Now parse the string literal (pos is at ")
            // Reuse string parsing but set type to T_REGEX
            self.next_char();  // consume opening "
            usz out_idx = 0;
            while (self.pos < self.len && self.source[self.pos] != '"') {
                char ch = self.source[self.pos];
                if (ch == '\\' && self.pos + 1 < self.len) {
                    self.next_char();
                    char escaped = self.source[self.pos];
                    if (out_idx < 63) { self.current.text[out_idx++] = '\\'; }
                    if (out_idx < 63) { self.current.text[out_idx++] = escaped; }
                    self.next_char();
                } else {
                    if (out_idx < 63) { self.current.text[out_idx++] = ch; }
                    self.next_char();
                }
            }
            if (self.pos < self.len) self.next_char();  // consume closing "
            self.current.text[out_idx] = 0;
            self.current.text_len = out_idx;
            self.current.type = T_REGEX;
            return true;
        }
    }
    // Unrecognized # dispatch — treat as symbol char for backward compat
    return false;
}

fn void Lexer.scan_symbol(Lexer* self) {
    usz start = self.pos;
    bool has_dot = false;
    while (self.pos < self.len && is_symbol_char(self.source[self.pos])) {
        // Stop before .[ so it can be lexed as T_DOT_BRACKET
        if (self.source[self.pos] == '.' &&
            self.pos + 1 < self.len && self.source[self.pos + 1] == '[') {
            break;
        }
        // Stop before .. so it can be lexed as T_DOTDOT
        if (self.source[self.pos] == '.' &&
            self.pos + 1 < self.len && self.source[self.pos + 1] == '.') {
            break;
        }
        if (self.source[self.pos] == '.') {
            has_dot = true;
        }
        self.next_char();
    }
    usz len = self.pos - start;
    if (len > 63) {
        self.current.type = T_ERROR;
        char[] err = "symbol name too long (max 63 bytes)";
        for (usz i = 0; i < err.len; i++) self.current.text[i] = err[i];
        self.current.text[err.len] = 0;
        self.current.text_len = err.len;
        return;
    }

    for (usz i = 0; i < len; i++) {
        self.current.text[i] = self.source[start + i];
    }
    self.current.text[len] = 0;
    self.current.text_len = len;

    // Classify as T_PATH if contains dot (and not just a dot at start/end)
    // e.g., "point.x" is a path, ".x" or "x." or "." alone are just symbols
    if (has_dot && len > 1 && self.current.text[0] != '.' && self.current.text[len-1] != '.') {
        self.current.type = T_PATH;
    } else {
        self.current.type = T_SYMBOL;
    }
}

fn void Lexer.scan_number(Lexer* self) {
    usz start = self.pos;
    bool negative = false;
    if (self.source[self.pos] == '-') {
        negative = true;
        self.next_char();
    }
    long val = 0;
    while (self.pos < self.len &&
           self.source[self.pos] >= '0' && self.source[self.pos] <= '9') {
        long digit = (long)(self.source[self.pos] - '0');
        if (val > (long.max - digit) / 10) {
            self.current.type = T_ERROR;
            char[] err = "integer literal overflow";
            for (usz i = 0; i < err.len; i++) self.current.text[i] = err[i];
            self.current.text[err.len] = 0;
            self.current.text_len = err.len;
            return;
        }
        val = val * 10 + digit;
        self.next_char();
    }

    // Check for decimal point → float literal
    if (self.pos < self.len && self.source[self.pos] == '.' &&
        self.pos + 1 < self.len && self.source[self.pos + 1] >= '0' && self.source[self.pos + 1] <= '9') {
        // Parse as float: collect digits into text buffer, then convert
        double dval = (double)val;
        self.next_char(); // consume '.'
        double frac = 0.1;
        while (self.pos < self.len &&
               self.source[self.pos] >= '0' && self.source[self.pos] <= '9') {
            dval += (double)(self.source[self.pos] - '0') * frac;
            frac *= 0.1;
            self.next_char();
        }
        // Optional exponent: e/E followed by optional +/- and digits
        if (self.pos < self.len && (self.source[self.pos] == 'e' || self.source[self.pos] == 'E')) {
            self.next_char(); // consume e/E
            bool exp_neg = false;
            if (self.pos < self.len && (self.source[self.pos] == '+' || self.source[self.pos] == '-')) {
                exp_neg = self.source[self.pos] == '-';
                self.next_char();
            }
            long exp_val = 0;
            while (self.pos < self.len &&
                   self.source[self.pos] >= '0' && self.source[self.pos] <= '9') {
                exp_val = exp_val * 10 + (long)(self.source[self.pos] - '0');
                self.next_char();
            }
            double multiplier = 1.0;
            for (long i = 0; i < exp_val; i++) multiplier *= 10.0;
            if (exp_neg) { dval /= multiplier; } else { dval *= multiplier; }
        }
        if (negative) dval = -dval;
        self.current.type = T_FLOAT;
        self.current.double_value = dval;
        self.current.text_len = self.pos - start;
        return;
    }

    if (negative) val = -val;

    self.current.type = T_INT;
    self.current.int_value = val;
    self.current.text_len = self.pos - start;
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

    if (c == '(') {
        self.current.type = T_LPAREN;
        self.next_char();
        return;
    }

    if (c == ')') {
        self.current.type = T_RPAREN;
        self.next_char();
        return;
    }

    if (c == '`') {
        self.current.type = T_BACKTICK;
        self.next_char();
        return;
    }

    if (c == ',') {
        if (self.pos + 1 < self.len && self.source[self.pos + 1] == '@') {
            self.current.type = T_COMMA_AT;
            self.next_char();
            self.next_char();
        } else {
            self.current.type = T_COMMA;
            self.next_char();
        }
        return;
    }

    if (c == '\'') {
        self.current.type = T_QUOTE;
        self.next_char();
        return;
    }

    if (c == '[') {
        self.current.type = T_LBRACKET;
        self.next_char();
        return;
    }

    if (c == ']') {
        self.current.type = T_RBRACKET;
        self.next_char();
        return;
    }

    if (c == '{') {
        self.current.type = T_LBRACE;
        self.next_char();
        return;
    }

    if (c == '}') {
        self.current.type = T_RBRACE;
        self.next_char();
        return;
    }

    // Logic variable: ?name (for Deduce queries)
    if (c == '?' && self.pos + 1 < self.len) {
        char next = self.source[self.pos + 1];
        if ((next >= 'a' && next <= 'z') || (next >= 'A' && next <= 'Z') || next == '_') {
            // Read ?varname as a symbol with ? prefix
            usz start = self.pos;
            self.next_char();  // consume ?
            usz out_idx = 0;
            self.current.text[out_idx++] = '?';
            while (self.pos < self.len && is_symbol_char(self.source[self.pos]) && out_idx < 63) {
                self.current.text[out_idx++] = self.source[self.pos];
                self.next_char();
            }
            self.current.text[out_idx] = 0;
            self.current.text_len = out_idx;
            self.current.type = T_SYMBOL;  // ?var is a symbol starting with ?
            return;
        }
    }

    // Reader dispatch: # followed by {, r", _, N_
    if (c == '#') {
        if (self.scan_hash_dispatch()) return;
        // Unrecognized # dispatch — fall through to symbol parsing
    }

    // String literal: "..."
    if (c == '"') {
        self.scan_string();
        return;
    }

    // Integer or float (with optional leading minus)
    if ((c >= '0' && c <= '9') ||
        (c == '-' && self.pos + 1 < self.len &&
         self.source[self.pos + 1] >= '0' && self.source[self.pos + 1] <= '9')) {
        self.scan_number();
        return;
    }

    // Rest pattern: .. (must check before .[ and symbol)
    if (c == '.' && self.pos + 1 < self.len && self.source[self.pos + 1] == '.') {
        // Make sure it's not ... or longer
        if (self.pos + 2 >= self.len || self.source[self.pos + 2] != '.') {
            self.current.type = T_DOTDOT;
            self.current.text[0] = '.';
            self.current.text[1] = '.';
            self.current.text[2] = 0;
            self.current.text_len = 2;
            self.next_char();
            self.next_char();
            return;
        }
    }

    // Index access: .[ (dot-bracket notation)
    // Must check before symbol parsing since . is a symbol char
    if (c == '.' && self.pos + 1 < self.len && self.source[self.pos + 1] == '[') {
        self.current.type = T_DOT_BRACKET;
        self.current.text[0] = '.';
        self.current.text[1] = '[';
        self.current.text[2] = 0;
        self.current.text_len = 2;
        self.next_char();
        self.next_char();
        return;
    }

    // Wildcard: _ (standalone underscore)
    if (c == '_') {
        // Check if it's a standalone _ (not part of a symbol like _foo)
        if (self.pos + 1 >= self.len || !is_symbol_char(self.source[self.pos + 1])) {
            self.current.type = T_UNDERSCORE;
            self.current.text[0] = '_';
            self.current.text[1] = 0;
            self.current.text_len = 1;
            self.pos++;
            return;
        }
    }

    // Symbol or Path (dot-separated like point.x or person.address.city)
    // Stops when seeing .[ (index access) or .. (rest pattern)
    if (is_symbol_char(c)) {
        self.scan_symbol();
        return;
    }

    // Unknown character
    self.current.type = T_ERROR;
    self.pos++;
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

