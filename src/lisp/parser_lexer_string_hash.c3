module lisp;

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
    self.current.text_len = 0;
    if (self.current.text != null && self.current.text_cap > 0) self.current.text[0] = 0;

    while (self.pos < self.len && self.source[self.pos] != '"') {
        char ch = self.source[self.pos];

        if (ch == '\\' && self.pos + 1 < self.len) {
            // Handle escape sequences
            self.next_char();
            char escaped = self.source[self.pos];
            switch (escaped) {
                case 'n':
                    if (!self.push_current_text_char('\n')) {
                        self.set_error_text("out of memory while lexing string literal");
                        return;
                    }
                case 'r':
                    if (!self.push_current_text_char('\r')) {
                        self.set_error_text("out of memory while lexing string literal");
                        return;
                    }
                case 't':
                    if (!self.push_current_text_char('\t')) {
                        self.set_error_text("out of memory while lexing string literal");
                        return;
                    }
                case '\\':
                    if (!self.push_current_text_char('\\')) {
                        self.set_error_text("out of memory while lexing string literal");
                        return;
                    }
                case '"':
                    if (!self.push_current_text_char('"')) {
                        self.set_error_text("out of memory while lexing string literal");
                        return;
                    }
                default:
                    // Unknown escape, just include the character
                    if (!self.push_current_text_char(escaped)) {
                        self.set_error_text("out of memory while lexing string literal");
                        return;
                    }
            }
            self.next_char();
        } else {
            if (!self.push_current_text_char(ch)) {
                self.set_error_text("out of memory while lexing string literal");
                return;
            }
            self.next_char();
        }
    }

    if (self.pos < self.len && self.source[self.pos] == '"') {
        self.next_char();  // consume closing quote
    } else {
        // EOF without closing quote
        self.set_error_text("unterminated string literal");
        return;
    }

    self.current.type = T_STRING;
}

fn bool Lexer.scan_hash_underscore_form(Lexer* self, char next) @inline {
    if (next != '_') return false;
    self.next_char();  // consume #
    self.next_char();  // consume _
    self.current.type = T_HASH_UNDERSCORE;
    self.current.int_value = 1;  // default: skip 1 form
    return true;
}

fn bool Lexer.scan_hash_counted_underscore_form(Lexer* self, char next) {
    if (next < '1' || next > '9') return false;
    if (self.pos + 2 >= self.len || self.source[self.pos + 2] != '_') return false;
    self.next_char();  // consume #
    self.next_char();  // consume N
    self.next_char();  // consume _
    self.current.type = T_HASH_UNDERSCORE;
    self.current.int_value = (long)(next - '0');
    return true;
}

fn bool Lexer.scan_hash_regex_literal(Lexer* self, char next) {
    if (next != 'r') return false;
    if (self.pos + 2 >= self.len || self.source[self.pos + 2] != '"') return false;
    self.next_char();  // consume #
    self.next_char();  // consume r
    self.next_char();  // consume opening "
    self.current.text_len = 0;
    if (self.current.text != null && self.current.text_cap > 0) self.current.text[0] = 0;
    while (self.pos < self.len && self.source[self.pos] != '"') {
        char ch = self.source[self.pos];
        if (ch == '\\' && self.pos + 1 < self.len) {
            self.next_char();
            char escaped = self.source[self.pos];
            if (!self.push_current_text_char('\\')) {
                self.set_error_text("out of memory while lexing regex literal");
                return true;
            }
            if (!self.push_current_text_char(escaped)) {
                self.set_error_text("out of memory while lexing regex literal");
                return true;
            }
            self.next_char();
        } else {
            if (!self.push_current_text_char(ch)) {
                self.set_error_text("out of memory while lexing regex literal");
                return true;
            }
            self.next_char();
        }
    }
    if (self.pos < self.len && self.source[self.pos] == '"') {
        self.next_char();  // consume closing "
    } else {
        self.set_error_text("unterminated regex literal");
        return true;
    }
    self.current.type = T_REGEX;
    return true;
}

// Returns true after consuming or reporting a hash dispatch token.
fn bool Lexer.scan_hash_dispatch(Lexer* self) {
    if (self.pos + 1 >= self.len) {
        self.set_error_text("unknown # dispatch sequence");
        return true;
    }
    char next = self.source[self.pos + 1];
    switch (next) {
        case '_':
            return self.scan_hash_underscore_form(next);
        case 'r':
            if (self.scan_hash_regex_literal(next)) return true;
            self.set_error_text("unknown # dispatch sequence");
            return true;
        default:
            if (next >= '1' && next <= '9' && self.scan_hash_counted_underscore_form(next)) {
                return true;
            }
            self.set_error_text("unknown # dispatch sequence");
            return true;
    }
}
