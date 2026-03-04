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

fn bool Lexer.scan_hash_set_literal(Lexer* self, char next) @inline {
    if (next != '{') return false;
    self.next_char();  // consume #
    self.next_char();  // consume {
    self.current.type = T_HASH_LBRACE;
    return true;
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

// Returns true if a hash dispatch was matched, false to fall through to symbol parsing
fn bool Lexer.scan_hash_dispatch(Lexer* self) {
    if (self.pos + 1 >= self.len) return false;
    char next = self.source[self.pos + 1];
    if (self.scan_hash_set_literal(next)) return true;
    if (self.scan_hash_underscore_form(next)) return true;
    if (self.scan_hash_counted_underscore_form(next)) return true;
    if (self.scan_hash_regex_literal(next)) return true;
    // Unrecognized # dispatch — treat as symbol char for backward compat
    return false;
}
