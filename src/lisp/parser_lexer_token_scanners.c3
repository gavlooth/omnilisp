module lisp;

fn bool Lexer.scan_punctuation_token(Lexer* self, char c) {
    switch (c) {
        case '(':
            self.current.type = T_LPAREN;
            self.next_char();
            return true;
        case ')':
            self.current.type = T_RPAREN;
            self.next_char();
            return true;
        case '`':
            self.current.type = T_BACKTICK;
            self.next_char();
            return true;
        case '\'':
            self.current.type = T_QUOTE;
            self.next_char();
            return true;
        case '[':
            self.current.type = T_LBRACKET;
            self.next_char();
            return true;
        case ']':
            self.current.type = T_RBRACKET;
            self.next_char();
            return true;
        case '{':
            self.current.type = T_LBRACE;
            self.next_char();
            return true;
        case '}':
            self.current.type = T_RBRACE;
            self.next_char();
            return true;
        case ',':
            if (self.pos + 1 < self.len && self.source[self.pos + 1] == '@') {
                self.current.type = T_COMMA_AT;
                self.next_char();
                self.next_char();
            } else {
                self.current.type = T_COMMA;
                self.next_char();
            }
            return true;
        default:
            return false;
    }
}

fn bool Lexer.scan_logic_variable(Lexer* self, char c) {
    if (c != '?' || self.pos + 1 >= self.len) return false;

    char next = self.source[self.pos + 1];
    if (!((next >= 'a' && next <= 'z') || (next >= 'A' && next <= 'Z') || next == '_')) {
        return false;
    }

    // Read ?varname as a symbol with ? prefix
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
    return true;
}

fn bool Lexer.scan_dot_tokens(Lexer* self, char c) {
    if (c != '.') return false;

    // Rest pattern: .. (must check before .[ and symbol)
    if (self.pos + 1 < self.len && self.source[self.pos + 1] == '.') {
        // Make sure it's not ... or longer
        if (self.pos + 2 >= self.len || self.source[self.pos + 2] != '.') {
            self.current.type = T_DOTDOT;
            self.current.text[0] = '.';
            self.current.text[1] = '.';
            self.current.text[2] = 0;
            self.current.text_len = 2;
            self.next_char();
            self.next_char();
            return true;
        }
    }

    // Index access: .[ (dot-bracket notation)
    // Must check before symbol parsing since . is a symbol char
    if (self.pos + 1 < self.len && self.source[self.pos + 1] == '[') {
        self.current.type = T_DOT_BRACKET;
        self.current.text[0] = '.';
        self.current.text[1] = '[';
        self.current.text[2] = 0;
        self.current.text_len = 2;
        self.next_char();
        self.next_char();
        return true;
    }
    return false;
}

fn bool Lexer.scan_standalone_underscore(Lexer* self, char c) {
    if (c != '_') return false;
    // Check if it's a standalone _ (not part of a symbol like _foo)
    if (self.pos + 1 < self.len && is_symbol_char(self.source[self.pos + 1])) return false;

    self.current.type = T_UNDERSCORE;
    self.current.text[0] = '_';
    self.current.text[1] = 0;
    self.current.text_len = 1;
    self.pos++;
    return true;
}
