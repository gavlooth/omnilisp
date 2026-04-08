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
    self.current.text_len = 0;
    if (self.current.text != null && self.current.text_cap > 0) self.current.text[0] = 0;
    if (!self.push_current_text_char('?')) {
        self.set_error_text("out of memory while lexing logic variable");
        return true;
    }
    while (self.pos < self.len && is_symbol_char(self.source[self.pos])) {
        if (!self.push_current_text_char(self.source[self.pos])) {
            self.set_error_text("out of memory while lexing logic variable");
            return true;
        }
        self.next_char();
    }
    self.current.type = T_SYMBOL;  // ?var is a symbol starting with ?
    return true;
}
