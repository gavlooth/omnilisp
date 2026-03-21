module lisp;

fn bool Lexer.scan_dot_tokens(Lexer* self, char c) {
    if (c != '.') return false;

    // Rest pattern: .. (must check before .[ and symbol tokens)
    if (self.pos + 1 < self.len && self.source[self.pos + 1] == '.') {
        // Make sure it's not ... or longer
        if (self.pos + 2 >= self.len || self.source[self.pos + 2] != '.') {
            if (!self.ensure_text_capacity(3)) {
                self.set_error_text("out of memory while lexing token");
                return true;
            }
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

    // Compact dot + bracket surface: .[
    // Emit the '.' token only; the next lexer advance will emit '['.
    // This keeps leading-dot accessor shorthand and postfix expr.[key]
    // on the same parser path while preserving compact spelling.
    if (self.pos + 1 < self.len && self.source[self.pos + 1] == '[') {
        if (!self.ensure_text_capacity(2)) {
            self.set_error_text("out of memory while lexing token");
            return true;
        }
        self.current.type = T_SYMBOL;
        self.current.text[0] = '.';
        self.current.text[1] = 0;
        self.current.text_len = 1;
        self.next_char();
        return true;
    }

    return false;
}

fn bool Lexer.scan_standalone_underscore(Lexer* self, char c) {
    if (c != '_') return false;
    // Check if it's a standalone _ (not part of a symbol like _foo)
    if (self.pos + 1 < self.len && is_symbol_char(self.source[self.pos + 1])) return false;

    if (!self.ensure_text_capacity(2)) {
        self.set_error_text("out of memory while lexing token");
        return true;
    }
    self.current.type = T_UNDERSCORE;
    self.current.text[0] = '_';
    self.current.text[1] = 0;
    self.current.text_len = 1;
    self.pos++;
    return true;
}
