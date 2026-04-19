module lisp;

fn void Lexer.scan_symbol(Lexer* self) {
    usz start = self.pos;
    bool has_dot = false;
    while (self.pos < self.len && is_symbol_char(self.source[self.pos])) {
        // Stop before .[ so it can be parsed as '.' followed by '[' for index access.
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
    if (len == usz.max) {
        self.set_error_text("symbol literal too long");
        return;
    }
    if (!self.ensure_text_capacity(len + 1)) {
        self.set_error_text("out of memory while lexing symbol");
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
    double float_int_part = 0.0;
    bool is_long_min_literal = false;
    long max_prefix = long.max / 10;
    long max_final_digit = negative ? 8 : 7;
    while (self.pos < self.len &&
           self.source[self.pos] >= '0' && self.source[self.pos] <= '9') {
        long digit = (long)(self.source[self.pos] - '0');
        if (is_long_min_literal ||
            val > max_prefix ||
            (val == max_prefix && digit > max_final_digit)) {
            self.set_error_text("integer literal overflow");
            return;
        }
        float_int_part = float_int_part * 10.0 + (double)digit;
        if (negative && val == max_prefix && digit == 8) {
            is_long_min_literal = true;
        } else {
            val = val * 10 + digit;
        }
        self.next_char();
    }

    // Check for decimal point → float literal
    if (self.scan_number_float(start, float_int_part, negative)) return;

    if (is_long_min_literal) {
        val = long.min;
    } else if (negative) {
        val = -val;
    }

    self.current.type = T_INT;
    self.current.int_value = val;
    self.current.text_len = self.pos - start;
}
