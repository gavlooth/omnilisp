module lisp;

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
    if (len >= LEXER_TEXT_CAP) {
        self.current.type = T_ERROR;
        char[] err = "symbol name too long";
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
    if (self.scan_number_float(start, val, negative)) return;

    if (negative) val = -val;

    self.current.type = T_INT;
    self.current.int_value = val;
    self.current.text_len = self.pos - start;
}

fn bool Lexer.scan_number_float( Lexer* self, usz start, long int_part, bool negative) {
    if (self.pos >= self.len || self.source[self.pos] != '.') return false;
    if (self.pos + 1 >= self.len || self.source[self.pos + 1] < '0' || self.source[self.pos + 1] > '9') return false;

    double dval = (double)int_part;
    self.next_char();  // consume '.'
    dval = self.scan_number_fraction(dval);
    dval = self.scan_number_exponent(dval);
    if (negative) dval = -dval;
    self.current.type = T_FLOAT;
    self.current.double_value = dval;
    self.current.text_len = self.pos - start;
    return true;
}

fn double Lexer.scan_number_fraction(Lexer* self, double dval) {
    double frac = 0.1;
    while (self.pos < self.len &&
           self.source[self.pos] >= '0' && self.source[self.pos] <= '9') {
        dval += (double)(self.source[self.pos] - '0') * frac;
        frac *= 0.1;
        self.next_char();
    }
    return dval;
}

fn double Lexer.scan_number_exponent(Lexer* self, double dval) {
    if (self.pos >= self.len || (self.source[self.pos] != 'e' && self.source[self.pos] != 'E')) return dval;

    self.next_char();  // consume e/E
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
    if (exp_neg) return dval / multiplier;
    return dval * multiplier;
}
