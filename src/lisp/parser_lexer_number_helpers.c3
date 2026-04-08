module lisp;

fn bool Lexer.scan_number_float(Lexer* self, usz start, long int_part, bool negative) {
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
