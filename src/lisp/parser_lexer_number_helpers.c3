module lisp;

const usz LEXER_FLOAT_MAX_FRACTION_DIGITS = 324;
const long LEXER_FLOAT_MAX_POSITIVE_EXPONENT = 308;
const long LEXER_FLOAT_MAX_NEGATIVE_EXPONENT = 324;

fn bool Lexer.scan_number_float(Lexer* self, usz start, double int_part, bool negative) {
    if (self.pos >= self.len || self.source[self.pos] != '.') return false;
    if (self.pos + 1 >= self.len || self.source[self.pos + 1] < '0' || self.source[self.pos + 1] > '9') return false;

    double dval = int_part;
    self.next_char();  // consume '.'
    dval = self.scan_number_fraction(dval);
    dval = self.scan_number_exponent(dval);
    if (self.current.type == T_ERROR) return true;
    if (negative) dval = -dval;
    if (!conversion_double_is_finite(dval)) {
        self.set_error_text("float literal out of range");
        return true;
    }
    self.current.type = T_FLOAT;
    self.current.double_value = dval;
    self.current.text_len = self.pos - start;
    return true;
}

fn double Lexer.scan_number_fraction(Lexer* self, double dval) {
    double frac = 0.1;
    usz fraction_digits = 0;
    while (self.pos < self.len &&
           self.source[self.pos] >= '0' && self.source[self.pos] <= '9') {
        if (fraction_digits < LEXER_FLOAT_MAX_FRACTION_DIGITS) {
            dval += (double)(self.source[self.pos] - '0') * frac;
            frac *= 0.1;
        }
        fraction_digits++;
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
    long exp_limit = exp_neg ? LEXER_FLOAT_MAX_NEGATIVE_EXPONENT : LEXER_FLOAT_MAX_POSITIVE_EXPONENT;
    bool has_exp_digits = false;
    while (self.pos < self.len &&
           self.source[self.pos] >= '0' && self.source[self.pos] <= '9') {
        has_exp_digits = true;
        long digit = (long)(self.source[self.pos] - '0');
        if (exp_val > exp_limit / 10 ||
            (exp_val == exp_limit / 10 && digit > exp_limit % 10)) {
            self.set_error_text("float literal exponent out of range");
            return dval;
        }
        exp_val = exp_val * 10 + digit;
        self.next_char();
    }
    if (!has_exp_digits) {
        self.set_error_text("float literal exponent requires digits");
        return dval;
    }
    double multiplier = 1.0;
    for (long i = 0; i < exp_val; i++) multiplier *= 10.0;
    if (exp_neg) return dval / multiplier;
    return dval * multiplier;
}
