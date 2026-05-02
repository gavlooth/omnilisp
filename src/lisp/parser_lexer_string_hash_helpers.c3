module lisp;

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

fn int hash_radix_digit_value(char c) @inline {
    if (c >= '0' && c <= '9') return (int)(c - '0');
    if (c >= 'A' && c <= 'F') return 10 + (int)(c - 'A');
    return -1;
}

fn bool Lexer.scan_hash_radix_literal(Lexer* self, char next) {
    int radix = 0;
    if (next == 'x' || next == 'X') {
        radix = 16;
    } else if (next == 'b' || next == 'B') {
        radix = 2;
    } else if (next == 'o' || next == 'O') {
        radix = 8;
    } else {
        return false;
    }

    usz digit_pos = self.pos + 2;
    if (digit_pos < self.len && self.source[digit_pos] == '-') digit_pos++;
    if (digit_pos >= self.len) return false;
    int first_value = hash_radix_digit_value(self.source[digit_pos]);
    if (first_value < 0 || first_value >= radix) return false;

    usz end_pos = digit_pos;
    while (end_pos < self.len) {
        int digit = hash_radix_digit_value(self.source[end_pos]);
        if (digit < 0 || digit >= radix) break;
        end_pos++;
    }
    if (end_pos < self.len && is_symbol_char(self.source[end_pos])) return false;

    self.next_char();  // consume #
    self.next_char();  // consume radix marker

    bool negative = false;
    if (self.pos < self.len && self.source[self.pos] == '-') {
        negative = true;
        self.next_char();
    }

    long val = 0;
    bool saw_digit = false;
    while (self.pos < self.len) {
        int digit = hash_radix_digit_value(self.source[self.pos]);
        if (digit < 0 || digit >= radix) break;
        saw_digit = true;
        if (val > (long.max - (long)digit) / (long)radix) {
            self.set_error_text("integer literal overflow");
            return true;
        }
        val = val * (long)radix + (long)digit;
        self.next_char();
    }

    if (!saw_digit) {
        self.set_error_text("invalid radix integer literal");
        return true;
    }
    self.current.type = T_INT;
    self.current.int_value = negative ? -val : val;
    self.current.text_len = 0;
    return true;
}

fn bool is_reader_tag_start_char(char c) @inline {
    if (c >= 'a' && c <= 'z') return true;
    if (c >= 'A' && c <= 'Z') return true;
    if ((uint)c >= 0x80) return true;
    return false;
}

fn bool is_reader_tag_char(char c) @inline {
    if (c == '#') return false;
    return is_symbol_char(c);
}

fn bool Lexer.scan_hash_reader_tag(Lexer* self, char next) {
    if (!is_reader_tag_start_char(next)) return false;
    self.next_char();  // consume #
    self.current.text_len = 0;
    if (self.current.text != null && self.current.text_cap > 0) self.current.text[0] = 0;

    while (self.pos < self.len && is_reader_tag_char(self.source[self.pos])) {
        if (!self.push_current_text_char(self.source[self.pos])) {
            self.set_error_text("out of memory while lexing reader tag");
            return true;
        }
        self.next_char();
    }

    self.current.type = T_READER_TAG;
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
        case '{':
            self.next_char();  // consume #
            self.next_char();  // consume {
            self.current.type = T_HASH_LBRACE;
            return true;
        case 'r':
            if (self.scan_hash_regex_literal(next)) return true;
            if (self.scan_hash_reader_tag(next)) return true;
            self.set_error_text("unknown # dispatch sequence");
            return true;
        case 'x':
        case 'X':
        case 'b':
        case 'B':
        case 'o':
        case 'O':
            if (self.scan_hash_radix_literal(next)) return true;
            if (self.scan_hash_reader_tag(next)) return true;
            self.set_error_text("unknown # dispatch sequence");
            return true;
        default:
            if (next >= '1' && next <= '9' && self.scan_hash_counted_underscore_form(next)) {
                return true;
            }
            if (self.scan_hash_reader_tag(next)) return true;
            self.set_error_text("unknown # dispatch sequence");
            return true;
    }
}
