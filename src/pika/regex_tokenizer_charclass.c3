module pika;

import std::core::mem;

fn CharClassData* cc_new() @inline {
    CharClassData* cc = mem::new(CharClassData);
    cc.num_ranges = 0;
    cc.ranges_cap = 0;
    cc.num_chars = 0;
    cc.chars_cap = 0;
    cc.negated = false;
    return cc;
}

fn void cc_ensure_ranges_cap(CharClassData* cc, int needed) @inline {
    if (needed <= cc.ranges_cap) return;

    int new_cap = cc.ranges_cap < REGEX_CHAR_CLASS_INIT_CAP
        ? REGEX_CHAR_CLASS_INIT_CAP
        : cc.ranges_cap;
    while (new_cap < needed) new_cap *= 2;

    CharRange[] next = mem::new_array(CharRange, (usz)new_cap);
    for (int i = 0; i < cc.num_ranges; i++) {
        next[i] = cc.ranges[i];
    }
    if (cc.ranges_cap > 0) mem::free(cc.ranges.ptr);
    cc.ranges = next;
    cc.ranges_cap = new_cap;
}

fn void cc_ensure_chars_cap(CharClassData* cc, int needed) @inline {
    if (needed <= cc.chars_cap) return;

    int new_cap = cc.chars_cap < REGEX_CHAR_CLASS_INIT_CAP
        ? REGEX_CHAR_CLASS_INIT_CAP
        : cc.chars_cap;
    while (new_cap < needed) new_cap *= 2;

    char[] next = mem::new_array(char, (usz)new_cap);
    for (int i = 0; i < cc.num_chars; i++) {
        next[i] = cc.chars[i];
    }
    if (cc.chars_cap > 0) mem::free(cc.chars.ptr);
    cc.chars = next;
    cc.chars_cap = new_cap;
}

fn void cc_add_range(CharClassData* cc, char start, char end) @inline {
    cc_ensure_ranges_cap(cc, cc.num_ranges + 1);
    cc.ranges[cc.num_ranges].start = start;
    cc.ranges[cc.num_ranges].end = end;
    cc.num_ranges++;
}

fn void cc_add_char(CharClassData* cc, char c) @inline {
    cc_ensure_chars_cap(cc, cc.num_chars + 1);
    cc.chars[cc.num_chars++] = c;
}

fn char regex_escape_char(char c) @inline {
    switch (c) {
        case 'n': return '\n';
        case 't': return '\t';
        case 'r': return '\r';
        default: return c;
    }
}

fn bool regex_parse_decimal_i32(RegexTokenizer* self, int* out, bool* has_digits, char[] too_large_error) {
    long value = 0;
    *has_digits = false;
    while (self.peek() >= '0' && self.peek() <= '9') {
        *has_digits = true;
        int digit = (int)(self.advance() - '0');
        if (value > ((long)int.max - (long)digit) / 10) {
            self.set_error(too_large_error);
            return false;
        }
        value = value * 10 + (long)digit;
    }
    *out = (int)value;
    return true;
}

fn bool regex_handle_char_class_escape(CharClassData* cc, char esc, char* out_char) {
    switch (esc) {
        case 'n':
        case 't':
        case 'r':
            *out_char = regex_escape_char(esc);
            return false;
        case 'd':
            cc_add_range(cc, '0', '9');
            return true;
        case 'D':
            cc_add_range(cc, 0, '/');
            cc_add_range(cc, ':', 127);
            return true;
        case 'w':
            cc_add_range(cc, 'a', 'z');
            cc_add_range(cc, 'A', 'Z');
            cc_add_range(cc, '0', '9');
            cc_add_char(cc, '_');
            return true;
        case 'W':
            return true;
        case 's':
            cc_add_char(cc, ' ');
            cc_add_char(cc, '\t');
            cc_add_char(cc, '\n');
            cc_add_char(cc, '\r');
            return true;
        case 'S':
            return true;
        default:
            *out_char = esc;
            return false;
    }
}

fn bool RegexTokenizer.try_parse_char_class_range(RegexTokenizer* self, CharClassData* cc, char start) {
    if (self.peek() != '-' || self.pos + 1 >= (int)self.pattern.len) return false;
    char next_next = self.pattern[self.pos + 1];
    if (next_next == ']') return false;

    self.advance(); // Skip '-'
    char end = self.advance();
    if (end == '\\') end = regex_escape_char(self.advance());
    if (end < start) {
        self.set_error("Invalid character range");
        return true;
    }
    cc_add_range(cc, start, end);
    return true;
}

fn CharClassData* RegexTokenizer.parse_char_class(RegexTokenizer* self) {
    CharClassData* cc = cc_new();

    // Skip opening [
    self.advance();

    // Check for negation
    if (self.peek() == '^') {
        cc.negated = true;
        self.advance();
    }

    while (self.peek() != ']' && self.peek() != 0) {
        char c = self.advance();

        // Handle escape in char class
        if (c == '\\') {
            if (self.peek() == 0) {
                self.set_error("Trailing escape in character class");
                break;
            }
            char esc = self.advance();
            if (regex_handle_char_class_escape(cc, esc, &c)) continue;
        }

        if (self.try_parse_char_class_range(cc, c)) continue;

        cc_add_char(cc, c);
    }

    if (self.peek() == ']') {
        self.advance();
    } else {
        self.set_error("Unclosed character class");
    }

    if (cc.num_ranges == 0 && cc.num_chars == 0) {
        self.set_error("Empty character class");
    }

    return cc;
}
