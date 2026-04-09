module pika;

fn RegexToken RegexTokenizer.parse_bounded_quantifier(RegexTokenizer* self) {
    RegexToken tok;
    tok.type = RE_LBRACE;
    tok.bounds.min = 0;
    tok.bounds.max = 0;
    tok.bounds.possessive = false;

    self.advance(); // Skip {

    // Parse min
    int min = 0;
    bool has_min = false;
    if (!regex_parse_decimal_i32(self, &min, &has_min, "Bounded quantifier lower bound too large")) {
        tok.type = RE_ERROR;
        return tok;
    }
    if (!has_min) {
        self.set_error("Expected number after {");
        tok.type = RE_ERROR;
        return tok;
    }
    tok.bounds.min = min;

    if (self.peek() == '}') {
        // {n} - exact count
        self.advance();
        tok.bounds.max = min;
    } else if (self.peek() == ',') {
        self.advance(); // Skip ,
        if (self.peek() == '}') {
            // {n,} - at least n
            self.advance();
            tok.bounds.max = -1; // Unbounded
        } else {
            // {n,m} - between n and m
            int max = 0;
            bool has_max = false;
            if (!regex_parse_decimal_i32(self, &max, &has_max, "Bounded quantifier upper bound too large")) {
                tok.type = RE_ERROR;
                return tok;
            }
            if (!has_max) {
                self.set_error("Expected upper bound in quantifier");
                tok.type = RE_ERROR;
                return tok;
            }
            tok.bounds.max = max;
            if (tok.bounds.max < tok.bounds.min) {
                self.set_error("Quantifier upper bound < lower bound");
                tok.type = RE_ERROR;
                return tok;
            }
            if (self.peek() == '}') {
                self.advance();
            } else {
                self.set_error("Expected } in bounded quantifier");
                tok.type = RE_ERROR;
                return tok;
            }
        }
    } else {
        self.set_error("Invalid bounded quantifier syntax");
        tok.type = RE_ERROR;
        return tok;
    }

    // Check for possessive +
    if (self.peek() == '+') {
        self.advance();
        tok.bounds.possessive = true;
    }

    return tok;
}

fn RegexToken regex_simple_token(RegexTokenType t) {
    RegexToken tok;
    tok.type = t;
    return tok;
}

fn RegexToken RegexTokenizer.tokenize_possessive(RegexTokenizer* self, RegexTokenType normal, RegexTokenType possessive) {
    self.advance();
    if (self.peek() == '+') {
        self.advance();
        return regex_simple_token(possessive);
    }
    return regex_simple_token(normal);
}

fn CharClassData* regex_digit_class(bool negated) {
    CharClassData* cc = cc_new();
    cc_add_range(cc, '0', '9');
    cc.negated = negated;
    return cc;
}

fn CharClassData* regex_word_class(bool negated) {
    CharClassData* cc = cc_new();
    cc_add_range(cc, 'a', 'z');
    cc_add_range(cc, 'A', 'Z');
    cc_add_range(cc, '0', '9');
    cc_add_char(cc, '_');
    cc.negated = negated;
    return cc;
}

fn CharClassData* regex_space_class(bool negated) {
    CharClassData* cc = cc_new();
    cc_add_char(cc, ' ');
    cc_add_char(cc, '\t');
    cc_add_char(cc, '\n');
    cc_add_char(cc, '\r');
    cc.negated = negated;
    return cc;
}

fn RegexToken regex_class_token(CharClassData* cc) {
    RegexToken tok;
    tok.type = RE_CHAR_CLASS;
    tok.cclass = cc;
    return tok;
}

fn RegexToken RegexTokenizer.tokenize_group(RegexTokenizer* self) {
    self.advance(); // Skip (
    if (self.peek() != '?') {
        return regex_simple_token(RE_LPAREN);
    }

    self.advance(); // Skip ?
    char next = self.peek();
    switch (next) {
        case ':':
            self.advance();
            return regex_simple_token(RE_NONCAP);
        case '=':
            self.advance();
            return regex_simple_token(RE_LOOKAHEAD_POS);
        case '!':
            self.advance();
            return regex_simple_token(RE_LOOKAHEAD_NEG);
        default:
            self.set_error("Unknown group type");
            return regex_simple_token(RE_ERROR);
    }
}

fn RegexToken RegexTokenizer.tokenize_escape(RegexTokenizer* self) {
    self.advance(); // Skip '\'
    char c = self.advance();
    if (c == 0) {
        self.set_error("Trailing escape");
        return regex_simple_token(RE_ERROR);
    }

    switch (c) {
        case 'n':
            RegexToken tok = regex_simple_token(RE_LITERAL);
            tok.ch = '\n';
            return tok;
        case 't':
            RegexToken tok = regex_simple_token(RE_LITERAL);
            tok.ch = '\t';
            return tok;
        case 'r':
            RegexToken tok = regex_simple_token(RE_LITERAL);
            tok.ch = '\r';
            return tok;
        case 'd':
            return regex_class_token(regex_digit_class(false));
        case 'D':
            return regex_class_token(regex_digit_class(true));
        case 'w':
            return regex_class_token(regex_word_class(false));
        case 'W':
            return regex_class_token(regex_word_class(true));
        case 's':
            return regex_class_token(regex_space_class(false));
        case 'S':
            return regex_class_token(regex_space_class(true));
        default:
            RegexToken tok = regex_simple_token(RE_LITERAL);
            tok.ch = c;
            return tok;
    }
}

fn RegexToken RegexTokenizer.next_token(RegexTokenizer* self) {
    if (self.pos >= (int)self.pattern.len) {
        return regex_simple_token(RE_END);
    }

    char c = self.peek();

    switch (c) {
        case '[':
            return regex_class_token(self.parse_char_class());

        case '.':
            self.advance();
            return regex_simple_token(RE_DOT);

        case '*':
            return self.tokenize_possessive(RE_STAR, RE_STAR_POSS);

        case '+':
            return self.tokenize_possessive(RE_PLUS, RE_PLUS_POSS);

        case '?':
            return self.tokenize_possessive(RE_QUESTION, RE_QUEST_POSS);

        case '|':
            self.advance();
            return regex_simple_token(RE_PIPE);

        case '(':
            return self.tokenize_group();

        case ')':
            self.advance();
            return regex_simple_token(RE_RPAREN);

        case '^':
            self.advance();
            return regex_simple_token(RE_ANCHOR_START);

        case '$':
            self.advance();
            return regex_simple_token(RE_ANCHOR_END);

        case '{':
            return self.parse_bounded_quantifier();

        case '\\':
            return self.tokenize_escape();

        default:
            self.advance();
            RegexToken tok = regex_simple_token(RE_LITERAL);
            tok.ch = c;
            return tok;
    }
}
