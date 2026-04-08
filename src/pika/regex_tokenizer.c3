module pika;

import std::core::mem;
import std::io;

// ============================================================
// Pika Regex Engine
// Compiles regex patterns to PikaParser grammars
//
// Supported Features:
//   - Literals: abc
//   - Character classes: [abc], [a-z], [^abc] (negated)
//   - Shorthand classes: \d, \D, \w, \W, \s, \S
//   - Any character: .
//   - Quantifiers: *, +, ?
//   - Possessive quantifiers: *+, ++, ?+ (deterministic Pika semantics)
//   - Bounded quantifiers: {n}, {n,}, {n,m}
//   - Alternation: a|b
//   - Grouping: (ab)+
//   - Non-capturing groups: (?:...)
//   - Lookahead: (?=...) positive, (?!...) negative
//   - Anchors: ^ (start), $ (end)
//   - Escapes: \n, \t, \r, \\, \., etc.
// ============================================================

// ============================================================
// Regex Token Types
// ============================================================

enum RegexTokenType : char {
    RE_LITERAL,        // Single character literal
    RE_CHAR_CLASS,     // Character class [abc] or [a-z]
    RE_DOT,            // Any character
    RE_STAR,           // Zero or more *
    RE_PLUS,           // One or more +
    RE_QUESTION,       // Optional ?
    RE_STAR_POSS,      // Possessive zero or more *+
    RE_PLUS_POSS,      // Possessive one or more ++
    RE_QUEST_POSS,     // Possessive optional ?+
    RE_PIPE,           // Alternation |
    RE_LPAREN,         // Left paren (
    RE_RPAREN,         // Right paren )
    RE_NONCAP,         // Non-capturing group (?:
    RE_LOOKAHEAD_POS,  // Positive lookahead (?=
    RE_LOOKAHEAD_NEG,  // Negative lookahead (?!
    RE_LBRACE,         // Left brace { for quantifiers
    RE_ANCHOR_START,   // ^ anchor
    RE_ANCHOR_END,     // $ anchor
    RE_END,            // End of pattern
    RE_ERROR,          // Error token
}

// Character class range
struct CharRange {
    char start;
    char end;
}

// Character class data
struct CharClassData {
    CharRange[] ranges;     // Start-end pairs for ranges
    int num_ranges;
    int ranges_cap;
    char[] chars;           // Individual characters
    int num_chars;
    int chars_cap;
    bool negated;           // [^...] negation
}

// Bounded quantifier data
struct BoundsData {
    int min;
    int max;        // -1 for unbounded {n,}
    bool possessive;
}

// Regex token
struct RegexToken {
    RegexTokenType type;
    char ch;                 // For RE_LITERAL
    CharClassData* cclass;   // For RE_CHAR_CLASS
    BoundsData bounds;       // For RE_LBRACE
}

// ============================================================
// Regex Tokenizer
// ============================================================

struct RegexTokenizer {
    char[] pattern;
    int pos;
    char[256] error;
    bool has_error;
}

const int REGEX_CHAR_CLASS_INIT_CAP = 8;
const int REGEX_QUANTIFIER_COMPILE_PARTS_MAX = 131072;

fn RegexTokenizer regex_tokenizer_new(char[] pattern) {
    RegexTokenizer t;
    t.pattern = pattern;
    t.pos = 0;
    t.has_error = false;
    return t;
}

fn char RegexTokenizer.peek(RegexTokenizer* self) {
    if (self.pos >= (int)self.pattern.len) return 0;
    return self.pattern[self.pos];
}

fn char RegexTokenizer.advance(RegexTokenizer* self) {
    if (self.pos >= (int)self.pattern.len) return 0;
    return self.pattern[self.pos++];
}

fn void RegexTokenizer.set_error(RegexTokenizer* self, char[] msg) {
    self.has_error = true;
    usz len = msg.len;
    if (len > 255) len = 255;
    for (usz i = 0; i < len; i++) {
        self.error[i] = msg[i];
    }
    self.error[len] = 0;
}

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
