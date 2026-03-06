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

// ============================================================
// Compiled Regex Structure
// ============================================================

struct CompiledRegex {
    Grammar grammar;
    int start_clause;
    bool anchored_start;
    bool anchored_end;
    bool valid;
    char[256] error;
}

// ============================================================
// Regex Compiler - Converts regex to PikaParser grammar
// ============================================================

// SCAN function for character class matching
fn int scan_char_in_class(CharClassData* cc, char[] view) {
    if (view.len == 0) return 0;
    char c = view[0];

    bool matched = false;

    // Check ranges
    for (int i = 0; i < cc.num_ranges; i++) {
        if (c >= cc.ranges[i].start && c <= cc.ranges[i].end) {
            matched = true;
            break;
        }
    }

    // Check individual chars
    if (!matched) {
        for (int i = 0; i < cc.num_chars; i++) {
            if (c == cc.chars[i]) {
                matched = true;
                break;
            }
        }
    }

    // Handle negation
    if (cc.negated) {
        matched = !matched;
    }

    return matched ? 1 : 0;
}

// SCAN function for any character (.)
fn int scan_any_char(char[] view) {
    if (view.len == 0) return 0;
    // Match any char except newline (standard regex behavior)
    if (view[0] == '\n') return 0;
    return 1;
}

// SCAN function for single literal char
fn int scan_literal_char(char expected, char[] view) {
    if (view.len == 0) return 0;
    return view[0] == expected ? 1 : 0;
}

// ============================================================
// Regex Match Result
// ============================================================

struct RegexMatch {
    bool matched;
    int start;
    int end;
    char[] view;
}

fn RegexMatch no_match() {
    RegexMatch m;
    m.matched = false;
    m.start = 0;
    m.end = 0;
    m.view = "";
    return m;
}

fn RegexMatch regex_mk_match(int start, int end, char[] input) {
    RegexMatch m;
    m.matched = true;
    m.start = start;
    m.end = end;
    m.view = "";  // Initialize to empty
    // C3 slices [a..b] are INCLUSIVE on both ends, giving (b-a+1) elements
    // So [start..end] already includes index 'end'
    if (end >= start && start >= 0 && end < (int)input.len) {
        m.view = input[start..end];
    }
    return m;
}

// ============================================================
// Simple Regex Matching (without full grammar compilation)
// Uses direct interpretation for common patterns
// ============================================================

struct RegexState {
    char[] pattern;
    char[] input;
    int pat_pos;
    int inp_pos;
}

// Match a single regex element at current position
fn int match_element(RegexTokenizer* tok, RegexToken* token, char[] input, int pos) {
    if (pos >= (int)input.len) return -1;

    switch (token.type) {
        case RE_LITERAL:
            if (input[pos] == token.ch) return 1;
            return -1;

        case RE_DOT:
            if (input[pos] != '\n') return 1;
            return -1;

        case RE_CHAR_CLASS:
            return scan_char_in_class(token.cclass, input[pos..]);

        default:
            return -1;
    }
}

fn bool regex_is_atom(RegexTokenType t) @inline {
    switch (t) {
        case RE_LITERAL:
        case RE_DOT:
        case RE_CHAR_CLASS:
            return true;
        default:
            return false;
    }
}

fn bool regex_extract_quantifier(RegexTokenizer* tok, int* min_count, int* max_count) {
    int saved_pos = tok.pos;
    RegexToken qtok = tok.next_token();
    switch (qtok.type) {
        case RE_STAR:
        case RE_STAR_POSS:
            *min_count = 0;
            *max_count = -1;
            return true;

        case RE_PLUS:
        case RE_PLUS_POSS:
            *min_count = 1;
            *max_count = -1;
            return true;

        case RE_QUESTION:
        case RE_QUEST_POSS:
            *min_count = 0;
            *max_count = 1;
            return true;

        case RE_LBRACE:
            *min_count = qtok.bounds.min;
            *max_count = qtok.bounds.max;
            return true;

        default:
            tok.pos = saved_pos;
            return false;
    }
}

fn bool regex_match_repeated_atom(RegexTokenizer* tok, RegexToken* atom, int min_count, int max_count, char[] input, int* inp_pos) {
    int pos = *inp_pos;
    int count = 0;

    // -1 means unbounded.
    int limit = max_count;
    if (limit < 0) limit = (int)input.len - pos;

    while (count < limit && pos < (int)input.len) {
        int matched = match_element(tok, atom, input, pos);
        if (matched <= 0) break;
        pos += matched;
        count++;
    }

    if (count < min_count) return false;
    *inp_pos = pos;
    return true;
}

// ============================================================
// Regex to PikaParser Compiler
// ============================================================

fn int regex_scan_char_class_ctx(void* ctx, char[] view) {
    return scan_char_in_class((CharClassData*)ctx, view);
}

// Dot (match any except newline)
fn int regex_scan_dot(char[] view) {
    if (view.len == 0) return 0;
    if (view[0] == '\n') return 0;
    return 1;
}

// Compiler state
struct RegexCompiler {
    RuleBuilder rb;
    RegexTokenizer* tok;
    int rule_counter;
}

fn RegexCompiler regex_compiler_new(RegexTokenizer* tok) {
    RegexCompiler rc;
    rc.rb = rb_new();
    rc.tok = tok;
    rc.rule_counter = 0;
    return rc;
}

// Generate unique rule name
fn String regex_gen_name(RegexCompiler* rc, char[] prefix) {
    int counter = rc.rule_counter;
    rc.rule_counter++;

    int digits = 1;
    int tmp = counter;
    while (tmp >= 10) {
        tmp /= 10;
        digits++;
    }

    usz plen = prefix.len;
    usz len = plen + 1 + (usz)digits;
    char[] name_mem = mem::new_array(char, len + 1);
    char[] name = name_mem[:len];
    for (usz i = 0; i < plen; i++) {
        name[i] = prefix[i];
    }
    name[plen] = '_';

    int pos = (int)len - 1;
    int n = counter;
    while (pos >= (int)plen + 1) {
        name[(usz)pos] = (char)('0' + (n % 10));
        n /= 10;
        pos--;
    }
    name_mem[len] = 0;

    return (String)name;
}

fn bool regex_is_quantifier(RegexTokenType t) @inline {
    switch (t) {
        case RE_STAR:
        case RE_PLUS:
        case RE_QUESTION:
        case RE_STAR_POSS:
        case RE_PLUS_POSS:
        case RE_QUEST_POSS:
        case RE_LBRACE:
            return true;
        default:
            return false;
    }
}

fn char[] regex_trailing_token_message(RegexTokenType t) {
    if (regex_is_quantifier(t)) return "quantifier without target atom";
    switch (t) {
        case RE_RPAREN: return "unmatched closing parenthesis";
        case RE_LPAREN:
        case RE_NONCAP:
        case RE_LOOKAHEAD_POS:
        case RE_LOOKAHEAD_NEG:
            return "unclosed group";
        case RE_ERROR: return "invalid regex token";
        default: return "unexpected trailing token in regex pattern";
    }
}

fn char[] regex_tokenizer_error_view(RegexTokenizer* tok) {
    usz tok_len = 0;
    while (tok_len < 256 && tok.error[tok_len] != 0) tok_len++;
    return tok.error[:tok_len];
}

fn void regex_parts_push(int[]* parts, int* part_count, int* part_cap, int value) {
    if (*part_count >= *part_cap) {
        int new_cap = *part_cap < 8 ? 8 : (*part_cap * 2);
        int[] next = mem::new_array(int, (usz)new_cap);
        for (int i = 0; i < *part_count; i++) {
            next[i] = (*parts)[i];
        }
        if (*part_cap > 0) mem::free((*parts).ptr);
        *parts = next;
        *part_cap = new_cap;
    }
    (*parts)[*part_count] = value;
    (*part_count)++;
}

// Compile an atom (base element: literal, char class, dot, group)
fn int compile_regex_atom(RegexCompiler* rc) {
    int saved_pos = rc.tok.pos;
    RegexToken tok = rc.tok.next_token();

    switch (tok.type) {
        case RE_LITERAL:
            return rc.rb.add(regex_gen_name(rc, "lit"), mk_token(tok.ch));

        case RE_DOT:
            return rc.rb.add(regex_gen_name(rc, "dot"), mk_scan(&regex_scan_dot));

        case RE_CHAR_CLASS:
            return rc.rb.add(regex_gen_name(rc, "cls"), mk_scan_ctx(&regex_scan_char_class_ctx, (void*)tok.cclass));

        case RE_LPAREN:
            // Group: parse inner expression
            int inner = compile_regex_alternation(rc);
            // Expect closing paren
            RegexToken close = rc.tok.next_token();
            if (close.type != RE_RPAREN) {
                rc.tok.set_error("Unclosed group");
                return rc.rb.add("_err", mk_fail());
            }
            return inner;

        case RE_NONCAP:
            // Non-capturing group (?:...): same as regular group for Pika
            int inner2 = compile_regex_alternation(rc);
            RegexToken close2 = rc.tok.next_token();
            if (close2.type != RE_RPAREN) {
                rc.tok.set_error("Unclosed group");
                return rc.rb.add("_err", mk_fail());
            }
            return inner2;

        case RE_LOOKAHEAD_POS:
            // Positive lookahead (?=...)
            int inner3 = compile_regex_alternation(rc);
            RegexToken close3 = rc.tok.next_token();
            if (close3.type != RE_RPAREN) {
                rc.tok.set_error("Unclosed group");
                return rc.rb.add("_err", mk_fail());
            }
            return rc.rb.add(regex_gen_name(rc, "la"), mk_followed_by(inner3));

        case RE_LOOKAHEAD_NEG:
            // Negative lookahead (?!...)
            int inner4 = compile_regex_alternation(rc);
            RegexToken close4 = rc.tok.next_token();
            if (close4.type != RE_RPAREN) {
                rc.tok.set_error("Unclosed group");
                return rc.rb.add("_err", mk_fail());
            }
            return rc.rb.add(regex_gen_name(rc, "nla"), mk_not_followed_by(inner4));

        case RE_ANCHOR_END:
            return rc.rb.add("_end", mk_end_of_input());

        case RE_ERROR:
            return rc.rb.add(regex_gen_name(rc, "err"), mk_fail());

        default:
            // Push back token for caller to handle
            rc.tok.pos = saved_pos;
            return -1;  // No atom found
    }
}

// Compile quantified expression (atom with optional quantifier)
fn int compile_regex_quantified(RegexCompiler* rc) {
    int atom = compile_regex_atom(rc);
    if (atom < 0) return -1;

    // Check for quantifier
    int saved_pos = rc.tok.pos;
    RegexToken qtok = rc.tok.next_token();

    switch (qtok.type) {
        case RE_STAR:
        case RE_STAR_POSS:
            return rc.rb.add(regex_gen_name(rc, "star"), mk_many(atom));

        case RE_PLUS:
        case RE_PLUS_POSS:
            return rc.rb.add(regex_gen_name(rc, "plus"), mk_some(atom));

        case RE_QUESTION:
        case RE_QUEST_POSS:
            int eps = rc.rb.add("_eps", mk_epsilon());
            int[] alts = make_children_2(atom, eps);
            return rc.rb.add(regex_gen_name(rc, "opt"), mk_first(alts));

        case RE_LBRACE:
            // Bounded quantifier {n} or {n,} or {n,m}
            int min = qtok.bounds.min;
            int max = qtok.bounds.max;
            int required_parts = max < 0 ? min : max;
            if (max < 0) {
                if (min == int.max) {
                    rc.tok.set_error("Bounded quantifier too large for compiled regex");
                    return rc.rb.add(regex_gen_name(rc, "err"), mk_fail());
                }
                required_parts = min + 1; // extra mk_many(atom) tail
            }
            if (required_parts > REGEX_QUANTIFIER_COMPILE_PARTS_MAX) {
                rc.tok.set_error("Bounded quantifier too large for compiled regex");
                return rc.rb.add(regex_gen_name(rc, "err"), mk_fail());
            }

            int[] parts;
            int part_count = 0;
            int part_cap = 0;

            // Required part (min times)
            for (int i = 0; i < min; i++) {
                regex_parts_push(&parts, &part_count, &part_cap, atom);
            }

            if (max < 0) {
                int rest = rc.rb.add(regex_gen_name(rc, "many"), mk_many(atom));
                regex_parts_push(&parts, &part_count, &part_cap, rest);
            } else if (max > min) {
                // Optional part (min to max)
                int eps = rc.rb.add("_eps", mk_epsilon());
                int[] opt_alts = make_children_2(atom, eps);
                int opt = rc.rb.add(regex_gen_name(rc, "bnd"), mk_first(opt_alts));
                for (int i = min; i < max; i++) {
                    regex_parts_push(&parts, &part_count, &part_cap, opt);
                }
            }

            if (part_count == 0) {
                if (part_cap > 0) mem::free(parts.ptr);
                return rc.rb.add("_eps", mk_epsilon());
            }
            if (part_count == 1) {
                int one = parts[0];
                if (part_cap > 0) mem::free(parts.ptr);
                return one;
            }
            int[] children = mem::new_array(int, (usz)part_count);
            for (int i = 0; i < part_count; i++) {
                children[i] = parts[i];
            }
            if (part_cap > 0) mem::free(parts.ptr);
            return rc.rb.add(regex_gen_name(rc, "seq"), mk_seq(children));

        default:
            // Not a quantifier, push back
            rc.tok.pos = saved_pos;
            return atom;
    }
}

// Compile sequence (concatenation of quantified elements)
fn int compile_regex_sequence(RegexCompiler* rc) {
    int[] parts;
    int part_count = 0;
    int part_cap = 0;

    while (true) {
        int saved_pos = rc.tok.pos;
        int elem = compile_regex_quantified(rc);
        if (elem < 0) {
            rc.tok.pos = saved_pos;
            break;
        }
        if (part_count >= part_cap) {
            int new_cap = part_cap < 16 ? 16 : part_cap * 2;
            int[] new_parts = mem::new_array(int, (usz)new_cap);
            for (int i = 0; i < part_count; i++) {
                new_parts[i] = parts[i];
            }
            if (part_cap > 0) mem::free(parts.ptr);
            parts = new_parts;
            part_cap = new_cap;
        }
        parts[part_count++] = elem;
    }

    if (part_count == 0) {
        if (part_cap > 0) mem::free(parts.ptr);
        return rc.rb.add("_eps", mk_epsilon());
    }
    if (part_count == 1) {
        int one = parts[0];
        if (part_cap > 0) mem::free(parts.ptr);
        return one;
    }

    int[] children = mem::new_array(int, (usz)part_count);
    for (int i = 0; i < part_count; i++) {
        children[i] = parts[i];
    }
    if (part_cap > 0) mem::free(parts.ptr);
    return rc.rb.add(regex_gen_name(rc, "seq"), mk_seq(children));
}

// Compile alternation (top level: alternatives separated by |)
fn int compile_regex_alternation(RegexCompiler* rc) {
    int[] alts;
    int alt_count = 0;
    int alt_cap = 0;

    // First alternative
    int first_alt = compile_regex_sequence(rc);
    alt_cap = 8;
    alts = mem::new_array(int, (usz)alt_cap);
    alts[0] = first_alt;
    alt_count = 1;

    // Additional alternatives
    while (true) {
        int saved_pos = rc.tok.pos;
        RegexToken tok = rc.tok.next_token();
        if (tok.type != RE_PIPE) {
            rc.tok.pos = saved_pos;
            break;
        }
        if (alt_count >= alt_cap) {
            int new_cap = alt_cap * 2;
            int[] new_alts = mem::new_array(int, (usz)new_cap);
            for (int i = 0; i < alt_count; i++) {
                new_alts[i] = alts[i];
            }
            mem::free(alts.ptr);
            alts = new_alts;
            alt_cap = new_cap;
        }
        alts[alt_count++] = compile_regex_sequence(rc);
    }

    if (alt_count == 1) {
        int one = alts[0];
        mem::free(alts.ptr);
        return one;
    }

    int[] children = mem::new_array(int, (usz)alt_count);
    for (int i = 0; i < alt_count; i++) {
        children[i] = alts[i];
    }
    mem::free(alts.ptr);
    return rc.rb.add(regex_gen_name(rc, "alt"), mk_first(children));
}

// ============================================================
// High-Level Regex API
// ============================================================

const int REGEX_CACHE_INIT_CAP = 32;
const int REGEX_CACHE_MAX_ENTRIES = 128;

struct RegexCacheEntry {
    char[] pattern;
    CompiledRegex* rx;
    usz last_used;
}

RegexCacheEntry[] g_regex_cache;
int g_regex_cache_len = 0;
int g_regex_cache_cap = 0;
usz g_regex_cache_tick = 0;
usz g_regex_cache_hits = 0;
usz g_regex_cache_misses = 0;
usz g_regex_cache_evictions = 0;
usz g_regex_fallback_uses = 0;

fn void regex_cache_ensure_init() {
    if (g_regex_cache_cap > 0) return;
    g_regex_cache_cap = REGEX_CACHE_INIT_CAP;
    g_regex_cache = mem::new_array(RegexCacheEntry, (usz)g_regex_cache_cap);
}

fn void regex_cache_grow() {
    if (g_regex_cache_cap >= REGEX_CACHE_MAX_ENTRIES) return;
    int new_cap = g_regex_cache_cap * 2;
    if (new_cap > REGEX_CACHE_MAX_ENTRIES) new_cap = REGEX_CACHE_MAX_ENTRIES;
    if (new_cap <= g_regex_cache_cap) return;
    RegexCacheEntry[] next = mem::new_array(RegexCacheEntry, (usz)new_cap);
    for (int i = 0; i < g_regex_cache_len; i++) {
        next[i] = g_regex_cache[i];
    }
    if (g_regex_cache_cap > 0) mem::free(g_regex_cache.ptr);
    g_regex_cache = next;
    g_regex_cache_cap = new_cap;
}

fn void regex_cache_reset() {
    for (int i = 0; i < g_regex_cache_len; i++) {
        if (g_regex_cache[i].pattern.len > 0) mem::free(g_regex_cache[i].pattern.ptr);
        regex_free(g_regex_cache[i].rx);
    }
    if (g_regex_cache_cap > 0) mem::free(g_regex_cache.ptr);

    RegexCacheEntry[] empty;
    g_regex_cache = empty;
    g_regex_cache_len = 0;
    g_regex_cache_cap = 0;
    g_regex_cache_tick = 0;
    g_regex_cache_hits = 0;
    g_regex_cache_misses = 0;
    g_regex_cache_evictions = 0;
    g_regex_fallback_uses = 0;
}

fn bool regex_cache_pattern_eq(char[] a, char[] b) {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

fn void regex_cache_touch(int idx) {
    g_regex_cache_tick++;
    g_regex_cache[idx].last_used = g_regex_cache_tick;
}

fn int regex_cache_find(char[] pattern) {
    for (int i = 0; i < g_regex_cache_len; i++) {
        if (regex_cache_pattern_eq(g_regex_cache[i].pattern, pattern)) {
            g_regex_cache_hits++;
            regex_cache_touch(i);
            return i;
        }
    }
    g_regex_cache_misses++;
    return -1;
}

fn int regex_cache_find_lru_index() {
    if (g_regex_cache_len <= 0) return -1;
    int lru_idx = 0;
    usz oldest = g_regex_cache[0].last_used;
    for (int i = 1; i < g_regex_cache_len; i++) {
        if (g_regex_cache[i].last_used < oldest) {
            oldest = g_regex_cache[i].last_used;
            lru_idx = i;
        }
    }
    return lru_idx;
}

fn char[] regex_cache_copy_pattern(char[] pattern) {
    if (pattern.len == 0) {
        char[] empty;
        return empty;
    }
    char[] stored = mem::new_array(char, pattern.len);
    for (usz i = 0; i < pattern.len; i++) {
        stored[i] = pattern[i];
    }
    return stored;
}

fn void regex_set_compiled_error(CompiledRegex* rx, char[] msg) {
    rx.valid = false;
    usz len = msg.len;
    if (len > 255) len = 255;
    for (usz i = 0; i < len; i++) {
        rx.error[i] = msg[i];
    }
    rx.error[len] = 0;
}

// Compile a regex pattern to PikaParser grammar
fn CompiledRegex* regex_compile(char[] pattern) {
    CompiledRegex* rx = mem::new(CompiledRegex);
    rx.valid = true;
    rx.start_clause = -1;
    rx.anchored_start = false;
    rx.anchored_end = false;

    RegexTokenizer tok = regex_tokenizer_new(pattern);

    // Check for start anchor
    RegexToken first = tok.next_token();
    if (first.type == RE_ANCHOR_START) {
        rx.anchored_start = true;
    } else {
        tok.pos = 0; // Reset
    }

    if (tok.has_error) {
        regex_set_compiled_error(rx, regex_tokenizer_error_view(&tok));
        return rx;
    }

    // Compile the regex to grammar rules
    RegexCompiler rc = regex_compiler_new(&tok);
    int top_rule = compile_regex_alternation(&rc);
    if (tok.has_error) {
        regex_set_compiled_error(rx, regex_tokenizer_error_view(&tok));
        return rx;
    }

    // Add end-of-input if there's an end anchor
    int saved_pos = tok.pos;
    first = tok.next_token();
    if (first.type == RE_ANCHOR_END) {
        rx.anchored_end = true;
        int end_rule = rc.rb.add("_end", mk_end_of_input());
        int[] seq = make_children_2(top_rule, end_rule);
        top_rule = rc.rb.add("_top", mk_seq(seq));
    } else {
        tok.pos = saved_pos;
    }

    RegexToken end_tok = tok.next_token();
    if (tok.has_error) {
        regex_set_compiled_error(rx, regex_tokenizer_error_view(&tok));
        return rx;
    }
    if (end_tok.type != RE_END) {
        regex_set_compiled_error(rx, regex_trailing_token_message(end_tok.type));
        return rx;
    }

    // Use a stable exported top rule name for traversal/lookups.
    top_rule = rc.rb.add("_re_top", mk_tie(top_rule));

    // Build the grammar
    if (rc.rb.len > 0) {
        String[] starts = mem::new_array(String, 1);
        starts[0] = "_re_top";
        rx.grammar = make_grammar(starts, rc.rb.to_array());
        rx.start_clause = find_name_index(&rx.grammar, "_re_top");
        if (rx.start_clause < 0) {
            rx.valid = false;
        }
    }

    return rx;
}

// Free compiled regex
fn void regex_free(CompiledRegex* rx) {
    if (rx != null) {
        mem::free(rx);
    }
}

fn CompiledRegex* regex_get_compiled_cached(char[] pattern) {
    regex_cache_ensure_init();

    int existing = regex_cache_find(pattern);
    if (existing >= 0) return g_regex_cache[existing].rx;

    CompiledRegex* rx = regex_compile(pattern);
    if (rx == null || !rx.valid || rx.start_clause < 0 || rx.grammar.num_clauses <= 0 || rx.grammar.names.len == 0) {
        regex_free(rx);
        return null;
    }

    if (g_regex_cache_len < REGEX_CACHE_MAX_ENTRIES) {
        if (g_regex_cache_len >= g_regex_cache_cap) regex_cache_grow();
        if (g_regex_cache_len < g_regex_cache_cap) {
            g_regex_cache[g_regex_cache_len].pattern = regex_cache_copy_pattern(pattern);
            g_regex_cache[g_regex_cache_len].rx = rx;
            regex_cache_touch(g_regex_cache_len);
            g_regex_cache_len++;
            return rx;
        }
    }

    int evict_idx = regex_cache_find_lru_index();
    if (evict_idx >= 0) {
        if (g_regex_cache[evict_idx].pattern.len > 0) mem::free(g_regex_cache[evict_idx].pattern.ptr);
        regex_free(g_regex_cache[evict_idx].rx);
        g_regex_cache[evict_idx].pattern = regex_cache_copy_pattern(pattern);
        g_regex_cache[evict_idx].rx = rx;
        regex_cache_touch(evict_idx);
        g_regex_cache_evictions++;
        return rx;
    }

    // Conservative fallback: if cache bookkeeping is inconsistent, do not cache this entry.
    regex_free(rx);
    return null;
}

fn void regex_release_parser_state(ParserState* st) {
    st.q.free();
    if (st.matches_cap > 0) mem::free(st.matches.ptr);
    if (st.submatches_cap > 0) mem::free(st.submatches.ptr);
    st.matches_cap = 0;
    st.submatches_cap = 0;
}

// Simple pattern match (greedy, no backtracking - Pika style)
fn RegexMatch regex_match_simple(char[] pattern, char[] input, int start_pos) {
    RegexTokenizer tok = regex_tokenizer_new(pattern);

    int inp_pos = start_pos;
    int match_start = start_pos;

    // Check for start anchor
    RegexToken token = tok.next_token();
    if (token.type == RE_ANCHOR_START) {
        if (start_pos != 0) return no_match();
        token = tok.next_token();
    }

    while (token.type != RE_END && token.type != RE_ERROR) {
        if (regex_is_atom(token.type)) {
            int min_count = 1;
            int max_count = 1;
            regex_extract_quantifier(&tok, &min_count, &max_count);
            if (!regex_match_repeated_atom(&tok, &token, min_count, max_count, input, &inp_pos)) {
                return no_match();
            }
            token = tok.next_token();
            continue;
        }

        switch (token.type) {
            case RE_ANCHOR_END:
                if (inp_pos != (int)input.len) return no_match();
                token = tok.next_token();
                continue;

            default:
                return no_match();
        }
    }

    if (tok.has_error) return no_match();
    return regex_mk_match(match_start, inp_pos - 1, input);
}

fn RegexMatch regex_search_simple_fallback(char[] pattern, char[] input) {
    // Try matching at each position
    for (int i = 0; i < (int)input.len; i++) {
        RegexMatch m = regex_match_simple(pattern, input, i);
        if (m.matched) return m;
    }
    return no_match();
}

fn RegexMatch regex_fullmatch_simple_fallback(char[] pattern, char[] input) {
    RegexMatch m = regex_match_simple(pattern, input, 0);
    if (m.matched && m.start == 0 && m.end == (int)input.len - 1) {
        return m;
    }
    return no_match();
}

fn void regex_matches_push(RegexMatch[]* matches, int* count, int* cap, RegexMatch match) {
    if (*count >= *cap) {
        int new_cap = *cap < 8 ? 8 : *cap * 2;
        RegexMatch[] next = mem::new_array(RegexMatch, (usz)new_cap);
        for (int i = 0; i < *count; i++) {
            next[i] = (*matches)[i];
        }
        if (*cap > 0) mem::free((*matches).ptr);
        *matches = next;
        *cap = new_cap;
    }
    (*matches)[*count] = match;
    (*count)++;
}

fn RegexMatch[] regex_find_all_simple_fallback(char[] pattern, char[] input, int* count) {
    RegexMatch[] matches;
    int cap = 0;
    int match_count = 0;
    *count = 0;

    int pos = 0;
    while (pos < (int)input.len) {
        RegexMatch m = regex_match_simple(pattern, input, pos);
        if (m.matched) {
            regex_matches_push(&matches, &match_count, &cap, m);
            pos = m.end + 1;
            if (pos <= m.start) pos = m.start + 1; // Avoid infinite loop on empty match
        } else {
            pos++;
        }
    }
    *count = match_count;
    return match_count > 0 ? matches[:(usz)match_count] : matches;
}

fn RegexMatch regex_search_compiled(char[] pattern, char[] input, bool* usable) {
    *usable = false;
    CompiledRegex* rx = regex_get_compiled_cached(pattern);
    if (rx == null) return no_match();

    *usable = true;
    if (input.len == 0) return no_match();

    ParserState st = parse_simple(&rx.grammar, input);
    defer regex_release_parser_state(&st);

    int start = rx.anchored_start ? 0 : 0;
    int end = rx.anchored_start ? 0 : (int)input.len - 1;
    for (int i = start; i <= end; i++) {
        int mid = lookup_best_match_id(i, rx.start_clause, &st);
        if (mid >= 0) {
            Match* m = &st.matches[mid];
            return regex_mk_match(m.first, m.last, input);
        }
        if (rx.anchored_start) break;
    }
    return no_match();
}

fn RegexMatch regex_fullmatch_compiled(char[] pattern, char[] input, bool* usable) {
    *usable = false;
    CompiledRegex* rx = regex_get_compiled_cached(pattern);
    if (rx == null) return no_match();

    *usable = true;
    ParserState st = parse_simple(&rx.grammar, input);
    defer regex_release_parser_state(&st);

    int mid = lookup_best_match_id(0, rx.start_clause, &st);
    if (mid < 0) return no_match();

    Match* m = &st.matches[mid];
    if (m.first == 0 && m.last == (int)input.len - 1) {
        return regex_mk_match(m.first, m.last, input);
    }
    return no_match();
}

fn RegexMatch[] regex_find_all_compiled(char[] pattern, char[] input, int* count, bool* usable) {
    RegexMatch[] matches;
    int cap = 0;
    int match_count = 0;
    *count = 0;
    *usable = false;

    CompiledRegex* rx = regex_get_compiled_cached(pattern);
    if (rx == null) return matches;

    *usable = true;
    if (input.len == 0) return matches;

    ParserState st = parse_simple(&rx.grammar, input);
    defer regex_release_parser_state(&st);

    int pos = 0;
    while (pos < (int)input.len) {
        bool found = false;
        int start = rx.anchored_start ? 0 : pos;
        int end = rx.anchored_start ? 0 : (int)input.len - 1;

        for (int i = start; i <= end; i++) {
            if (i < pos) continue;
            int mid = lookup_best_match_id(i, rx.start_clause, &st);
            if (mid < 0) {
                if (rx.anchored_start) break;
                continue;
            }

            Match* mm = &st.matches[mid];
            RegexMatch m = regex_mk_match(mm.first, mm.last, input);
            regex_matches_push(&matches, &match_count, &cap, m);
            pos = m.end + 1;
            if (pos <= m.start) pos = m.start + 1; // Avoid infinite loop on empty match
            found = true;
            break;
        }

        if (!found) break;
        if (rx.anchored_start && pos > 0) break;
    }
    *count = match_count;
    return match_count > 0 ? matches[:(usz)match_count] : matches;
}

// Search for pattern anywhere in input
fn RegexMatch regex_search(char[] pattern, char[] input) {
    bool usable = false;
    RegexMatch m = regex_search_compiled(pattern, input, &usable);
    if (usable) return m;
    g_regex_fallback_uses++;
    return regex_search_simple_fallback(pattern, input);
}

// Match pattern against entire input
fn RegexMatch regex_fullmatch(char[] pattern, char[] input) {
    bool usable = false;
    RegexMatch m = regex_fullmatch_compiled(pattern, input, &usable);
    if (usable) return m;
    g_regex_fallback_uses++;
    return regex_fullmatch_simple_fallback(pattern, input);
}

// Find all non-overlapping matches
fn RegexMatch[] regex_find_all(char[] pattern, char[] input, int* count) {
    bool usable = false;
    RegexMatch[] matches = regex_find_all_compiled(pattern, input, count, &usable);
    if (usable) return matches;
    g_regex_fallback_uses++;
    return regex_find_all_simple_fallback(pattern, input, count);
}
