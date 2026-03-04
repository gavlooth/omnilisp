module pika;

import std::core::mem;
import std::io;

// ============================================================
// PEG-Based Regex Engine
// Compiles regex patterns to PikaParser grammars
//
// Supported Features:
//   - Literals: abc
//   - Character classes: [abc], [a-z], [^abc] (negated)
//   - Shorthand classes: \d, \D, \w, \W, \s, \S
//   - Any character: .
//   - Quantifiers: *, +, ?
//   - Possessive quantifiers: *+, ++, ?+ (no backtracking - PEG native)
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
    CharRange[64] ranges;   // Start-end pairs for ranges
    int num_ranges;
    char[64] chars;         // Individual characters
    int num_chars;
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

fn void cc_add_range(CharClassData* cc, char start, char end) @inline {
    if (cc.num_ranges < 64) {
        cc.ranges[cc.num_ranges].start = start;
        cc.ranges[cc.num_ranges].end = end;
        cc.num_ranges++;
    }
}

fn void cc_add_char(CharClassData* cc, char c) @inline {
    if (cc.num_chars < 64) cc.chars[cc.num_chars++] = c;
}

fn char regex_escape_char(char c) @inline {
    switch (c) {
        case 'n': return '\n';
        case 't': return '\t';
        case 'r': return '\r';
        default: return c;
    }
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
            if (cc.num_ranges < 62) {
                cc_add_range(cc, 0, '/');
                cc_add_range(cc, ':', 127);
            }
            return true;
        case 'w':
            if (cc.num_ranges < 61) {
                cc_add_range(cc, 'a', 'z');
                cc_add_range(cc, 'A', 'Z');
                cc_add_range(cc, '0', '9');
            }
            cc_add_char(cc, '_');
            return true;
        case 'W':
            return true;
        case 's':
            if (cc.num_chars < 60) {
                cc_add_char(cc, ' ');
                cc_add_char(cc, '\t');
                cc_add_char(cc, '\n');
                cc_add_char(cc, '\r');
            }
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
    cc_add_range(cc, start, end);
    return true;
}

fn CharClassData* RegexTokenizer.parse_char_class(RegexTokenizer* self) {
    CharClassData* cc = mem::new(CharClassData);
    cc.num_ranges = 0;
    cc.num_chars = 0;
    cc.negated = false;

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
    while (self.peek() >= '0' && self.peek() <= '9') {
        min = min * 10 + (int)(self.advance() - '0');
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
            while (self.peek() >= '0' && self.peek() <= '9') {
                max = max * 10 + (int)(self.advance() - '0');
            }
            tok.bounds.max = max;
            if (self.peek() == '}') {
                self.advance();
            } else {
                self.set_error("Expected } in bounded quantifier");
            }
        }
    } else {
        self.set_error("Invalid bounded quantifier syntax");
        tok.type = RE_LITERAL;
        tok.ch = '{';
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
    CharClassData* cc = mem::new(CharClassData);
    cc.num_ranges = 1;
    cc.ranges[0].start = '0';
    cc.ranges[0].end = '9';
    cc.num_chars = 0;
    cc.negated = negated;
    return cc;
}

fn CharClassData* regex_word_class(bool negated) {
    CharClassData* cc = mem::new(CharClassData);
    cc.num_ranges = 3;
    cc.ranges[0].start = 'a';
    cc.ranges[0].end = 'z';
    cc.ranges[1].start = 'A';
    cc.ranges[1].end = 'Z';
    cc.ranges[2].start = '0';
    cc.ranges[2].end = '9';
    cc.num_chars = 1;
    cc.chars[0] = '_';
    cc.negated = negated;
    return cc;
}

fn CharClassData* regex_space_class(bool negated) {
    CharClassData* cc = mem::new(CharClassData);
    cc.num_ranges = 0;
    cc.num_chars = 4;
    cc.chars[0] = ' ';
    cc.chars[1] = '\t';
    cc.chars[2] = '\n';
    cc.chars[3] = '\r';
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

// ============================================================
// Regex to PikaParser Compiler
// ============================================================

// Storage for character class scan functions (like in lisp_pika.c3)
CharClassData*[32] g_regex_char_classes;
int g_regex_char_class_count = 0;

// Scan functions for regex character classes
fn int regex_scan_class_0(char[] view) { return scan_char_in_class(g_regex_char_classes[0], view); }
fn int regex_scan_class_1(char[] view) { return scan_char_in_class(g_regex_char_classes[1], view); }
fn int regex_scan_class_2(char[] view) { return scan_char_in_class(g_regex_char_classes[2], view); }
fn int regex_scan_class_3(char[] view) { return scan_char_in_class(g_regex_char_classes[3], view); }
fn int regex_scan_class_4(char[] view) { return scan_char_in_class(g_regex_char_classes[4], view); }
fn int regex_scan_class_5(char[] view) { return scan_char_in_class(g_regex_char_classes[5], view); }
fn int regex_scan_class_6(char[] view) { return scan_char_in_class(g_regex_char_classes[6], view); }
fn int regex_scan_class_7(char[] view) { return scan_char_in_class(g_regex_char_classes[7], view); }

ScanFn[8] g_regex_scan_fns = {
    &regex_scan_class_0, &regex_scan_class_1, &regex_scan_class_2, &regex_scan_class_3,
    &regex_scan_class_4, &regex_scan_class_5, &regex_scan_class_6, &regex_scan_class_7
};

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
    char[32] name;
    usz i = 0;
    while (i < prefix.len && i < 28) {
        name[i] = prefix[i];
        i++;
    }
    name[i++] = '_';
    name[i++] = (char)('0' + (rc.rule_counter / 10) % 10);
    name[i++] = (char)('0' + rc.rule_counter % 10);
    name[i] = 0;
    rc.rule_counter++;
    return (String)name[:i];
}

// Compile an atom (base element: literal, char class, dot, group)
fn int compile_regex_atom(RegexCompiler* rc) {
    RegexToken tok = rc.tok.next_token();

    switch (tok.type) {
        case RE_LITERAL:
            return rc.rb.add(regex_gen_name(rc, "lit"), mk_token(tok.ch));

        case RE_DOT:
            return rc.rb.add(regex_gen_name(rc, "dot"), mk_scan(&regex_scan_dot));

        case RE_CHAR_CLASS:
            if (g_regex_char_class_count >= 8) {
                return rc.rb.add(regex_gen_name(rc, "err"), mk_fail());
            }
            int idx = g_regex_char_class_count;
            g_regex_char_classes[idx] = tok.cclass;
            g_regex_char_class_count++;
            return rc.rb.add(regex_gen_name(rc, "cls"), mk_scan(g_regex_scan_fns[idx]));

        case RE_LPAREN:
            // Group: parse inner expression
            int inner = compile_regex_alternation(rc);
            // Expect closing paren
            RegexToken close = rc.tok.next_token();
            if (close.type != RE_RPAREN) {
                return rc.rb.add("_err", mk_fail());
            }
            return inner;

        case RE_NONCAP:
            // Non-capturing group (?:...): same as regular group for PEG
            int inner2 = compile_regex_alternation(rc);
            RegexToken close2 = rc.tok.next_token();
            if (close2.type != RE_RPAREN) {
                return rc.rb.add("_err", mk_fail());
            }
            return inner2;

        case RE_LOOKAHEAD_POS:
            // Positive lookahead (?=...)
            int inner3 = compile_regex_alternation(rc);
            RegexToken close3 = rc.tok.next_token();
            if (close3.type != RE_RPAREN) {
                return rc.rb.add("_err", mk_fail());
            }
            return rc.rb.add(regex_gen_name(rc, "la"), mk_followed_by(inner3));

        case RE_LOOKAHEAD_NEG:
            // Negative lookahead (?!...)
            int inner4 = compile_regex_alternation(rc);
            RegexToken close4 = rc.tok.next_token();
            if (close4.type != RE_RPAREN) {
                return rc.rb.add("_err", mk_fail());
            }
            return rc.rb.add(regex_gen_name(rc, "nla"), mk_not_followed_by(inner4));

        case RE_ANCHOR_END:
            return rc.rb.add("_end", mk_end_of_input());

        default:
            // Push back token for caller to handle
            rc.tok.pos -= 1;
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
            // For now, expand into sequence
            int min = qtok.bounds.min;
            int max = qtok.bounds.max;
            if (max == 0) max = min;
            if (max < 0) max = 100;  // Cap unbounded at 100

            int[] parts = mem::new_array(int, (usz)(max + 1));
            int part_count = 0;

            // Required part (min times)
            for (int i = 0; i < min && i < max; i++) {
                parts[part_count++] = atom;
            }

            // Optional part (min to max)
            if (max > min) {
                int eps = rc.rb.add("_eps", mk_epsilon());
                for (int i = min; i < max; i++) {
                    int[] opt_alts = make_children_2(atom, eps);
                    parts[part_count++] = rc.rb.add(regex_gen_name(rc, "bnd"), mk_first(opt_alts));
                }
            }

            if (part_count == 0) {
                return rc.rb.add("_eps", mk_epsilon());
            }
            if (part_count == 1) {
                return parts[0];
            }
            return rc.rb.add(regex_gen_name(rc, "seq"), mk_seq(parts[:part_count]));

        default:
            // Not a quantifier, push back
            rc.tok.pos = saved_pos;
            return atom;
    }
}

// Compile sequence (concatenation of quantified elements)
fn int compile_regex_sequence(RegexCompiler* rc) {
    int[64] parts;
    int part_count = 0;

    while (part_count < 64) {
        int saved_pos = rc.tok.pos;
        int elem = compile_regex_quantified(rc);
        if (elem < 0) {
            rc.tok.pos = saved_pos;
            break;
        }
        parts[part_count++] = elem;
    }

    if (part_count == 0) {
        return rc.rb.add("_eps", mk_epsilon());
    }
    if (part_count == 1) {
        return parts[0];
    }

    int[] children = mem::new_array(int, (usz)part_count);
    for (int i = 0; i < part_count; i++) {
        children[i] = parts[i];
    }
    return rc.rb.add(regex_gen_name(rc, "seq"), mk_seq(children));
}

// Compile alternation (top level: alternatives separated by |)
fn int compile_regex_alternation(RegexCompiler* rc) {
    int[32] alts;
    int alt_count = 0;

    // First alternative
    alts[0] = compile_regex_sequence(rc);
    alt_count = 1;

    // Additional alternatives
    while (alt_count < 32) {
        int saved_pos = rc.tok.pos;
        RegexToken tok = rc.tok.next_token();
        if (tok.type != RE_PIPE) {
            rc.tok.pos = saved_pos;
            break;
        }
        alts[alt_count++] = compile_regex_sequence(rc);
    }

    if (alt_count == 1) {
        return alts[0];
    }

    int[] children = mem::new_array(int, (usz)alt_count);
    for (int i = 0; i < alt_count; i++) {
        children[i] = alts[i];
    }
    return rc.rb.add(regex_gen_name(rc, "alt"), mk_first(children));
}

// ============================================================
// High-Level Regex API
// ============================================================

// Compile a regex pattern to PikaParser grammar
fn CompiledRegex* regex_compile(char[] pattern) {
    CompiledRegex* rx = mem::new(CompiledRegex);
    rx.valid = true;
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
        rx.valid = false;
        for (usz i = 0; i < 256 && tok.error[i] != 0; i++) {
            rx.error[i] = tok.error[i];
        }
        return rx;
    }

    // Compile the regex to grammar rules
    RegexCompiler rc = regex_compiler_new(&tok);
    int top_rule = compile_regex_alternation(&rc);

    // Add end-of-input if there's an end anchor
    first = tok.next_token();
    if (first.type == RE_ANCHOR_END) {
        rx.anchored_end = true;
        int end_rule = rc.rb.add("_end", mk_end_of_input());
        int[] seq = make_children_2(top_rule, end_rule);
        top_rule = rc.rb.add("_top", mk_seq(seq));
    }

    // Build the grammar
    if (rc.rb.len > 0) {
        String[] starts = mem::new_array(String, 1);
        starts[0] = rc.rb.rules[top_rule].name;
        rx.grammar = make_grammar(starts, rc.rb.to_array());
    }

    return rx;
}

// Free compiled regex
fn void regex_free(CompiledRegex* rx) {
    if (rx != null) {
        mem::free(rx);
    }
}

// Simple pattern match (greedy, no backtracking - PEG style)
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
        if (inp_pos >= (int)input.len) {
            // Check if remaining pattern is optional
            if (token.type == RE_ANCHOR_END) {
                break; // OK, end anchor at end of input
            }
            return no_match();
        }

        switch (token.type) {
            case RE_LITERAL:
                if (input[inp_pos] != token.ch) return no_match();
                inp_pos++;

            case RE_DOT:
                if (input[inp_pos] == '\n') return no_match();
                inp_pos++;

            case RE_CHAR_CLASS:
                int matched = scan_char_in_class(token.cclass, input[inp_pos..]);
                if (matched == 0) return no_match();
                inp_pos += matched;

            case RE_STAR:
            case RE_STAR_POSS:
                // Zero or more - need to look at next token
                RegexToken quant_elem = token;
                token = tok.next_token();
                // Greedy match as many as possible
                // For PEG, we don't backtrack
                // TODO: implement properly
                ;

            case RE_PLUS:
            case RE_PLUS_POSS:
                // One or more
                ;

            case RE_QUESTION:
            case RE_QUEST_POSS:
                // Optional
                ;

            case RE_ANCHOR_END:
                if (inp_pos != (int)input.len) return no_match();

            default:
                ;
        }

        token = tok.next_token();
    }

    return regex_mk_match(match_start, inp_pos - 1, input);
}

// Search for pattern anywhere in input
fn RegexMatch regex_search(char[] pattern, char[] input) {
    // Try matching at each position
    for (int i = 0; i < (int)input.len; i++) {
        RegexMatch m = regex_match_simple(pattern, input, i);
        if (m.matched) return m;
    }
    return no_match();
}

// Match pattern against entire input
fn RegexMatch regex_fullmatch(char[] pattern, char[] input) {
    RegexMatch m = regex_match_simple(pattern, input, 0);
    if (m.matched && m.start == 0 && m.end == (int)input.len - 1) {
        return m;
    }
    return no_match();
}

// Find all non-overlapping matches
fn RegexMatch[64] regex_find_all(char[] pattern, char[] input, int* count) {
    RegexMatch[64] matches;
    *count = 0;

    int pos = 0;
    while (pos < (int)input.len && *count < 64) {
        RegexMatch m = regex_match_simple(pattern, input, pos);
        if (m.matched) {
            matches[*count] = m;
            (*count)++;
            pos = m.end + 1;
            if (pos <= m.start) pos = m.start + 1; // Avoid infinite loop on empty match
        } else {
            pos++;
        }
    }

    return matches;
}
