module pika;

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
