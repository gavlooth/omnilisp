module pika;

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
