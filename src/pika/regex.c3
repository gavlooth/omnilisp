module pika;

import std::core::mem;
import std::io;

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
