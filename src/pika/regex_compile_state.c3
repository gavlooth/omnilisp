module pika;

import std::core::mem;

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
