module pika;

// ============================================================
// Terminal matching
// Returns the last position of match, or pos-1 if no match.
// Input is 0-indexed char[].
// ============================================================

fn int match_terminal_satisfy(Clause* c, char[] input, int pos) {
    if (pos < (int)input.len && c.satisfy_fn(input[pos])) {
        return pos;
    }
    return pos - 1;
}

fn int match_terminal_scan(Clause* c, char[] input, int pos) {
    char[] v = input[pos..];
    int result = c.scan_fn(v);
    if (result <= 0) return pos - 1;
    // scan_fn returns the number of characters matched (1-based last index)
    return pos + result - 1;
}

fn int match_terminal_scan_ctx(Clause* c, char[] input, int pos) {
    char[] v = input[pos..];
    int result = c.scan_ctx_fn(c.scan_ctx, v);
    if (result <= 0) return pos - 1;
    return pos + result - 1;
}

fn int match_terminal_token(Clause* c, char[] input, int pos) {
    if (pos < (int)input.len && input[pos] == c.token_val) {
        return pos;
    }
    return pos - 1;
}

fn int match_terminal_tokens(Clause* c, char[] input, int pos) {
    char[] toks = c.tokens_val;
    int ii = pos;
    int ie = (int)input.len;
    int ti = 0;
    int te = (int)toks.len;
    while (true) {
        if (ii >= ie) break;
        if (ti >= te) break;
        if (input[ii] != toks[ti]) break;
        if (ti == te - 1) return ii;
        ii++;
        ti++;
    }
    return pos - 1;
}

fn int do_match_terminal(Clause* c, char[] input, int pos) {
    switch (c.tag) {
        case SATISFY: return match_terminal_satisfy(c, input, pos);
        case SCAN: return match_terminal_scan(c, input, pos);
        case SCAN_CTX: return match_terminal_scan_ctx(c, input, pos);
        case TOKEN: return match_terminal_token(c, input, pos);
        case TOKENS: return match_terminal_tokens(c, input, pos);
        default: return pos - 1;
    }
}
