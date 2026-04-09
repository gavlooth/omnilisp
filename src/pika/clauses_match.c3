module pika;

// ============================================================
// Clause matching - dispatches to specific match logic
// Returns match index or -1 if no match.
// ============================================================

// Forward declarations handled by C3's module system

fn int match_clause_terminal(Clause* c, int id, int pos, ParserState* st) {
    int last = do_match_terminal(c, st.input, pos);
    if (last < pos) {
        return -1;
    }
    Match m = make_match(id, pos, last, 0, submatch_empty(st));
    return new_match(m, st);
}

fn int steplastind(char[] data, int last) {
    // Advance past last matched position. Cap at len (past-end sentinel).
    int len = (int)data.len;
    if (last + 1 > len) return len;
    return last + 1;
}

fn int match_clause_seq(Clause* c, int id, int orig_pos, ParserState* st) {
    // check first pass
    int pos = orig_pos;
    for (int ci = 0; ci < (int)c.children.len; ci++) {
        int child = c.children[ci];
        int mid = lookup_best_match_id(pos, child, st);
        if (mid == -1) return -1;
        pos = steplastind(st.input, st.matches[mid].last);
    }

    // allocate submatches
    pos = orig_pos;
    int last = pos - 1;
    int seq_start = submatch_start(st);
    for (int ci = 0; ci < (int)c.children.len; ci++) {
        int child = c.children[ci];
        int mid = lookup_best_match_id(pos, child, st);
        submatch_record_one(st, mid);
        last = st.matches[mid].last;
        pos = steplastind(st.input, last);
    }
    Match m = make_match(id, orig_pos, last, 0, seq_start);
    return new_match(m, st);
}

fn int match_clause_first(Clause* c, int id, int pos, ParserState* st) {
    for (int i = 0; i < (int)c.children.len; i++) {
        int child = c.children[i];
        int mid = lookup_best_match_id(pos, child, st);
        if (mid == -1) continue;
        int sm = submatch_record_one(st, mid);
        Match m = make_match(id, pos, st.matches[mid].last, i + 1, sm);
        return new_match(m, st);
    }
    return -1;
}

fn int match_clause_followed_by(Clause* c, int id, int pos, ParserState* st) {
    int mid = lookup_best_match_id(pos, c.child, st);
    if (mid == -1) return -1;
    int sm = submatch_record_one(st, mid);
    Match m = make_match(id, pos, pos - 1, 1, sm);
    return new_match(m, st);
}

fn int match_clause_some(Clause* c, int id, int pos, ParserState* st) {
    int mid1 = lookup_best_match_id(pos, c.child, st);
    if (mid1 == -1) return -1;
    int next_pos = steplastind(st.input, st.matches[mid1].last);
    int mid2 = lookup_best_match_id(next_pos, id, st);
    if (mid2 == -1) {
        int sm = submatch_record_one(st, mid1);
        Match m = make_match(id, pos, st.matches[mid1].last, 0, sm);
        return new_match(m, st);
    } else {
        int sm = submatch_record_two(st, mid1, mid2);
        Match m = make_match(id, pos, st.matches[mid2].last, 1, sm);
        return new_match(m, st);
    }
}

fn int match_clause_many(Clause* c, int id, int pos, ParserState* st) {
    int mid1 = lookup_best_match_id(pos, c.child, st);
    if (mid1 == -1) {
        return match_epsilon_clause(c, id, pos, st);
    }
    int next_pos = steplastind(st.input, st.matches[mid1].last);
    int mid2 = lookup_best_match_id(next_pos, id, st);
    // mid2 should not be -1 for Many
    int sm = submatch_record_two(st, mid1, mid2);
    Match m = make_match(id, pos, st.matches[mid2].last, 1, sm);
    return new_match(m, st);
}

fn int match_clause_tie(Clause* c, int id, int pos, ParserState* st) {
    int mid = lookup_best_match_id(pos, c.child, st);
    if (mid == -1) return -1;
    int sm = submatch_record_one(st, mid);
    Match m = make_match(id, pos, st.matches[mid].last, 1, sm);
    return new_match(m, st);
}

fn int do_match_clause(Clause* c, int id, int pos, ParserState* st) {
    switch (c.tag) {
        case SATISFY: return match_clause_terminal(c, id, pos, st);
        case SCAN: return match_clause_terminal(c, id, pos, st);
        case SCAN_CTX: return match_clause_terminal(c, id, pos, st);
        case TOKEN: return match_clause_terminal(c, id, pos, st);
        case TOKENS: return match_clause_terminal(c, id, pos, st);
        case SEQ: return match_clause_seq(c, id, pos, st);
        case FIRST: return match_clause_first(c, id, pos, st);
        case FOLLOWED_BY: return match_clause_followed_by(c, id, pos, st);
        case SOME_CLAUSE: return match_clause_some(c, id, pos, st);
        case MANY_CLAUSE: return match_clause_many(c, id, pos, st);
        case TIE: return match_clause_tie(c, id, pos, st);
        default: return -1;
    }
}

// ============================================================
// Epsilon matching
// ============================================================

fn int match_epsilon_default(int id, int pos, ParserState* st) {
    Match m = make_match(id, pos, pos - 1, 0, submatch_empty(st));
    return new_match(m, st);
}

fn int match_epsilon_end_of_input(int id, int pos, ParserState* st) {
    if (pos < (int)st.input.len) return -1;
    Match m = make_match(id, pos, pos - 1, 0, submatch_empty(st));
    return new_match(m, st);
}

fn int match_epsilon_followed_by(Clause* c, int id, int pos, ParserState* st) {
    int mid = lookup_best_match_id(pos, c.child, st);
    if (mid == -1) return -1;
    int sm = submatch_record_one(st, mid);
    Match m = make_match(id, pos, pos - 1, 1, sm);
    return new_match(m, st);
}

fn int match_epsilon_not_followed_by(Clause* c, int id, int pos, ParserState* st) {
    int mid = lookup_best_match_id(pos, c.child, st);
    if (mid != -1) return -1;
    Match m = make_match(id, pos, pos - 1, 0, submatch_empty(st));
    return new_match(m, st);
}

fn int match_epsilon_clause(Clause* c, int id, int pos, ParserState* st) {
    switch (c.tag) {
        case END_OF_INPUT: return match_epsilon_end_of_input(id, pos, st);
        case FOLLOWED_BY: return match_epsilon_followed_by(c, id, pos, st);
        case NOT_FOLLOWED_BY: return match_epsilon_not_followed_by(c, id, pos, st);
        case MANY_CLAUSE:
            // Many epsilon: if item doesn't match, return empty
            int mid1 = lookup_best_match_id(pos, c.child, st);
            if (mid1 == -1) {
                return match_epsilon_default(id, pos, st);
            }
            // If item matches, this shouldn't be called as epsilon
            return match_epsilon_default(id, pos, st);
        default: return match_epsilon_default(id, pos, st);
    }
}
