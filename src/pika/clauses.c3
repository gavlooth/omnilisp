module pika;

// ============================================================
// Clause property queries
// ============================================================

fn bool is_terminal(Clause* c) {
    switch (c.tag) {
        case SATISFY: return true;
        case SCAN: return true;
        case TOKEN: return true;
        case TOKENS: return true;
        default: return false;
    }
}

fn int[] child_clauses(Clause* c) {
    switch (c.tag) {
        case SEQ: return c.children;
        case FIRST: return c.children;
        case NOT_FOLLOWED_BY: return (&c.child)[0..0];
        case FOLLOWED_BY: return (&c.child)[0..0];
        case SOME_CLAUSE: return (&c.child)[0..0];
        case MANY_CLAUSE: return (&c.child)[0..0];
        case TIE: return (&c.child)[0..0];
        default:
            int[] empty;
            return empty;
    }
}

fn bool clause_can_match_epsilon(Clause* c, bool[] child_eps) {
    switch (c.tag) {
        case SATISFY: return false;
        case SCAN: return false;
        case TOKEN: return false;
        case TOKENS: return false;
        case FAIL: return false;
        case EPSILON: return true;
        case END_OF_INPUT: return true;
        case SEQ:
            // all children must be emptiable
            for (int i = 0; i < (int)child_eps.len; i++) {
                if (!child_eps[i]) return false;
            }
            return true;
        case FIRST:
            if (child_eps.len == 0) return false;
            // non-last children must NOT be emptiable (enforced)
            for (int i = 0; i < (int)child_eps.len - 1; i++) {
                if (child_eps[i]) return false; // error condition
            }
            return child_eps[child_eps.len - 1];
        case NOT_FOLLOWED_BY:
            if (child_eps[0]) return false; // error: contents must not be epsilon
            return true;
        case FOLLOWED_BY:
            return child_eps[0];
        case SOME_CLAUSE:
            if (child_eps[0]) return false; // error: contents must not be epsilon
            return false;
        case MANY_CLAUSE:
            if (child_eps[0]) return false; // error: contents must not be epsilon
            return true;
        case TIE:
            return child_eps[0];
    }
    return false;
}

// seeded_by: which children seed this clause
fn int[] seeded_by(Clause* c, bool[] child_eps) {
    switch (c.tag) {
        case SEQ:
            // seed children up to first non-epsilon-matchable
            int first_nonempty;
            bool all_eps = true;
            for (int i = 0; i < (int)child_eps.len; i++) {
                if (!child_eps[i]) {
                    all_eps = false;
                    first_nonempty = i;
                    break;
                }
            }
            if (all_eps) first_nonempty = (int)child_eps.len - 1;
            return c.children[0..first_nonempty];
        case FIRST: return c.children;
        case FOLLOWED_BY: return (&c.child)[0..1];
        case SOME_CLAUSE: return (&c.child)[0..1];
        case MANY_CLAUSE: return (&c.child)[0..1];
        case TIE: return (&c.child)[0..1];
        default:
            int[] empty;
            return empty;
    }
}

fn bool better_match_than(Clause* clause, Match* new_m, Match* old_m) {
    if (clause.tag == FIRST) {
        if (new_m.option_idx == old_m.option_idx) {
            return new_m.last > old_m.last;
        }
        return new_m.option_idx < old_m.option_idx;
    }
    return new_m.last > old_m.last;
}

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
        case TOKEN: return match_terminal_token(c, input, pos);
        case TOKENS: return match_terminal_tokens(c, input, pos);
        default: return pos - 1;
    }
}

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

// ============================================================
// User view construction for traverse
// ============================================================

fn UserMatch make_user_match(int id, Match* m, int[] subs, ParserState* st) {
    UserMatch um;
    um.rule = id;
    um.first = m.first;
    um.last = m.last;
    um.submatches = subs;
    int len = (int)st.input.len;
    if (m.last >= m.first && m.first >= 0 && m.first < len && m.last < len) {
        um.view = st.input[m.first..m.last];
    }
    return um;
}

fn UserMatch user_view_default(int id, int mid, ParserState* st) {
    int[] empty;
    return make_user_match(id, &st.matches[mid], empty, st);
}

fn UserMatch user_view_with_submatches(int id, int mid, ParserState* st) {
    int[] subs = get_submatches(st, mid);
    // Copy to owned array
    int[] owned;
    if (subs.len > 0) {
        owned = mem::new_array(int, subs.len);
        for (usz i = 0; i < subs.len; i++) {
            owned[i] = subs[i];
        }
    }
    return make_user_match(id, &st.matches[mid], owned, st);
}

fn UserMatch user_view_some(int id, int mid, ParserState* st) {
    // Count items
    int items = 1;
    int m = mid;
    while (st.matches[m].option_idx == 1) {
        items += 1;
        int[] s = get_submatches(st, m);
        if (s.len < 2) break;
        m = s[1];
    }
    int[] res = mem::new_array(int, (usz)items);
    m = mid;
    int idx = 0;
    while (st.matches[m].option_idx == 1) {
        int[] s = get_submatches(st, m);
        res[idx] = s[0];
        idx++;
        if (s.len < 2) break;
        m = s[1];
    }
    if (idx < items) {
        int[] s = get_submatches(st, m);
        if (s.len > 0) res[idx] = s[0];
    }
    return make_user_match(id, &st.matches[mid], res, st);
}

fn UserMatch user_view_many(int id, int mid, ParserState* st) {
    // Count items
    int items = 0;
    int m = mid;
    while (st.matches[m].option_idx == 1) {
        items += 1;
        int[] s = get_submatches(st, m);
        if (s.len < 2) break;
        m = s[1];
    }
    int[] res = mem::new_array(int, (usz)items);
    m = mid;
    int idx = 0;
    while (st.matches[m].option_idx == 1) {
        int[] s = get_submatches(st, m);
        if (s.len > 0) res[idx] = s[0];
        idx++;
        if (s.len < 2) break;
        m = s[1];
    }
    return make_user_match(id, &st.matches[mid], res, st);
}

fn UserMatch user_view_tie(int id, int mid, ParserState* st) {
    Match* m = &st.matches[mid];
    int[] empty_subs;
    if (m.option_idx == 0) {
        return make_user_match(id, m, empty_subs, st);
    }

    int[] top_subs = get_submatches(st, mid);
    if (top_subs.len == 0) {
        return make_user_match(id, m, empty_subs, st);
    }
    int tmid = top_subs[0];
    int tcl = st.matches[tmid].clause;

    // Get the user_view of the inner clause
    UserMatch inner = get_user_view(tcl, tmid, st);

    // Flatten: collect grandchildren
    // Count first
    int count = 0;
    for (usz i = 0; i < inner.submatches.len; i++) {
        int cmid = inner.submatches[i];
        int cl = st.matches[cmid].clause;
        UserMatch child = get_user_view(cl, cmid, st);
        count += (int)child.submatches.len;
    }

    int[] ccmids = mem::new_array(int, (usz)count);
    int idx = 0;
    for (usz i = 0; i < inner.submatches.len; i++) {
        int cmid = inner.submatches[i];
        int cl = st.matches[cmid].clause;
        UserMatch child = get_user_view(cl, cmid, st);
        for (usz j = 0; j < child.submatches.len; j++) {
            ccmids[idx] = child.submatches[j];
            idx++;
        }
    }

    return make_user_match(id, m, ccmids, st);
}

fn UserMatch get_user_view(int clause_id, int mid, ParserState* st) {
    Clause* c = &st.grammar.clauses[clause_id];
    switch (c.tag) {
        case SEQ: return user_view_with_submatches(clause_id, mid, st);
        case FIRST: return user_view_with_submatches(clause_id, mid, st);
        case FOLLOWED_BY: return user_view_with_submatches(clause_id, mid, st);
        case SOME_CLAUSE: return user_view_some(clause_id, mid, st);
        case MANY_CLAUSE: return user_view_many(clause_id, mid, st);
        case TIE: return user_view_tie(clause_id, mid, st);
        default: return user_view_default(clause_id, mid, st);
    }
}
