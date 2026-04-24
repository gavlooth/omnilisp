module pika;

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

fn void UserMatch.release(UserMatch* self) {
    if (self.submatches.len > 0) mem::free(self.submatches.ptr);
    int[] empty_submatches;
    self.submatches = empty_submatches;
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
    defer inner.release();

    // Flatten: collect grandchildren
    // Count first
    int count = 0;
    for (usz i = 0; i < inner.submatches.len; i++) {
        int cmid = inner.submatches[i];
        int cl = st.matches[cmid].clause;
        UserMatch child = get_user_view(cl, cmid, st);
        count += (int)child.submatches.len;
        child.release();
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
        child.release();
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
