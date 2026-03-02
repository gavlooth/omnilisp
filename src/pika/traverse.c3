module pika;

import std::io;

// ============================================================
// Match finding and viewing
// ============================================================

fn int find_match_at(ParserState* st, String rule, int pos) {
    int clause_idx = find_name_index(st.grammar, rule);
    if (clause_idx == -1) return -1;
    return lookup_best_match_id(pos, clause_idx, st);
}

fn char[] view_match_by_id(ParserState* st, int mid) {
    Match* m = &st.matches[mid];
    int len = (int)st.input.len;
    if (m.last >= m.first && m.first >= 0 && m.first < len && m.last < len) {
        return st.input[m.first..m.last];
    }
    char[] empty;
    return empty;
}

// ============================================================
// Parse tree traversal
// ============================================================

alias OpenFn = fn bool[](UserMatch* m, ParserState* st);
alias FoldFn = fn long(UserMatch* m, ParserState* st, long[] subvals);

fn bool[] default_open(UserMatch* m, ParserState* st) {
    if (m.submatches.len == 0) {
        bool[] empty;
        return empty;
    }
    bool[] result = mem::new_array(bool, m.submatches.len);
    for (usz i = 0; i < m.submatches.len; i++) {
        result[i] = true;
    }
    return result;
}

struct TNode {
    int parent_idx;
    int parent_sub_idx;
    UserMatch match;
    bool opened;
    long[] subvals;
    int subvals_count;
}

fn TNode make_tnode(int pidx, int psub, UserMatch um) {
    TNode n;
    n.parent_idx = pidx;
    n.parent_sub_idx = psub;
    n.match = um;
    n.opened = false;
    n.subvals_count = 0;
    return n;
}

fn long traverse_match(ParserState* st, int mid, OpenFn open_fn, FoldFn fold_fn) {
    int stk_cap = 64;
    TNode[] stk = mem::new_array(TNode, (usz)stk_cap);
    int stk_len = 0;

    int root_clause = st.matches[mid].clause;
    UserMatch root_um = get_user_view(root_clause, mid, st);

    stk[0] = make_tnode(-1, 0, root_um);
    stk_len = 1;

    while (true) {
        TNode* cur = &stk[stk_len - 1];
        if (!cur.opened) {
            cur.opened = true;
            int nsubs = (int)cur.match.submatches.len;
            if (nsubs > 0) {
                cur.subvals = mem::new_array(long, (usz)nsubs);
            }
            cur.subvals_count = nsubs;
            for (int j = 0; j < nsubs; j++) cur.subvals[j] = 0;

            bool[] mask = open_fn(&cur.match, st);
            int parent_idx = stk_len - 1;

            for (int i = nsubs - 1; i >= 0; i--) {
                if (i < (int)mask.len && mask[i]) {
                    int submid = cur.match.submatches[i];
                    int clause = st.matches[submid].clause;
                    UserMatch um = get_user_view(clause, submid, st);

                    if (stk_len >= stk_cap) {
                        int new_cap = stk_cap * 2;
                        TNode[] new_stk = mem::new_array(TNode, (usz)new_cap);
                        for (int k = 0; k < stk_len; k++) new_stk[k] = stk[k];
                        mem::free(stk.ptr);
                        stk = new_stk;
                        stk_cap = new_cap;
                        cur = &stk[parent_idx];
                    }

                    stk[stk_len] = make_tnode(parent_idx, i, um);
                    stk_len++;
                }
            }
            if (mask.len > 0) mem::free(mask.ptr);
        } else {
            long[] sv;
            if (cur.subvals_count > 0) {
                sv = cur.subvals[0..(usz)(cur.subvals_count - 1)];
            }
            long val = fold_fn(&cur.match, st, sv);
            if (cur.subvals_count > 0) mem::free(cur.subvals.ptr);

            if (cur.parent_idx == -1) {
                mem::free(stk.ptr);
                return val;
            }

            stk[cur.parent_idx].subvals[cur.parent_sub_idx] = val;
            stk_len--;
        }
    }
}

// ============================================================
// Print parse tree (for debugging)
// ============================================================

fn void print_tree(ParserState* st, int mid, int depth) {
    int clause = st.matches[mid].clause;
    UserMatch um = get_user_view(clause, mid, st);

    for (int i = 0; i < depth; i++) io::print("  ");
    io::printf("%s", st.grammar.names[um.rule]);
    if (um.submatches.len == 0) {
        io::printf("(\"%s\")", (String)um.view);
    }
    io::printn("");

    for (usz i = 0; i < um.submatches.len; i++) {
        print_tree(st, um.submatches[i], depth + 1);
    }
}
