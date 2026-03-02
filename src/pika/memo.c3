module pika;


// ============================================================
// Submatch helpers
// ============================================================

fn int submatch_start(ParserState* st) {
    return st.submatches_len;  // next available index (0-based)
}

fn int submatch_empty(ParserState* st) {
    return st.submatches_len;
}

fn void ps_ensure_submatches(ParserState* st, int needed) {
    while (st.submatches_cap < needed) {
        int new_cap = st.submatches_cap < 16 ? 16 : st.submatches_cap * 2;
        int[] new_arr = mem::new_array(int, (usz)new_cap);
        for (int i = 0; i < st.submatches_len; i++) {
            new_arr[i] = st.submatches[i];
        }
        if (st.submatches_cap > 0) mem::free(st.submatches.ptr);
        st.submatches = new_arr;
        st.submatches_cap = new_cap;
    }
}

fn int submatch_record_one(ParserState* st, int mid) {
    ps_ensure_submatches(st, st.submatches_len + 1);
    int idx = st.submatches_len;
    st.submatches[idx] = mid;
    st.submatches_len += 1;
    return idx;
}

fn int submatch_record_two(ParserState* st, int mid1, int mid2) {
    ps_ensure_submatches(st, st.submatches_len + 2);
    int idx = st.submatches_len;
    st.submatches[idx] = mid1;
    st.submatches[idx + 1] = mid2;
    st.submatches_len += 2;
    return idx;
}

fn void submatch_rollback(ParserState* st, int start) {
    st.submatches_len = start;
}

// Get submatches for a given match ID
// Returns a slice of the submatches array
fn int[] get_submatches(ParserState* st, int mid) {
    int b = st.matches[mid].submatches;
    if (b < 0 || b >= st.submatches_len) {
        // empty
        int[] empty;
        return empty;
    }
    int e;
    if (mid + 1 < st.matches_len) {
        e = st.matches[mid + 1].submatches;
    } else {
        e = st.submatches_len;
    }
    if (e <= b) {
        int[] empty;
        return empty;
    }
    return st.submatches[b..e - 1];
}

// ============================================================
// Match storage helpers
// ============================================================

fn void ps_ensure_matches(ParserState* st, int needed) {
    while (st.matches_cap < needed) {
        int new_cap = st.matches_cap < 16 ? 16 : st.matches_cap * 2;
        Match[] new_arr = mem::new_array(Match, (usz)new_cap);
        for (int i = 0; i < st.matches_len; i++) {
            new_arr[i] = st.matches[i];
        }
        if (st.matches_cap > 0) mem::free(st.matches.ptr);
        st.matches = new_arr;
        st.matches_cap = new_cap;
    }
}

fn int ps_push_match(ParserState* st, Match m) {
    ps_ensure_matches(st, st.matches_len + 1);
    int idx = st.matches_len;
    st.matches[idx] = m;
    st.matches_len += 1;
    return idx;
}

// ============================================================
// Splay Tree Operations for memo table
// Match ordering: primary by first (ascending), secondary by clause (descending)
// Uses 0-based indexing. -1 means null/empty.
// ============================================================

fn void adjust_child(ParserState* st, int mid, int old_child, int new_child) {
    if (mid == -1) {
        st.memo_root = new_child;
        return;
    }
    if (st.matches[mid].left == old_child) {
        st.matches[mid].left = new_child;
    } else if (st.matches[mid].right == old_child) {
        st.matches[mid].right = new_child;
    }
}

fn void adjust_parent(ParserState* st, int mid, int new_parent) {
    if (mid == -1) return;
    st.matches[mid].parent = new_parent;
}

fn void match_splay(ParserState* st, int mid) {
    while (true) {
        Match* m = &st.matches[mid];

        if (m.parent == -1) break;

        int pid = m.parent;
        Match* p = &st.matches[pid];

        if (mid == p.left) {
            // left child
            if (p.parent == -1) {
                // zig left
                int m_right = m.right;
                st.matches[mid].right = pid;
                st.matches[mid].parent = -1;
                st.matches[pid].left = m_right;
                st.matches[pid].parent = mid;
                adjust_parent(st, m_right, pid);
                break;
            }
            int ppid = p.parent;
            Match* pp = &st.matches[ppid];
            if (pid == pp.left) {
                // left of left (zig-zig)
                int m_right = st.matches[mid].right;
                int p_right = st.matches[pid].right;
                adjust_child(st, pp.parent, ppid, mid);
                st.matches[mid].parent = pp.parent;
                st.matches[mid].right = pid;
                st.matches[pid].parent = mid;
                st.matches[pid].left = m_right;
                st.matches[pid].right = ppid;
                st.matches[ppid].parent = pid;
                st.matches[ppid].left = p_right;
                adjust_parent(st, m_right, pid);
                adjust_parent(st, p_right, ppid);
            } else {
                // left of right (zig-zag)
                int m_left = st.matches[mid].left;
                int m_right = st.matches[mid].right;
                adjust_child(st, pp.parent, ppid, mid);
                st.matches[mid].parent = pp.parent;
                st.matches[mid].left = ppid;
                st.matches[mid].right = pid;
                st.matches[pid].parent = mid;
                st.matches[pid].left = m_right;
                st.matches[ppid].parent = mid;
                st.matches[ppid].right = m_left;
                adjust_parent(st, m_right, pid);
                adjust_parent(st, m_left, ppid);
            }
        } else {
            // right child
            if (p.parent == -1) {
                // zig right
                int m_left = m.left;
                st.matches[mid].left = pid;
                st.matches[mid].parent = -1;
                st.matches[pid].right = m_left;
                st.matches[pid].parent = mid;
                adjust_parent(st, m_left, pid);
                break;
            }
            int ppid = p.parent;
            Match* pp = &st.matches[ppid];
            if (pid == pp.left) {
                // right of left (zig-zag)
                int m_left = st.matches[mid].left;
                int m_right = st.matches[mid].right;
                adjust_child(st, pp.parent, ppid, mid);
                st.matches[mid].parent = pp.parent;
                st.matches[mid].right = ppid;
                st.matches[mid].left = pid;
                st.matches[pid].parent = mid;
                st.matches[pid].right = m_left;
                st.matches[ppid].parent = mid;
                st.matches[ppid].left = m_right;
                adjust_parent(st, m_left, pid);
                adjust_parent(st, m_right, ppid);
            } else {
                // right of right (zig-zig)
                int m_left = st.matches[mid].left;
                int p_left = st.matches[pid].left;
                adjust_child(st, pp.parent, ppid, mid);
                st.matches[mid].parent = pp.parent;
                st.matches[mid].left = pid;
                st.matches[pid].parent = mid;
                st.matches[pid].right = m_left;
                st.matches[pid].left = ppid;
                st.matches[ppid].parent = pid;
                st.matches[ppid].right = p_left;
                adjust_parent(st, m_left, pid);
                adjust_parent(st, p_left, ppid);
            }
        }
    }

    st.memo_root = mid;
}

fn void match_insert(ParserState* st, int nmid) {
    Match* nm = &st.matches[nmid];

    if (st.memo_root == -1) {
        st.memo_root = nmid;
        return;
    }

    int mid = st.memo_root;
    while (true) {
        Match* m = &st.matches[mid];
        if (nm.first < m.first || (nm.first == m.first && nm.clause > m.clause)) {
            if (m.left == -1) {
                st.matches[nmid].parent = mid;
                st.matches[mid].left = nmid;
                match_splay(st, nmid);
                return;
            } else {
                mid = m.left;
            }
        } else if (nm.first > m.first || (nm.first == m.first && nm.clause < m.clause)) {
            if (m.right == -1) {
                st.matches[nmid].parent = mid;
                st.matches[mid].right = nmid;
                match_splay(st, nmid);
                return;
            } else {
                mid = m.right;
            }
        } else {
            // replace: same (first, clause)
            st.matches[nmid].parent = m.parent;
            st.matches[nmid].left = m.left;
            st.matches[nmid].right = m.right;

            adjust_child(st, m.parent, mid, nmid);
            adjust_parent(st, m.left, nmid);
            adjust_parent(st, m.right, nmid);

            // disconnect old
            st.matches[mid].parent = -1;
            st.matches[mid].left = -1;
            st.matches[mid].right = -1;

            match_splay(st, nmid);
            return;
        }
    }
}

// Returns match index or -1 if not found
fn int match_find(ParserState* st, int clause, int pos) {
    int mid = st.memo_root;
    if (mid == -1) return -1;

    while (true) {
        Match* m = &st.matches[mid];
        if (pos == m.first && clause == m.clause) {
            match_splay(st, mid);
            return mid;
        }

        int nmid;
        if (pos < m.first || (pos == m.first && clause > m.clause)) {
            nmid = m.left;
        } else {
            nmid = m.right;
        }

        if (nmid == -1) {
            match_splay(st, mid);
            return -1;
        }

        mid = nmid;
    }
}
