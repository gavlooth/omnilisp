module pika;

import std::core::mem;

fn int submatch_start(ParserState* st) {
    return st.submatches_len;
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

fn int[] get_submatches(ParserState* st, int mid) {
    int b = st.matches[mid].submatches;
    if (b < 0 || b >= st.submatches_len) {
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
