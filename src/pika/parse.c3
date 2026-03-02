module pika;


// ============================================================
// Core parsing engine
// ============================================================

alias FastMatchFn = fn void(char[] input, int pos, ParserState* st);

fn int lookup_best_match_id(int pos, int clause, ParserState* st) {
    int mid = match_find(st, clause, pos);
    if (mid != -1) return mid;

    if (st.grammar.can_match_epsilon[clause]) {
        Clause* cls = &st.grammar.clauses[clause];
        return match_epsilon_clause(cls, clause, pos, st);
    }

    return -1;
}

fn int new_match(Match match, ParserState* st) {
    bool updated = false;

    int best = match_find(st, match.clause, match.first);

    if (best == -1 || better_match_than(&st.grammar.clauses[match.clause], &match, &st.matches[best])) {
        // Set tree pointers to null before inserting
        match.left = -1;
        match.right = -1;
        match.parent = -1;
        best = ps_push_match(st, match);
        match_insert(st, best);
        updated = true;
    } else {
        // Rollback submatches that were allocated before calling new_match
        submatch_rollback(st, match.submatches);
    }

    int[] seeds = st.grammar.seed_clauses[match.clause];
    for (int i = 0; i < (int)seeds.len; i++) {
        int seed = seeds[i];
        if (updated || st.grammar.can_match_epsilon[seed]) {
            st.q.push(seed);
        }
    }

    return best;
}

fn void report_terminal_match(ParserState* st, int clause_idx, int pos, int last) {
    Match m = make_match(clause_idx, pos, last, 0, submatch_empty(st));
    m.left = -1;
    m.right = -1;
    m.parent = -1;
    new_match(m, st);
}

// ============================================================
// Main parse function
// ============================================================

fn ParserState parse(Grammar* grammar, char[] input, FastMatchFn fast_match) {
    ParserState st;
    st.grammar = grammar;
    st.q = pq_new(grammar.num_clauses);
    st.matches = mem::new_array(Match, 256);
    st.matches_len = 0;
    st.matches_cap = 256;
    st.memo_root = -1;
    st.submatches = mem::new_array(int, 256);
    st.submatches_len = 0;
    st.submatches_cap = 256;
    st.input = input;

    // Pre-fill terminal queue
    PikaQueue terminal_q = pq_new(grammar.num_clauses);
    terminal_q.reset_with_sorted(grammar.terminals);

    int input_len = (int)input.len;

    // Process from right to left
    int i = input_len - 1;
    while (i >= 0) {
        if (fast_match == null) {
            st.q.reset_from(&terminal_q);
        } else {
            fast_match(input, i, &st);
        }

        while (!st.q.is_empty()) {
            int clause = st.q.pop();
            do_match_clause(&grammar.clauses[clause], clause, i, &st);
        }
        i--;
    }

    terminal_q.free();
    return st;
}

fn ParserState parse_simple(Grammar* grammar, char[] input) {
    return parse(grammar, input, null);
}

// ============================================================
// Lexing
// ============================================================

struct LexEntry {
    int rule_idx;  // clause index
    int last;      // last position of terminal match
}

struct LexResult {
    LexEntry[][] entries;  // entries[pos] = array of (rule, last) for that position
    int len;
}

fn LexResult lex(Grammar* g, char[] input) {
    int input_len = (int)input.len;
    LexResult result;
    if (input_len == 0) {
        result.len = 0;
        return result;
    }

    result.entries = mem::new_array(LexEntry[], (usz)input_len);
    result.len = input_len;

    // Temporary storage for each position
    // Use a simple queue to track which positions to visit
    PikaQueue pq = pq_new(input_len);
    pq.push(0);

    // Temporary per-position entry lists
    int[] entry_counts = mem::new_array(int, (usz)input_len);
    int[] entry_caps = mem::new_array(int, (usz)input_len);
    for (int i = 0; i < input_len; i++) {
        entry_counts[i] = 0;
        entry_caps[i] = 0;
    }

    while (!pq.is_empty()) {
        int pos = pq.pop();
        for (int ti = 0; ti < (int)g.terminals.len; ti++) {
            int tidx = g.terminals[ti];
            int mlast = do_match_terminal(&g.clauses[tidx], input, pos);
            if (mlast >= pos) {
                // Add entry
                lex_add_entry(&result, entry_counts, entry_caps, pos, tidx, mlast);
            }
            int nxt = mlast + 1;
            if (nxt > pos && nxt < input_len) {
                pq.push(nxt);
            }
        }
    }

    pq.free();
    mem::free(entry_counts.ptr);
    mem::free(entry_caps.ptr);
    return result;
}

fn void lex_add_entry(LexResult* result, int[] counts, int[] caps, int pos, int rule, int last) {
    if (counts[pos] >= caps[pos]) {
        int new_cap = caps[pos] < 4 ? 4 : caps[pos] * 2;
        LexEntry[] new_arr = mem::new_array(LexEntry, (usz)new_cap);
        for (int i = 0; i < counts[pos]; i++) {
            new_arr[i] = result.entries[pos][i];
        }
        if (caps[pos] > 0) mem::free(result.entries[pos].ptr);
        result.entries[pos] = new_arr;
        caps[pos] = new_cap;
    }
    LexEntry entry;
    entry.rule_idx = rule;
    entry.last = last;
    result.entries[pos][counts[pos]] = entry;
    counts[pos]++;
}

fn void lex_fast_match(char[] input, int pos, ParserState* st) {
    // This is called during parse with pre-lexed data
    // The lex result is stored in the parser state's grammar (via closure workaround)
    // We'll use a different approach - see parse_lex
}

fn ParserState parse_lex(Grammar* g, char[] input) {
    LexResult lexemes = lex(g, input);

    // We need a way to pass lexemes into the fast_match callback
    // Since C3 doesn't have closures, we'll inline the parse loop

    ParserState st;
    st.grammar = g;
    st.q = pq_new(g.num_clauses);
    st.matches = mem::new_array(Match, 256);
    st.matches_len = 0;
    st.matches_cap = 256;
    st.memo_root = -1;
    st.submatches = mem::new_array(int, 256);
    st.submatches_len = 0;
    st.submatches_cap = 256;
    st.input = input;

    int input_len = (int)input.len;
    int i = input_len - 1;
    while (i >= 0) {
        // Fast match: report pre-lexed terminals
        if (i < lexemes.len) {
            LexEntry[] entries = lexemes.entries[i];
            for (int ei = 0; ei < (int)entries.len; ei++) {
                report_terminal_match(&st, entries[ei].rule_idx, i, entries[ei].last);
            }
        }

        while (!st.q.is_empty()) {
            int clause = st.q.pop();
            do_match_clause(&g.clauses[clause], clause, i, &st);
        }
        i--;
    }

    return st;
}
