module pika;

import std::core::mem;

struct RuleEntry {
    String name;
    Clause clause;
}

fn void build_topo_order(String[] starts, RuleEntry[] rules, int[][] edges,
    int[] opened, int[] stk, int[] topo_order, int[] topo_order_idx, int* last_order) {
    int n_rules = (int)rules.len;
    assert(starts.len > 0, "make_grammar: starts must not be empty");
    assert(n_rules > 0, "make_grammar: rules must not be empty");
    for (int i = 0; i < n_rules; i++) {
        opened[i] = 0;
        topo_order_idx[i] = -1;
    }

    int stk_len = 0;
    for (int si = 0; si < (int)starts.len; si++) {
        int idx = find_rule_index(rules, starts[si]);
        assert(idx >= 0, "make_grammar: missing start rule");
        stk[stk_len++] = idx;
    }

    *last_order = 0;
    while (stk_len > 0) {
        int cur = stk[stk_len - 1];
        int[] cur_edges = edges[cur];
        if (opened[cur] < (int)cur_edges.len) {
            int child_idx = cur_edges[opened[cur]];
            opened[cur]++;
            if (opened[child_idx] == 0) {
                stk[stk_len++] = child_idx;
            }
        } else if (opened[cur] == (int)cur_edges.len) {
            opened[cur]++;
            stk_len--;
            topo_order[*last_order] = cur;
            topo_order_idx[cur] = *last_order;
            (*last_order)++;
        } else {
            stk_len--;
        }
    }
}

fn void build_reordered_rules(RuleEntry[] rules, int[] topo_order, int[] topo_order_idx,
    Clause[] clauses, String[] names, int reachable_count) {
    for (int i = 0; i < reachable_count; i++) {
        int orig_idx = topo_order[i];
        names[i] = rules[orig_idx].name;
        clauses[i] = reindex_clause(&rules[orig_idx].clause, topo_order_idx);
    }
}

fn int[][] build_parent_clauses(Clause[] clauses, int[] parent_counts) {
    int n_rules = (int)clauses.len;
    for (int i = 0; i < n_rules; i++) parent_counts[i] = 0;

    for (int i = 0; i < n_rules; i++) {
        int[] cc = child_clauses(&clauses[i]);
        for (int j = 0; j < (int)cc.len; j++) {
            parent_counts[cc[j]]++;
        }
    }

    int[][] parent_clauses_arr = mem::new_array(int[], (usz)n_rules);
    for (int i = 0; i < n_rules; i++) {
        parent_clauses_arr[i] = mem::new_array(int, (usz)parent_counts[i]);
        parent_counts[i] = 0;
    }

    for (int i = 0; i < n_rules; i++) {
        int[] cc = child_clauses(&clauses[i]);
        for (int j = 0; j < (int)cc.len; j++) {
            int ch = cc[j];
            parent_clauses_arr[ch][parent_counts[ch]] = i;
            parent_counts[ch]++;
        }
    }
    return parent_clauses_arr;
}

fn void compute_emptiable_clauses(Clause[] clauses, int[][] parent_clauses_arr,
    int[] parent_counts, bool[] emptiable) {
    int n_rules = (int)clauses.len;
    for (int i = 0; i < n_rules; i++) emptiable[i] = false;

    PikaQueue q = pq_full(n_rules);
    while (!q.is_empty()) {
        int cur = q.pop();
        int[] cc = child_clauses(&clauses[cur]);
        bool[] child_eps = mem::new_array(bool, cc.len);
        for (int j = 0; j < (int)cc.len; j++) {
            child_eps[j] = emptiable[cc[j]];
        }
        bool cur_emp = clause_can_match_epsilon(&clauses[cur], child_eps);
        if (!emptiable[cur] && cur_emp) {
            emptiable[cur] = true;
            for (int j = 0; j < parent_counts[cur]; j++) {
                q.push(parent_clauses_arr[cur][j]);
            }
        }
        mem::free(child_eps.ptr);
    }
    q.free();
}

fn int[][] build_seed_clauses(Clause[] clauses, bool[] emptiable, int[] seed_counts) {
    int n_rules = (int)clauses.len;
    for (int i = 0; i < n_rules; i++) seed_counts[i] = 0;

    for (int i = 0; i < n_rules; i++) {
        int[] cc = child_clauses(&clauses[i]);
        bool[] child_eps = mem::new_array(bool, cc.len);
        for (int j = 0; j < (int)cc.len; j++) {
            child_eps[j] = emptiable[cc[j]];
        }
        int[] sb = seeded_by(&clauses[i], child_eps);
        for (int j = 0; j < (int)sb.len; j++) {
            seed_counts[sb[j]]++;
        }
        mem::free(child_eps.ptr);
    }

    int[][] seed_cls = mem::new_array(int[], (usz)n_rules);
    for (int i = 0; i < n_rules; i++) {
        seed_cls[i] = mem::new_array(int, (usz)seed_counts[i]);
        seed_counts[i] = 0;
    }

    for (int i = 0; i < n_rules; i++) {
        int[] cc = child_clauses(&clauses[i]);
        bool[] child_eps = mem::new_array(bool, cc.len);
        for (int j = 0; j < (int)cc.len; j++) {
            child_eps[j] = emptiable[cc[j]];
        }
        int[] sb = seeded_by(&clauses[i], child_eps);
        for (int j = 0; j < (int)sb.len; j++) {
            int ch = sb[j];
            seed_cls[ch][seed_counts[ch]] = i;
            seed_counts[ch]++;
        }
        mem::free(child_eps.ptr);
    }
    return seed_cls;
}

fn int[] collect_terminal_indices(Clause[] clauses) {
    int n_rules = (int)clauses.len;
    int term_count = 0;
    for (int i = 0; i < n_rules; i++) {
        if (is_terminal(&clauses[i])) term_count++;
    }
    int[] terminals = mem::new_array(int, (usz)term_count);
    int ti = 0;
    for (int i = 0; i < n_rules; i++) {
        if (is_terminal(&clauses[i])) terminals[ti++] = i;
    }
    return terminals;
}

fn int find_rule_index(RuleEntry[] rules, String name) {
    for (int i = 0; i < (int)rules.len; i++) {
        if (str_eq(rules[i].name, name)) return i;
    }
    return -1;
}

fn int find_name_index(Grammar* g, String name) {
    for (int i = 0; i < (int)g.names.len; i++) {
        if (str_eq(g.names[i], name)) return i;
    }
    return -1;
}

fn bool str_eq(String a, String b) {
    if (a.len != b.len) return false;
    for (usz i = 0; i < a.len; i++) {
        if (a[i] != b[i]) return false;
    }
    return true;
}

fn Clause reindex_clause(Clause* orig, int[] topo_idx) {
    Clause c = *orig;
    switch (c.tag) {
        case SEQ:
            int[] new_ch = mem::new_array(int, orig.children.len);
            for (usz i = 0; i < orig.children.len; i++) {
                new_ch[i] = topo_idx[orig.children[i]];
            }
            c.children = new_ch;
        case FIRST:
            int[] new_ch2 = mem::new_array(int, orig.children.len);
            for (usz i = 0; i < orig.children.len; i++) {
                new_ch2[i] = topo_idx[orig.children[i]];
            }
            c.children = new_ch2;
        case NOT_FOLLOWED_BY:
            c.child = topo_idx[orig.child];
        case FOLLOWED_BY:
            c.child = topo_idx[orig.child];
        case SOME_CLAUSE:
            c.child = topo_idx[orig.child];
        case MANY_CLAUSE:
            c.child = topo_idx[orig.child];
        case TIE:
            c.child = topo_idx[orig.child];
        default:
            ;
    }
    return c;
}
