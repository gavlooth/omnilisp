module pika;

import std::io;

// ============================================================
// Grammar construction from named rules
// Equivalent to Julia's make_grammar()
// ============================================================

// Input: arrays of start rule names and all rules (name -> clause pairs)
// All clause children reference other rules by index into the rules array.
//
// After make_grammar:
// - Rules are topologically sorted
// - Children are reindexed to the topo order
// - Epsilon-matchability is computed
// - Seed relationships are computed

struct RuleEntry {
    String name;
    Clause clause;
}

fn Grammar make_grammar(String[] starts, RuleEntry[] rules) {
    int n_rules = (int)rules.len;

    // Build name -> index mapping
    int[] rule_idx_map = mem::new_array(int, (usz)n_rules); // not really needed, index IS position

    // edges[i] = child clause indices for rule i
    int[][] edges = mem::new_array(int[], (usz)n_rules);
    for (int i = 0; i < n_rules; i++) {
        edges[i] = child_clauses(&rules[i].clause);
    }

    // Find rule index by name (linear search - fine for grammar-sized data)
    // We need a lookup function
    // Build the topological order using DFS
    int[] opened = mem::new_array(int, (usz)n_rules);
    for (int i = 0; i < n_rules; i++) opened[i] = 0;

    // Stack for DFS
    int[] stk = mem::new_array(int, (usz)(n_rules + 1));
    int stk_len = 0;

    // Push start rules
    for (int si = 0; si < (int)starts.len; si++) {
        int idx = find_rule_index(rules, starts[si]);
        if (idx >= 0) {
            stk[stk_len] = idx;
            stk_len++;
        }
    }

    int[] topo_order = mem::new_array(int, (usz)n_rules);
    int[] topo_order_idx = mem::new_array(int, (usz)n_rules);
    for (int i = 0; i < n_rules; i++) topo_order_idx[i] = -1;
    int last_order = 0;

    while (stk_len > 0) {
        int cur = stk[stk_len - 1];
        int[] cur_edges = edges[cur];
        if (opened[cur] < (int)cur_edges.len) {
            int child_idx = cur_edges[opened[cur]];
            opened[cur]++;
            if (opened[child_idx] == 0) {
                stk[stk_len] = child_idx;
                stk_len++;
            }
        } else if (opened[cur] == (int)cur_edges.len) {
            opened[cur]++;
            stk_len--;
            topo_order[last_order] = cur;
            topo_order_idx[cur] = last_order;
            last_order++;
        } else {
            stk_len--;
        }
    }

    // Check all rules are reachable
    for (int i = 0; i < n_rules; i++) {
        if (opened[i] == 0) {
            io::printfn("Warning: unreachable rule: %s", rules[i].name);
        }
    }

    // Build reordered clauses with reindexed children
    Clause[] clauses = mem::new_array(Clause, (usz)n_rules);
    String[] names = mem::new_array(String, (usz)n_rules);

    for (int i = 0; i < n_rules; i++) {
        int orig_idx = topo_order[i];
        names[i] = rules[orig_idx].name;
        clauses[i] = reindex_clause(&rules[orig_idx].clause, topo_order_idx);
    }

    // Compute can_match_epsilon via fixed-point iteration
    bool[] emptiable = mem::new_array(bool, (usz)n_rules);
    for (int i = 0; i < n_rules; i++) emptiable[i] = false;

    // Build parent_clauses (reverse edges)
    int[][] parent_clauses_arr = mem::new_array(int[], (usz)n_rules);
    // Count parents first
    int[] parent_counts = mem::new_array(int, (usz)n_rules);
    for (int i = 0; i < n_rules; i++) parent_counts[i] = 0;
    for (int i = 0; i < n_rules; i++) {
        int[] cc = child_clauses(&clauses[i]);
        for (int j = 0; j < (int)cc.len; j++) {
            parent_counts[cc[j]]++;
        }
    }
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

    // Use a full queue for fixed-point iteration
    PikaQueue q = pq_full(n_rules);

    while (!q.is_empty()) {
        int cur = q.pop();
        int[] cc = child_clauses(&clauses[cur]);
        // Collect child emptiability
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

    // Compute seed_clauses (inverse of seeded_by)
    // First compute seeded_by for each clause, then invert
    // Count seeds per clause
    int[] seed_counts = mem::new_array(int, (usz)n_rules);
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

    // Collect terminal indices
    int term_count = 0;
    for (int i = 0; i < n_rules; i++) {
        if (is_terminal(&clauses[i])) term_count++;
    }
    int[] terminals = mem::new_array(int, (usz)term_count);
    int ti = 0;
    for (int i = 0; i < n_rules; i++) {
        if (is_terminal(&clauses[i])) {
            terminals[ti] = i;
            ti++;
        }
    }

    // Clean up temporary arrays
    mem::free(opened.ptr);
    mem::free(stk.ptr);
    mem::free(topo_order.ptr);
    mem::free(topo_order_idx.ptr);
    mem::free(edges.ptr);
    mem::free(parent_counts.ptr);
    mem::free(seed_counts.ptr);
    mem::free(rule_idx_map.ptr);

    // Don't free parent_clauses_arr entries - they may still be useful
    mem::free(parent_clauses_arr.ptr);

    Grammar grammar;
    grammar.names = names;
    grammar.clauses = clauses;
    grammar.can_match_epsilon = emptiable;
    grammar.seed_clauses = seed_cls;
    grammar.terminals = terminals;
    grammar.num_clauses = n_rules;
    return grammar;
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

// Reindex a clause's children from original indices to topological order indices
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
            // terminals and zero-child clauses: no reindexing needed
            ;
    }
    return c;
}
