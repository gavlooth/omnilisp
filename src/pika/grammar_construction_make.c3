module pika;

import std::core::mem;

fn Grammar make_grammar(String[] starts, RuleEntry[] rules) {
    int n_rules = (int)rules.len;

    int[][] edges = mem::new_array(int[], (usz)n_rules);
    for (int i = 0; i < n_rules; i++) {
        edges[i] = child_clauses(&rules[i].clause);
    }

    int[] opened = mem::new_array(int, (usz)n_rules);
    int[] stk = mem::new_array(int, (usz)(n_rules + 1));
    int[] topo_order = mem::new_array(int, (usz)n_rules);
    int[] topo_order_idx = mem::new_array(int, (usz)n_rules);
    int last_order = 0;
    build_topo_order(
        starts, rules, edges, opened, stk, topo_order, topo_order_idx, &last_order
    );
    int reachable_count = last_order;
    assert(reachable_count > 0, "make_grammar: no reachable rules from starts");

    Clause[] clauses = mem::new_array(Clause, (usz)reachable_count);
    String[] names = mem::new_array(String, (usz)reachable_count);
    build_reordered_rules(rules, topo_order, topo_order_idx, clauses, names, reachable_count);

    bool[] emptiable = mem::new_array(bool, (usz)n_rules);

    int[] parent_counts = mem::new_array(int, (usz)n_rules);
    int[][] parent_clauses_arr = build_parent_clauses(clauses, parent_counts);
    compute_emptiable_clauses(clauses, parent_clauses_arr, parent_counts, emptiable);

    int[] seed_counts = mem::new_array(int, (usz)n_rules);
    int[][] seed_cls = build_seed_clauses(clauses, emptiable, seed_counts);
    int[] terminals = collect_terminal_indices(clauses);

    mem::free(opened.ptr);
    mem::free(stk.ptr);
    mem::free(topo_order.ptr);
    mem::free(topo_order_idx.ptr);
    mem::free(edges.ptr);
    mem::free(parent_counts.ptr);
    mem::free(seed_counts.ptr);
    for (usz i = 0; i < parent_clauses_arr.len; i++) {
        if (parent_clauses_arr[i].len > 0) mem::free(parent_clauses_arr[i].ptr);
    }
    mem::free(parent_clauses_arr.ptr);

    Grammar grammar;
    grammar.names = names;
    grammar.clauses = clauses;
    grammar.can_match_epsilon = emptiable;
    grammar.seed_clauses = seed_cls;
    grammar.terminals = terminals;
    grammar.num_clauses = reachable_count;
    return grammar;
}
