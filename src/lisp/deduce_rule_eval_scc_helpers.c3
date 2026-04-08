module lisp;

import std::core::mem;
import std::collections::list;

struct DeduceSccPlan {
    SymbolId[]            predicates;
    DeduceRuleGraphEdge[] edges;
    int[]                 predicate_component;
    usz                   component_count;
    bool[]                component_recursive;
    usz[]                 component_strata;
    usz                   stratum_count;
    usz[]                 component_order;
    usz[]                 rule_component_ids;
}

fn void deduce_rule_dependency_graph_free(DeduceSccPlan* graph) {
    deduce_scc_plan_free(graph);
}

fn Value* deduce_rule_dependency_graph_build(
    DeduceDb* db,
    DeduceRuleIr* candidate,
    DeduceSccPlan* out,
    Interp* interp
) {
    if (db == null || out == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid SCC state");
    }
    *out = {};

    List{SymbolId} predicate_list = {};
    defer predicate_list.free();
    List{DeduceRuleGraphEdge} edge_list = {};
    defer edge_list.free();

    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature sig = db.rule_signatures[i];
        int from = deduce_rule_get_or_add_predicate_index(&predicate_list, sig.head_predicate);
        for (usz b = 0; b < sig.body_count; b++) {
            int to = deduce_rule_get_or_add_predicate_index(&predicate_list, sig.body_predicates[b]);
            edge_list.push({ .from = from, .to = to, .negated = sig.body_negated[b] });
        }
    }

    if (candidate != null) {
        int cand_from = deduce_rule_get_or_add_predicate_index(&predicate_list, candidate.head.predicate);
        for (usz i = 0; i < candidate.body_count; i++) {
            DeduceRuleAtom atom = candidate.body[i];
            int to = deduce_rule_get_or_add_predicate_index(&predicate_list, atom.predicate);
            edge_list.push({ .from = cand_from, .to = to, .negated = atom.negated });
        }
    }

    out.predicates = mem::new_array(SymbolId, predicate_list.len());
    if (out.predicates.ptr == null && predicate_list.len() > 0) {
        deduce_rule_dependency_graph_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    for (usz i = 0; i < predicate_list.len(); i++) out.predicates[i] = predicate_list[i];

    out.edges = mem::new_array(DeduceRuleGraphEdge, edge_list.len());
    if (out.edges.ptr == null && edge_list.len() > 0) {
        deduce_rule_dependency_graph_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    for (usz i = 0; i < edge_list.len(); i++) out.edges[i] = edge_list[i];

    return null;
}

fn void deduce_scc_plan_free(DeduceSccPlan* plan) {
    if (plan == null) return;
    if (plan.predicates.ptr != null) {
        mem::free(plan.predicates.ptr);
        plan.predicates = {};
    }
    if (plan.edges.ptr != null) {
        mem::free(plan.edges.ptr);
        plan.edges = {};
    }
    if (plan.predicate_component.ptr != null) {
        mem::free(plan.predicate_component.ptr);
        plan.predicate_component = {};
    }
    if (plan.component_recursive.ptr != null) {
        mem::free(plan.component_recursive.ptr);
        plan.component_recursive = {};
    }
    if (plan.component_strata.ptr != null) {
        mem::free(plan.component_strata.ptr);
        plan.component_strata = {};
    }
    if (plan.component_order.ptr != null) {
        mem::free(plan.component_order.ptr);
        plan.component_order = {};
    }
    if (plan.rule_component_ids.ptr != null) {
        mem::free(plan.rule_component_ids.ptr);
        plan.rule_component_ids = {};
    }
    plan.component_count = 0;
    plan.stratum_count = 0;
}

fn int deduce_scc_find_predicate_index(SymbolId[] predicates, SymbolId predicate) {
    for (usz i = 0; i < predicates.len; i++) {
        if (predicates[i] == predicate) return (int)i;
    }
    return -1;
}

fn void deduce_scc_tarjan_visit(
    int node,
    DeduceRuleGraphEdge[] edges,
    int[] node_index,
    int[] lowlink,
    bool[] on_stack,
    List{int}* stack,
    int* next_index,
    int[] component_of_node,
    usz* component_count
) {
    node_index[(usz)node] = *next_index;
    lowlink[(usz)node] = *next_index;
    *next_index = *next_index + 1;
    stack.push(node);
    on_stack[(usz)node] = true;

    foreach (edge : edges) {
        if (edge.from != node) continue;
        int to = edge.to;
        if (to < 0 || (usz)to >= node_index.len) continue;

        if (node_index[(usz)to] < 0) {
            deduce_scc_tarjan_visit(
                to,
                edges,
                node_index,
                lowlink,
                on_stack,
                stack,
                next_index,
                component_of_node,
                component_count
            );
            if (lowlink[(usz)to] < lowlink[(usz)node]) {
                lowlink[(usz)node] = lowlink[(usz)to];
            }
        } else if (on_stack[(usz)to] && node_index[(usz)to] < lowlink[(usz)node]) {
            lowlink[(usz)node] = node_index[(usz)to];
        }
    }

    if (lowlink[(usz)node] != node_index[(usz)node]) return;

    while (stack.len() > 0) {
        int w = 0;
        if (try popped = stack.pop()) {
            w = popped;
        } else {
            break;
        }
        if (w < 0 || (usz)w >= on_stack.len || (usz)w >= component_of_node.len) break;
        on_stack[(usz)w] = false;
        component_of_node[(usz)w] = (int)*component_count;
        if (w == node) break;
    }
    *component_count = *component_count + 1;
}
