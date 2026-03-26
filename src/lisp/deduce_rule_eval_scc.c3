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

fn Value* deduce_build_scc_plan(
    DeduceDb* db,
    DeduceSccPlan* out,
    Interp* interp
) {
    Value* graph_err = deduce_rule_dependency_graph_build(db, null, out, interp);
    if (graph_err != null) return graph_err;

    out.predicate_component = mem::new_array(int, out.predicates.len);
    if (out.predicate_component.ptr == null && out.predicates.len > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    for (usz i = 0; i < out.predicate_component.len; i++) out.predicate_component[i] = -1;

    int[] node_index = mem::new_array(int, out.predicates.len);
    if (node_index.ptr == null && out.predicates.len > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (node_index.ptr != null) mem::free(node_index.ptr);
    for (usz i = 0; i < node_index.len; i++) node_index[i] = -1;

    int[] lowlink = mem::new_array(int, out.predicates.len);
    if (lowlink.ptr == null && out.predicates.len > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (lowlink.ptr != null) mem::free(lowlink.ptr);
    for (usz i = 0; i < lowlink.len; i++) lowlink[i] = -1;

    bool[] on_stack = mem::new_array(bool, out.predicates.len);
    if (on_stack.ptr == null && out.predicates.len > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (on_stack.ptr != null) mem::free(on_stack.ptr);
    for (usz i = 0; i < on_stack.len; i++) on_stack[i] = false;

    List{int} stack = {};
    defer stack.free();

    int next_index = 0;
    out.component_count = 0;
    for (usz i = 0; i < out.predicates.len; i++) {
        if (node_index[i] >= 0) continue;
        deduce_scc_tarjan_visit(
            (int)i,
            out.edges,
            node_index,
            lowlink,
            on_stack,
            &stack,
            &next_index,
            out.predicate_component,
            &out.component_count
        );
    }

    out.component_recursive = mem::new_array(bool, out.component_count);
    if (out.component_recursive.ptr == null && out.component_count > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    for (usz i = 0; i < out.component_recursive.len; i++) out.component_recursive[i] = false;

    usz[] component_sizes = mem::new_array(usz, out.component_count);
    if (component_sizes.ptr == null && out.component_count > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (component_sizes.ptr != null) mem::free(component_sizes.ptr);
    for (usz i = 0; i < component_sizes.len; i++) component_sizes[i] = 0;

    for (usz i = 0; i < out.predicate_component.len; i++) {
        int comp = out.predicate_component[i];
        if (comp < 0 || (usz)comp >= component_sizes.len) continue;
        component_sizes[(usz)comp]++;
    }
    for (usz c = 0; c < component_sizes.len; c++) {
        if (component_sizes[c] > 1) out.component_recursive[c] = true;
    }
    foreach (edge : out.edges) {
        if (edge.from < 0 || edge.to < 0) continue;
        if ((usz)edge.from >= out.predicate_component.len || (usz)edge.to >= out.predicate_component.len) continue;
        int from_comp = out.predicate_component[(usz)edge.from];
        int to_comp = out.predicate_component[(usz)edge.to];
        if (from_comp < 0 || to_comp < 0 || from_comp != to_comp) continue;
        if (edge.from == edge.to) out.component_recursive[(usz)from_comp] = true;
    }

    out.rule_component_ids = mem::new_array(usz, db.rule_signature_count);
    if (out.rule_component_ids.ptr == null && db.rule_signature_count > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    for (usz i = 0; i < db.rule_signature_count; i++) {
        int pred_idx = deduce_scc_find_predicate_index(out.predicates, db.rule_signatures[i].head_predicate);
        if (pred_idx < 0 || (usz)pred_idx >= out.predicate_component.len) {
            deduce_scc_plan_free(out);
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: SCC plan missing head predicate");
        }
        int comp = out.predicate_component[(usz)pred_idx];
        if (comp < 0 || (usz)comp >= out.component_count) {
            deduce_scc_plan_free(out);
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid rule SCC assignment");
        }
        out.rule_component_ids[i] = (usz)comp;
    }

    out.component_order = mem::new_array(usz, out.component_count);
    if (out.component_order.ptr == null && out.component_count > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }

    bool[] reversed_edges = mem::new_array(bool, out.component_count * out.component_count);
    if (reversed_edges.ptr == null && out.component_count > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (reversed_edges.ptr != null) mem::free(reversed_edges.ptr);
    for (usz i = 0; i < reversed_edges.len; i++) reversed_edges[i] = false;

    int[] indegree = mem::new_array(int, out.component_count);
    if (indegree.ptr == null && out.component_count > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (indegree.ptr != null) mem::free(indegree.ptr);
    for (usz i = 0; i < indegree.len; i++) indegree[i] = 0;

    foreach (edge : out.edges) {
        if (edge.from < 0 || edge.to < 0) continue;
        if ((usz)edge.from >= out.predicate_component.len || (usz)edge.to >= out.predicate_component.len) continue;
        int from_comp = out.predicate_component[(usz)edge.from];
        int to_comp = out.predicate_component[(usz)edge.to];
        if (from_comp < 0 || to_comp < 0 || from_comp == to_comp) continue;
        usz rev_from = (usz)to_comp;
        usz rev_to = (usz)from_comp;
        usz edge_idx = rev_from * out.component_count + rev_to;
        if (edge_idx >= reversed_edges.len || reversed_edges[edge_idx]) continue;
        reversed_edges[edge_idx] = true;
        indegree[rev_to] = indegree[rev_to] + 1;
    }

    bool[] placed = mem::new_array(bool, out.component_count);
    if (placed.ptr == null && out.component_count > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (placed.ptr != null) mem::free(placed.ptr);
    for (usz i = 0; i < placed.len; i++) placed[i] = false;

    for (usz pos = 0; pos < out.component_count; pos++) {
        int selected = -1;
        for (usz c = 0; c < out.component_count; c++) {
            if (placed[c]) continue;
            if (indegree[c] == 0) {
                selected = (int)c;
                break;
            }
        }
        if (selected < 0) {
            for (usz c = 0; c < out.component_count; c++) {
                if (!placed[c]) {
                    selected = (int)c;
                    break;
                }
            }
        }
        if (selected < 0) {
            deduce_scc_plan_free(out);
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: SCC ordering failed");
        }

        usz selected_comp = (usz)selected;
        out.component_order[pos] = selected_comp;
        placed[selected_comp] = true;
        for (usz to = 0; to < out.component_count; to++) {
            usz edge_idx = selected_comp * out.component_count + to;
            if (edge_idx >= reversed_edges.len || !reversed_edges[edge_idx]) continue;
            if (indegree[to] > 0) indegree[to] = indegree[to] - 1;
        }
    }

    out.component_strata = mem::new_array(usz, out.component_count);
    if (out.component_strata.ptr == null && out.component_count > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    for (usz i = 0; i < out.component_count; i++) out.component_strata[i] = 0;

    usz relax_limit = out.component_count * (out.edges.len + 1);
    if (relax_limit < out.component_count + 1) relax_limit = out.component_count + 1;
    bool converged = out.component_count == 0;
    for (usz pass = 0; pass < relax_limit; pass++) {
        bool changed = false;
        foreach (edge : out.edges) {
            if (edge.from < 0 || edge.to < 0) continue;
            if ((usz)edge.from >= out.predicate_component.len || (usz)edge.to >= out.predicate_component.len) continue;
            int from_comp = out.predicate_component[(usz)edge.from];
            int to_comp = out.predicate_component[(usz)edge.to];
            if (from_comp < 0 || to_comp < 0) continue;
            if (edge.negated && from_comp == to_comp) {
                deduce_scc_plan_free(out);
                return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: negated dependency remained within one SCC");
            }
            if (from_comp == to_comp) continue;
            if ((usz)from_comp >= out.component_strata.len || (usz)to_comp >= out.component_strata.len) continue;

            usz required_stratum = out.component_strata[(usz)to_comp];
            if (edge.negated) {
                if (required_stratum == usz.max) {
                    deduce_scc_plan_free(out);
                    return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: stratum index overflow");
                }
                required_stratum++;
            }
            if (required_stratum > out.component_strata[(usz)from_comp]) {
                out.component_strata[(usz)from_comp] = required_stratum;
                changed = true;
            }
        }
        if (!changed) {
            converged = true;
            break;
        }
    }
    if (!converged) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: stratum scheduling did not converge");
    }

    out.stratum_count = 0;
    for (usz c = 0; c < out.component_strata.len; c++) {
        usz next = out.component_strata[c] + 1;
        if (next > out.stratum_count) out.stratum_count = next;
    }

    usz[] topo_order = mem::new_array(usz, out.component_count);
    if (topo_order.ptr == null && out.component_count > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (topo_order.ptr != null) mem::free(topo_order.ptr);
    for (usz i = 0; i < out.component_count; i++) topo_order[i] = out.component_order[i];

    int[] topo_rank = mem::new_array(int, out.component_count);
    if (topo_rank.ptr == null && out.component_count > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (topo_rank.ptr != null) mem::free(topo_rank.ptr);
    for (usz i = 0; i < topo_rank.len; i++) topo_rank[i] = int.max;
    for (usz i = 0; i < topo_order.len; i++) {
        if (topo_order[i] < topo_rank.len) topo_rank[topo_order[i]] = (int)i;
    }

    bool[] stratum_placed = mem::new_array(bool, out.component_count);
    if (stratum_placed.ptr == null && out.component_count > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (stratum_placed.ptr != null) mem::free(stratum_placed.ptr);
    for (usz i = 0; i < stratum_placed.len; i++) stratum_placed[i] = false;

    for (usz pos = 0; pos < out.component_count; pos++) {
        int selected = -1;
        usz selected_stratum = usz.max;
        int selected_rank = int.max;
        for (usz c = 0; c < out.component_count; c++) {
            if (stratum_placed[c]) continue;
            usz candidate_stratum = out.component_strata[c];
            int candidate_rank = topo_rank[c];
            if (selected < 0 ||
                candidate_stratum < selected_stratum ||
                (candidate_stratum == selected_stratum && candidate_rank < selected_rank)) {
                selected = (int)c;
                selected_stratum = candidate_stratum;
                selected_rank = candidate_rank;
            }
        }
        if (selected < 0) {
            deduce_scc_plan_free(out);
            return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: stratum scheduling failed");
        }
        usz selected_comp = (usz)selected;
        out.component_order[pos] = selected_comp;
        stratum_placed[selected_comp] = true;
    }

    return null;
}

fn Value* deduce_build_parallel_component_batch_metadata(
    DeduceSccPlan* plan,
    usz[]* wave_ids_out,
    usz[]* batch_sizes_out,
    Interp* interp
) {
    if (wave_ids_out == null || batch_sizes_out == null || plan == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state",
            "deduce/analyze: invalid parallel batch state");
    }

    *wave_ids_out = {};
    *batch_sizes_out = {};
    if (plan.component_count == 0) return null;

    usz[] wave_ids = mem::new_array(usz, plan.component_count);
    if (wave_ids.ptr == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    usz[] batch_sizes = mem::new_array(usz, plan.component_count);
    if (batch_sizes.ptr == null) {
        mem::free(wave_ids.ptr);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }

    for (usz i = 0; i < wave_ids.len; i++) {
        wave_ids[i] = 0;
        batch_sizes[i] = 0;
    }

    for (usz pos = 0; pos < plan.component_order.len; pos++) {
        usz component_id = plan.component_order[pos];
        if (component_id >= plan.component_count || component_id >= plan.component_strata.len) continue;

        usz stratum = plan.component_strata[component_id];
        usz wave = 0;
        foreach (edge : plan.edges) {
            if (edge.from < 0 || edge.to < 0) continue;
            if ((usz)edge.from >= plan.predicate_component.len || (usz)edge.to >= plan.predicate_component.len) continue;

            int from_component = plan.predicate_component[(usz)edge.from];
            int to_component = plan.predicate_component[(usz)edge.to];
            if (from_component < 0 || to_component < 0 || from_component == to_component) continue;
            if ((usz)from_component != component_id) continue;
            if ((usz)to_component >= plan.component_strata.len || plan.component_strata[(usz)to_component] != stratum) continue;

            usz candidate_wave = wave_ids[(usz)to_component] + 1;
            if (candidate_wave > wave) wave = candidate_wave;
        }
        wave_ids[component_id] = wave;
    }

    for (usz component_id = 0; component_id < plan.component_count; component_id++) {
        if (component_id >= plan.component_strata.len || component_id >= wave_ids.len) continue;
        usz count = 0;
        usz stratum = plan.component_strata[component_id];
        usz wave = wave_ids[component_id];
        for (usz other = 0; other < plan.component_count; other++) {
            if (other >= plan.component_strata.len || other >= wave_ids.len) continue;
            if (plan.component_strata[other] != stratum) continue;
            if (wave_ids[other] != wave) continue;
            count++;
        }
        batch_sizes[component_id] = count;
    }

    *wave_ids_out = wave_ids;
    *batch_sizes_out = batch_sizes;
    return null;
}

fn Value* deduce_parallel_component_batch_topology_payload(
    DeduceSccPlan* plan,
    usz[] wave_ids,
    usz[] batch_sizes,
    Interp* interp
) {
    if (plan == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid parallel batch state");
    }
    if (plan.component_count == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz component_id = 0; component_id < plan.component_count; component_id++) {
        bool recursive =
            plan.component_recursive.ptr != null &&
            component_id < plan.component_recursive.len &&
            plan.component_recursive[component_id];
        if (!recursive) continue;

        bool batch_seen = false;
        if (component_id < wave_ids.len && component_id < plan.component_strata.len) {
            usz stratum = plan.component_strata[component_id];
            usz wave = wave_ids[component_id];
            for (usz prev = 0; prev < component_id; prev++) {
                bool prev_recursive =
                    plan.component_recursive.ptr != null &&
                    prev < plan.component_recursive.len &&
                    plan.component_recursive[prev];
                if (!prev_recursive) continue;
                if (prev >= wave_ids.len || prev >= plan.component_strata.len) continue;
                if (plan.component_strata[prev] != stratum) continue;
                if (wave_ids[prev] != wave) continue;
                batch_seen = true;
                break;
            }
        }
        if (batch_seen) continue;

        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }

        Value* predicate_rev = make_nil(interp);
        for (usz predicate_id = 0; predicate_id < plan.predicates.len; predicate_id++) {
            if (predicate_id >= plan.predicate_component.len) continue;
            if (plan.predicate_component[predicate_id] != (int)component_id) continue;
            predicate_rev = make_cons(
                interp,
                make_symbol(interp, plan.predicates[predicate_id]),
                predicate_rev
            );
        }

        explain_dict_set(entry, interp, "leader-component-id", make_int(interp, (long)component_id));
        explain_dict_set(entry, interp, "stratum",
            make_int(interp, component_id < plan.component_strata.len ? (long)plan.component_strata[component_id] : 0));
        explain_dict_set(entry, interp, "wave",
            make_int(interp, component_id < wave_ids.len ? (long)wave_ids[component_id] : 0));
        explain_dict_set(entry, interp, "batch-size",
            make_int(interp, component_id < batch_sizes.len ? (long)batch_sizes[component_id] : 0));
        explain_dict_set(entry, interp, "component-predicates", deduce_reverse_list(predicate_rev, interp));
        rev = make_cons(interp, entry, rev);
    }

    return deduce_reverse_list(rev, interp);
}
