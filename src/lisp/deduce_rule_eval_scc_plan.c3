module lisp;

import std::core::mem;
import std::collections::list;

fn Value* deduce_build_scc_plan(
    DeduceDb* db,
    DeduceSccPlan* out,
    Interp* interp
) {
    Value* component_err = deduce_build_scc_plan_components(db, out, interp);
    if (component_err != null) return component_err;

    out.component_order = mem::new_array(usz, out.component_count);
    if (out.component_order.ptr == null && out.component_count > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }

    if (out.component_count > 0 && out.component_count > usz.max / out.component_count) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    usz reversed_edge_count = out.component_count * out.component_count;
    bool[] reversed_edges = mem::new_array(bool, reversed_edge_count);
    if (reversed_edges.ptr == null && reversed_edge_count > 0) {
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
