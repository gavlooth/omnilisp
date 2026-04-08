module lisp;

import std::core::mem;
import std::collections::list;

fn Value* deduce_build_scc_plan_components(
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

    return null;
}
