module lisp;

import std::core::mem;

fn Value* deduce_rule_validate_aggregate_projections(DeduceRuleIr* rule, Interp* interp) {
    if (rule == null) return null;
    if (rule.max_var_id == 0) return null;

    bool has_aggregate_projection = false;
    for (usz i = 0; i < rule.head.term_count; i++) {
        if (rule.head.terms[i].kind == DEDUCE_RULE_TERM_AGGREGATE) {
            has_aggregate_projection = true;
            break;
        }
    }
    if (!has_aggregate_projection) return null;

    bool[] bound_in_positive_body = mem::new_array(bool, (usz)rule.max_var_id + 1);
    if (bound_in_positive_body.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(bound_in_positive_body.ptr);
    for (usz i = 0; i < bound_in_positive_body.len; i++) bound_in_positive_body[i] = false;

    for (usz i = 0; i < rule.body_count; i++) {
        DeduceRuleAtom atom = rule.body[i];
        if (atom.negated) continue;
        for (usz t = 0; t < atom.term_count; t++) {
            DeduceRuleTerm term = atom.terms[t];
            if (term.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
            if (term.normalized_var_id < bound_in_positive_body.len) {
                bound_in_positive_body[term.normalized_var_id] = true;
            }
        }
    }

    for (usz i = 0; i < rule.head.term_count; i++) {
        DeduceRuleTerm term = rule.head.terms[i];
        if (term.kind == DEDUCE_RULE_TERM_LITERAL) {
            return deduce_raise(interp, "deduce/invalid-aggregate", "deduce-rule!: aggregate heads may only contain group-key variables and aggregate projections");
        }
        if (term.kind != DEDUCE_RULE_TERM_AGGREGATE) continue;
        if (term.normalized_var_id >= bound_in_positive_body.len ||
            !bound_in_positive_body[term.normalized_var_id]) {
            return deduce_raise(interp, "deduce/invalid-aggregate", "deduce-rule!: aggregate input variable is not grounded by a positive body atom");
        }
    }
    return null;
}

fn bool deduce_rule_has_aggregate_head(DeduceRuleIr* rule) @inline {
    if (rule == null) return false;
    for (usz i = 0; i < rule.head.term_count; i++) {
        if (rule.head.terms[i].kind == DEDUCE_RULE_TERM_AGGREGATE) return true;
    }
    return false;
}

fn bool deduce_rule_aggregate_head_matches_signature(
    DeduceRuleIr* rule,
    DeduceRuleSignature* sig
) {
    if (rule == null || sig == null) return false;
    if (!deduce_rule_has_aggregate_head(rule) || !deduce_rule_signature_has_aggregates(sig)) return false;
    if (rule.head.term_count != sig.head_term_count) return false;
    if (sig.head_term_kinds == null || sig.head_term_aggregate_ops == null || sig.head_term_literals == null) return false;

    for (usz i = 0; i < rule.head.term_count; i++) {
        DeduceRuleTerm term = rule.head.terms[i];
        if (term.kind != sig.head_term_kinds[i]) return false;
        switch (term.kind) {
            case DEDUCE_RULE_TERM_VARIABLE:
                break;
            case DEDUCE_RULE_TERM_LITERAL:
                if (!values_equal(term.literal_value, sig.head_term_literals[i])) return false;
                break;
            case DEDUCE_RULE_TERM_AGGREGATE:
                if (term.aggregate_op != sig.head_term_aggregate_ops[i]) return false;
                break;
        }
    }
    return true;
}

fn Value* deduce_rule_validate_aggregate_head_migration(
    DeduceDb* db,
    DeduceRuleIr* rule,
    Interp* interp
) {
    if (db == null || rule == null) return null;

    bool candidate_has_aggregate = deduce_rule_has_aggregate_head(rule);
    bool existing_head_has_aggregate = false;
    bool existing_same_head = false;
    bool incompatible_existing_aggregate = false;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != rule.head.predicate) continue;
        existing_same_head = true;
        if (deduce_rule_signature_has_aggregates(sig)) {
            existing_head_has_aggregate = true;
            if (candidate_has_aggregate && !deduce_rule_aggregate_head_matches_signature(rule, sig)) {
                incompatible_existing_aggregate = true;
            }
        }
    }

    if (candidate_has_aggregate && existing_same_head &&
        (!existing_head_has_aggregate || incompatible_existing_aggregate)) {
        return deduce_raise(interp, "deduce/unsupported-aggregate",
            "deduce-rule!: aggregate head predicates may not share definitions with other rules");
    }
    if (!candidate_has_aggregate && existing_head_has_aggregate) {
        return deduce_raise(interp, "deduce/unsupported-aggregate",
            "deduce-rule!: aggregate head predicates may not share definitions with other rules");
    }
    return null;
}

fn Value* deduce_rule_validate_aggregate_recursion(
    DeduceDb* db,
    DeduceRuleIr* rule,
    Interp* interp
) {
    if (db == null || rule == null || !deduce_rule_has_aggregate_head(rule)) return null;
    return null;
}

fn Value* deduce_rule_validate_stratification(DeduceDb* db, DeduceRuleIr* candidate, Interp* interp) {
    DeduceSccPlan graph = {};
    Value* graph_err = deduce_rule_dependency_graph_build(db, candidate, &graph, interp);
    if (graph_err != null) return graph_err;
    defer deduce_rule_dependency_graph_free(&graph);

    usz n = graph.predicates.len;
    if (n == 0) return null;

    bool[] reach = mem::new_array(bool, n * n);
    if (reach.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(reach.ptr);
    for (usz i = 0; i < reach.len; i++) reach[i] = false;

    for (usz i = 0; i < graph.edges.len; i++) {
        DeduceRuleGraphEdge e = graph.edges[i];
        if (e.from < 0 || e.to < 0) continue;
        reach[(usz)e.from * n + (usz)e.to] = true;
    }

    for (usz k = 0; k < n; k++) {
        for (usz i = 0; i < n; i++) {
            if (!reach[i * n + k]) continue;
            for (usz j = 0; j < n; j++) {
                if (!reach[k * n + j]) continue;
                reach[i * n + j] = true;
            }
        }
    }

    for (usz i = 0; i < graph.edges.len; i++) {
        DeduceRuleGraphEdge e = graph.edges[i];
        if (!e.negated) continue;
        if (e.from < 0 || e.to < 0) continue;
        if (reach[(usz)e.to * n + (usz)e.from]) {
            return deduce_raise(interp, "deduce/non-stratifiable-program", "deduce-rule!: rule introduces a negative recursion cycle");
        }
    }
    return null;
}

fn Value* deduce_rule_validate_constrained_head_relation(
    DeduceDb* db,
    DeduceRuleIr* rule,
    Interp* interp
) {
    if (db == null || rule == null || interp == null) return null;
    return null;
}
