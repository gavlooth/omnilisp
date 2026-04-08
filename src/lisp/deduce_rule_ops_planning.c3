module lisp;

import std::collections::list;

fn ulong deduce_rule_projection_mask_for_step(
    DeduceRuleIr* rule,
    DeduceBodyPlanStep[] plan_steps,
    usz plan_step_index
) {
    if (rule == null || plan_step_index >= plan_steps.len) return 0;
    usz logical_idx = plan_steps[plan_step_index].logical_body_index;
    DeduceRuleAtom* atom = &rule.body[logical_idx];
    ulong mask = 0;
    usz width = (usz)(ulong.sizeof * 8);
    for (usz t = 0; t < atom.term_count; t++) {
        DeduceRuleTerm term = atom.terms[t];
        if (term.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
        bool needed = deduce_rule_var_used_in_head(rule, term.normalized_var_id) ||
                      deduce_rule_var_used_in_planned_suffix(rule, plan_steps, plan_step_index, term.normalized_var_id);
        if (!needed || t >= width) continue;
        mask |= ((ulong)1 << t);
    }
    return mask;
}

fn int deduce_rule_plan_step_score(
    DeduceRuleAtom* atom,
    ulong bound_mask,
    int selected_index,
    DeduceRelationSchema* schema
) {
    int score = deduce_rule_bound_mask_popcount(bound_mask) * 32;
    if (selected_index >= 0) score += 256;
    if (atom != null && atom.negated) score -= 64;
    if (schema != null) {
        ulong card = schema.cardinality_estimate;
        ulong distinct = schema.distinct_estimate;
        if (card == 0) {
            score += 4;
        } else if (card > 16384) {
            score -= 16;
        } else if (card > 2048) {
            score -= 8;
        } else if (card > 256) {
            score -= 4;
        }
        if (distinct > 0 && distinct < card / 2) score += 4;
    }
    return score;
}

fn Value* deduce_plan_conjunctive_body(
    DeduceDb* db,
    DeduceRuleAtom* body_atoms,
    usz body_count,
    uint max_var_id,
    DeduceBodyPlanStep* out_steps,
    usz out_step_count,
    Interp* interp
) {
    if (db == null || body_atoms == null || out_steps == null) {
        return deduce_raise(interp, "deduce/planner-no-valid-plan", "deduce-rule!: planner could not build a valid body plan");
    }
    if (body_count != out_step_count) {
        return deduce_raise(interp, "deduce/planner-no-valid-plan", "deduce-rule!: planner output shape mismatch");
    }

    usz bound_len = (usz)max_var_id + 1;
    if (bound_len == 0) bound_len = 1;
    bool[] bound_vars = mem::new_array(bool, bound_len);
    if (bound_vars.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(bound_vars.ptr);
    for (usz i = 0; i < bound_vars.len; i++) bound_vars[i] = false;

    bool[] planned = mem::new_array(bool, body_count);
    if (planned.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(planned.ptr);
    for (usz i = 0; i < planned.len; i++) planned[i] = false;

    for (usz step_idx = 0; step_idx < body_count; step_idx++) {
        bool best_found = false;
        usz best_logical_idx = 0;
        int best_score = 0;
        ulong best_mask = 0;
        int best_selected_index = -1;

        for (usz logical_idx = 0; logical_idx < body_count; logical_idx++) {
            if (planned[logical_idx]) continue;

            DeduceRuleAtom* atom = &body_atoms[logical_idx];
            ulong bound_mask = deduce_rule_atom_bound_mask(atom, bound_vars);

            DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, atom.predicate);
            if (schema == null) {
                return deduce_raise(interp, "deduce/relation-not-found", "deduce-rule!: predicate relation is not registered");
            }
            int selected_index = deduce_schema_select_index_for_bound_mask(schema, bound_mask);
            int score = deduce_rule_plan_step_score(atom, bound_mask, selected_index, schema);

            if (!best_found ||
                score > best_score ||
                (score == best_score && logical_idx < best_logical_idx)) {
                best_found = true;
                best_logical_idx = logical_idx;
                best_score = score;
                best_mask = bound_mask;
                best_selected_index = selected_index;
            }
        }

        if (!best_found) {
            return deduce_raise(interp, "deduce/planner-no-valid-plan", "deduce-rule!: planner could not build a valid body plan");
        }

        DeduceRuleAtom* chosen = &body_atoms[best_logical_idx];
        out_steps[step_idx].logical_body_index = best_logical_idx;
        out_steps[step_idx].bound_mask = best_mask;
        out_steps[step_idx].selected_index = best_selected_index;
        out_steps[step_idx].op_kind = deduce_rule_choose_op_kind(chosen, best_selected_index);

        planned[best_logical_idx] = true;
        if (!chosen.negated) {
            deduce_rule_mark_atom_variables_bound(chosen, bound_vars);
        }
    }
    return null;
}

fn int deduce_rule_get_or_add_predicate_index(List{SymbolId}* preds, SymbolId predicate) {
    for (usz i = 0; i < preds.len(); i++) {
        if ((*preds)[i] == predicate) return (int)i;
    }
    preds.push(predicate);
    return (int)(preds.len() - 1);
}
