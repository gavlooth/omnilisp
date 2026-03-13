module lisp;

import std::core::mem;
import std::collections::list;

enum DeduceRuleTermKind {
    DEDUCE_RULE_TERM_LITERAL,
    DEDUCE_RULE_TERM_VARIABLE,
}

struct DeduceRuleTerm {
    DeduceRuleTermKind kind;
    uint               normalized_var_id;
    Value*             literal_value;
}

struct DeduceRuleAtom {
    SymbolId        predicate;
    bool            negated;
    DeduceRuleTerm* terms;
    usz             term_count;
}

struct DeduceRuleIr {
    DeduceRuleAtom  head;
    DeduceRuleAtom* body;
    usz             body_count;
    uint            max_var_id;
}

struct DeduceRuleVarBinding {
    SymbolId name;
    uint     id;
}

struct DeduceRuleGraphEdge {
    int  from;
    int  to;
    bool negated;
}

enum DeducePhysicalOpKind {
    DEDUCE_PHYSICAL_FULL_SCAN,
    DEDUCE_PHYSICAL_INDEX_SCAN,
    DEDUCE_PHYSICAL_NEGATED_SCAN,
    DEDUCE_PHYSICAL_NEGATED_INDEX_PROBE,
}

struct DeduceBodyPlanStep {
    usz                logical_body_index;
    ulong              bound_mask;
    int                selected_index;
    DeducePhysicalOpKind op_kind;
}

fn bool deduce_rule_is_logic_var_symbol(SymbolId sym, Interp* interp) @inline {
    char[] name = interp.symbols.get_name(sym);
    return name.len > 1 && name[0] == '?';
}

fn uint deduce_rule_get_or_add_var_id(
    List{DeduceRuleVarBinding}* vars,
    SymbolId name,
    uint* max_var_id
) {
    for (usz i = 0; i < vars.len(); i++) {
        if ((*vars)[i].name == name) return (*vars)[i].id;
    }

    if (*max_var_id == uint.max) return 0;
    *max_var_id = *max_var_id + 1;
    vars.push({ .name = name, .id = *max_var_id });
    return *max_var_id;
}

fn void deduce_rule_atom_free(DeduceRuleAtom* atom) {
    if (atom == null) return;
    if (atom.terms != null) {
        mem::free(atom.terms);
        atom.terms = null;
    }
    atom.term_count = 0;
    atom.negated = false;
    atom.predicate = 0;
}

fn void deduce_rule_ir_free(DeduceRuleIr* rule) {
    if (rule == null) return;
    deduce_rule_atom_free(&rule.head);
    if (rule.body != null) {
        for (usz i = 0; i < rule.body_count; i++) {
            deduce_rule_atom_free(&rule.body[i]);
        }
        mem::free(rule.body);
        rule.body = null;
    }
    rule.body_count = 0;
    rule.max_var_id = 0;
}

fn Value* deduce_rule_parse_atom(
    Value* form,
    bool allow_not_wrapper,
    List{DeduceRuleVarBinding}* vars,
    uint* max_var_id,
    DeduceRuleAtom* out,
    Interp* interp
) {
    out.predicate = 0;
    out.negated = false;
    out.terms = null;
    out.term_count = 0;

    Value* atom_form = form;
    if (allow_not_wrapper && is_cons(atom_form)) {
        Value* head = car(atom_form);
        if (head != null && head.tag == SYMBOL) {
            char[] head_name = interp.symbols.get_name(head.sym_val);
            if (deduce_name_equals(head_name, "not")) {
                Value* not_args = cdr(atom_form);
                if (!is_cons(not_args) || !is_nil(cdr(not_args))) {
                    return deduce_raise(interp, "deduce/invalid-rule", "deduce-rule!: (not ...) expects exactly one atom");
                }
                atom_form = car(not_args);
                out.negated = true;
            }
        }
    }

    if (!is_cons(atom_form)) {
        return deduce_raise(interp, "deduce/invalid-rule", "deduce-rule!: atom must be a list");
    }

    Value* predicate = car(atom_form);
    if (predicate == null || predicate.tag != SYMBOL) {
        return deduce_raise(interp, "deduce/invalid-rule", "deduce-rule!: atom predicate must be a symbol");
    }
    out.predicate = predicate.sym_val;

    Value* args = cdr(atom_form);
    usz term_count = 0;
    Value* probe = args;
    while (is_cons(probe)) {
        term_count++;
        probe = cdr(probe);
    }
    if (!is_nil(probe)) {
        return deduce_raise(interp, "deduce/invalid-rule", "deduce-rule!: atom tail must be a proper list");
    }

    if (term_count > 0) {
        out.terms = (DeduceRuleTerm*)mem::malloc(DeduceRuleTerm.sizeof * term_count);
        if (out.terms == null) {
            return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
        }
    }

    out.term_count = term_count;
    Value* curr = args;
    usz idx = 0;
    while (is_cons(curr)) {
        Value* term = car(curr);
        DeduceRuleTerm parsed_term;
        parsed_term.kind = DEDUCE_RULE_TERM_LITERAL;
        parsed_term.normalized_var_id = 0;
        parsed_term.literal_value = term;

        if (term != null && term.tag == SYMBOL &&
            deduce_rule_is_logic_var_symbol(term.sym_val, interp)) {
            uint var_id = deduce_rule_get_or_add_var_id(vars, term.sym_val, max_var_id);
            if (var_id == 0) {
                return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
            }
            parsed_term.kind = DEDUCE_RULE_TERM_VARIABLE;
            parsed_term.normalized_var_id = var_id;
            parsed_term.literal_value = null;
        }

        out.terms[idx] = parsed_term;
        idx++;
        curr = cdr(curr);
    }
    return null;
}

fn Value* deduce_rule_parse_ir(
    Value*[] args,
    DeduceRuleIr* out,
    Interp* interp
) {
    out.head = {};
    out.body = null;
    out.body_count = 0;
    out.max_var_id = 0;

    if (args.len < 2) {
        return deduce_raise(interp, "deduce/invalid-rule", "deduce-rule!: expected head atom and at least one body atom");
    }

    List{DeduceRuleVarBinding} vars = {};
    defer vars.free();

    Value* head_err = deduce_rule_parse_atom(args[0], false, &vars, &out.max_var_id, &out.head, interp);
    if (head_err != null) return head_err;
    if (out.head.negated) {
        return deduce_raise(interp, "deduce/invalid-rule", "deduce-rule!: head atom cannot be negated");
    }

    out.body_count = args.len - 1;
    out.body = (DeduceRuleAtom*)mem::malloc(DeduceRuleAtom.sizeof * out.body_count);
    if (out.body == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }

    for (usz i = 0; i < out.body_count; i++) out.body[i] = {};
    for (usz i = 0; i < out.body_count; i++) {
        Value* body_err = deduce_rule_parse_atom(args[i + 1], true, &vars, &out.max_var_id, &out.body[i], interp);
        if (body_err != null) return body_err;
    }
    return null;
}

fn Value* deduce_rule_validate_arity_for_atom(
    DeduceDb* db,
    DeduceRuleAtom* atom,
    Interp* interp
) {
    usz expected = 0;
    if (!deduce_db_lookup_relation_arity(db, atom.predicate, &expected)) {
        return deduce_raise(interp, "deduce/relation-not-found", "deduce-rule!: predicate relation is not registered");
    }
    if (expected != atom.term_count) {
        return deduce_raise(interp, "deduce/arity-mismatch", "deduce-rule!: predicate arity mismatch");
    }
    return null;
}

fn Value* deduce_rule_validate_arity(DeduceDb* db, DeduceRuleIr* rule, Interp* interp) {
    Value* err = deduce_rule_validate_arity_for_atom(db, &rule.head, interp);
    if (err != null) return err;
    for (usz i = 0; i < rule.body_count; i++) {
        err = deduce_rule_validate_arity_for_atom(db, &rule.body[i], interp);
        if (err != null) return err;
    }
    return null;
}

fn Value* deduce_rule_validate_head_variable_safety(DeduceRuleIr* rule, Interp* interp) {
    if (rule.max_var_id == 0) return null;

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
        if (term.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
        if (term.normalized_var_id >= bound_in_positive_body.len ||
            !bound_in_positive_body[term.normalized_var_id]) {
            return deduce_raise(interp, "deduce/unsafe-rule", "deduce-rule!: head variable is not grounded by a positive body atom");
        }
    }
    return null;
}

fn Value* deduce_rule_validate_negation_safety(DeduceRuleIr* rule, Interp* interp) {
    if (rule.max_var_id == 0) return null;

    bool[] bound_so_far = mem::new_array(bool, (usz)rule.max_var_id + 1);
    if (bound_so_far.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(bound_so_far.ptr);

    for (usz i = 0; i < bound_so_far.len; i++) bound_so_far[i] = false;

    for (usz i = 0; i < rule.body_count; i++) {
        DeduceRuleAtom atom = rule.body[i];
        if (atom.negated) {
            for (usz t = 0; t < atom.term_count; t++) {
                DeduceRuleTerm term = atom.terms[t];
                if (term.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
                if (term.normalized_var_id >= bound_so_far.len ||
                    !bound_so_far[term.normalized_var_id]) {
                    return deduce_raise(interp, "deduce/unsafe-rule", "deduce-rule!: negated atom uses an unbound variable");
                }
            }
            continue;
        }

        for (usz t = 0; t < atom.term_count; t++) {
            DeduceRuleTerm term = atom.terms[t];
            if (term.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
            if (term.normalized_var_id < bound_so_far.len) {
                bound_so_far[term.normalized_var_id] = true;
            }
        }
    }
    return null;
}

fn ulong deduce_rule_atom_bound_mask(DeduceRuleAtom* atom, bool[] bound_vars) {
    if (atom == null) return 0;
    ulong mask = 0;
    usz width = (usz)(ulong.sizeof * 8);
    for (usz t = 0; t < atom.term_count; t++) {
        bool bound = false;
        DeduceRuleTerm term = atom.terms[t];
        if (term.kind == DEDUCE_RULE_TERM_LITERAL) {
            bound = true;
        } else if (term.kind == DEDUCE_RULE_TERM_VARIABLE &&
                   term.normalized_var_id < bound_vars.len &&
                   bound_vars[term.normalized_var_id]) {
            bound = true;
        }

        if (bound && t < width) {
            mask |= ((ulong)1 << t);
        }
    }
    return mask;
}

fn void deduce_rule_mark_atom_variables_bound(DeduceRuleAtom* atom, bool[] bound_vars) {
    if (atom == null) return;
    for (usz t = 0; t < atom.term_count; t++) {
        DeduceRuleTerm term = atom.terms[t];
        if (term.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
        if (term.normalized_var_id >= bound_vars.len) continue;
        bound_vars[term.normalized_var_id] = true;
    }
}

fn int deduce_rule_bound_mask_popcount(ulong mask) {
    int count = 0;
    ulong v = mask;
    while (v != 0) {
        count += (int)(v & 1);
        v >>= 1;
    }
    return count;
}

fn DeducePhysicalOpKind deduce_rule_choose_op_kind(DeduceRuleAtom* atom, int selected_index) {
    if (atom != null && atom.negated) {
        if (selected_index >= 0) return DEDUCE_PHYSICAL_NEGATED_INDEX_PROBE;
        return DEDUCE_PHYSICAL_NEGATED_SCAN;
    }
    if (selected_index >= 0) return DEDUCE_PHYSICAL_INDEX_SCAN;
    return DEDUCE_PHYSICAL_FULL_SCAN;
}

fn bool deduce_rule_atom_has_local_equality_constraint(DeduceRuleAtom* atom) {
    if (atom == null || atom.term_count < 2) return false;
    for (usz i = 0; i < atom.term_count; i++) {
        DeduceRuleTerm ti = atom.terms[i];
        if (ti.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
        for (usz j = i + 1; j < atom.term_count; j++) {
            DeduceRuleTerm tj = atom.terms[j];
            if (tj.kind != DEDUCE_RULE_TERM_VARIABLE) continue;
            if (ti.normalized_var_id == tj.normalized_var_id) return true;
        }
    }
    return false;
}

fn bool deduce_rule_var_used_in_head(DeduceRuleIr* rule, uint var_id) {
    if (rule == null || var_id == 0) return false;
    for (usz i = 0; i < rule.head.term_count; i++) {
        DeduceRuleTerm term = rule.head.terms[i];
        if (term.kind == DEDUCE_RULE_TERM_VARIABLE && term.normalized_var_id == var_id) {
            return true;
        }
    }
    return false;
}

fn bool deduce_rule_var_used_in_planned_suffix(
    DeduceRuleIr* rule,
    DeduceBodyPlanStep[] plan_steps,
    usz from_step_exclusive,
    uint var_id
) {
    if (rule == null || var_id == 0) return false;
    for (usz step = from_step_exclusive + 1; step < plan_steps.len; step++) {
        usz logical_idx = plan_steps[step].logical_body_index;
        DeduceRuleAtom* atom = &rule.body[logical_idx];
        for (usz t = 0; t < atom.term_count; t++) {
            DeduceRuleTerm term = atom.terms[t];
            if (term.kind == DEDUCE_RULE_TERM_VARIABLE && term.normalized_var_id == var_id) {
                return true;
            }
        }
    }
    return false;
}

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

fn Value* deduce_explain_reverse_list(Value* list, Interp* interp) {
    Value* out = make_nil(interp);
    Value* cur = list;
    while (is_cons(cur)) {
        out = make_cons(interp, cur.cons_val.car, out);
        cur = cur.cons_val.cdr;
    }
    return out;
}

fn SymbolId deduce_explain_operator_symbol(Interp* interp, DeducePhysicalOpKind op_kind) {
    switch (op_kind) {
        case DEDUCE_PHYSICAL_FULL_SCAN:
            return interp.symbols.intern("full-scan");
        case DEDUCE_PHYSICAL_INDEX_SCAN:
            return interp.symbols.intern("index-scan");
        case DEDUCE_PHYSICAL_NEGATED_SCAN:
            return interp.symbols.intern("negated-scan");
        case DEDUCE_PHYSICAL_NEGATED_INDEX_PROBE:
            return interp.symbols.intern("negated-index-probe");
    }
}

fn long deduce_explain_rows_read_estimate(DeduceRelationSchema* schema) {
    if (schema == null) return 0;
    if (schema.cardinality_estimate > (ulong)long.max) return long.max;
    return (long)schema.cardinality_estimate;
}

fn long deduce_explain_rows_emitted_estimate(
    DeduceRelationSchema* schema,
    ulong bound_mask,
    int selected_index
) {
    if (schema == null) return 0;
    long rows_read = deduce_explain_rows_read_estimate(schema);
    if (rows_read <= 0) return 0;
    if (selected_index < 0) return rows_read;
    if (bound_mask == 0) return rows_read;
    if (schema.distinct_estimate == 0) return 1;

    ulong distinct = schema.distinct_estimate;
    if (distinct > schema.cardinality_estimate) distinct = schema.cardinality_estimate;
    if (distinct == 0) return 1;
    if (distinct >= schema.cardinality_estimate) return 1;

    long emitted = rows_read / (long)distinct;
    if (emitted <= 0) emitted = 1;
    if (emitted > rows_read) emitted = rows_read;
    return emitted;
}

fn Value* deduce_explain_build_step_counters(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    usz step_index,
    ulong bound_mask,
    int selected_index,
    Interp* interp
) {
    Value* counters = make_hashmap(interp, 8);
    if (counters == null || counters.tag != HASHMAP || counters.hashmap_val == null) {
        return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
    }

    DeduceRelationSchema* schema = null;
    if (db != null && sig != null && sig.body_predicates != null && step_index < sig.body_count) {
        schema = deduce_db_lookup_relation_schema(db, sig.body_predicates[step_index]);
    }

    long rows_read = deduce_explain_rows_read_estimate(schema);
    long rows_emitted = deduce_explain_rows_emitted_estimate(schema, bound_mask, selected_index);
    long index_hits = selected_index >= 0 ? rows_emitted : 0;
    long index_misses = 0;
    if (selected_index >= 0 && rows_read > index_hits) {
        index_misses = rows_read - index_hits;
    }

    explain_dict_set(counters, interp, "rows-read", make_int(interp, rows_read));
    explain_dict_set(counters, interp, "rows-emitted", make_int(interp, rows_emitted));
    explain_dict_set(counters, interp, "index-hits", make_int(interp, index_hits));
    explain_dict_set(counters, interp, "index-misses", make_int(interp, index_misses));
    explain_dict_set_symbol(counters, interp, "counter-kind", interp.symbols.intern("estimated"));
    return counters;
}

fn Value* deduce_explain_build_join_order(
    DeduceRuleSignature* sig,
    Interp* interp
) {
    if (sig == null || sig.body_count == 0 || sig.body_predicates == null) {
        return make_nil(interp);
    }
    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.body_count; i++) {
        rev = make_cons(interp, make_symbol(interp, sig.body_predicates[i]), rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* deduce_explain_build_plan_steps(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    Interp* interp
) {
    if (sig == null || sig.body_count == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.body_count; i++) {
        Value* step = make_hashmap(interp, 16);
        if (step == null || step.tag != HASHMAP || step.hashmap_val == null) {
            return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
        }
        DeduceRuleAtom op_probe = {};
        op_probe.negated = sig.body_negated != null ? sig.body_negated[i] : false;
        int selected_index = sig.body_selected_indexes != null ? sig.body_selected_indexes[i] : -1;
        ulong bound_mask = sig.body_bound_masks != null ? sig.body_bound_masks[i] : 0;
        DeducePhysicalOpKind op_kind = deduce_rule_choose_op_kind(&op_probe, selected_index);

        explain_dict_set(step, interp, "position", make_int(interp, (long)i));
        if (sig.body_predicates != null) {
            explain_dict_set(step, interp, "predicate", make_symbol(interp, sig.body_predicates[i]));
        } else {
            explain_dict_set(step, interp, "predicate", make_nil(interp));
        }
        explain_dict_set(step, interp, "negated",
            op_probe.negated ? make_symbol(interp, interp.sym_true) : make_nil(interp));
        explain_dict_set(step, interp, "bound-mask", make_int(interp, (long)bound_mask));
        if (selected_index >= 0) {
            explain_dict_set(step, interp, "selected-index", make_int(interp, (long)selected_index));
        } else {
            explain_dict_set(step, interp, "selected-index", make_nil(interp));
        }
        bool filter_pushdown = sig.body_filter_pushdown != null ? sig.body_filter_pushdown[i] : (bound_mask != 0);
        ulong projection_mask = sig.body_projection_masks != null ? sig.body_projection_masks[i] : 0;
        Value* counters = deduce_explain_build_step_counters(db, sig, i, bound_mask, selected_index, interp);
        if (counters != null && counters.tag == ERROR) return counters;
        explain_dict_set(step, interp, "filter-pushdown",
            filter_pushdown ? make_symbol(interp, interp.sym_true) : make_nil(interp));
        explain_dict_set(step, interp, "projection-mask", make_int(interp, (long)projection_mask));
        explain_dict_set(step, interp, "operator",
            make_symbol(interp, deduce_explain_operator_symbol(interp, op_kind)));
        explain_dict_set(step, interp, "counters", counters);
        rev = make_cons(interp, step, rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}

fn Value* prim_deduce_explain(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) {
        return deduce_raise(interp, "deduce/explain-expected-db-selector",
            "deduce/explain: expected (deduce/explain db [head-symbol|rule-index])");
    }

    Value* db_val = args[0];
    if (!deduce_ffi_handle_name_equals(db_val, "deduce-db")) {
        return deduce_raise(interp, "deduce/explain-db-not-handle",
            "deduce/explain: first argument must be a deduce database handle");
    }
    if (db_val.ffi_val == null || db_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/explain-db-handle-closed",
            "deduce/explain: database handle is closed");
    }

    DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
    if (!db.open || db.env == null) {
        return deduce_raise(interp, "deduce/explain-db-not-open", "deduce/explain: database is not open");
    }
    if (db.rule_signature_count == 0) {
        return deduce_raise(interp, "deduce/explain-no-rules", "deduce/explain: no rules are registered");
    }

    isz selected_rule_index = (isz)db.rule_signature_count - 1;
    if (args.len == 2) {
        Value* selector = args[1];
        if (selector != null && selector.tag == INT) {
            if (selector.int_val < 0 || (usz)selector.int_val >= db.rule_signature_count) {
                return deduce_raise(interp, "deduce/explain-rule-index-out-of-range",
                    "deduce/explain: rule index is out of range");
            }
            selected_rule_index = (isz)selector.int_val;
        } else if (selector != null && selector.tag == SYMBOL) {
            bool found = false;
            for (usz i = 0; i < db.rule_signature_count; i++) {
                if (db.rule_signatures[i].head_predicate == selector.sym_val) {
                    selected_rule_index = (isz)i;
                    found = true;
                    break;
                }
            }
            if (!found) {
                return deduce_raise(interp, "deduce/explain-rule-not-found",
                    "deduce/explain: no rule found for requested head predicate");
            }
        } else {
            return deduce_raise(interp, "deduce/explain-selector-not-symbol-or-int",
                "deduce/explain: selector must be a head symbol or rule index");
        }
    }

    DeduceRuleSignature* sig = &db.rule_signatures[(usz)selected_rule_index];
    Value* out = make_hashmap(interp, 16);
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) {
        return deduce_raise(interp, "deduce/explain-out-of-memory", "deduce/explain: out of memory");
    }

    Value* steps = deduce_explain_build_plan_steps(db, sig, interp);
    if (steps != null && steps.tag == ERROR) return steps;

    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-plan"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set(out, interp, "rule-index", make_int(interp, (long)selected_rule_index));
    explain_dict_set(out, interp, "rule-count", make_int(interp, (long)db.rule_signature_count));
    explain_dict_set(out, interp, "head-predicate", make_symbol(interp, sig.head_predicate));
    explain_dict_set(out, interp, "join-order", deduce_explain_build_join_order(sig, interp));
    explain_dict_set(out, interp, "steps", steps);
    return out;
}

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

    out.predicates = mem::new_array(SymbolId, predicate_list.len());
    if (out.predicates.ptr == null && predicate_list.len() > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    for (usz i = 0; i < predicate_list.len(); i++) out.predicates[i] = predicate_list[i];

    out.edges = mem::new_array(DeduceRuleGraphEdge, edge_list.len());
    if (out.edges.ptr == null && edge_list.len() > 0) {
        deduce_scc_plan_free(out);
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    for (usz i = 0; i < edge_list.len(); i++) out.edges[i] = edge_list[i];

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

struct DeduceNaiveRuleExec {
    DeduceDb*             db;
    DeduceRuleSignature*  sig;
    MdbTxn*               txn;
    MdbDbi                head_dbi;
    DeduceRelationSchema* head_schema;
    Value*[]              head_tuple;
    long*                 derived_out;
    bool*                 changed_out;
    Interp*               interp;
}

struct DeduceSemiNaiveRuleExec {
    DeduceDb*             db;
    DeduceRuleSignature*  sig;
    MdbTxn*               txn;
    MdbDbi                head_dbi;
    DeduceRelationSchema* head_schema;
    Value*[]              head_tuple;
    DeduceSccPlan*        plan;
    usz                   component_id;
    Value*[]              current_delta_sets;
    Value*[]              next_delta_sets;
    bool                  anchor_active;
    usz                   anchor_step;
    long*                 derived_out;
    bool*                 changed_out;
    Interp*               interp;
}

fn Value* deduce_delta_set_insert(Value** set_ref, void* key_data, usz key_len, Interp* interp) {
    if (set_ref == null || key_data == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid delta-set state");
    }

    Value* set = *set_ref;
    if (set == null) {
        set = make_hashmap(interp, 64);
        if (set == null || set.tag != HASHMAP || set.hashmap_val == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
        *set_ref = set;
    }

    if (set.tag != HASHMAP || set.hashmap_val == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid delta-set payload");
    }

    char[] key_slice = ((char*)key_data)[:key_len];
    Value* key_val = make_string(interp, key_slice);
    hashmap_set(set.hashmap_val, key_val, make_symbol(interp, interp.sym_true), interp);
    return null;
}

fn bool deduce_delta_set_contains(Value* set, void* key_data, usz key_len, Interp* interp) {
    if (set == null || key_data == null || interp == null) return false;
    if (set.tag != HASHMAP || set.hashmap_val == null) return false;
    char[] key_slice = ((char*)key_data)[:key_len];
    Value* key_val = make_string(interp, key_slice);
    return hashmap_get(set.hashmap_val, key_val) != null;
}

fn int deduce_scc_predicate_index(DeduceSccPlan* plan, SymbolId predicate) {
    if (plan == null || plan.predicates.len == 0) return -1;
    return deduce_scc_find_predicate_index(plan.predicates, predicate);
}

fn bool deduce_scc_predicate_in_component(DeduceSccPlan* plan, SymbolId predicate, usz component_id) {
    if (plan == null || component_id >= plan.component_count) return false;
    int pred_idx = deduce_scc_predicate_index(plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= plan.predicate_component.len) return false;
    return (usz)plan.predicate_component[(usz)pred_idx] == component_id;
}

fn bool deduce_seminaive_anchor_tuple_allowed(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len
) {
    if (exec == null || exec.plan == null) return false;
    int pred_idx = deduce_scc_predicate_index(exec.plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= exec.current_delta_sets.len) return false;
    if ((usz)pred_idx >= exec.plan.predicate_component.len) return false;
    if ((usz)exec.plan.predicate_component[(usz)pred_idx] != exec.component_id) return false;
    Value* delta_set = exec.current_delta_sets[(usz)pred_idx];
    return deduce_delta_set_contains(delta_set, key_data, key_len, exec.interp);
}

fn Value* deduce_seminaive_record_new_delta_tuple(
    DeduceSemiNaiveRuleExec* exec,
    SymbolId predicate,
    void* key_data,
    usz key_len
) {
    if (exec == null || exec.plan == null || exec.interp == null) return null;
    int pred_idx = deduce_scc_predicate_index(exec.plan, predicate);
    if (pred_idx < 0 || (usz)pred_idx >= exec.next_delta_sets.len) return null;
    if ((usz)pred_idx >= exec.plan.predicate_component.len) return null;
    if ((usz)exec.plan.predicate_component[(usz)pred_idx] != exec.component_id) return null;
    return deduce_delta_set_insert(&exec.next_delta_sets[(usz)pred_idx], key_data, key_len, exec.interp);
}

fn bool deduce_seminaive_component_has_delta(
    DeduceSccPlan* plan,
    usz component_id,
    Value*[] delta_sets
) {
    if (plan == null || component_id >= plan.component_count) return false;
    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len || pred_idx >= delta_sets.len) continue;
        int comp = plan.predicate_component[pred_idx];
        if (comp < 0 || (usz)comp != component_id) continue;
        Value* set = delta_sets[pred_idx];
        if (set != null && set.tag == HASHMAP && set.hashmap_val != null && set.hashmap_val.count > 0) {
            return true;
        }
    }
    return false;
}

fn Value* deduce_seminaive_seed_component_delta_sets(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    Value*[] delta_sets,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid delta seed state");
    }
    if (delta_sets.len < plan.predicates.len) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid delta seed table");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }
    defer if (txn != null) mdb_txn_abort(txn);

    for (usz pred_idx = 0; pred_idx < plan.predicates.len; pred_idx++) {
        if (pred_idx >= plan.predicate_component.len) continue;
        int comp = plan.predicate_component[pred_idx];
        if (comp < 0 || (usz)comp != component_id) continue;

        SymbolId predicate = plan.predicates[pred_idx];
        MdbDbi dbi = 0;
        DeduceRelationSchema* schema = null;
        if (!deduce_rule_open_existing_dbi(db, txn, predicate, &dbi, &schema, interp)) {
            return deduce_raise(interp, "deduce/relation-not-found", "deduce/analyze: predicate relation is not registered");
        }

        MdbCursor* cursor = null;
        rc = mdb_cursor_open(txn, dbi, &cursor);
        if (rc != MDB_SUCCESS || cursor == null) {
            return deduce_raise(interp, "deduce/analyze-scan-failed", "deduce/analyze: failed to open scan cursor");
        }

        MdbVal key = {};
        MdbVal data = {};
        rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);
        while (rc == MDB_SUCCESS) {
            Value* err = deduce_delta_set_insert(&delta_sets[pred_idx], key.mv_data, key.mv_size, interp);
            if (err != null) {
                mdb_cursor_close(cursor);
                return err;
            }
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
        }
        mdb_cursor_close(cursor);
    }
    return null;
}

fn usz deduce_seminaive_collect_anchor_steps(
    DeduceRuleSignature* sig,
    DeduceSccPlan* plan,
    usz component_id,
    usz[] anchor_steps
) {
    if (sig == null || plan == null) return 0;
    usz count = 0;
    for (usz i = 0; i < sig.body_count; i++) {
        if (sig.body_negated != null && sig.body_negated[i]) continue;
        if (sig.body_predicates == null) continue;
        SymbolId predicate = sig.body_predicates[i];
        if (!deduce_scc_predicate_in_component(plan, predicate, component_id)) continue;
        if (count < anchor_steps.len) anchor_steps[count] = i;
        count++;
    }
    return count;
}

fn Value* deduce_analyze_iteration_limit_payload(
    Interp* interp,
    usz component_id,
    usz iteration_limit
) {
    Value* payload = make_hashmap(interp, 8);
    if (payload == null || payload.tag != HASHMAP || payload.hashmap_val == null) return null;
    hashmap_set(payload.hashmap_val, make_symbol(interp, interp.symbols.intern("component-id")), make_int(interp, (long)component_id), interp);
    hashmap_set(payload.hashmap_val, make_symbol(interp, interp.symbols.intern("iteration-limit")), make_int(interp, (long)iteration_limit), interp);
    return payload;
}

fn bool deduce_rule_open_existing_dbi(
    DeduceDb* db,
    MdbTxn* txn,
    SymbolId predicate,
    MdbDbi* dbi_out,
    DeduceRelationSchema** schema_out,
    Interp* interp
) {
    if (db == null || txn == null || dbi_out == null || schema_out == null || interp == null) return false;
    DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, predicate);
    if (schema == null) return false;

    char[] name = interp.symbols.get_name(predicate);
    char* name_c = deduce_copy_c_string(name);
    if (name_c == null) return false;
    defer mem::free(name_c);

    MdbDbi dbi = 0;
    int rc = mdb_dbi_open(txn, name_c, 0, &dbi);
    if (rc != MDB_SUCCESS) return false;
    *dbi_out = dbi;
    *schema_out = schema;
    return true;
}

fn bool deduce_rule_match_positive_tuple(
    DeduceRuleSignature* sig,
    usz step_idx,
    Value*[] tuple_vals,
    bool[] var_bound,
    Value*[] var_values
) {
    if (sig == null || step_idx >= sig.body_count) return false;
    if (sig.body_term_offsets == null || sig.body_term_counts == null) return false;
    if (sig.body_term_is_variable == null || sig.body_term_var_ids == null || sig.body_term_literals == null) {
        return false;
    }
    usz offset = sig.body_term_offsets[step_idx];
    usz term_count = sig.body_term_counts[step_idx];
    if (offset + term_count > sig.body_term_total_count || term_count > tuple_vals.len) return false;

    for (usz t = 0; t < term_count; t++) {
        usz term_idx = offset + t;
        Value* tuple_v = tuple_vals[t];
        if (sig.body_term_is_variable[term_idx]) {
            uint var_id = sig.body_term_var_ids[term_idx];
            if (var_id >= var_bound.len || var_id >= var_values.len) return false;
            if (var_bound[var_id]) {
                if (!values_equal(var_values[var_id], tuple_v)) return false;
            } else {
                var_bound[var_id] = true;
                var_values[var_id] = tuple_v;
            }
        } else {
            Value* expected = sig.body_term_literals[term_idx];
            if (!values_equal(tuple_v, expected)) return false;
        }
    }
    return true;
}

fn bool deduce_rule_match_negated_tuple(
    DeduceRuleSignature* sig,
    usz step_idx,
    Value*[] tuple_vals,
    bool[] var_bound,
    Value*[] var_values
) {
    if (sig == null || step_idx >= sig.body_count) return false;
    if (sig.body_term_offsets == null || sig.body_term_counts == null) return false;
    if (sig.body_term_is_variable == null || sig.body_term_var_ids == null || sig.body_term_literals == null) {
        return false;
    }
    usz offset = sig.body_term_offsets[step_idx];
    usz term_count = sig.body_term_counts[step_idx];
    if (offset + term_count > sig.body_term_total_count || term_count > tuple_vals.len) return false;

    for (usz t = 0; t < term_count; t++) {
        usz term_idx = offset + t;
        Value* tuple_v = tuple_vals[t];
        if (sig.body_term_is_variable[term_idx]) {
            uint var_id = sig.body_term_var_ids[term_idx];
            if (var_id >= var_bound.len || var_id >= var_values.len) return false;
            if (!var_bound[var_id]) return false;
            if (!values_equal(var_values[var_id], tuple_v)) return false;
        } else {
            Value* expected = sig.body_term_literals[term_idx];
            if (!values_equal(tuple_v, expected)) return false;
        }
    }
    return true;
}

fn Value* deduce_naive_emit_head_fact(
    DeduceNaiveRuleExec* exec,
    bool[] var_bound,
    Value*[] var_values
) {
    if (exec == null || exec.sig == null || exec.txn == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
        }
        return null;
    }
    if (exec.sig.head_term_count != exec.head_tuple.len) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid head term metadata");
    }
    if (exec.sig.head_term_is_variable == null || exec.sig.head_term_var_ids == null || exec.sig.head_term_literals == null) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: missing head term metadata");
    }

    for (usz i = 0; i < exec.sig.head_term_count; i++) {
        if (exec.sig.head_term_is_variable[i]) {
            uint var_id = exec.sig.head_term_var_ids[i];
            if (var_id >= var_bound.len || var_id >= var_values.len || !var_bound[var_id]) {
                return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: unbound head variable at runtime");
            }
            exec.head_tuple[i] = var_values[var_id];
        } else {
            exec.head_tuple[i] = exec.sig.head_term_literals[i];
        }
    }

    char[4096] key_buf;
    usz? key_len = encode_tuple(exec.head_tuple, key_buf[..], key_buf.len);
    if (catch err = key_len) {
        return deduce_raise(exec.interp, "deduce/analyze-tuple-too-large", "deduce/analyze: derived tuple too large for buffer");
    }

    MdbVal key = { .mv_size = key_len, .mv_data = &key_buf[0] };
    MdbVal data = {};
    char empty = 0;
    data.mv_size = 0;
    data.mv_data = &empty;

    MdbVal existing = {};
    if (mdb_get(exec.txn, exec.head_dbi, &key, &existing) == MDB_SUCCESS) {
        return null;
    }

    int rc = mdb_put(exec.txn, exec.head_dbi, &key, &data, 0);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(exec.interp, "deduce/analyze-put-failed", "deduce/analyze: failed to insert derived tuple");
    }

    if (exec.changed_out != null) *exec.changed_out = true;
    if (exec.derived_out != null) *exec.derived_out = *exec.derived_out + 1;
    if (exec.head_schema != null && exec.head_schema.cardinality_estimate < ulong.max) {
        exec.head_schema.cardinality_estimate++;
        if (exec.head_schema.distinct_estimate < exec.head_schema.cardinality_estimate) {
            exec.head_schema.distinct_estimate = exec.head_schema.cardinality_estimate;
        }
    }
    return null;
}

fn Value* deduce_seminaive_emit_head_fact(
    DeduceSemiNaiveRuleExec* exec,
    bool[] var_bound,
    Value*[] var_values
) {
    if (exec == null || exec.sig == null || exec.txn == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
        }
        return null;
    }
    if (exec.sig.head_term_count != exec.head_tuple.len) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid head term metadata");
    }
    if (exec.sig.head_term_is_variable == null || exec.sig.head_term_var_ids == null || exec.sig.head_term_literals == null) {
        return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: missing head term metadata");
    }

    for (usz i = 0; i < exec.sig.head_term_count; i++) {
        if (exec.sig.head_term_is_variable[i]) {
            uint var_id = exec.sig.head_term_var_ids[i];
            if (var_id >= var_bound.len || var_id >= var_values.len || !var_bound[var_id]) {
                return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: unbound head variable at runtime");
            }
            exec.head_tuple[i] = var_values[var_id];
        } else {
            exec.head_tuple[i] = exec.sig.head_term_literals[i];
        }
    }

    char[4096] key_buf;
    usz? key_len = encode_tuple(exec.head_tuple, key_buf[..], key_buf.len);
    if (catch err = key_len) {
        return deduce_raise(exec.interp, "deduce/analyze-tuple-too-large", "deduce/analyze: derived tuple too large for buffer");
    }

    MdbVal key = { .mv_size = key_len, .mv_data = &key_buf[0] };
    MdbVal data = {};
    char empty = 0;
    data.mv_size = 0;
    data.mv_data = &empty;

    MdbVal existing = {};
    if (mdb_get(exec.txn, exec.head_dbi, &key, &existing) == MDB_SUCCESS) {
        return null;
    }

    int rc = mdb_put(exec.txn, exec.head_dbi, &key, &data, 0);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(exec.interp, "deduce/analyze-put-failed", "deduce/analyze: failed to insert derived tuple");
    }

    if (exec.changed_out != null) *exec.changed_out = true;
    if (exec.derived_out != null) *exec.derived_out = *exec.derived_out + 1;
    if (exec.head_schema != null && exec.head_schema.cardinality_estimate < ulong.max) {
        exec.head_schema.cardinality_estimate++;
        if (exec.head_schema.distinct_estimate < exec.head_schema.cardinality_estimate) {
            exec.head_schema.distinct_estimate = exec.head_schema.cardinality_estimate;
        }
    }

    return deduce_seminaive_record_new_delta_tuple(exec, exec.sig.head_predicate, &key_buf[0], key_len);
}

fn Value* deduce_seminaive_eval_rule_step(
    DeduceSemiNaiveRuleExec* exec,
    usz step_idx,
    bool[] var_bound,
    Value*[] var_values
) {
    if (exec == null || exec.sig == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
        }
        return null;
    }
    if (step_idx >= exec.sig.body_count) {
        return deduce_seminaive_emit_head_fact(exec, var_bound, var_values);
    }

    SymbolId predicate = exec.sig.body_predicates != null ? exec.sig.body_predicates[step_idx] : (SymbolId)0;
    bool negated = exec.sig.body_negated != null ? exec.sig.body_negated[step_idx] : false;

    MdbDbi body_dbi = 0;
    DeduceRelationSchema* body_schema = null;
    if (!deduce_rule_open_existing_dbi(exec.db, exec.txn, predicate, &body_dbi, &body_schema, exec.interp)) {
        return deduce_raise(exec.interp, "deduce/relation-not-found", "deduce/analyze: predicate relation is not registered");
    }
    if (body_schema == null) {
        return deduce_raise(exec.interp, "deduce/relation-not-found", "deduce/analyze: predicate relation is not registered");
    }

    Value*[] tuple_vals = {};
    if (body_schema.arity > 0) {
        tuple_vals = mem::new_array(Value*, body_schema.arity);
        if (tuple_vals.ptr == null) {
            return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    defer if (tuple_vals.ptr != null) mem::free(tuple_vals.ptr);

    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(exec.txn, body_dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(exec.interp, "deduce/analyze-scan-failed", "deduce/analyze: failed to open scan cursor");
    }
    defer mdb_cursor_close(cursor);

    MdbVal key = {};
    MdbVal data = {};
    rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);

    if (negated) {
        bool found_match = false;
        while (rc == MDB_SUCCESS) {
            usz? decoded_count = decode_tuple((char*)key.mv_data, key.mv_size, tuple_vals, exec.interp);
            if (catch err = decoded_count) {
                rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                continue;
            }
            if (decoded_count != body_schema.arity) {
                rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                continue;
            }
            if (deduce_rule_match_negated_tuple(exec.sig, step_idx, tuple_vals, var_bound, var_values)) {
                found_match = true;
                break;
            }
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
        }
        if (found_match) return null;
        return deduce_seminaive_eval_rule_step(exec, step_idx + 1, var_bound, var_values);
    }

    bool[] candidate_bound = mem::new_array(bool, var_bound.len);
    if (candidate_bound.ptr == null && var_bound.len > 0) {
        return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (candidate_bound.ptr != null) mem::free(candidate_bound.ptr);

    Value*[] candidate_values = mem::new_array(Value*, var_values.len);
    if (candidate_values.ptr == null && var_values.len > 0) {
        return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (candidate_values.ptr != null) mem::free(candidate_values.ptr);

    while (rc == MDB_SUCCESS) {
        usz? decoded_count = decode_tuple((char*)key.mv_data, key.mv_size, tuple_vals, exec.interp);
        if (catch err = decoded_count) {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
            continue;
        }
        if (decoded_count != body_schema.arity) {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
            continue;
        }

        if (exec.anchor_active && step_idx == exec.anchor_step &&
            !deduce_seminaive_anchor_tuple_allowed(exec, predicate, key.mv_data, key.mv_size)) {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
            continue;
        }

        for (usz i = 0; i < var_bound.len; i++) candidate_bound[i] = var_bound[i];
        for (usz i = 0; i < var_values.len; i++) candidate_values[i] = var_values[i];

        if (deduce_rule_match_positive_tuple(exec.sig, step_idx, tuple_vals, candidate_bound, candidate_values)) {
            Value* err = deduce_seminaive_eval_rule_step(exec, step_idx + 1, candidate_bound, candidate_values);
            if (err != null) return err;
        }
        rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
    }
    return null;
}

fn Value* deduce_seminaive_evaluate_rule(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    DeduceRuleSignature* sig,
    MdbTxn* txn,
    Value*[] current_delta_sets,
    Value*[] next_delta_sets,
    long* derived_out,
    bool* changed_out,
    Interp* interp
) {
    if (db == null || plan == null || sig == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
    }
    if (sig.body_count == 0 || sig.body_predicates == null) {
        return null;
    }

    MdbDbi head_dbi = 0;
    DeduceRelationSchema* head_schema = null;
    if (!deduce_rule_open_existing_dbi(db, txn, sig.head_predicate, &head_dbi, &head_schema, interp)) {
        return deduce_raise(interp, "deduce/relation-not-found", "deduce/analyze: head predicate relation is not registered");
    }

    Value*[] var_values = {};
    bool[] var_bound = {};
    usz var_capacity = (usz)sig.max_var_id + 1;
    if (var_capacity == 0) var_capacity = 1;

    var_values = mem::new_array(Value*, var_capacity);
    if (var_values.ptr == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer mem::free(var_values.ptr);
    for (usz i = 0; i < var_values.len; i++) var_values[i] = null;

    var_bound = mem::new_array(bool, var_capacity);
    if (var_bound.ptr == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer mem::free(var_bound.ptr);
    for (usz i = 0; i < var_bound.len; i++) var_bound[i] = false;

    Value*[] head_tuple = {};
    if (sig.head_term_count > 0) {
        head_tuple = mem::new_array(Value*, sig.head_term_count);
        if (head_tuple.ptr == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    defer if (head_tuple.ptr != null) mem::free(head_tuple.ptr);

    usz[] anchor_steps = {};
    if (sig.body_count > 0) {
        anchor_steps = mem::new_array(usz, sig.body_count);
        if (anchor_steps.ptr == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    defer if (anchor_steps.ptr != null) mem::free(anchor_steps.ptr);

    DeduceSemiNaiveRuleExec exec = {
        .db = db,
        .sig = sig,
        .txn = txn,
        .head_dbi = head_dbi,
        .head_schema = head_schema,
        .head_tuple = head_tuple,
        .plan = plan,
        .component_id = component_id,
        .current_delta_sets = current_delta_sets,
        .next_delta_sets = next_delta_sets,
        .anchor_active = false,
        .anchor_step = 0,
        .derived_out = derived_out,
        .changed_out = changed_out,
        .interp = interp,
    };

    usz anchor_count = deduce_seminaive_collect_anchor_steps(sig, plan, component_id, anchor_steps);
    if (anchor_count == 0) {
        return deduce_seminaive_eval_rule_step(&exec, 0, var_bound, var_values);
    }

    for (usz i = 0; i < anchor_count; i++) {
        if (i >= anchor_steps.len) break;
        exec.anchor_active = true;
        exec.anchor_step = anchor_steps[i];
        Value* err = deduce_seminaive_eval_rule_step(&exec, 0, var_bound, var_values);
        if (err != null) return err;
    }
    return null;
}

fn Value* deduce_naive_eval_rule_step(
    DeduceNaiveRuleExec* exec,
    usz step_idx,
    bool[] var_bound,
    Value*[] var_values
) {
    if (exec == null || exec.sig == null || exec.interp == null) {
        if (exec != null && exec.interp != null) {
            return deduce_raise(exec.interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
        }
        return null;
    }
    if (step_idx >= exec.sig.body_count) {
        return deduce_naive_emit_head_fact(exec, var_bound, var_values);
    }

    SymbolId predicate = exec.sig.body_predicates != null ? exec.sig.body_predicates[step_idx] : (SymbolId)0;
    bool negated = exec.sig.body_negated != null ? exec.sig.body_negated[step_idx] : false;

    MdbDbi body_dbi = 0;
    DeduceRelationSchema* body_schema = null;
    if (!deduce_rule_open_existing_dbi(exec.db, exec.txn, predicate, &body_dbi, &body_schema, exec.interp)) {
        return deduce_raise(exec.interp, "deduce/relation-not-found", "deduce/analyze: predicate relation is not registered");
    }
    if (body_schema == null) {
        return deduce_raise(exec.interp, "deduce/relation-not-found", "deduce/analyze: predicate relation is not registered");
    }

    Value*[] tuple_vals = {};
    if (body_schema.arity > 0) {
        tuple_vals = mem::new_array(Value*, body_schema.arity);
        if (tuple_vals.ptr == null) {
            return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    defer if (tuple_vals.ptr != null) mem::free(tuple_vals.ptr);

    MdbCursor* cursor = null;
    int rc = mdb_cursor_open(exec.txn, body_dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(exec.interp, "deduce/analyze-scan-failed", "deduce/analyze: failed to open scan cursor");
    }
    defer mdb_cursor_close(cursor);

    MdbVal key = {};
    MdbVal data = {};
    rc = mdb_cursor_get(cursor, &key, &data, MDB_FIRST);

    if (negated) {
        bool found_match = false;
        while (rc == MDB_SUCCESS) {
            usz? decoded_count = decode_tuple((char*)key.mv_data, key.mv_size, tuple_vals, exec.interp);
            if (catch err = decoded_count) {
                rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                continue;
            }
            if (decoded_count != body_schema.arity) {
                rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
                continue;
            }
            if (deduce_rule_match_negated_tuple(exec.sig, step_idx, tuple_vals, var_bound, var_values)) {
                found_match = true;
                break;
            }
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
        }
        if (found_match) return null;
        return deduce_naive_eval_rule_step(exec, step_idx + 1, var_bound, var_values);
    }

    bool[] candidate_bound = mem::new_array(bool, var_bound.len);
    if (candidate_bound.ptr == null && var_bound.len > 0) {
        return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (candidate_bound.ptr != null) mem::free(candidate_bound.ptr);

    Value*[] candidate_values = mem::new_array(Value*, var_values.len);
    if (candidate_values.ptr == null && var_values.len > 0) {
        return deduce_raise(exec.interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (candidate_values.ptr != null) mem::free(candidate_values.ptr);

    while (rc == MDB_SUCCESS) {
        usz? decoded_count = decode_tuple((char*)key.mv_data, key.mv_size, tuple_vals, exec.interp);
        if (catch err = decoded_count) {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
            continue;
        }
        if (decoded_count != body_schema.arity) {
            rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
            continue;
        }

        for (usz i = 0; i < var_bound.len; i++) candidate_bound[i] = var_bound[i];
        for (usz i = 0; i < var_values.len; i++) candidate_values[i] = var_values[i];

        if (deduce_rule_match_positive_tuple(exec.sig, step_idx, tuple_vals, candidate_bound, candidate_values)) {
            Value* err = deduce_naive_eval_rule_step(exec, step_idx + 1, candidate_bound, candidate_values);
            if (err != null) return err;
        }
        rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
    }
    return null;
}

fn Value* deduce_naive_evaluate_rule(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    MdbTxn* txn,
    long* derived_out,
    bool* changed_out,
    Interp* interp
) {
    if (db == null || sig == null || txn == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid evaluator state");
    }
    if (sig.body_count == 0 || sig.body_predicates == null) {
        return null;
    }

    MdbDbi head_dbi = 0;
    DeduceRelationSchema* head_schema = null;
    if (!deduce_rule_open_existing_dbi(db, txn, sig.head_predicate, &head_dbi, &head_schema, interp)) {
        return deduce_raise(interp, "deduce/relation-not-found", "deduce/analyze: head predicate relation is not registered");
    }

    Value*[] var_values = {};
    bool[] var_bound = {};
    usz var_capacity = (usz)sig.max_var_id + 1;
    if (var_capacity == 0) var_capacity = 1;

    var_values = mem::new_array(Value*, var_capacity);
    if (var_values.ptr == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer mem::free(var_values.ptr);
    for (usz i = 0; i < var_values.len; i++) var_values[i] = null;

    var_bound = mem::new_array(bool, var_capacity);
    if (var_bound.ptr == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer mem::free(var_bound.ptr);
    for (usz i = 0; i < var_bound.len; i++) var_bound[i] = false;

    Value*[] head_tuple = {};
    if (sig.head_term_count > 0) {
        head_tuple = mem::new_array(Value*, sig.head_term_count);
        if (head_tuple.ptr == null) {
            return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
        }
    }
    defer if (head_tuple.ptr != null) mem::free(head_tuple.ptr);

    DeduceNaiveRuleExec exec = {
        .db = db,
        .sig = sig,
        .txn = txn,
        .head_dbi = head_dbi,
        .head_schema = head_schema,
        .head_tuple = head_tuple,
        .derived_out = derived_out,
        .changed_out = changed_out,
        .interp = interp,
    };
    return deduce_naive_eval_rule_step(&exec, 0, var_bound, var_values);
}

fn Value* deduce_naive_evaluate_component_pass(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    bool* changed_out,
    long* derived_out,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }

    bool changed = false;
    long derived = 0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        Value* err = deduce_naive_evaluate_rule(db, sig, txn, &derived, &changed, interp);
        if (err != null) {
            mdb_txn_abort(txn);
            return err;
        }
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn commit failed");
    }

    if (changed_out != null) *changed_out = changed;
    if (derived_out != null) *derived_out = derived;
    return null;
}

fn Value* deduce_seminaive_evaluate_recursive_component(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    long* iterations_out,
    long* derived_out,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    if (plan.predicates.len == 0) return null;

    Value*[] current_delta_sets = mem::new_array(Value*, plan.predicates.len);
    if (current_delta_sets.ptr == null && plan.predicates.len > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (current_delta_sets.ptr != null) mem::free(current_delta_sets.ptr);

    Value*[] next_delta_sets = mem::new_array(Value*, plan.predicates.len);
    if (next_delta_sets.ptr == null && plan.predicates.len > 0) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    defer if (next_delta_sets.ptr != null) mem::free(next_delta_sets.ptr);

    for (usz i = 0; i < plan.predicates.len; i++) {
        current_delta_sets[i] = null;
        next_delta_sets[i] = null;
    }

    Value* seed_err = deduce_seminaive_seed_component_delta_sets(db, plan, component_id, current_delta_sets, interp);
    if (seed_err != null) return seed_err;

    usz limit = iteration_limit > 0 ? iteration_limit : 256;
    for (usz iter = 0; iter < limit; iter++) {
        MdbTxn* txn = null;
        int rc = mdb_txn_begin(db.env, null, 0, &txn);
        if (rc != MDB_SUCCESS || txn == null) {
            return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
        }

        bool changed = false;
        long derived = 0;
        for (usz i = 0; i < db.rule_signature_count; i++) {
            if (i >= plan.rule_component_ids.len || plan.rule_component_ids[i] != component_id) continue;
            DeduceRuleSignature* sig = &db.rule_signatures[i];
            Value* err = deduce_seminaive_evaluate_rule(
                db,
                plan,
                component_id,
                sig,
                txn,
                current_delta_sets,
                next_delta_sets,
                &derived,
                &changed,
                interp
            );
            if (err != null) {
                mdb_txn_abort(txn);
                return err;
            }
        }

        rc = mdb_txn_commit(txn);
        if (rc != MDB_SUCCESS) {
            return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn commit failed");
        }

        if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
        if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;

        bool has_delta = deduce_seminaive_component_has_delta(plan, component_id, next_delta_sets);
        for (usz i = 0; i < plan.predicates.len; i++) {
            current_delta_sets[i] = next_delta_sets[i];
            next_delta_sets[i] = null;
        }
        if (!has_delta && !changed) return null;
        if (!has_delta) return null;
    }

    Value* data = deduce_analyze_iteration_limit_payload(interp, component_id, limit);
    return deduce_raise(interp, "deduce/analyze-iteration-limit", "deduce/analyze: recursive stratum iteration limit reached", data);
}

fn Value* deduce_naive_evaluate_recursive_component(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    long* iterations_out,
    long* derived_out,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    usz limit = iteration_limit > 0 ? iteration_limit : 256;
    for (usz iter = 0; iter < limit; iter++) {
        bool changed = false;
        long derived = 0;
        Value* err = deduce_naive_evaluate_component_pass(db, plan, component_id, &changed, &derived, interp);
        if (err != null) return err;

        if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
        if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;
        if (!changed) return null;
    }

    Value* data = deduce_analyze_iteration_limit_payload(interp, component_id, limit);
    return deduce_raise(interp, "deduce/analyze-iteration-limit", "deduce/analyze: recursive stratum iteration limit reached", data);
}

fn Value* deduce_naive_evaluate_component(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    bool use_seminaive_recursive,
    long* iterations_out,
    long* derived_out,
    Interp* interp
) {
    if (db == null || plan == null || component_id >= plan.component_count || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: invalid component evaluation state");
    }

    bool has_rules = false;
    for (usz i = 0; i < plan.rule_component_ids.len; i++) {
        if (plan.rule_component_ids[i] == component_id) {
            has_rules = true;
            break;
        }
    }
    if (!has_rules) return null;

    bool recursive = plan.component_recursive.ptr != null && plan.component_recursive[component_id];
    if (!recursive) {
        bool changed = false;
        long derived = 0;
        Value* err = deduce_naive_evaluate_component_pass(db, plan, component_id, &changed, &derived, interp);
        if (err != null) return err;
        if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
        if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;
        return null;
    }

    if (use_seminaive_recursive) {
        return deduce_seminaive_evaluate_recursive_component(
            db,
            plan,
            component_id,
            iteration_limit,
            iterations_out,
            derived_out,
            interp
        );
    }

    return deduce_naive_evaluate_recursive_component(
        db,
        plan,
        component_id,
        iteration_limit,
        iterations_out,
        derived_out,
        interp
    );
}

fn Value* deduce_naive_fixpoint_evaluate_with_plan(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz max_iterations,
    bool use_seminaive_recursive,
    long* iterations_out,
    long* derived_out,
    long* max_component_iterations_out,
    Interp* interp
) {
    if (iterations_out != null) *iterations_out = 0;
    if (derived_out != null) *derived_out = 0;
    if (max_component_iterations_out != null) *max_component_iterations_out = 0;
    if (db == null || !db.open || db.env == null || plan == null || interp == null) {
        return deduce_raise(interp, "deduce/analyze-invalid-state", "deduce/analyze: database is not open");
    }
    if (db.rule_signature_count == 0 || plan.component_count == 0) return null;

    usz hard_limit = max_iterations > 0 ? max_iterations : 256;
    for (usz i = 0; i < plan.component_order.len; i++) {
        usz component_id = plan.component_order[i];
        long before_iterations = iterations_out != null ? *iterations_out : 0;
        Value* err = deduce_naive_evaluate_component(
            db,
            plan,
            component_id,
            hard_limit,
            use_seminaive_recursive,
            iterations_out,
            derived_out,
            interp
        );
        if (err != null) return err;

        if (max_component_iterations_out != null && iterations_out != null) {
            long component_iterations = *iterations_out - before_iterations;
            if (component_iterations > *max_component_iterations_out) {
                *max_component_iterations_out = component_iterations;
            }
        }
    }
    return null;
}

fn Value* prim_deduce_analyze(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 2) {
        return deduce_raise(interp, "deduce/analyze-expected-db", "deduce/analyze: expected (deduce/analyze db [engine])");
    }

    Value* db_val = args[0];
    if (!deduce_ffi_handle_name_equals(db_val, "deduce-db")) {
        return deduce_raise(interp, "deduce/analyze-db-not-handle",
            "deduce/analyze: first argument must be a deduce database handle");
    }
    if (db_val.ffi_val == null || db_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/analyze-db-handle-closed",
            "deduce/analyze: database handle is closed");
    }

    DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
    if (!db.open || db.env == null) {
        return deduce_raise(interp, "deduce/analyze-db-not-open", "deduce/analyze: database is not open");
    }

    long dependency_edges = 0;
    deduce_db_dependency_edge_counts(db, &dependency_edges, null);
    long incremental_dirty_predicates = (long)db.incremental_dirty_count;
    SymbolId incremental_invalidation_mode =
        interp.symbols.intern(db.incremental_full_recompute_required ? "full-recompute" : "tracked");

    bool use_seminaive_recursive = true;
    SymbolId execution_engine_sym = interp.symbols.intern("semi-naive-scc");
    if (args.len == 2) {
        Value* engine_arg = args[1];
        if (engine_arg == null || engine_arg.tag != SYMBOL) {
            return deduce_raise(interp, "deduce/analyze-invalid-engine", "deduce/analyze: engine must be symbol 'naive or 'semi-naive");
        }
        char[] engine_name = interp.symbols.get_name(engine_arg.sym_val);
        if (deduce_name_equals(engine_name, "naive") || deduce_name_equals(engine_name, "naive-scc")) {
            use_seminaive_recursive = false;
            execution_engine_sym = interp.symbols.intern("naive-scc");
        } else if (deduce_name_equals(engine_name, "semi-naive") || deduce_name_equals(engine_name, "semi-naive-scc")) {
            use_seminaive_recursive = true;
            execution_engine_sym = interp.symbols.intern("semi-naive-scc");
        } else {
            return deduce_raise(interp, "deduce/analyze-invalid-engine", "deduce/analyze: engine must be 'naive or 'semi-naive");
        }
    }

    DeduceSccPlan scc_plan = {};
    Value* plan_err = deduce_build_scc_plan(db, &scc_plan, interp);
    if (plan_err != null) return plan_err;
    defer deduce_scc_plan_free(&scc_plan);

    const usz ANALYZE_ITERATION_LIMIT = 256;
    long iterations = 0;
    long derived = 0;
    long max_component_iterations = 0;
    Value* err = deduce_naive_fixpoint_evaluate_with_plan(
        db,
        &scc_plan,
        ANALYZE_ITERATION_LIMIT,
        use_seminaive_recursive,
        &iterations,
        &derived,
        &max_component_iterations,
        interp
    );
    if (err != null) return err;

    long recursive_strata = 0;
    for (usz i = 0; i < scc_plan.component_recursive.len; i++) {
        if (scc_plan.component_recursive[i]) recursive_strata++;
    }

    Value* out = make_hashmap(interp, 64);
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) {
        return deduce_raise(interp, "deduce/analyze-out-of-memory", "deduce/analyze: out of memory");
    }
    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-analyze"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set_symbol(out, interp, "mode", interp.symbols.intern("naive-bottom-up"));
    explain_dict_set_symbol(out, interp, "execution-engine", execution_engine_sym);
    explain_dict_set(out, interp, "version", make_int(interp, 1));
    explain_dict_set(out, interp, "rule-count", make_int(interp, (long)db.rule_signature_count));
    explain_dict_set(out, interp, "strata-count", make_int(interp, (long)scc_plan.component_count));
    explain_dict_set(out, interp, "stratum-count", make_int(interp, (long)scc_plan.stratum_count));
    explain_dict_set(out, interp, "recursive-strata", make_int(interp, recursive_strata));
    explain_dict_set(out, interp, "iteration-limit", make_int(interp, (long)ANALYZE_ITERATION_LIMIT));
    explain_dict_set(out, interp, "max-component-iterations", make_int(interp, max_component_iterations));
    explain_dict_set(out, interp, "iterations", make_int(interp, iterations));
    explain_dict_set(out, interp, "derived-facts", make_int(interp, derived));
    explain_dict_set(out, interp, "incremental-dependency-edges", make_int(interp, dependency_edges));
    explain_dict_set(out, interp, "incremental-dirty-predicate-count", make_int(interp, incremental_dirty_predicates));
    explain_dict_set_symbol(out, interp, "incremental-invalidation-mode", incremental_invalidation_mode);
    deduce_db_reset_incremental_tracking(db);
    return out;
}

fn Value* deduce_rule_validate_stratification(DeduceDb* db, DeduceRuleIr* candidate, Interp* interp) {
    List{SymbolId} predicates = {};
    defer predicates.free();

    List{DeduceRuleGraphEdge} edges = {};
    defer edges.free();

    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature sig = db.rule_signatures[i];
        int from = deduce_rule_get_or_add_predicate_index(&predicates, sig.head_predicate);
        for (usz b = 0; b < sig.body_count; b++) {
            int to = deduce_rule_get_or_add_predicate_index(&predicates, sig.body_predicates[b]);
            edges.push({ .from = from, .to = to, .negated = sig.body_negated[b] });
        }
    }

    int cand_from = deduce_rule_get_or_add_predicate_index(&predicates, candidate.head.predicate);
    for (usz i = 0; i < candidate.body_count; i++) {
        DeduceRuleAtom atom = candidate.body[i];
        int to = deduce_rule_get_or_add_predicate_index(&predicates, atom.predicate);
        edges.push({ .from = cand_from, .to = to, .negated = atom.negated });
    }

    usz n = predicates.len();
    if (n == 0) return null;

    bool[] reach = mem::new_array(bool, n * n);
    if (reach.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(reach.ptr);
    for (usz i = 0; i < reach.len; i++) reach[i] = false;

    for (usz i = 0; i < edges.len(); i++) {
        DeduceRuleGraphEdge e = edges[i];
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

    for (usz i = 0; i < edges.len(); i++) {
        DeduceRuleGraphEdge e = edges[i];
        if (!e.negated) continue;
        if (e.from < 0 || e.to < 0) continue;
        if (reach[(usz)e.to * n + (usz)e.from]) {
            return deduce_raise(interp, "deduce/non-stratifiable-program", "deduce-rule!: rule introduces a negative recursion cycle");
        }
    }
    return null;
}

fn Value* prim_deduce_rule(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 3) {
        return deduce_raise(interp, "deduce/rule-expected-db-head-body", "deduce-rule!: expected (deduce/rule! db head-atom body-atom...)");
    }

    Value* db_val = args[0];
    if (!deduce_ffi_handle_name_equals(db_val, "deduce-db")) {
        return deduce_raise(interp, "deduce/rule-db-not-handle", "deduce-rule!: first argument must be a deduce database handle");
    }
    if (db_val.ffi_val == null || db_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/rule-db-handle-closed", "deduce-rule!: database handle is closed");
    }

    DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
    if (!db.open || db.env == null) {
        return deduce_raise(interp, "deduce/rule-db-not-open", "deduce-rule!: database is not open");
    }

    DeduceRuleIr rule = {};
    Value* parse_err = deduce_rule_parse_ir(args[1..], &rule, interp);
    if (parse_err != null) {
        deduce_rule_ir_free(&rule);
        return parse_err;
    }
    defer deduce_rule_ir_free(&rule);

    Value* err = deduce_rule_validate_arity(db, &rule, interp);
    if (err != null) return err;
    err = deduce_rule_validate_head_variable_safety(&rule, interp);
    if (err != null) return err;
    err = deduce_rule_validate_negation_safety(&rule, interp);
    if (err != null) return err;
    err = deduce_rule_validate_stratification(db, &rule, interp);
    if (err != null) return err;

    SymbolId[] body_preds = mem::new_array(SymbolId, rule.body_count);
    if (body_preds.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_preds.ptr);

    bool[] body_negated = mem::new_array(bool, rule.body_count);
    if (body_negated.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_negated.ptr);

    ulong[] body_bound_masks = mem::new_array(ulong, rule.body_count);
    if (body_bound_masks.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_bound_masks.ptr);

    int[] body_selected_indexes = mem::new_array(int, rule.body_count);
    if (body_selected_indexes.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_selected_indexes.ptr);

    bool[] body_filter_pushdown = mem::new_array(bool, rule.body_count);
    if (body_filter_pushdown.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_filter_pushdown.ptr);

    ulong[] body_projection_masks = mem::new_array(ulong, rule.body_count);
    if (body_projection_masks.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_projection_masks.ptr);

    bool[] head_term_is_variable = mem::new_array(bool, rule.head.term_count);
    if (head_term_is_variable.ptr == null && rule.head.term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (head_term_is_variable.ptr != null) mem::free(head_term_is_variable.ptr);

    uint[] head_term_var_ids = mem::new_array(uint, rule.head.term_count);
    if (head_term_var_ids.ptr == null && rule.head.term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (head_term_var_ids.ptr != null) mem::free(head_term_var_ids.ptr);

    Value*[] head_term_literals = mem::new_array(Value*, rule.head.term_count);
    if (head_term_literals.ptr == null && rule.head.term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (head_term_literals.ptr != null) mem::free(head_term_literals.ptr);

    usz[] body_term_offsets = mem::new_array(usz, rule.body_count);
    if (body_term_offsets.ptr == null && rule.body_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (body_term_offsets.ptr != null) mem::free(body_term_offsets.ptr);

    usz[] body_term_counts = mem::new_array(usz, rule.body_count);
    if (body_term_counts.ptr == null && rule.body_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (body_term_counts.ptr != null) mem::free(body_term_counts.ptr);

    usz total_body_term_count = 0;
    for (usz i = 0; i < rule.body_count; i++) {
        if (total_body_term_count > usz.max - rule.body[i].term_count) {
            return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
        }
        total_body_term_count += rule.body[i].term_count;
    }

    bool[] body_term_is_variable = mem::new_array(bool, total_body_term_count);
    if (body_term_is_variable.ptr == null && total_body_term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (body_term_is_variable.ptr != null) mem::free(body_term_is_variable.ptr);

    uint[] body_term_var_ids = mem::new_array(uint, total_body_term_count);
    if (body_term_var_ids.ptr == null && total_body_term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (body_term_var_ids.ptr != null) mem::free(body_term_var_ids.ptr);

    Value*[] body_term_literals = mem::new_array(Value*, total_body_term_count);
    if (body_term_literals.ptr == null && total_body_term_count > 0) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer if (body_term_literals.ptr != null) mem::free(body_term_literals.ptr);

    DeduceBodyPlanStep[] body_plan = mem::new_array(DeduceBodyPlanStep, rule.body_count);
    if (body_plan.ptr == null) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    defer mem::free(body_plan.ptr);

    for (usz i = 0; i < rule.body_count; i++) {
        body_plan[i].logical_body_index = 0;
        body_plan[i].bound_mask = 0;
        body_plan[i].selected_index = -1;
        body_plan[i].op_kind = DEDUCE_PHYSICAL_FULL_SCAN;
    }

    err = deduce_plan_conjunctive_body(
        db,
        rule.body,
        rule.body_count,
        rule.max_var_id,
        body_plan.ptr,
        body_plan.len,
        interp
    );
    if (err != null) return err;

    for (usz i = 0; i < rule.body_count; i++) {
        usz logical_idx = body_plan[i].logical_body_index;
        DeduceRuleAtom atom = rule.body[logical_idx];
        body_preds[i] = atom.predicate;
        body_negated[i] = atom.negated;
        body_bound_masks[i] = body_plan[i].bound_mask;
        body_selected_indexes[i] = body_plan[i].selected_index;
        body_filter_pushdown[i] = body_plan[i].bound_mask != 0 ||
            deduce_rule_atom_has_local_equality_constraint(&atom);
        body_projection_masks[i] = deduce_rule_projection_mask_for_step(&rule, body_plan, i);
    }

    for (usz i = 0; i < rule.head.term_count; i++) {
        DeduceRuleTerm term = rule.head.terms[i];
        bool is_var = term.kind == DEDUCE_RULE_TERM_VARIABLE;
        head_term_is_variable[i] = is_var;
        head_term_var_ids[i] = is_var ? term.normalized_var_id : 0;
        if (is_var) {
            head_term_literals[i] = null;
        } else {
            head_term_literals[i] = boundary_promote_to_escape(term.literal_value, interp);
        }
    }

    usz body_term_cursor = 0;
    for (usz i = 0; i < rule.body_count; i++) {
        usz logical_idx = body_plan[i].logical_body_index;
        DeduceRuleAtom atom = rule.body[logical_idx];
        body_term_offsets[i] = body_term_cursor;
        body_term_counts[i] = atom.term_count;
        for (usz t = 0; t < atom.term_count; t++) {
            DeduceRuleTerm term = atom.terms[t];
            bool is_var = term.kind == DEDUCE_RULE_TERM_VARIABLE;
            body_term_is_variable[body_term_cursor] = is_var;
            body_term_var_ids[body_term_cursor] = is_var ? term.normalized_var_id : 0;
            if (is_var) {
                body_term_literals[body_term_cursor] = null;
            } else {
                body_term_literals[body_term_cursor] = boundary_promote_to_escape(term.literal_value, interp);
            }
            body_term_cursor++;
        }
    }

    DeduceRuleRegisterStatus register_status =
        deduce_db_add_rule_signature(
            db,
            rule.head.predicate,
            body_preds,
            body_negated,
            body_bound_masks,
            body_selected_indexes,
            body_filter_pushdown,
            body_projection_masks,
            head_term_is_variable,
            head_term_var_ids,
            head_term_literals,
            body_term_offsets,
            body_term_counts,
            body_term_is_variable,
            body_term_var_ids,
            body_term_literals,
            rule.max_var_id
        );
    if (register_status != DEDUCE_RULE_REGISTER_OK) {
        return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
    }
    return make_void(interp);
}
