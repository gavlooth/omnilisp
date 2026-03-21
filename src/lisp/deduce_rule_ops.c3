module lisp;

import std::core::mem;
import std::collections::list;

enum DeduceRuleTermKind {
    DEDUCE_RULE_TERM_LITERAL,
    DEDUCE_RULE_TERM_VARIABLE,
    DEDUCE_RULE_TERM_AGGREGATE,
}

enum DeduceAggregateOpKind {
    DEDUCE_AGGREGATE_NONE,
    DEDUCE_AGGREGATE_COUNT,
    DEDUCE_AGGREGATE_SUM,
    DEDUCE_AGGREGATE_MIN,
    DEDUCE_AGGREGATE_MAX,
}

struct DeduceRuleTerm {
    DeduceRuleTermKind   kind;
    DeduceAggregateOpKind aggregate_op;
    uint                 normalized_var_id;
    Value*               literal_value;
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
    bool allow_aggregate_terms,
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
        parsed_term.aggregate_op = DEDUCE_AGGREGATE_NONE;
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
        } else if (is_cons(term)) {
            if (!allow_aggregate_terms) {
                return deduce_raise(interp, "deduce/invalid-aggregate", "deduce-rule!: aggregate terms are only allowed in head projections");
            }

            Value* agg_fn = car(term);
            Value* agg_args = cdr(term);
            if (agg_fn == null || agg_fn.tag != SYMBOL ||
                !is_cons(agg_args) || !is_nil(cdr(agg_args))) {
                return deduce_raise(interp, "deduce/invalid-aggregate", "deduce-rule!: aggregate projection must be (count|sum|min|max ?var)");
            }

            char[] agg_name = interp.symbols.get_name(agg_fn.sym_val);
            DeduceAggregateOpKind aggregate_op = DEDUCE_AGGREGATE_NONE;
            bool deferred_aggregate = false;
            if (deduce_name_equals(agg_name, "count")) {
                aggregate_op = DEDUCE_AGGREGATE_COUNT;
            } else if (deduce_name_equals(agg_name, "sum")) {
                aggregate_op = DEDUCE_AGGREGATE_SUM;
            } else if (deduce_name_equals(agg_name, "min")) {
                aggregate_op = DEDUCE_AGGREGATE_MIN;
            } else if (deduce_name_equals(agg_name, "max")) {
                aggregate_op = DEDUCE_AGGREGATE_MAX;
            } else if (deduce_name_equals(agg_name, "avg") ||
                       deduce_name_equals(agg_name, "distinct-count")) {
                deferred_aggregate = true;
            } else {
                return deduce_raise(interp, "deduce/invalid-aggregate", "deduce-rule!: unknown aggregate function");
            }

            if (deferred_aggregate) {
                return deduce_raise(interp, "deduce/unsupported-aggregate", "deduce-rule!: aggregate function is deferred from the v1.5 surface");
            }

            Value* agg_input = car(agg_args);
            if (agg_input == null || agg_input.tag != SYMBOL ||
                !deduce_rule_is_logic_var_symbol(agg_input.sym_val, interp)) {
                return deduce_raise(interp, "deduce/invalid-aggregate", "deduce-rule!: aggregate projection must be (count|sum|min|max ?var)");
            }

            uint var_id = deduce_rule_get_or_add_var_id(vars, agg_input.sym_val, max_var_id);
            if (var_id == 0) {
                return deduce_raise(interp, "deduce/rule-out-of-memory", "deduce-rule!: out of memory");
            }

            parsed_term.kind = DEDUCE_RULE_TERM_AGGREGATE;
            parsed_term.aggregate_op = aggregate_op;
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

    Value* head_err = deduce_rule_parse_atom(args[0], false, true, &vars, &out.max_var_id, &out.head, interp);
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
        Value* body_err = deduce_rule_parse_atom(args[i + 1], true, false, &vars, &out.max_var_id, &out.body[i], interp);
        if (body_err != null) return body_err;
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
        if (term.kind != DEDUCE_RULE_TERM_VARIABLE &&
            term.kind != DEDUCE_RULE_TERM_AGGREGATE) continue;
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
        if ((term.kind == DEDUCE_RULE_TERM_VARIABLE ||
             term.kind == DEDUCE_RULE_TERM_AGGREGATE) &&
            term.normalized_var_id == var_id) {
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
