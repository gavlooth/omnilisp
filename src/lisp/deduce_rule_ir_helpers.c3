module lisp;

import std::core::mem;
import std::collections::list;

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
