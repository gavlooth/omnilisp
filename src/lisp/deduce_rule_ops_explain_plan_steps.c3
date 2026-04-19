module lisp;

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
    DeduceObservedRuleCounters* observed_rule_counters,
    Interp* interp
) {
    if (sig == null || sig.body_count == 0) return make_nil(interp);
    Value* rev = make_nil(interp);
    for (usz i = 0; i < sig.body_count; i++) {
        Value* step = explain_make_dict(interp, 16);
        DeduceRuleAtom op_probe = {};
        op_probe.negated = sig.body_negated != null ? sig.body_negated[i] : false;
        int selected_index = sig.body_selected_indexes != null ? sig.body_selected_indexes[i] : -1;
        ulong bound_mask = sig.body_bound_masks != null ? sig.body_bound_masks[i] : 0;
        DeducePhysicalOpKind op_kind = deduce_rule_choose_op_kind(&op_probe, selected_index);

        if (Value* err = explain_dict_set(step, interp, "position", make_int(interp, (long)i))) return err;
        if (sig.body_predicates != null) {
            if (Value* err = explain_dict_set(step, interp, "predicate", make_symbol(interp, sig.body_predicates[i]))) return err;
        } else {
            if (Value* err = explain_dict_set(step, interp, "predicate", make_nil(interp))) return err;
        }
        if (Value* err = explain_dict_set(step, interp, "negated", op_probe.negated ? make_symbol(interp, interp.sym_true) : make_nil(interp))) return err;
        if (Value* err = explain_dict_set(step, interp, "bound-mask", make_int(interp, (long)bound_mask))) return err;
        if (selected_index >= 0) {
            if (Value* err = explain_dict_set(step, interp, "selected-index", make_int(interp, (long)selected_index))) return err;
        } else {
            if (Value* err = explain_dict_set(step, interp, "selected-index", make_nil(interp))) return err;
        }
        bool filter_pushdown = sig.body_filter_pushdown != null ? sig.body_filter_pushdown[i] : (bound_mask != 0);
        ulong projection_mask = sig.body_projection_masks != null ? sig.body_projection_masks[i] : 0;
        Value* counters = deduce_explain_build_step_counters(
            db,
            sig,
            i,
            bound_mask,
            selected_index,
            observed_rule_counters,
            interp
        );
        if (counters != null && counters.tag == ERROR) return counters;
        if (Value* err = explain_dict_set(step, interp, "filter-pushdown", filter_pushdown ? make_symbol(interp, interp.sym_true) : make_nil(interp))) return err;
        if (Value* err = explain_dict_set(step, interp, "projection-mask", make_int(interp, (long)projection_mask))) return err;
        if (Value* err = explain_dict_set(step, interp, "operator", make_symbol(interp, deduce_explain_operator_symbol(interp, op_kind)))) return err;
        if (Value* err = explain_dict_set(step, interp, "counters", counters)) return err;
        rev = make_cons(interp, step, rev);
    }
    return deduce_explain_reverse_list(rev, interp);
}
