module lisp;

fn Value* deduce_analyze_append_read_and_rule_tail_payload(
    Value* out,
    DeduceDb* db,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    explain_dict_set(out, interp, "last-goal-directed-read-execution-path",
        db.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_symbol(interp, db.last_goal_directed_read_execution_path)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-surface",
        db.last_goal_directed_read_surface != (SymbolId)0
            ? make_symbol(interp, db.last_goal_directed_read_surface)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-fallback-reason",
        db.last_goal_directed_read_fallback_reason != (SymbolId)0
            ? make_symbol(interp, db.last_goal_directed_read_fallback_reason)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-relation",
        db.last_goal_directed_read_relation != (SymbolId)0
            ? make_symbol(interp, db.last_goal_directed_read_relation)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-selector-rule-index",
        db.last_goal_directed_read_selector_rule_index >= 0
            ? make_int(interp, db.last_goal_directed_read_selector_rule_index)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-mutation-epoch",
        db.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_int(interp, (long)db.last_goal_directed_read_mutation_epoch)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-count",
        db.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_int(interp, (long)db.last_goal_directed_read_requested_bound_count)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-requested-bound-positions",
        db.last_goal_directed_read_execution_path != (SymbolId)0
            ? deduce_bound_positions_payload(
                db.last_goal_directed_read_requested_bound_positions,
                db.last_goal_directed_read_requested_bound_count,
                interp
            )
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-count",
        db.last_goal_directed_read_execution_path != (SymbolId)0
            ? make_int(interp, (long)db.last_goal_directed_read_applied_bound_count)
            : make_nil(interp));
    explain_dict_set(out, interp, "last-goal-directed-read-applied-bound-positions",
        db.last_goal_directed_read_execution_path != (SymbolId)0
            ? deduce_bound_positions_payload(
                db.last_goal_directed_read_applied_bound_positions,
                db.last_goal_directed_read_applied_bound_count,
                interp
            )
            : make_nil(interp));

    Value* recent_integrity_violations = deduce_integrity_violation_history_payload(
        db.integrity_violation_history[..],
        db.integrity_violation_history_count,
        interp
    );
    if (recent_integrity_violations != null && recent_integrity_violations.tag == ERROR) {
        return recent_integrity_violations;
    }
    explain_dict_set(out, interp, "recent-integrity-violations", recent_integrity_violations);

    Value* rule_execution = deduce_explain_build_rule_execution_list(db, observed_rules, interp);
    if (rule_execution != null && rule_execution.tag == ERROR) {
        return rule_execution;
    }
    explain_dict_set(out, interp, "rule-execution", rule_execution);
    return null;
}
