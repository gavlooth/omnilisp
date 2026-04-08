module lisp;

fn Value* deduce_naive_evaluate_nonrecursive_component(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    if (db.incremental_full_recompute_required) {
        MdbTxn* txn = null;
        int rc = mdb_txn_begin(db.env, null, 0, &txn);
        if (rc != MDB_SUCCESS || txn == null) {
            return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
        }

        bool changed = false;
        long derived = 0;
        Value* err = deduce_seminaive_clear_component_predicates(db, plan, component_id, txn, interp);
        if (err == null) {
            err = deduce_seminaive_seed_component_extensional_state_in_txn(
                db,
                plan,
                component_id,
                txn,
                {},
                {},
                true,
                interp
            );
        }
        if (err == null) {
            err = deduce_naive_evaluate_component_pass_in_txn(
                db,
                plan,
                component_id,
                txn,
                null,
                &changed,
                &derived,
                observed_rules,
                interp
            );
        }
        if (err == null) {
            err = deduce_validate_component_ref_head_snapshots(db, plan, component_id, txn, interp);
        }
        if (err != null) {
            mdb_txn_abort(txn);
            return err;
        }
        rc = mdb_txn_commit(txn);
        if (rc != MDB_SUCCESS) {
            return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn commit failed");
        }
        if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
        if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;
        return null;
    }

    bool changed = false;
    long derived = 0;
    Value* err = deduce_naive_evaluate_component_pass(
        db,
        plan,
        component_id,
        &changed,
        &derived,
        observed_rules,
        interp
    );
    if (err != null) return err;
    if (iterations_out != null && *iterations_out < long.max) *iterations_out = *iterations_out + 1;
    if (derived_out != null && derived > 0) *derived_out = *derived_out + derived;
    return null;
}
