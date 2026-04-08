module lisp;

fn Value* deduce_seminaive_evaluate_recursive_component(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }

    Value* err = deduce_seminaive_evaluate_recursive_component_in_txn(
        db,
        plan,
        component_id,
        iteration_limit,
        txn,
        null,
        iterations_out,
        derived_out,
        observed_rules,
        interp
    );
    if (err != null) {
        mdb_txn_abort(txn);
        return err;
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn commit failed");
    }
    return null;
}

fn Value* deduce_naive_evaluate_recursive_component(
    DeduceDb* db,
    DeduceSccPlan* plan,
    usz component_id,
    usz iteration_limit,
    long* iterations_out,
    long* derived_out,
    DeduceObservedRuleCounters[] observed_rules,
    Interp* interp
) {
    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn open failed");
    }

    Value* err = deduce_naive_evaluate_recursive_component_in_txn(
        db,
        plan,
        component_id,
        iteration_limit,
        txn,
        null,
        iterations_out,
        derived_out,
        observed_rules,
        interp
    );
    if (err != null) {
        mdb_txn_abort(txn);
        return err;
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/txn-failed", "deduce/analyze: txn commit failed");
    }
    return null;
}
