module lisp;

<*
@require interp != null
*>
fn Value* prim_deduce_explain(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 3) {
        return deduce_raise(interp, "deduce/explain-expected-db-selector",
            "deduce/explain: expected (deduce/explain db [head-symbol|rule-index] [engine])");
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

    DeduceExplainRequest request = {};
    Value* request_err = deduce_explain_parse_request(db, args, &request, interp);
    if (request_err != null) return request_err;

    DeduceSccPlan scc_plan = {};
    Value* plan_err = deduce_build_scc_plan(db, &scc_plan, interp);
    if (plan_err != null) return plan_err;
    defer deduce_scc_plan_free(&scc_plan);
    usz[] parallel_wave_ids = {};
    usz[] parallel_batch_sizes = {};
    Value* parallel_err = deduce_build_parallel_component_batch_metadata(
        &scc_plan,
        &parallel_wave_ids,
        &parallel_batch_sizes,
        interp
    );
    if (parallel_err != null) return parallel_err;
    defer if (parallel_wave_ids.ptr != null) mem::free(parallel_wave_ids.ptr);
    defer if (parallel_batch_sizes.ptr != null) mem::free(parallel_batch_sizes.ptr);

    DeduceExplainGoalDirectedMetadata metadata = {};
    Value* metadata_err = deduce_explain_prepare_goal_directed_metadata(
        db,
        &scc_plan,
        parallel_wave_ids,
        &request,
        &metadata,
        interp
    );
    if (metadata_err != null) return metadata_err;

    return deduce_explain_build_snapshot(
        db,
        &request,
        &scc_plan,
        parallel_wave_ids,
        parallel_batch_sizes,
        &metadata,
        interp
    );
}
