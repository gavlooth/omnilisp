module lisp;

fn Value* prim_deduce_analyze(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1 || args.len > 3) {
        return deduce_raise(interp, "deduce/analyze-expected-db",
            "deduce/analyze: expected (deduce/analyze db [rule-index] [engine])");
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
    return deduce_analyze_run(db, args, interp);
}
