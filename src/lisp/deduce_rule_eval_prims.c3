module lisp;

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

    Value* err = deduce_rule_validate_install_candidate(db, &rule, interp);
    if (err != null) return err;
    return deduce_rule_register_ir(db, &rule, interp);
}
