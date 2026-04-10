module lisp;

fn Value* deduce_why_result_build_missing_payload(
    Relation* rel,
    Value*[] tuple,
    Interp* interp
) {
    return deduce_why_result_build_payload(
        rel,
        tuple,
        interp.symbols.intern("missing"),
        make_nil(interp),
        0,
        0,
        false,
        interp
    );
}

fn Value* deduce_why_result_build_seed_payload(
    Relation* rel,
    Value*[] tuple,
    char* subject_key,
    usz subject_key_len,
    Interp* interp
) {
    Value* path = deduce_why_result_build_seed_path(rel, tuple, interp);
    if (path == null) {
        return deduce_raise(interp, "deduce/why-result-out-of-memory", "deduce/why-result: out of memory");
    }
    if (path.tag == ERROR) return path;
    deduce_why_result_maybe_attach_path_goal_directed_read_context(
        path,
        rel,
        subject_key,
        subject_key_len,
        interp
    );
    Value* paths = make_cons(interp, path, make_nil(interp));
    return deduce_why_result_build_payload(
        rel,
        tuple,
        interp.symbols.intern("ok"),
        paths,
        1,
        1,
        false,
        interp
    );
}

fn Value* deduce_why_result_build_lookup_payload(
    Relation* rel,
    Value*[] tuple,
    MdbTxn* txn,
    char* subject_key,
    usz subject_key_len,
    Interp* interp
) {
    MdbVal key = { .mv_size = subject_key_len, .mv_data = subject_key };
    MdbVal data = {};
    bool found = mdb_get(txn, rel.dbi, &key, &data) == MDB_SUCCESS;
    if (!found) {
        return deduce_why_result_build_missing_payload(rel, tuple, interp);
    }

    if (deduce_relation_schema_has_derived_rules(rel.db, deduce_relation_lookup_schema(rel))) {
        return deduce_why_result_build_derived_payload(
            rel,
            tuple,
            txn,
            subject_key,
            subject_key_len,
            interp
        );
    }

    return deduce_why_result_build_seed_payload(
        rel,
        tuple,
        subject_key,
        subject_key_len,
        interp
    );
}
