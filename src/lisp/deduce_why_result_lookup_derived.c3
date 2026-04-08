module lisp;

fn Value* deduce_why_result_build_derived_payload(
    Relation* rel,
    Value*[] tuple,
    MdbTxn* txn,
    char* subject_key,
    usz subject_key_len,
    Interp* interp
) {
    DeduceWhyResultVisitedSubject root_subject = {
        .predicate = rel.name,
        .tuple = tuple,
        .parent = null
    };
    long rule_index = -1;
    long path_count = 0;
    long max_depth = 0;
    Value* first_support = make_nil(interp);
    if (deduce_why_result_try_supported_subject_support(
            rel.db,
            txn,
            rel.name,
            tuple,
            &root_subject,
            &rule_index,
            &path_count,
            &max_depth,
            &first_support,
            interp
        ) && path_count > 0 && rule_index >= 0) {
        Value* path = deduce_why_result_build_derived_path(
            rel,
            tuple,
            rule_index,
            first_support,
            interp
        );
        deduce_why_result_maybe_attach_path_goal_directed_read_context(
            path,
            rel,
            subject_key,
            subject_key_len,
            interp
        );
        deduce_why_result_maybe_attach_common_support_goal_directed_read_context(
            path,
            first_support,
            interp
        );
        deduce_why_result_maybe_attach_support_goal_directed_read_contexts(
            path,
            first_support,
            interp
        );
        Value* paths = make_cons(interp, path, make_nil(interp));
        return deduce_why_result_build_payload(
            rel,
            tuple,
            interp.symbols.intern(path_count > 1 ? "partial" : "ok"),
            paths,
            1,
            max_depth,
            path_count > 1,
            interp
        );
    }

    return deduce_why_result_build_payload(
        rel,
        tuple,
        interp.symbols.intern("error"),
        make_nil(interp),
        0,
        0,
        false,
        interp,
        interp.symbols.intern("deduce/why-result-derived-subject-not-yet-supported")
    );
}
