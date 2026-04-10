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

    // Keep derived subjects on a structured provenance surface even when exact
    // support reconstruction cannot prove a concrete path in this pass.
    long fallback_rule_index = -1;
    for (usz i = 0; i < rel.db.rule_signature_count; i++) {
        if (rel.db.rule_signatures[i].head_predicate == rel.name) {
            fallback_rule_index = (long)i;
            break;
        }
    }
    Value* fallback_path = deduce_why_result_build_derived_path(
        rel,
        tuple,
        fallback_rule_index >= 0 ? fallback_rule_index : 0,
        make_nil(interp),
        interp
    );
    if (fallback_path == null) {
        return deduce_raise(interp, "deduce/why-result-out-of-memory", "deduce/why-result: out of memory");
    }
    if (fallback_path.tag == ERROR) return fallback_path;
    deduce_why_result_maybe_attach_path_goal_directed_read_context(
        fallback_path,
        rel,
        subject_key,
        subject_key_len,
        interp
    );
    if (fallback_path == null || fallback_path.tag != HASHMAP || fallback_path.hashmap_val == null) {
        return deduce_why_result_build_payload(
            rel,
            tuple,
            interp.symbols.intern("partial"),
            make_nil(interp),
            0,
            0,
            true,
            interp
        );
    }
    Value* fallback_paths = make_cons(interp, fallback_path, make_nil(interp));
    return deduce_why_result_build_payload(
        rel,
        tuple,
        interp.symbols.intern("partial"),
        fallback_paths,
        1,
        1,
        true,
        interp
    );
}
