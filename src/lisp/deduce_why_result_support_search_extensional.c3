module lisp;

fn bool deduce_why_result_search_extensional_support_path(
    DeduceDb* db,
    MdbTxn* txn,
    DeduceRuleSignature* sig,
    usz step_index,
    bool[] bound,
    Value*[] bound_values,
    DeduceWhyResultVisitedSubject* visited,
    long* path_count_out,
    long* first_body_depth_out,
    Value** first_support_out,
    Interp* interp
) {
    if (db == null || txn == null || sig == null || path_count_out == null ||
        first_body_depth_out == null || first_support_out == null || interp == null) {
        return false;
    }

    if (step_index >= sig.body_count) {
        *path_count_out = *path_count_out + 1;
        if (*path_count_out == 1) {
            *first_support_out = make_nil(interp);
            *first_body_depth_out = 0;
        }
        return *path_count_out >= 2;
    }

    MdbDbi source_dbi = 0;
    DeduceRelationSchema* source_schema = null;
    if (!deduce_rule_open_existing_dbi(
            db,
            txn,
            sig.body_predicates[step_index],
            &source_dbi,
            &source_schema,
            interp
        )) {
        return false;
    }
    if (source_schema == null) return false;

    Relation source_rel = deduce_relation_view_from_schema(db, source_schema, source_dbi);
    DeduceTupleIterator it = {};
    if (!deduce_tuple_iterator_open_in_txn(
            &it,
            &source_rel,
            interp,
            txn,
            null,
            0,
            false,
            {},
            {},
            false
        )) {
        return false;
    }
    defer deduce_tuple_iterator_close(&it);

    while (deduce_tuple_iterator_next(&it)) {
        bool stop = deduce_why_result_search_extensional_support_candidate(
            db,
            txn,
            sig,
            step_index,
            bound,
            bound_values,
            &source_rel,
            source_schema,
            it.vals,
            visited,
            path_count_out,
            first_body_depth_out,
            first_support_out,
            interp
        );
        if (stop) return true;
    }

    return false;
}
