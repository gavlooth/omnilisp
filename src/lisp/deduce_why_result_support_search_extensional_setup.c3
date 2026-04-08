module lisp;

import std::core::mem;

fn bool deduce_why_result_try_exact_one_rule_extensional_support(
    DeduceDb* db,
    MdbTxn* txn,
    SymbolId predicate,
    Value*[] tuple,
    long* rule_index_out,
    long* path_count_out,
    long* max_depth_out,
    Value** first_support_out,
    Interp* interp
) {
    if (rule_index_out != null) *rule_index_out = -1;
    if (path_count_out != null) *path_count_out = 0;
    if (max_depth_out != null) *max_depth_out = 0;
    if (first_support_out != null) *first_support_out = make_nil(interp);
    if (db == null || txn == null || predicate == (SymbolId)0 ||
        path_count_out == null || first_support_out == null || interp == null) {
        return false;
    }

    long rule_index = -1;
    SymbolId source_predicate = (SymbolId)0;
    DeduceRuleSignature* sig = null;
    if (!deduce_why_result_find_supported_extensional_rule(
            db,
            predicate,
            &rule_index,
            &source_predicate,
            &sig
        )) {
        return false;
    }

    Value* first_support = make_nil(interp);
    long path_count = 0;
    long body_depth = 0;
    if (!deduce_why_result_try_support_path_from_signature(
            db,
            txn,
            sig,
            tuple,
            null,
            &path_count,
            &body_depth,
            &first_support,
            interp
        )) {
        return false;
    }

    if (rule_index_out != null) *rule_index_out = rule_index;
    *path_count_out = path_count;
    if (max_depth_out != null) *max_depth_out = body_depth + 1;
    *first_support_out = first_support;
    return true;
}

fn bool deduce_why_result_try_supported_single_rule_support(
    DeduceDb* db,
    MdbTxn* txn,
    DeduceRuleSignature* sig,
    Value*[] tuple,
    DeduceWhyResultVisitedSubject* visited,
    long* path_count_out,
    long* max_depth_out,
    Value** first_support_out,
    Interp* interp
) {
    if (path_count_out != null) *path_count_out = 0;
    if (max_depth_out != null) *max_depth_out = 0;
    if (first_support_out != null) *first_support_out = make_nil(interp);
    if (db == null || txn == null || sig == null || path_count_out == null ||
        first_support_out == null || interp == null) {
        return false;
    }

    Value* first_support = make_nil(interp);
    long path_count = 0;
    long body_depth = 0;
    if (!deduce_why_result_try_support_path_from_signature(
            db,
            txn,
            sig,
            tuple,
            visited,
            &path_count,
            &body_depth,
            &first_support,
            interp
        )) {
        return false;
    }

    *path_count_out = path_count;
    if (max_depth_out != null) *max_depth_out = body_depth + 1;
    *first_support_out = first_support;
    return true;
}
