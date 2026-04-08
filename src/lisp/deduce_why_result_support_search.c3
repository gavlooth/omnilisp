module lisp;

import std::core::mem;

fn bool deduce_why_result_try_supported_subject_support(
    DeduceDb* db,
    MdbTxn* txn,
    SymbolId predicate,
    Value*[] tuple,
    DeduceWhyResultVisitedSubject* visited,
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

    usz head_rule_count = 0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (db.rule_signatures[i].head_predicate == predicate) head_rule_count++;
    }
    if (head_rule_count == 0) return false;

    if (head_rule_count == 1) {
        long direct_rule_index = -1;
        SymbolId source_predicate = (SymbolId)0;
        DeduceRuleSignature* source_sig = null;
        if (deduce_why_result_find_supported_single_rule(
                db,
                predicate,
                &direct_rule_index,
                &source_predicate,
                &source_sig
            )) {
                long path_count = 0;
                long max_depth = 0;
                Value* first_support = make_nil(interp);
                if (deduce_why_result_try_supported_single_rule_support(
                        db,
                        txn,
                        source_sig,
                        tuple,
                        visited,
                        &path_count,
                        &max_depth,
                        &first_support,
                        interp
                    ) && path_count > 0) {
                        if (rule_index_out != null) *rule_index_out = direct_rule_index;
                        *path_count_out = path_count;
                        if (max_depth_out != null) *max_depth_out = max_depth;
                        *first_support_out = first_support;
                        return true;
                }
        }
    }

    long total_path_count = 0;
    long first_rule_index = -1;
    long first_max_depth = 0;
    Value* first_support = make_nil(interp);
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        if (sig.head_predicate != predicate) continue;

        long rule_path_count = 0;
        long rule_max_depth = 0;
        Value* rule_support = make_nil(interp);
        if (!deduce_why_result_try_supported_single_rule_support(
                db,
                txn,
                sig,
                tuple,
                visited,
                &rule_path_count,
                &rule_max_depth,
                &rule_support,
                interp
            )) {
            continue;
        }
        if (rule_path_count <= 0) continue;
        if (total_path_count == 0) {
            first_rule_index = (long)i;
            first_max_depth = rule_max_depth;
            first_support = rule_support;
        }
        total_path_count += rule_path_count;
        if (total_path_count >= 2) {
            total_path_count = 2;
            break;
        }
    }

    if (total_path_count == 0 || first_rule_index < 0) return false;
    if (rule_index_out != null) *rule_index_out = first_rule_index;
    *path_count_out = total_path_count;
    if (max_depth_out != null) *max_depth_out = first_max_depth;
    *first_support_out = first_support;
    return true;
}
