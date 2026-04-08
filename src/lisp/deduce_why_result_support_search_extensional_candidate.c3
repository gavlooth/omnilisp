module lisp;

import std::core::mem;

fn bool deduce_why_result_search_extensional_support_candidate(
    DeduceDb* db,
    MdbTxn* txn,
    DeduceRuleSignature* sig,
    usz step_index,
    bool[] bound,
    Value*[] bound_values,
    Relation* source_rel,
    DeduceRelationSchema* source_schema,
    Value*[] tuple,
    DeduceWhyResultVisitedSubject* visited,
    long* path_count_out,
    long* first_body_depth_out,
    Value** first_support_out,
    Interp* interp
) {
    bool[] next_bound = {};
    Value*[] next_values = {};
    if (!deduce_why_result_clone_bindings(bound, bound_values, &next_bound, &next_values)) {
        return false;
    }
    defer {
        if (next_bound.ptr != null) mem::free(next_bound.ptr);
        if (next_values.ptr != null) mem::free(next_values.ptr);
    }

    if (!deduce_rule_match_positive_tuple(sig, step_index, tuple, next_bound, next_values)) {
        return false;
    }

    Value* matched_support = make_nil(interp);
    long matched_path_count = 1;
    long matched_depth = 1;
    if (deduce_relation_schema_has_derived_rules(db, source_schema)) {
        if (deduce_why_result_visited_contains(visited, source_rel.name, tuple)) {
            return false;
        }
        DeduceWhyResultVisitedSubject child_visited = {
            .predicate = source_rel.name,
            .tuple = tuple,
            .parent = visited
        };
        long child_rule_index = -1;
        if (!deduce_why_result_try_supported_subject_support(
                db,
                txn,
                source_rel.name,
                tuple,
                &child_visited,
                &child_rule_index,
                &matched_path_count,
                &matched_depth,
                &matched_support,
                interp
            ) || matched_path_count != 1) {
            return false;
        }
        if (child_rule_index < 0) return false;
        Value* rule_step = deduce_why_result_build_rule_step_frame(
            source_rel,
            tuple,
            child_rule_index,
            (long)step_index,
            interp
        );
        if (rule_step == null || rule_step.tag != HASHMAP || rule_step.hashmap_val == null) {
            return false;
        }
        matched_support = deduce_why_result_concat_support(
            matched_support,
            make_cons(interp, rule_step, make_nil(interp)),
            interp
        );
    } else {
        Value* frame = deduce_why_result_build_fact_frame(source_rel, tuple, interp);
        if (frame == null || frame.tag != HASHMAP || frame.hashmap_val == null) {
            return false;
        }
        matched_support = make_cons(interp, frame, make_nil(interp));
    }

    long count_before = *path_count_out;
    long tail_body_depth = 0;
    bool stop = deduce_why_result_search_extensional_support_path(
        db,
        txn,
        sig,
        step_index + 1,
        next_bound,
        next_values,
        visited,
        path_count_out,
        &tail_body_depth,
        first_support_out,
        interp
    );
    if (count_before == 0 && *path_count_out > 0) {
        *first_support_out = deduce_why_result_concat_support(matched_support, *first_support_out, interp);
        *first_body_depth_out = matched_depth > tail_body_depth ? matched_depth : tail_body_depth;
    }
    return stop;
}
