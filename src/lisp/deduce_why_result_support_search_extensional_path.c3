module lisp;

import std::core::mem;

fn bool deduce_why_result_try_support_path_from_signature(
    DeduceDb* db,
    MdbTxn* txn,
    DeduceRuleSignature* sig,
    Value*[] tuple,
    DeduceWhyResultVisitedSubject* visited,
    long* path_count_out,
    long* body_depth_out,
    Value** first_support_out,
    Interp* interp
) {
    bool[] bound = {};
    Value*[] bound_values = {};
    if (!deduce_why_result_bind_head_tuple_variables(sig, tuple, &bound, &bound_values)) {
        return false;
    }
    defer if (bound.ptr != null) mem::free(bound.ptr);
    defer if (bound_values.ptr != null) mem::free(bound_values.ptr);

    Value* first_support = make_nil(interp);
    long path_count = 0;
    long body_depth = 0;
    deduce_why_result_search_extensional_support_path(
        db,
        txn,
        sig,
        0,
        bound,
        bound_values,
        visited,
        &path_count,
        &body_depth,
        &first_support,
        interp
    );
    if (path_count == 0) return false;

    if (path_count_out != null) *path_count_out = path_count;
    if (body_depth_out != null) *body_depth_out = body_depth;
    if (first_support_out != null) *first_support_out = first_support;
    return true;
}
