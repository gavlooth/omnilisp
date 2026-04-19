module lisp;

import std::io;

fn bool deduce_rule_test_seed_pair_rows(Interp* interp, Value* rel, usz row_count) {
    if (interp == null || rel == null || rel.tag != FFI_HANDLE || rel.ffi_val == null) return false;
    for (usz i = 0; i < row_count; i++) {
        Value*[3] fact_args;
        fact_args[0] = rel;
        fact_args[1] = make_int(interp, (long)i);
        fact_args[2] = make_int(interp, (long)(i + 1000));
        Value* rv = prim_assert_fact(fact_args[..], null, interp);
        if (rv == null || rv.tag != VOID) return false;
    }
    return true;
}

fn bool deduce_rule_test_list_contains_symbol(Value* list, SymbolId sym) {
    Value* cur = list;
    while (is_cons(cur)) {
        Value* item = car(cur);
        if (item != null && item.tag == SYMBOL && item.sym_val == sym) return true;
        cur = cdr(cur);
    }
    return false;
}

fn bool deduce_rule_test_list_contains_int(Value* list, long value) {
    Value* cur = list;
    while (is_cons(cur)) {
        Value* item = car(cur);
        if (item != null && item.tag == INT && item.int_val == value) return true;
        cur = cdr(cur);
    }
    return false;
}

fn long deduce_rule_test_list_length(Value* list) {
    long count = 0;
    Value* cur = list;
    while (is_cons(cur)) {
        count++;
        cur = cdr(cur);
    }
    return count;
}

fn void run_deduce_rule_validation_tests_explain(Interp* interp, int* pass, int* fail) {
    run_deduce_rule_validation_tests_explain_goal_directed(interp, pass, fail);
    run_deduce_rule_validation_tests_explain_plan_shape(interp, pass, fail);
    run_deduce_rule_validation_tests_explain_aggregate_projection(interp, pass, fail);
    run_deduce_rule_validation_tests_explain_recursive_aggregate(interp, pass, fail);
    run_deduce_rule_validation_tests_more(interp, pass, fail);
}
