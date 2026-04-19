module lisp;

import std::io;

fn void run_deduce_rule_validation_tests_explain_head_part4(Interp* interp, int* pass, int* fail) {
    {
        Value* db_val = null;
        Value* out_rel = null;
        usz expected_rows = 0;
        bool ok = deduce_query_bench_seed_recursive_aggregate_negated_db(interp, 4, &db_val, &out_rel, &expected_rows);

        Value* explained = null;
        if (ok) {
            Value*[2] explain_args;
            explain_args[0] = db_val;
            explain_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("bench_recursive_neg_total"));
            explained = prim_deduce_explain(explain_args[..], null, interp);
            ok = explained != null && explained.tag == HASHMAP && explained.hashmap_val != null;
        }

        if (ok) {
            Value* eligible = deduce_rule_test_dict_get(interp, explained, "goal-directed-eligible");
            Value* shape = deduce_rule_test_dict_get(interp, explained, "goal-directed-shape");
            Value* blockers = deduce_rule_test_dict_get(interp, explained, "goal-directed-blockers");
            Value* execution_path = deduce_rule_test_dict_get(interp, explained, "goal-directed-execution-path");
            Value* selected_components = deduce_rule_test_dict_get(interp, explained, "goal-directed-selected-components");
            Value* selected_predicates = deduce_rule_test_dict_get(interp, explained, "goal-directed-selected-predicates");
            Value* demand_bound_head_positions = deduce_rule_test_dict_get(
                interp,
                explained,
                "goal-directed-demand-bound-head-positions"
            );
            ok = eligible != null && eligible.tag == NIL &&
                 shape != null && shape.tag == NIL &&
                 execution_path != null && execution_path.tag == NIL &&
                 selected_components != null && selected_components.tag == NIL &&
                 demand_bound_head_positions != null && demand_bound_head_positions.tag == NIL &&
                 selected_predicates != null && selected_predicates.tag == NIL &&
                 deduce_rule_test_list_contains_symbol(blockers, interp.symbols.intern("aggregates-present")) &&
                 deduce_rule_test_list_contains_symbol(blockers, interp.symbols.intern("negated-body-atom"));
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain reports goal-directed blockers for recursive aggregate/negated shapes");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain reports goal-directed blockers for recursive aggregate/negated shapes");
            (*fail)++;
        }
    }
}
