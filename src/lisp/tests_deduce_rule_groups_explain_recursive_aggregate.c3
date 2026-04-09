module lisp;

import std::io;

fn void run_deduce_rule_validation_tests_explain_recursive_aggregate(Interp* interp, int* pass, int* fail) {
    {
        Value* db_val = null;
        Value* out_rel = null;
        usz expected_rows = 0;
        bool ok = deduce_query_bench_seed_recursive_aggregate_db(interp, 4, &db_val, &out_rel, &expected_rows);

        Value* analyzed = null;
        Value* explained = null;
        if (ok) {
            Value*[2] analyze_args;
            analyze_args[0] = db_val;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("semi-naive"));
            analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            ok = analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null;
        }

        if (ok) {
            Value*[2] explain_args;
            explain_args[0] = db_val;
            explain_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("bench_recursive_total"));
            explained = prim_deduce_explain(explain_args[..], null, interp);
            ok = explained != null && explained.tag == HASHMAP && explained.hashmap_val != null;
        }

        if (ok) {
            Value* analyze_status = deduce_rule_test_dict_get(interp, analyzed, "status");
            Value* analyze_engine = deduce_rule_test_dict_get(interp, analyzed, "execution-engine");
            Value* analyze_requested = deduce_rule_test_dict_get(interp, analyzed, "requested-execution-engine");
            Value* analyze_fallback = deduce_rule_test_dict_get(interp, analyzed, "recursive-aggregate-naive-fallback");
            Value* aggregate_count = deduce_rule_test_dict_get(interp, explained, "aggregate-projection-count");

            ok = analyze_status != null && analyze_status.tag == SYMBOL &&
                 analyze_status.sym_val == interp.symbols.intern("ok") &&
                 analyze_engine != null && analyze_engine.tag == SYMBOL &&
                 analyze_engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 analyze_requested != null && analyze_requested.tag == SYMBOL &&
                 analyze_requested.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 analyze_fallback != null && analyze_fallback.tag == NIL &&
                 expected_rows == 4 &&
                 aggregate_count != null && aggregate_count.tag == INT &&
                 aggregate_count.int_val == 1;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain preserves recursive aggregate metadata on semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain preserves recursive aggregate metadata on semi-naive-scc");
            (*fail)++;
        }
    }

    {
        Value* db_val = null;
        Value* out_rel = null;
        usz expected_rows = 0;
        bool ok = deduce_query_bench_seed_recursive_aggregate_db(interp, 4, &db_val, &out_rel, &expected_rows);

        Value* explained = null;
        if (ok) {
            Value*[3] explain_args;
            explain_args[0] = db_val;
            explain_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("bench_recursive_total"));
            explain_args[2] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("naive"));
            explained = prim_deduce_explain(explain_args[..], null, interp);
            ok = explained != null && explained.tag == HASHMAP && explained.hashmap_val != null;
        }

        if (ok) {
            Value* execution_engine = deduce_rule_test_dict_get(interp, explained, "execution-engine");
            Value* requested_engine = deduce_rule_test_dict_get(interp, explained, "requested-execution-engine");
            Value* fallback = deduce_rule_test_dict_get(interp, explained, "recursive-aggregate-naive-fallback");
            Value* routed = deduce_rule_test_dict_get(
                interp,
                explained,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );

            ok = execution_engine != null && execution_engine.tag == SYMBOL &&
                 execution_engine.sym_val == interp.symbols.intern("naive-scc") &&
                 requested_engine != null && requested_engine.tag == SYMBOL &&
                 requested_engine.sym_val == interp.symbols.intern("naive-scc") &&
                 fallback != null && fallback.tag == NIL &&
                 routed != null && routed.tag == NIL;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain keeps naive pure positive recursive aggregates off mixed-route diagnostics");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain keeps naive pure positive recursive aggregates off mixed-route diagnostics");
            (*fail)++;
        }
    }

    {
        Value* db_val = null;
        Value* out_rel = null;
        usz expected_rows = 0;
        bool ok = deduce_query_bench_seed_recursive_aggregate_negated_db(interp, 4, &db_val, &out_rel, &expected_rows);

        Value* explained = null;
        if (ok) {
            Value*[3] explain_args;
            explain_args[0] = db_val;
            explain_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("bench_recursive_neg_total"));
            explain_args[2] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("naive"));
            explained = prim_deduce_explain(explain_args[..], null, interp);
            ok = explained != null && explained.tag == HASHMAP && explained.hashmap_val != null;
        }

        if (ok) {
            Value* execution_engine = deduce_rule_test_dict_get(interp, explained, "execution-engine");
            Value* requested_engine = deduce_rule_test_dict_get(interp, explained, "requested-execution-engine");
            Value* fallback = deduce_rule_test_dict_get(interp, explained, "recursive-aggregate-naive-fallback");
            Value* routed = deduce_rule_test_dict_get(
                interp,
                explained,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );

            ok = execution_engine != null && execution_engine.tag == SYMBOL &&
                 execution_engine.sym_val == interp.symbols.intern("naive-scc") &&
                 requested_engine != null && requested_engine.tag == SYMBOL &&
                 requested_engine.sym_val == interp.symbols.intern("naive-scc") &&
                 fallback != null && fallback.tag == NIL &&
                 routed != null && routed.tag == NIL;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain keeps naive pure negated recursive aggregates off mixed-route diagnostics");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain keeps naive pure negated recursive aggregates off mixed-route diagnostics");
            (*fail)++;
        }
    }

    {
        Value* db_val = null;
        Value* score_rel = null;
        Value* chosen_rel = null;
        usz expected_rows = 0;
        bool ok = deduce_query_bench_seed_recursive_mixed_aggregate_db(
            interp,
            4,
            &db_val,
            &score_rel,
            &chosen_rel,
            &expected_rows
        );

        Value* analyzed = null;
        Value* explained = null;
        if (ok) {
            Value*[2] analyze_args;
            analyze_args[0] = db_val;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("semi-naive"));
            analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            ok = analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null;
        }

        if (ok) {
            Value*[2] explain_args;
            explain_args[0] = db_val;
            explain_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("bench_mixed_score"));
            explained = prim_deduce_explain(explain_args[..], null, interp);
            ok = explained != null && explained.tag == HASHMAP && explained.hashmap_val != null;
        }

        if (ok) {
            Value* analyze_status = deduce_rule_test_dict_get(interp, analyzed, "status");
            Value* analyze_engine = deduce_rule_test_dict_get(interp, analyzed, "execution-engine");
            Value* analyze_requested = deduce_rule_test_dict_get(interp, analyzed, "requested-execution-engine");
            Value* analyze_fallback = deduce_rule_test_dict_get(interp, analyzed, "recursive-aggregate-naive-fallback");
            Value* aggregate_count = deduce_rule_test_dict_get(interp, explained, "aggregate-projection-count");
            Value*[1] score_count_args;
            score_count_args[0] = score_rel;
            Value* score_count = prim_deduce_count(score_count_args[..], null, interp);
            Value*[1] chosen_count_args;
            chosen_count_args[0] = chosen_rel;
            Value* chosen_count = prim_deduce_count(chosen_count_args[..], null, interp);

            ok = analyze_status != null && analyze_status.tag == SYMBOL &&
                 analyze_status.sym_val == interp.symbols.intern("ok") &&
                 analyze_engine != null && analyze_engine.tag == SYMBOL &&
                 analyze_engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 analyze_requested != null && analyze_requested.tag == SYMBOL &&
                 analyze_requested.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 analyze_fallback != null && analyze_fallback.tag == NIL &&
                 score_count != null && score_count.tag == INT && score_count.int_val == (long)expected_rows &&
                 chosen_count != null && chosen_count.tag == INT && chosen_count.int_val == (long)expected_rows &&
                 aggregate_count != null && aggregate_count.tag == INT &&
                 aggregate_count.int_val == 1;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain preserves mixed positive recursive aggregate metadata on semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain preserves mixed positive recursive aggregate metadata on semi-naive-scc");
            (*fail)++;
        }
    }

    {
        Value* db_val = null;
        Value* score_rel = null;
        Value* chosen_rel = null;
        usz expected_rows = 0;
        bool ok = deduce_query_bench_seed_recursive_mixed_aggregate_db(
            interp,
            4,
            &db_val,
            &score_rel,
            &chosen_rel,
            &expected_rows
        );

        Value* explained = null;
        if (ok) {
            Value*[3] explain_args;
            explain_args[0] = db_val;
            explain_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("bench_mixed_score"));
            explain_args[2] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("naive"));
            explained = prim_deduce_explain(explain_args[..], null, interp);
            ok = explained != null && explained.tag == HASHMAP && explained.hashmap_val != null;
        }

        if (ok) {
            Value* execution_engine = deduce_rule_test_dict_get(interp, explained, "execution-engine");
            Value* requested_engine = deduce_rule_test_dict_get(interp, explained, "requested-execution-engine");
            Value* fallback = deduce_rule_test_dict_get(interp, explained, "recursive-aggregate-naive-fallback");
            Value* routed = deduce_rule_test_dict_get(
                interp,
                explained,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );

            ok = execution_engine != null && execution_engine.tag == SYMBOL &&
                 execution_engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 requested_engine != null && requested_engine.tag == SYMBOL &&
                 requested_engine.sym_val == interp.symbols.intern("naive-scc") &&
                 fallback != null && fallback.tag == NIL &&
                 routed != null && routed.tag == SYMBOL &&
                 routed.sym_val == interp.sym_true;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain routes naive mixed positive recursive aggregate SCCs to semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain routes naive mixed positive recursive aggregate SCCs to semi-naive-scc");
            (*fail)++;
        }
    }

    {
        Value* db_val = null;
        Value* out_rel = null;
        usz expected_rows = 0;
        bool ok = deduce_query_bench_seed_recursive_aggregate_negated_db(interp, 4, &db_val, &out_rel, &expected_rows);

        Value* analyzed = null;
        Value* explained = null;
        if (ok) {
            Value*[2] analyze_args;
            analyze_args[0] = db_val;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("semi-naive"));
            analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            ok = analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null;
        }

        if (ok) {
            Value*[2] explain_args;
            explain_args[0] = db_val;
            explain_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("bench_recursive_neg_total"));
            explained = prim_deduce_explain(explain_args[..], null, interp);
            ok = explained != null && explained.tag == HASHMAP && explained.hashmap_val != null;
        }

        if (ok) {
            Value* analyze_status = deduce_rule_test_dict_get(interp, analyzed, "status");
            Value* analyze_engine = deduce_rule_test_dict_get(interp, analyzed, "execution-engine");
            Value* analyze_requested = deduce_rule_test_dict_get(interp, analyzed, "requested-execution-engine");
            Value* analyze_fallback = deduce_rule_test_dict_get(interp, analyzed, "recursive-aggregate-naive-fallback");
            Value* aggregate_count = deduce_rule_test_dict_get(interp, explained, "aggregate-projection-count");

            ok = analyze_status != null && analyze_status.tag == SYMBOL &&
                 analyze_status.sym_val == interp.symbols.intern("ok") &&
                 analyze_engine != null && analyze_engine.tag == SYMBOL &&
                 analyze_engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 analyze_requested != null && analyze_requested.tag == SYMBOL &&
                 analyze_requested.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 analyze_fallback != null && analyze_fallback.tag == NIL &&
                 expected_rows == 3 &&
                 aggregate_count != null && aggregate_count.tag == INT &&
                 aggregate_count.int_val == 1;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain preserves negated recursive aggregate metadata on semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain preserves negated recursive aggregate metadata on semi-naive-scc");
            (*fail)++;
        }
    }

    {
        Value* db_val = null;
        Value* score_rel = null;
        Value* chosen_rel = null;
        usz expected_score_rows = 0;
        usz expected_chosen_rows = 0;
        bool ok = deduce_query_bench_seed_recursive_mixed_aggregate_negated_db(
            interp,
            4,
            &db_val,
            &score_rel,
            &chosen_rel,
            &expected_score_rows,
            &expected_chosen_rows
        );

        Value* analyzed = null;
        Value* explained = null;
        if (ok) {
            Value*[2] analyze_args;
            analyze_args[0] = db_val;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("semi-naive"));
            analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            ok = analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null;
        }

        if (ok) {
            Value*[2] explain_args;
            explain_args[0] = db_val;
            explain_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("bench_mixed_neg_score"));
            explained = prim_deduce_explain(explain_args[..], null, interp);
            ok = explained != null && explained.tag == HASHMAP && explained.hashmap_val != null;
        }

        if (ok) {
            Value* analyze_status = deduce_rule_test_dict_get(interp, analyzed, "status");
            Value* analyze_engine = deduce_rule_test_dict_get(interp, analyzed, "execution-engine");
            Value* analyze_requested = deduce_rule_test_dict_get(interp, analyzed, "requested-execution-engine");
            Value* analyze_fallback = deduce_rule_test_dict_get(interp, analyzed, "recursive-aggregate-naive-fallback");
            Value* aggregate_count = deduce_rule_test_dict_get(interp, explained, "aggregate-projection-count");
            Value*[1] score_count_args;
            score_count_args[0] = score_rel;
            Value* score_count = prim_deduce_count(score_count_args[..], null, interp);
            Value*[1] chosen_count_args;
            chosen_count_args[0] = chosen_rel;
            Value* chosen_count = prim_deduce_count(chosen_count_args[..], null, interp);

            ok = analyze_status != null && analyze_status.tag == SYMBOL &&
                 analyze_status.sym_val == interp.symbols.intern("ok") &&
                 analyze_engine != null && analyze_engine.tag == SYMBOL &&
                 analyze_engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 analyze_requested != null && analyze_requested.tag == SYMBOL &&
                 analyze_requested.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 analyze_fallback != null && analyze_fallback.tag == NIL &&
                 score_count != null && score_count.tag == INT && score_count.int_val == (long)expected_score_rows &&
                 chosen_count != null && chosen_count.tag == INT && chosen_count.int_val == (long)expected_chosen_rows &&
                 aggregate_count != null && aggregate_count.tag == INT &&
                 aggregate_count.int_val == 1;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain preserves mixed negated recursive aggregate metadata on semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain preserves mixed negated recursive aggregate metadata on semi-naive-scc");
            (*fail)++;
        }
    }

    {
        Value* db_val = null;
        Value* score_rel = null;
        Value* chosen_rel = null;
        usz expected_score_rows = 0;
        usz expected_chosen_rows = 0;
        bool ok = deduce_query_bench_seed_recursive_mixed_aggregate_negated_db(
            interp,
            4,
            &db_val,
            &score_rel,
            &chosen_rel,
            &expected_score_rows,
            &expected_chosen_rows
        );

        Value* explained = null;
        if (ok) {
            Value*[3] explain_args;
            explain_args[0] = db_val;
            explain_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("bench_mixed_neg_score"));
            explain_args[2] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("naive"));
            explained = prim_deduce_explain(explain_args[..], null, interp);
            ok = explained != null && explained.tag == HASHMAP && explained.hashmap_val != null;
        }

        if (ok) {
            Value* execution_engine = deduce_rule_test_dict_get(interp, explained, "execution-engine");
            Value* requested_engine = deduce_rule_test_dict_get(interp, explained, "requested-execution-engine");
            Value* fallback = deduce_rule_test_dict_get(interp, explained, "recursive-aggregate-naive-fallback");
            Value* routed = deduce_rule_test_dict_get(
                interp,
                explained,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );

            ok = execution_engine != null && execution_engine.tag == SYMBOL &&
                 execution_engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 requested_engine != null && requested_engine.tag == SYMBOL &&
                 requested_engine.sym_val == interp.symbols.intern("naive-scc") &&
                 fallback != null && fallback.tag == NIL &&
                 routed != null && routed.tag == SYMBOL &&
                 routed.sym_val == interp.sym_true;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain routes naive mixed negated recursive aggregate SCCs to semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain routes naive mixed negated recursive aggregate SCCs to semi-naive-scc");
            (*fail)++;
        }
    }
}
