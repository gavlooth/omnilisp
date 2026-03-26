module lisp;

import std::io;

fn void run_deduce_recursive_aggregate_rule_tests(Interp* interp, int* pass, int* fail) {
    if (deduce_query_filter_try_run_targeted_why_result(interp, pass, fail)) return;

    Value* db_val = null;
    Value* out_rel = null;
    usz expected_rows = 0;
    bool ok = deduce_query_bench_seed_recursive_aggregate_db(interp, 4, &db_val, &out_rel, &expected_rows);

    Value* analyzed = null;
    Value* rows = null;
    if (ok) {
        Value*[2] analyze_args;
        analyze_args[0] = db_val;
        analyze_args[1] = make_symbol(interp, interp.symbols.intern("semi-naive"));
        analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
        ok = analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null;
    }

    if (ok) {
        Value*[1] scan_args;
        scan_args[0] = out_rel;
        rows = prim_deduce_scan(scan_args[..], null, interp);

        Value* status = deduce_query_test_dict_get(interp, analyzed, "status");
        Value* engine = deduce_query_test_dict_get(interp, analyzed, "execution-engine");
        Value* row0 = deduce_query_test_find_row_by_value(interp, rows, "node", make_int(interp, 0));
        Value* row1 = deduce_query_test_find_row_by_value(interp, rows, "node", make_int(interp, 1));
        Value* row2 = deduce_query_test_find_row_by_value(interp, rows, "node", make_int(interp, 2));
        Value* row3 = deduce_query_test_find_row_by_value(interp, rows, "node", make_int(interp, 3));
        Value* total0 = deduce_query_test_dict_get(interp, row0, "total");
        Value* total1 = deduce_query_test_dict_get(interp, row1, "total");
        Value* total2 = deduce_query_test_dict_get(interp, row2, "total");
        Value* total3 = deduce_query_test_dict_get(interp, row3, "total");
        Value*[1] count_args;
        count_args[0] = out_rel;
        Value* row_count = prim_deduce_count(count_args[..], null, interp);

        ok = status != null && status.tag == SYMBOL && status.sym_val == interp.symbols.intern("ok") &&
             engine != null && engine.tag == SYMBOL && engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
             row_count != null && row_count.tag == INT && row_count.int_val == (long)expected_rows &&
             total0 != null && total0.tag == INT && total0.int_val == 4 &&
             total1 != null && total1.tag == INT && total1.int_val == 3 &&
             total2 != null && total2.tag == INT && total2.int_val == 2 &&
             total3 != null && total3.tag == INT && total3.int_val == 1;
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce recursive aggregate sum runs on semi-naive SCC and remains exact");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce recursive aggregate sum runs on semi-naive SCC and remains exact");
        (*fail)++;
    }

    {
        Value* mixed_db = null;
        Value* mixed_score = null;
        Value* mixed_chosen = null;
        usz mixed_expected_rows = 0;
        bool mixed_ok = deduce_query_bench_seed_recursive_mixed_aggregate_db(
            interp,
            4,
            &mixed_db,
            &mixed_score,
            &mixed_chosen,
            &mixed_expected_rows
        );

        Value* mixed_analyzed = null;
        Value* mixed_score_rows = null;
        Value* mixed_chosen_rows = null;
        if (mixed_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = mixed_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("semi-naive"));
            mixed_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            mixed_ok = mixed_analyzed != null && mixed_analyzed.tag == HASHMAP && mixed_analyzed.hashmap_val != null;
        }

        if (mixed_ok) {
            Value*[1] scan_score_args;
            scan_score_args[0] = mixed_score;
            mixed_score_rows = prim_deduce_scan(scan_score_args[..], null, interp);

            Value*[1] scan_chosen_args;
            scan_chosen_args[0] = mixed_chosen;
            mixed_chosen_rows = prim_deduce_scan(scan_chosen_args[..], null, interp);

            Value* status = deduce_query_test_dict_get(interp, mixed_analyzed, "status");
            Value* engine = deduce_query_test_dict_get(interp, mixed_analyzed, "execution-engine");
            Value* score_row0 = deduce_query_test_find_row_by_value(interp, mixed_score_rows, "node", make_int(interp, 0));
            Value* score_row1 = deduce_query_test_find_row_by_value(interp, mixed_score_rows, "node", make_int(interp, 1));
            Value* score_row2 = deduce_query_test_find_row_by_value(interp, mixed_score_rows, "node", make_int(interp, 2));
            Value* score_row3 = deduce_query_test_find_row_by_value(interp, mixed_score_rows, "node", make_int(interp, 3));
            Value* chosen_row0 = deduce_query_test_find_row_by_value(interp, mixed_chosen_rows, "node", make_int(interp, 0));
            Value* score_total0 = deduce_query_test_dict_get(interp, score_row0, "total");
            Value* score_total1 = deduce_query_test_dict_get(interp, score_row1, "total");
            Value* score_total2 = deduce_query_test_dict_get(interp, score_row2, "total");
            Value* score_total3 = deduce_query_test_dict_get(interp, score_row3, "total");
            Value* chosen_total0 = deduce_query_test_dict_get(interp, chosen_row0, "total");
            Value*[1] count_score_args;
            count_score_args[0] = mixed_score;
            Value* score_count = prim_deduce_count(count_score_args[..], null, interp);
            Value*[1] count_chosen_args;
            count_chosen_args[0] = mixed_chosen;
            Value* chosen_count = prim_deduce_count(count_chosen_args[..], null, interp);

            mixed_ok = status != null && status.tag == SYMBOL && status.sym_val == interp.symbols.intern("ok") &&
                       engine != null && engine.tag == SYMBOL && engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                       score_count != null && score_count.tag == INT && score_count.int_val == (long)mixed_expected_rows &&
                       chosen_count != null && chosen_count.tag == INT && chosen_count.int_val == (long)mixed_expected_rows &&
                       score_total0 != null && score_total0.tag == INT && score_total0.int_val == 4 &&
                       score_total1 != null && score_total1.tag == INT && score_total1.int_val == 3 &&
                       score_total2 != null && score_total2.tag == INT && score_total2.int_val == 2 &&
                       score_total3 != null && score_total3.tag == INT && score_total3.int_val == 1 &&
                       chosen_total0 != null && chosen_total0.tag == INT && chosen_total0.int_val == 4;
        }

        if (mixed_ok) {
            emit_pass_literal("[PASS] deduce mixed positive recursive aggregate SCCs run on semi-naive-scc and stay exact");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce mixed positive recursive aggregate SCCs run on semi-naive-scc and stay exact");
            (*fail)++;
        }
    }

    {
        Value* pure_db = null;
        Value* pure_out = null;
        usz pure_expected_rows = 0;
        bool pure_ok = deduce_query_bench_seed_recursive_aggregate_db(
            interp,
            4,
            &pure_db,
            &pure_out,
            &pure_expected_rows
        );

        Value* pure_analyzed = null;
        if (pure_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = pure_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("naive"));
            pure_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            pure_ok = pure_analyzed != null && pure_analyzed.tag == HASHMAP && pure_analyzed.hashmap_val != null;
        }

        if (pure_ok) {
            Value* engine = deduce_query_test_dict_get(interp, pure_analyzed, "execution-engine");
            Value* requested = deduce_query_test_dict_get(interp, pure_analyzed, "requested-execution-engine");
            Value* fallback = deduce_query_test_dict_get(interp, pure_analyzed, "recursive-aggregate-naive-fallback");
            Value* routed = deduce_query_test_dict_get(
                interp,
                pure_analyzed,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value*[1] count_args;
            count_args[0] = pure_out;
            Value* row_count = prim_deduce_count(count_args[..], null, interp);

            pure_ok = engine != null && engine.tag == SYMBOL &&
                      engine.sym_val == interp.symbols.intern("naive-scc") &&
                      requested != null && requested.tag == SYMBOL &&
                      requested.sym_val == interp.symbols.intern("naive-scc") &&
                      fallback != null && fallback.tag == NIL &&
                      routed != null && routed.tag == NIL &&
                      row_count != null && row_count.tag == INT &&
                      row_count.int_val == (long)pure_expected_rows;
        }

        if (pure_ok) {
            emit_pass_literal("[PASS] deduce analyze keeps naive pure positive recursive aggregates off mixed-route diagnostics");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce analyze keeps naive pure positive recursive aggregates off mixed-route diagnostics");
            (*fail)++;
        }
    }

    {
        Value* mixed_db = null;
        Value* mixed_score = null;
        Value* mixed_chosen = null;
        usz mixed_expected_rows = 0;
        bool mixed_ok = deduce_query_bench_seed_recursive_mixed_aggregate_db(
            interp,
            4,
            &mixed_db,
            &mixed_score,
            &mixed_chosen,
            &mixed_expected_rows
        );

        Value* mixed_analyzed = null;
        if (mixed_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = mixed_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("naive"));
            mixed_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            mixed_ok = mixed_analyzed != null && mixed_analyzed.tag == HASHMAP && mixed_analyzed.hashmap_val != null;
        }

        if (mixed_ok) {
            Value* engine = deduce_query_test_dict_get(interp, mixed_analyzed, "execution-engine");
            Value* requested = deduce_query_test_dict_get(interp, mixed_analyzed, "requested-execution-engine");
            Value* fallback = deduce_query_test_dict_get(interp, mixed_analyzed, "recursive-aggregate-naive-fallback");
            Value* routed = deduce_query_test_dict_get(
                interp,
                mixed_analyzed,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value*[1] count_score_args;
            count_score_args[0] = mixed_score;
            Value* score_count = prim_deduce_count(count_score_args[..], null, interp);
            Value*[1] count_chosen_args;
            count_chosen_args[0] = mixed_chosen;
            Value* chosen_count = prim_deduce_count(count_chosen_args[..], null, interp);

            mixed_ok = engine != null && engine.tag == SYMBOL &&
                       engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                       requested != null && requested.tag == SYMBOL &&
                       requested.sym_val == interp.symbols.intern("naive-scc") &&
                       fallback != null && fallback.tag == NIL &&
                       routed != null && routed.tag == SYMBOL &&
                       routed.sym_val == interp.sym_true &&
                       score_count != null && score_count.tag == INT &&
                       score_count.int_val == (long)mixed_expected_rows &&
                       chosen_count != null && chosen_count.tag == INT &&
                       chosen_count.int_val == (long)mixed_expected_rows;
        }

        if (mixed_ok) {
            emit_pass_literal("[PASS] deduce analyze routes naive mixed recursive aggregate SCCs to semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce analyze routes naive mixed recursive aggregate SCCs to semi-naive-scc");
            (*fail)++;
        }
    }

    {
        Value* neg_db = null;
        Value* neg_out = null;
        usz neg_expected_rows = 0;
        bool neg_ok = deduce_query_bench_seed_recursive_aggregate_negated_db(
            interp,
            4,
            &neg_db,
            &neg_out,
            &neg_expected_rows
        );

        Value* neg_analyzed = null;
        Value* neg_rows = null;
        if (neg_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = neg_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("semi-naive"));
            neg_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            neg_ok = neg_analyzed != null && neg_analyzed.tag == HASHMAP && neg_analyzed.hashmap_val != null;
        }

        if (neg_ok) {
            Value*[1] scan_args;
            scan_args[0] = neg_out;
            neg_rows = prim_deduce_scan(scan_args[..], null, interp);

            Value* status = deduce_query_test_dict_get(interp, neg_analyzed, "status");
            Value* engine = deduce_query_test_dict_get(interp, neg_analyzed, "execution-engine");
            Value* row0 = deduce_query_test_find_row_by_value(interp, neg_rows, "node", make_int(interp, 0));
            Value* row1 = deduce_query_test_find_row_by_value(interp, neg_rows, "node", make_int(interp, 1));
            Value* row2 = deduce_query_test_find_row_by_value(interp, neg_rows, "node", make_int(interp, 2));
            Value* row3 = deduce_query_test_find_row_by_value(interp, neg_rows, "node", make_int(interp, 3));
            Value* total0 = deduce_query_test_dict_get(interp, row0, "total");
            Value* total1 = deduce_query_test_dict_get(interp, row1, "total");
            Value* total2 = deduce_query_test_dict_get(interp, row2, "total");
            Value*[1] count_args;
            count_args[0] = neg_out;
            Value* row_count = prim_deduce_count(count_args[..], null, interp);

            neg_ok = status != null && status.tag == SYMBOL && status.sym_val == interp.symbols.intern("ok") &&
                     engine != null && engine.tag == SYMBOL && engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                     row_count != null && row_count.tag == INT && row_count.int_val == (long)neg_expected_rows &&
                     total0 != null && total0.tag == INT && total0.int_val == 3 &&
                     total1 != null && total1.tag == INT && total1.int_val == 2 &&
                     total2 != null && total2.tag == INT && total2.int_val == 1 &&
                     row3 == null;
        }

        if (neg_ok) {
            emit_pass_literal("[PASS] deduce recursive aggregate SCCs with lower-stratum negation run on semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce recursive aggregate SCCs with lower-stratum negation run on semi-naive-scc");
            (*fail)++;
        }
    }

    {
        Value* mixed_neg_db = null;
        Value* mixed_neg_score = null;
        Value* mixed_neg_chosen = null;
        usz expected_score_rows = 0;
        usz expected_chosen_rows = 0;
        bool mixed_neg_ok = deduce_query_bench_seed_recursive_mixed_aggregate_negated_db(
            interp,
            4,
            &mixed_neg_db,
            &mixed_neg_score,
            &mixed_neg_chosen,
            &expected_score_rows,
            &expected_chosen_rows
        );

        Value* mixed_neg_analyzed = null;
        Value* mixed_neg_score_rows = null;
        Value* mixed_neg_chosen_rows = null;
        if (mixed_neg_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = mixed_neg_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("semi-naive"));
            mixed_neg_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            mixed_neg_ok = mixed_neg_analyzed != null && mixed_neg_analyzed.tag == HASHMAP && mixed_neg_analyzed.hashmap_val != null;
        }

        if (mixed_neg_ok) {
            Value*[1] scan_score_args;
            scan_score_args[0] = mixed_neg_score;
            mixed_neg_score_rows = prim_deduce_scan(scan_score_args[..], null, interp);

            Value*[1] scan_chosen_args;
            scan_chosen_args[0] = mixed_neg_chosen;
            mixed_neg_chosen_rows = prim_deduce_scan(scan_chosen_args[..], null, interp);

            Value* status = deduce_query_test_dict_get(interp, mixed_neg_analyzed, "status");
            Value* engine = deduce_query_test_dict_get(interp, mixed_neg_analyzed, "execution-engine");
            Value* score_row0 = deduce_query_test_find_row_by_value(interp, mixed_neg_score_rows, "node", make_int(interp, 0));
            Value* score_row1 = deduce_query_test_find_row_by_value(interp, mixed_neg_score_rows, "node", make_int(interp, 1));
            Value* score_row2 = deduce_query_test_find_row_by_value(interp, mixed_neg_score_rows, "node", make_int(interp, 2));
            Value* score_row3 = deduce_query_test_find_row_by_value(interp, mixed_neg_score_rows, "node", make_int(interp, 3));
            Value* chosen_row0 = deduce_query_test_find_row_by_value(interp, mixed_neg_chosen_rows, "node", make_int(interp, 0));
            Value* chosen_row1 = deduce_query_test_find_row_by_value(interp, mixed_neg_chosen_rows, "node", make_int(interp, 1));
            Value* chosen_row2 = deduce_query_test_find_row_by_value(interp, mixed_neg_chosen_rows, "node", make_int(interp, 2));
            Value* chosen_row3 = deduce_query_test_find_row_by_value(interp, mixed_neg_chosen_rows, "node", make_int(interp, 3));
            Value* score_total0 = deduce_query_test_dict_get(interp, score_row0, "total");
            Value* score_total1 = deduce_query_test_dict_get(interp, score_row1, "total");
            Value* score_total2 = deduce_query_test_dict_get(interp, score_row2, "total");
            Value* score_total3 = deduce_query_test_dict_get(interp, score_row3, "total");
            Value* chosen_total0 = deduce_query_test_dict_get(interp, chosen_row0, "total");
            Value* chosen_total1 = deduce_query_test_dict_get(interp, chosen_row1, "total");
            Value* chosen_total2 = deduce_query_test_dict_get(interp, chosen_row2, "total");
            Value*[1] count_score_args;
            count_score_args[0] = mixed_neg_score;
            Value* score_count = prim_deduce_count(count_score_args[..], null, interp);
            Value*[1] count_chosen_args;
            count_chosen_args[0] = mixed_neg_chosen;
            Value* chosen_count = prim_deduce_count(count_chosen_args[..], null, interp);

            mixed_neg_ok =
                status != null && status.tag == SYMBOL && status.sym_val == interp.symbols.intern("ok") &&
                engine != null && engine.tag == SYMBOL && engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                score_count != null && score_count.tag == INT && score_count.int_val == (long)expected_score_rows &&
                chosen_count != null && chosen_count.tag == INT && chosen_count.int_val == (long)expected_chosen_rows &&
                score_total0 != null && score_total0.tag == INT && score_total0.int_val == 3 &&
                score_total1 != null && score_total1.tag == INT && score_total1.int_val == 2 &&
                score_total2 != null && score_total2.tag == INT && score_total2.int_val == 1 &&
                score_total3 != null && score_total3.tag == INT && score_total3.int_val == 1 &&
                chosen_total0 != null && chosen_total0.tag == INT && chosen_total0.int_val == 3 &&
                chosen_total1 != null && chosen_total1.tag == INT && chosen_total1.int_val == 2 &&
                chosen_total2 != null && chosen_total2.tag == INT && chosen_total2.int_val == 1 &&
                chosen_row3 == null;
        }

        if (mixed_neg_ok) {
            emit_pass_literal("[PASS] deduce mixed recursive aggregate SCCs with lower-stratum negation run on semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce mixed recursive aggregate SCCs with lower-stratum negation run on semi-naive-scc");
            (*fail)++;
        }
    }

    {
        Value* neg_db = null;
        Value* neg_out = null;
        usz neg_expected_rows = 0;
        bool neg_ok = deduce_query_bench_seed_recursive_aggregate_negated_db(
            interp,
            4,
            &neg_db,
            &neg_out,
            &neg_expected_rows
        );

        Value* neg_analyzed = null;
        if (neg_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = neg_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("naive"));
            neg_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            neg_ok = neg_analyzed != null && neg_analyzed.tag == HASHMAP && neg_analyzed.hashmap_val != null;
        }

        if (neg_ok) {
            Value* engine = deduce_query_test_dict_get(interp, neg_analyzed, "execution-engine");
            Value* requested = deduce_query_test_dict_get(interp, neg_analyzed, "requested-execution-engine");
            Value* fallback = deduce_query_test_dict_get(interp, neg_analyzed, "recursive-aggregate-naive-fallback");
            Value* routed = deduce_query_test_dict_get(
                interp,
                neg_analyzed,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value*[1] count_args;
            count_args[0] = neg_out;
            Value* row_count = prim_deduce_count(count_args[..], null, interp);

            neg_ok = engine != null && engine.tag == SYMBOL &&
                     engine.sym_val == interp.symbols.intern("naive-scc") &&
                     requested != null && requested.tag == SYMBOL &&
                     requested.sym_val == interp.symbols.intern("naive-scc") &&
                     fallback != null && fallback.tag == NIL &&
                     routed != null && routed.tag == NIL &&
                     row_count != null && row_count.tag == INT &&
                     row_count.int_val == (long)neg_expected_rows;
        }

        if (neg_ok) {
            emit_pass_literal("[PASS] deduce analyze keeps naive pure negated recursive aggregates off mixed-route diagnostics");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce analyze keeps naive pure negated recursive aggregates off mixed-route diagnostics");
            (*fail)++;
        }
    }

    {
        Value* mixed_neg_db = null;
        Value* mixed_neg_score = null;
        Value* mixed_neg_chosen = null;
        usz expected_score_rows = 0;
        usz expected_chosen_rows = 0;
        bool mixed_neg_ok = deduce_query_bench_seed_recursive_mixed_aggregate_negated_db(
            interp,
            4,
            &mixed_neg_db,
            &mixed_neg_score,
            &mixed_neg_chosen,
            &expected_score_rows,
            &expected_chosen_rows
        );

        Value* mixed_neg_analyzed = null;
        if (mixed_neg_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = mixed_neg_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("naive"));
            mixed_neg_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            mixed_neg_ok = mixed_neg_analyzed != null && mixed_neg_analyzed.tag == HASHMAP && mixed_neg_analyzed.hashmap_val != null;
        }

        if (mixed_neg_ok) {
            Value* engine = deduce_query_test_dict_get(interp, mixed_neg_analyzed, "execution-engine");
            Value* requested = deduce_query_test_dict_get(interp, mixed_neg_analyzed, "requested-execution-engine");
            Value* fallback = deduce_query_test_dict_get(interp, mixed_neg_analyzed, "recursive-aggregate-naive-fallback");
            Value* routed = deduce_query_test_dict_get(
                interp,
                mixed_neg_analyzed,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value*[1] count_score_args;
            count_score_args[0] = mixed_neg_score;
            Value* score_count = prim_deduce_count(count_score_args[..], null, interp);
            Value*[1] count_chosen_args;
            count_chosen_args[0] = mixed_neg_chosen;
            Value* chosen_count = prim_deduce_count(count_chosen_args[..], null, interp);

            mixed_neg_ok = engine != null && engine.tag == SYMBOL &&
                           engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                           requested != null && requested.tag == SYMBOL &&
                           requested.sym_val == interp.symbols.intern("naive-scc") &&
                           fallback != null && fallback.tag == NIL &&
                           routed != null && routed.tag == SYMBOL &&
                           routed.sym_val == interp.sym_true &&
                           score_count != null && score_count.tag == INT &&
                           score_count.int_val == (long)expected_score_rows &&
                           chosen_count != null && chosen_count.tag == INT &&
                           chosen_count.int_val == (long)expected_chosen_rows;
        }

        if (mixed_neg_ok) {
            emit_pass_literal("[PASS] deduce analyze routes naive mixed negated recursive aggregate SCCs to semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce analyze routes naive mixed negated recursive aggregate SCCs to semi-naive-scc");
            (*fail)++;
        }
    }
}

