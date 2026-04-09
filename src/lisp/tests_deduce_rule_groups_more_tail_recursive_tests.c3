module lisp;

import std::io;
import std::core::mem;

fn void run_deduce_rule_validation_tests_more_tail_recursive_tests(Interp* interp, int* pass, int* fail) {
    {
        Value* db_naive = null;
        Value* out_naive = null;
        usz expected_naive = 0;
        Value* db_semi = null;
        Value* out_semi = null;
        usz expected_semi = 0;
        bool ok = deduce_query_bench_seed_recursive_aggregate_db(interp, 4, &db_naive, &out_naive, &expected_naive);
        if (ok) {
            ok = deduce_query_bench_seed_recursive_aggregate_db(interp, 4, &db_semi, &out_semi, &expected_semi) &&
                 expected_naive == expected_semi;
        }

        Value* analyzed_naive = null;
        Value* analyzed_semi = null;
        if (ok) {
            Value*[2] analyze_naive_args;
            analyze_naive_args[0] = db_naive;
            analyze_naive_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("naive"));
            analyzed_naive = prim_deduce_analyze(analyze_naive_args[..], null, interp);

            Value*[2] analyze_semi_args;
            analyze_semi_args[0] = db_semi;
            analyze_semi_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("semi-naive"));
            analyzed_semi = prim_deduce_analyze(analyze_semi_args[..], null, interp);

            ok = analyzed_naive != null && analyzed_naive.tag == HASHMAP && analyzed_naive.hashmap_val != null &&
                 analyzed_semi != null && analyzed_semi.tag == HASHMAP && analyzed_semi.hashmap_val != null;
        }

        if (ok) {
            Value* engine_naive = deduce_rule_test_dict_get(interp, analyzed_naive, "execution-engine");
            Value* requested_naive = deduce_rule_test_dict_get(interp, analyzed_naive, "requested-execution-engine");
            Value* fallback_naive = deduce_rule_test_dict_get(interp, analyzed_naive, "recursive-aggregate-naive-fallback");
            Value* routed_naive = deduce_rule_test_dict_get(
                interp,
                analyzed_naive,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value* engine_semi = deduce_rule_test_dict_get(interp, analyzed_semi, "execution-engine");
            Value* requested_semi = deduce_rule_test_dict_get(interp, analyzed_semi, "requested-execution-engine");
            Value* fallback_semi = deduce_rule_test_dict_get(interp, analyzed_semi, "recursive-aggregate-naive-fallback");
            Value* routed_semi = deduce_rule_test_dict_get(
                interp,
                analyzed_semi,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value*[1] count_naive_args;
            count_naive_args[0] = out_naive;
            Value* count_naive = prim_deduce_count(count_naive_args[..], null, interp);
            Value*[1] count_semi_args;
            count_semi_args[0] = out_semi;
            Value* count_semi = prim_deduce_count(count_semi_args[..], null, interp);

            ok = engine_naive != null && engine_naive.tag == SYMBOL &&
                 engine_naive.sym_val == interp.symbols.intern("naive-scc") &&
                 requested_naive != null && requested_naive.tag == SYMBOL &&
                 requested_naive.sym_val == interp.symbols.intern("naive-scc") &&
                 fallback_naive != null && fallback_naive.tag == NIL &&
                 routed_naive != null && routed_naive.tag == NIL &&
                 engine_semi != null && engine_semi.tag == SYMBOL &&
                 engine_semi.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 requested_semi != null && requested_semi.tag == SYMBOL &&
                 requested_semi.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 fallback_semi != null && fallback_semi.tag == NIL &&
                 routed_semi != null && routed_semi.tag == NIL &&
                 count_naive != null && count_naive.tag == INT && count_naive.int_val == 4 &&
                 count_semi != null && count_semi.tag == INT && count_semi.int_val == 4;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce recursive aggregate-only SCCs keep pure engine selectors off mixed-route diagnostics");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce recursive aggregate-only SCCs keep pure engine selectors off mixed-route diagnostics");
            (*fail)++;
        }
    }

    {
        DeduceTupleSupportTable table = {};
        defer deduce_tuple_support_table_release(&table);

        char[] recursive_key = "support-recursive-only";
        char[] base_key = "support-base-seeded";

        bool ok = true;
        bool became_present = false;
        bool became_absent = false;
        long total = -1;

        Value* err = deduce_tuple_support_table_adjust_recursive_count(
            &table,
            recursive_key.ptr,
            recursive_key.len,
            1,
            &total,
            &became_present,
            &became_absent,
            interp
        );
        ok = err == null &&
             total == 1 &&
             became_present &&
             !became_absent &&
             deduce_tuple_support_table_total_count(&table, recursive_key.ptr, recursive_key.len, interp) == 1;

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                recursive_key.ptr,
                recursive_key.len,
                1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 2 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_total_count(&table, recursive_key.ptr, recursive_key.len, interp) == 2;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                recursive_key.ptr,
                recursive_key.len,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 1 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_total_count(&table, recursive_key.ptr, recursive_key.len, interp) == 1;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                recursive_key.ptr,
                recursive_key.len,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 0 &&
                 !became_present &&
                 became_absent &&
                 deduce_tuple_support_table_total_count(&table, recursive_key.ptr, recursive_key.len, interp) == 0;
        }

        if (ok) {
            err = deduce_tuple_support_table_seed_base_tuple(&table, base_key.ptr, base_key.len, &total, interp);
            ok = err == null &&
                 total == 1 &&
                 deduce_tuple_support_table_lookup_count(table.base_counts, base_key.ptr, base_key.len, interp) == 1 &&
                 deduce_tuple_support_table_lookup_count(table.recursive_counts, base_key.ptr, base_key.len, interp) == 0;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                base_key.ptr,
                base_key.len,
                1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 2 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_lookup_count(table.base_counts, base_key.ptr, base_key.len, interp) == 1 &&
                 deduce_tuple_support_table_lookup_count(table.recursive_counts, base_key.ptr, base_key.len, interp) == 1;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                base_key.ptr,
                base_key.len,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 1 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_lookup_count(table.base_counts, base_key.ptr, base_key.len, interp) == 1 &&
                 deduce_tuple_support_table_lookup_count(table.recursive_counts, base_key.ptr, base_key.len, interp) == 0;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce tuple support tables preserve zero-crossing transitions");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce tuple support tables preserve zero-crossing transitions");
            (*fail)++;
        }
    }

    {
        DeduceSemiNaiveRuleExec exec = {
            .aggregate_component_mode = true,
            .anchor_delta_is_removal = false,
            .interp = interp,
        };

        bool[] var_bound = mem::new_array(bool, 2);
        Value*[] var_values = mem::new_array(Value*, 2);
        bool ok = var_bound.ptr != null && var_values.ptr != null;
        defer if (var_bound.ptr != null) mem::free(var_bound.ptr);
        defer if (var_values.ptr != null) mem::free(var_values.ptr);

        if (ok) {
            for (usz i = 0; i < var_bound.len; i++) {
                var_bound[i] = false;
                var_values[i] = null;
            }
            var_bound[0] = true;
            var_bound[1] = true;
            var_values[0] = make_int(interp, 7);
            var_values[1] = make_string(interp, "alpha");

            bool already_seen = false;
            Value* err = deduce_seminaive_mark_current_proof_seen(&exec, var_bound, var_values, &already_seen);
            ok = err == null && !already_seen;

            if (ok) {
                err = deduce_seminaive_mark_current_proof_seen(&exec, var_bound, var_values, &already_seen);
                ok = err == null && already_seen;
            }

            if (ok) {
                exec.anchor_delta_is_removal = true;
                err = deduce_seminaive_mark_current_proof_seen(&exec, var_bound, var_values, &already_seen);
                ok = err == null && !already_seen;
            }

            if (ok) {
                exec.anchor_delta_is_removal = false;
                var_values[1] = make_string(interp, "beta");
                err = deduce_seminaive_mark_current_proof_seen(&exec, var_bound, var_values, &already_seen);
                ok = err == null && !already_seen;
            }
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce seminaive proof dedupe keys normalized bindings and polarity");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce seminaive proof dedupe keys normalized bindings and polarity");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-naive (deduce/open 'memory)) "
            "  (define [relation db-naive] tail-mixed-naive-edge (src) (dst)) "
            "  (define [relation db-naive] tail-mixed-naive-unit (node) (weight)) "
            "  (define [relation db-naive] tail-mixed-naive-score (node) (total)) "
            "  (define [relation db-naive] tail-mixed-naive-chosen (node) (total)) "
            "  (deduce/rule! db-naive '(tail-mixed-naive-chosen ?x ?n) '(tail-mixed-naive-score ?x ?n)) "
            "  (deduce/rule! db-naive '(tail-mixed-naive-score ?x (sum ?n)) '(tail-mixed-naive-unit ?x ?n)) "
            "  (deduce/rule! db-naive '(tail-mixed-naive-score ?x (sum ?n)) '(tail-mixed-naive-edge ?x ?y) '(tail-mixed-naive-chosen ?y ?n)) "
            "  (deduce/fact! tail-mixed-naive-unit 0 1) "
            "  (deduce/fact! tail-mixed-naive-edge 0 1) "
            "  (deduce/fact! tail-mixed-naive-unit 1 1) "
            "  (deduce/fact! tail-mixed-naive-edge 1 2) "
            "  (deduce/fact! tail-mixed-naive-unit 2 1) "
            "  (deduce/fact! tail-mixed-naive-edge 2 3) "
            "  (deduce/fact! tail-mixed-naive-unit 3 1) "
            "  (define db-semi (deduce/open 'memory)) "
            "  (define [relation db-semi] tail-mixed-semi-edge (src) (dst)) "
            "  (define [relation db-semi] tail-mixed-semi-unit (node) (weight)) "
            "  (define [relation db-semi] tail-mixed-semi-score (node) (total)) "
            "  (define [relation db-semi] tail-mixed-semi-chosen (node) (total)) "
            "  (deduce/rule! db-semi '(tail-mixed-semi-chosen ?x ?n) '(tail-mixed-semi-score ?x ?n)) "
            "  (deduce/rule! db-semi '(tail-mixed-semi-score ?x (sum ?n)) '(tail-mixed-semi-unit ?x ?n)) "
            "  (deduce/rule! db-semi '(tail-mixed-semi-score ?x (sum ?n)) '(tail-mixed-semi-edge ?x ?y) '(tail-mixed-semi-chosen ?y ?n)) "
            "  (deduce/fact! tail-mixed-semi-unit 0 1) "
            "  (deduce/fact! tail-mixed-semi-edge 0 1) "
            "  (deduce/fact! tail-mixed-semi-unit 1 1) "
            "  (deduce/fact! tail-mixed-semi-edge 1 2) "
            "  (deduce/fact! tail-mixed-semi-unit 2 1) "
            "  (deduce/fact! tail-mixed-semi-edge 2 3) "
            "  (deduce/fact! tail-mixed-semi-unit 3 1) "
            "  (define a-naive (deduce/analyze db-naive 'naive)) "
            "  (define a-semi (deduce/analyze db-semi 'semi-naive)) "
            "  (define ok1 (= a-naive.execution-engine 'semi-naive-scc)) "
            "  (define ok2 (= a-naive.requested-execution-engine 'naive-scc)) "
            "  (define ok3 (= a-naive.recursive-aggregate-naive-fallback nil)) "
            "  (define ok4 (= a-naive.naive-recursive-aggregate-mixed-routed-to-seminaive true)) "
            "  (define ok5 (= a-semi.execution-engine 'semi-naive-scc)) "
            "  (define ok6 (= a-semi.requested-execution-engine 'semi-naive-scc)) "
            "  (define ok7 (= a-semi.recursive-aggregate-naive-fallback nil)) "
            "  (define ok8 (= a-semi.naive-recursive-aggregate-mixed-routed-to-seminaive nil)) "
            "  (define ok9 (= (deduce/count tail-mixed-semi-score) 4)) "
            "  (define ok10 (= (deduce/count tail-mixed-semi-chosen) 4)) "
            "  (and ok1 (and ok2 (and ok3 (and ok4 (and ok5 (and ok6 (and ok7 (and ok8 (and ok9 ok10))))))))))"
            ,
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce mixed positive recursive aggregate SCCs run on semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce mixed positive recursive aggregate SCCs run on semi-naive-scc");
            (*fail)++;
        }
    }

    {
        Value* db_naive = null;
        Value* out_naive = null;
        usz expected_naive = 0;
        Value* db_semi = null;
        Value* out_semi = null;
        usz expected_semi = 0;
        bool ok = deduce_query_bench_seed_recursive_aggregate_negated_db(interp, 4, &db_naive, &out_naive, &expected_naive);
        if (ok) {
            ok = deduce_query_bench_seed_recursive_aggregate_negated_db(interp, 4, &db_semi, &out_semi, &expected_semi) &&
                 expected_naive == expected_semi;
        }

        Value* analyzed_naive = null;
        Value* analyzed_semi = null;
        if (ok) {
            Value*[2] analyze_naive_args;
            analyze_naive_args[0] = db_naive;
            analyze_naive_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("naive"));
            analyzed_naive = prim_deduce_analyze(analyze_naive_args[..], null, interp);

            Value*[2] analyze_semi_args;
            analyze_semi_args[0] = db_semi;
            analyze_semi_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("semi-naive"));
            analyzed_semi = prim_deduce_analyze(analyze_semi_args[..], null, interp);

            ok = analyzed_naive != null && analyzed_naive.tag == HASHMAP && analyzed_naive.hashmap_val != null &&
                 analyzed_semi != null && analyzed_semi.tag == HASHMAP && analyzed_semi.hashmap_val != null;
        }

        if (ok) {
            Value* engine_naive = deduce_rule_test_dict_get(interp, analyzed_naive, "execution-engine");
            Value* requested_naive = deduce_rule_test_dict_get(interp, analyzed_naive, "requested-execution-engine");
            Value* fallback_naive = deduce_rule_test_dict_get(interp, analyzed_naive, "recursive-aggregate-naive-fallback");
            Value* routed_naive = deduce_rule_test_dict_get(
                interp,
                analyzed_naive,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value* engine_semi = deduce_rule_test_dict_get(interp, analyzed_semi, "execution-engine");
            Value* requested_semi = deduce_rule_test_dict_get(interp, analyzed_semi, "requested-execution-engine");
            Value* fallback_semi = deduce_rule_test_dict_get(interp, analyzed_semi, "recursive-aggregate-naive-fallback");
            Value* routed_semi = deduce_rule_test_dict_get(
                interp,
                analyzed_semi,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value*[1] count_naive_args;
            count_naive_args[0] = out_naive;
            Value* count_naive = prim_deduce_count(count_naive_args[..], null, interp);
            Value*[1] count_semi_args;
            count_semi_args[0] = out_semi;
            Value* count_semi = prim_deduce_count(count_semi_args[..], null, interp);

            ok = engine_naive != null && engine_naive.tag == SYMBOL &&
                 engine_naive.sym_val == interp.symbols.intern("naive-scc") &&
                 requested_naive != null && requested_naive.tag == SYMBOL &&
                 requested_naive.sym_val == interp.symbols.intern("naive-scc") &&
                 fallback_naive != null && fallback_naive.tag == NIL &&
                 routed_naive != null && routed_naive.tag == NIL &&
                 engine_semi != null && engine_semi.tag == SYMBOL &&
                 engine_semi.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 requested_semi != null && requested_semi.tag == SYMBOL &&
                 requested_semi.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 fallback_semi != null && fallback_semi.tag == NIL &&
                 routed_semi != null && routed_semi.tag == NIL &&
                 count_naive != null && count_naive.tag == INT && count_naive.int_val == (long)expected_naive &&
                 count_semi != null && count_semi.tag == INT && count_semi.int_val == (long)expected_semi;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce negated recursive aggregate SCCs keep pure engine selectors off mixed-route diagnostics");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce negated recursive aggregate SCCs keep pure engine selectors off mixed-route diagnostics");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-naive (deduce/open 'memory)) "
            "  (define [relation db-naive] tail-mixed-neg-naive-edge (src) (dst)) "
            "  (define [relation db-naive] tail-mixed-neg-naive-unit (node) (weight)) "
            "  (define [relation db-naive] tail-mixed-neg-naive-blocked (node)) "
            "  (define [relation db-naive] tail-mixed-neg-naive-score (node) (total)) "
            "  (define [relation db-naive] tail-mixed-neg-naive-chosen (node) (total)) "
            "  (deduce/rule! db-naive '(tail-mixed-neg-naive-chosen ?x ?n) '(tail-mixed-neg-naive-score ?x ?n) '(not (tail-mixed-neg-naive-blocked ?x))) "
            "  (deduce/rule! db-naive '(tail-mixed-neg-naive-score ?x (sum ?n)) '(tail-mixed-neg-naive-unit ?x ?n)) "
            "  (deduce/rule! db-naive '(tail-mixed-neg-naive-score ?x (sum ?n)) '(tail-mixed-neg-naive-edge ?x ?y) '(tail-mixed-neg-naive-chosen ?y ?n)) "
            "  (deduce/fact! tail-mixed-neg-naive-unit 0 1) "
            "  (deduce/fact! tail-mixed-neg-naive-edge 0 1) "
            "  (deduce/fact! tail-mixed-neg-naive-unit 1 1) "
            "  (deduce/fact! tail-mixed-neg-naive-edge 1 2) "
            "  (deduce/fact! tail-mixed-neg-naive-unit 2 1) "
            "  (deduce/fact! tail-mixed-neg-naive-edge 2 3) "
            "  (deduce/fact! tail-mixed-neg-naive-unit 3 1) "
            "  (deduce/fact! tail-mixed-neg-naive-blocked 3) "
            "  (define db-semi (deduce/open 'memory)) "
            "  (define [relation db-semi] tail-mixed-neg-semi-edge (src) (dst)) "
            "  (define [relation db-semi] tail-mixed-neg-semi-unit (node) (weight)) "
            "  (define [relation db-semi] tail-mixed-neg-semi-blocked (node)) "
            "  (define [relation db-semi] tail-mixed-neg-semi-score (node) (total)) "
            "  (define [relation db-semi] tail-mixed-neg-semi-chosen (node) (total)) "
            "  (deduce/rule! db-semi '(tail-mixed-neg-semi-chosen ?x ?n) '(tail-mixed-neg-semi-score ?x ?n) '(not (tail-mixed-neg-semi-blocked ?x))) "
            "  (deduce/rule! db-semi '(tail-mixed-neg-semi-score ?x (sum ?n)) '(tail-mixed-neg-semi-unit ?x ?n)) "
            "  (deduce/rule! db-semi '(tail-mixed-neg-semi-score ?x (sum ?n)) '(tail-mixed-neg-semi-edge ?x ?y) '(tail-mixed-neg-semi-chosen ?y ?n)) "
            "  (deduce/fact! tail-mixed-neg-semi-unit 0 1) "
            "  (deduce/fact! tail-mixed-neg-semi-edge 0 1) "
            "  (deduce/fact! tail-mixed-neg-semi-unit 1 1) "
            "  (deduce/fact! tail-mixed-neg-semi-edge 1 2) "
            "  (deduce/fact! tail-mixed-neg-semi-unit 2 1) "
            "  (deduce/fact! tail-mixed-neg-semi-edge 2 3) "
            "  (deduce/fact! tail-mixed-neg-semi-unit 3 1) "
            "  (deduce/fact! tail-mixed-neg-semi-blocked 3) "
            "  (define a-naive (deduce/analyze db-naive 'naive)) "
            "  (define a-semi (deduce/analyze db-semi 'semi-naive)) "
            "  (define ok1 (= a-naive.execution-engine 'semi-naive-scc)) "
            "  (define ok2 (= a-naive.requested-execution-engine 'naive-scc)) "
            "  (define ok3 (= a-naive.recursive-aggregate-naive-fallback nil)) "
            "  (define ok4 (= a-naive.naive-recursive-aggregate-mixed-routed-to-seminaive true)) "
            "  (define ok5 (= a-semi.execution-engine 'semi-naive-scc)) "
            "  (define ok6 (= a-semi.requested-execution-engine 'semi-naive-scc)) "
            "  (define ok7 (= a-semi.recursive-aggregate-naive-fallback nil)) "
            "  (define ok8 (= a-semi.naive-recursive-aggregate-mixed-routed-to-seminaive nil)) "
            "  (define ok9 (= (deduce/count tail-mixed-neg-semi-score) 4)) "
            "  (define ok10 (= (deduce/count tail-mixed-neg-semi-chosen) 3)) "
            "  (and ok1 (and ok2 (and ok3 (and ok4 (and ok5 (and ok6 (and ok7 (and ok8 (and ok9 ok10))))))))))"
            ,
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce mixed negated recursive aggregate SCCs run on semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce mixed negated recursive aggregate SCCs run on semi-naive-scc");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-strata (deduce/open 'memory)) "
            "  (define [relation db-strata] base-neg (x)) "
            "  (define [relation db-strata] deny-neg (x)) "
            "  (define [relation db-strata] allow-neg (x)) "
            "  (deduce/fact! base-neg \"A\") "
            "  (deduce/rule! db-strata '(deny-neg ?x) '(base-neg ?x)) "
            "  (deduce/rule! db-strata '(allow-neg ?x) '(base-neg ?x) '(not (deny-neg ?x))) "
            "  (define a (deduce/analyze db-strata)) "
            "  (define ok1 (= (deduce/count deny-neg) 1)) "
            "  (define ok2 (= (deduce/count allow-neg) 0)) "
            "  (define ok3 (>= a.stratum-count 2)) "
            "  (and ok1 (and ok2 ok3)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze enforces negation stratum order");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze enforces negation stratum order (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze enforces negation stratum order");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-mixed (deduce/open 'memory)) "
            "  (define [relation db-mixed] edge-m (src) (dst)) "
            "  (define [relation db-mixed] reach-m (src) (dst)) "
            "  (define [relation db-mixed] banned-m (node)) "
            "  (define [relation db-mixed] reach-ok-m (src) (dst)) "
            "  (deduce/fact! edge-m \"A\" \"B\") "
            "  (deduce/fact! edge-m \"B\" \"C\") "
            "  (deduce/fact! banned-m \"C\") "
            "  (deduce/rule! db-mixed '(reach-m ?x ?y) '(edge-m ?x ?y)) "
            "  (deduce/rule! db-mixed '(reach-m ?x ?z) '(edge-m ?x ?y) '(reach-m ?y ?z)) "
            "  (deduce/rule! db-mixed '(reach-ok-m ?x ?y) '(reach-m ?x ?y) '(not (banned-m ?y))) "
            "  (define a (deduce/analyze db-mixed)) "
            "  (define ok1 (= (deduce/count reach-m) 3)) "
            "  (define ok2 (= (deduce/count reach-ok-m) 1)) "
            "  (define ok3 (>= a.stratum-count 2)) "
            "  (and ok1 (and ok2 ok3)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze handles mixed recursive and negated stratum boundaries");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze handles mixed recursive and negated stratum boundaries (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze handles mixed recursive and negated stratum boundaries");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-inc (deduce/open 'memory)) "
            "  (define [relation db-inc] base-inc (x)) "
            "  (define [relation db-inc] derived-inc (x)) "
            "  (deduce/rule! db-inc '(derived-inc ?x) '(base-inc ?x)) "
            "  (define a0 (deduce/analyze db-inc)) "
            "  (deduce/fact! base-inc \"A\") "
            "  (define a1 (deduce/analyze db-inc)) "
            "  (define tx1 (deduce 'block db-inc 'write)) "
            "  (deduce/fact! tx1 base-inc \"B\") "
            "  (deduce 'commit tx1) "
            "  (define a2 (deduce/analyze db-inc)) "
            "  (define tx2 (deduce 'block db-inc 'write)) "
            "  (deduce/fact! tx2 base-inc \"C\") "
            "  (deduce 'abort tx2) "
            "  (define a3 (deduce/analyze db-inc)) "
            "  (define ok1 (= a0.incremental-dependency-edges 1)) "
            "  (define ok2 (= a0.incremental-dirty-predicate-count 0)) "
            "  (define ok3 (= a0.incremental-invalidation-mode 'tracked)) "
            "  (define ok4 (>= a1.incremental-dirty-predicate-count 2)) "
            "  (define ok5 (= a1.incremental-invalidation-mode 'tracked)) "
            "  (define ok6 (= a2.incremental-dirty-predicate-count 2)) "
            "  (define ok7 (= a2.incremental-invalidation-mode 'tracked)) "
            "  (define ok8 (= (deduce/count base-inc) 2)) "
            "  (define ok8a (= (deduce/count derived-inc) 2)) "
            "  (define ok9 (= a3.incremental-dirty-predicate-count 0)) "
            "  (define ok10 (= a3.incremental-invalidation-mode 'tracked)) "
            "  (define ok11 (= (deduce/count derived-inc) 2)) "
            "  (and ok1 ok2 ok3 ok4 ok5 ok6 ok7 ok8 ok8a ok9 ok10 ok11))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze reports transaction-safe incremental commit and abort boundaries");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze reports transaction-safe incremental commit and abort boundaries (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze reports transaction-safe incremental commit and abort boundaries");
            }
            (*fail)++;
        }
    }
}
