module lisp;

import std::io;

fn void run_deduce_aggregate_rule_tests(Interp* interp, int* pass, int* fail) {
    if (deduce_query_filter_try_run_targeted_why_result(interp, pass, fail)) return;

    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = make_symbol(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;

        SymbolId rel_src = interp.symbols.intern("agg_src");
        SymbolId rel_count = interp.symbols.intern("agg_count");
        SymbolId rel_sum = interp.symbols.intern("agg_sum");
        SymbolId rel_min = interp.symbols.intern("agg_min");
        SymbolId rel_max = interp.symbols.intern("agg_max");
        SymbolId col_dept = interp.symbols.intern("dept");
        SymbolId col_amount = interp.symbols.intern("amount");
        SymbolId col_n = interp.symbols.intern("n");
        SymbolId col_total = interp.symbols.intern("total");
        SymbolId col_low = interp.symbols.intern("low");
        SymbolId col_high = interp.symbols.intern("high");
        SymbolId var_dept = interp.symbols.intern("?dept");
        SymbolId var_amount = interp.symbols.intern("?amount");
        SymbolId sym_count = interp.symbols.intern("count");
        SymbolId sym_sum = interp.symbols.intern("sum");
        SymbolId sym_min = interp.symbols.intern("min");
        SymbolId sym_max = interp.symbols.intern("max");

        Value* src = null;
        Value* out_count = null;
        Value* out_sum = null;
        Value* out_min = null;
        Value* out_max = null;
        if (ok) {
            src = setup_deduce_relation_for_db(interp, db_val, rel_src, col_dept, col_amount);
            out_count = setup_deduce_relation_for_db(interp, db_val, rel_count, col_dept, col_n);
            out_sum = setup_deduce_relation_for_db(interp, db_val, rel_sum, col_dept, col_total);
            out_min = setup_deduce_relation_for_db(interp, db_val, rel_min, col_dept, col_low);
            out_max = setup_deduce_relation_for_db(interp, db_val, rel_max, col_dept, col_high);
            ok = src != null && src.tag == FFI_HANDLE &&
                 out_count != null && out_count.tag == FFI_HANDLE &&
                 out_sum != null && out_sum.tag == FFI_HANDLE &&
                 out_min != null && out_min.tag == FFI_HANDLE &&
                 out_max != null && out_max.tag == FFI_HANDLE;
        }

        if (ok) {
            char[][] depts = { "eng", "eng", "sales", "sales" };
            long[] amounts = { 10, 15, 3, 9 };
            for (usz i = 0; i < depts.len; i++) {
                Value*[3] fact_args;
                fact_args[0] = src;
                fact_args[1] = make_string(interp, depts[i]);
                fact_args[2] = make_int(interp, amounts[i]);
                Value* inserted = prim_assert_fact(fact_args[..], null, interp);
                if (inserted == null || inserted.tag == ERROR) {
                    ok = false;
                    break;
                }
            }
        }

        if (ok) {
            Value* count_head =
                make_cons(interp,
                    deduce_rule_test_make_symbol_value(interp, rel_count),
                    make_cons(interp,
                        deduce_rule_test_make_symbol_value(interp, var_dept),
                        make_cons(interp,
                            make_cons(interp,
                                deduce_rule_test_make_symbol_value(interp, sym_count),
                                make_cons(interp,
                                    deduce_rule_test_make_symbol_value(interp, var_amount),
                                    make_nil(interp))),
                            make_nil(interp))));
            Value* sum_head =
                make_cons(interp,
                    deduce_rule_test_make_symbol_value(interp, rel_sum),
                    make_cons(interp,
                        deduce_rule_test_make_symbol_value(interp, var_dept),
                        make_cons(interp,
                            make_cons(interp,
                                deduce_rule_test_make_symbol_value(interp, sym_sum),
                                make_cons(interp,
                                    deduce_rule_test_make_symbol_value(interp, var_amount),
                                    make_nil(interp))),
                            make_nil(interp))));
            Value* min_head =
                make_cons(interp,
                    deduce_rule_test_make_symbol_value(interp, rel_min),
                    make_cons(interp,
                        deduce_rule_test_make_symbol_value(interp, var_dept),
                        make_cons(interp,
                            make_cons(interp,
                                deduce_rule_test_make_symbol_value(interp, sym_min),
                                make_cons(interp,
                                    deduce_rule_test_make_symbol_value(interp, var_amount),
                                    make_nil(interp))),
                            make_nil(interp))));
            Value* max_head =
                make_cons(interp,
                    deduce_rule_test_make_symbol_value(interp, rel_max),
                    make_cons(interp,
                        deduce_rule_test_make_symbol_value(interp, var_dept),
                        make_cons(interp,
                            make_cons(interp,
                                deduce_rule_test_make_symbol_value(interp, sym_max),
                                make_cons(interp,
                                    deduce_rule_test_make_symbol_value(interp, var_amount),
                                    make_nil(interp))),
                            make_nil(interp))));
            Value* body = deduce_rule_test_make_atom2(
                interp,
                rel_src,
                deduce_rule_test_make_symbol_value(interp, var_dept),
                deduce_rule_test_make_symbol_value(interp, var_amount)
            );

            Value*[3] count_rule_args = { db_val, count_head, body };
            Value*[3] sum_rule_args = { db_val, sum_head, body };
            Value*[3] min_rule_args = { db_val, min_head, body };
            Value*[3] max_rule_args = { db_val, max_head, body };
            Value* count_rule_ok = prim_deduce_rule(count_rule_args[..], null, interp);
            Value* sum_rule_ok = prim_deduce_rule(sum_rule_args[..], null, interp);
            Value* min_rule_ok = prim_deduce_rule(min_rule_args[..], null, interp);
            Value* max_rule_ok = prim_deduce_rule(max_rule_args[..], null, interp);
            ok = count_rule_ok != null && count_rule_ok.tag == VOID &&
                 sum_rule_ok != null && sum_rule_ok.tag == VOID &&
                 min_rule_ok != null && min_rule_ok.tag == VOID &&
                 max_rule_ok != null && max_rule_ok.tag == VOID;
        }

        Value* analyzed = null;
        Value* count_rows = null;
        Value* sum_rows = null;
        Value* min_rows = null;
        Value* max_rows = null;
        if (ok) {
            Value*[1] analyze_args;
            analyze_args[0] = db_val;
            analyzed = prim_deduce_analyze(analyze_args[..], null, interp);

            Value*[1] scan_args = { out_count };
            count_rows = prim_deduce_scan(scan_args[..], null, interp);
            scan_args[0] = out_sum;
            sum_rows = prim_deduce_scan(scan_args[..], null, interp);
            scan_args[0] = out_min;
            min_rows = prim_deduce_scan(scan_args[..], null, interp);
            scan_args[0] = out_max;
            max_rows = prim_deduce_scan(scan_args[..], null, interp);

            ok = analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null;
        }

        if (ok) {
            Value* analyze_status = deduce_query_test_dict_get(interp, analyzed, "status");
            Value* eng = make_string(interp, "eng");
            Value* sales = make_string(interp, "sales");
            Value* count_eng_row = deduce_query_test_find_row_by_value(interp, count_rows, "dept", eng);
            Value* count_sales_row = deduce_query_test_find_row_by_value(interp, count_rows, "dept", sales);
            Value* sum_eng_row = deduce_query_test_find_row_by_value(interp, sum_rows, "dept", eng);
            Value* sum_sales_row = deduce_query_test_find_row_by_value(interp, sum_rows, "dept", sales);
            Value* min_eng_row = deduce_query_test_find_row_by_value(interp, min_rows, "dept", eng);
            Value* min_sales_row = deduce_query_test_find_row_by_value(interp, min_rows, "dept", sales);
            Value* max_eng_row = deduce_query_test_find_row_by_value(interp, max_rows, "dept", eng);
            Value* max_sales_row = deduce_query_test_find_row_by_value(interp, max_rows, "dept", sales);

            Value* count_eng = deduce_query_test_dict_get(interp, count_eng_row, "n");
            Value* count_sales = deduce_query_test_dict_get(interp, count_sales_row, "n");
            Value* sum_eng = deduce_query_test_dict_get(interp, sum_eng_row, "total");
            Value* sum_sales = deduce_query_test_dict_get(interp, sum_sales_row, "total");
            Value* min_eng = deduce_query_test_dict_get(interp, min_eng_row, "low");
            Value* min_sales = deduce_query_test_dict_get(interp, min_sales_row, "low");
            Value* max_eng = deduce_query_test_dict_get(interp, max_eng_row, "high");
            Value* max_sales = deduce_query_test_dict_get(interp, max_sales_row, "high");

            ok = analyze_status != null && analyze_status.tag == SYMBOL && analyze_status.sym_val == interp.symbols.intern("ok") &&
                 count_eng != null && count_eng.tag == INT && count_eng.int_val == 2 &&
                 count_sales != null && count_sales.tag == INT && count_sales.int_val == 2 &&
                 sum_eng != null && sum_eng.tag == INT && sum_eng.int_val == 25 &&
                 sum_sales != null && sum_sales.tag == INT && sum_sales.int_val == 12 &&
                 min_eng != null && min_eng.tag == INT && min_eng.int_val == 10 &&
                 min_sales != null && min_sales.tag == INT && min_sales.int_val == 3 &&
                 max_eng != null && max_eng.tag == INT && max_eng.int_val == 15 &&
                 max_sales != null && max_sales.tag == INT && max_sales.int_val == 9;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce aggregate rules execute grouped count/sum/min/max");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce aggregate rules execute grouped count/sum/min/max");
            (*fail)++;
        }
    }
}

