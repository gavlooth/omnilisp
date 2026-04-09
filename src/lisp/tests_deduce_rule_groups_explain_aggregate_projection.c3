module lisp;

import std::io;

fn void run_deduce_rule_validation_tests_explain_aggregate_projection(Interp* interp, int* pass, int* fail) {
    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        SymbolId rel_src = interp.symbols.intern("sales_src");
        SymbolId rel_out = interp.symbols.intern("sales_rollup");
        SymbolId col_dept = interp.symbols.intern("dept");
        SymbolId col_amount = interp.symbols.intern("amount");
        SymbolId col_total = interp.symbols.intern("total");
        SymbolId var_dept = interp.symbols.intern("?dept");
        SymbolId var_amount = interp.symbols.intern("?amount");
        SymbolId sym_sum = interp.symbols.intern("sum");

        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool setup_ok = db_val != null &&
                        db_val.tag == FFI_HANDLE &&
                        db_val.ffi_val != null &&
                        db_val.ffi_val.lib_handle != null;
        if (setup_ok) {
            Value* src = setup_deduce_relation_for_db(interp, db_val, rel_src, col_dept, col_amount);
            Value* out = setup_deduce_relation_for_db(interp, db_val, rel_out, col_dept, col_total);
            setup_ok = src != null && src.tag == FFI_HANDLE &&
                       out != null && out.tag == FFI_HANDLE;
        }

        bool explain_ok = false;
        bool analyze_ok = false;
        if (setup_ok) {
            Value* head =
                make_cons(interp,
                    deduce_rule_test_make_symbol_value(interp, rel_out),
                    make_cons(interp,
                        deduce_rule_test_make_symbol_value(interp, var_dept),
                        make_cons(interp,
                            make_cons(interp,
                                deduce_rule_test_make_symbol_value(interp, sym_sum),
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

            Value*[3] rule_args;
            rule_args[0] = db_val;
            rule_args[1] = head;
            rule_args[2] = body;
            Value* rv = prim_deduce_rule(rule_args[..], null, interp);
            if (rv != null && rv.tag == VOID) {
                Value*[1] analyze_args;
                analyze_args[0] = db_val;
                Value* analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
                analyze_ok = analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null;

                Value*[2] explain_args;
                explain_args[0] = db_val;
                explain_args[1] = deduce_rule_test_make_symbol_value(interp, rel_out);
                Value* explained = prim_deduce_explain(explain_args[..], null, interp);
                explain_ok = explained != null && explained.tag == HASHMAP && explained.hashmap_val != null;
                if (explain_ok) {
                    Value* aggregate_count = deduce_rule_test_dict_get(interp, explained, "aggregate-projection-count");
                    Value* projections = deduce_rule_test_dict_get(interp, explained, "head-projections");
                    Value* group_proj = is_cons(projections) ? car(projections) : null;
                    Value* agg_proj = is_cons(projections) && is_cons(cdr(projections)) ? car(cdr(projections)) : null;
                    Value* group_kind = deduce_rule_test_dict_get(interp, group_proj, "projection-kind");
                    Value* group_source = deduce_rule_test_dict_get(interp, group_proj, "source-var-id");
                    Value* agg_kind = deduce_rule_test_dict_get(interp, agg_proj, "projection-kind");
                    Value* agg_source = deduce_rule_test_dict_get(interp, agg_proj, "source-var-id");

                    explain_ok = aggregate_count != null &&
                                 aggregate_count.tag == INT &&
                                 aggregate_count.int_val == 1 &&
                                 is_cons(projections) &&
                                 is_cons(cdr(projections)) &&
                                 is_nil(cdr(cdr(projections))) &&
                                 group_kind != null &&
                                 group_kind.tag == SYMBOL &&
                                 group_kind.sym_val == interp.symbols.intern("group-key") &&
                                 group_source != null &&
                                 group_source.tag == INT &&
                                 group_source.int_val == 1 &&
                                 agg_kind != null &&
                                 agg_kind.tag == SYMBOL &&
                                 agg_kind.sym_val == interp.symbols.intern("aggregate") &&
                                 agg_source != null &&
                                 agg_source.tag == INT &&
                                 agg_source.int_val == 2;
                }
            }
        }

        if (explain_ok) {
            emit_pass_literal("[PASS] deduce explain exposes aggregate projection metadata");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain exposes aggregate projection metadata");
            (*fail)++;
        }

        if (analyze_ok) {
            emit_pass_literal("[PASS] deduce analyze executes aggregate rules once grouped execution lands");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce analyze executes aggregate rules once grouped execution lands");
            (*fail)++;
        }
    }
}
