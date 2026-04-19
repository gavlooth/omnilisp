module lisp;

import std::io;

fn void run_deduce_rule_validation_tests_explain_head_part5(Interp* interp, int* pass, int* fail) {
    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        SymbolId col_left = interp.symbols.intern("left");
        SymbolId col_right = interp.symbols.intern("right");
        SymbolId rel_src = interp.symbols.intern("plan_regress_src");
        SymbolId rel_filter = interp.symbols.intern("plan_regress_filter");
        SymbolId rel_out = interp.symbols.intern("plan_regress_out");
        if (ok) {
            Value* src = setup_deduce_relation_for_db(interp, db_val, rel_src, col_left, col_right);
            Value* filter = setup_deduce_relation_for_db(interp, db_val, rel_filter, col_left, col_right);
            Value* out = setup_deduce_relation_for_db(interp, db_val, rel_out, col_left, col_right);
            ok = src != null && src.tag == FFI_HANDLE &&
                 filter != null && filter.tag == FFI_HANDLE &&
                 out != null && out.tag == FFI_HANDLE &&
                 deduce_rule_test_seed_pair_rows(interp, src, 4) &&
                 deduce_rule_test_seed_pair_rows(interp, filter, 1);
        }

        if (ok) {
            SymbolId var_x = interp.symbols.intern("?x");
            SymbolId var_y = interp.symbols.intern("?y");
            Value* head = deduce_rule_test_make_atom2(
                interp,
                rel_out,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_src = deduce_rule_test_make_atom2(
                interp,
                rel_src,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_filter = deduce_rule_test_make_atom2(
                interp,
                rel_filter,
                make_string(interp, "Alice"),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );

            Value*[4] rule_args;
            rule_args[0] = db_val;
            rule_args[1] = head;
            rule_args[2] = body_src;
            rule_args[3] = body_filter;
            Value* rv = prim_deduce_rule(rule_args[..], null, interp);
            ok = rv != null && rv.tag == VOID;
        }

        if (ok) {
            Value*[2] explain_args;
            explain_args[0] = db_val;
            explain_args[1] = deduce_rule_test_make_symbol_value(interp, rel_out);
            Value* explained = prim_deduce_explain(explain_args[..], null, interp);
            ok = explained != null &&
                 explained.tag == HASHMAP &&
                 explained.hashmap_val != null;
            if (ok) {
                Value* join_order = deduce_rule_test_dict_get(interp, explained, "join-order");
                Value* steps = deduce_rule_test_dict_get(interp, explained, "steps");
                Value* step0 = is_cons(steps) ? car(steps) : null;
                Value* step1 = is_cons(steps) && is_cons(cdr(steps)) ? car(cdr(steps)) : null;
                Value* step0_pred = deduce_rule_test_dict_get(interp, step0, "predicate");
                Value* step0_operator = deduce_rule_test_dict_get(interp, step0, "operator");
                Value* step0_counters = deduce_rule_test_dict_get(interp, step0, "counters");
                Value* step0_rows_read = deduce_rule_test_dict_get(interp, step0_counters, "rows-read");
                Value* step0_rows_emitted = deduce_rule_test_dict_get(interp, step0_counters, "rows-emitted");
                Value* step0_join_probes = deduce_rule_test_dict_get(interp, step0_counters, "join-probes");
                Value* step0_counter_kind = deduce_rule_test_dict_get(interp, step0_counters, "counter-kind");
                Value* step1_pred = deduce_rule_test_dict_get(interp, step1, "predicate");
                Value* step1_operator = deduce_rule_test_dict_get(interp, step1, "operator");
                Value* step1_counters = deduce_rule_test_dict_get(interp, step1, "counters");
                Value* step1_rows_read = deduce_rule_test_dict_get(interp, step1_counters, "rows-read");
                Value* step1_rows_emitted = deduce_rule_test_dict_get(interp, step1_counters, "rows-emitted");
                Value* step1_join_probes = deduce_rule_test_dict_get(interp, step1_counters, "join-probes");
                Value* step1_counter_kind = deduce_rule_test_dict_get(interp, step1_counters, "counter-kind");

                ok = is_cons(join_order) &&
                     car(join_order) != null &&
                     car(join_order).tag == SYMBOL &&
                     car(join_order).sym_val == rel_filter &&
                     is_cons(cdr(join_order)) &&
                     car(cdr(join_order)) != null &&
                     car(cdr(join_order)).tag == SYMBOL &&
                     car(cdr(join_order)).sym_val == rel_src &&
                     step0_pred != null && step0_pred.tag == SYMBOL && step0_pred.sym_val == rel_filter &&
                     step0_operator != null && step0_operator.tag == SYMBOL &&
                     step0_operator.sym_val == interp.symbols.intern("index-scan") &&
                     step0_rows_read != null && step0_rows_read.tag == INT && step0_rows_read.int_val == 1 &&
                     step0_rows_emitted != null && step0_rows_emitted.tag == INT && step0_rows_emitted.int_val == 0 &&
                     step0_join_probes != null && step0_join_probes.tag == INT && step0_join_probes.int_val == 0 &&
                     step0_counter_kind != null && step0_counter_kind.tag == SYMBOL &&
                     step0_counter_kind.sym_val == interp.symbols.intern("observed") &&
                     step1_pred != null && step1_pred.tag == SYMBOL && step1_pred.sym_val == rel_src &&
                     step1_operator != null && step1_operator.tag == SYMBOL &&
                     step1_operator.sym_val == interp.symbols.intern("index-scan") &&
                     step1_rows_read != null && step1_rows_read.tag == INT && step1_rows_read.int_val == 0 &&
                     step1_rows_emitted != null && step1_rows_emitted.tag == INT && step1_rows_emitted.int_val == 0 &&
                     step1_join_probes != null && step1_join_probes.tag == INT && step1_join_probes.int_val == 0 &&
                     step1_counter_kind != null && step1_counter_kind.tag == SYMBOL &&
                     step1_counter_kind.sym_val == interp.symbols.intern("observed");
            }
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain regression preserves selective lookup ordering");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain regression preserves selective lookup ordering");
            (*fail)++;
        }
    }
}
