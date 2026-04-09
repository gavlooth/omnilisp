module lisp;

import std::io;

fn void run_deduce_rule_validation_tests_explain_plan_shape(Interp* interp, int* pass, int* fail) {
    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        SymbolId rel_src = interp.symbols.intern("plan_explain_src");
        SymbolId rel_filter = interp.symbols.intern("plan_explain_filter");
        SymbolId rel_out = interp.symbols.intern("plan_explain_out");
        SymbolId col_left = interp.symbols.intern("left");
        SymbolId col_right = interp.symbols.intern("right");
        if (ok) {
            Value* src = setup_deduce_relation_for_db(interp, db_val, rel_src, col_left, col_right);
            Value* filter = setup_deduce_relation_for_db(interp, db_val, rel_filter, col_left, col_right);
            Value* out = setup_deduce_relation_for_db(interp, db_val, rel_out, col_left, col_right);
            ok = src != null && src.tag == FFI_HANDLE &&
                 filter != null && filter.tag == FFI_HANDLE &&
                 out != null && out.tag == FFI_HANDLE;
        }

        if (ok) {
            SymbolId var_x = interp.symbols.intern("?x");
            SymbolId var_y = interp.symbols.intern("?y");
            Value* lit_alice_a = make_string(interp, "Alice");

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
                lit_alice_a,
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
                SymbolId sym_deduce_plan = interp.symbols.intern("deduce-plan");
                SymbolId sym_index_scan = interp.symbols.intern("index-scan");

                Value* kind = deduce_rule_test_dict_get(interp, explained, "kind");
                Value* surface_kind = deduce_rule_test_dict_get(interp, explained, "surface-kind");
                Value* head_pred = deduce_rule_test_dict_get(interp, explained, "head-predicate");
                Value* join_order = deduce_rule_test_dict_get(interp, explained, "join-order");
                Value* steps = deduce_rule_test_dict_get(interp, explained, "steps");

                ok = kind != null && kind.tag == SYMBOL && kind.sym_val == sym_deduce_plan &&
                     surface_kind != null && surface_kind.tag == SYMBOL &&
                     surface_kind.sym_val == interp.symbols.intern("planner-snapshot") &&
                     head_pred != null && head_pred.tag == SYMBOL && head_pred.sym_val == rel_out &&
                     is_cons(join_order) &&
                     car(join_order) != null &&
                     car(join_order).tag == SYMBOL &&
                     car(join_order).sym_val == rel_filter &&
                     is_cons(cdr(join_order)) &&
                     car(cdr(join_order)) != null &&
                     car(cdr(join_order)).tag == SYMBOL &&
                     car(cdr(join_order)).sym_val == rel_src &&
                     is_nil(cdr(cdr(join_order))) &&
                     is_cons(steps) &&
                     is_cons(cdr(steps)) &&
                     is_nil(cdr(cdr(steps)));

                if (ok) {
                    Value* step0 = car(steps);
                    Value* step1 = car(cdr(steps));
                    Value* step0_pred = deduce_rule_test_dict_get(interp, step0, "predicate");
                    Value* step0_op = deduce_rule_test_dict_get(interp, step0, "operator");
                    Value* step0_mask = deduce_rule_test_dict_get(interp, step0, "bound-mask");
                    Value* step0_idx = deduce_rule_test_dict_get(interp, step0, "selected-index");
                    Value* step0_filter_pushdown = deduce_rule_test_dict_get(interp, step0, "filter-pushdown");
                    Value* step0_projection_mask = deduce_rule_test_dict_get(interp, step0, "projection-mask");
                    Value* step0_counters = deduce_rule_test_dict_get(interp, step0, "counters");
                    Value* step1_pred = deduce_rule_test_dict_get(interp, step1, "predicate");
                    Value* step1_op = deduce_rule_test_dict_get(interp, step1, "operator");
                    Value* step1_mask = deduce_rule_test_dict_get(interp, step1, "bound-mask");
                    Value* step1_idx = deduce_rule_test_dict_get(interp, step1, "selected-index");
                    Value* step1_filter_pushdown = deduce_rule_test_dict_get(interp, step1, "filter-pushdown");
                    Value* step1_projection_mask = deduce_rule_test_dict_get(interp, step1, "projection-mask");
                    Value* step1_counters = deduce_rule_test_dict_get(interp, step1, "counters");
                    SymbolId sym_observed = interp.symbols.intern("observed");

                    Value* step0_rows_read = deduce_rule_test_dict_get(interp, step0_counters, "rows-read");
                    Value* step0_rows_emitted = deduce_rule_test_dict_get(interp, step0_counters, "rows-emitted");
                    Value* step0_index_hits = deduce_rule_test_dict_get(interp, step0_counters, "index-hits");
                    Value* step0_index_misses = deduce_rule_test_dict_get(interp, step0_counters, "index-misses");
                    Value* step0_join_probes = deduce_rule_test_dict_get(interp, step0_counters, "join-probes");
                    Value* step0_counter_kind = deduce_rule_test_dict_get(interp, step0_counters, "counter-kind");
                    Value* step1_rows_read = deduce_rule_test_dict_get(interp, step1_counters, "rows-read");
                    Value* step1_rows_emitted = deduce_rule_test_dict_get(interp, step1_counters, "rows-emitted");
                    Value* step1_index_hits = deduce_rule_test_dict_get(interp, step1_counters, "index-hits");
                    Value* step1_index_misses = deduce_rule_test_dict_get(interp, step1_counters, "index-misses");
                    Value* step1_join_probes = deduce_rule_test_dict_get(interp, step1_counters, "join-probes");
                    Value* step1_counter_kind = deduce_rule_test_dict_get(interp, step1_counters, "counter-kind");

                    ok = step0_pred != null && step0_pred.tag == SYMBOL && step0_pred.sym_val == rel_filter &&
                         step0_op != null && step0_op.tag == SYMBOL && step0_op.sym_val == sym_index_scan &&
                         step0_mask != null && step0_mask.tag == INT && step0_mask.int_val == 1 &&
                         step0_idx != null && step0_idx.tag == INT && step0_idx.int_val >= 0 &&
                         step0_filter_pushdown != null && step0_filter_pushdown.tag == SYMBOL &&
                         step0_filter_pushdown.sym_val == interp.sym_true &&
                         step0_projection_mask != null && step0_projection_mask.tag == INT &&
                         step0_projection_mask.int_val == 2 &&
                         step1_pred != null && step1_pred.tag == SYMBOL && step1_pred.sym_val == rel_src &&
                         step1_op != null && step1_op.tag == SYMBOL && step1_op.sym_val == sym_index_scan &&
                         step1_mask != null && step1_mask.tag == INT && step1_mask.int_val == 2 &&
                         step1_idx != null && step1_idx.tag == INT && step1_idx.int_val >= 0 &&
                         step1_filter_pushdown != null && step1_filter_pushdown.tag == SYMBOL &&
                         step1_filter_pushdown.sym_val == interp.sym_true &&
                         step1_projection_mask != null && step1_projection_mask.tag == INT &&
                         step1_projection_mask.int_val == 3 &&
                         step0_counters != null && step0_counters.tag == HASHMAP &&
                         step0_rows_read != null && step0_rows_read.tag == INT && step0_rows_read.int_val == 0 &&
                         step0_rows_emitted != null && step0_rows_emitted.tag == INT && step0_rows_emitted.int_val == 0 &&
                         step0_index_hits != null && step0_index_hits.tag == INT && step0_index_hits.int_val == 0 &&
                         step0_index_misses != null && step0_index_misses.tag == INT && step0_index_misses.int_val == 0 &&
                         step0_join_probes != null && step0_join_probes.tag == INT && step0_join_probes.int_val == 0 &&
                         step0_counter_kind != null && step0_counter_kind.tag == SYMBOL &&
                         step0_counter_kind.sym_val == sym_observed &&
                         step1_counters != null && step1_counters.tag == HASHMAP &&
                         step1_rows_read != null && step1_rows_read.tag == INT && step1_rows_read.int_val == 0 &&
                         step1_rows_emitted != null && step1_rows_emitted.tag == INT && step1_rows_emitted.int_val == 0 &&
                         step1_index_hits != null && step1_index_hits.tag == INT && step1_index_hits.int_val == 0 &&
                         step1_index_misses != null && step1_index_misses.tag == INT && step1_index_misses.int_val == 0 &&
                         step1_join_probes != null && step1_join_probes.tag == INT && step1_join_probes.int_val == 0 &&
                         step1_counter_kind != null && step1_counter_kind.tag == SYMBOL &&
                         step1_counter_kind.sym_val == sym_observed;
                }
            }
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain emits deterministic operator + join-order payload");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain emits deterministic operator + join-order payload");
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
        SymbolId rel_src = interp.symbols.intern("plan_regress_scan_src");
        SymbolId rel_out = interp.symbols.intern("plan_regress_scan_out");
        if (ok) {
            Value* src = setup_deduce_relation_for_db(interp, db_val, rel_src, col_left, col_right);
            Value* out = setup_deduce_relation_for_db(interp, db_val, rel_out, col_left, col_right);
            ok = src != null && src.tag == FFI_HANDLE &&
                 out != null && out.tag == FFI_HANDLE;
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

            Value*[3] rule_args;
            rule_args[0] = db_val;
            rule_args[1] = head;
            rule_args[2] = body_src;
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
                Value* steps = deduce_rule_test_dict_get(interp, explained, "steps");
                Value* step0 = is_cons(steps) ? car(steps) : null;
                Value* step0_operator = deduce_rule_test_dict_get(interp, step0, "operator");
                ok = is_cons(steps) &&
                     is_nil(cdr(steps)) &&
                     step0_operator != null &&
                     step0_operator.tag == SYMBOL &&
                     step0_operator.sym_val == interp.symbols.intern("full-scan");
            }
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain regression preserves broad scan shape");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain regression preserves broad scan shape");
            (*fail)++;
        }
    }

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
        SymbolId rel_big = interp.symbols.intern("plan_regress_skew_big");
        SymbolId rel_small = interp.symbols.intern("plan_regress_skew_small");
        SymbolId rel_out = interp.symbols.intern("plan_regress_skew_out");
        Value* big_rel = null;
        Value* small_rel = null;
        if (ok) {
            big_rel = setup_deduce_relation_for_db(interp, db_val, rel_big, col_left, col_right);
            small_rel = setup_deduce_relation_for_db(interp, db_val, rel_small, col_left, col_right);
            Value* out = setup_deduce_relation_for_db(interp, db_val, rel_out, col_left, col_right);
            ok = big_rel != null && big_rel.tag == FFI_HANDLE &&
                 small_rel != null && small_rel.tag == FFI_HANDLE &&
                 out != null && out.tag == FFI_HANDLE &&
                 deduce_rule_test_seed_pair_rows(interp, big_rel, 320) &&
                 deduce_rule_test_seed_pair_rows(interp, small_rel, 2);
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
            Value* body_big = deduce_rule_test_make_atom2(
                interp,
                rel_big,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_small = deduce_rule_test_make_atom2(
                interp,
                rel_small,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );

            Value*[4] rule_args;
            rule_args[0] = db_val;
            rule_args[1] = head;
            rule_args[2] = body_big;
            rule_args[3] = body_small;
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
                Value* step0_pred = deduce_rule_test_dict_get(interp, step0, "predicate");
                ok = is_cons(join_order) &&
                     car(join_order) != null &&
                     car(join_order).tag == SYMBOL &&
                     car(join_order).sym_val == rel_small &&
                     is_cons(cdr(join_order)) &&
                     car(cdr(join_order)) != null &&
                     car(cdr(join_order)).tag == SYMBOL &&
                     car(cdr(join_order)).sym_val == rel_big &&
                     step0_pred != null &&
                     step0_pred.tag == SYMBOL &&
                     step0_pred.sym_val == rel_small;
            }
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain regression preserves skew-sensitive join ordering");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain regression preserves skew-sensitive join ordering");
            (*fail)++;
        }
    }
}
