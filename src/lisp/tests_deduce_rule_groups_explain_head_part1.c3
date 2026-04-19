module lisp;

import std::io;

fn void run_deduce_rule_validation_tests_explain_head_part1(Interp* interp, int* pass, int* fail) {
    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        SymbolId edge_sym = interp.symbols.intern("goal_explain_edge");
        SymbolId reach_sym = interp.symbols.intern("goal_explain_reach");
        SymbolId col_src = interp.symbols.intern("src");
        SymbolId col_dst = interp.symbols.intern("dst");
        Value* edge_rel = null;
        Value* reach_rel = null;
        if (ok) {
            edge_rel = setup_deduce_relation_for_db(interp, db_val, edge_sym, col_src, col_dst);
            reach_rel = setup_deduce_relation_for_db(interp, db_val, reach_sym, col_src, col_dst);
            ok = edge_rel != null && edge_rel.tag == FFI_HANDLE &&
                 reach_rel != null && reach_rel.tag == FFI_HANDLE;
        }

        if (ok) {
            Value*[3] insert0 = { edge_rel, make_int(interp, 1), make_int(interp, 2) };
            Value*[3] insert1 = { edge_rel, make_int(interp, 2), make_int(interp, 3) };
            Value* rv0 = prim_assert_fact(insert0[..], null, interp);
            Value* rv1 = prim_assert_fact(insert1[..], null, interp);
            ok = rv0 != null && rv0.tag == VOID && rv1 != null && rv1.tag == VOID;
        }

        if (ok) {
            SymbolId var_x = interp.symbols.intern("?x");
            SymbolId var_y = interp.symbols.intern("?y");
            SymbolId var_z = interp.symbols.intern("?z");

            Value* head_base = deduce_rule_test_make_atom2(
                interp,
                reach_sym,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_edge = deduce_rule_test_make_atom2(
                interp,
                edge_sym,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value*[3] base_rule_args = { db_val, head_base, body_edge };
            Value* base_rv = prim_deduce_rule(base_rule_args[..], null, interp);

            Value* head_rec = deduce_rule_test_make_atom2(
                interp,
                reach_sym,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_z)
            );
            Value* body_edge_rec = deduce_rule_test_make_atom2(
                interp,
                edge_sym,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_reach_rec = deduce_rule_test_make_atom2(
                interp,
                reach_sym,
                deduce_rule_test_make_symbol_value(interp, var_y),
                deduce_rule_test_make_symbol_value(interp, var_z)
            );
            Value*[4] rec_rule_args = { db_val, head_rec, body_edge_rec, body_reach_rec };
            Value* rec_rv = prim_deduce_rule(rec_rule_args[..], null, interp);
            ok = base_rv != null && base_rv.tag == VOID && rec_rv != null && rec_rv.tag == VOID;
        }

        if (ok) {
            SymbolId wildcard_sym = interp.symbols.intern("_");
            Value* match_pattern = make_cons(
                interp,
                deduce_rule_test_make_symbol_value(interp, reach_sym),
                make_cons(
                    interp,
                    deduce_rule_test_make_symbol_value(interp, wildcard_sym),
                    make_cons(interp, make_int(interp, 3), make_nil(interp))
                )
            );
            Value*[2] match_args = { reach_rel, match_pattern };
            Value* matched = prim_deduce_match(match_args[..], null, interp);
            ok = matched != null &&
                 matched.tag == CONS &&
                 deduce_rule_test_list_length(matched) == 2;
        }

        if (ok) {
            Value*[2] explain_args;
            explain_args[0] = db_val;
            explain_args[1] = make_int(interp, 1);
            Value* explained = prim_deduce_explain(explain_args[..], null, interp);
            ok = explained != null && explained.tag == HASHMAP && explained.hashmap_val != null;
            if (ok) {
                Value* eligible = deduce_rule_test_dict_get(interp, explained, "goal-directed-eligible");
                Value* shape = deduce_rule_test_dict_get(interp, explained, "goal-directed-shape");
                Value* blockers = deduce_rule_test_dict_get(interp, explained, "goal-directed-blockers");
                Value* component_id = deduce_rule_test_dict_get(interp, explained, "goal-directed-component-id");
                Value* execution_path = deduce_rule_test_dict_get(interp, explained, "goal-directed-execution-path");
                Value* selected_components = deduce_rule_test_dict_get(interp, explained, "goal-directed-selected-components");
                Value* selected_predicates = deduce_rule_test_dict_get(interp, explained, "goal-directed-selected-predicates");
                Value* demand_bound_head_positions = deduce_rule_test_dict_get(
                    interp,
                    explained,
                    "goal-directed-demand-bound-head-positions"
                );
                Value* last_execution_path = deduce_rule_test_dict_get(
                    interp,
                    explained,
                    "last-goal-directed-read-execution-path"
                );
                Value* last_surface = deduce_rule_test_dict_get(
                    interp,
                    explained,
                    "last-goal-directed-read-surface"
                );
                Value* last_fallback_reason = deduce_rule_test_dict_get(
                    interp,
                    explained,
                    "last-goal-directed-read-fallback-reason"
                );
                Value* requested_bound_count = deduce_rule_test_dict_get(
                    interp,
                    explained,
                    "last-goal-directed-read-requested-bound-count"
                );
                Value* requested_bound_positions = deduce_rule_test_dict_get(
                    interp,
                    explained,
                    "last-goal-directed-read-requested-bound-positions"
                );
                Value* applied_bound_count = deduce_rule_test_dict_get(
                    interp,
                    explained,
                    "last-goal-directed-read-applied-bound-count"
                );
                Value* applied_bound_positions = deduce_rule_test_dict_get(
                    interp,
                    explained,
                    "last-goal-directed-read-applied-bound-positions"
                );
                ok = eligible != null && eligible.tag == SYMBOL && eligible.sym_val == interp.sym_true &&
                     shape != null && shape.tag == SYMBOL &&
                     shape.sym_val == interp.symbols.intern("positive-recursive-closure") &&
                     blockers != null && blockers.tag == NIL &&
                     component_id != null && component_id.tag == INT && component_id.int_val >= 0 &&
                     execution_path != null && execution_path.tag == SYMBOL &&
                     execution_path.sym_val == interp.symbols.intern("selected-component-closure") &&
                     deduce_rule_test_list_contains_int(selected_components, component_id.int_val) &&
                     deduce_rule_test_list_contains_symbol(selected_predicates, edge_sym) &&
                     deduce_rule_test_list_contains_symbol(selected_predicates, reach_sym) &&
                     deduce_rule_test_list_contains_int(demand_bound_head_positions, 1) &&
                     !deduce_rule_test_list_contains_int(demand_bound_head_positions, 0) &&
                     last_execution_path != null && last_execution_path.tag == SYMBOL &&
                     last_execution_path.sym_val == interp.symbols.intern("ephemeral-head-demand-match") &&
                     last_surface != null && last_surface.tag == SYMBOL &&
                     last_surface.sym_val == interp.symbols.intern("match") &&
                     last_fallback_reason != null && last_fallback_reason.tag == NIL &&
                     requested_bound_count != null && requested_bound_count.tag == INT &&
                     requested_bound_count.int_val == 1 &&
                     requested_bound_positions != null &&
                     deduce_rule_test_list_length(requested_bound_positions) == 1 &&
                     deduce_rule_test_list_contains_int(requested_bound_positions, 1) &&
                     applied_bound_count != null && applied_bound_count.tag == INT &&
                     applied_bound_count.int_val == 1 &&
                     applied_bound_positions != null &&
                     deduce_rule_test_list_length(applied_bound_positions) == 1 &&
                     deduce_rule_test_list_contains_int(applied_bound_positions, 1);
            }
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain marks positive recursive closure rules as goal-directed candidates");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain marks positive recursive closure rules as goal-directed candidates");
            (*fail)++;
        }
    }
}
