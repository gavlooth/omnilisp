module lisp;

import std::io;


fn void run_deduce_rule_validation_tests_explain_head(Interp* interp, int* pass, int* fail) {
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

    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        SymbolId edge_a = interp.symbols.intern("parallel_edge_a_explain");
        SymbolId reach_a = interp.symbols.intern("parallel_reach_a_explain");
        SymbolId edge_b = interp.symbols.intern("parallel_edge_b_explain");
        SymbolId reach_b = interp.symbols.intern("parallel_reach_b_explain");
        SymbolId seed_c = interp.symbols.intern("parallel_seed_c_explain");
        SymbolId reach_c = interp.symbols.intern("parallel_reach_c_explain");
        SymbolId col_src = interp.symbols.intern("src");
        SymbolId col_dst = interp.symbols.intern("dst");
        if (ok) {
            Value* ea = setup_deduce_relation_for_db(interp, db_val, edge_a, col_src, col_dst);
            Value* ra = setup_deduce_relation_for_db(interp, db_val, reach_a, col_src, col_dst);
            Value* eb = setup_deduce_relation_for_db(interp, db_val, edge_b, col_src, col_dst);
            Value* rb = setup_deduce_relation_for_db(interp, db_val, reach_b, col_src, col_dst);
            Value* sc = setup_deduce_relation_for_db(interp, db_val, seed_c, col_src, col_dst);
            Value* rc = setup_deduce_relation_for_db(interp, db_val, reach_c, col_src, col_dst);
            ok = ea != null && ea.tag == FFI_HANDLE &&
                 ra != null && ra.tag == FFI_HANDLE &&
                 eb != null && eb.tag == FFI_HANDLE &&
                 rb != null && rb.tag == FFI_HANDLE &&
                 sc != null && sc.tag == FFI_HANDLE &&
                 rc != null && rc.tag == FFI_HANDLE;
        }

        if (ok) {
            SymbolId var_x = interp.symbols.intern("?x");
            SymbolId var_y = interp.symbols.intern("?y");
            SymbolId var_z = interp.symbols.intern("?z");

            Value* head_a_base = deduce_rule_test_make_atom2(
                interp, reach_a,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_a_edge = deduce_rule_test_make_atom2(
                interp, edge_a,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* head_a_rec = deduce_rule_test_make_atom2(
                interp, reach_a,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_z)
            );
            Value* body_a_rec_left = deduce_rule_test_make_atom2(
                interp, edge_a,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_a_rec_right = deduce_rule_test_make_atom2(
                interp, reach_a,
                deduce_rule_test_make_symbol_value(interp, var_y),
                deduce_rule_test_make_symbol_value(interp, var_z)
            );

            Value* head_b_base = deduce_rule_test_make_atom2(
                interp, reach_b,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_b_edge = deduce_rule_test_make_atom2(
                interp, edge_b,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* head_b_rec = deduce_rule_test_make_atom2(
                interp, reach_b,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_z)
            );
            Value* body_b_rec_left = deduce_rule_test_make_atom2(
                interp, edge_b,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_b_rec_right = deduce_rule_test_make_atom2(
                interp, reach_b,
                deduce_rule_test_make_symbol_value(interp, var_y),
                deduce_rule_test_make_symbol_value(interp, var_z)
            );

            Value* head_c_base = deduce_rule_test_make_atom2(
                interp, reach_c,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_c_seed = deduce_rule_test_make_atom2(
                interp, seed_c,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* head_c_rec = deduce_rule_test_make_atom2(
                interp, reach_c,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_z)
            );
            Value* body_c_self = deduce_rule_test_make_atom2(
                interp, reach_c,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_c_dep = deduce_rule_test_make_atom2(
                interp, reach_a,
                deduce_rule_test_make_symbol_value(interp, var_y),
                deduce_rule_test_make_symbol_value(interp, var_z)
            );

            Value*[3] rule_a_base_args = { db_val, head_a_base, body_a_edge };
            Value*[4] rule_a_rec_args = { db_val, head_a_rec, body_a_rec_left, body_a_rec_right };
            Value*[3] rule_b_base_args = { db_val, head_b_base, body_b_edge };
            Value*[4] rule_b_rec_args = { db_val, head_b_rec, body_b_rec_left, body_b_rec_right };
            Value*[3] rule_c_base_args = { db_val, head_c_base, body_c_seed };
            Value*[4] rule_c_rec_args = { db_val, head_c_rec, body_c_self, body_c_dep };
            Value* rv0 = prim_deduce_rule(rule_a_base_args[..], null, interp);
            Value* rv1 = prim_deduce_rule(rule_a_rec_args[..], null, interp);
            Value* rv2 = prim_deduce_rule(rule_b_base_args[..], null, interp);
            Value* rv3 = prim_deduce_rule(rule_b_rec_args[..], null, interp);
            Value* rv4 = prim_deduce_rule(rule_c_base_args[..], null, interp);
            Value* rv5 = prim_deduce_rule(rule_c_rec_args[..], null, interp);
            ok = rv0 != null && rv0.tag == VOID &&
                 rv1 != null && rv1.tag == VOID &&
                 rv2 != null && rv2.tag == VOID &&
                 rv3 != null && rv3.tag == VOID &&
                 rv4 != null && rv4.tag == VOID &&
                 rv5 != null && rv5.tag == VOID;
        }

        if (ok) {
            Value*[2] explain_args;
            explain_args[0] = db_val;
            explain_args[1] = make_int(interp, 1);
            Value* explained = prim_deduce_explain(explain_args[..], null, interp);
            ok = explained != null && explained.tag == HASHMAP && explained.hashmap_val != null;
            if (ok) {
                Value* execution_engine = deduce_rule_test_dict_get(interp, explained, "execution-engine");
                Value* requested_engine = deduce_rule_test_dict_get(interp, explained, "requested-execution-engine");
                Value* execution_path = deduce_rule_test_dict_get(interp, explained, "goal-directed-execution-path");
                Value* component_id = deduce_rule_test_dict_get(interp, explained, "goal-directed-component-id");
                Value* parallel_wave = deduce_rule_test_dict_get(interp, explained, "parallel-wave");
                Value* parallel_batch_size = deduce_rule_test_dict_get(interp, explained, "parallel-batch-size");
                Value* parallel_runtime_mode = deduce_rule_test_dict_get(interp, explained, "parallel-runtime-mode");
                Value* parallel_batch_components = deduce_rule_test_dict_get(interp, explained, "parallel-batch-components");
                Value* parallel_batch_topology = deduce_rule_test_dict_get(interp, explained, "parallel-batch-topology");
                Value* first_batch = is_cons(parallel_batch_topology) ? car(parallel_batch_topology) : null;
                Value* second_batch = is_cons(parallel_batch_topology) ? car(cdr(parallel_batch_topology)) : null;
                Value* first_batch_size = first_batch != null ? deduce_rule_test_dict_get(interp, first_batch, "batch-size") : null;
                Value* second_batch_size = second_batch != null ? deduce_rule_test_dict_get(interp, second_batch, "batch-size") : null;
                ok = execution_engine != null && execution_engine.tag == SYMBOL &&
                     execution_engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                     requested_engine != null && requested_engine.tag == SYMBOL &&
                     requested_engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                     execution_path != null && execution_path.tag == SYMBOL &&
                     execution_path.sym_val == interp.symbols.intern("selected-component-closure") &&
                     component_id != null && component_id.tag == INT &&
                     parallel_wave != null && parallel_wave.tag == INT && parallel_wave.int_val == 1 &&
                     parallel_batch_size != null && parallel_batch_size.tag == INT &&
                     parallel_batch_size.int_val == 2 &&
                     parallel_runtime_mode != null && parallel_runtime_mode.tag == SYMBOL &&
                     parallel_runtime_mode.sym_val == interp.symbols.intern("serial-runtime-parallel-topology-planned") &&
                     deduce_rule_test_list_contains_int(parallel_batch_components, component_id.int_val) &&
                     deduce_rule_test_list_length(parallel_batch_components) == 2 &&
                     parallel_batch_topology != null &&
                     deduce_rule_test_list_length(parallel_batch_topology) == 2 &&
                     first_batch_size != null && first_batch_size.tag == INT && first_batch_size.int_val == 2 &&
                     second_batch_size != null && second_batch_size.tag == INT && second_batch_size.int_val == 1;
            }
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce explain reports parallel batch metadata and topology for recursive component closures");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce explain reports parallel batch metadata and topology for recursive component closures");
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
}
