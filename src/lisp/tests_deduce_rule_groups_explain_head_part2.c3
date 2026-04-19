module lisp;

import std::io;

fn void run_deduce_rule_validation_tests_explain_head_part2(Interp* interp, int* pass, int* fail) {
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
}
