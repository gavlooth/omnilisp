module lisp;

import std::io;

fn bool deduce_query_bench_seed_recursive_aggregate_db(
    Interp* interp,
    usz node_count,
    Value** db_out,
    Value** out_rel_out,
    usz* expected_rows_out
) {
    if (interp == null || db_out == null || out_rel_out == null || expected_rows_out == null || node_count < 2) {
        return false;
    }
    *db_out = null;
    *out_rel_out = null;
    *expected_rows_out = 0;

    Value*[1] open_args;
    open_args[0] = make_symbol(interp, interp.symbols.intern("memory"));
    Value* db = prim_deduce_open(open_args[..], null, interp);
    if (db == null || db.tag != FFI_HANDLE || db.ffi_val == null) return false;

    SymbolId edge_sym = interp.symbols.intern("bench_recursive_edge");
    SymbolId unit_sym = interp.symbols.intern("bench_recursive_unit");
    SymbolId total_sym = interp.symbols.intern("bench_recursive_total");
    SymbolId col_node = interp.symbols.intern("node");
    SymbolId col_dst = interp.symbols.intern("dst");
    SymbolId col_weight = interp.symbols.intern("weight");
    SymbolId col_total = interp.symbols.intern("total");
    Value* edge_rel = setup_deduce_relation_for_db(interp, db, edge_sym, col_node, col_dst);
    Value* unit_rel = setup_deduce_relation_for_db(interp, db, unit_sym, col_node, col_weight);
    Value* out_rel = setup_deduce_relation_for_db(interp, db, total_sym, col_node, col_total);
    if (edge_rel == null || edge_rel.tag != FFI_HANDLE || edge_rel.ffi_val == null) return false;
    if (unit_rel == null || unit_rel.tag != FFI_HANDLE || unit_rel.ffi_val == null) return false;
    if (out_rel == null || out_rel.tag != FFI_HANDLE || out_rel.ffi_val == null) return false;

    SymbolId var_x = interp.symbols.intern("?x");
    SymbolId var_y = interp.symbols.intern("?y");
    SymbolId var_n = interp.symbols.intern("?n");
    SymbolId sym_sum = interp.symbols.intern("sum");

    Value* base_head =
        make_cons(interp,
            deduce_query_bench_make_symbol_value(interp, total_sym),
            make_cons(interp,
                deduce_query_bench_make_symbol_value(interp, var_x),
                make_cons(interp,
                    make_cons(interp,
                        deduce_query_bench_make_symbol_value(interp, sym_sum),
                        make_cons(interp,
                            deduce_query_bench_make_symbol_value(interp, var_n),
                            make_nil(interp))),
                    make_nil(interp))));
    Value* base_body = deduce_query_bench_make_atom2(
        interp,
        unit_sym,
        deduce_query_bench_make_symbol_value(interp, var_x),
        deduce_query_bench_make_symbol_value(interp, var_n)
    );
    Value*[3] base_rule_args;
    base_rule_args[0] = db;
    base_rule_args[1] = base_head;
    base_rule_args[2] = base_body;
    Value* base_rule_ok = prim_deduce_rule(base_rule_args[..], null, interp);
    if (base_rule_ok == null || base_rule_ok.tag == ERROR) return false;

    Value* rec_head =
        make_cons(interp,
            deduce_query_bench_make_symbol_value(interp, total_sym),
            make_cons(interp,
                deduce_query_bench_make_symbol_value(interp, var_x),
                make_cons(interp,
                    make_cons(interp,
                        deduce_query_bench_make_symbol_value(interp, sym_sum),
                        make_cons(interp,
                            deduce_query_bench_make_symbol_value(interp, var_n),
                            make_nil(interp))),
                    make_nil(interp))));
    Value* rec_edge_body = deduce_query_bench_make_atom2(
        interp,
        edge_sym,
        deduce_query_bench_make_symbol_value(interp, var_x),
        deduce_query_bench_make_symbol_value(interp, var_y)
    );
    Value* rec_total_body = deduce_query_bench_make_atom2(
        interp,
        total_sym,
        deduce_query_bench_make_symbol_value(interp, var_y),
        deduce_query_bench_make_symbol_value(interp, var_n)
    );
    Value*[4] rec_rule_args;
    rec_rule_args[0] = db;
    rec_rule_args[1] = rec_head;
    rec_rule_args[2] = rec_edge_body;
    rec_rule_args[3] = rec_total_body;
    Value* rec_rule_ok = prim_deduce_rule(rec_rule_args[..], null, interp);
    if (rec_rule_ok == null || rec_rule_ok.tag == ERROR) return false;

    for (usz i = 0; i < node_count; i++) {
        Value*[3] unit_args;
        unit_args[0] = unit_rel;
        unit_args[1] = make_int(interp, (long)i);
        unit_args[2] = make_int(interp, 1);
        Value* inserted = prim_assert_fact(unit_args[..], null, interp);
        if (inserted == null || inserted.tag == ERROR) return false;
        if (i + 1 < node_count) {
            Value*[3] edge_args;
            edge_args[0] = edge_rel;
            edge_args[1] = make_int(interp, (long)i);
            edge_args[2] = make_int(interp, (long)(i + 1));
            Value* edge_inserted = prim_assert_fact(edge_args[..], null, interp);
            if (edge_inserted == null || edge_inserted.tag == ERROR) return false;
        }
    }

    *db_out = db;
    *out_rel_out = out_rel;
    *expected_rows_out = node_count;
    return true;
}

fn bool deduce_query_bench_seed_recursive_mixed_aggregate_db_named(
    Interp* interp,
    usz node_count,
    SymbolId edge_sym,
    SymbolId unit_sym,
    SymbolId score_sym,
    SymbolId chosen_sym,
    Value** db_out,
    Value** score_rel_out,
    Value** chosen_rel_out,
    usz* expected_rows_out
) {
    if (interp == null || db_out == null || score_rel_out == null ||
        chosen_rel_out == null || expected_rows_out == null || node_count < 2) {
        return false;
    }
    *db_out = null;
    *score_rel_out = null;
    *chosen_rel_out = null;
    *expected_rows_out = 0;

    Value*[1] open_args;
    open_args[0] = make_symbol(interp, interp.symbols.intern("memory"));
    Value* db = prim_deduce_open(open_args[..], null, interp);
    if (db == null || db.tag != FFI_HANDLE || db.ffi_val == null) return false;

    SymbolId col_node = interp.symbols.intern("node");
    SymbolId col_dst = interp.symbols.intern("dst");
    SymbolId col_weight = interp.symbols.intern("weight");
    SymbolId col_total = interp.symbols.intern("total");
    Value* edge_rel = setup_deduce_relation_for_db(interp, db, edge_sym, col_node, col_dst);
    Value* unit_rel = setup_deduce_relation_for_db(interp, db, unit_sym, col_node, col_weight);
    Value* score_rel = setup_deduce_relation_for_db(interp, db, score_sym, col_node, col_total);
    Value* chosen_rel = setup_deduce_relation_for_db(interp, db, chosen_sym, col_node, col_total);
    if (edge_rel == null || edge_rel.tag != FFI_HANDLE || edge_rel.ffi_val == null) return false;
    if (unit_rel == null || unit_rel.tag != FFI_HANDLE || unit_rel.ffi_val == null) return false;
    if (score_rel == null || score_rel.tag != FFI_HANDLE || score_rel.ffi_val == null) return false;
    if (chosen_rel == null || chosen_rel.tag != FFI_HANDLE || chosen_rel.ffi_val == null) return false;

    SymbolId var_x = interp.symbols.intern("?x");
    SymbolId var_y = interp.symbols.intern("?y");
    SymbolId var_n = interp.symbols.intern("?n");
    SymbolId sym_sum = interp.symbols.intern("sum");

    Value* chosen_head = deduce_query_bench_make_atom2(
        interp,
        chosen_sym,
        deduce_query_bench_make_symbol_value(interp, var_x),
        deduce_query_bench_make_symbol_value(interp, var_n)
    );
    Value* chosen_body = deduce_query_bench_make_atom2(
        interp,
        score_sym,
        deduce_query_bench_make_symbol_value(interp, var_x),
        deduce_query_bench_make_symbol_value(interp, var_n)
    );
    Value*[3] chosen_rule_args;
    chosen_rule_args[0] = db;
    chosen_rule_args[1] = chosen_head;
    chosen_rule_args[2] = chosen_body;
    Value* chosen_rule_ok = prim_deduce_rule(chosen_rule_args[..], null, interp);
    if (chosen_rule_ok == null || chosen_rule_ok.tag == ERROR) return false;

    Value* base_head =
        make_cons(interp,
            deduce_query_bench_make_symbol_value(interp, score_sym),
            make_cons(interp,
                deduce_query_bench_make_symbol_value(interp, var_x),
                make_cons(interp,
                    make_cons(interp,
                        deduce_query_bench_make_symbol_value(interp, sym_sum),
                        make_cons(interp,
                            deduce_query_bench_make_symbol_value(interp, var_n),
                            make_nil(interp))),
                    make_nil(interp))));
    Value* base_body = deduce_query_bench_make_atom2(
        interp,
        unit_sym,
        deduce_query_bench_make_symbol_value(interp, var_x),
        deduce_query_bench_make_symbol_value(interp, var_n)
    );
    Value*[3] base_rule_args;
    base_rule_args[0] = db;
    base_rule_args[1] = base_head;
    base_rule_args[2] = base_body;
    Value* base_rule_ok = prim_deduce_rule(base_rule_args[..], null, interp);
    if (base_rule_ok == null || base_rule_ok.tag == ERROR) return false;

    Value* rec_head =
        make_cons(interp,
            deduce_query_bench_make_symbol_value(interp, score_sym),
            make_cons(interp,
                deduce_query_bench_make_symbol_value(interp, var_x),
                make_cons(interp,
                    make_cons(interp,
                        deduce_query_bench_make_symbol_value(interp, sym_sum),
                        make_cons(interp,
                            deduce_query_bench_make_symbol_value(interp, var_n),
                            make_nil(interp))),
                    make_nil(interp))));
    Value* rec_edge_body = deduce_query_bench_make_atom2(
        interp,
        edge_sym,
        deduce_query_bench_make_symbol_value(interp, var_x),
        deduce_query_bench_make_symbol_value(interp, var_y)
    );
    Value* rec_chosen_body = deduce_query_bench_make_atom2(
        interp,
        chosen_sym,
        deduce_query_bench_make_symbol_value(interp, var_y),
        deduce_query_bench_make_symbol_value(interp, var_n)
    );
    Value*[4] rec_rule_args;
    rec_rule_args[0] = db;
    rec_rule_args[1] = rec_head;
    rec_rule_args[2] = rec_edge_body;
    rec_rule_args[3] = rec_chosen_body;
    Value* rec_rule_ok = prim_deduce_rule(rec_rule_args[..], null, interp);
    if (rec_rule_ok == null || rec_rule_ok.tag == ERROR) return false;

    for (usz i = 0; i < node_count; i++) {
        Value*[3] unit_args;
        unit_args[0] = unit_rel;
        unit_args[1] = make_int(interp, (long)i);
        unit_args[2] = make_int(interp, 1);
        Value* inserted = prim_assert_fact(unit_args[..], null, interp);
        if (inserted == null || inserted.tag == ERROR) return false;
        if (i + 1 < node_count) {
            Value*[3] edge_args;
            edge_args[0] = edge_rel;
            edge_args[1] = make_int(interp, (long)i);
            edge_args[2] = make_int(interp, (long)(i + 1));
            Value* edge_inserted = prim_assert_fact(edge_args[..], null, interp);
            if (edge_inserted == null || edge_inserted.tag == ERROR) return false;
        }
    }

    *db_out = db;
    *score_rel_out = score_rel;
    *chosen_rel_out = chosen_rel;
    *expected_rows_out = node_count;
    return true;
}

fn bool deduce_query_bench_seed_recursive_mixed_aggregate_db(
    Interp* interp,
    usz node_count,
    Value** db_out,
    Value** score_rel_out,
    Value** chosen_rel_out,
    usz* expected_rows_out
) {
    SymbolId edge_sym = interp.symbols.intern("bench_mixed_edge");
    SymbolId unit_sym = interp.symbols.intern("bench_mixed_unit");
    SymbolId score_sym = interp.symbols.intern("bench_mixed_score");
    SymbolId chosen_sym = interp.symbols.intern("bench_mixed_chosen");
    return deduce_query_bench_seed_recursive_mixed_aggregate_db_named(
        interp,
        node_count,
        edge_sym,
        unit_sym,
        score_sym,
        chosen_sym,
        db_out,
        score_rel_out,
        chosen_rel_out,
        expected_rows_out
    );
}
