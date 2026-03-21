module lisp;

import std::io;
import std::core::mem;
import main;

fn bool deduce_bench_seed_relation(
    Interp* interp,
    Value** rel_out,
    usz* query_expected_out
) {
    if (interp == null || rel_out == null || query_expected_out == null) return false;
    *rel_out = null;
    *query_expected_out = 0;

    Value*[1] open_args;
    open_args[0] = make_symbol(interp, interp.symbols.intern("memory"));
    Value* db = prim_deduce_open(open_args[..], null, interp);
    if (db == null || db.tag != FFI_HANDLE || db.ffi_val == null) return false;

    SymbolId rel_name = interp.symbols.intern("bench_person");
    SymbolId id_sym = interp.symbols.intern("id");
    SymbolId score_sym = interp.symbols.intern("score");
    Value* rel = setup_deduce_relation_for_db(interp, db, rel_name, id_sym, score_sym);
    if (rel == null || rel.tag != FFI_HANDLE || rel.ffi_val == null) return false;

    usz query_expected = 0;
    for (usz i = 0; i < DEDUCE_BENCH_CORPUS_ROWS; i++) {
        long score = (long)(i % 100);
        if (score > 50) query_expected++;

        Value*[3] fact_args;
        fact_args[0] = rel;
        fact_args[1] = make_int(interp, (long)i);
        fact_args[2] = make_int(interp, score);
        Value* inserted = prim_assert_fact(fact_args[..], null, interp);
        if (inserted == null || inserted.tag == ERROR) return false;
    }

    *rel_out = rel;
    *query_expected_out = query_expected;
    return true;
}

fn Value* deduce_query_bench_make_symbol_value(Interp* interp, SymbolId sym) {
    Value* v = interp.alloc_value();
    v.tag = SYMBOL;
    v.sym_val = sym;
    return v;
}

fn Value* deduce_query_bench_make_atom2(Interp* interp, SymbolId predicate, Value* a, Value* b) {
    Value* pred = deduce_query_bench_make_symbol_value(interp, predicate);
    return make_cons(interp, pred, make_cons(interp, a, make_cons(interp, b, make_nil(interp))));
}

fn Value* deduce_query_bench_make_atom1(Interp* interp, SymbolId predicate, Value* a) {
    Value* pred = deduce_query_bench_make_symbol_value(interp, predicate);
    return make_cons(interp, pred, make_cons(interp, a, make_nil(interp)));
}

fn Value* deduce_query_bench_setup_unary_relation(
    Interp* interp,
    Value* db_val,
    SymbolId rel_name,
    SymbolId col1
) {
    if (interp == null || db_val == null || db_val.tag != FFI_HANDLE || db_val.ffi_val == null) return null;

    Value* name_sym = interp.alloc_value();
    name_sym.tag = SYMBOL;
    name_sym.sym_val = rel_name;

    Value* c1_sym = interp.alloc_value();
    c1_sym.tag = SYMBOL;
    c1_sym.sym_val = col1;

    Value*[3] args;
    args[0] = db_val;
    args[1] = name_sym;
    args[2] = c1_sym;
    return prim_define_relation(args[..], null, interp);
}

fn Value* deduce_query_bench_dict_get(Interp* interp, Value* dict, char[] key) {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return null;
    Value* key_val = make_symbol(interp, interp.symbols.intern(key));
    return hashmap_get(dict.hashmap_val, key_val);
}

fn bool deduce_query_bench_seed_incremental_db(
    Interp* interp,
    Value** db_out,
    Value** edge_out
) {
    if (interp == null || db_out == null || edge_out == null) return false;
    *db_out = null;
    *edge_out = null;

    Value*[1] open_args;
    open_args[0] = make_symbol(interp, interp.symbols.intern("memory"));
    Value* db = prim_deduce_open(open_args[..], null, interp);
    if (db == null || db.tag != FFI_HANDLE || db.ffi_val == null) return false;

    SymbolId edge_sym = interp.symbols.intern("bench_edge");
    SymbolId twohop_sym = interp.symbols.intern("bench_twohop");
    SymbolId from_sym = interp.symbols.intern("from");
    SymbolId to_sym = interp.symbols.intern("to");
    Value* edge_rel = setup_deduce_relation_for_db(interp, db, edge_sym, from_sym, to_sym);
    Value* twohop_rel = setup_deduce_relation_for_db(interp, db, twohop_sym, from_sym, to_sym);
    if (edge_rel == null || edge_rel.tag != FFI_HANDLE || edge_rel.ffi_val == null) return false;
    if (twohop_rel == null || twohop_rel.tag != FFI_HANDLE || twohop_rel.ffi_val == null) return false;

    SymbolId x = interp.symbols.intern("?x");
    SymbolId y = interp.symbols.intern("?y");
    SymbolId z = interp.symbols.intern("?z");

    Value* head = deduce_query_bench_make_atom2(
        interp,
        twohop_sym,
        deduce_query_bench_make_symbol_value(interp, x),
        deduce_query_bench_make_symbol_value(interp, z)
    );
    Value* body0 = deduce_query_bench_make_atom2(
        interp,
        edge_sym,
        deduce_query_bench_make_symbol_value(interp, x),
        deduce_query_bench_make_symbol_value(interp, y)
    );
    Value* body1 = deduce_query_bench_make_atom2(
        interp,
        edge_sym,
        deduce_query_bench_make_symbol_value(interp, y),
        deduce_query_bench_make_symbol_value(interp, z)
    );
    Value*[4] rule_args;
    rule_args[0] = db;
    rule_args[1] = head;
    rule_args[2] = body0;
    rule_args[3] = body1;
    Value* rule_ok = prim_deduce_rule(rule_args[..], null, interp);
    if (rule_ok == null || rule_ok.tag == ERROR) return false;

    for (usz i = 0; i < DEDUCE_BENCH_MUTATION_SEED_ROWS; i++) {
        Value*[3] fact_args;
        fact_args[0] = edge_rel;
        fact_args[1] = make_int(interp, (long)i);
        fact_args[2] = make_int(interp, (long)(i + 1));
        Value* inserted = prim_assert_fact(fact_args[..], null, interp);
        if (inserted == null || inserted.tag == ERROR) return false;
    }

    Value*[1] analyze_args;
    analyze_args[0] = db;
    Value* baseline = prim_deduce_analyze(analyze_args[..], null, interp);
    if (baseline == null || baseline.tag == ERROR) return false;

    *db_out = db;
    *edge_out = edge_rel;
    return true;
}

fn bool deduce_query_bench_run_mutation_lane(
    Interp* interp,
    main::ScopeRegion* bench_scope,
    Value* db,
    Value* edge_rel,
    usz iters,
    bool destructive,
    SymbolId expected_mode_sym,
    long* mutation_ms_out,
    long* analyze_ms_out,
    usz* analyze_attempted_out,
    usz* analyze_ok_out,
    usz* mode_hits_out,
    usz* iter_scope_fail_out
) {
    if (interp == null || bench_scope == null || db == null || edge_rel == null) return false;
    if (mutation_ms_out == null || analyze_ms_out == null ||
        analyze_attempted_out == null || analyze_ok_out == null ||
        mode_hits_out == null || iter_scope_fail_out == null) return false;

    *mutation_ms_out = 0;
    *analyze_ms_out = 0;
    *analyze_attempted_out = 0;
    *analyze_ok_out = 0;
    *mode_hits_out = 0;

    long mutation_ms = 0;
    long analyze_ms = 0;
    usz analyze_attempted = 0;
    usz analyze_ok = 0;
    usz mode_hits = 0;
    SymbolId status_ok_sym = interp.symbols.intern("ok");

    Value*[1] analyze_args;
    analyze_args[0] = db;

    for (usz i = 0; i < iters; i++) {
        main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
        if (iter_scope == null) {
            (*iter_scope_fail_out)++;
            break;
        }
        interp.current_scope = iter_scope;

        long mutation_start = deduce_bench_now_ms();
        bool mutation_ok = false;
        if (destructive) {
            long row_id = (long)(i % DEDUCE_BENCH_MUTATION_SEED_ROWS);
            Value*[3] retract_args;
            retract_args[0] = edge_rel;
            retract_args[1] = make_int(interp, row_id);
            retract_args[2] = make_int(interp, row_id + 1);
            Value* deleted = prim_retract_fact(retract_args[..], null, interp);
            if (deleted != null && deleted.tag != ERROR) {
                Value*[3] fact_args;
                fact_args[0] = edge_rel;
                fact_args[1] = make_int(interp, row_id);
                fact_args[2] = make_int(interp, row_id + 1);
                Value* inserted = prim_assert_fact(fact_args[..], null, interp);
                mutation_ok = inserted != null && inserted.tag != ERROR;
            }
        } else {
            long row_id = (long)(DEDUCE_BENCH_MUTATION_SEED_ROWS + i);
            Value*[3] fact_args;
            fact_args[0] = edge_rel;
            fact_args[1] = make_int(interp, row_id);
            fact_args[2] = make_int(interp, row_id + 1);
            Value* inserted = prim_assert_fact(fact_args[..], null, interp);
            mutation_ok = inserted != null && inserted.tag != ERROR;
        }
        long mutation_done = deduce_bench_now_ms();
        if (mutation_done > mutation_start) {
            mutation_ms += mutation_done - mutation_start;
        }

        if (mutation_ok) {
            analyze_attempted++;
            long analyze_start = deduce_bench_now_ms();
            Value* analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            long analyze_done = deduce_bench_now_ms();
            if (analyze_done > analyze_start) {
                analyze_ms += analyze_done - analyze_start;
            }

            if (analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null) {
                Value* status = deduce_query_bench_dict_get(interp, analyzed, "status");
                Value* mode = deduce_query_bench_dict_get(interp, analyzed, "incremental-invalidation-mode");
                if (status != null && status.tag == SYMBOL && status.sym_val == status_ok_sym) {
                    analyze_ok++;
                }
                if (mode != null && mode.tag == SYMBOL && mode.sym_val == expected_mode_sym) {
                    mode_hits++;
                }
            }
        }

        interp.current_scope = bench_scope;
        main::scope_release(iter_scope);
    }

    interp.current_scope = bench_scope;
    *mutation_ms_out = mutation_ms;
    *analyze_ms_out = analyze_ms;
    *analyze_attempted_out = analyze_attempted;
    *analyze_ok_out = analyze_ok;
    *mode_hits_out = mode_hits;
    return true;
}

fn bool deduce_query_bench_seed_selective_join_db(
    Interp* interp,
    Value** db_out,
    Value** out_rel_out,
    SymbolId* out_sym_out,
    long* expected_rows_out
) {
    if (interp == null || db_out == null || out_rel_out == null || out_sym_out == null || expected_rows_out == null) return false;
    *db_out = null;
    *out_rel_out = null;
    *out_sym_out = 0;
    *expected_rows_out = 0;

    Value*[1] open_args;
    open_args[0] = make_symbol(interp, interp.symbols.intern("memory"));
    Value* db = prim_deduce_open(open_args[..], null, interp);
    if (db == null || db.tag != FFI_HANDLE || db.ffi_val == null) return false;

    SymbolId src_sym = interp.symbols.intern("bench_selective_src");
    SymbolId filter_sym = interp.symbols.intern("bench_selective_filter");
    SymbolId out_sym = interp.symbols.intern("bench_selective_out");
    SymbolId col_a = interp.symbols.intern("a");
    SymbolId col_b = interp.symbols.intern("b");
    Value* src_rel = setup_deduce_relation_for_db(interp, db, src_sym, col_a, col_b);
    Value* filter_rel = setup_deduce_relation_for_db(interp, db, filter_sym, col_a, col_b);
    Value* out_rel = setup_deduce_relation_for_db(interp, db, out_sym, col_a, col_b);
    if (src_rel == null || src_rel.tag != FFI_HANDLE || src_rel.ffi_val == null) return false;
    if (filter_rel == null || filter_rel.tag != FFI_HANDLE || filter_rel.ffi_val == null) return false;
    if (out_rel == null || out_rel.tag != FFI_HANDLE || out_rel.ffi_val == null) return false;

    SymbolId x = interp.symbols.intern("?x");
    SymbolId y = interp.symbols.intern("?y");
    Value* head = deduce_query_bench_make_atom2(
        interp,
        out_sym,
        deduce_query_bench_make_symbol_value(interp, x),
        deduce_query_bench_make_symbol_value(interp, y)
    );
    Value* body_src = deduce_query_bench_make_atom2(
        interp,
        src_sym,
        deduce_query_bench_make_symbol_value(interp, x),
        deduce_query_bench_make_symbol_value(interp, y)
    );
    Value* body_filter = deduce_query_bench_make_atom2(
        interp,
        filter_sym,
        make_int(interp, 0),
        deduce_query_bench_make_symbol_value(interp, y)
    );
    Value*[4] rule_args;
    rule_args[0] = db;
    rule_args[1] = head;
    rule_args[2] = body_src;
    rule_args[3] = body_filter;
    Value* rule_ok = prim_deduce_rule(rule_args[..], null, interp);
    if (rule_ok == null || rule_ok.tag == ERROR) return false;

    for (usz i = 0; i < DEDUCE_BENCH_SELECTIVE_FILTER_ROWS; i++) {
        Value*[3] fact_args;
        fact_args[0] = filter_rel;
        fact_args[1] = make_int(interp, 0);
        fact_args[2] = make_int(interp, (long)i);
        Value* inserted = prim_assert_fact(fact_args[..], null, interp);
        if (inserted == null || inserted.tag == ERROR) return false;
    }

    long expected_rows = 0;
    for (usz i = 0; i < DEDUCE_BENCH_SELECTIVE_SRC_ROWS; i++) {
        long bucket = (long)(i % 512);
        if (bucket < (long)DEDUCE_BENCH_SELECTIVE_FILTER_ROWS) expected_rows++;

        Value*[3] fact_args;
        fact_args[0] = src_rel;
        fact_args[1] = make_int(interp, (long)i);
        fact_args[2] = make_int(interp, bucket);
        Value* inserted = prim_assert_fact(fact_args[..], null, interp);
        if (inserted == null || inserted.tag == ERROR) return false;
    }

    *db_out = db;
    *out_rel_out = out_rel;
    *out_sym_out = out_sym;
    *expected_rows_out = expected_rows;
    return true;
}

fn bool deduce_query_bench_seed_aggregate_db(
    Interp* interp,
    Value** db_out,
    Value** out_rel_out,
    long* expected_groups_out
) {
    if (interp == null || db_out == null || out_rel_out == null || expected_groups_out == null) return false;
    *db_out = null;
    *out_rel_out = null;
    *expected_groups_out = 0;

    Value*[1] open_args;
    open_args[0] = make_symbol(interp, interp.symbols.intern("memory"));
    Value* db = prim_deduce_open(open_args[..], null, interp);
    if (db == null || db.tag != FFI_HANDLE || db.ffi_val == null) return false;

    SymbolId src_sym = interp.symbols.intern("bench_agg_src");
    SymbolId out_sym = interp.symbols.intern("bench_agg_sum");
    SymbolId dept_sym = interp.symbols.intern("dept");
    SymbolId amount_sym = interp.symbols.intern("amount");
    SymbolId total_sym = interp.symbols.intern("total");
    Value* src_rel = setup_deduce_relation_for_db(interp, db, src_sym, dept_sym, amount_sym);
    Value* out_rel = setup_deduce_relation_for_db(interp, db, out_sym, dept_sym, total_sym);
    if (src_rel == null || src_rel.tag != FFI_HANDLE || src_rel.ffi_val == null) return false;
    if (out_rel == null || out_rel.tag != FFI_HANDLE || out_rel.ffi_val == null) return false;

    SymbolId dept_var = interp.symbols.intern("?dept");
    SymbolId amount_var = interp.symbols.intern("?amount");
    SymbolId sym_sum = interp.symbols.intern("sum");
    Value* head =
        make_cons(interp,
            deduce_query_bench_make_symbol_value(interp, out_sym),
            make_cons(interp,
                deduce_query_bench_make_symbol_value(interp, dept_var),
                make_cons(interp,
                    make_cons(interp,
                        deduce_query_bench_make_symbol_value(interp, sym_sum),
                        make_cons(interp,
                            deduce_query_bench_make_symbol_value(interp, amount_var),
                            make_nil(interp))),
                    make_nil(interp))));
    Value* body = deduce_query_bench_make_atom2(
        interp,
        src_sym,
        deduce_query_bench_make_symbol_value(interp, dept_var),
        deduce_query_bench_make_symbol_value(interp, amount_var)
    );
    Value*[3] rule_args;
    rule_args[0] = db;
    rule_args[1] = head;
    rule_args[2] = body;
    Value* rule_ok = prim_deduce_rule(rule_args[..], null, interp);
    if (rule_ok == null || rule_ok.tag == ERROR) return false;

    const long GROUP_COUNT = 32;
    for (usz i = 0; i < DEDUCE_BENCH_CORPUS_ROWS; i++) {
        long bucket = (long)(i % GROUP_COUNT);
        char[24] dept_buf;
        char[] dept_name = io::bprintf(&dept_buf, "dept-%d", (int)bucket)!!;
        Value*[3] fact_args;
        fact_args[0] = src_rel;
        fact_args[1] = make_string(interp, dept_name);
        fact_args[2] = make_int(interp, (long)(1 + (i % 13)));
        Value* inserted = prim_assert_fact(fact_args[..], null, interp);
        if (inserted == null || inserted.tag == ERROR) return false;
    }

    *db_out = db;
    *out_rel_out = out_rel;
    *expected_groups_out = GROUP_COUNT;
    return true;
}

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

fn bool deduce_query_bench_seed_recursive_aggregate_negated_db(
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

    SymbolId edge_sym = interp.symbols.intern("bench_recursive_neg_edge");
    SymbolId unit_sym = interp.symbols.intern("bench_recursive_neg_unit");
    SymbolId banned_sym = interp.symbols.intern("bench_recursive_neg_banned");
    SymbolId total_sym = interp.symbols.intern("bench_recursive_neg_total");
    SymbolId col_node = interp.symbols.intern("node");
    SymbolId col_dst = interp.symbols.intern("dst");
    SymbolId col_weight = interp.symbols.intern("weight");
    SymbolId col_total = interp.symbols.intern("total");
    Value* edge_rel = setup_deduce_relation_for_db(interp, db, edge_sym, col_node, col_dst);
    Value* unit_rel = setup_deduce_relation_for_db(interp, db, unit_sym, col_node, col_weight);
    Value* banned_rel = deduce_query_bench_setup_unary_relation(interp, db, banned_sym, col_node);
    Value* out_rel = setup_deduce_relation_for_db(interp, db, total_sym, col_node, col_total);
    if (edge_rel == null || edge_rel.tag != FFI_HANDLE || edge_rel.ffi_val == null) return false;
    if (unit_rel == null || unit_rel.tag != FFI_HANDLE || unit_rel.ffi_val == null) return false;
    if (banned_rel == null || banned_rel.tag != FFI_HANDLE || banned_rel.ffi_val == null) return false;
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
    Value* base_not_banned =
        make_cons(interp,
            deduce_query_bench_make_symbol_value(interp, interp.symbols.intern("not")),
            make_cons(interp,
                deduce_query_bench_make_atom1(
                    interp,
                    banned_sym,
                    deduce_query_bench_make_symbol_value(interp, var_x)
                ),
                make_nil(interp)));
    Value*[4] base_rule_args;
    base_rule_args[0] = db;
    base_rule_args[1] = base_head;
    base_rule_args[2] = base_body;
    base_rule_args[3] = base_not_banned;
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
    Value* rec_not_banned =
        make_cons(interp,
            deduce_query_bench_make_symbol_value(interp, interp.symbols.intern("not")),
            make_cons(interp,
                deduce_query_bench_make_atom1(
                    interp,
                    banned_sym,
                    deduce_query_bench_make_symbol_value(interp, var_x)
                ),
                make_nil(interp)));
    Value*[5] rec_rule_args;
    rec_rule_args[0] = db;
    rec_rule_args[1] = rec_head;
    rec_rule_args[2] = rec_edge_body;
    rec_rule_args[3] = rec_total_body;
    rec_rule_args[4] = rec_not_banned;
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

    Value*[2] banned_args;
    banned_args[0] = banned_rel;
    banned_args[1] = make_int(interp, (long)(node_count - 1));
    Value* banned_inserted = prim_assert_fact(banned_args[..], null, interp);
    if (banned_inserted == null || banned_inserted.tag == ERROR) return false;

    *db_out = db;
    *out_rel_out = out_rel;
    *expected_rows_out = node_count - 1;
    return true;
}

fn bool deduce_query_bench_seed_recursive_mixed_aggregate_negated_db_named(
    Interp* interp,
    usz node_count,
    SymbolId edge_sym,
    SymbolId unit_sym,
    SymbolId blocked_sym,
    SymbolId score_sym,
    SymbolId chosen_sym,
    Value** db_out,
    Value** score_rel_out,
    Value** chosen_rel_out,
    usz* score_rows_out,
    usz* chosen_rows_out
) {
    if (interp == null || db_out == null || score_rel_out == null || chosen_rel_out == null ||
        score_rows_out == null || chosen_rows_out == null || node_count < 2) {
        return false;
    }
    *db_out = null;
    *score_rel_out = null;
    *chosen_rel_out = null;
    *score_rows_out = 0;
    *chosen_rows_out = 0;

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
    Value* blocked_rel = deduce_query_bench_setup_unary_relation(interp, db, blocked_sym, col_node);
    Value* score_rel = setup_deduce_relation_for_db(interp, db, score_sym, col_node, col_total);
    Value* chosen_rel = setup_deduce_relation_for_db(interp, db, chosen_sym, col_node, col_total);
    if (edge_rel == null || edge_rel.tag != FFI_HANDLE || edge_rel.ffi_val == null) return false;
    if (unit_rel == null || unit_rel.tag != FFI_HANDLE || unit_rel.ffi_val == null) return false;
    if (blocked_rel == null || blocked_rel.tag != FFI_HANDLE || blocked_rel.ffi_val == null) return false;
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
    Value* chosen_not_blocked =
        make_cons(interp,
            deduce_query_bench_make_symbol_value(interp, interp.symbols.intern("not")),
            make_cons(interp,
                deduce_query_bench_make_atom1(
                    interp,
                    blocked_sym,
                    deduce_query_bench_make_symbol_value(interp, var_x)
                ),
                make_nil(interp)));
    Value*[4] chosen_rule_args;
    chosen_rule_args[0] = db;
    chosen_rule_args[1] = chosen_head;
    chosen_rule_args[2] = chosen_body;
    chosen_rule_args[3] = chosen_not_blocked;
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

    Value*[2] blocked_args;
    blocked_args[0] = blocked_rel;
    blocked_args[1] = make_int(interp, (long)(node_count - 1));
    Value* blocked_inserted = prim_assert_fact(blocked_args[..], null, interp);
    if (blocked_inserted == null || blocked_inserted.tag == ERROR) return false;

    *db_out = db;
    *score_rel_out = score_rel;
    *chosen_rel_out = chosen_rel;
    *score_rows_out = node_count;
    *chosen_rows_out = node_count - 1;
    return true;
}

fn bool deduce_query_bench_seed_recursive_mixed_aggregate_negated_db(
    Interp* interp,
    usz node_count,
    Value** db_out,
    Value** score_rel_out,
    Value** chosen_rel_out,
    usz* score_rows_out,
    usz* chosen_rows_out
) {
    SymbolId edge_sym = interp.symbols.intern("bench_mixed_neg_edge");
    SymbolId unit_sym = interp.symbols.intern("bench_mixed_neg_unit");
    SymbolId blocked_sym = interp.symbols.intern("bench_mixed_neg_blocked");
    SymbolId score_sym = interp.symbols.intern("bench_mixed_neg_score");
    SymbolId chosen_sym = interp.symbols.intern("bench_mixed_neg_chosen");
    return deduce_query_bench_seed_recursive_mixed_aggregate_negated_db_named(
        interp,
        node_count,
        edge_sym,
        unit_sym,
        blocked_sym,
        score_sym,
        chosen_sym,
        db_out,
        score_rel_out,
        chosen_rel_out,
        score_rows_out,
        chosen_rows_out
    );
}

fn bool deduce_query_bench_assert_selective_explain(
    Interp* interp,
    Value* explained,
    long* step0_rows_read_out,
    long* step1_rows_read_out
) {
    if (interp == null || explained == null || explained.tag != HASHMAP || explained.hashmap_val == null) return false;
    Value* steps = deduce_query_bench_dict_get(interp, explained, "steps");
    if (!is_cons(steps) || !is_cons(cdr(steps)) || !is_nil(cdr(cdr(steps)))) return false;

    SymbolId index_scan_sym = interp.symbols.intern("index-scan");
    SymbolId observed_sym = interp.symbols.intern("observed");
    Value* step0 = car(steps);
    Value* step1 = car(cdr(steps));
    Value* step0_op = deduce_query_bench_dict_get(interp, step0, "operator");
    Value* step1_op = deduce_query_bench_dict_get(interp, step1, "operator");
    Value* step0_counters = deduce_query_bench_dict_get(interp, step0, "counters");
    Value* step1_counters = deduce_query_bench_dict_get(interp, step1, "counters");
    Value* step0_rows_read = deduce_query_bench_dict_get(interp, step0_counters, "rows-read");
    Value* step1_rows_read = deduce_query_bench_dict_get(interp, step1_counters, "rows-read");
    Value* step0_counter_kind = deduce_query_bench_dict_get(interp, step0_counters, "counter-kind");
    Value* step1_counter_kind = deduce_query_bench_dict_get(interp, step1_counters, "counter-kind");

    bool ok = step0_op != null && step0_op.tag == SYMBOL && step0_op.sym_val == index_scan_sym &&
        step1_op != null && step1_op.tag == SYMBOL && step1_op.sym_val == index_scan_sym &&
        step0_rows_read != null && step0_rows_read.tag == INT &&
        step1_rows_read != null && step1_rows_read.tag == INT &&
        step0_counter_kind != null && step0_counter_kind.tag == SYMBOL && step0_counter_kind.sym_val == observed_sym &&
        step1_counter_kind != null && step1_counter_kind.tag == SYMBOL && step1_counter_kind.sym_val == observed_sym &&
        step0_rows_read.int_val <= step1_rows_read.int_val;
    if (!ok) return false;

    if (step0_rows_read_out != null) *step0_rows_read_out = step0_rows_read.int_val;
    if (step1_rows_read_out != null) *step1_rows_read_out = step1_rows_read.int_val;
    return true;
}

fn bool deduce_query_bench_assert_explain_counters_present(
    Interp* interp,
    Value* explained,
    long* step0_rows_read_out,
    long* step1_rows_read_out
) {
    if (interp == null || explained == null || explained.tag != HASHMAP || explained.hashmap_val == null) return false;
    Value* steps = deduce_query_bench_dict_get(interp, explained, "steps");
    if (!is_cons(steps) || !is_cons(cdr(steps)) || !is_nil(cdr(cdr(steps)))) return false;

    SymbolId observed_sym = interp.symbols.intern("observed");
    Value* step0 = car(steps);
    Value* step1 = car(cdr(steps));
    Value* step0_counters = deduce_query_bench_dict_get(interp, step0, "counters");
    Value* step1_counters = deduce_query_bench_dict_get(interp, step1, "counters");
    Value* step0_rows_read = deduce_query_bench_dict_get(interp, step0_counters, "rows-read");
    Value* step1_rows_read = deduce_query_bench_dict_get(interp, step1_counters, "rows-read");
    Value* step0_rows_emitted = deduce_query_bench_dict_get(interp, step0_counters, "rows-emitted");
    Value* step1_rows_emitted = deduce_query_bench_dict_get(interp, step1_counters, "rows-emitted");
    Value* step0_index_hits = deduce_query_bench_dict_get(interp, step0_counters, "index-hits");
    Value* step1_index_hits = deduce_query_bench_dict_get(interp, step1_counters, "index-hits");
    Value* step0_index_misses = deduce_query_bench_dict_get(interp, step0_counters, "index-misses");
    Value* step1_index_misses = deduce_query_bench_dict_get(interp, step1_counters, "index-misses");
    Value* step0_join_probes = deduce_query_bench_dict_get(interp, step0_counters, "join-probes");
    Value* step1_join_probes = deduce_query_bench_dict_get(interp, step1_counters, "join-probes");
    Value* step0_counter_kind = deduce_query_bench_dict_get(interp, step0_counters, "counter-kind");
    Value* step1_counter_kind = deduce_query_bench_dict_get(interp, step1_counters, "counter-kind");

    bool ok = step0_rows_read != null && step0_rows_read.tag == INT &&
        step1_rows_read != null && step1_rows_read.tag == INT &&
        step0_rows_emitted != null && step0_rows_emitted.tag == INT &&
        step1_rows_emitted != null && step1_rows_emitted.tag == INT &&
        step0_index_hits != null && step0_index_hits.tag == INT &&
        step1_index_hits != null && step1_index_hits.tag == INT &&
        step0_index_misses != null && step0_index_misses.tag == INT &&
        step1_index_misses != null && step1_index_misses.tag == INT &&
        step0_join_probes != null && step0_join_probes.tag == INT &&
        step1_join_probes != null && step1_join_probes.tag == INT &&
        step0_counter_kind != null && step0_counter_kind.tag == SYMBOL && step0_counter_kind.sym_val == observed_sym &&
        step1_counter_kind != null && step1_counter_kind.tag == SYMBOL && step1_counter_kind.sym_val == observed_sym;
    if (!ok) return false;

    if (step0_rows_read_out != null) *step0_rows_read_out = step0_rows_read.int_val;
    if (step1_rows_read_out != null) *step1_rows_read_out = step1_rows_read.int_val;
    return true;
}
