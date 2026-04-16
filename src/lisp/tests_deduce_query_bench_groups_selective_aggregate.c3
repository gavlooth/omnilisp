module lisp;

import std::io;

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
    *out_sym_out = (SymbolId)0;
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
        char[] dept_name;
        if (try char[] rendered = io::bprintf(&dept_buf, "dept-%d", (int)bucket)) {
            dept_name = rendered;
        } else {
            return false;
        }
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
