module lisp;


import std::core::mem;
import std::io;
import main;

const usz DEDUCE_BENCH_CORPUS_ROWS = 4096;
const usz DEDUCE_BENCH_SCAN_ITERS = 16;
const usz DEDUCE_BENCH_SCAN_RANGE_ITERS = 16;
const usz DEDUCE_BENCH_QUERY_ITERS = 16;
const usz DEDUCE_BENCH_COUNT_ITERS = 4096;
const usz DEDUCE_BENCH_MUTATION_SEED_ROWS = 1024;
const usz DEDUCE_BENCH_MUTATION_TRACKED_ITERS = 128;
const usz DEDUCE_BENCH_MUTATION_FULL_ITERS = 128;
const usz DEDUCE_BENCH_SELECTIVE_SRC_ROWS = 4096;
const usz DEDUCE_BENCH_SELECTIVE_FILTER_ROWS = 32;
const usz DEDUCE_BENCH_SELECTIVE_EXPLAIN_ITERS = 256;
const usz DEDUCE_BENCH_SELECTIVE_ANALYZE_ITERS = 64;
const usz DEDUCE_BENCH_RECURSIVE_CHAIN_EDGES = 128;
const usz DEDUCE_BENCH_RECURSIVE_NAIVE_ITERS = 16;
const usz DEDUCE_BENCH_RECURSIVE_SEMI_ITERS = 16;
const usz DEDUCE_BENCH_SKEW_LEFT_ROWS = 4096;
const usz DEDUCE_BENCH_SKEW_HEAVY_ROWS = 3584;
const usz DEDUCE_BENCH_SKEW_RARE_KEYS = 64;
const usz DEDUCE_BENCH_SKEW_EXPLAIN_ITERS = 128;
const usz DEDUCE_BENCH_SKEW_ANALYZE_ITERS = 64;

fn void run_deduce_match_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run("(length (deduce 'match person '(person ?name ?age)))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 1) {
            emit_pass_literal("[PASS] deduce 'match all");
            (*pass)++;
        } else {
            io::printfn("[FAIL] deduce 'match all (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }

    {
        EvalResult r = run("('name (car (deduce 'match person '(person ?name ?age))))", interp);
        if (!r.error.has_error && is_string(r.value)) {
            emit_pass_literal("[PASS] deduce 'match ?var binding");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'match ?var binding");
            (*fail)++;
        }
    }

    {
        EvalResult open = run("(deduce 'open 'memory)", interp);
        if (open.error.has_error || open.value == null || open.value.tag != FFI_HANDLE || open.value.ffi_val == null) {
            io::printn("[FAIL] deduce 'match preserves >512 results (open)");
            (*fail)++;
            return;
        }

        SymbolId rel_name = interp.symbols.intern("crowd");
        SymbolId id_sym = interp.symbols.intern("id");
        SymbolId age_sym = interp.symbols.intern("age");
        Value* rel = setup_deduce_relation_for_db(interp, open.value, rel_name, id_sym, age_sym);
        if (rel == null || rel.tag != FFI_HANDLE || rel.ffi_val == null) {
            io::printn("[FAIL] deduce 'match preserves >512 results (define)");
            (*fail)++;
            return;
        }

        const usz ROW_COUNT = 600;
        for (usz i = 0; i < ROW_COUNT; i++) {
            Value*[3] fact_args;
            fact_args[0] = rel;
            fact_args[1] = make_int(interp, (long)i);
            fact_args[2] = make_int(interp, (long)(20 + (int)(i % 50)));
            Value* insert = prim_assert_fact(fact_args[..], null, interp);
            if (insert == null || insert.tag == ERROR) {
                io::printfn("[FAIL] deduce 'match preserves >512 results (insert %d)", (int)i);
                (*fail)++;
                return;
            }
        }

        Value* clause =
            make_cons(interp,
                make_symbol(interp, rel_name),
                make_cons(interp,
                    make_symbol(interp, interp.symbols.intern("?id")),
                    make_cons(interp,
                        make_symbol(interp, interp.symbols.intern("?age")),
                        make_nil(interp))));
        Value*[2] match_args;
        match_args[0] = rel;
        match_args[1] = clause;
        Value* matches = prim_deduce_match(match_args[..], null, interp);

        usz match_count = 0;
        bool found_tail = false;
        Value* cur = matches;
        while (is_cons(cur)) {
            Value* row = car(cur);
            if (row != null && row.tag == HASHMAP && row.hashmap_val != null) {
                Value* id_val = hashmap_get(row.hashmap_val, make_symbol(interp, id_sym));
                if (id_val != null && id_val.tag == INT && id_val.int_val == 599) found_tail = true;
            }
            match_count++;
            cur = cdr(cur);
        }

        if (match_count == ROW_COUNT && found_tail) {
            emit_pass_literal("[PASS] deduce 'match preserves >512 results");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] deduce 'match preserves >512 results (count=%d tail=%s)",
                (int)match_count,
                found_tail ? "yes" : "no"
            );
            (*fail)++;
        }
    }

    {
        EvalResult open = run("(deduce 'open 'memory)", interp);
        if (open.error.has_error || open.value == null || open.value.tag != FFI_HANDLE || open.value.ffi_val == null) {
            io::printn("[FAIL] deduce 'match preserves >32 bindings (open)");
            (*fail)++;
            return;
        }

        const usz COL_COUNT = 40;
        SymbolId[] col_syms = mem::new_array(SymbolId, COL_COUNT);
        SymbolId[] var_syms = mem::new_array(SymbolId, COL_COUNT);
        SymbolId[] key_syms = mem::new_array(SymbolId, COL_COUNT);
        Value*[] define_args = mem::new_array(Value*, COL_COUNT + 2);
        Value*[] fact_args = mem::new_array(Value*, COL_COUNT + 1);
        if (col_syms.ptr == null || var_syms.ptr == null || key_syms.ptr == null ||
            define_args.ptr == null || fact_args.ptr == null) {
            if (col_syms.ptr != null) mem::free(col_syms.ptr);
            if (var_syms.ptr != null) mem::free(var_syms.ptr);
            if (key_syms.ptr != null) mem::free(key_syms.ptr);
            if (define_args.ptr != null) mem::free(define_args.ptr);
            if (fact_args.ptr != null) mem::free(fact_args.ptr);
            io::printn("[FAIL] deduce 'match preserves >32 bindings (alloc)");
            (*fail)++;
            return;
        }
        defer mem::free(col_syms.ptr);
        defer mem::free(var_syms.ptr);
        defer mem::free(key_syms.ptr);
        defer mem::free(define_args.ptr);
        defer mem::free(fact_args.ptr);

        SymbolId rel_name = interp.symbols.intern("binding_wide");
        define_args[0] = open.value;
        define_args[1] = make_symbol(interp, rel_name);
        fact_args[0] = null;

        for (usz i = 0; i < COL_COUNT; i++) {
            char[16] col_buf;
            char[] col_name = io::bprintf(&col_buf, "c%d", (int)i)!!;
            col_syms[i] = interp.symbols.intern(col_name);
            define_args[i + 2] = make_symbol(interp, col_syms[i]);

            char[16] var_buf;
            char[] var_name = io::bprintf(&var_buf, "?v%d", (int)i)!!;
            var_syms[i] = interp.symbols.intern(var_name);
            key_syms[i] = interp.symbols.intern(var_name[1..]);

            fact_args[i + 1] = make_int(interp, (long)i);
        }

        Value* rel = prim_define_relation(define_args, null, interp);
        if (rel == null || rel.tag != FFI_HANDLE || rel.ffi_val == null) {
            io::printn("[FAIL] deduce 'match preserves >32 bindings (define)");
            (*fail)++;
            return;
        }
        fact_args[0] = rel;

        Value* insert = prim_assert_fact(fact_args, null, interp);
        if (insert == null || insert.tag == ERROR) {
            io::printn("[FAIL] deduce 'match preserves >32 bindings (insert)");
            (*fail)++;
            return;
        }

        Value* clause = make_nil(interp);
        for (usz i = COL_COUNT; i > 0; i--) {
            clause = make_cons(interp, make_symbol(interp, var_syms[i - 1]), clause);
        }
        clause = make_cons(interp, make_symbol(interp, rel_name), clause);

        Value*[2] match_args;
        match_args[0] = rel;
        match_args[1] = clause;
        Value* matches = prim_deduce_match(match_args[..], null, interp);

        Value* row = is_cons(matches) ? car(matches) : null;
        Value* last = row != null && row.tag == HASHMAP
            ? hashmap_get(row.hashmap_val, make_symbol(interp, key_syms[COL_COUNT - 1]))
            : null;

        bool ok = is_cons(matches) &&
                  row != null &&
                  row.tag == HASHMAP &&
                  row.hashmap_val != null &&
                  row.hashmap_val.count == COL_COUNT &&
                  last != null &&
                  last.tag == INT &&
                  last.int_val == (long)(COL_COUNT - 1);

        if (ok) {
            emit_pass_literal("[PASS] deduce 'match preserves >32 bindings");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] deduce 'match preserves >32 bindings (count=%d last=%d)",
                row != null && row.tag == HASHMAP && row.hashmap_val != null ? (int)row.hashmap_val.count : -1,
                last != null && last.tag == INT ? (int)last.int_val : -1
            );
            (*fail)++;
        }
    }
}

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
    SymbolId estimated_sym = interp.symbols.intern("estimated");
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
        step0_counter_kind != null && step0_counter_kind.tag == SYMBOL && step0_counter_kind.sym_val == estimated_sym &&
        step1_counter_kind != null && step1_counter_kind.tag == SYMBOL && step1_counter_kind.sym_val == estimated_sym &&
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

    SymbolId estimated_sym = interp.symbols.intern("estimated");
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
        step0_counter_kind != null && step0_counter_kind.tag == SYMBOL && step0_counter_kind.sym_val == estimated_sym &&
        step1_counter_kind != null && step1_counter_kind.tag == SYMBOL && step1_counter_kind.sym_val == estimated_sym;
    if (!ok) return false;

    if (step0_rows_read_out != null) *step0_rows_read_out = step0_rows_read.int_val;
    if (step1_rows_read_out != null) *step1_rows_read_out = step1_rows_read.int_val;
    return true;
}

fn void run_deduce_selective_join_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled()) return;
    if (interp == null) return;

    io::printn("\n--- Deduce Selective Join Benchmarks ---");

    main::ScopeRegion* bench_scope = interp.current_scope;
    Value* db = null;
    Value* out_rel = null;
    SymbolId out_sym = 0;
    long expected_rows = 0;
    bool seeded = deduce_query_bench_seed_selective_join_db(interp, &db, &out_rel, &out_sym, &expected_rows);

    long explain_ms = 0;
    long analyze_ms = 0;
    usz explain_attempted = 0;
    usz explain_ok = 0;
    usz analyze_ok = 0;
    usz iter_scope_fail = 0;
    long step0_rows_read = -1;
    long step1_rows_read = -1;

    if (seeded) {
        Value*[2] explain_args;
        explain_args[0] = db;
        explain_args[1] = make_symbol(interp, out_sym);
        long explain_start = deduce_bench_now_ms();
        for (usz i = 0; i < DEDUCE_BENCH_SELECTIVE_EXPLAIN_ITERS; i++) {
            main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
            if (iter_scope == null) {
                iter_scope_fail++;
                break;
            }
            interp.current_scope = iter_scope;

            Value* explained = prim_deduce_explain(explain_args[..], null, interp);
            if (explained != null && explained.tag == HASHMAP && explained.hashmap_val != null) {
                explain_attempted++;
                long rows0 = -1;
                long rows1 = -1;
                if (deduce_query_bench_assert_selective_explain(interp, explained, &rows0, &rows1)) {
                    explain_ok++;
                    step0_rows_read = rows0;
                    step1_rows_read = rows1;
                }
            }

            interp.current_scope = bench_scope;
            main::scope_release(iter_scope);
        }
        long explain_done = deduce_bench_now_ms();
        explain_ms = explain_done > explain_start ? explain_done - explain_start : 0;

        Value*[1] analyze_args;
        analyze_args[0] = db;
        Value*[1] count_args;
        count_args[0] = out_rel;
        SymbolId status_ok_sym = interp.symbols.intern("ok");
        long analyze_start = deduce_bench_now_ms();
        for (usz i = 0; i < DEDUCE_BENCH_SELECTIVE_ANALYZE_ITERS; i++) {
            main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
            if (iter_scope == null) {
                iter_scope_fail++;
                break;
            }
            interp.current_scope = iter_scope;

            Value* analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            Value* count = prim_deduce_count(count_args[..], null, interp);
            Value* status = deduce_query_bench_dict_get(interp, analyzed, "status");
            if (analyzed != null &&
                analyzed.tag == HASHMAP &&
                analyzed.hashmap_val != null &&
                status != null &&
                status.tag == SYMBOL &&
                status.sym_val == status_ok_sym &&
                count != null &&
                count.tag == INT &&
                count.int_val == expected_rows) {
                analyze_ok++;
            }

            interp.current_scope = bench_scope;
            main::scope_release(iter_scope);
        }
        long analyze_done = deduce_bench_now_ms();
        analyze_ms = analyze_done > analyze_start ? analyze_done - analyze_start : 0;
    }

    usz explain_mode_miss = explain_attempted >= explain_ok
        ? explain_attempted - explain_ok
        : 0;

    interp.current_scope = bench_scope;
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=deduce_selective_join src_rows=%d filter_rows=%d explain_iters=%d analyze_iters=%d seeded_ok=%d expected_rows=%d explain_ms=%d explain_attempted=%d explain_ok=%d explain_mode_miss=%d step0_rows_read=%d step1_rows_read=%d analyze_ms=%d analyze_ok=%d iter_scope_fail=%d",
        (long)DEDUCE_BENCH_SELECTIVE_SRC_ROWS,
        (long)DEDUCE_BENCH_SELECTIVE_FILTER_ROWS,
        (long)DEDUCE_BENCH_SELECTIVE_EXPLAIN_ITERS,
        (long)DEDUCE_BENCH_SELECTIVE_ANALYZE_ITERS,
        seeded ? 1 : 0,
        expected_rows,
        explain_ms,
        (long)explain_attempted,
        (long)explain_ok,
        (long)explain_mode_miss,
        step0_rows_read,
        step1_rows_read,
        analyze_ms,
        (long)analyze_ok,
        (long)iter_scope_fail
    );
}

fn bool deduce_query_bench_seed_recursive_closure_db(
    Interp* interp,
    Value** db_out,
    Value** ancestor_rel_out,
    long* expected_rows_out
) {
    if (interp == null || db_out == null || ancestor_rel_out == null || expected_rows_out == null) return false;
    *db_out = null;
    *ancestor_rel_out = null;
    *expected_rows_out = 0;

    Value*[1] open_args;
    open_args[0] = make_symbol(interp, interp.symbols.intern("memory"));
    Value* db = prim_deduce_open(open_args[..], null, interp);
    if (db == null || db.tag != FFI_HANDLE || db.ffi_val == null) return false;

    SymbolId parent_sym = interp.symbols.intern("bench_parent_tc");
    SymbolId ancestor_sym = interp.symbols.intern("bench_ancestor_tc");
    SymbolId src_sym = interp.symbols.intern("src");
    SymbolId dst_sym = interp.symbols.intern("dst");
    Value* parent_rel = setup_deduce_relation_for_db(interp, db, parent_sym, src_sym, dst_sym);
    Value* ancestor_rel = setup_deduce_relation_for_db(interp, db, ancestor_sym, src_sym, dst_sym);
    if (parent_rel == null || parent_rel.tag != FFI_HANDLE || parent_rel.ffi_val == null) return false;
    if (ancestor_rel == null || ancestor_rel.tag != FFI_HANDLE || ancestor_rel.ffi_val == null) return false;

    for (usz i = 0; i < DEDUCE_BENCH_RECURSIVE_CHAIN_EDGES; i++) {
        Value*[3] fact_args;
        fact_args[0] = parent_rel;
        fact_args[1] = make_int(interp, (long)i);
        fact_args[2] = make_int(interp, (long)(i + 1));
        Value* inserted = prim_assert_fact(fact_args[..], null, interp);
        if (inserted == null || inserted.tag == ERROR) return false;
    }

    SymbolId x = interp.symbols.intern("?x");
    SymbolId y = interp.symbols.intern("?y");
    SymbolId z = interp.symbols.intern("?z");
    Value* head0 = deduce_query_bench_make_atom2(
        interp,
        ancestor_sym,
        deduce_query_bench_make_symbol_value(interp, x),
        deduce_query_bench_make_symbol_value(interp, y)
    );
    Value* body0 = deduce_query_bench_make_atom2(
        interp,
        parent_sym,
        deduce_query_bench_make_symbol_value(interp, x),
        deduce_query_bench_make_symbol_value(interp, y)
    );
    Value*[3] rule0_args;
    rule0_args[0] = db;
    rule0_args[1] = head0;
    rule0_args[2] = body0;
    Value* rule0_ok = prim_deduce_rule(rule0_args[..], null, interp);
    if (rule0_ok == null || rule0_ok.tag == ERROR) return false;

    Value* head1 = deduce_query_bench_make_atom2(
        interp,
        ancestor_sym,
        deduce_query_bench_make_symbol_value(interp, x),
        deduce_query_bench_make_symbol_value(interp, z)
    );
    Value* body1a = deduce_query_bench_make_atom2(
        interp,
        parent_sym,
        deduce_query_bench_make_symbol_value(interp, x),
        deduce_query_bench_make_symbol_value(interp, y)
    );
    Value* body1b = deduce_query_bench_make_atom2(
        interp,
        ancestor_sym,
        deduce_query_bench_make_symbol_value(interp, y),
        deduce_query_bench_make_symbol_value(interp, z)
    );
    Value*[4] rule1_args;
    rule1_args[0] = db;
    rule1_args[1] = head1;
    rule1_args[2] = body1a;
    rule1_args[3] = body1b;
    Value* rule1_ok = prim_deduce_rule(rule1_args[..], null, interp);
    if (rule1_ok == null || rule1_ok.tag == ERROR) return false;

    long edges = (long)DEDUCE_BENCH_RECURSIVE_CHAIN_EDGES;
    long expected_rows = edges * (edges + 1) / 2;
    *db_out = db;
    *ancestor_rel_out = ancestor_rel;
    *expected_rows_out = expected_rows;
    return true;
}

fn bool deduce_query_bench_run_recursive_engine_lane(
    Interp* interp,
    main::ScopeRegion* bench_scope,
    Value* db,
    Value* ancestor_rel,
    SymbolId requested_engine,
    SymbolId expected_execution_engine,
    usz iters,
    long expected_rows,
    long* elapsed_ms_out,
    usz* attempted_out,
    usz* ok_out,
    usz* engine_hit_out,
    usz* iter_scope_fail_out
) {
    if (interp == null || bench_scope == null || db == null || ancestor_rel == null) return false;
    if (elapsed_ms_out == null || attempted_out == null || ok_out == null || engine_hit_out == null || iter_scope_fail_out == null) return false;

    *elapsed_ms_out = 0;
    *attempted_out = 0;
    *ok_out = 0;
    *engine_hit_out = 0;

    SymbolId status_ok_sym = interp.symbols.intern("ok");
    Value* engine_arg = make_symbol(interp, requested_engine);
    Value*[2] analyze_args;
    analyze_args[0] = db;
    analyze_args[1] = engine_arg;
    Value*[1] count_args;
    count_args[0] = ancestor_rel;

    long elapsed_ms = 0;
    usz attempted = 0;
    usz ok = 0;
    usz engine_hit = 0;

    long lane_start = deduce_bench_now_ms();
    for (usz i = 0; i < iters; i++) {
        main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
        if (iter_scope == null) {
            (*iter_scope_fail_out)++;
            break;
        }
        interp.current_scope = iter_scope;

        Value* analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
        Value* count = prim_deduce_count(count_args[..], null, interp);
        if (analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null) {
            attempted++;
            Value* status = deduce_query_bench_dict_get(interp, analyzed, "status");
            Value* engine = deduce_query_bench_dict_get(interp, analyzed, "execution-engine");
            bool row_ok = count != null && count.tag == INT && count.int_val == expected_rows;
            if (status != null && status.tag == SYMBOL && status.sym_val == status_ok_sym && row_ok) {
                ok++;
            }
            if (engine != null && engine.tag == SYMBOL && engine.sym_val == expected_execution_engine) {
                engine_hit++;
            }
        }

        interp.current_scope = bench_scope;
        main::scope_release(iter_scope);
    }
    long lane_done = deduce_bench_now_ms();
    elapsed_ms = lane_done > lane_start ? lane_done - lane_start : 0;

    interp.current_scope = bench_scope;
    *elapsed_ms_out = elapsed_ms;
    *attempted_out = attempted;
    *ok_out = ok;
    *engine_hit_out = engine_hit;
    return true;
}

fn void run_deduce_recursive_closure_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled()) return;
    if (interp == null) return;

    io::printn("\n--- Deduce Recursive Closure Benchmarks ---");

    main::ScopeRegion* bench_scope = interp.current_scope;
    Value* naive_db = null;
    Value* naive_ancestor_rel = null;
    long expected_rows = 0;
    bool naive_seeded = deduce_query_bench_seed_recursive_closure_db(
        interp,
        &naive_db,
        &naive_ancestor_rel,
        &expected_rows
    );

    Value* semi_db = null;
    Value* semi_ancestor_rel = null;
    long semi_expected_rows = 0;
    bool semi_seeded = deduce_query_bench_seed_recursive_closure_db(
        interp,
        &semi_db,
        &semi_ancestor_rel,
        &semi_expected_rows
    );

    SymbolId naive_engine = interp.symbols.intern("naive");
    SymbolId semi_engine = interp.symbols.intern("semi-naive");
    SymbolId naive_exec = interp.symbols.intern("naive-scc");
    SymbolId semi_exec = interp.symbols.intern("semi-naive-scc");

    long naive_ms = 0;
    usz naive_attempted = 0;
    usz naive_ok = 0;
    usz naive_engine_hit = 0;

    long semi_ms = 0;
    usz semi_attempted = 0;
    usz semi_ok = 0;
    usz semi_engine_hit = 0;

    usz iter_scope_fail = 0;
    if (naive_seeded) {
        (void)deduce_query_bench_run_recursive_engine_lane(
            interp,
            bench_scope,
            naive_db,
            naive_ancestor_rel,
            naive_engine,
            naive_exec,
            DEDUCE_BENCH_RECURSIVE_NAIVE_ITERS,
            expected_rows,
            &naive_ms,
            &naive_attempted,
            &naive_ok,
            &naive_engine_hit,
            &iter_scope_fail
        );
    }

    if (semi_seeded) {
        (void)deduce_query_bench_run_recursive_engine_lane(
            interp,
            bench_scope,
            semi_db,
            semi_ancestor_rel,
            semi_engine,
            semi_exec,
            DEDUCE_BENCH_RECURSIVE_SEMI_ITERS,
            semi_expected_rows,
            &semi_ms,
            &semi_attempted,
            &semi_ok,
            &semi_engine_hit,
            &iter_scope_fail
        );
    }

    long speedup_x1000 = semi_ms > 0 ? (naive_ms * 1000) / semi_ms : 0;
    usz naive_mode_miss = naive_attempted >= naive_engine_hit
        ? naive_attempted - naive_engine_hit
        : 0;
    usz semi_mode_miss = semi_attempted >= semi_engine_hit
        ? semi_attempted - semi_engine_hit
        : 0;

    interp.current_scope = bench_scope;
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=deduce_recursive_closure chain_edges=%d expected_rows=%d naive_iters=%d semi_iters=%d naive_seeded_ok=%d semi_seeded_ok=%d naive_ms=%d naive_attempted=%d naive_ok=%d naive_engine_hit=%d naive_mode_miss=%d semi_ms=%d semi_attempted=%d semi_ok=%d semi_engine_hit=%d semi_mode_miss=%d speedup_x1000=%d iter_scope_fail=%d",
        (long)DEDUCE_BENCH_RECURSIVE_CHAIN_EDGES,
        expected_rows,
        (long)DEDUCE_BENCH_RECURSIVE_NAIVE_ITERS,
        (long)DEDUCE_BENCH_RECURSIVE_SEMI_ITERS,
        naive_seeded ? 1 : 0,
        semi_seeded ? 1 : 0,
        naive_ms,
        (long)naive_attempted,
        (long)naive_ok,
        (long)naive_engine_hit,
        (long)naive_mode_miss,
        semi_ms,
        (long)semi_attempted,
        (long)semi_ok,
        (long)semi_engine_hit,
        (long)semi_mode_miss,
        speedup_x1000,
        (long)iter_scope_fail
    );
}

fn bool deduce_query_bench_seed_skewed_cardinality_db(
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

    SymbolId left_sym = interp.symbols.intern("bench_skew_left");
    SymbolId right_sym = interp.symbols.intern("bench_skew_right");
    SymbolId out_sym = interp.symbols.intern("bench_skew_out");
    SymbolId key_sym = interp.symbols.intern("key");
    SymbolId value_sym = interp.symbols.intern("value");
    Value* left_rel = setup_deduce_relation_for_db(interp, db, left_sym, key_sym, value_sym);
    Value* right_rel = setup_deduce_relation_for_db(interp, db, right_sym, key_sym, value_sym);
    Value* out_rel = setup_deduce_relation_for_db(interp, db, out_sym, key_sym, value_sym);
    if (left_rel == null || left_rel.tag != FFI_HANDLE || left_rel.ffi_val == null) return false;
    if (right_rel == null || right_rel.tag != FFI_HANDLE || right_rel.ffi_val == null) return false;
    if (out_rel == null || out_rel.tag != FFI_HANDLE || out_rel.ffi_val == null) return false;

    SymbolId k = interp.symbols.intern("?k");
    SymbolId v = interp.symbols.intern("?v");
    SymbolId w = interp.symbols.intern("?w");
    Value* head = deduce_query_bench_make_atom2(
        interp,
        out_sym,
        deduce_query_bench_make_symbol_value(interp, k),
        deduce_query_bench_make_symbol_value(interp, v)
    );
    Value* body_left = deduce_query_bench_make_atom2(
        interp,
        left_sym,
        deduce_query_bench_make_symbol_value(interp, k),
        deduce_query_bench_make_symbol_value(interp, v)
    );
    Value* body_right = deduce_query_bench_make_atom2(
        interp,
        right_sym,
        deduce_query_bench_make_symbol_value(interp, k),
        deduce_query_bench_make_symbol_value(interp, w)
    );
    Value*[4] rule_args;
    rule_args[0] = db;
    rule_args[1] = head;
    rule_args[2] = body_left;
    rule_args[3] = body_right;
    Value* rule_ok = prim_deduce_rule(rule_args[..], null, interp);
    if (rule_ok == null || rule_ok.tag == ERROR) return false;

    for (usz i = 0; i < DEDUCE_BENCH_SKEW_HEAVY_ROWS; i++) {
        Value*[3] fact_args;
        fact_args[0] = left_rel;
        fact_args[1] = make_int(interp, 0);
        fact_args[2] = make_int(interp, (long)i);
        Value* inserted = prim_assert_fact(fact_args[..], null, interp);
        if (inserted == null || inserted.tag == ERROR) return false;
    }

    usz rare_start = DEDUCE_BENCH_SKEW_LEFT_ROWS - DEDUCE_BENCH_SKEW_HEAVY_ROWS;
    for (usz i = 0; i < rare_start; i++) {
        long key = (long)(i + 1);
        Value*[3] fact_args;
        fact_args[0] = left_rel;
        fact_args[1] = make_int(interp, key);
        fact_args[2] = make_int(interp, key);
        Value* inserted = prim_assert_fact(fact_args[..], null, interp);
        if (inserted == null || inserted.tag == ERROR) return false;
    }

    long expected_rows = 0;
    long right_base_key = (long)(rare_start - DEDUCE_BENCH_SKEW_RARE_KEYS + 1);
    for (usz i = 0; i < DEDUCE_BENCH_SKEW_RARE_KEYS; i++) {
        long key = right_base_key + (long)i;
        Value*[3] fact_args;
        fact_args[0] = right_rel;
        fact_args[1] = make_int(interp, key);
        fact_args[2] = make_int(interp, (long)i);
        Value* inserted = prim_assert_fact(fact_args[..], null, interp);
        if (inserted == null || inserted.tag == ERROR) return false;
        expected_rows++;
    }

    *db_out = db;
    *out_rel_out = out_rel;
    *out_sym_out = out_sym;
    *expected_rows_out = expected_rows;
    return true;
}

fn void run_deduce_skewed_cardinality_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled()) return;
    if (interp == null) return;

    io::printn("\n--- Deduce Skewed Cardinality Benchmarks ---");

    main::ScopeRegion* bench_scope = interp.current_scope;
    Value* db = null;
    Value* out_rel = null;
    SymbolId out_sym = 0;
    long expected_rows = 0;
    bool seeded = deduce_query_bench_seed_skewed_cardinality_db(interp, &db, &out_rel, &out_sym, &expected_rows);

    long explain_ms = 0;
    long analyze_ms = 0;
    usz explain_attempted = 0;
    usz explain_ok = 0;
    usz explain_selectivity_hits = 0;
    usz analyze_ok = 0;
    usz iter_scope_fail = 0;
    long step0_rows_read = -1;
    long step1_rows_read = -1;

    if (seeded) {
        Value*[2] explain_args;
        explain_args[0] = db;
        explain_args[1] = make_symbol(interp, out_sym);
        long explain_start = deduce_bench_now_ms();
        for (usz i = 0; i < DEDUCE_BENCH_SKEW_EXPLAIN_ITERS; i++) {
            main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
            if (iter_scope == null) {
                iter_scope_fail++;
                break;
            }
            interp.current_scope = iter_scope;

            Value* explained = prim_deduce_explain(explain_args[..], null, interp);
            if (explained != null && explained.tag == HASHMAP && explained.hashmap_val != null) {
                explain_attempted++;
                long rows0 = -1;
                long rows1 = -1;
                if (deduce_query_bench_assert_explain_counters_present(interp, explained, &rows0, &rows1)) {
                    explain_ok++;
                    step0_rows_read = rows0;
                    step1_rows_read = rows1;
                    if (rows0 <= rows1) explain_selectivity_hits++;
                }
            }

            interp.current_scope = bench_scope;
            main::scope_release(iter_scope);
        }
        long explain_done = deduce_bench_now_ms();
        explain_ms = explain_done > explain_start ? explain_done - explain_start : 0;

        Value*[1] analyze_args;
        analyze_args[0] = db;
        Value*[1] count_args;
        count_args[0] = out_rel;
        SymbolId status_ok_sym = interp.symbols.intern("ok");
        long analyze_start = deduce_bench_now_ms();
        for (usz i = 0; i < DEDUCE_BENCH_SKEW_ANALYZE_ITERS; i++) {
            main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
            if (iter_scope == null) {
                iter_scope_fail++;
                break;
            }
            interp.current_scope = iter_scope;

            Value* analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            Value* count = prim_deduce_count(count_args[..], null, interp);
            Value* status = deduce_query_bench_dict_get(interp, analyzed, "status");
            if (analyzed != null &&
                analyzed.tag == HASHMAP &&
                analyzed.hashmap_val != null &&
                status != null &&
                status.tag == SYMBOL &&
                status.sym_val == status_ok_sym &&
                count != null &&
                count.tag == INT &&
                count.int_val == expected_rows) {
                analyze_ok++;
            }

            interp.current_scope = bench_scope;
            main::scope_release(iter_scope);
        }
        long analyze_done = deduce_bench_now_ms();
        analyze_ms = analyze_done > analyze_start ? analyze_done - analyze_start : 0;
    }

    usz explain_mode_miss = explain_attempted >= explain_ok
        ? explain_attempted - explain_ok
        : 0;

    interp.current_scope = bench_scope;
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=deduce_skewed_cardinality left_rows=%d heavy_rows=%d rare_keys=%d explain_iters=%d analyze_iters=%d seeded_ok=%d expected_rows=%d explain_ms=%d explain_attempted=%d explain_ok=%d explain_mode_miss=%d explain_selectivity_hits=%d step0_rows_read=%d step1_rows_read=%d analyze_ms=%d analyze_ok=%d iter_scope_fail=%d",
        (long)DEDUCE_BENCH_SKEW_LEFT_ROWS,
        (long)DEDUCE_BENCH_SKEW_HEAVY_ROWS,
        (long)DEDUCE_BENCH_SKEW_RARE_KEYS,
        (long)DEDUCE_BENCH_SKEW_EXPLAIN_ITERS,
        (long)DEDUCE_BENCH_SKEW_ANALYZE_ITERS,
        seeded ? 1 : 0,
        expected_rows,
        explain_ms,
        (long)explain_attempted,
        (long)explain_ok,
        (long)explain_mode_miss,
        (long)explain_selectivity_hits,
        step0_rows_read,
        step1_rows_read,
        analyze_ms,
        (long)analyze_ok,
        (long)iter_scope_fail
    );
}

fn void run_deduce_incremental_mutation_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled()) return;
    if (interp == null) return;

    io::printn("\n--- Deduce Incremental Mutation Benchmarks ---");

    main::ScopeRegion* bench_scope = interp.current_scope;
    SymbolId tracked_mode_sym = interp.symbols.intern("tracked");
    SymbolId full_mode_sym = interp.symbols.intern("full-recompute");

    Value* tracked_db = null;
    Value* tracked_edge = null;
    bool tracked_seeded = deduce_query_bench_seed_incremental_db(interp, &tracked_db, &tracked_edge);

    Value* full_db = null;
    Value* full_edge = null;
    bool full_seeded = deduce_query_bench_seed_incremental_db(interp, &full_db, &full_edge);

    long tracked_mutation_ms = 0;
    long tracked_analyze_ms = 0;
    usz tracked_analyze_attempted = 0;
    usz tracked_analyze_ok = 0;
    usz tracked_mode_hits = 0;

    long full_mutation_ms = 0;
    long full_analyze_ms = 0;
    usz full_analyze_attempted = 0;
    usz full_analyze_ok = 0;
    usz full_mode_hits = 0;

    usz iter_scope_fail = 0;
    if (tracked_seeded) {
        (void)deduce_query_bench_run_mutation_lane(
            interp,
            bench_scope,
            tracked_db,
            tracked_edge,
            DEDUCE_BENCH_MUTATION_TRACKED_ITERS,
            false,
            tracked_mode_sym,
            &tracked_mutation_ms,
            &tracked_analyze_ms,
            &tracked_analyze_attempted,
            &tracked_analyze_ok,
            &tracked_mode_hits,
            &iter_scope_fail
        );
    }

    if (full_seeded) {
        (void)deduce_query_bench_run_mutation_lane(
            interp,
            bench_scope,
            full_db,
            full_edge,
            DEDUCE_BENCH_MUTATION_FULL_ITERS,
            true,
            full_mode_sym,
            &full_mutation_ms,
            &full_analyze_ms,
            &full_analyze_attempted,
            &full_analyze_ok,
            &full_mode_hits,
            &iter_scope_fail
        );
    }

    usz tracked_mode_miss = tracked_analyze_attempted >= tracked_mode_hits
        ? tracked_analyze_attempted - tracked_mode_hits
        : 0;
    usz full_mode_miss = full_analyze_attempted >= full_mode_hits
        ? full_analyze_attempted - full_mode_hits
        : 0;

    interp.current_scope = bench_scope;
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=deduce_incremental_mutation seed_rows=%d tracked_iters=%d full_iters=%d tracked_seeded_ok=%d full_seeded_ok=%d tracked_mutation_ms=%d tracked_analyze_ms=%d tracked_analyze_attempted=%d tracked_analyze_ok=%d tracked_mode_hits=%d tracked_mode_miss=%d full_mutation_ms=%d full_analyze_ms=%d full_analyze_attempted=%d full_analyze_ok=%d full_mode_hits=%d full_mode_miss=%d iter_scope_fail=%d",
        (long)DEDUCE_BENCH_MUTATION_SEED_ROWS,
        (long)DEDUCE_BENCH_MUTATION_TRACKED_ITERS,
        (long)DEDUCE_BENCH_MUTATION_FULL_ITERS,
        tracked_seeded ? 1 : 0,
        full_seeded ? 1 : 0,
        tracked_mutation_ms,
        tracked_analyze_ms,
        (long)tracked_analyze_attempted,
        (long)tracked_analyze_ok,
        (long)tracked_mode_hits,
        (long)tracked_mode_miss,
        full_mutation_ms,
        full_analyze_ms,
        (long)full_analyze_attempted,
        (long)full_analyze_ok,
        (long)full_mode_hits,
        (long)full_mode_miss,
        (long)iter_scope_fail
    );
}

fn void run_deduce_scan_query_count_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled()) return;
    if (interp == null) return;

    io::printn("\n--- Deduce Scan/Query/Count Benchmarks ---");

    main::ScopeRegion* bench_scope = interp.current_scope;
    Value* rel = null;
    usz query_expected = 0;
    bool seeded = deduce_bench_seed_relation(interp, &rel, &query_expected);

    long scan_ms = 0;
    long scan_range_ms = 0;
    long query_ms = 0;
    long count_ms = 0;

    usz scan_ok = 0;
    usz scan_range_ok = 0;
    usz query_ok = 0;
    usz count_ok = 0;
    usz iter_scope_fail = 0;
    DeduceScanRangeBenchStats scan_range_stats = {};

    EvalResult filter_eval = run("(lambda (row) (> ('score row) 50))", interp);
    Value* filter_fn = filter_eval.error.has_error ? null : filter_eval.value;
    bool filter_ok = !filter_eval.error.has_error &&
        filter_fn != null &&
        filter_fn.tag == CLOSURE;

    if (seeded) {
        Value*[1] scan_args;
        scan_args[0] = rel;
        Value*[3] scan_range_args;
        scan_range_args[0] = rel;
        scan_range_args[1] = deduce_bench_make_bound_tuple(interp, 0, 0);
        scan_range_args[2] = deduce_bench_make_bound_tuple(
            interp,
            (long)(DEDUCE_BENCH_CORPUS_ROWS - 1),
            99
        );
        Value*[2] query_args;
        query_args[0] = rel;
        query_args[1] = filter_fn;
        Value*[1] count_args;
        count_args[0] = rel;

        long scan_start = deduce_bench_now_ms();
        for (usz i = 0; i < DEDUCE_BENCH_SCAN_ITERS; i++) {
            main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
            if (iter_scope == null) {
                iter_scope_fail++;
                break;
            }
            interp.current_scope = iter_scope;

            Value* rows = prim_deduce_scan(scan_args[..], null, interp);
            bool ok = rows != null &&
                rows.tag != ERROR &&
                list_length(rows) == DEDUCE_BENCH_CORPUS_ROWS;
            if (ok) scan_ok++;

            interp.current_scope = bench_scope;
            main::scope_release(iter_scope);
        }
        long scan_done = deduce_bench_now_ms();
        scan_ms = scan_done > scan_start ? scan_done - scan_start : 0;

        if (filter_ok) {
            long query_start = deduce_bench_now_ms();
            for (usz i = 0; i < DEDUCE_BENCH_QUERY_ITERS; i++) {
                main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
                if (iter_scope == null) {
                    iter_scope_fail++;
                    break;
                }
                interp.current_scope = iter_scope;

                Value* rows = prim_deduce_query(query_args[..], null, interp);
                bool ok = rows != null &&
                    rows.tag != ERROR &&
                    list_length(rows) == query_expected;
                if (ok) query_ok++;

                interp.current_scope = bench_scope;
                main::scope_release(iter_scope);
            }
            long query_done = deduce_bench_now_ms();
            query_ms = query_done > query_start ? query_done - query_start : 0;
        }

        long count_start = deduce_bench_now_ms();
        for (usz i = 0; i < DEDUCE_BENCH_COUNT_ITERS; i++) {
            Value* count = prim_deduce_count(count_args[..], null, interp);
            bool ok = count != null &&
                count.tag == INT &&
                count.int_val == (long)DEDUCE_BENCH_CORPUS_ROWS;
            if (ok) count_ok++;
        }
        long count_done = deduce_bench_now_ms();
        count_ms = count_done > count_start ? count_done - count_start : 0;

        deduce_scan_range_bench_set_enabled(true);
        long scan_range_start = deduce_bench_now_ms();
        for (usz i = 0; i < DEDUCE_BENCH_SCAN_RANGE_ITERS; i++) {
            main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
            if (iter_scope == null) {
                iter_scope_fail++;
                break;
            }
            interp.current_scope = iter_scope;

            Value* rows = prim_deduce_scan_range(scan_range_args[..], null, interp);
            bool ok = rows != null &&
                rows.tag != ERROR &&
                list_length(rows) == DEDUCE_BENCH_CORPUS_ROWS;
            if (ok) scan_range_ok++;

            interp.current_scope = bench_scope;
            main::scope_release(iter_scope);
        }
        long scan_range_done = deduce_bench_now_ms();
        scan_range_ms = scan_range_done > scan_range_start ? scan_range_done - scan_range_start : 0;
        scan_range_stats = deduce_scan_range_bench_snapshot();
        deduce_scan_range_bench_set_enabled(false);
    }

    interp.current_scope = bench_scope;
    io::printfn(
        "OMNI_BENCH_SUMMARY suite=deduce_scan_query_count iters_scan=%d iters_scan_range=%d iters_query=%d iters_count=%d corpus_rows=%d seeded_ok=%d filter_ok=%d scan_ms=%d scan_ok=%d scan_range_ms=%d scan_range_ok=%d scan_range_calls=%d scan_range_rows=%d scan_range_key_symbols=%d scan_range_materialize_ms=%d query_ms=%d query_ok=%d query_expected=%d count_ms=%d count_ok=%d iter_scope_fail=%d",
        (long)DEDUCE_BENCH_SCAN_ITERS,
        (long)DEDUCE_BENCH_SCAN_RANGE_ITERS,
        (long)DEDUCE_BENCH_QUERY_ITERS,
        (long)DEDUCE_BENCH_COUNT_ITERS,
        (long)DEDUCE_BENCH_CORPUS_ROWS,
        seeded ? 1 : 0,
        filter_ok ? 1 : 0,
        scan_ms,
        (long)scan_ok,
        scan_range_ms,
        (long)scan_range_ok,
        (long)scan_range_stats.calls,
        (long)scan_range_stats.rows_materialized,
        (long)scan_range_stats.key_symbols_allocated,
        scan_range_stats.materialize_ms,
        query_ms,
        (long)query_ok,
        (long)query_expected,
        count_ms,
        (long)count_ok,
        (long)iter_scope_fail
    );
}

fn void run_deduce_query_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_scan_tests(interp, pass, fail);
    run_deduce_scan_range_tests(interp, pass, fail);
    run_deduce_query_filter_tests(interp, pass, fail);
    run_deduce_match_tests(interp, pass, fail);
    run_deduce_scan_query_count_benchmarks(interp);
    run_deduce_incremental_mutation_benchmarks(interp);
    run_deduce_selective_join_benchmarks(interp);
    run_deduce_recursive_closure_benchmarks(interp);
    run_deduce_skewed_cardinality_benchmarks(interp);
}
