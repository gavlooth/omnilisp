module lisp;


import std::core::mem;
import std::io;
import main;

const usz DEDUCE_BENCH_CORPUS_ROWS = 4096;
const usz DEDUCE_BENCH_SCAN_ITERS = 16;
const usz DEDUCE_BENCH_SCAN_RANGE_ITERS = 16;
const usz DEDUCE_BENCH_QUERY_ITERS = 16;
const usz DEDUCE_BENCH_COUNT_ITERS = 4096;

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
}
