module lisp;

import std::io;
import main;

fn void run_deduce_stats_maintenance_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled() || interp == null) return;

    main::ScopeRegion* root_scope = interp.current_scope;
    main::ScopeRegion* bench_scope = main::scope_create(root_scope);
    if (bench_scope == null) {
        io::printn("[FAIL] deduce stats maintenance benchmark scope");
        return;
    }
    defer {
        interp.current_scope = root_scope;
        main::scope_release(bench_scope);
    }

    interp.current_scope = bench_scope;

    long tracked_mutation_ms = 0;
    long tracked_refresh_ms = 0;
    long tracked_stats_ms = 0;
    long rebuild_rule_change_ms = 0;
    long rebuild_refresh_ms = 0;
    long rebuild_stats_ms = 0;
    usz tracked_refresh_hits = 0;
    usz tracked_stats_hits = 0;
    usz rebuild_pre_stats_hits = 0;
    usz rebuild_fallback_hits = 0;
    usz rebuild_post_stats_hits = 0;
    usz iter_scope_fail = 0;

    bool tracked_seeded_ok = deduce_query_bench_run_stats_maintenance_tracked_lane(
        interp,
        bench_scope,
        &tracked_mutation_ms,
        &tracked_refresh_ms,
        &tracked_stats_ms,
        &tracked_refresh_hits,
        &tracked_stats_hits,
        &iter_scope_fail
    );
    bool rebuild_seeded_ok = deduce_query_bench_run_stats_maintenance_rebuild_lane(
        interp,
        bench_scope,
        &rebuild_rule_change_ms,
        &rebuild_refresh_ms,
        &rebuild_stats_ms,
        &rebuild_pre_stats_hits,
        &rebuild_fallback_hits,
        &rebuild_post_stats_hits,
        &iter_scope_fail
    );

    io::printfn(
        "OMNI_BENCH_SUMMARY suite=deduce_stats_maintenance seed_rows=%d tracked_iters=%d rebuild_iters=%d tracked_seeded_ok=%d tracked_mutation_ms=%d tracked_refresh_ms=%d tracked_stats_ms=%d tracked_refresh_hits=%d tracked_refresh_miss=%d tracked_stats_hits=%d tracked_stats_miss=%d rebuild_seeded_ok=%d rebuild_rule_change_ms=%d rebuild_refresh_ms=%d rebuild_stats_ms=%d rebuild_pre_stats_hits=%d rebuild_pre_stats_miss=%d rebuild_fallback_hits=%d rebuild_fallback_miss=%d rebuild_post_stats_hits=%d rebuild_post_stats_miss=%d iter_scope_fail=%d",
        (long)DEDUCE_BENCH_STATS_MAINTENANCE_SEED_ROWS,
        (long)DEDUCE_BENCH_STATS_MAINTENANCE_TRACKED_ITERS,
        (long)DEDUCE_BENCH_STATS_MAINTENANCE_REBUILD_ITERS,
        tracked_seeded_ok ? 1 : 0,
        tracked_mutation_ms,
        tracked_refresh_ms,
        tracked_stats_ms,
        (long)tracked_refresh_hits,
        (long)(DEDUCE_BENCH_STATS_MAINTENANCE_TRACKED_ITERS - tracked_refresh_hits),
        (long)tracked_stats_hits,
        (long)(DEDUCE_BENCH_STATS_MAINTENANCE_TRACKED_ITERS - tracked_stats_hits),
        rebuild_seeded_ok ? 1 : 0,
        rebuild_rule_change_ms,
        rebuild_refresh_ms,
        rebuild_stats_ms,
        (long)rebuild_pre_stats_hits,
        (long)(DEDUCE_BENCH_STATS_MAINTENANCE_REBUILD_ITERS - rebuild_pre_stats_hits),
        (long)rebuild_fallback_hits,
        (long)(DEDUCE_BENCH_STATS_MAINTENANCE_REBUILD_ITERS - rebuild_fallback_hits),
        (long)rebuild_post_stats_hits,
        (long)(DEDUCE_BENCH_STATS_MAINTENANCE_REBUILD_ITERS - rebuild_post_stats_hits),
        (long)iter_scope_fail
    );

    bool ok = tracked_seeded_ok &&
        rebuild_seeded_ok &&
        iter_scope_fail == 0 &&
        tracked_refresh_hits == DEDUCE_BENCH_STATS_MAINTENANCE_TRACKED_ITERS &&
        tracked_stats_hits == DEDUCE_BENCH_STATS_MAINTENANCE_TRACKED_ITERS &&
        rebuild_pre_stats_hits == DEDUCE_BENCH_STATS_MAINTENANCE_REBUILD_ITERS &&
        rebuild_fallback_hits == DEDUCE_BENCH_STATS_MAINTENANCE_REBUILD_ITERS &&
        rebuild_post_stats_hits == DEDUCE_BENCH_STATS_MAINTENANCE_REBUILD_ITERS;
    if (ok) {
        emit_pass_literal("[PASS] deduce stats maintenance benchmark smoke");
    } else {
        io::printn("[FAIL] deduce stats maintenance benchmark smoke");
    }
}

fn void run_deduce_selective_join_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled() || interp == null) return;

    Value* db = null;
    Value* out_rel = null;
    SymbolId out_sym = (SymbolId)0;
    long expected_rows = 0;
    if (!deduce_query_bench_seed_selective_join_db(interp, &db, &out_rel, &out_sym, &expected_rows)) {
        io::printn("[FAIL] deduce selective join benchmark seed");
        return;
    }

    Value*[1] explain_args;
    explain_args[0] = db;
    Value* explained = prim_deduce_explain(explain_args[..], null, interp);
    long step0_rows_read = 0;
    long step1_rows_read = 0;
    bool ok = deduce_query_bench_assert_selective_explain(
        interp,
        explained,
        &step0_rows_read,
        &step1_rows_read
    ) && out_rel != null && out_sym != (SymbolId)0 && expected_rows > 0;

    if (ok) {
        emit_pass_literal("[PASS] deduce selective join benchmark smoke");
    } else {
        io::printn("[FAIL] deduce selective join benchmark smoke");
    }
}

fn void run_deduce_recursive_closure_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled() || interp == null) return;
}

fn void run_deduce_skewed_cardinality_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled() || interp == null) return;
}

fn void run_deduce_incremental_mutation_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled() || interp == null) return;
}

fn void run_deduce_aggregate_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled() || interp == null) return;

    Value* db = null;
    Value* out_rel = null;
    long expected_groups = 0;
    if (!deduce_query_bench_seed_aggregate_db(interp, &db, &out_rel, &expected_groups)) {
        io::printn("[FAIL] deduce aggregate benchmark seed");
        return;
    }

    long analyze_start = deduce_bench_now_ms();
    Value*[1] analyze_args;
    analyze_args[0] = db;
    Value* analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
    long analyze_done = deduce_bench_now_ms();

    Value*[1] count_args;
    count_args[0] = out_rel;
    Value* count_val = prim_deduce_count(count_args[..], null, interp);

    bool ok = analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null &&
        count_val != null && count_val.tag == INT && count_val.int_val == expected_groups &&
        analyze_done >= analyze_start;
    if (ok) {
        emit_pass_literal("[PASS] deduce aggregate benchmark smoke");
    } else {
        io::printn("[FAIL] deduce aggregate benchmark smoke");
    }
}

fn void run_deduce_recursive_aggregate_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled() || interp == null) return;

    Value* db = null;
    Value* out_rel = null;
    usz expected_rows = 0;
    if (!deduce_query_bench_seed_recursive_aggregate_db(
            interp,
            DEDUCE_BENCH_RECURSIVE_CHAIN_EDGES + 1,
            &db,
            &out_rel,
            &expected_rows
        )) {
        io::printn("[FAIL] deduce recursive aggregate benchmark seed");
        return;
    }

    long analyze_start = deduce_bench_now_ms();
    Value*[2] analyze_args;
    analyze_args[0] = db;
    analyze_args[1] = make_symbol(interp, interp.symbols.intern("semi-naive"));
    Value* analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
    long analyze_done = deduce_bench_now_ms();

    Value*[1] count_args;
    count_args[0] = out_rel;
    Value* count_val = prim_deduce_count(count_args[..], null, interp);
    Value* engine = null;
    if (analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null) {
        engine = deduce_query_bench_dict_get(interp, analyzed, "execution-engine");
    }

    bool ok = analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null &&
        engine != null && engine.tag == SYMBOL && engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
        count_val != null && count_val.tag == INT && count_val.int_val == (long)expected_rows &&
        analyze_done >= analyze_start;
    if (ok) {
        emit_pass_literal("[PASS] deduce recursive aggregate benchmark smoke");
    } else {
        io::printn("[FAIL] deduce recursive aggregate benchmark smoke");
    }
}

fn void run_deduce_scan_query_count_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled() || interp == null) return;

    main::ScopeRegion* root_scope = interp.current_scope;
    main::ScopeRegion* bench_scope = main::scope_create(root_scope);
    if (bench_scope == null) {
        io::printn("[FAIL] deduce scan/query/count benchmark scope");
        return;
    }
    defer {
        deduce_scan_range_bench_set_enabled(false);
        interp.current_scope = root_scope;
        main::scope_release(bench_scope);
    }

    interp.current_scope = bench_scope;

    Value* rel = null;
    usz query_expected = 0;
    bool seeded_ok = deduce_bench_seed_relation(interp, &rel, &query_expected);

    EvalResult filter_eval = run("(lambda (row) (> row.score 50))", interp);
    Value* filter_fn = filter_eval.value;
    bool filter_ok = !filter_eval.error.has_error && filter_fn != null && filter_fn.tag != ERROR;

    Value* lower_bound = deduce_bench_make_bound_tuple(interp, 0, 0);
    Value* upper_bound = deduce_bench_make_bound_tuple(
        interp,
        (long)(DEDUCE_BENCH_CORPUS_ROWS - 1),
        (long)((DEDUCE_BENCH_CORPUS_ROWS - 1) % 100)
    );

    long scan_ms = 0;
    long scan_range_ms = 0;
    long query_ms = 0;
    long count_ms = 0;
    long scan_range_materialize_us_per_row = 0;
    usz scan_ok = 0;
    usz scan_range_ok = 0;
    usz query_ok = 0;
    usz count_ok = 0;
    usz iter_scope_fail = 0;

    if (seeded_ok && filter_ok) {
        Value*[1] scan_args;
        scan_args[0] = rel;
        for (usz i = 0; i < DEDUCE_BENCH_SCAN_ITERS; i++) {
            main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
            if (iter_scope == null) {
                iter_scope_fail++;
                break;
            }
            interp.current_scope = iter_scope;

            long start_ms = deduce_bench_now_ms();
            Value* scanned = prim_deduce_scan(scan_args[..], null, interp);
            long done_ms = deduce_bench_now_ms();
            if (done_ms > start_ms) scan_ms += done_ms - start_ms;

            if (scanned != null && deduce_query_bench_list_length(scanned) == DEDUCE_BENCH_CORPUS_ROWS) {
                scan_ok++;
            }

            interp.current_scope = bench_scope;
            main::scope_release(iter_scope);
        }

        deduce_scan_range_bench_set_enabled(true);
        Value*[3] scan_range_args;
        scan_range_args[0] = rel;
        scan_range_args[1] = lower_bound;
        scan_range_args[2] = upper_bound;
        for (usz i = 0; i < DEDUCE_BENCH_SCAN_RANGE_ITERS; i++) {
            main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
            if (iter_scope == null) {
                iter_scope_fail++;
                break;
            }
            interp.current_scope = iter_scope;

            long start_ms = deduce_bench_now_ms();
            Value* scanned = prim_deduce_scan_range(scan_range_args[..], null, interp);
            long done_ms = deduce_bench_now_ms();
            if (done_ms > start_ms) scan_range_ms += done_ms - start_ms;

            if (scanned != null && deduce_query_bench_list_length(scanned) == DEDUCE_BENCH_CORPUS_ROWS) {
                scan_range_ok++;
            }

            interp.current_scope = bench_scope;
            main::scope_release(iter_scope);
        }
        DeduceScanRangeBenchStats scan_range_stats = deduce_scan_range_bench_snapshot();
        if (scan_range_stats.rows_materialized > 0) {
            scan_range_materialize_us_per_row =
                (scan_range_stats.materialize_ms * 1000) / (long)scan_range_stats.rows_materialized;
        }
        deduce_scan_range_bench_set_enabled(false);

        Value*[2] query_args;
        query_args[0] = rel;
        query_args[1] = filter_fn;
        for (usz i = 0; i < DEDUCE_BENCH_QUERY_ITERS; i++) {
            main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
            if (iter_scope == null) {
                iter_scope_fail++;
                break;
            }
            interp.current_scope = iter_scope;

            long start_ms = deduce_bench_now_ms();
            Value* filtered = prim_deduce_query(query_args[..], null, interp);
            long done_ms = deduce_bench_now_ms();
            if (done_ms > start_ms) query_ms += done_ms - start_ms;

            if (filtered != null && deduce_query_bench_list_length(filtered) == query_expected) {
                query_ok++;
            }

            interp.current_scope = bench_scope;
            main::scope_release(iter_scope);
        }

        Value*[1] count_args;
        count_args[0] = rel;
        for (usz i = 0; i < DEDUCE_BENCH_COUNT_ITERS; i++) {
            long start_ms = deduce_bench_now_ms();
            Value* counted = prim_deduce_count(count_args[..], null, interp);
            long done_ms = deduce_bench_now_ms();
            if (done_ms > start_ms) count_ms += done_ms - start_ms;

            if (counted != null && counted.tag == INT && counted.int_val == (long)DEDUCE_BENCH_CORPUS_ROWS) {
                count_ok++;
            }
        }

        io::printfn(
            "OMNI_BENCH_SUMMARY suite=deduce_scan_query_count iters_scan=%d iters_scan_range=%d iters_query=%d iters_count=%d corpus_rows=%d seeded_ok=%d filter_ok=%d scan_ms=%d scan_ok=%d scan_range_ms=%d scan_range_ok=%d scan_range_calls=%d scan_range_rows=%d scan_range_key_symbols=%d scan_range_materialize_ms=%d scan_range_materialize_us_per_row=%d query_ms=%d query_ok=%d query_expected=%d count_ms=%d count_ok=%d iter_scope_fail=%d",
            (long)DEDUCE_BENCH_SCAN_ITERS,
            (long)DEDUCE_BENCH_SCAN_RANGE_ITERS,
            (long)DEDUCE_BENCH_QUERY_ITERS,
            (long)DEDUCE_BENCH_COUNT_ITERS,
            (long)DEDUCE_BENCH_CORPUS_ROWS,
            seeded_ok ? 1 : 0,
            filter_ok ? 1 : 0,
            scan_ms,
            (long)scan_ok,
            scan_range_ms,
            (long)scan_range_ok,
            (long)scan_range_stats.calls,
            (long)scan_range_stats.rows_materialized,
            (long)scan_range_stats.key_symbols_allocated,
            scan_range_stats.materialize_ms,
            scan_range_materialize_us_per_row,
            query_ms,
            (long)query_ok,
            (long)query_expected,
            count_ms,
            (long)count_ok,
            (long)iter_scope_fail
        );

        bool ok = scan_ok == DEDUCE_BENCH_SCAN_ITERS &&
            scan_range_ok == DEDUCE_BENCH_SCAN_RANGE_ITERS &&
            query_ok == DEDUCE_BENCH_QUERY_ITERS &&
            count_ok == DEDUCE_BENCH_COUNT_ITERS &&
            iter_scope_fail == 0;
        if (ok) {
            emit_pass_literal("[PASS] deduce scan/query/count benchmark smoke");
        } else {
            io::printn("[FAIL] deduce scan/query/count benchmark smoke");
        }
        return;
    }

    io::printfn(
        "OMNI_BENCH_SUMMARY suite=deduce_scan_query_count iters_scan=%d iters_scan_range=%d iters_query=%d iters_count=%d corpus_rows=%d seeded_ok=%d filter_ok=%d scan_ms=%d scan_ok=%d scan_range_ms=%d scan_range_ok=%d scan_range_calls=%d scan_range_rows=%d scan_range_key_symbols=%d scan_range_materialize_ms=%d scan_range_materialize_us_per_row=%d query_ms=%d query_ok=%d query_expected=%d count_ms=%d count_ok=%d iter_scope_fail=%d",
        (long)DEDUCE_BENCH_SCAN_ITERS,
        (long)DEDUCE_BENCH_SCAN_RANGE_ITERS,
        (long)DEDUCE_BENCH_QUERY_ITERS,
        (long)DEDUCE_BENCH_COUNT_ITERS,
        (long)DEDUCE_BENCH_CORPUS_ROWS,
        seeded_ok ? 1 : 0,
        filter_ok ? 1 : 0,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        0L,
        (long)query_expected,
        0L,
        0L,
        (long)iter_scope_fail
    );
    io::printn("[FAIL] deduce scan/query/count benchmark smoke");
}
