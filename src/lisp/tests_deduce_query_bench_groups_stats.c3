module lisp;

import std::io;
import main;

fn bool deduce_query_bench_seed_stats_maintenance_db(
    Interp* interp,
    Value** db_out,
    Value** edge_out,
    Value** rebuild_out,
    Value** view_out
) {
    if (interp == null || db_out == null || edge_out == null || rebuild_out == null || view_out == null) return false;
    *db_out = null;
    *edge_out = null;
    *rebuild_out = null;
    *view_out = null;

    Value*[1] open_args;
    open_args[0] = make_symbol(interp, interp.symbols.intern("memory"));
    Value* db = prim_deduce_open(open_args[..], null, interp);
    if (db == null || db.tag != FFI_HANDLE || db.ffi_val == null) {
        io::printfn(
            "[deduce-debug] recursive-mixed-negated seed failed: db open (tag=%d msg=%s)",
            db != null ? (int)db.tag : -1,
            (ZString)(db != null && db.tag == ERROR ? db.str_chars[:db.str_len] : "<none>")
        );
        return false;
    }

    SymbolId edge_sym = interp.symbols.intern("bench_stats_edge");
    SymbolId rebuild_sym = interp.symbols.intern("bench_stats_rebuild");
    SymbolId view_sym = interp.symbols.intern("bench_stats_view");
    SymbolId from_sym = interp.symbols.intern("from");
    SymbolId to_sym = interp.symbols.intern("to");
    Value* edge_rel = setup_deduce_relation_for_db(interp, db, edge_sym, from_sym, to_sym);
    Value* rebuild_rel = setup_deduce_relation_for_db(interp, db, rebuild_sym, from_sym, to_sym);
    Value* view_rel = setup_deduce_relation_for_db(interp, db, view_sym, from_sym, to_sym);
    if (edge_rel == null || edge_rel.tag != FFI_HANDLE || edge_rel.ffi_val == null) return false;
    if (rebuild_rel == null || rebuild_rel.tag != FFI_HANDLE || rebuild_rel.ffi_val == null) return false;
    if (view_rel == null || view_rel.tag != FFI_HANDLE || view_rel.ffi_val == null) return false;

    SymbolId var_x = interp.symbols.intern("?x");
    SymbolId var_y = interp.symbols.intern("?y");
    Value* head = deduce_query_bench_make_atom2(
        interp,
        view_sym,
        deduce_query_bench_make_symbol_value(interp, var_x),
        deduce_query_bench_make_symbol_value(interp, var_y)
    );
    Value* body = deduce_query_bench_make_atom2(
        interp,
        edge_sym,
        deduce_query_bench_make_symbol_value(interp, var_x),
        deduce_query_bench_make_symbol_value(interp, var_y)
    );
    Value*[3] rule_args;
    rule_args[0] = db;
    rule_args[1] = head;
    rule_args[2] = body;
    Value* rule_ok = prim_deduce_rule(rule_args[..], null, interp);
    if (rule_ok == null || rule_ok.tag == ERROR) return false;

    for (usz i = 0; i < DEDUCE_BENCH_STATS_MAINTENANCE_SEED_ROWS; i++) {
        Value*[3] fact_args;
        fact_args[0] = edge_rel;
        fact_args[1] = make_int(interp, (long)i);
        fact_args[2] = make_int(interp, (long)(i + 1));
        Value* inserted = prim_assert_fact(fact_args[..], null, interp);
        if (inserted == null || inserted.tag == ERROR) return false;
    }

    Value*[1] materialize_args;
    materialize_args[0] = view_rel;
    Value* materialized = prim_deduce_materialize(materialize_args[..], null, interp);
    if (materialized == null || materialized.tag == ERROR) return false;

    Value*[1] refresh_args;
    refresh_args[0] = view_rel;
    Value* refreshed = prim_deduce_refresh(refresh_args[..], null, interp);
    if (refreshed == null || refreshed.tag != HASHMAP || refreshed.hashmap_val == null) return false;

    Value* refresh_scope = deduce_query_bench_dict_get(interp, refreshed, "refresh-scope");
    Value* refresh_path = deduce_query_bench_dict_get(interp, refreshed, "refresh-execution-path");
    Value* refresh_fallback = deduce_query_bench_dict_get(interp, refreshed, "refresh-fallback-reason");
    Value* refreshed_count = deduce_query_bench_dict_get(interp, refreshed, "refreshed-materialized-view-count");
    Value* refreshed_relations = deduce_query_bench_dict_get(interp, refreshed, "refreshed-materialized-relations");
    bool refresh_ok =
        deduce_query_bench_value_is_symbol(refresh_scope, interp.symbols.intern("relation")) &&
        deduce_query_bench_value_is_symbol(refresh_path, interp.symbols.intern("targeted")) &&
        is_nil(refresh_fallback) &&
        deduce_query_bench_value_is_int(refreshed_count, 1) &&
        is_cons(refreshed_relations) &&
        deduce_query_bench_value_is_symbol(car(refreshed_relations), view_sym);
    if (!refresh_ok) return false;

    Value*[1] stats_args;
    stats_args[0] = view_rel;
    Value* stats = prim_deduce_stats(stats_args[..], null, interp);
    if (stats == null || stats.tag != HASHMAP || stats.hashmap_val == null) return false;

    Value* status = deduce_query_bench_dict_get(interp, stats, "status");
    Value* refresh_count = deduce_query_bench_dict_get(interp, stats, "materialized-refresh-count");
    Value* stale_reason = deduce_query_bench_dict_get(interp, stats, "materialized-last-stale-reason");
    Value* materialized_stale = deduce_query_bench_dict_get(interp, stats, "materialized-stale");
    bool stats_ok =
        deduce_query_bench_value_is_symbol(status, interp.symbols.intern("ok")) &&
        deduce_query_bench_value_is_int(refresh_count, 1) &&
        is_nil(stale_reason) &&
        is_nil(materialized_stale);
    if (!stats_ok) return false;

    *db_out = db;
    *edge_out = edge_rel;
    *rebuild_out = rebuild_rel;
    *view_out = view_rel;
    return true;
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

fn bool deduce_query_bench_run_stats_maintenance_tracked_lane(
    Interp* interp,
    main::ScopeRegion* bench_scope,
    long* mutation_ms_out,
    long* refresh_ms_out,
    long* stats_ms_out,
    usz* refresh_hits_out,
    usz* stats_hits_out,
    usz* iter_scope_fail_out
) {
    if (interp == null || bench_scope == null) return false;
    if (mutation_ms_out == null || refresh_ms_out == null || stats_ms_out == null ||
        refresh_hits_out == null || stats_hits_out == null || iter_scope_fail_out == null) {
        return false;
    }

    *mutation_ms_out = 0;
    *refresh_ms_out = 0;
    *stats_ms_out = 0;
    *refresh_hits_out = 0;
    *stats_hits_out = 0;

    Value* db = null;
    Value* edge_rel = null;
    Value* rebuild_rel = null;
    Value* view_rel = null;
    if (!deduce_query_bench_seed_stats_maintenance_db(interp, &db, &edge_rel, &rebuild_rel, &view_rel)) return false;
    (void)rebuild_rel;

    long mutation_ms = 0;
    long refresh_ms = 0;
    long stats_ms = 0;
    usz refresh_hits = 0;
    usz stats_hits = 0;
    SymbolId relation_sym = interp.symbols.intern("relation");
    SymbolId incremental_targeted_sym = interp.symbols.intern("incremental-targeted");
    SymbolId ok_sym = interp.symbols.intern("ok");

    for (usz i = 0; i < DEDUCE_BENCH_STATS_MAINTENANCE_TRACKED_ITERS; i++) {
        main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
        if (iter_scope == null) {
            (*iter_scope_fail_out)++;
            break;
        }
        interp.current_scope = iter_scope;

        long mutation_start = deduce_bench_now_ms();
        long row_id = (long)(DEDUCE_BENCH_STATS_MAINTENANCE_SEED_ROWS + i);
        Value*[3] fact_args;
        fact_args[0] = edge_rel;
        fact_args[1] = make_int(interp, row_id);
        fact_args[2] = make_int(interp, row_id + 1);
        Value* inserted = prim_assert_fact(fact_args[..], null, interp);
        long mutation_done = deduce_bench_now_ms();
        if (mutation_done > mutation_start) mutation_ms += mutation_done - mutation_start;

        if (inserted != null && inserted.tag != ERROR) {
            Value*[1] refresh_args;
            refresh_args[0] = view_rel;
            long refresh_start = deduce_bench_now_ms();
            Value* refreshed = prim_deduce_refresh(refresh_args[..], null, interp);
            long refresh_done = deduce_bench_now_ms();
            if (refresh_done > refresh_start) refresh_ms += refresh_done - refresh_start;

            bool refresh_ok = refreshed != null && refreshed.tag == HASHMAP && refreshed.hashmap_val != null;
            if (refresh_ok) {
                Value* refresh_scope = deduce_query_bench_dict_get(interp, refreshed, "refresh-scope");
                Value* refresh_path = deduce_query_bench_dict_get(interp, refreshed, "refresh-execution-path");
                Value* refresh_fallback = deduce_query_bench_dict_get(interp, refreshed, "refresh-fallback-reason");
                Value* refreshed_count = deduce_query_bench_dict_get(interp, refreshed, "refreshed-materialized-view-count");
                refresh_ok =
                    deduce_query_bench_value_is_symbol(refresh_scope, relation_sym) &&
                    deduce_query_bench_value_is_symbol(refresh_path, incremental_targeted_sym) &&
                    is_nil(refresh_fallback) &&
                    deduce_query_bench_value_is_int(refreshed_count, 1);
            }
            if (refresh_ok) refresh_hits++;

            Value*[1] stats_args;
            stats_args[0] = view_rel;
            long stats_start = deduce_bench_now_ms();
            Value* stats = prim_deduce_stats(stats_args[..], null, interp);
            long stats_done = deduce_bench_now_ms();
            if (stats_done > stats_start) stats_ms += stats_done - stats_start;

            bool stats_ok = stats != null && stats.tag == HASHMAP && stats.hashmap_val != null;
            if (stats_ok) {
                Value* status = deduce_query_bench_dict_get(interp, stats, "status");
                Value* refresh_count = deduce_query_bench_dict_get(interp, stats, "materialized-refresh-count");
                Value* stale_reason = deduce_query_bench_dict_get(interp, stats, "materialized-last-stale-reason");
                Value* materialized_stale = deduce_query_bench_dict_get(interp, stats, "materialized-stale");
                Value* full_recompute_required = deduce_query_bench_dict_get(interp, stats, "full-recompute-required");
                Value* dirty_self = deduce_query_bench_dict_get(interp, stats, "dirty-self");
                stats_ok =
                    deduce_query_bench_value_is_symbol(status, ok_sym) &&
                    deduce_query_bench_value_is_int(refresh_count, (long)(i + 2)) &&
                    is_nil(stale_reason) &&
                    is_nil(materialized_stale) &&
                    is_nil(full_recompute_required) &&
                    is_nil(dirty_self);
            }
            if (stats_ok) stats_hits++;
        }

        interp.current_scope = bench_scope;
        main::scope_release(iter_scope);
    }

    interp.current_scope = bench_scope;
    *mutation_ms_out = mutation_ms;
    *refresh_ms_out = refresh_ms;
    *stats_ms_out = stats_ms;
    *refresh_hits_out = refresh_hits;
    *stats_hits_out = stats_hits;
    return true;
}

fn bool deduce_query_bench_run_stats_maintenance_rebuild_lane(
    Interp* interp,
    main::ScopeRegion* bench_scope,
    long* rule_change_ms_out,
    long* refresh_ms_out,
    long* stats_ms_out,
    usz* pre_stats_hits_out,
    usz* fallback_hits_out,
    usz* post_stats_hits_out,
    usz* iter_scope_fail_out
) {
    if (interp == null || bench_scope == null) return false;
    if (rule_change_ms_out == null || refresh_ms_out == null || stats_ms_out == null ||
        pre_stats_hits_out == null || fallback_hits_out == null ||
        post_stats_hits_out == null || iter_scope_fail_out == null) {
        return false;
    }

    *rule_change_ms_out = 0;
    *refresh_ms_out = 0;
    *stats_ms_out = 0;
    *pre_stats_hits_out = 0;
    *fallback_hits_out = 0;
    *post_stats_hits_out = 0;

    long rule_change_ms = 0;
    long refresh_ms = 0;
    long stats_ms = 0;
    usz pre_stats_hits = 0;
    usz fallback_hits = 0;
    usz post_stats_hits = 0;
    SymbolId relation_sym = interp.symbols.intern("relation");
    SymbolId db_wide_sym = interp.symbols.intern("db-wide");
    SymbolId full_recompute_required_sym = interp.symbols.intern("full-recompute-required");
    SymbolId rule_set_change_sym = interp.symbols.intern("rule-set-change");
    SymbolId ok_sym = interp.symbols.intern("ok");

    for (usz i = 0; i < DEDUCE_BENCH_STATS_MAINTENANCE_REBUILD_ITERS; i++) {
        main::ScopeRegion* iter_scope = main::scope_create(bench_scope);
        if (iter_scope == null) {
            (*iter_scope_fail_out)++;
            break;
        }
        interp.current_scope = iter_scope;

        Value* db = null;
        Value* edge_rel = null;
        Value* rebuild_rel = null;
        Value* view_rel = null;
        if (deduce_query_bench_seed_stats_maintenance_db(interp, &db, &edge_rel, &rebuild_rel, &view_rel)) {
            (void)edge_rel;

            long rule_change_start = deduce_bench_now_ms();
            Value*[3] rebuild_fact_args;
            rebuild_fact_args[0] = rebuild_rel;
            rebuild_fact_args[1] = make_int(interp, (long)(DEDUCE_BENCH_STATS_MAINTENANCE_SEED_ROWS + i));
            rebuild_fact_args[2] = make_int(interp, (long)(DEDUCE_BENCH_STATS_MAINTENANCE_SEED_ROWS + i + 1));
            Value* rebuild_inserted = prim_assert_fact(rebuild_fact_args[..], null, interp);

            SymbolId rebuild_sym = interp.symbols.intern("bench_stats_rebuild");
            SymbolId view_sym = interp.symbols.intern("bench_stats_view");
            SymbolId var_x = interp.symbols.intern("?x");
            SymbolId var_y = interp.symbols.intern("?y");
            Value* head = deduce_query_bench_make_atom2(
                interp,
                view_sym,
                deduce_query_bench_make_symbol_value(interp, var_x),
                deduce_query_bench_make_symbol_value(interp, var_y)
            );
            Value* body = deduce_query_bench_make_atom2(
                interp,
                rebuild_sym,
                deduce_query_bench_make_symbol_value(interp, var_x),
                deduce_query_bench_make_symbol_value(interp, var_y)
            );
            Value*[3] rule_args;
            rule_args[0] = db;
            rule_args[1] = head;
            rule_args[2] = body;
            Value* rule_ok = prim_deduce_rule(rule_args[..], null, interp);
            long rule_change_done = deduce_bench_now_ms();
            if (rule_change_done > rule_change_start) rule_change_ms += rule_change_done - rule_change_start;

            if (rebuild_inserted != null && rebuild_inserted.tag != ERROR && rule_ok != null && rule_ok.tag != ERROR) {
                Value*[1] stats_args;
                stats_args[0] = view_rel;
                long pre_stats_start = deduce_bench_now_ms();
                Value* stats0 = prim_deduce_stats(stats_args[..], null, interp);
                long pre_stats_done = deduce_bench_now_ms();
                if (pre_stats_done > pre_stats_start) stats_ms += pre_stats_done - pre_stats_start;

                bool pre_stats_ok = stats0 != null && stats0.tag == HASHMAP && stats0.hashmap_val != null;
                if (pre_stats_ok) {
                    Value* status = deduce_query_bench_dict_get(interp, stats0, "status");
                    Value* stale_reason = deduce_query_bench_dict_get(interp, stats0, "materialized-last-stale-reason");
                    Value* full_recompute_required = deduce_query_bench_dict_get(interp, stats0, "full-recompute-required");
                    Value* materialized_stale = deduce_query_bench_dict_get(interp, stats0, "materialized-stale");
                    pre_stats_ok =
                        deduce_query_bench_value_is_symbol(status, ok_sym) &&
                        deduce_query_bench_value_is_symbol(stale_reason, rule_set_change_sym) &&
                        deduce_query_bench_value_is_true(interp, full_recompute_required) &&
                        deduce_query_bench_value_is_true(interp, materialized_stale);
                }
                if (pre_stats_ok) pre_stats_hits++;

                Value*[1] refresh_args;
                refresh_args[0] = view_rel;
                long refresh_start = deduce_bench_now_ms();
                Value* refreshed = prim_deduce_refresh(refresh_args[..], null, interp);
                long refresh_done = deduce_bench_now_ms();
                if (refresh_done > refresh_start) refresh_ms += refresh_done - refresh_start;

                bool refresh_ok = refreshed != null && refreshed.tag == HASHMAP && refreshed.hashmap_val != null;
                if (refresh_ok) {
                    Value* refresh_scope = deduce_query_bench_dict_get(interp, refreshed, "refresh-scope");
                    Value* refresh_path = deduce_query_bench_dict_get(interp, refreshed, "refresh-execution-path");
                    Value* refresh_fallback = deduce_query_bench_dict_get(interp, refreshed, "refresh-fallback-reason");
                    Value* refreshed_count = deduce_query_bench_dict_get(interp, refreshed, "refreshed-materialized-view-count");
                    refresh_ok =
                        deduce_query_bench_value_is_symbol(refresh_scope, relation_sym) &&
                        deduce_query_bench_value_is_symbol(refresh_path, db_wide_sym) &&
                        deduce_query_bench_value_is_symbol(refresh_fallback, full_recompute_required_sym) &&
                        deduce_query_bench_value_is_int(refreshed_count, 1);
                }
                if (refresh_ok) fallback_hits++;

                long post_stats_start = deduce_bench_now_ms();
                Value* stats1 = prim_deduce_stats(stats_args[..], null, interp);
                long post_stats_done = deduce_bench_now_ms();
                if (post_stats_done > post_stats_start) stats_ms += post_stats_done - post_stats_start;

                bool post_stats_ok = stats1 != null && stats1.tag == HASHMAP && stats1.hashmap_val != null;
                if (post_stats_ok) {
                    Value* status = deduce_query_bench_dict_get(interp, stats1, "status");
                    Value* refresh_count = deduce_query_bench_dict_get(interp, stats1, "materialized-refresh-count");
                    Value* stale_reason = deduce_query_bench_dict_get(interp, stats1, "materialized-last-stale-reason");
                    Value* materialized_stale = deduce_query_bench_dict_get(interp, stats1, "materialized-stale");
                    Value* full_recompute_required = deduce_query_bench_dict_get(interp, stats1, "full-recompute-required");
                    post_stats_ok =
                        deduce_query_bench_value_is_symbol(status, ok_sym) &&
                        deduce_query_bench_value_is_int(refresh_count, 2) &&
                        is_nil(stale_reason) &&
                        is_nil(materialized_stale) &&
                        is_nil(full_recompute_required);
                }
                if (post_stats_ok) post_stats_hits++;
            }
        }

        interp.current_scope = bench_scope;
        main::scope_release(iter_scope);
    }

    interp.current_scope = bench_scope;
    *rule_change_ms_out = rule_change_ms;
    *refresh_ms_out = refresh_ms;
    *stats_ms_out = stats_ms;
    *pre_stats_hits_out = pre_stats_hits;
    *fallback_hits_out = fallback_hits;
    *post_stats_hits_out = post_stats_hits;
    return true;
}
