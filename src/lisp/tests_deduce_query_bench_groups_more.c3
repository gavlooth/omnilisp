module lisp;

import std::io;
import main;

fn void run_deduce_selective_join_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled() || interp == null) return;

    Value* db = null;
    Value* out_rel = null;
    SymbolId out_sym = 0;
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
    ) && out_rel != null && out_sym != 0 && expected_rows > 0;

    if (ok) {
        io::printn("[PASS] deduce selective join benchmark smoke");
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
        io::printn("[PASS] deduce aggregate benchmark smoke");
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
        io::printn("[PASS] deduce recursive aggregate benchmark smoke");
    } else {
        io::printn("[FAIL] deduce recursive aggregate benchmark smoke");
    }
}

fn void run_deduce_scan_query_count_benchmarks(Interp* interp) {
    if (!deduce_bench_enabled() || interp == null) return;
}
