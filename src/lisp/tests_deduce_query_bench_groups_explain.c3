module lisp;

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
