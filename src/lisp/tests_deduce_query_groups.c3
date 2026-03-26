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
const usz DEDUCE_BENCH_STATS_MAINTENANCE_SEED_ROWS = 1024;
const usz DEDUCE_BENCH_STATS_MAINTENANCE_TRACKED_ITERS = 32;
const usz DEDUCE_BENCH_STATS_MAINTENANCE_REBUILD_ITERS = 8;
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

fn Value* deduce_query_test_dict_get(Interp* interp, Value* dict, char[] key) {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return null;
    return hashmap_get(dict.hashmap_val, make_symbol(interp, interp.symbols.intern(key)));
}

fn Value* deduce_query_test_find_row_by_value(Interp* interp, Value* rows, char[] key, Value* expected) {
    Value* cur = rows;
    while (is_cons(cur)) {
        Value* row = car(cur);
        Value* actual = deduce_query_test_dict_get(interp, row, key);
        if (values_equal(actual, expected)) return row;
        cur = cdr(cur);
    }
    return null;
}

fn bool deduce_query_filter_matches(char* filter, char[] name) {
    if (filter == null) return false;
    usz len = 0;
    while (filter[len] != 0) len++;
    if (len != name.len) return false;
    for (usz i = 0; i < len; i++) {
        if (filter[i] != name[i]) return false;
    }
    return true;
}

bool deduce_query_filter_why_result_ran = false;

fn bool deduce_query_filter_try_run_targeted_why_result(Interp* interp, int* pass, int* fail) {
    char* query_filter = test_c_getenv("OMNI_DEDUCE_QUERY_FILTER");
    if (!deduce_query_filter_matches(query_filter, "why-result-bounded")) return false;
    if (!deduce_query_filter_why_result_ran) {
        deduce_query_filter_why_result_ran = true;
        run_deduce_query_why_result_bounded_context_test(interp, pass, fail);
    }
    return true;
}

fn void run_deduce_query_why_result_bounded_context_test(Interp* interp, int* pass, int* fail) {
    EvalResult r = run(
        "(block "
        "  (define db-why-result-gd-query-many (deduce 'open 'memory)) "
        "  (define [relation db-why-result-gd-query-many] edge-gd-query-many (src) (dst)) "
        "  (define [relation db-why-result-gd-query-many materialized] reach-gd-query-many (src) (dst)) "
        "  (deduce/rule! db-why-result-gd-query-many '(reach-gd-query-many ?x ?y) '(edge-gd-query-many ?x ?y)) "
        "  (deduce/rule! db-why-result-gd-query-many '(reach-gd-query-many ?x ?z) '(edge-gd-query-many ?x ?y) '(reach-gd-query-many ?y ?z)) "
        "  (deduce/fact! edge-gd-query-many 8 9) "
        "  (deduce/fact! edge-gd-query-many 9 10) "
        "  (deduce/fact! edge-gd-query-many 1 2) "
        "  (deduce/refresh! reach-gd-query-many) "
        "  (deduce/fact! edge-gd-query-many 8 10) "
        "  (define query-rows (deduce/query reach-gd-query-many (lambda (row) (= (ref row 'src) 8)))) "
        "  (define why-query-1 (deduce/why-result reach-gd-query-many 8 9)) "
        "  (define why-query-2 (deduce/why-result reach-gd-query-many 8 10)) "
        "  (define why-query-other (deduce/why-result reach-gd-query-many 1 2)) "
        "  (define query-path-gd-1 (ref (car (ref why-query-1 'paths)) 'goal-directed-read-context)) "
        "  (define query-path-gd-2 (ref (car (ref why-query-2 'paths)) 'goal-directed-read-context)) "
        "  (define query-path-gd-other (ref (car (ref why-query-other 'paths)) 'goal-directed-read-context)) "
        "  (define db-why-result-gd-match-many (deduce 'open 'memory)) "
        "  (define [relation db-why-result-gd-match-many] edge-gd-match-many (src) (dst)) "
        "  (define [relation db-why-result-gd-match-many materialized] reach-gd-match-many (src) (dst)) "
        "  (deduce/rule! db-why-result-gd-match-many '(reach-gd-match-many ?x ?y) '(edge-gd-match-many ?x ?y)) "
        "  (deduce/rule! db-why-result-gd-match-many '(reach-gd-match-many ?x ?z) '(edge-gd-match-many ?x ?y) '(reach-gd-match-many ?y ?z)) "
        "  (deduce/fact! edge-gd-match-many 8 9) "
        "  (deduce/fact! edge-gd-match-many 9 10) "
        "  (deduce/fact! edge-gd-match-many 1 2) "
        "  (deduce/refresh! reach-gd-match-many) "
        "  (deduce/fact! edge-gd-match-many 8 10) "
        "  (define match-rows (deduce/match reach-gd-match-many '(reach-gd-match-many 8 ?dst))) "
        "  (define why-match-1 (deduce/why-result reach-gd-match-many 8 9)) "
        "  (define why-match-2 (deduce/why-result reach-gd-match-many 8 10)) "
        "  (define why-match-other (deduce/why-result reach-gd-match-many 1 2)) "
        "  (define match-path-gd-1 (ref (car (ref why-match-1 'paths)) 'goal-directed-read-context)) "
        "  (define match-path-gd-2 (ref (car (ref why-match-2 'paths)) 'goal-directed-read-context)) "
        "  (define match-path-gd-other (ref (car (ref why-match-other 'paths)) 'goal-directed-read-context)) "
        "  (define db-why-result-gd-range-many (deduce 'open 'memory)) "
        "  (define [relation db-why-result-gd-range-many] edge-gd-range-many (src) (dst)) "
        "  (define [relation db-why-result-gd-range-many materialized] reach-gd-range-many (src) (dst)) "
        "  (deduce/rule! db-why-result-gd-range-many '(reach-gd-range-many ?x ?y) '(edge-gd-range-many ?x ?y)) "
        "  (deduce/rule! db-why-result-gd-range-many '(reach-gd-range-many ?x ?z) '(edge-gd-range-many ?x ?y) '(reach-gd-range-many ?y ?z)) "
        "  (deduce/fact! edge-gd-range-many 8 9) "
        "  (deduce/fact! edge-gd-range-many 9 10) "
        "  (deduce/fact! edge-gd-range-many 1 2) "
        "  (deduce/refresh! reach-gd-range-many) "
        "  (deduce/fact! edge-gd-range-many 8 10) "
        "  (define range-rows (deduce/scan-range reach-gd-range-many '(8 9) '(8 10))) "
        "  (define why-range-1 (deduce/why-result reach-gd-range-many 8 9)) "
        "  (define why-range-2 (deduce/why-result reach-gd-range-many 8 10)) "
        "  (define why-range-other (deduce/why-result reach-gd-range-many 1 2)) "
        "  (define range-path-gd-1 (ref (car (ref why-range-1 'paths)) 'goal-directed-read-context)) "
        "  (define range-path-gd-2 (ref (car (ref why-range-2 'paths)) 'goal-directed-read-context)) "
        "  (define range-path-gd-other (ref (car (ref why-range-other 'paths)) 'goal-directed-read-context)) "
        "  (define db-why-result-gd-scan-many (deduce 'open 'memory)) "
        "  (define [relation db-why-result-gd-scan-many] edge-gd-scan-many (src) (dst)) "
        "  (define [relation db-why-result-gd-scan-many materialized] reach-gd-scan-many (src) (dst)) "
        "  (deduce/rule! db-why-result-gd-scan-many '(reach-gd-scan-many ?x ?y) '(edge-gd-scan-many ?x ?y)) "
        "  (deduce/fact! edge-gd-scan-many 8 9) "
        "  (deduce/fact! edge-gd-scan-many 8 10) "
        "  (deduce/fact! edge-gd-scan-many 1 2) "
        "  (define scan-rows (deduce/scan reach-gd-scan-many)) "
        "  (define why-scan-1 (deduce/why-result reach-gd-scan-many 8 9)) "
        "  (define why-scan-2 (deduce/why-result reach-gd-scan-many 8 10)) "
        "  (define why-scan-3 (deduce/why-result reach-gd-scan-many 1 2)) "
        "  (define why-scan-other (deduce/why-result reach-gd-scan-many 9 9)) "
        "  (define scan-path-gd-1 (ref (car (ref why-scan-1 'paths)) 'goal-directed-read-context)) "
        "  (define scan-path-gd-2 (ref (car (ref why-scan-2 'paths)) 'goal-directed-read-context)) "
        "  (define scan-path-gd-3 (ref (car (ref why-scan-3 'paths)) 'goal-directed-read-context)) "
        "  (define db-why-result-gd-selector-query-many (deduce 'open 'memory)) "
        "  (define [relation db-why-result-gd-selector-query-many] edge-gd-selector-query-many (src) (dst)) "
        "  (define [relation db-why-result-gd-selector-query-many materialized] reach-gd-selector-query-many (src) (dst)) "
        "  (deduce/rule! db-why-result-gd-selector-query-many '(reach-gd-selector-query-many ?x ?y) '(edge-gd-selector-query-many ?x ?y)) "
        "  (deduce/rule! db-why-result-gd-selector-query-many '(reach-gd-selector-query-many ?x ?z) '(edge-gd-selector-query-many ?x ?y) '(reach-gd-selector-query-many ?y ?z)) "
        "  (deduce/fact! edge-gd-selector-query-many 8 9) "
        "  (deduce/fact! edge-gd-selector-query-many 9 10) "
        "  (deduce/fact! edge-gd-selector-query-many 1 2) "
        "  (deduce/refresh! reach-gd-selector-query-many) "
        "  (deduce/fact! edge-gd-selector-query-many 8 10) "
        "  (deduce/query reach-gd-selector-query-many (lambda (row) (= (ref row 'src) 8)) 0) "
        "  (define why-selector-1 (deduce/why-result reach-gd-selector-query-many 8 9)) "
        "  (define why-selector-2 (deduce/why-result reach-gd-selector-query-many 8 10)) "
        "  (define why-selector-other (deduce/why-result reach-gd-selector-query-many 1 2)) "
        "  (define selector-path-gd-1 (ref (car (ref why-selector-1 'paths)) 'goal-directed-read-context)) "
        "  (define selector-path-gd-2 (ref (car (ref why-selector-2 'paths)) 'goal-directed-read-context)) "
        "  (define selector-path-gd-other (ref (car (ref why-selector-other 'paths)) 'goal-directed-read-context)) "
        "  (and "
        "    (= (length query-rows) 2) "
        "    (= (ref query-path-gd-1 'surface) 'query) "
        "    (= (ref query-path-gd-2 'surface) 'query) "
        "    (= query-path-gd-other nil) "
        "    (= (length match-rows) 2) "
        "    (= (ref match-path-gd-1 'surface) 'match) "
        "    (= (ref match-path-gd-2 'surface) 'match) "
        "    (= match-path-gd-other nil) "
        "    (= (length range-rows) 2) "
        "    (= (ref range-path-gd-1 'surface) 'scan-range) "
        "    (= (ref range-path-gd-2 'surface) 'scan-range) "
        "    (= range-path-gd-other nil) "
        "    (= (length scan-rows) 3) "
        "    (= (ref scan-path-gd-1 'surface) 'scan) "
        "    (= (ref scan-path-gd-2 'surface) 'scan) "
        "    (= (ref scan-path-gd-3 'surface) 'scan) "
        "    (= (ref why-scan-other 'paths) nil) "
        "    (= (ref selector-path-gd-1 'surface) 'query) "
        "    (= (ref selector-path-gd-1 'selector-rule-index) 0) "
        "    (= (ref selector-path-gd-2 'selector-rule-index) 0) "
        "    (= selector-path-gd-other nil)))",
        interp
    );
    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] deduce why-result attaches bounded complete goal-directed context to matching multi-row proof paths");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce why-result attaches bounded complete goal-directed context to matching multi-row proof paths (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce why-result attaches bounded complete goal-directed context to matching multi-row proof paths");
        }
        (*fail)++;
    }
}

fn void run_deduce_match_tests(Interp* interp, int* pass, int* fail) {
    if (deduce_query_filter_try_run_targeted_why_result(interp, pass, fail)) return;

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

fn void run_deduce_empty_relation_stats_tests(Interp* interp, int* pass, int* fail) {
    if (deduce_query_filter_try_run_targeted_why_result(interp, pass, fail)) return;

    EvalResult r = run(
        "(block "
        "  (define db-empty-stats (deduce 'open 'memory)) "
        "  (define [relation db-empty-stats] empty-stats (id) (value)) "
        "  (define count0 (deduce 'count empty-stats)) "
        "  (define scan0 (deduce 'scan empty-stats)) "
        "  (and (= count0 0) (null? scan0)))",
        interp
    );
    if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
        emit_pass_literal("[PASS] deduce empty relation stats stay deterministic");
        (*pass)++;
    } else {
        if (r.error.has_error) {
            io::printfn("[FAIL] deduce empty relation stats stay deterministic (error: %s)", (ZString)r.error.message[:256]);
        } else {
            io::printn("[FAIL] deduce empty relation stats stay deterministic");
        }
        (*fail)++;
    }
}

fn void run_deduce_admin_surface_tests(Interp* interp, int* pass, int* fail) {
    if (deduce_query_filter_try_run_targeted_why_result(interp, pass, fail)) return;

    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = make_symbol(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        SymbolId rel_name = interp.symbols.intern("admin_person");
        SymbolId col_name = interp.symbols.intern("name");
        SymbolId col_age = interp.symbols.intern("age");
        Value* rel = null;
        if (ok) {
            rel = setup_deduce_relation_for_db(interp, db_val, rel_name, col_name, col_age);
            ok = rel != null && rel.tag == FFI_HANDLE && rel.ffi_val != null;
        }

        if (ok) {
            Value*[3] fact_args;
            fact_args[0] = rel;
            fact_args[1] = make_string(interp, "Alice");
            fact_args[2] = make_int(interp, 30);
            Value* inserted = prim_assert_fact(fact_args[..], null, interp);
            ok = inserted != null && inserted.tag == VOID;
        }

        Value* schema = null;
        Value* stats = null;
        Value* indexes = null;
        if (ok) {
            Value*[1] admin_args;
            admin_args[0] = rel;
            schema = prim_deduce_schema(admin_args[..], null, interp);
            stats = prim_deduce_stats(admin_args[..], null, interp);
            indexes = prim_deduce_indexes(admin_args[..], null, interp);
            ok = schema != null && schema.tag == HASHMAP && schema.hashmap_val != null &&
                 stats != null && stats.tag == HASHMAP && stats.hashmap_val != null;
        }

        if (ok) {
            Value* relation = deduce_query_test_dict_get(interp, schema, "relation");
            Value* arity = deduce_query_test_dict_get(interp, schema, "arity");
            Value* columns = deduce_query_test_dict_get(interp, schema, "columns");
            Value* key_column = deduce_query_test_dict_get(interp, schema, "key-column");
            Value* unique_column = deduce_query_test_dict_get(interp, schema, "unique-column");
            Value* index_column = deduce_query_test_dict_get(interp, schema, "index-column");
            Value* index_count = deduce_query_test_dict_get(interp, schema, "index-count");
            Value* constraint_count = deduce_query_test_dict_get(interp, schema, "constraint-count");
            Value* constraints = deduce_query_test_dict_get(interp, schema, "constraints");
            Value* cardinality = deduce_query_test_dict_get(interp, stats, "cardinality-estimate");
            Value* distinct = deduce_query_test_dict_get(interp, stats, "distinct-estimate");
            Value* dirty = deduce_query_test_dict_get(interp, stats, "dirty-predicate-count");
            Value* epoch = deduce_query_test_dict_get(interp, stats, "mutation-epoch");
            Value* first_index = is_cons(indexes) ? car(indexes) : null;
            Value* first_index_column_count = deduce_query_test_dict_get(interp, first_index, "column-count");
            Value* first_index_unique = deduce_query_test_dict_get(interp, first_index, "unique");

            usz column_len = 0;
            Value* col_cur = columns;
            while (is_cons(col_cur)) {
                column_len++;
                col_cur = cdr(col_cur);
            }

            usz index_len = 0;
            Value* index_cur = indexes;
            while (is_cons(index_cur)) {
                index_len++;
                index_cur = cdr(index_cur);
            }

            ok = relation != null && relation.tag == SYMBOL && relation.sym_val == rel_name &&
                 arity != null && arity.tag == INT && arity.int_val == 2 &&
                 column_len == 2 &&
                 key_column != null && key_column.tag == NIL &&
                 unique_column != null && unique_column.tag == NIL &&
                 index_column != null && index_column.tag == NIL &&
                 index_count != null && index_count.tag == INT && index_count.int_val == 3 &&
                 constraint_count != null && constraint_count.tag == INT && constraint_count.int_val == 0 &&
                 constraints != null && constraints.tag == NIL &&
                 cardinality != null && cardinality.tag == INT && cardinality.int_val == 1 &&
                 distinct != null && distinct.tag == INT && distinct.int_val == 1 &&
                 dirty != null && dirty.tag == INT && dirty.int_val == 1 &&
                 epoch != null && epoch.tag == INT && epoch.int_val == 1 &&
                 index_len == 3 &&
                 first_index_column_count != null && first_index_column_count.tag == INT &&
                 first_index_column_count.int_val == 2 &&
                 first_index_unique != null && first_index_unique.tag == SYMBOL &&
                 first_index_unique.sym_val == interp.sym_true;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce schema/indexes/stats surfaces return deterministic metadata");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce schema/indexes/stats surfaces return deterministic metadata");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-key (deduce 'open 'memory)) "
            "  (define [relation db-admin-key] admin-keyed (key id) (name)) "
            "  (deduce 'fact! admin-keyed 1 \"Alice\") "
            "  (define schema (deduce/schema admin-keyed)) "
            "  (define analyze (deduce/analyze db-admin-key)) "
            "  (define constraints (ref schema 'constraints)) "
            "  (define first (car constraints)) "
            "  (and (= (ref schema 'constraint-count) 1) "
            "       (and (= (ref analyze 'keyed-relation-count) 1) "
            "            (and (= (ref analyze 'integrity-constraint-count) 1) "
            "                 (and (= (ref schema 'key-column) 'id) "
            "                      (and (= (ref first 'kind) 'key) "
            "                           (and (= (ref first 'unique) true) "
            "                                (= (ref first 'enforced) true))))))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce schema/analyze surfaces expose keyed integrity metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce schema/analyze surfaces expose keyed integrity metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce schema/analyze surfaces expose keyed integrity metadata");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-gd-query-many (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-query-many] edge-gd-query-many (src) (dst)) "
            "  (define [relation db-why-result-gd-query-many materialized] reach-gd-query-many (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-query-many '(reach-gd-query-many ?x ?y) '(edge-gd-query-many ?x ?y)) "
            "  (deduce/rule! db-why-result-gd-query-many '(reach-gd-query-many ?x ?z) '(edge-gd-query-many ?x ?y) '(reach-gd-query-many ?y ?z)) "
            "  (deduce/fact! edge-gd-query-many 8 9) "
            "  (deduce/fact! edge-gd-query-many 9 10) "
            "  (deduce/fact! edge-gd-query-many 1 2) "
            "  (deduce/refresh! reach-gd-query-many) "
            "  (deduce/fact! edge-gd-query-many 8 10) "
            "  (define query-rows (deduce/query reach-gd-query-many (lambda (row) (= (ref row 'src) 8)))) "
            "  (define why-query-1 (deduce/why-result reach-gd-query-many 8 9)) "
            "  (define why-query-2 (deduce/why-result reach-gd-query-many 8 10)) "
            "  (define why-query-other (deduce/why-result reach-gd-query-many 1 2)) "
            "  (define query-path-gd-1 (ref (car (ref why-query-1 'paths)) 'goal-directed-read-context)) "
            "  (define query-path-gd-2 (ref (car (ref why-query-2 'paths)) 'goal-directed-read-context)) "
            "  (define query-path-gd-other (ref (car (ref why-query-other 'paths)) 'goal-directed-read-context)) "
            "  (define db-why-result-gd-match-many (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-match-many] edge-gd-match-many (src) (dst)) "
            "  (define [relation db-why-result-gd-match-many materialized] reach-gd-match-many (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-match-many '(reach-gd-match-many ?x ?y) '(edge-gd-match-many ?x ?y)) "
            "  (deduce/rule! db-why-result-gd-match-many '(reach-gd-match-many ?x ?z) '(edge-gd-match-many ?x ?y) '(reach-gd-match-many ?y ?z)) "
            "  (deduce/fact! edge-gd-match-many 8 9) "
            "  (deduce/fact! edge-gd-match-many 9 10) "
            "  (deduce/fact! edge-gd-match-many 1 2) "
            "  (deduce/refresh! reach-gd-match-many) "
            "  (deduce/fact! edge-gd-match-many 8 10) "
            "  (define match-rows (deduce/match reach-gd-match-many '(reach-gd-match-many 8 ?dst))) "
            "  (define why-match-1 (deduce/why-result reach-gd-match-many 8 9)) "
            "  (define why-match-2 (deduce/why-result reach-gd-match-many 8 10)) "
            "  (define why-match-other (deduce/why-result reach-gd-match-many 1 2)) "
            "  (define match-path-gd-1 (ref (car (ref why-match-1 'paths)) 'goal-directed-read-context)) "
            "  (define match-path-gd-2 (ref (car (ref why-match-2 'paths)) 'goal-directed-read-context)) "
            "  (define match-path-gd-other (ref (car (ref why-match-other 'paths)) 'goal-directed-read-context)) "
            "  (define db-why-result-gd-range-many (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-range-many] edge-gd-range-many (src) (dst)) "
            "  (define [relation db-why-result-gd-range-many materialized] reach-gd-range-many (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-range-many '(reach-gd-range-many ?x ?y) '(edge-gd-range-many ?x ?y)) "
            "  (deduce/rule! db-why-result-gd-range-many '(reach-gd-range-many ?x ?z) '(edge-gd-range-many ?x ?y) '(reach-gd-range-many ?y ?z)) "
            "  (deduce/fact! edge-gd-range-many 8 9) "
            "  (deduce/fact! edge-gd-range-many 9 10) "
            "  (deduce/fact! edge-gd-range-many 1 2) "
            "  (deduce/refresh! reach-gd-range-many) "
            "  (deduce/fact! edge-gd-range-many 8 10) "
            "  (define range-rows (deduce/scan-range reach-gd-range-many '(8 9) '(8 10))) "
            "  (define why-range-1 (deduce/why-result reach-gd-range-many 8 9)) "
            "  (define why-range-2 (deduce/why-result reach-gd-range-many 8 10)) "
            "  (define why-range-other (deduce/why-result reach-gd-range-many 1 2)) "
            "  (define range-path-gd-1 (ref (car (ref why-range-1 'paths)) 'goal-directed-read-context)) "
            "  (define range-path-gd-2 (ref (car (ref why-range-2 'paths)) 'goal-directed-read-context)) "
            "  (define range-path-gd-other (ref (car (ref why-range-other 'paths)) 'goal-directed-read-context)) "
            "  (define db-why-result-gd-scan-many (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-scan-many] edge-gd-scan-many (src) (dst)) "
            "  (define [relation db-why-result-gd-scan-many materialized] reach-gd-scan-many (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-scan-many '(reach-gd-scan-many ?x ?y) '(edge-gd-scan-many ?x ?y)) "
            "  (deduce/fact! edge-gd-scan-many 8 9) "
            "  (deduce/fact! edge-gd-scan-many 8 10) "
            "  (deduce/fact! edge-gd-scan-many 1 2) "
            "  (define scan-rows (deduce/scan reach-gd-scan-many)) "
            "  (define why-scan-1 (deduce/why-result reach-gd-scan-many 8 9)) "
            "  (define why-scan-2 (deduce/why-result reach-gd-scan-many 8 10)) "
            "  (define why-scan-3 (deduce/why-result reach-gd-scan-many 1 2)) "
            "  (define why-scan-other (deduce/why-result reach-gd-scan-many 9 9)) "
            "  (define scan-path-gd-1 (ref (car (ref why-scan-1 'paths)) 'goal-directed-read-context)) "
            "  (define scan-path-gd-2 (ref (car (ref why-scan-2 'paths)) 'goal-directed-read-context)) "
            "  (define scan-path-gd-3 (ref (car (ref why-scan-3 'paths)) 'goal-directed-read-context)) "
            "  (define db-why-result-gd-selector-query-many (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-selector-query-many] edge-gd-selector-query-many (src) (dst)) "
            "  (define [relation db-why-result-gd-selector-query-many materialized] reach-gd-selector-query-many (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-selector-query-many '(reach-gd-selector-query-many ?x ?y) '(edge-gd-selector-query-many ?x ?y)) "
            "  (deduce/rule! db-why-result-gd-selector-query-many '(reach-gd-selector-query-many ?x ?z) '(edge-gd-selector-query-many ?x ?y) '(reach-gd-selector-query-many ?y ?z)) "
            "  (deduce/fact! edge-gd-selector-query-many 8 9) "
            "  (deduce/fact! edge-gd-selector-query-many 9 10) "
            "  (deduce/fact! edge-gd-selector-query-many 1 2) "
            "  (deduce/refresh! reach-gd-selector-query-many) "
            "  (deduce/fact! edge-gd-selector-query-many 8 10) "
            "  (deduce/query reach-gd-selector-query-many (lambda (row) (= (ref row 'src) 8)) 0) "
            "  (define why-selector-1 (deduce/why-result reach-gd-selector-query-many 8 9)) "
            "  (define why-selector-2 (deduce/why-result reach-gd-selector-query-many 8 10)) "
            "  (define why-selector-other (deduce/why-result reach-gd-selector-query-many 1 2)) "
            "  (define selector-path-gd-1 (ref (car (ref why-selector-1 'paths)) 'goal-directed-read-context)) "
            "  (define selector-path-gd-2 (ref (car (ref why-selector-2 'paths)) 'goal-directed-read-context)) "
            "  (define selector-path-gd-other (ref (car (ref why-selector-other 'paths)) 'goal-directed-read-context)) "
            "  (and "
            "    (= (length query-rows) 2) "
            "    (= (ref query-path-gd-1 'surface) 'query) "
            "    (= (ref query-path-gd-2 'surface) 'query) "
            "    (= query-path-gd-other nil) "
            "    (= (length match-rows) 2) "
            "    (= (ref match-path-gd-1 'surface) 'match) "
            "    (= (ref match-path-gd-2 'surface) 'match) "
            "    (= match-path-gd-other nil) "
            "    (= (length range-rows) 2) "
            "    (= (ref range-path-gd-1 'surface) 'scan-range) "
            "    (= (ref range-path-gd-2 'surface) 'scan-range) "
            "    (= range-path-gd-other nil) "
            "    (= (length scan-rows) 3) "
            "    (= (ref scan-path-gd-1 'surface) 'scan) "
            "    (= (ref scan-path-gd-2 'surface) 'scan) "
            "    (= (ref scan-path-gd-3 'surface) 'scan) "
            "    (= (ref why-scan-other 'paths) nil) "
            "    (= (ref selector-path-gd-1 'surface) 'query) "
            "    (= (ref selector-path-gd-1 'selector-rule-index) 0) "
            "    (= (ref selector-path-gd-2 'selector-rule-index) 0) "
            "    (= selector-path-gd-other nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result attaches bounded complete goal-directed context to matching multi-row proof paths");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result attaches bounded complete goal-directed context to matching multi-row proof paths (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result attaches bounded complete goal-directed context to matching multi-row proof paths");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-gd-many-overflow (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-many-overflow] edge-gd-many-overflow (src) (dst)) "
            "  (define [relation db-why-result-gd-many-overflow materialized] reach-gd-many-overflow (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-many-overflow '(reach-gd-many-overflow ?x ?y) '(edge-gd-many-overflow ?x ?y)) "
            "  (deduce/rule! db-why-result-gd-many-overflow '(reach-gd-many-overflow ?x ?z) '(edge-gd-many-overflow ?x ?y) '(reach-gd-many-overflow ?y ?z)) "
            "  (deduce/fact! edge-gd-many-overflow 0 0) "
            "  (deduce/refresh! reach-gd-many-overflow) "
            "  (deduce/fact! edge-gd-many-overflow 8 1) "
            "  (deduce/fact! edge-gd-many-overflow 8 2) "
            "  (deduce/fact! edge-gd-many-overflow 8 3) "
            "  (deduce/fact! edge-gd-many-overflow 8 4) "
            "  (deduce/fact! edge-gd-many-overflow 8 5) "
            "  (deduce/fact! edge-gd-many-overflow 8 6) "
            "  (deduce/fact! edge-gd-many-overflow 8 7) "
            "  (deduce/fact! edge-gd-many-overflow 8 8) "
            "  (deduce/fact! edge-gd-many-overflow 8 9) "
            "  (define rows (deduce/query reach-gd-many-overflow (lambda (row) (= (ref row 'src) 8)))) "
            "  (define why (deduce/why-result reach-gd-many-overflow 0 0)) "
            "  (define path-gd (ref (car (ref why 'paths)) 'goal-directed-read-context)) "
            "  (and "
            "    (= (length rows) 9) "
            "    (= (ref why 'status) 'ok) "
            "    (= (dict? (ref why 'goal-directed-read-context)) true) "
            "    (= path-gd nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result keeps only top-level goal-directed context when the matching row set exceeds the bounded path-local capture limit");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result keeps only top-level goal-directed context when the matching row set exceeds the bounded path-local capture limit (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result keeps only top-level goal-directed context when the matching row set exceeds the bounded path-local capture limit");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-gd (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd] edge-gd-why (src) (dst)) "
            "  (define [relation db-why-result-gd materialized] reach-gd-why (src) (dst)) "
            "  (deduce/rule! db-why-result-gd '(reach-gd-why ?x ?y) '(edge-gd-why ?x ?y)) "
            "  (deduce/rule! db-why-result-gd '(reach-gd-why ?x ?z) '(edge-gd-why ?x ?y) '(reach-gd-why ?y ?z)) "
            "  (deduce/fact! edge-gd-why 1 2) "
            "  (deduce/fact! edge-gd-why 2 3) "
            "  (deduce/refresh! reach-gd-why) "
            "  (deduce/fact! edge-gd-why 3 4) "
            "  (define rows (deduce/query reach-gd-why (lambda (row) (= (ref row 'dst) 4)))) "
            "  (define why (deduce/why-result reach-gd-why 1 4)) "
            "  (define gd (ref why 'goal-directed-read-context)) "
            "  (and "
            "    (= rows nil) "
            "    (= (ref why 'status) 'missing) "
            "    (= (dict? gd) true) "
            "    (= (ref gd 'relation) 'reach-gd-why) "
            "    (= (ref gd 'execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref gd 'surface) 'query) "
            "    (= (ref gd 'fallback-reason) nil) "
            "    (= (ref gd 'selector-rule-index) nil) "
            "    (= (ref gd 'requested-bound-count) 1) "
            "    (= (length (ref gd 'requested-bound-positions)) 1) "
            "    (= (car (ref gd 'requested-bound-positions)) 1) "
            "    (= (ref gd 'applied-bound-count) 1) "
            "    (= (length (ref gd 'applied-bound-positions)) 1) "
            "    (= (car (ref gd 'applied-bound-positions)) 1) "
            "    (= (ref why 'path-count) 0) "
            "    (= (ref why 'paths) nil) "
            "    (= (length (ref gd 'step-counters)) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result exposes last goal-directed read context as optional metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result exposes last goal-directed read context as optional metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result exposes last goal-directed read context as optional metadata");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-gd-path (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-path] edge-gd-path-why (src) (dst)) "
            "  (define [relation db-why-result-gd-path materialized] reach-gd-path-why (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-path '(reach-gd-path-why ?x ?y) '(edge-gd-path-why ?x ?y)) "
            "  (deduce/rule! db-why-result-gd-path '(reach-gd-path-why ?x ?z) '(edge-gd-path-why ?x ?y) '(reach-gd-path-why ?y ?z)) "
            "  (deduce/fact! edge-gd-path-why 8 9) "
            "  (deduce/fact! edge-gd-path-why 9 10) "
            "  (deduce/fact! edge-gd-path-why 1 2) "
            "  (deduce/refresh! reach-gd-path-why) "
            "  (deduce/fact! edge-gd-path-why 10 11) "
            "  (define rows (deduce/query reach-gd-path-why (lambda (row) (and (= (ref row 'src) 8) (= (ref row 'dst) 10))))) "
            "  (define why (deduce/why-result reach-gd-path-why 8 10)) "
            "  (define path-gd (ref (car (ref why 'paths)) 'goal-directed-read-context)) "
            "  (define other (deduce/why-result reach-gd-path-why 8 9)) "
            "  (define other-path-gd (ref (car (ref other 'paths)) 'goal-directed-read-context)) "
            "  (and "
            "    (= (length rows) 1) "
            "    (= (ref why 'status) 'ok) "
            "    (= (length (ref why 'paths)) 1) "
            "    (= (dict? path-gd) true) "
            "    (= (ref path-gd 'relation) 'reach-gd-path-why) "
            "    (= (ref path-gd 'execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref path-gd 'surface) 'query) "
            "    (= (ref path-gd 'fallback-reason) nil) "
            "    (= (ref path-gd 'selector-rule-index) nil) "
            "    (= (ref path-gd 'requested-bound-count) 2) "
            "    (= (length (ref path-gd 'requested-bound-positions)) 2) "
            "    (= (car (ref path-gd 'requested-bound-positions)) 0) "
            "    (= (car (cdr (ref path-gd 'requested-bound-positions))) 1) "
            "    (= (ref path-gd 'applied-bound-count) 1) "
            "    (= (length (ref path-gd 'applied-bound-positions)) 1) "
            "    (= (car (ref path-gd 'applied-bound-positions)) 1) "
            "    (= (dict? (ref why 'goal-directed-read-context)) true) "
            "    (= other-path-gd nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result attaches exact-one goal-directed query context to the matching proof path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result attaches exact-one goal-directed query context to the matching proof path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result attaches exact-one goal-directed query context to the matching proof path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-gd-match-path (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-match-path] edge-gd-match-path-why (src) (dst)) "
            "  (define [relation db-why-result-gd-match-path materialized] reach-gd-match-path-why (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-match-path '(reach-gd-match-path-why ?x ?y) '(edge-gd-match-path-why ?x ?y)) "
            "  (deduce/rule! db-why-result-gd-match-path '(reach-gd-match-path-why ?x ?z) '(edge-gd-match-path-why ?x ?y) '(reach-gd-match-path-why ?y ?z)) "
            "  (deduce/fact! edge-gd-match-path-why 8 9) "
            "  (deduce/fact! edge-gd-match-path-why 9 10) "
            "  (deduce/fact! edge-gd-match-path-why 1 2) "
            "  (deduce/refresh! reach-gd-match-path-why) "
            "  (deduce/fact! edge-gd-match-path-why 10 11) "
            "  (define matches (deduce/match reach-gd-match-path-why '(reach-gd-match-path-why 8 10))) "
            "  (define why (deduce/why-result reach-gd-match-path-why 8 10)) "
            "  (define path-gd (ref (car (ref why 'paths)) 'goal-directed-read-context)) "
            "  (define other (deduce/why-result reach-gd-match-path-why 8 9)) "
            "  (define other-path-gd (ref (car (ref other 'paths)) 'goal-directed-read-context)) "
            "  (and "
            "    (= (length matches) 1) "
            "    (= (ref why 'status) 'ok) "
            "    (= (length (ref why 'paths)) 1) "
            "    (= (dict? path-gd) true) "
            "    (= (ref path-gd 'relation) 'reach-gd-match-path-why) "
            "    (= (ref path-gd 'execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref path-gd 'surface) 'match) "
            "    (= (ref path-gd 'fallback-reason) nil) "
            "    (= (ref path-gd 'selector-rule-index) nil) "
            "    (= (ref path-gd 'requested-bound-count) 2) "
            "    (= (length (ref path-gd 'requested-bound-positions)) 2) "
            "    (= (car (ref path-gd 'requested-bound-positions)) 0) "
            "    (= (car (cdr (ref path-gd 'requested-bound-positions))) 1) "
            "    (= (ref path-gd 'applied-bound-count) 1) "
            "    (= (length (ref path-gd 'applied-bound-positions)) 1) "
            "    (= (car (ref path-gd 'applied-bound-positions)) 1) "
            "    (= (dict? (ref why 'goal-directed-read-context)) true) "
            "    (= other-path-gd nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result attaches exact-one goal-directed match context to the matching proof path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result attaches exact-one goal-directed match context to the matching proof path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result attaches exact-one goal-directed match context to the matching proof path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-gd-range-path (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-range-path] edge-gd-range-path-why (src) (dst)) "
            "  (define [relation db-why-result-gd-range-path materialized] reach-gd-range-path-why (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-range-path '(reach-gd-range-path-why ?x ?y) '(edge-gd-range-path-why ?x ?y)) "
            "  (deduce/rule! db-why-result-gd-range-path '(reach-gd-range-path-why ?x ?z) '(edge-gd-range-path-why ?x ?y) '(reach-gd-range-path-why ?y ?z)) "
            "  (deduce/fact! edge-gd-range-path-why 8 9) "
            "  (deduce/fact! edge-gd-range-path-why 9 10) "
            "  (deduce/fact! edge-gd-range-path-why 1 2) "
            "  (deduce/refresh! reach-gd-range-path-why) "
            "  (deduce/fact! edge-gd-range-path-why 10 11) "
            "  (define rows (deduce/scan-range reach-gd-range-path-why '(8 10) '(8 10))) "
            "  (define why (deduce/why-result reach-gd-range-path-why 8 10)) "
            "  (define path-gd (ref (car (ref why 'paths)) 'goal-directed-read-context)) "
            "  (define other (deduce/why-result reach-gd-range-path-why 8 9)) "
            "  (define other-path-gd (ref (car (ref other 'paths)) 'goal-directed-read-context)) "
            "  (and "
            "    (= (length rows) 1) "
            "    (= (ref why 'status) 'ok) "
            "    (= (length (ref why 'paths)) 1) "
            "    (= (dict? path-gd) true) "
            "    (= (ref path-gd 'relation) 'reach-gd-range-path-why) "
            "    (= (ref path-gd 'execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref path-gd 'surface) 'scan-range) "
            "    (= (ref path-gd 'fallback-reason) nil) "
            "    (= (ref path-gd 'selector-rule-index) nil) "
            "    (= (ref path-gd 'requested-bound-count) 2) "
            "    (= (length (ref path-gd 'requested-bound-positions)) 2) "
            "    (= (car (ref path-gd 'requested-bound-positions)) 0) "
            "    (= (car (cdr (ref path-gd 'requested-bound-positions))) 1) "
            "    (= (ref path-gd 'applied-bound-count) 1) "
            "    (= (length (ref path-gd 'applied-bound-positions)) 1) "
            "    (= (car (ref path-gd 'applied-bound-positions)) 1) "
            "    (= (dict? (ref why 'goal-directed-read-context)) true) "
            "    (= other-path-gd nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result attaches exact-one goal-directed scan-range context to the matching proof path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result attaches exact-one goal-directed scan-range context to the matching proof path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result attaches exact-one goal-directed scan-range context to the matching proof path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-gd-scan-path (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-scan-path] edge-gd-scan-path-why (src) (dst)) "
            "  (define [relation db-why-result-gd-scan-path materialized] reach-gd-scan-path-why (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-scan-path '(reach-gd-scan-path-why ?x ?y) '(edge-gd-scan-path-why ?x ?y)) "
            "  (deduce/fact! edge-gd-scan-path-why 8 10) "
            "  (define rows (deduce/scan reach-gd-scan-path-why)) "
            "  (define why (deduce/why-result reach-gd-scan-path-why 8 10)) "
            "  (define path-gd (ref (car (ref why 'paths)) 'goal-directed-read-context)) "
            "  (define other (deduce/why-result reach-gd-scan-path-why 1 2)) "
            "  (and "
            "    (= (length rows) 1) "
            "    (= (ref why 'status) 'ok) "
            "    (= (length (ref why 'paths)) 1) "
            "    (= (dict? path-gd) true) "
            "    (= (ref path-gd 'relation) 'reach-gd-scan-path-why) "
            "    (= (ref path-gd 'execution-path) 'full-db-fixpoint) "
            "    (= (ref path-gd 'surface) 'scan) "
            "    (= (ref path-gd 'fallback-reason) 'full-recompute-required) "
            "    (= (ref path-gd 'selector-rule-index) nil) "
            "    (= (dict? (ref why 'goal-directed-read-context)) true) "
            "    (= (ref other 'status) 'missing) "
            "    (= (ref other 'paths) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result attaches exact-one goal-directed scan context to the matching proof path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result attaches exact-one goal-directed scan context to the matching proof path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result attaches exact-one goal-directed scan context to the matching proof path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-gd-selector-query (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-selector-query] edge-gd-selector-query (src) (dst)) "
            "  (define [relation db-why-result-gd-selector-query materialized] reach-gd-selector-query (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-selector-query '(reach-gd-selector-query ?x ?y) '(edge-gd-selector-query ?x ?y)) "
            "  (deduce/rule! db-why-result-gd-selector-query '(reach-gd-selector-query ?x ?z) '(edge-gd-selector-query ?x ?y) '(reach-gd-selector-query ?y ?z)) "
            "  (deduce/fact! edge-gd-selector-query 8 9) "
            "  (deduce/fact! edge-gd-selector-query 9 10) "
            "  (deduce/fact! edge-gd-selector-query 1 2) "
            "  (deduce/refresh! reach-gd-selector-query) "
            "  (deduce/fact! edge-gd-selector-query 10 11) "
            "  (deduce/query reach-gd-selector-query (lambda (row) (and (= (ref row 'src) 8) (= (ref row 'dst) 10))) 1) "
            "  (define why-query (deduce/why-result reach-gd-selector-query 8 10)) "
            "  (define query-path-gd (ref (car (ref why-query 'paths)) 'goal-directed-read-context)) "
            "  (define other-query (deduce/why-result reach-gd-selector-query 8 9)) "
            "  (define other-query-path-gd (ref (car (ref other-query 'paths)) 'goal-directed-read-context)) "
            "  (define db-why-result-gd-selector-match (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-selector-match] edge-gd-selector-match (src) (dst)) "
            "  (define [relation db-why-result-gd-selector-match materialized] reach-gd-selector-match (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-selector-match '(reach-gd-selector-match ?x ?y) '(edge-gd-selector-match ?x ?y)) "
            "  (deduce/rule! db-why-result-gd-selector-match '(reach-gd-selector-match ?x ?z) '(edge-gd-selector-match ?x ?y) '(reach-gd-selector-match ?y ?z)) "
            "  (deduce/fact! edge-gd-selector-match 8 9) "
            "  (deduce/fact! edge-gd-selector-match 9 10) "
            "  (deduce/fact! edge-gd-selector-match 1 2) "
            "  (deduce/refresh! reach-gd-selector-match) "
            "  (deduce/fact! edge-gd-selector-match 10 11) "
            "  (deduce/match reach-gd-selector-match '(reach-gd-selector-match 8 10) 1) "
            "  (define why-match (deduce/why-result reach-gd-selector-match 8 10)) "
            "  (define match-path-gd (ref (car (ref why-match 'paths)) 'goal-directed-read-context)) "
            "  (define other-match (deduce/why-result reach-gd-selector-match 8 9)) "
            "  (define other-match-path-gd (ref (car (ref other-match 'paths)) 'goal-directed-read-context)) "
            "  (define db-why-result-gd-selector-range (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-selector-range] edge-gd-selector-range (src) (dst)) "
            "  (define [relation db-why-result-gd-selector-range materialized] reach-gd-selector-range (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-selector-range '(reach-gd-selector-range ?x ?y) '(edge-gd-selector-range ?x ?y)) "
            "  (deduce/rule! db-why-result-gd-selector-range '(reach-gd-selector-range ?x ?z) '(edge-gd-selector-range ?x ?y) '(reach-gd-selector-range ?y ?z)) "
            "  (deduce/fact! edge-gd-selector-range 8 9) "
            "  (deduce/fact! edge-gd-selector-range 9 10) "
            "  (deduce/fact! edge-gd-selector-range 1 2) "
            "  (deduce/refresh! reach-gd-selector-range) "
            "  (deduce/fact! edge-gd-selector-range 10 11) "
            "  (deduce/scan-range reach-gd-selector-range '(8 10) '(8 10) 1) "
            "  (define why-range (deduce/why-result reach-gd-selector-range 8 10)) "
            "  (define range-path-gd (ref (car (ref why-range 'paths)) 'goal-directed-read-context)) "
            "  (define other-range (deduce/why-result reach-gd-selector-range 8 9)) "
            "  (define other-range-path-gd (ref (car (ref other-range 'paths)) 'goal-directed-read-context)) "
            "  (define db-why-result-gd-selector-scan (deduce 'open 'memory)) "
            "  (define [relation db-why-result-gd-selector-scan] edge-gd-selector-scan (src) (dst)) "
            "  (define [relation db-why-result-gd-selector-scan materialized] reach-gd-selector-scan (src) (dst)) "
            "  (deduce/rule! db-why-result-gd-selector-scan '(reach-gd-selector-scan ?x ?y) '(edge-gd-selector-scan ?x ?y)) "
            "  (deduce/rule! db-why-result-gd-selector-scan '(reach-gd-selector-scan ?x ?z) '(edge-gd-selector-scan ?x ?y) '(reach-gd-selector-scan ?y ?z)) "
            "  (deduce/fact! edge-gd-selector-scan 8 9) "
            "  (deduce/fact! edge-gd-selector-scan 9 10) "
            "  (deduce/fact! edge-gd-selector-scan 1 2) "
            "  (deduce/refresh! reach-gd-selector-scan) "
            "  (deduce/fact! edge-gd-selector-scan 10 11) "
            "  (deduce/scan reach-gd-selector-scan 1) "
            "  (define why-scan-1 (deduce/why-result reach-gd-selector-scan 8 9)) "
            "  (define why-scan-2 (deduce/why-result reach-gd-selector-scan 8 10)) "
            "  (define why-scan-3 (deduce/why-result reach-gd-selector-scan 1 2)) "
            "  (define scan-path-gd-1 (ref (car (ref why-scan-1 'paths)) 'goal-directed-read-context)) "
            "  (define scan-path-gd-2 (ref (car (ref why-scan-2 'paths)) 'goal-directed-read-context)) "
            "  (define scan-path-gd-3 (ref (car (ref why-scan-3 'paths)) 'goal-directed-read-context)) "
            "  (and "
            "    (= (ref query-path-gd 'surface) 'query) "
            "    (= (ref query-path-gd 'selector-rule-index) 1) "
            "    (= other-query-path-gd nil) "
            "    (= (ref match-path-gd 'surface) 'match) "
            "    (= (ref match-path-gd 'selector-rule-index) 1) "
            "    (= other-match-path-gd nil) "
            "    (= (ref range-path-gd 'surface) 'scan-range) "
            "    (= (ref range-path-gd 'selector-rule-index) 1) "
            "    (= other-range-path-gd nil) "
            "    (= (ref scan-path-gd-1 'surface) 'scan) "
            "    (= (ref scan-path-gd-1 'selector-rule-index) 1) "
            "    (= (ref scan-path-gd-2 'selector-rule-index) 1) "
            "    (= (ref scan-path-gd-3 'selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result keeps bounded path-local goal-directed context in selector-scoped query, match, scan, and scan-range reads");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result keeps bounded path-local goal-directed context in selector-scoped query, match, scan, and scan-range reads (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result keeps bounded path-local goal-directed context in selector-scoped query, match, scan, and scan-range reads");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-rec (deduce 'open 'memory)) "
            "  (define [relation db-why-result-rec] edge-rec-why (src) (dst)) "
            "  (define [relation db-why-result-rec materialized] reach-rec-why (src) (dst)) "
            "  (deduce/rule! db-why-result-rec '(reach-rec-why ?x ?y) '(edge-rec-why ?x ?y)) "
            "  (deduce/rule! db-why-result-rec '(reach-rec-why ?x ?z) '(edge-rec-why ?x ?y) '(reach-rec-why ?y ?z)) "
            "  (deduce/fact! edge-rec-why 1 2) "
            "  (deduce/fact! edge-rec-why 2 3) "
            "  (deduce/fact! edge-rec-why 1 4) "
            "  (deduce/fact! edge-rec-why 4 3) "
            "  (deduce/fact! edge-rec-why 8 9) "
            "  (deduce/fact! edge-rec-why 9 10) "
            "  (deduce/refresh! reach-rec-why) "
            "  (define direct (deduce/why-result reach-rec-why 1 2)) "
            "  (define recursive-ok (deduce/why-result reach-rec-why 8 10)) "
            "  (define recursive-partial (deduce/why-result reach-rec-why 1 3)) "
            "  (define direct-path (car (ref direct 'paths))) "
            "  (define recursive-ok-path (car (ref recursive-ok 'paths))) "
            "  (define recursive-partial-path (car (ref recursive-partial 'paths))) "
            "  (define recursive-ok-support (ref recursive-ok-path 'support)) "
            "  (define recursive-ok-step (car (reverse recursive-ok-support))) "
            "  (define recursive-partial-support (ref recursive-partial-path 'support)) "
            "  (define recursive-partial-step (car (reverse recursive-partial-support))) "
            "  (and "
            "    (= (ref direct 'status) 'ok) "
            "    (= (ref direct 'path-count) 1) "
            "    (= (ref direct 'max-depth) 2) "
            "    (= (ref direct 'truncated) false) "
            "    (= (ref direct-path 'path-kind) 'derived) "
            "    (= (ref direct-path 'rule-index) 0) "
            "    (= (length (ref direct-path 'support)) 1) "
            "    (= (ref recursive-ok 'status) 'ok) "
            "    (= (ref recursive-ok 'path-count) 1) "
            "    (= (ref recursive-ok 'max-depth) 3) "
            "    (= (ref recursive-ok 'truncated) false) "
            "    (= (ref recursive-ok-path 'path-kind) 'derived) "
            "    (= (ref recursive-ok-path 'rule-index) 1) "
            "    (= (length recursive-ok-support) 3) "
            "    (= (ref recursive-ok-step 'kind) 'rule-step) "
            "    (= (ref recursive-ok-step 'predicate) 'reach-rec-why) "
            "    (= (ref recursive-ok-step 'rule-index) 0) "
            "    (= (ref recursive-ok-step 'step-index) 1) "
            "    (= (ref recursive-partial 'status) 'partial) "
            "    (= (ref recursive-partial 'path-count) 1) "
            "    (= (ref recursive-partial 'max-depth) 3) "
            "    (= (ref recursive-partial 'truncated) true) "
            "    (= (ref recursive-partial-path 'path-kind) 'derived) "
            "    (= (ref recursive-partial-path 'rule-index) 1) "
            "    (= (length recursive-partial-support) 3) "
            "    (= (ref recursive-partial-step 'kind) 'rule-step) "
            "    (= (ref recursive-partial-step 'predicate) 'reach-rec-why) "
            "    (= (ref recursive-partial-step 'rule-index) 0) "
            "    (= (ref recursive-partial-step 'step-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result exposes positive recursive closure ok/partial payloads");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result exposes positive recursive closure ok/partial payloads (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result exposes positive recursive closure ok/partial payloads");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-proof-gd (deduce 'open 'memory)) "
            "  (define [relation db-why-result-proof-gd] parent-proof-gd (src) (dst)) "
            "  (define [relation db-why-result-proof-gd materialized] ancestor-proof-gd (src) (dst)) "
            "  (define [relation db-why-result-proof-gd materialized] reach-proof-gd (src) (dst)) "
            "  (deduce/rule! db-why-result-proof-gd '(ancestor-proof-gd ?x ?y) '(parent-proof-gd ?x ?y)) "
            "  (deduce/rule! db-why-result-proof-gd '(reach-proof-gd ?x ?z) '(parent-proof-gd ?x ?y) '(ancestor-proof-gd ?y ?z)) "
            "  (deduce/fact! parent-proof-gd 8 9) "
            "  (deduce/fact! parent-proof-gd 9 10) "
            "  (deduce/refresh! ancestor-proof-gd) "
            "  (deduce/refresh! reach-proof-gd) "
            "  (deduce/scan-range ancestor-proof-gd '(9 10) '(9 10)) "
            "  (define why (deduce/why-result reach-proof-gd 8 10)) "
            "  (define path (car (ref why 'paths))) "
            "  (define support (ref path 'support)) "
            "  (define derived-step (car (reverse support))) "
            "  (define path-gd (ref path 'goal-directed-read-context)) "
            "  (define top-gd (ref why 'goal-directed-read-context)) "
            "  (define step-gd (ref derived-step 'goal-directed-read-context)) "
            "  (and "
            "    (= (ref why 'status) 'ok) "
            "    (= (dict? top-gd) true) "
            "    (= (dict? path-gd) true) "
            "    (= (ref derived-step 'kind) 'rule-step) "
            "    (= (ref derived-step 'predicate) 'ancestor-proof-gd) "
            "    (= (ref derived-step 'tuple) '(9 10)) "
            "    (= (ref path-gd 'surface) 'scan-range) "
            "    (= (ref path-gd 'execution-path) 'no-op) "
            "    (= (ref path-gd 'selector-rule-index) nil) "
            "    (= (dict? step-gd) true) "
            "    (= (ref step-gd 'surface) 'scan-range) "
            "    (= (ref step-gd 'execution-path) 'no-op) "
            "    (= (ref step-gd 'selector-rule-index) nil) "
            "    (= (ref step-gd 'requested-bound-positions) nil) "
            "    (= (ref step-gd 'applied-bound-positions) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result attaches goal-directed context to matching derived support frames");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result attaches goal-directed context to matching derived support frames (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result attaches goal-directed context to matching derived support frames");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-proof-gd-facts (deduce 'open 'memory)) "
            "  (define [relation db-why-result-proof-gd-facts] parent-proof-gd-facts (src) (dst)) "
            "  (define [relation db-why-result-proof-gd-facts materialized] reach-proof-gd-facts (src) (dst)) "
            "  (deduce/rule! db-why-result-proof-gd-facts '(reach-proof-gd-facts ?x ?z) '(parent-proof-gd-facts ?x ?y) '(parent-proof-gd-facts ?y ?z)) "
            "  (deduce/fact! parent-proof-gd-facts 8 9) "
            "  (deduce/fact! parent-proof-gd-facts 9 10) "
            "  (deduce/refresh! reach-proof-gd-facts) "
            "  (deduce/query parent-proof-gd-facts (lambda (row) true)) "
            "  (define why (deduce/why-result reach-proof-gd-facts 8 10)) "
            "  (define path (car (ref why 'paths))) "
            "  (define support (ref path 'support)) "
            "  (define fact-1 (car support)) "
            "  (define fact-2 (car (cdr support))) "
            "  (define top-gd (ref why 'goal-directed-read-context)) "
            "  (define path-gd (ref path 'goal-directed-read-context)) "
            "  (define fact-1-gd (ref fact-1 'goal-directed-read-context)) "
            "  (define fact-2-gd (ref fact-2 'goal-directed-read-context)) "
            "  (and "
            "    (= (ref why 'status) 'ok) "
            "    (= (dict? top-gd) nil) "
            "    (= (dict? path-gd) true) "
            "    (= (ref fact-1 'kind) 'fact) "
            "    (= (ref fact-1 'predicate) 'parent-proof-gd-facts) "
            "    (= (ref fact-1 'tuple) '(8 9)) "
            "    (= (ref fact-2 'kind) 'fact) "
            "    (= (ref fact-2 'predicate) 'parent-proof-gd-facts) "
            "    (= (ref fact-2 'tuple) '(9 10)) "
            "    (= (ref path-gd 'surface) 'query) "
            "    (= (ref path-gd 'execution-path) 'no-op) "
            "    (= (ref path-gd 'selector-rule-index) nil) "
            "    (= (dict? fact-1-gd) true) "
            "    (= (dict? fact-2-gd) true) "
            "    (= (ref fact-1-gd 'surface) 'query) "
            "    (= (ref fact-2-gd 'surface) 'query) "
            "    (= (ref fact-1-gd 'execution-path) 'no-op) "
            "    (= (ref fact-2-gd 'execution-path) 'no-op) "
            "    (= (ref fact-1-gd 'selector-rule-index) nil) "
            "    (= (ref fact-2-gd 'selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result attaches goal-directed context to matching fact support frames");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result attaches goal-directed context to matching fact support frames (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result attaches goal-directed context to matching fact support frames");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-proof-gd-mixed (deduce 'open 'memory)) "
            "  (define [relation db-why-result-proof-gd-mixed] parent-proof-gd-mixed (src) (dst)) "
            "  (define [relation db-why-result-proof-gd-mixed materialized] ancestor-proof-gd-mixed (src) (dst)) "
            "  (define [relation db-why-result-proof-gd-mixed materialized] reach-proof-gd-mixed (src) (dst)) "
            "  (deduce/rule! db-why-result-proof-gd-mixed '(ancestor-proof-gd-mixed ?x ?y) '(parent-proof-gd-mixed ?x ?y)) "
            "  (deduce/rule! db-why-result-proof-gd-mixed '(reach-proof-gd-mixed ?x ?z) '(parent-proof-gd-mixed ?x ?y) '(ancestor-proof-gd-mixed ?y ?z)) "
            "  (deduce/fact! parent-proof-gd-mixed 8 9) "
            "  (deduce/fact! parent-proof-gd-mixed 9 10) "
            "  (deduce/refresh! ancestor-proof-gd-mixed) "
            "  (deduce/refresh! reach-proof-gd-mixed) "
            "  (deduce/query parent-proof-gd-mixed (lambda (row) true)) "
            "  (deduce/scan-range ancestor-proof-gd-mixed '(9 10) '(9 10)) "
            "  (define why (deduce/why-result reach-proof-gd-mixed 8 10)) "
            "  (define path (car (ref why 'paths))) "
            "  (define support (ref path 'support)) "
            "  (define fact-frame (car support)) "
            "  (define step-frame (car (reverse support))) "
            "  (define path-gd (ref path 'goal-directed-read-context)) "
            "  (define path-gds (ref path 'goal-directed-read-contexts)) "
            "  (define path-gd-1 (car path-gds)) "
            "  (define path-gd-2 (car (cdr path-gds))) "
            "  (define path-rel-1 (ref path-gd-1 'relation)) "
            "  (define path-rel-2 (ref path-gd-2 'relation)) "
            "  (define fact-gd (ref fact-frame 'goal-directed-read-context)) "
            "  (define step-gd (ref step-frame 'goal-directed-read-context)) "
            "  (and "
            "    (= (ref why 'status) 'ok) "
            "    (= path-gd nil) "
            "    (= (length path-gds) 2) "
            "    (or "
            "      (and (= path-rel-1 'parent-proof-gd-mixed) (= path-rel-2 'ancestor-proof-gd-mixed)) "
            "      (and (= path-rel-1 'ancestor-proof-gd-mixed) (= path-rel-2 'parent-proof-gd-mixed))) "
            "    (= (dict? fact-gd) true) "
            "    (= (dict? step-gd) true) "
            "    (= (ref fact-gd 'relation) 'parent-proof-gd-mixed) "
            "    (= (ref step-gd 'relation) 'ancestor-proof-gd-mixed)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result exposes path-level goal-directed context lists across mixed support-frame relations");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result exposes path-level goal-directed context lists across mixed support-frame relations (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result exposes path-level goal-directed context lists across mixed support-frame relations");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-noop-query (deduce 'open 'memory)) "
            "  (define [relation db-why-result-noop-query] parent-noop-query (src) (dst)) "
            "  (define [relation db-why-result-noop-query materialized] ancestor-noop-query (src) (dst)) "
            "  (deduce/rule! db-why-result-noop-query '(ancestor-noop-query ?x ?y) '(parent-noop-query ?x ?y)) "
            "  (deduce/fact! parent-noop-query 8 9) "
            "  (deduce/fact! parent-noop-query 9 10) "
            "  (deduce/refresh! ancestor-noop-query) "
            "  (deduce/query ancestor-noop-query (lambda (row) (= (ref row 'src) 9))) "
            "  (define why (deduce/why-result ancestor-noop-query 9 10)) "
            "  (define path-gd (ref (car (ref why 'paths)) 'goal-directed-read-context)) "
            "  (and "
            "    (= (ref why 'status) 'ok) "
            "    (= (dict? path-gd) true) "
            "    (= (ref path-gd 'surface) 'query) "
            "    (= (ref path-gd 'execution-path) 'no-op) "
            "    (= (ref path-gd 'selector-rule-index) nil) "
            "    (= (ref path-gd 'requested-bound-positions) nil) "
            "    (= (ref path-gd 'applied-bound-positions) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result attaches bounded path-local context after plain no-op query reads");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result attaches bounded path-local context after plain no-op query reads (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result attaches bounded path-local context after plain no-op query reads");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-noop-match (deduce 'open 'memory)) "
            "  (define [relation db-why-result-noop-match] parent-noop-match (src) (dst)) "
            "  (define [relation db-why-result-noop-match materialized] ancestor-noop-match (src) (dst)) "
            "  (deduce/rule! db-why-result-noop-match '(ancestor-noop-match ?x ?y) '(parent-noop-match ?x ?y)) "
            "  (deduce/fact! parent-noop-match 8 9) "
            "  (deduce/fact! parent-noop-match 9 10) "
            "  (deduce/refresh! ancestor-noop-match) "
            "  (deduce/match ancestor-noop-match '(ancestor-noop-match 9 10)) "
            "  (define why (deduce/why-result ancestor-noop-match 9 10)) "
            "  (define path-gd (ref (car (ref why 'paths)) 'goal-directed-read-context)) "
            "  (and "
            "    (= (ref why 'status) 'ok) "
            "    (= (dict? path-gd) true) "
            "    (= (ref path-gd 'surface) 'match) "
            "    (= (ref path-gd 'execution-path) 'no-op) "
            "    (= (ref path-gd 'selector-rule-index) nil) "
            "    (= (ref path-gd 'requested-bound-positions) nil) "
            "    (= (ref path-gd 'applied-bound-positions) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result attaches bounded path-local context after plain no-op match reads");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result attaches bounded path-local context after plain no-op match reads (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result attaches bounded path-local context after plain no-op match reads");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result-sel-shapes-query (deduce 'open 'memory)) "
            "  (define [relation db-why-result-sel-shapes-query] edge-sel-shapes-query (src) (dst)) "
            "  (define [relation db-why-result-sel-shapes-query materialized] reach-sel-shapes-query (src) (dst)) "
            "  (deduce/rule! db-why-result-sel-shapes-query '(reach-sel-shapes-query ?x ?y) '(edge-sel-shapes-query ?x ?y)) "
            "  (deduce/rule! db-why-result-sel-shapes-query '(reach-sel-shapes-query ?x ?z) '(edge-sel-shapes-query ?x ?y) '(reach-sel-shapes-query ?y ?z)) "
            "  (deduce/fact! edge-sel-shapes-query 9 10) "
            "  (deduce/refresh! reach-sel-shapes-query) "
            "  (deduce/query reach-sel-shapes-query (lambda (row) (= (ref row 'src) 9)) 1) "
            "  (define why-query (deduce/why-result reach-sel-shapes-query 9 10)) "
            "  (define query-path-gd (ref (car (ref why-query 'paths)) 'goal-directed-read-context)) "
            "  (define db-why-result-sel-shapes-match (deduce 'open 'memory)) "
            "  (define [relation db-why-result-sel-shapes-match] edge-sel-shapes-match (src) (dst)) "
            "  (define [relation db-why-result-sel-shapes-match materialized] reach-sel-shapes-match (src) (dst)) "
            "  (deduce/rule! db-why-result-sel-shapes-match '(reach-sel-shapes-match ?x ?y) '(edge-sel-shapes-match ?x ?y)) "
            "  (deduce/rule! db-why-result-sel-shapes-match '(reach-sel-shapes-match ?x ?z) '(edge-sel-shapes-match ?x ?y) '(reach-sel-shapes-match ?y ?z)) "
            "  (deduce/fact! edge-sel-shapes-match 9 10) "
            "  (deduce/refresh! reach-sel-shapes-match) "
            "  (deduce/match reach-sel-shapes-match '(reach-sel-shapes-match 9 10) 1) "
            "  (define why-match (deduce/why-result reach-sel-shapes-match 9 10)) "
            "  (define match-path-gd (ref (car (ref why-match 'paths)) 'goal-directed-read-context)) "
            "  (define db-why-result-sel-shapes-scan (deduce 'open 'memory)) "
            "  (define [relation db-why-result-sel-shapes-scan] edge-sel-shapes-scan (src) (dst)) "
            "  (define [relation db-why-result-sel-shapes-scan materialized] reach-sel-shapes-scan (src) (dst)) "
            "  (deduce/rule! db-why-result-sel-shapes-scan '(reach-sel-shapes-scan ?x ?y) '(edge-sel-shapes-scan ?x ?y)) "
            "  (deduce/rule! db-why-result-sel-shapes-scan '(reach-sel-shapes-scan ?x ?z) '(edge-sel-shapes-scan ?x ?y) '(reach-sel-shapes-scan ?y ?z)) "
            "  (deduce/fact! edge-sel-shapes-scan 9 10) "
            "  (deduce/refresh! reach-sel-shapes-scan) "
            "  (deduce/scan reach-sel-shapes-scan 1) "
            "  (define why-scan (deduce/why-result reach-sel-shapes-scan 9 10)) "
            "  (define scan-path-gd (ref (car (ref why-scan 'paths)) 'goal-directed-read-context)) "
            "  (define db-why-result-sel-shapes-range (deduce 'open 'memory)) "
            "  (define [relation db-why-result-sel-shapes-range] edge-sel-shapes-range (src) (dst)) "
            "  (define [relation db-why-result-sel-shapes-range materialized] reach-sel-shapes-range (src) (dst)) "
            "  (deduce/rule! db-why-result-sel-shapes-range '(reach-sel-shapes-range ?x ?y) '(edge-sel-shapes-range ?x ?y)) "
            "  (deduce/rule! db-why-result-sel-shapes-range '(reach-sel-shapes-range ?x ?z) '(edge-sel-shapes-range ?x ?y) '(reach-sel-shapes-range ?y ?z)) "
            "  (deduce/fact! edge-sel-shapes-range 9 10) "
            "  (deduce/refresh! reach-sel-shapes-range) "
            "  (deduce/scan-range reach-sel-shapes-range '(9 10) '(9 10) 1) "
            "  (define why-range (deduce/why-result reach-sel-shapes-range 9 10)) "
            "  (define range-path-gd (ref (car (ref why-range 'paths)) 'goal-directed-read-context)) "
            "  (and "
            "    (= (dict? query-path-gd) true) "
            "    (= (ref query-path-gd 'surface) 'query) "
            "    (= (ref query-path-gd 'execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref query-path-gd 'selector-rule-index) 1) "
            "    (= (dict? match-path-gd) true) "
            "    (= (ref match-path-gd 'surface) 'match) "
            "    (= (ref match-path-gd 'execution-path) 'no-op) "
            "    (= (ref match-path-gd 'selector-rule-index) 1) "
            "    (= (dict? scan-path-gd) true) "
            "    (= (ref scan-path-gd 'surface) 'scan) "
            "    (= (ref scan-path-gd 'execution-path) 'no-op) "
            "    (= (ref scan-path-gd 'selector-rule-index) 1) "
            "    (= (dict? range-path-gd) true) "
            "    (= (ref range-path-gd 'surface) 'scan-range) "
            "    (= (ref range-path-gd 'execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref range-path-gd 'selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result keeps truthful selector-scoped path-local context across no-op and ephemeral row-read shapes");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result keeps truthful selector-scoped path-local context across no-op and ephemeral row-read shapes (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result keeps truthful selector-scoped path-local context across no-op and ephemeral row-read shapes");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-why-result (deduce 'open 'memory)) "
            "  (define [relation db-why-result] parent-why (src) (dst)) "
            "  (define [relation db-why-result materialized] ancestor-why (src) (dst)) "
            "  (define [relation db-why-result materialized] reverse-parent-why (dst) (src)) "
            "  (define [relation db-why-result materialized] chain3-why (src) (mid) (dst)) "
            "  (define [relation db-why-result materialized] reach-ext-why (src) (dst)) "
            "  (define [relation db-why-result materialized] reach-mixed-one-why (src) (dst)) "
            "  (define [relation db-why-result materialized] reach-why (src) (dst)) "
            "  (deduce/rule! db-why-result '(ancestor-why ?x ?y) '(parent-why ?x ?y)) "
            "  (deduce/rule! db-why-result '(reverse-parent-why ?y ?x) '(parent-why ?x ?y)) "
            "  (deduce/rule! db-why-result '(chain3-why ?x ?y ?z) '(parent-why ?x ?y) '(parent-why ?y ?z)) "
            "  (deduce/rule! db-why-result '(reach-ext-why ?x ?z) '(parent-why ?x ?y) '(parent-why ?y ?z)) "
            "  (deduce/rule! db-why-result '(reach-mixed-one-why ?x ?z) '(parent-why ?x ?y) '(ancestor-why ?y ?z)) "
            "  (deduce/rule! db-why-result '(reach-why ?x ?y) '(parent-why ?x ?y)) "
            "  (deduce/rule! db-why-result '(reach-why ?x ?z) '(parent-why ?x ?y) '(ancestor-why ?y ?z)) "
            "  (deduce/fact! parent-why 1 2) "
            "  (deduce/fact! parent-why 2 3) "
            "  (deduce/fact! parent-why 1 4) "
            "  (deduce/fact! parent-why 4 3) "
            "  (deduce/fact! parent-why 8 9) "
            "  (deduce/fact! parent-why 9 10) "
            "  (deduce/refresh! ancestor-why) "
            "  (deduce/refresh! reverse-parent-why) "
            "  (deduce/refresh! chain3-why) "
            "  (deduce/refresh! reach-ext-why) "
            "  (deduce/refresh! reach-mixed-one-why) "
            "  (deduce/refresh! reach-why) "
            "  (define seed (deduce/why-result parent-why 1 2)) "
            "  (define missing (deduce/why-result parent-why 9 10)) "
            "  (define derived (deduce/why-result ancestor-why 1 2)) "
            "  (define projected-derived (deduce/why-result reverse-parent-why 2 1)) "
            "  (define multi-derived (deduce/why-result chain3-why 1 2 3)) "
            "  (define search-derived (deduce/why-result reach-ext-why 8 10)) "
            "  (define search-derived-partial (deduce/why-result reach-ext-why 1 3)) "
            "  (define mixed-derived (deduce/why-result reach-mixed-one-why 8 10)) "
            "  (define multi-rule-direct (deduce/why-result reach-why 1 2)) "
            "  (define multi-rule-derived (deduce/why-result reach-why 1 3)) "
            "  (define seed-paths (ref seed 'paths)) "
            "  (define seed-path (car seed-paths)) "
            "  (define seed-support (ref seed-path 'support)) "
            "  (define seed-frame (car seed-support)) "
            "  (define derived-paths (ref derived 'paths)) "
            "  (define derived-path (car derived-paths)) "
            "  (define derived-support (ref derived-path 'support)) "
            "  (define derived-frame (car derived-support)) "
            "  (define projected-derived-path (car (ref projected-derived 'paths))) "
            "  (define projected-derived-frame (car (ref projected-derived-path 'support))) "
            "  (define multi-derived-path (car (ref multi-derived 'paths))) "
            "  (define multi-derived-support (ref multi-derived-path 'support)) "
            "  (define multi-derived-frame0 (car multi-derived-support)) "
            "  (define multi-derived-frame1 (car (cdr multi-derived-support))) "
            "  (define search-derived-path (car (ref search-derived 'paths))) "
            "  (define search-derived-partial-path (car (ref search-derived-partial 'paths))) "
            "  (define mixed-derived-path (car (ref mixed-derived 'paths))) "
            "  (and "
            "    (= (ref seed 'kind) 'why-result) "
            "    (= (ref seed 'status) 'ok) "
            "    (= (ref seed 'surface-kind) 'provenance-snapshot) "
            "    (= (ref seed 'subject-kind) 'row) "
            "    (= (ref (ref seed 'subject) 'relation) 'parent-why) "
            "    (= (length (ref (ref seed 'subject) 'tuple)) 2) "
            "    (= (ref seed 'path-count) 1) "
            "    (= (ref seed 'max-depth) 1) "
            "    (= (ref seed 'truncated) false) "
            "    (= (length seed-paths) 1) "
            "    (= (ref seed-path 'path-kind) 'seed) "
            "    (= (ref seed-path 'head-predicate) 'parent-why) "
            "    (= (ref seed-path 'rule-index) nil) "
            "    (= (ref seed-path 'truncated) false) "
            "    (= (length seed-support) 1) "
            "    (= (ref seed-frame 'kind) 'fact) "
            "    (= (ref seed-frame 'predicate) 'parent-why) "
            "    (= (ref missing 'status) 'missing) "
            "    (= (ref missing 'path-count) 0) "
            "    (= (ref missing 'max-depth) 0) "
            "    (= (ref missing 'truncated) false) "
            "    (= (ref missing 'paths) nil) "
            "    (= (ref derived 'status) 'ok) "
            "    (= (ref derived 'path-count) 1) "
            "    (= (ref derived 'max-depth) 2) "
            "    (= (ref derived 'truncated) false) "
            "    (= (length derived-paths) 1) "
            "    (= (ref derived-path 'path-kind) 'derived) "
            "    (= (ref derived-path 'head-predicate) 'ancestor-why) "
            "    (= (ref derived-path 'rule-index) 0) "
            "    (= (length derived-support) 1) "
            "    (= (ref derived-frame 'kind) 'fact) "
            "    (= (ref derived-frame 'predicate) 'parent-why) "
            "    (= (ref projected-derived 'status) 'ok) "
            "    (= (ref projected-derived 'path-count) 1) "
            "    (= (ref projected-derived-path 'path-kind) 'derived) "
            "    (= (ref projected-derived-path 'head-predicate) 'reverse-parent-why) "
            "    (= (ref projected-derived-frame 'predicate) 'parent-why) "
            "    (= (ref multi-derived 'status) 'ok) "
            "    (= (ref multi-derived 'path-count) 1) "
            "    (= (ref multi-derived-path 'path-kind) 'derived) "
            "    (= (ref multi-derived-path 'head-predicate) 'chain3-why) "
            "    (= (length multi-derived-support) 2) "
            "    (= (ref multi-derived-frame0 'predicate) 'parent-why) "
            "    (= (ref multi-derived-frame1 'predicate) 'parent-why) "
            "    (= (ref search-derived 'status) 'ok) "
            "    (= (ref search-derived 'path-count) 1) "
            "    (= (ref search-derived 'truncated) false) "
            "    (= (ref search-derived-path 'path-kind) 'derived) "
            "    (= (length (ref search-derived-path 'support)) 2) "
            "    (= (ref search-derived-partial 'status) 'partial) "
            "    (= (ref search-derived-partial 'path-count) 1) "
            "    (= (ref search-derived-partial 'truncated) true) "
            "    (= (ref search-derived-partial-path 'path-kind) 'derived) "
            "    (= (length (ref search-derived-partial-path 'support)) 2) "
            "    (= (ref mixed-derived 'status) 'ok) "
            "    (= (ref mixed-derived 'path-count) 1) "
            "    (= (ref mixed-derived 'truncated) false) "
            "    (= (ref mixed-derived-path 'path-kind) 'derived) "
            "    (= (length (ref mixed-derived-path 'support)) 2) "
            "    (= (ref multi-rule-direct 'status) 'ok) "
            "    (= (ref multi-rule-direct 'path-count) 1) "
            "    (= (ref multi-rule-direct 'max-depth) 2) "
            "    (= (ref multi-rule-direct 'truncated) false) "
            "    (= (length (ref multi-rule-direct 'paths)) 1) "
            "    (= (ref (car (ref multi-rule-direct 'paths)) 'rule-index) 5) "
            "    (= (length (ref (car (ref multi-rule-direct 'paths)) 'support)) 1) "
            "    (= (ref multi-rule-derived 'status) 'partial) "
            "    (= (ref multi-rule-derived 'path-count) 1) "
            "    (= (ref multi-rule-derived 'max-depth) 2) "
            "    (= (ref multi-rule-derived 'truncated) true) "
            "    (= (length (ref multi-rule-derived 'paths)) 1) "
            "    (= (ref (car (ref multi-rule-derived 'paths)) 'path-kind) 'derived) "
            "    (= (ref (car (ref multi-rule-derived 'paths)) 'head-predicate) 'reach-why) "
            "    (= (ref (car (ref multi-rule-derived 'paths)) 'rule-index) 6) "
            "    (= (length (ref (car (ref multi-rule-derived 'paths)) 'support)) 2)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce why-result exposes seed plus non-recursive extensional, mixed-body, and multi-rule derived ok/partial payloads");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce why-result exposes seed plus non-recursive extensional, mixed-body, and multi-rule derived ok/partial payloads (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce why-result exposes seed plus non-recursive extensional, mixed-body, and multi-rule derived ok/partial payloads");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-not-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-selector] gd-edge-qdns (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-selector] gd-reach-qdns (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-selector] gd-other-src-qdns (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-selector] gd-other-out-qdns (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-not-selector '(gd-reach-qdns ?x ?y) '(gd-edge-qdns ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-not-selector '(gd-reach-qdns ?x ?z) '(gd-edge-qdns ?x ?y) '(gd-reach-qdns ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-not-selector '(gd-other-out-qdns ?x ?y) '(gd-other-src-qdns ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-not-selector) "
            "  (deduce/fact! gd-edge-qdns 1 2) "
            "  (deduce/fact! gd-edge-qdns 2 3) "
            "  (deduce/fact! gd-edge-qdns 10 11) "
            "  (deduce/fact! gd-other-src-qdns 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdns "
            "        (lambda (row) "
            "          (if (not (> target 9)) "
            "            (= (ref row 'dst) target) "
            "            false)) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdns)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-not-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdns) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use closed not guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use closed not guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use closed not guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-not-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-plain] gd-edge-qdnp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-plain] gd-reach-qdnp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-plain] gd-other-src-qdnp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-not-plain] gd-other-out-qdnp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-not-plain '(gd-reach-qdnp ?x ?y) '(gd-edge-qdnp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-not-plain '(gd-reach-qdnp ?x ?z) '(gd-edge-qdnp ?x ?y) '(gd-reach-qdnp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-not-plain '(gd-other-out-qdnp ?x ?y) '(gd-other-src-qdnp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-not-plain) "
            "  (deduce/fact! gd-edge-qdnp 1 2) "
            "  (deduce/fact! gd-edge-qdnp 2 3) "
            "  (deduce/fact! gd-edge-qdnp 10 11) "
            "  (deduce/fact! gd-other-src-qdnp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdnp "
            "        (lambda (row) "
            "          (if (not (> target 9)) "
            "            (= (ref row 'dst) target) "
            "            false))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdnp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-not-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdnp)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use closed not guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use closed not guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use closed not guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-and-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-selector] gd-edge-qdias (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-selector] gd-reach-qdias (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-selector] gd-other-src-qdias (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-selector] gd-other-out-qdias (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-and-selector '(gd-reach-qdias ?x ?y) '(gd-edge-qdias ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-and-selector '(gd-reach-qdias ?x ?z) '(gd-edge-qdias ?x ?y) '(gd-reach-qdias ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-and-selector '(gd-other-out-qdias ?x ?y) '(gd-other-src-qdias ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-and-selector) "
            "  (deduce/fact! gd-edge-qdias 1 2) "
            "  (deduce/fact! gd-edge-qdias 2 3) "
            "  (deduce/fact! gd-edge-qdias 10 11) "
            "  (deduce/fact! gd-other-src-qdias 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdias "
            "        (lambda (row) "
            "          (if (and true (> target 0)) "
            "            (= (ref row 'dst) target) "
            "            false)) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdias)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-and-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdias) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use closed and guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use closed and guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use closed and guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-and-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-plain] gd-edge-qdiap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-plain] gd-reach-qdiap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-plain] gd-other-src-qdiap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-and-plain] gd-other-out-qdiap (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-and-plain '(gd-reach-qdiap ?x ?y) '(gd-edge-qdiap ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-and-plain '(gd-reach-qdiap ?x ?z) '(gd-edge-qdiap ?x ?y) '(gd-reach-qdiap ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-and-plain '(gd-other-out-qdiap ?x ?y) '(gd-other-src-qdiap ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-and-plain) "
            "  (deduce/fact! gd-edge-qdiap 1 2) "
            "  (deduce/fact! gd-edge-qdiap 2 3) "
            "  (deduce/fact! gd-edge-qdiap 10 11) "
            "  (deduce/fact! gd-other-src-qdiap 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdiap "
            "        (lambda (row) "
            "          (if (and true (> target 0)) "
            "            (= (ref row 'dst) target) "
            "            false))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdiap)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-and-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdiap)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use closed and guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use closed and guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use closed and guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-numeric (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-numeric] gd-edge-qdn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-numeric] gd-reach-qdn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-numeric] gd-other-src-qdn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-numeric] gd-other-out-qdn (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-numeric '(gd-reach-qdn ?x ?y) '(gd-edge-qdn ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-numeric '(gd-reach-qdn ?x ?z) '(gd-edge-qdn ?x ?y) '(gd-reach-qdn ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-numeric '(gd-other-out-qdn ?x ?y) '(gd-other-src-qdn ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-numeric) "
            "  (deduce/fact! gd-edge-qdn 1 2) "
            "  (deduce/fact! gd-edge-qdn 2 3) "
            "  (deduce/fact! gd-edge-qdn 10 11) "
            "  (deduce/fact! gd-other-src-qdn 9 10) "
            "  (define selected "
            "    (let (target -3) "
            "      (deduce/query gd-reach-qdn "
            "        (lambda (row) "
            "          (= (ref row 'dst) (abs target))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdn)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-numeric)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdn) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query can use closed numeric wrappers in row-independent filter demand");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query can use closed numeric wrappers in row-independent filter demand (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query can use closed numeric wrappers in row-independent filter demand");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-pow (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-pow] gd-edge-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pow] gd-reach-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pow] gd-other-src-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pow] gd-other-out-qdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pow '(gd-reach-qdp ?x ?y) '(gd-edge-qdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pow '(gd-reach-qdp ?x ?z) '(gd-edge-qdp ?x ?y) '(gd-reach-qdp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pow '(gd-other-out-qdp ?x ?y) '(gd-other-src-qdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-pow) "
            "  (deduce/fact! gd-edge-qdp 1 2) "
            "  (deduce/fact! gd-edge-qdp 2 3) "
            "  (deduce/fact! gd-edge-qdp 3 4) "
            "  (deduce/fact! gd-other-src-qdp 9 10) "
            "  (define selected "
            "    (let (base 2 exponent 2) "
            "      (deduce/query gd-reach-qdp "
            "        (lambda (row) "
            "          (= (ref row 'dst) (pow base exponent))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-pow)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 6) "
            "    (= (ref (car selected) 'dst) 4) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query can use closed binary numeric wrappers in row-independent filter demand");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query can use closed binary numeric wrappers in row-independent filter demand (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query can use closed binary numeric wrappers in row-independent filter demand");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-atan2 (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-atan2] gd-edge-qdat (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-atan2] gd-reach-qdat (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-atan2] gd-other-src-qdat (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-atan2] gd-other-out-qdat (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-atan2 '(gd-reach-qdat ?x ?y) '(gd-edge-qdat ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-atan2 '(gd-reach-qdat ?x ?z) '(gd-edge-qdat ?x ?y) '(gd-reach-qdat ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-atan2 '(gd-other-out-qdat ?x ?y) '(gd-other-src-qdat ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-atan2) "
            "  (deduce/fact! gd-edge-qdat 1 2) "
            "  (deduce/fact! gd-edge-qdat 2 3) "
            "  (deduce/fact! gd-edge-qdat 3 0) "
            "  (deduce/fact! gd-other-src-qdat 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdat "
            "      (lambda (row) "
            "        (= (ref row 'dst) (atan2 0 1))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qdat)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-atan2)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 6) "
            "    (= (ref (car selected) 'dst) 0) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdat) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query can use closed atan2 wrappers in row-independent filter demand");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query can use closed atan2 wrappers in row-independent filter demand (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query can use closed atan2 wrappers in row-independent filter demand");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-compare-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-selector] gd-edge-qdics (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-selector] gd-reach-qdics (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-selector] gd-other-src-qdics (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-selector] gd-other-out-qdics (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-selector '(gd-reach-qdics ?x ?y) '(gd-edge-qdics ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-selector '(gd-reach-qdics ?x ?z) '(gd-edge-qdics ?x ?y) '(gd-reach-qdics ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-selector '(gd-other-out-qdics ?x ?y) '(gd-other-src-qdics ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-compare-selector) "
            "  (deduce/fact! gd-edge-qdics 1 2) "
            "  (deduce/fact! gd-edge-qdics 2 3) "
            "  (deduce/fact! gd-edge-qdics 10 11) "
            "  (deduce/fact! gd-other-src-qdics 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdics "
            "        (lambda (row) "
            "          (if (> target 0) "
            "            (= (ref row 'dst) target) "
            "            false)) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdics)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-compare-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdics) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use closed comparison guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use closed comparison guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use closed comparison guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-compare-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-plain] gd-edge-qdicp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-plain] gd-reach-qdicp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-plain] gd-other-src-qdicp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-compare-plain] gd-other-out-qdicp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-plain '(gd-reach-qdicp ?x ?y) '(gd-edge-qdicp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-plain '(gd-reach-qdicp ?x ?z) '(gd-edge-qdicp ?x ?y) '(gd-reach-qdicp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-compare-plain '(gd-other-out-qdicp ?x ?y) '(gd-other-src-qdicp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-compare-plain) "
            "  (deduce/fact! gd-edge-qdicp 1 2) "
            "  (deduce/fact! gd-edge-qdicp 2 3) "
            "  (deduce/fact! gd-edge-qdicp 10 11) "
            "  (deduce/fact! gd-other-src-qdicp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdicp "
            "        (lambda (row) "
            "          (if (> target 0) "
            "            (= (ref row 'dst) target) "
            "            false))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdicp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-compare-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdicp)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use closed comparison guards in row-independent if wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use closed comparison guards in row-independent if wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use closed comparison guards in row-independent if wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-or-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-selector] gd-edge-qdos (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-selector] gd-reach-qdos (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-selector] gd-other-src-qdos (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-selector] gd-other-out-qdos (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-selector '(gd-reach-qdos ?x ?y) '(gd-edge-qdos ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-selector '(gd-reach-qdos ?x ?z) '(gd-edge-qdos ?x ?y) '(gd-reach-qdos ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-selector '(gd-other-out-qdos ?x ?y) '(gd-other-src-qdos ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-or-selector) "
            "  (deduce/fact! gd-edge-qdos 1 2) "
            "  (deduce/fact! gd-edge-qdos 2 3) "
            "  (deduce/fact! gd-edge-qdos 10 11) "
            "  (deduce/fact! gd-other-src-qdos 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdos "
            "        (lambda (row) "
            "          (or false (= (ref row 'dst) target))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdos)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-or-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdos) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use row-independent false or wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use row-independent false or wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use row-independent false or wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-or-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-plain] gd-edge-qdop (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-plain] gd-reach-qdop (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-plain] gd-other-src-qdop (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-or-plain] gd-other-out-qdop (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-plain '(gd-reach-qdop ?x ?y) '(gd-edge-qdop ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-plain '(gd-reach-qdop ?x ?z) '(gd-edge-qdop ?x ?y) '(gd-reach-qdop ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-or-plain '(gd-other-out-qdop ?x ?y) '(gd-other-src-qdop ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-or-plain) "
            "  (deduce/fact! gd-edge-qdop 1 2) "
            "  (deduce/fact! gd-edge-qdop 2 3) "
            "  (deduce/fact! gd-edge-qdop 10 11) "
            "  (deduce/fact! gd-other-src-qdop 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdop "
            "        (lambda (row) "
            "          (or false (= (ref row 'dst) target)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdop)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-or-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdop)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use row-independent false or wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use row-independent false or wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use row-independent false or wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-let-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-selector] gd-edge-qdls (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-selector] gd-reach-qdls (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-selector] gd-other-src-qdls (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-selector] gd-other-out-qdls (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-selector '(gd-reach-qdls ?x ?y) '(gd-edge-qdls ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-selector '(gd-reach-qdls ?x ?z) '(gd-edge-qdls ?x ?y) '(gd-reach-qdls ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-selector '(gd-other-out-qdls ?x ?y) '(gd-other-src-qdls ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-let-selector) "
            "  (deduce/fact! gd-edge-qdls 1 2) "
            "  (deduce/fact! gd-edge-qdls 2 3) "
            "  (deduce/fact! gd-edge-qdls 10 11) "
            "  (deduce/fact! gd-other-src-qdls 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdls "
            "        (lambda (row) (let (k (+ target 0)) (= (ref row 'dst) k))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdls)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-let-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdls) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use let-bound row-independent equalities in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use let-bound row-independent equalities in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use let-bound row-independent equalities in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-let-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-plain] gd-edge-qdlp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-plain] gd-reach-qdlp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-plain] gd-other-src-qdlp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-let-plain] gd-other-out-qdlp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-plain '(gd-reach-qdlp ?x ?y) '(gd-edge-qdlp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-plain '(gd-reach-qdlp ?x ?z) '(gd-edge-qdlp ?x ?y) '(gd-reach-qdlp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-let-plain '(gd-other-out-qdlp ?x ?y) '(gd-other-src-qdlp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-let-plain) "
            "  (deduce/fact! gd-edge-qdlp 1 2) "
            "  (deduce/fact! gd-edge-qdlp 2 3) "
            "  (deduce/fact! gd-edge-qdlp 10 11) "
            "  (deduce/fact! gd-other-src-qdlp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdlp "
            "        (lambda (row) (let (k (+ target 0)) (= (ref row 'dst) k)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdlp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-let-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdlp)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use let-bound row-independent equalities in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use let-bound row-independent equalities in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use let-bound row-independent equalities in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-swapped] gd-edge-pms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-swapped] gd-sym-pms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-swapped] gd-other-src-pms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-swapped] gd-other-out-pms (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-swapped '(gd-sym-pms ?x ?y) '(gd-edge-pms ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-swapped '(gd-sym-pms ?x ?y) '(gd-sym-pms ?y ?x)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-swapped '(gd-other-out-pms ?x ?y) '(gd-other-src-pms ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-swapped) "
            "  (deduce/fact! gd-edge-pms 1 2) "
            "  (deduce/fact! gd-edge-pms 10 11) "
            "  (deduce/fact! gd-other-src-pms 20 21) "
            "  (define selected (deduce/match gd-sym-pms '(gd-sym-pms 1 ?y))) "
            "  (define sym-stats (deduce/stats gd-sym-pms)) "
            "  (define other-stats (deduce/stats gd-other-out-pms)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'y) 2) "
            "    (= (ref sym-stats 'dirty-self) true) "
            "    (= (ref sym-stats 'cardinality-estimate) 0) "
            "    (= (ref sym-stats 'distinct-estimate) 0) "
            "    (= (ref sym-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref sym-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref sym-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match demand execution supports variable-preserving recursive body reordering");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match demand execution supports variable-preserving recursive body reordering (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match demand execution supports variable-preserving recursive body reordering");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-plain] gd-edge-srdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-plain] gd-reach-srdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-plain] gd-other-src-srdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-plain] gd-other-out-srdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-plain '(gd-reach-srdp ?x ?y) '(gd-edge-srdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-plain '(gd-reach-srdp ?x ?z) '(gd-reach-srdp ?x ?y) '(gd-edge-srdp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-plain '(gd-other-out-srdp ?x ?y) '(gd-other-src-srdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand-plain) "
            "  (deduce/fact! gd-edge-srdp 1 2) "
            "  (deduce/fact! gd-edge-srdp 2 3) "
            "  (deduce/fact! gd-edge-srdp 10 11) "
            "  (deduce/fact! gd-edge-srdp 11 12) "
            "  (deduce/fact! gd-other-src-srdp 20 21) "
            "  (define selected (deduce/scan-range gd-reach-srdp '(1 0) '(1 999))) "
            "  (define reach-stats (deduce/stats gd-reach-srdp)) "
            "  (define other-stats (deduce/stats gd-other-out-srdp)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (every? (lambda (row) (= (ref row 'src) 1)) selected) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'cardinality-estimate) 0) "
            "    (= (ref reach-stats 'distinct-estimate) 0) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce scan-range with exact bound positions can use ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce scan-range with exact bound positions can use ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce scan-range with exact bound positions can use ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-full (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-full] gd-edge-qf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-full] gd-reach-qf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-full] gd-other-src-qf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-full] gd-other-out-qf (src) (dst)) "
            "  (deduce/fact! gd-edge-qf 1 2) "
            "  (deduce/fact! gd-edge-qf 2 3) "
            "  (deduce/fact! gd-other-src-qf 9 10) "
            "  (deduce/rule! db-admin-goal-directed-query-full '(gd-reach-qf ?x ?y) '(gd-edge-qf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-full '(gd-reach-qf ?x ?z) '(gd-edge-qf ?x ?y) '(gd-reach-qf ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-full '(gd-other-out-qf ?x ?y) '(gd-other-src-qf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-full '(gd-other-out-qf ?x ?y) '(gd-edge-qf ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-full) "
            "  (deduce/retract! gd-edge-qf 2 3) "
            "  (define stats0 (deduce/stats gd-other-out-qf)) "
            "  (define selected (deduce/query gd-reach-qf (lambda (row) true))) "
            "  (define stats1 (deduce/stats gd-other-out-qf)) "
            "  (define other-after (deduce/count gd-other-out-qf)) "
            "  (and "
            "    (= (ref stats0 'full-recompute-required) true) "
            "    (= (length selected) 1) "
            "    (= (ref stats1 'full-recompute-required) false) "
            "    (= other-after 2)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query falls back to full DB recompute after destructive changes");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query falls back to full DB recompute after destructive changes (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query falls back to full DB recompute after destructive changes");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-full (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-full] gd-edge-mf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-full] gd-reach-mf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-full] gd-other-src-mf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-full] gd-other-out-mf (src) (dst)) "
            "  (deduce/fact! gd-edge-mf 1 2) "
            "  (deduce/fact! gd-edge-mf 2 3) "
            "  (deduce/fact! gd-other-src-mf 9 10) "
            "  (deduce/rule! db-admin-goal-directed-match-full '(gd-reach-mf ?x ?y) '(gd-edge-mf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-full '(gd-reach-mf ?x ?z) '(gd-edge-mf ?x ?y) '(gd-reach-mf ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-full '(gd-other-out-mf ?x ?y) '(gd-other-src-mf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-full '(gd-other-out-mf ?x ?y) '(gd-edge-mf ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-full) "
            "  (deduce/retract! gd-edge-mf 2 3) "
            "  (define stats0 (deduce/stats gd-other-out-mf)) "
            "  (define selected (deduce/match gd-reach-mf '(gd-reach-mf ?x ?y))) "
            "  (define stats1 (deduce/stats gd-other-out-mf)) "
            "  (define other-after (deduce/count gd-other-out-mf)) "
            "  (and "
            "    (= (ref stats0 'full-recompute-required) true) "
            "    (= (length selected) 1) "
            "    (= (ref stats1 'full-recompute-required) false) "
            "    (= other-after 2)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match falls back to full DB recompute after destructive changes");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match falls back to full DB recompute after destructive changes (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match falls back to full DB recompute after destructive changes");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-count-full (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-count-full] gd-edge-cf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-full] gd-reach-cf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-full] gd-other-src-cf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-full] gd-other-out-cf (src) (dst)) "
            "  (deduce/fact! gd-edge-cf 1 2) "
            "  (deduce/fact! gd-edge-cf 2 3) "
            "  (deduce/fact! gd-other-src-cf 9 10) "
            "  (deduce/rule! db-admin-goal-directed-count-full '(gd-reach-cf ?x ?y) '(gd-edge-cf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-count-full '(gd-reach-cf ?x ?z) '(gd-edge-cf ?x ?y) '(gd-reach-cf ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-count-full '(gd-other-out-cf ?x ?y) '(gd-other-src-cf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-count-full '(gd-other-out-cf ?x ?y) '(gd-edge-cf ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-count-full) "
            "  (deduce/retract! gd-edge-cf 2 3) "
            "  (define count-selected (deduce/count gd-reach-cf)) "
            "  (define count-stats1 (deduce/stats gd-other-out-cf)) "
            "  (define count-other-after (deduce/count gd-other-out-cf)) "
            "  (define db-admin-goal-directed-scan-full (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-full] gd-edge-sf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-full] gd-reach-sf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-full] gd-other-src-sf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-full] gd-other-out-sf (src) (dst)) "
            "  (deduce/fact! gd-edge-sf 1 2) "
            "  (deduce/fact! gd-edge-sf 2 3) "
            "  (deduce/fact! gd-other-src-sf 9 10) "
            "  (deduce/rule! db-admin-goal-directed-scan-full '(gd-reach-sf ?x ?y) '(gd-edge-sf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-full '(gd-reach-sf ?x ?z) '(gd-edge-sf ?x ?y) '(gd-reach-sf ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-full '(gd-other-out-sf ?x ?y) '(gd-other-src-sf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-full '(gd-other-out-sf ?x ?y) '(gd-edge-sf ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-full) "
            "  (deduce/retract! gd-edge-sf 2 3) "
            "  (define scan-selected (deduce/scan gd-reach-sf)) "
            "  (define scan-stats1 (deduce/stats gd-other-out-sf)) "
            "  (define scan-other-after (deduce/count gd-other-out-sf)) "
            "  (define db-admin-goal-directed-scan-range-full (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-full] gd-edge-srf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-full] gd-reach-srf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-full] gd-other-src-srf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-full] gd-other-out-srf (src) (dst)) "
            "  (deduce/fact! gd-edge-srf 1 2) "
            "  (deduce/fact! gd-edge-srf 2 3) "
            "  (deduce/fact! gd-other-src-srf 9 10) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-full '(gd-reach-srf ?x ?y) '(gd-edge-srf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-full '(gd-reach-srf ?x ?z) '(gd-edge-srf ?x ?y) '(gd-reach-srf ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-full '(gd-other-out-srf ?x ?y) '(gd-other-src-srf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-full '(gd-other-out-srf ?x ?y) '(gd-edge-srf ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-full) "
            "  (deduce/retract! gd-edge-srf 2 3) "
            "  (define range-selected (deduce/scan-range gd-reach-srf '(1 2) '(1 2))) "
            "  (define range-stats1 (deduce/stats gd-other-out-srf)) "
            "  (define range-other-after (deduce/count gd-other-out-srf)) "
            "  (and "
            "    (= count-selected 1) "
            "    (= (ref count-stats1 'full-recompute-required) false) "
            "    (= count-other-after 2) "
            "    (= (length scan-selected) 1) "
            "    (= (ref scan-stats1 'full-recompute-required) false) "
            "    (= scan-other-after 2) "
            "    (= (length range-selected) 1) "
            "    (= (ref range-stats1 'full-recompute-required) false) "
            "    (= range-other-after 2)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce count, scan, and scan-range fall back to full DB recompute after destructive changes");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce count, scan, and scan-range fall back to full DB recompute after destructive changes (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce count, scan, and scan-range fall back to full DB recompute after destructive changes");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-selector-full (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-selector-full] gd-edge-sel-full (src) (dst)) "
            "  (define [relation db-admin-goal-directed-selector-full] gd-reach-sel-full (src) (dst)) "
            "  (deduce/fact! gd-edge-sel-full 1 2) "
            "  (deduce/fact! gd-edge-sel-full 2 3) "
            "  (deduce/rule! db-admin-goal-directed-selector-full '(gd-reach-sel-full ?x ?y) '(gd-edge-sel-full ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-selector-full '(gd-reach-sel-full ?x ?z) '(gd-edge-sel-full ?x ?y) '(gd-reach-sel-full ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-selector-full) "
            "  (deduce/retract! gd-edge-sel-full 2 3) "
            "  (define query-payload "
            "    (handle "
            "      (deduce/query gd-reach-sel-full (lambda (row) true) 1) "
            "      (raise msg msg))) "
            "  (define match-payload "
            "    (handle "
            "      (deduce/match gd-reach-sel-full '(gd-reach-sel-full ?x ?y) 1) "
            "      (raise msg msg))) "
            "  (define count-payload "
            "    (handle "
            "      (deduce/count gd-reach-sel-full 1) "
            "      (raise msg msg))) "
            "  (define scan-payload "
            "    (handle "
            "      (deduce/scan gd-reach-sel-full 1) "
            "      (raise msg msg))) "
            "  (define range-payload "
            "    (handle "
            "      (deduce/scan-range gd-reach-sel-full '(1 2) '(9 9) 1) "
            "      (raise msg msg))) "
            "  (and "
            "    (= (ref query-payload 'code) 'deduce/query-goal-directed-selector-full-recompute-required) "
            "    (= (ref match-payload 'code) 'deduce/match-goal-directed-selector-full-recompute-required) "
            "    (= (ref count-payload 'code) 'deduce/count-goal-directed-selector-full-recompute-required) "
            "    (= (ref scan-payload 'code) 'deduce/scan-goal-directed-selector-full-recompute-required) "
            "    (= (ref range-payload 'code) 'deduce/scan-range-goal-directed-selector-full-recompute-required)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped goal-directed reads reject once the DB escalates to full-recompute-required");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped goal-directed reads reject once the DB escalates to full-recompute-required (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped goal-directed reads reject once the DB escalates to full-recompute-required");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-edge-md0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-reach-md0 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand '(gd-reach-md0 ?x ?y) '(gd-edge-md0 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand '(gd-reach-md0 ?x ?z) '(gd-reach-md0 ?x ?y) '(gd-edge-md0 ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand) "
            "  (deduce/fact! gd-edge-md0 1 2) "
            "  (deduce/fact! gd-edge-md0 2 3) "
            "  (deduce/fact! gd-edge-md0 10 11) "
            "  (deduce/fact! gd-edge-md0 11 12) "
            "  (define selected (deduce/match gd-reach-md0 '(gd-reach-md0 1 ?to) 1 'semi-naive)) "
            "  (define stats-after (deduce/stats gd-reach-md0)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-match-demand)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'to)) selected)) 5) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref stats-after 'last-goal-directed-read-mutation-epoch) (ref stats-after 'mutation-epoch)) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-md0) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce match with bound literals uses ephemeral selected-closure execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce match with bound literals uses ephemeral selected-closure execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce match with bound literals uses ephemeral selected-closure execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-plain] gd-edge-pmd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-plain] gd-reach-pmd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-plain] gd-other-src-pmd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-plain] gd-other-out-pmd (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-plain '(gd-reach-pmd ?x ?y) '(gd-edge-pmd ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-plain '(gd-reach-pmd ?x ?z) '(gd-reach-pmd ?x ?y) '(gd-edge-pmd ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-plain '(gd-other-out-pmd ?x ?y) '(gd-other-src-pmd ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-plain) "
            "  (deduce/fact! gd-edge-pmd 1 2) "
            "  (deduce/fact! gd-edge-pmd 2 3) "
            "  (deduce/fact! gd-edge-pmd 10 11) "
            "  (deduce/fact! gd-edge-pmd 11 12) "
            "  (deduce/fact! gd-other-src-pmd 20 21) "
            "  (define selected (deduce/match gd-reach-pmd '(gd-reach-pmd 1 ?y))) "
            "  (define reach-stats (deduce/stats gd-reach-pmd)) "
            "  (define other-stats (deduce/stats gd-other-out-pmd)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (every? (lambda (row) (or (= (ref row 'y) 2) (= (ref row 'y) 3))) selected) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'cardinality-estimate) 0) "
            "    (= (ref reach-stats 'distinct-estimate) 0) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match with bound literals can use ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match with bound literals can use ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match with bound literals can use ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-projected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-projected] gd-edge-pmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-projected] gd-reach-pmp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-projected '(gd-reach-pmp ?x ?y) '(gd-edge-pmp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-projected '(gd-reach-pmp ?x ?z) '(gd-reach-pmp ?x ?y) '(gd-edge-pmp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-projected) "
            "  (deduce/fact! gd-edge-pmp 1 2) "
            "  (deduce/fact! gd-edge-pmp 2 3) "
            "  (deduce/fact! gd-edge-pmp 10 11) "
            "  (define selected (deduce/match gd-reach-pmp '(gd-reach-pmp 1 3))) "
            "  (define stats-after (deduce/stats gd-reach-pmp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-match-demand-projected)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref stats-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (ref stats-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-pmp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match can project unsupported bound positions out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match can project unsupported bound positions out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match can project unsupported bound positions out of the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-selector-projected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-selector-projected] gd-edge-smp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-selector-projected] gd-reach-smp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-selector-projected '(gd-reach-smp ?x ?y) '(gd-edge-smp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-selector-projected '(gd-reach-smp ?x ?z) '(gd-reach-smp ?x ?y) '(gd-edge-smp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-selector-projected) "
            "  (deduce/fact! gd-edge-smp 1 2) "
            "  (deduce/fact! gd-edge-smp 2 3) "
            "  (deduce/fact! gd-edge-smp 10 11) "
            "  (define selected (deduce/match gd-reach-smp '(gd-reach-smp 1 3) 1 'semi-naive)) "
            "  (define stats-after (deduce/stats gd-reach-smp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-match-demand-selector-projected)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref stats-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref stats-after 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref stats-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref stats-after 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref stats-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref stats-after 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref stats-after 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-smp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref analyze-after 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce match can project unsupported bound positions out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce match can project unsupported bound positions out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce match can project unsupported bound positions out of the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-prefix (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-prefix] gd-edge-pfx (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-prefix] gd-reach-pfx (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-prefix '(gd-reach-pfx ?x ?y) '(gd-edge-pfx ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-prefix '(gd-reach-pfx ?x ?z) '(gd-edge-pfx ?x ?y) '(gd-reach-pfx ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-prefix) "
            "  (deduce/fact! gd-edge-pfx 1 2) "
            "  (deduce/fact! gd-edge-pfx 2 5) "
            "  (deduce/fact! gd-edge-pfx 3 4) "
            "  (deduce/fact! gd-edge-pfx 4 5) "
            "  (deduce/fact! gd-edge-pfx 9 10) "
            "  (deduce/fact! gd-edge-pfx 10 5) "
            "  (define selected (deduce/match gd-reach-pfx '(gd-reach-pfx ?src 5) 1 'naive)) "
            "  (define stats-after (deduce/stats gd-reach-pfx)) "
            "  (define step-counters (ref stats-after 'last-goal-directed-read-step-counters)) "
            "  (define step0 (car step-counters)) "
            "  (define step1 (car (cdr step-counters))) "
            "  (and "
            "    (= (length selected) 6) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (length step-counters) 2) "
            "    (= (ref step0 'rows-read) 12) "
            "    (= (ref step1 'rows-read) 16) "
            "    (= (ref step0 'counter-kind) 'observed) "
            "    (= (ref step1 'counter-kind) 'observed)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce match stores observed step counters for preserved-bound prefix pruning");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce match stores observed step counters for preserved-bound prefix pruning (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce match stores observed step counters for preserved-bound prefix pruning");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-prefix-base (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-prefix-base] gd-edge-pfxb (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-prefix-base] gd-gate-pfxb (src) (mid)) "
            "  (define [relation db-admin-goal-directed-match-demand-prefix-base] gd-tail-pfxb (mid) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-prefix-base] gd-path-pfxb (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-prefix-base '(gd-path-pfxb ?x ?y) '(gd-edge-pfxb ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-prefix-base '(gd-path-pfxb ?x ?z) '(gd-path-pfxb ?x ?y) '(gd-gate-pfxb ?x ?m) '(gd-tail-pfxb ?m ?z)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-prefix-base) "
            "  (deduce/fact! gd-edge-pfxb 1 2) "
            "  (deduce/fact! gd-edge-pfxb 1 3) "
            "  (deduce/fact! gd-edge-pfxb 9 90) "
            "  (deduce/fact! gd-edge-pfxb 10 100) "
            "  (deduce/fact! gd-gate-pfxb 1 7) "
            "  (deduce/fact! gd-gate-pfxb 1 8) "
            "  (deduce/fact! gd-gate-pfxb 9 70) "
            "  (deduce/fact! gd-gate-pfxb 9 71) "
            "  (deduce/fact! gd-gate-pfxb 10 72) "
            "  (deduce/fact! gd-gate-pfxb 10 73) "
            "  (deduce/fact! gd-tail-pfxb 7 10) "
            "  (deduce/fact! gd-tail-pfxb 8 11) "
            "  (deduce/fact! gd-tail-pfxb 70 12) "
            "  (deduce/fact! gd-tail-pfxb 71 13) "
            "  (deduce/fact! gd-tail-pfxb 72 14) "
            "  (deduce/fact! gd-tail-pfxb 73 15) "
            "  (define selected (deduce/match gd-path-pfxb '(gd-path-pfxb 1 ?dst) 1 'naive)) "
            "  (define stats-after (deduce/stats gd-path-pfxb)) "
            "  (define step-counters (ref stats-after 'last-goal-directed-read-step-counters)) "
            "  (define step0 (car step-counters)) "
            "  (define step1 (car (cdr step-counters))) "
            "  (define step2 (car (cdr (cdr step-counters)))) "
            "  (and "
            "    (= (length selected) 4) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (length step-counters) 3) "
            "    (= (ref step0 'counter-kind) 'observed) "
            "    (= (ref step1 'counter-kind) 'observed) "
            "    (= (ref step2 'counter-kind) 'observed) "
            "    (= (ref step0 'rows-read) 8) "
            "    (= (ref step1 'rows-read) 24) "
            "    (= (ref step2 'rows-read) 32)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce match prefix-prunes positive non-recursive body scans");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce match prefix-prunes positive non-recursive body scans (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce match prefix-prunes positive non-recursive body scans");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand-multi (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand-multi] gd-edge-mdm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-multi] gd-path-mdm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-multi] gd-other-src-mdm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand-multi] gd-other-out-mdm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-multi '(gd-path-mdm ?x ?y) '(gd-edge-mdm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-multi '(gd-path-mdm ?x ?z) '(gd-path-mdm ?x ?y) '(gd-path-mdm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand-multi '(gd-other-out-mdm ?x ?y) '(gd-other-src-mdm ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand-multi) "
            "  (deduce/fact! gd-edge-mdm 1 2) "
            "  (deduce/fact! gd-edge-mdm 2 3) "
            "  (deduce/fact! gd-edge-mdm 3 4) "
            "  (deduce/fact! gd-other-src-mdm 20 21) "
            "  (define selected (deduce/match gd-path-mdm '(gd-path-mdm 1 4))) "
            "  (define path-stats (deduce/stats gd-path-mdm)) "
            "  (define other-stats (deduce/stats gd-other-out-mdm)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-match-demand-multi)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref path-stats 'dirty-self) nil) "
            "    (= (ref path-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref path-stats 'last-goal-directed-read-surface) 'match) "
            "    (= (ref path-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref path-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (length (ref path-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (ref path-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (length (ref path-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'match) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-path-mdm) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match falls back to selected closure when demanded positions require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match falls back to selected closure when demanded positions require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match falls back to selected closure when demanded positions require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand] gd-edge-srd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand] gd-reach-srd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand] gd-other-src-srd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand] gd-other-out-srd (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand '(gd-reach-srd ?x ?y) '(gd-edge-srd ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand '(gd-reach-srd ?x ?z) '(gd-reach-srd ?x ?y) '(gd-edge-srd ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand '(gd-other-out-srd ?x ?y) '(gd-other-src-srd ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand) "
            "  (deduce/fact! gd-edge-srd 1 2) "
            "  (deduce/fact! gd-edge-srd 2 3) "
            "  (deduce/fact! gd-edge-srd 10 11) "
            "  (deduce/fact! gd-edge-srd 11 12) "
            "  (deduce/fact! gd-other-src-srd 20 21) "
            "  (define selected (deduce/scan-range gd-reach-srd '(1 0) '(1 999) 1 'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-srd)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-scan-range-demand)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (every? (lambda (row) (= (ref row 'src) 1)) selected) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'cardinality-estimate) 0) "
            "    (= (ref reach-stats 'distinct-estimate) 0) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-srd) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce scan-range with equality-bound positions uses ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce scan-range with equality-bound positions uses ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce scan-range with equality-bound positions uses ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-swapped] gd-edge-srds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-swapped] gd-sym-srds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-swapped] gd-other-src-srds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-swapped] gd-other-out-srds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-swapped '(gd-sym-srds ?x ?y) '(gd-edge-srds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-swapped '(gd-sym-srds ?x ?y) '(gd-sym-srds ?y ?x)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-swapped '(gd-other-out-srds ?x ?y) '(gd-other-src-srds ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand-swapped) "
            "  (deduce/fact! gd-edge-srds 1 2) "
            "  (deduce/fact! gd-edge-srds 10 11) "
            "  (deduce/fact! gd-other-src-srds 20 21) "
            "  (define selected (deduce/scan-range gd-sym-srds '(1 0) '(1 999))) "
            "  (define sym-stats (deduce/stats gd-sym-srds)) "
            "  (define other-stats (deduce/stats gd-other-out-srds)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref sym-stats 'dirty-self) true) "
            "    (= (ref sym-stats 'cardinality-estimate) 0) "
            "    (= (ref sym-stats 'distinct-estimate) 0) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce scan-range demand execution supports variable-preserving recursive body reordering");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce scan-range demand execution supports variable-preserving recursive body reordering (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce scan-range demand execution supports variable-preserving recursive body reordering");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand-projected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-projected] gd-edge-srp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-projected] gd-reach-srp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-projected '(gd-reach-srp ?x ?y) '(gd-edge-srp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-projected '(gd-reach-srp ?x ?z) '(gd-reach-srp ?x ?y) '(gd-edge-srp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand-projected) "
            "  (deduce/fact! gd-edge-srp 1 2) "
            "  (deduce/fact! gd-edge-srp 2 3) "
            "  (deduce/fact! gd-edge-srp 10 11) "
            "  (define selected (deduce/scan-range gd-reach-srp '(1 3) '(1 3))) "
            "  (define stats-after (deduce/stats gd-reach-srp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-scan-range-demand-projected)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref stats-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (ref stats-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-srp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce scan-range can project unsupported equality-bound positions out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce scan-range can project unsupported equality-bound positions out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce scan-range can project unsupported equality-bound positions out of the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-demand-selector-projected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-selector-projected] gd-edge-ssp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-demand-selector-projected] gd-reach-ssp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-selector-projected '(gd-reach-ssp ?x ?y) '(gd-edge-ssp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-demand-selector-projected '(gd-reach-ssp ?x ?z) '(gd-reach-ssp ?x ?y) '(gd-edge-ssp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-demand-selector-projected) "
            "  (deduce/fact! gd-edge-ssp 1 2) "
            "  (deduce/fact! gd-edge-ssp 2 3) "
            "  (deduce/fact! gd-edge-ssp 10 11) "
            "  (define selected (deduce/scan-range gd-reach-ssp '(1 3) '(1 3) 1 'semi-naive)) "
            "  (define stats-after (deduce/stats gd-reach-ssp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-scan-range-demand-selector-projected)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref stats-after 'dirty-self) true) "
            "    (= (ref stats-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref stats-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref stats-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref stats-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref stats-after 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref stats-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref stats-after 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref stats-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref stats-after 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref stats-after 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'scan-range) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-ssp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref analyze-after 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce scan-range can project unsupported equality-bound positions out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce scan-range can project unsupported equality-bound positions out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce scan-range can project unsupported equality-bound positions out of the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-unready (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-unready materialized] ancestor-unready (src) (dst)) "
            "  (define schema0 (deduce/schema ancestor-unready)) "
            "  (define stats0 (deduce/stats ancestor-unready)) "
            "  (define analyze0 (deduce/analyze db-admin-materialized-unready)) "
            "  (define payload0 (handle (deduce/refresh! ancestor-unready) (raise msg msg))) "
            "  (define [relation db-admin-materialized-unready] edge-unready (src) (dst)) "
            "  (deduce 'fact! edge-unready 1 2) "
            "  (deduce/rule! db-admin-materialized-unready '(ancestor-unready ?x ?y) '(edge-unready ?x ?y)) "
            "  (define schema1 (deduce/schema ancestor-unready)) "
            "  (define analyze1 (deduce/analyze db-admin-materialized-unready)) "
            "  (define refreshed (deduce/refresh! ancestor-unready)) "
            "  (define schema2 (deduce/schema ancestor-unready)) "
            "  (and "
            "    (= (ref schema0 'materialized-view) true) "
            "    (= (ref schema0 'materialized-derived-ready) false) "
            "    (= (ref schema0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref stats0 'materialized-derived-ready) false) "
            "    (= (ref stats0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref analyze0 'ready-materialized-view-count) 0) "
            "    (= (ref analyze0 'unready-materialized-view-count) 1) "
            "    (dict? payload0) "
            "    (= (ref payload0 'code) 'deduce/refresh-materialized-relation-not-derived) "
            "    (= (ref schema1 'materialized-derived-ready) true) "
            "    (= (ref schema1 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref analyze1 'ready-materialized-view-count) 1) "
            "    (= (ref analyze1 'unready-materialized-view-count) 0) "
            "    (= (ref refreshed 'refreshed-relation) 'ancestor-unready) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (ref schema2 'materialized-stale) false) "
            "    (= (ref schema2 'materialized-derived-ready) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce declared materialized relations become refreshable only after rule install");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce declared materialized relations become refreshable only after rule install (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce declared materialized relations become refreshable only after rule install");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-decl (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-decl] edge-decl (src) (dst)) "
            "  (define [relation db-admin-materialized-decl materialized] ancestor-decl (src) (dst)) "
            "  (deduce 'fact! edge-decl 1 2) "
            "  (deduce/rule! db-admin-materialized-decl '(ancestor-decl ?x ?y) '(edge-decl ?x ?y)) "
            "  (define schema0 (deduce/schema ancestor-decl)) "
            "  (define refreshed (deduce/refresh! ancestor-decl)) "
            "  (define schema1 (deduce/schema ancestor-decl)) "
            "  (and "
            "    (= (ref schema0 'materialized-view) true) "
            "    (= (ref schema0 'materialized-derived-ready) true) "
            "    (= (ref schema0 'materialized-refresh-policy) 'manual) "
            "    (= (ref schema0 'materialized-refresh-count) 0) "
            "    (= (ref schema0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref schema0 'materialized-stale) true) "
            "    (= (ref refreshed 'refreshed-relation) 'ancestor-decl) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (ref schema1 'materialized-stale) false) "
            "    (= (ref schema1 'materialized-refresh-count) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce [relation ... materialized] declaration syntax aligns with refresh metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce [relation ... materialized] declaration syntax aligns with refresh metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce [relation ... materialized] declaration syntax aligns with refresh metadata");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-on-read (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-on-read materialized on-read] ancestor-on-read (src) (dst)) "
            "  (define schema0 (deduce/schema ancestor-on-read)) "
            "  (define payload0 (handle (deduce 'count ancestor-on-read) (raise msg msg))) "
            "  (define [relation db-admin-materialized-on-read] edge-on-read (src) (dst)) "
            "  (deduce 'fact! edge-on-read 1 2) "
            "  (deduce/rule! db-admin-materialized-on-read '(ancestor-on-read ?x ?y) '(edge-on-read ?x ?y)) "
            "  (define schema1 (deduce/schema ancestor-on-read)) "
            "  (define count1 (deduce 'count ancestor-on-read)) "
            "  (define schema2 (deduce/schema ancestor-on-read)) "
            "  (deduce 'fact! edge-on-read 2 3) "
            "  (define count2 (deduce 'count ancestor-on-read)) "
            "  (define schema3 (deduce/schema ancestor-on-read)) "
            "  (and "
            "    (= (ref schema0 'materialized-refresh-policy) 'on-read) "
            "    (= (ref schema0 'materialized-derived-ready) false) "
            "    (= (ref schema0 'materialized-stale) true) "
            "    (dict? payload0) "
            "    (= (ref payload0 'code) 'deduce/on-read-refresh-unavailable) "
            "    (= (ref schema1 'materialized-derived-ready) true) "
            "    (= (ref schema1 'materialized-stale) true) "
            "    (= count1 1) "
            "    (= (ref schema2 'materialized-stale) false) "
            "    (= (ref schema2 'materialized-refresh-count) 1) "
            "    (= (ref schema2 'materialized-refresh-policy) 'on-read) "
            "    (= count2 2) "
            "    (= (ref schema3 'materialized-refresh-count) 2) "
            "    (= (ref schema3 'materialized-last-stale-reason) nil) "
            "    (= (ref schema3 'materialized-stale) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce on-read materialized relations refresh on ordinary stale reads once derived");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce on-read materialized relations refresh on ordinary stale reads once derived (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce on-read materialized relations refresh on ordinary stale reads once derived");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialize-miss (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialize-miss] ext-only (id) (name)) "
            "  (define payload (handle (deduce 'materialize! ext-only) (raise msg msg))) "
            "  (and (dict? payload) "
            "       (= (ref payload 'code) 'deduce/materialize-relation-not-derived)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce materialize! rejects non-derived relations");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce materialize! rejects non-derived relations (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce materialize! rejects non-derived relations");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized] edge (src) (dst)) "
            "  (define [relation db-admin-materialized] ancestor (src) (dst)) "
            "  (define [relation db-admin-materialized materialized] pending-view (src) (dst)) "
            "  (deduce 'fact! edge 1 2) "
            "  (deduce/rule! db-admin-materialized '(ancestor ?x ?y) '(edge ?x ?y)) "
            "  (deduce/materialize! ancestor) "
            "  (define schema0 (deduce/schema ancestor)) "
            "  (define pending0 (deduce/schema pending-view)) "
            "  (define analyze0 (deduce/analyze db-admin-materialized)) "
            "  (define refreshed (deduce/refresh! db-admin-materialized)) "
            "  (define schema1 (deduce/schema ancestor)) "
            "  (define stats1 (deduce/stats ancestor)) "
            "  (define pending-stats1 (deduce/stats pending-view)) "
            "  (define pending1 (deduce/schema pending-view)) "
            "  (deduce 'fact! edge 2 3) "
            "  (define schema2 (deduce/schema ancestor)) "
            "  (and "
            "    (= (ref schema0 'materialized-view) true) "
            "    (= (ref schema0 'materialized-derived-ready) true) "
            "    (= (ref schema0 'materialized-refresh-policy) 'manual) "
            "    (= (ref schema0 'materialized-refresh-count) 0) "
            "    (= (ref schema0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref schema0 'materialized-stale) true) "
            "    (= (ref pending0 'materialized-derived-ready) false) "
            "    (= (ref pending0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref analyze0 'materialized-view-count) 2) "
            "    (= (ref analyze0 'ready-materialized-view-count) 1) "
            "    (= (ref analyze0 'unready-materialized-view-count) 1) "
            "    (= (ref analyze0 'stale-materialized-view-count) 2) "
            "    (= (car (ref analyze0 'ready-materialized-relations)) 'ancestor) "
            "    (= (car (ref analyze0 'unready-materialized-relations)) 'pending-view) "
            "    (= (car (ref analyze0 'stale-materialized-relations)) 'ancestor) "
            "    (= (car (cdr (ref analyze0 'stale-materialized-relations))) 'pending-view) "
            "    (= (car (ref analyze0 'incremental-dirty-predicates)) 'edge) "
            "    (= (ref analyze0 'incremental-invalidation-mode) 'tracked) "
            "    (= (ref refreshed 'kind) 'deduce-refresh) "
            "    (= (ref refreshed 'refresh-scope) 'database) "
            "    (= (ref refreshed 'refresh-execution-path) 'db-wide) "
            "    (= (ref refreshed 'refresh-fallback-reason) nil) "
            "    (= (ref refreshed 'status) 'ok) "
            "    (= (ref refreshed 'stale-materialized-view-count) 1) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (car (ref refreshed 'refreshed-materialized-relations)) 'ancestor) "
            "    (= (ref refreshed 'cleared-dirty-predicates) nil) "
            "    (= (ref refreshed 'remaining-dirty-predicates) nil) "
            "    (= (car (ref refreshed 'remaining-stale-materialized-relations)) 'pending-view) "
            "    (= (deduce 'count ancestor) 1) "
            "    (= (ref schema1 'materialized-stale) false) "
            "    (= (ref schema1 'materialized-refresh-count) 1) "
            "    (= (ref stats1 'materialized-view) true) "
            "    (= (ref stats1 'materialized-derived-ready) true) "
            "    (= (ref stats1 'materialized-refresh-count) 1) "
            "    (= (ref stats1 'dirty-predicates) nil) "
            "    (= (ref stats1 'dirty-self) false) "
            "    (= (ref stats1 'materialized-stale) false) "
            "    (= (ref pending1 'materialized-stale) true) "
            "    (= (ref pending1 'materialized-derived-ready) false) "
            "    (= (ref pending-stats1 'dirty-self) false) "
            "    (= (ref schema2 'materialized-stale) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce materialized relation metadata and manual refresh stay aligned with dirty tracking");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce materialized relation metadata and manual refresh stay aligned with dirty tracking (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce materialized relation metadata and manual refresh stay aligned with dirty tracking");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-empty (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-empty] edge-empty-refresh (src) (dst)) "
            "  (define [relation db-admin-materialized-empty] ancestor-empty-refresh (src) (dst)) "
            "  (deduce/rule! db-admin-materialized-empty '(ancestor-empty-refresh ?x ?y) '(edge-empty-refresh ?x ?y)) "
            "  (deduce/rule! db-admin-materialized-empty '(ancestor-empty-refresh ?x ?z) '(edge-empty-refresh ?x ?y) '(ancestor-empty-refresh ?y ?z)) "
            "  (deduce/materialize! ancestor-empty-refresh) "
            "  (define stats0 (deduce/stats ancestor-empty-refresh)) "
            "  (define analyze0 (deduce/analyze db-admin-materialized-empty)) "
            "  (define refreshed (deduce/refresh! ancestor-empty-refresh)) "
            "  (define stats1 (deduce/stats ancestor-empty-refresh)) "
            "  (and "
            "    (= (ref stats0 'materialized-derived-ready) true) "
            "    (= (ref stats0 'materialized-refresh-count) 0) "
            "    (= (ref stats0 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref stats0 'materialized-stale) true) "
            "    (= (ref analyze0 'ready-materialized-view-count) 1) "
            "    (= (ref analyze0 'stale-materialized-view-count) 1) "
            "    (= (ref refreshed 'kind) 'deduce-refresh) "
            "    (= (ref refreshed 'refresh-scope) 'relation) "
            "    (= (ref refreshed 'refresh-execution-path) 'targeted) "
            "    (= (ref refreshed 'refresh-fallback-reason) nil) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (car (ref refreshed 'refreshed-materialized-relations)) 'ancestor-empty-refresh) "
            "    (= (ref refreshed 'cleared-dirty-predicates) nil) "
            "    (= (ref refreshed 'remaining-dirty-predicates) nil) "
            "    (= (ref refreshed 'remaining-stale-materialized-relations) nil) "
            "    (= (deduce 'count ancestor-empty-refresh) 0) "
            "    (= (ref stats1 'materialized-refresh-count) 1) "
            "    (= (ref stats1 'materialized-last-stale-reason) nil) "
            "    (= (ref stats1 'materialized-stale) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce refresh! keeps empty materialized relations deterministic");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce refresh! keeps empty materialized relations deterministic (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce refresh! keeps empty materialized relations deterministic");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-missing-maintenance (deduce 'open 'memory)) "
            "  (define [relation db-admin-missing-maintenance] edge-missing (src) (dst)) "
            "  (define [relation db-admin-missing-maintenance] ancestor-missing (src) (dst)) "
            "  (deduce/rule! db-admin-missing-maintenance '(ancestor-missing ?x ?y) '(edge-missing ?x ?y)) "
            "  (deduce/materialize! ancestor-missing) "
            "  (deduce 'drop! ancestor-missing) "
            "  (define stats-payload (deduce/stats ancestor-missing)) "
            "  (define refresh-payload (handle (deduce/refresh! ancestor-missing) (raise msg msg))) "
            "  (and "
            "    (= (ref stats-payload 'kind) 'deduce-stats) "
            "    (= (ref stats-payload 'status) 'ok) "
            "    (= (ref stats-payload 'relation) 'ancestor-missing) "
            "    (= (ref stats-payload 'materialized-view) true) "
            "    (= (ref stats-payload 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref stats-payload 'dropped) true) "
            "    (dict? refresh-payload) "
            "    (= (ref refresh-payload 'code) 'deduce/refresh-relation-dropped)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats/refresh keep dropped relation handling deterministic");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats/refresh keep dropped relation handling deterministic (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats/refresh keep dropped relation handling deterministic");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-refresh-reject (deduce 'open 'memory)) "
            "  (define [relation db-admin-refresh-reject] edge-r (src) (dst)) "
            "  (define [relation db-admin-refresh-reject] ancestor-r (src) (dst)) "
            "  (deduce/rule! db-admin-refresh-reject '(ancestor-r ?x ?y) '(edge-r ?x ?y)) "
            "  (define payload (handle (deduce/refresh! ancestor-r) (raise msg msg))) "
            "  (and (dict? payload) "
            "       (= (ref payload 'code) 'deduce/refresh-relation-not-materialized)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce refresh! rejects non-materialized relation handles");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce refresh! rejects non-materialized relation handles (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce refresh! rejects non-materialized relation handles");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-cleanup-errors (deduce 'open 'memory)) "
            "  (define [relation db-admin-cleanup-errors] cleanup-person (name) (age)) "
            "  (define db-admin-cleanup-other (deduce 'open 'memory)) "
            "  (define [relation db-admin-cleanup-other] cleanup-other (name) (age)) "
            "  (define tx (deduce 'block db-admin-cleanup-errors 'write)) "
            "  (define arity-payload (handle (deduce 'retract! cleanup-person \"Alice\") (raise msg msg))) "
            "  (define mismatch-payload (handle (deduce 'retract! tx cleanup-other \"Alice\" 30) (raise msg msg))) "
            "  (deduce 'abort tx) "
            "  (define clear-payload (handle (deduce 'clear! db-admin-cleanup-errors) (raise msg msg))) "
            "  (define drop-payload (handle (deduce 'drop! db-admin-cleanup-errors) (raise msg msg))) "
            "  (and "
            "    (dict? arity-payload) "
            "    (= (ref arity-payload 'code) 'deduce/retract-arity-mismatch) "
            "    (dict? mismatch-payload) "
            "    (= (ref mismatch-payload 'code) 'deduce/retract-transaction-database-mismatch) "
            "    (dict? clear-payload) "
            "    (= (ref clear-payload 'code) 'deduce/clear-relation-not-deduce-relation) "
            "    (dict? drop-payload) "
            "    (= (ref drop-payload 'code) 'deduce/drop-relation-not-deduce-relation) "
            "    (= (deduce 'count cleanup-person) 0) "
            "    (= (deduce 'count cleanup-other) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce cleanup write-intent errors stay code-first under schema and handle mismatch");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce cleanup write-intent errors stay code-first under schema and handle mismatch (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce cleanup write-intent errors stay code-first under schema and handle mismatch");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-rel (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-rel] edge-rel (src) (dst)) "
            "  (define [relation db-admin-materialized-rel] ancestor-rel (src) (dst)) "
            "  (define [relation db-admin-materialized-rel] ancestor-peer-rel (src) (dst)) "
            "  (deduce 'fact! edge-rel 1 2) "
            "  (deduce/rule! db-admin-materialized-rel '(ancestor-rel ?x ?y) '(edge-rel ?x ?y)) "
            "  (deduce/rule! db-admin-materialized-rel '(ancestor-peer-rel ?x ?y) '(edge-rel ?x ?y)) "
            "  (deduce/materialize! ancestor-rel) "
            "  (deduce/materialize! ancestor-peer-rel) "
            "  (deduce/refresh! db-admin-materialized-rel) "
            "  (deduce 'fact! edge-rel 2 3) "
            "  (define stale-schema (deduce/schema ancestor-rel)) "
            "  (define stale-peer-schema (deduce/schema ancestor-peer-rel)) "
            "  (define stale-stats (deduce/stats ancestor-rel)) "
            "  (define stale-peer-stats (deduce/stats ancestor-peer-rel)) "
            "  (define refreshed (deduce/refresh! ancestor-rel)) "
            "  (define fresh-schema (deduce/schema ancestor-rel)) "
            "  (define still-stale-peer-schema (deduce/schema ancestor-peer-rel)) "
            "  (define still-stale-peer-stats (deduce/stats ancestor-peer-rel)) "
            "  (define analyze-after (deduce/analyze db-admin-materialized-rel)) "
            "  (and "
            "    (= (ref stale-schema 'materialized-stale) true) "
            "    (= (ref stale-peer-schema 'materialized-stale) true) "
            "    (= (ref stale-schema 'materialized-last-stale-reason) 'dependency-dirty) "
            "    (> (ref stale-schema 'materialized-last-stale-mutation-epoch) 0) "
            "    (= (car (ref stale-stats 'dirty-predicates)) 'edge-rel) "
            "    (= (car (cdr (ref stale-stats 'dirty-predicates))) 'ancestor-rel) "
            "    (= (car (cdr (cdr (ref stale-stats 'dirty-predicates)))) 'ancestor-peer-rel) "
            "    (= (ref stale-stats 'dirty-self) true) "
            "    (= (ref stale-peer-stats 'dirty-self) true) "
            "    (= (ref refreshed 'kind) 'deduce-refresh) "
            "    (= (ref refreshed 'refresh-scope) 'relation) "
            "    (= (ref refreshed 'refresh-execution-path) 'incremental-targeted) "
            "    (= (ref refreshed 'refresh-fallback-reason) nil) "
            "    (= (ref refreshed 'requested-refresh-relation) 'ancestor-rel) "
            "    (= (ref refreshed 'refreshed-relation) 'ancestor-rel) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (car (ref refreshed 'refreshed-materialized-relations)) 'ancestor-rel) "
            "    (= (car (ref refreshed 'cleared-dirty-predicates)) 'edge-rel) "
            "    (= (car (cdr (ref refreshed 'cleared-dirty-predicates))) 'ancestor-rel) "
            "    (= (car (ref refreshed 'remaining-dirty-predicates)) 'ancestor-peer-rel) "
            "    (= (car (ref refreshed 'remaining-stale-materialized-relations)) 'ancestor-peer-rel) "
            "    (= (deduce 'count ancestor-rel) 2) "
            "    (= (deduce 'count ancestor-peer-rel) 1) "
            "    (= (ref refreshed 'materialized-stale) false) "
            "    (= (ref fresh-schema 'materialized-stale) false) "
            "    (= (ref fresh-schema 'materialized-refresh-count) 2) "
            "    (= (ref fresh-schema 'materialized-last-stale-reason) nil) "
            "    (= (ref still-stale-peer-schema 'materialized-stale) true) "
            "    (= (ref still-stale-peer-stats 'dirty-self) true) "
            "    (= (ref analyze-after 'stale-materialized-view-count) 1) "
            "    (= (car (ref analyze-after 'stale-materialized-relations)) 'ancestor-peer-rel)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce relation-scoped refresh and stale-reason metadata stay aligned");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce relation-scoped refresh and stale-reason metadata stay aligned (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce relation-scoped refresh and stale-reason metadata stay aligned");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-chain (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-chain] edge-chain (src) (dst)) "
            "  (define [relation db-admin-materialized-chain] mid-chain (src) (dst)) "
            "  (define [relation db-admin-materialized-chain] top-chain (src) (dst)) "
            "  (deduce 'fact! edge-chain 1 2) "
            "  (deduce/rule! db-admin-materialized-chain '(mid-chain ?x ?y) '(edge-chain ?x ?y)) "
            "  (deduce/rule! db-admin-materialized-chain '(top-chain ?x ?y) '(mid-chain ?x ?y)) "
            "  (deduce/materialize! mid-chain) "
            "  (deduce/materialize! top-chain) "
            "  (deduce/refresh! db-admin-materialized-chain) "
            "  (deduce 'fact! edge-chain 2 3) "
            "  (define mid-refreshed (deduce/refresh! mid-chain)) "
            "  (define top-stale-schema (deduce/schema top-chain)) "
            "  (define top-stale-count (deduce 'count top-chain)) "
            "  (define top-refreshed (deduce/refresh! top-chain)) "
            "  (define top-fresh-schema (deduce/schema top-chain)) "
            "  (and "
            "    (= (ref mid-refreshed 'refresh-execution-path) 'incremental-targeted) "
            "    (= (ref mid-refreshed 'remaining-dirty-predicates) '(top-chain)) "
            "    (= (ref top-stale-schema 'materialized-stale) true) "
            "    (= (ref top-stale-schema 'materialized-last-stale-reason) 'dependency-dirty) "
            "    (= top-stale-count 1) "
            "    (= (ref top-refreshed 'refresh-execution-path) 'incremental-targeted) "
            "    (= (ref top-refreshed 'refresh-fallback-reason) nil) "
            "    (= (ref top-refreshed 'refreshed-relation) 'top-chain) "
            "    (= (deduce 'count mid-chain) 2) "
            "    (= (deduce 'count top-chain) 2) "
            "    (= (ref top-fresh-schema 'materialized-refresh-count) 2) "
            "    (= (ref top-fresh-schema 'materialized-stale) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce incremental refresh can propagate one step through a refreshed materialized source");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce incremental refresh can propagate one step through a refreshed materialized source (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce incremental refresh can propagate one step through a refreshed materialized source");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-rule-change (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-rule-change] edge-a (src) (dst)) "
            "  (define [relation db-admin-materialized-rule-change] edge-b (src) (dst)) "
            "  (define [relation db-admin-materialized-rule-change] ancestor-rule-change (src) (dst)) "
            "  (define [relation db-admin-materialized-rule-change] ancestor-rule-change-peer (src) (dst)) "
            "  (deduce 'fact! edge-a 1 2) "
            "  (deduce/rule! db-admin-materialized-rule-change '(ancestor-rule-change ?x ?y) '(edge-a ?x ?y)) "
            "  (deduce/rule! db-admin-materialized-rule-change '(ancestor-rule-change-peer ?x ?y) '(edge-a ?x ?y)) "
            "  (deduce/materialize! ancestor-rule-change) "
            "  (deduce/materialize! ancestor-rule-change-peer) "
            "  (deduce/refresh! ancestor-rule-change) "
            "  (deduce/refresh! ancestor-rule-change-peer) "
            "  (define schema0 (deduce/schema ancestor-rule-change)) "
            "  (define stats0 (deduce/stats ancestor-rule-change)) "
            "  (deduce/rule! db-admin-materialized-rule-change '(ancestor-rule-change ?x ?y) '(edge-b ?x ?y)) "
            "  (define schema1 (deduce/schema ancestor-rule-change)) "
            "  (define stats1 (deduce/stats ancestor-rule-change)) "
            "  (define refreshed (deduce/refresh! ancestor-rule-change)) "
            "  (define stats2 (deduce/stats ancestor-rule-change)) "
            "  (and "
            "    (= (ref schema0 'materialized-stale) false) "
            "    (= (ref schema0 'materialized-last-stale-reason) nil) "
            "    (= (ref stats0 'full-recompute-required) false) "
            "    (= (ref schema1 'materialized-stale) true) "
            "    (= (ref schema1 'materialized-last-stale-reason) 'rule-set-change) "
            "    (= (ref stats1 'materialized-last-stale-reason) 'rule-set-change) "
            "    (= (ref stats1 'full-recompute-required) true) "
            "    (= (ref refreshed 'refresh-scope) 'relation) "
            "    (= (ref refreshed 'refresh-execution-path) 'db-wide) "
            "    (= (ref refreshed 'refresh-fallback-reason) 'full-recompute-required) "
            "    (= (ref refreshed 'requested-refresh-relation) 'ancestor-rule-change) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 2) "
            "    (= (car (ref refreshed 'refreshed-materialized-relations)) 'ancestor-rule-change) "
            "    (= (car (cdr (ref refreshed 'refreshed-materialized-relations))) 'ancestor-rule-change-peer) "
            "    (= (ref refreshed 'cleared-dirty-predicates) nil) "
            "    (= (ref refreshed 'remaining-dirty-predicates) nil) "
            "    (= (ref refreshed 'remaining-stale-materialized-relations) nil) "
            "    (= (ref stats2 'full-recompute-required) false) "
            "    (= (ref stats2 'materialized-stale) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce rule install invalidates refreshed materialized views with rule-set-change");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce rule install invalidates refreshed materialized views with rule-set-change (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce rule install invalidates refreshed materialized views with rule-set-change");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-materialized-rule-change-aligned (deduce 'open 'memory)) "
            "  (define [relation db-admin-materialized-rule-change-aligned] edge-a (src) (dst)) "
            "  (define [relation db-admin-materialized-rule-change-aligned] edge-b (src) (dst)) "
            "  (define [relation db-admin-materialized-rule-change-aligned] ancestor-a (src) (dst)) "
            "  (define [relation db-admin-materialized-rule-change-aligned] ancestor-peer-a (src) (dst)) "
            "  (deduce 'fact! edge-a 1 2) "
            "  (deduce/rule! db-admin-materialized-rule-change-aligned '(ancestor-a ?x ?y) '(edge-a ?x ?y)) "
            "  (deduce/rule! db-admin-materialized-rule-change-aligned '(ancestor-peer-a ?x ?y) '(edge-a ?x ?y)) "
            "  (deduce/materialize! ancestor-a) "
            "  (deduce/materialize! ancestor-peer-a) "
            "  (deduce/refresh! ancestor-a) "
            "  (deduce/refresh! ancestor-peer-a) "
            "  (deduce/rule! db-admin-materialized-rule-change-aligned '(ancestor-a ?x ?y) '(edge-b ?x ?y)) "
            "  (define stats1 (deduce/stats ancestor-a)) "
            "  (define analyze1 (deduce/analyze db-admin-materialized-rule-change-aligned)) "
            "  (define refreshed (deduce/refresh! ancestor-a)) "
            "  (define analyze2 (deduce/analyze db-admin-materialized-rule-change-aligned)) "
            "  (define stats2 (deduce/stats ancestor-a)) "
            "  (and "
            "    (= (ref stats1 'full-recompute-required) true) "
            "    (= (ref stats1 'materialized-stale) true) "
            "    (= (ref analyze1 'incremental-invalidation-mode) 'full-recompute) "
            "    (= (ref analyze1 'stale-materialized-view-count) 2) "
            "    (= (ref refreshed 'refresh-execution-path) 'targeted) "
            "    (= (ref refreshed 'refresh-fallback-reason) nil) "
            "    (= (ref refreshed 'refreshed-materialized-view-count) 1) "
            "    (= (car (ref refreshed 'remaining-stale-materialized-relations)) 'ancestor-peer-a) "
            "    (= (ref analyze2 'incremental-invalidation-mode) 'tracked) "
            "    (= (ref analyze2 'stale-materialized-view-count) 1) "
            "    (= (car (ref analyze2 'stale-materialized-relations)) 'ancestor-peer-a) "
            "    (= (ref stats2 'full-recompute-required) false) "
            "    (= (ref stats2 'materialized-stale) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats, analyze, and refresh stay aligned across degraded rule-change recovery");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats, analyze, and refresh stay aligned across degraded rule-change recovery (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats, analyze, and refresh stay aligned across degraded rule-change recovery");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-dematerialize (deduce 'open 'memory)) "
            "  (define [relation db-admin-dematerialize] edge-dem (src) (dst)) "
            "  (define [relation db-admin-dematerialize] ancestor-dem (src) (dst)) "
            "  (deduce 'fact! edge-dem 1 2) "
            "  (deduce/rule! db-admin-dematerialize '(ancestor-dem ?x ?y) '(edge-dem ?x ?y)) "
            "  (deduce/materialize! ancestor-dem) "
            "  (deduce/refresh! ancestor-dem) "
            "  (define schema0 (deduce/schema ancestor-dem)) "
            "  (define analyze0 (deduce/analyze db-admin-dematerialize)) "
            "  (deduce/dematerialize! ancestor-dem) "
            "  (define schema1 (deduce/schema ancestor-dem)) "
            "  (define stats1 (deduce/stats ancestor-dem)) "
            "  (define analyze1 (deduce/analyze db-admin-dematerialize)) "
            "  (define refresh-payload (handle (deduce/refresh! ancestor-dem) (raise msg msg))) "
            "  (deduce/materialize! ancestor-dem) "
            "  (define schema2 (deduce/schema ancestor-dem)) "
            "  (define analyze2 (deduce/analyze db-admin-dematerialize)) "
            "  (and "
            "    (= (ref schema0 'materialized-view) true) "
            "    (= (ref schema0 'materialized-refresh-count) 1) "
            "    (= (ref schema0 'materialized-stale) nil) "
            "    (= (ref analyze0 'materialized-view-count) 1) "
            "    (= (ref analyze0 'ready-materialized-view-count) 1) "
            "    (= (ref schema1 'materialized-view) nil) "
            "    (= (ref schema1 'materialized-derived-ready) nil) "
            "    (= (ref schema1 'materialized-refresh-policy) nil) "
            "    (= (ref schema1 'materialized-refresh-count) 0) "
            "    (= (ref schema1 'materialized-last-refresh-mutation-epoch) nil) "
            "    (= (ref schema1 'materialized-last-stale-mutation-epoch) nil) "
            "    (= (ref schema1 'materialized-last-stale-reason) nil) "
            "    (= (ref schema1 'materialized-stale) nil) "
            "    (= (ref stats1 'materialized-view) nil) "
            "    (= (ref stats1 'materialized-refresh-count) 0) "
            "    (= (ref analyze1 'materialized-view-count) 0) "
            "    (= (ref analyze1 'ready-materialized-view-count) 0) "
            "    (= (ref analyze1 'unready-materialized-view-count) 0) "
            "    (= (ref analyze1 'stale-materialized-view-count) 0) "
            "    (= (dict? refresh-payload) true) "
            "    (= (ref refresh-payload 'code) 'deduce/refresh-relation-not-materialized) "
            "    (= (ref schema2 'materialized-view) true) "
            "    (= (ref schema2 'materialized-derived-ready) true) "
            "    (= (ref schema2 'materialized-refresh-policy) 'manual) "
            "    (= (ref schema2 'materialized-refresh-count) 0) "
            "    (= (ref schema2 'materialized-last-stale-reason) 'never-refreshed) "
            "    (= (ref schema2 'materialized-stale) true) "
            "    (= (ref analyze2 'materialized-view-count) 1) "
            "    (= (ref analyze2 'ready-materialized-view-count) 1) "
            "    (= (ref analyze2 'stale-materialized-view-count) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce dematerialize! clears materialized lifecycle state without dropping the relation");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce dematerialize! clears materialized lifecycle state without dropping the relation (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce dematerialize! clears materialized lifecycle state without dropping the relation");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-positive (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-positive] gd-edge (src) (dst)) "
            "  (define [relation db-admin-goal-directed-positive] gd-reach (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-positive '(gd-reach ?x ?y) '(gd-edge ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-positive '(gd-reach ?x ?z) '(gd-edge ?x ?y) '(gd-reach ?y ?z)) "
            "  (define analyze (deduce/analyze db-admin-goal-directed-positive)) "
            "  (define comps (ref analyze 'goal-directed-components)) "
            "  (define first (car comps)) "
            "  (and "
            "    (= (ref analyze 'goal-directed-recursive-component-count) 1) "
            "    (= (ref analyze 'goal-directed-eligible-component-count) 1) "
            "    (= (ref analyze 'goal-directed-blocked-component-count) 0) "
            "    (= (ref analyze 'goal-directed-aggregate-blocked-component-count) 0) "
            "    (= (ref analyze 'goal-directed-negated-blocked-component-count) 0) "
            "    (= (length comps) 1) "
            "    (= (ref first 'rule-count) 2) "
            "    (= (ref first 'goal-directed-eligible) true) "
            "    (= (ref first 'goal-directed-shape) 'positive-recursive-closure) "
            "    (= (ref first 'goal-directed-blockers) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze reports goal-directed eligible recursive component counts");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze reports goal-directed eligible recursive component counts (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze reports goal-directed eligible recursive component counts");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-blocked (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-blocked] gd-edge-b (src) (dst)) "
            "  (define [relation db-admin-goal-directed-blocked] gd-blocked-b (node)) "
            "  (define [relation db-admin-goal-directed-blocked] gd-chosen-b (src) (score)) "
            "  (define [relation db-admin-goal-directed-blocked] gd-score-b (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-blocked '(gd-chosen-b ?x ?n) '(gd-score-b ?x ?n) '(not (gd-blocked-b ?x))) "
            "  (deduce/rule! db-admin-goal-directed-blocked '(gd-score-b ?x (sum ?n)) '(gd-edge-b ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-blocked '(gd-score-b ?x (sum ?n)) '(gd-edge-b ?x ?y) '(gd-chosen-b ?y ?n)) "
            "  (define analyze (deduce/analyze db-admin-goal-directed-blocked)) "
            "  (define comps (ref analyze 'goal-directed-components)) "
            "  (define first (car comps)) "
            "  (define blockers (ref first 'goal-directed-blockers)) "
            "  (and "
            "    (= (ref analyze 'goal-directed-recursive-component-count) 1) "
            "    (= (ref analyze 'goal-directed-eligible-component-count) 0) "
            "    (= (ref analyze 'goal-directed-blocked-component-count) 1) "
            "    (= (ref analyze 'goal-directed-aggregate-blocked-component-count) 1) "
            "    (= (ref analyze 'goal-directed-negated-blocked-component-count) 1) "
            "    (= (length comps) 1) "
            "    (= (ref first 'rule-count) 3) "
            "    (= (ref first 'goal-directed-eligible) nil) "
            "    (= (ref first 'goal-directed-shape) nil) "
            "    (= (car blockers) 'aggregates-present) "
            "    (= (car (cdr blockers)) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze reports goal-directed blocker counts for recursive aggregate and negated shapes");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze reports goal-directed blocker counts for recursive aggregate and negated shapes (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze reports goal-directed blocker counts for recursive aggregate and negated shapes");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-parallel-batches (deduce 'open 'memory)) "
            "  (define [relation db-admin-parallel-batches] pb-edge-a (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-reach-a (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-edge-b (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-reach-b (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-seed-c (src) (dst)) "
            "  (define [relation db-admin-parallel-batches] pb-reach-c (src) (dst)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-a ?x ?y) '(pb-edge-a ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-a ?x ?z) '(pb-edge-a ?x ?y) '(pb-reach-a ?y ?z)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-b ?x ?y) '(pb-edge-b ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-b ?x ?z) '(pb-edge-b ?x ?y) '(pb-reach-b ?y ?z)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-c ?x ?y) '(pb-seed-c ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-batches '(pb-reach-c ?x ?z) '(pb-reach-c ?x ?y) '(pb-reach-a ?y ?z)) "
            "  (define analyze (deduce/analyze db-admin-parallel-batches)) "
            "  (and "
            "    (= (ref analyze 'parallel-runtime-mode) 'metadata-only) "
            "    (= (ref analyze 'parallel-recursive-component-count) 3) "
            "    (= (ref analyze 'parallel-batched-component-count) 2) "
            "    (= (ref analyze 'parallel-batch-count) 2) "
            "    (= (ref analyze 'parallel-max-batch-size) 2)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze reports parallel recursive batch counts from SCC topology");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze reports parallel recursive batch counts from SCC topology (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze reports parallel recursive batch counts from SCC topology");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-parallel-stats (deduce 'open 'memory)) "
            "  (define [relation db-admin-parallel-stats] pb-edge-a (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-reach-a (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-edge-b (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-reach-b (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-seed-c (src) (dst)) "
            "  (define [relation db-admin-parallel-stats] pb-reach-c (src) (dst)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-a ?x ?y) '(pb-edge-a ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-a ?x ?z) '(pb-edge-a ?x ?y) '(pb-reach-a ?y ?z)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-b ?x ?y) '(pb-edge-b ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-b ?x ?z) '(pb-edge-b ?x ?y) '(pb-reach-b ?y ?z)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-c ?x ?y) '(pb-seed-c ?x ?y)) "
            "  (deduce/rule! db-admin-parallel-stats '(pb-reach-c ?x ?z) '(pb-reach-c ?x ?y) '(pb-reach-a ?y ?z)) "
            "  (define analyze (deduce/analyze db-admin-parallel-stats)) "
            "  (define stats (deduce/stats pb-reach-a)) "
            "  (define analyze-topology (ref analyze 'parallel-batch-topology)) "
            "  (define stats-topology (ref stats 'parallel-batch-topology)) "
            "  (and "
            "    (= (ref stats 'parallel-runtime-mode) 'metadata-only) "
            "    (= (ref stats 'parallel-recursive-component-count) 3) "
            "    (= (ref stats 'parallel-batched-component-count) 2) "
            "    (= (ref stats 'parallel-batch-count) 2) "
            "    (= (ref stats 'parallel-max-batch-size) 2) "
            "    (= (length analyze-topology) 2) "
            "    (= (length stats-topology) 2) "
            "    (= (ref (car analyze-topology) 'stratum) 0) "
            "    (= (ref (car analyze-topology) 'wave) 1) "
            "    (= (ref (car analyze-topology) 'batch-size) 2) "
            "    (= (ref (car (cdr analyze-topology)) 'stratum) 0) "
            "    (= (ref (car (cdr analyze-topology)) 'wave) 2) "
            "    (= (ref (car (cdr analyze-topology)) 'batch-size) 1) "
            "    (= (ref (car stats-topology) 'stratum) 0) "
            "    (= (ref (car stats-topology) 'wave) 1) "
            "    (= (ref (car stats-topology) 'batch-size) 2) "
            "    (= (ref (car (cdr stats-topology)) 'stratum) 0) "
            "    (= (ref (car (cdr stats-topology)) 'wave) 2) "
            "    (= (ref (car (cdr stats-topology)) 'batch-size) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats reports parallel recursive batch counts from SCC topology");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats reports parallel recursive batch counts from SCC topology (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats reports parallel recursive batch counts from SCC topology");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-selected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-selected] gd-edge-s (src) (dst)) "
            "  (define [relation db-admin-goal-directed-selected] gd-reach-s (src) (dst)) "
            "  (define [relation db-admin-goal-directed-selected] gd-other-src-s (src) (dst)) "
            "  (define [relation db-admin-goal-directed-selected] gd-other-out-s (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-selected '(gd-reach-s ?x ?y) '(gd-edge-s ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-selected '(gd-reach-s ?x ?z) '(gd-edge-s ?x ?y) '(gd-reach-s ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-selected '(gd-other-out-s ?x ?y) '(gd-other-src-s ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-selected) "
            "  (deduce/fact! gd-edge-s 1 2) "
            "  (deduce/fact! gd-edge-s 2 3) "
            "  (deduce/fact! gd-other-src-s 9 10) "
            "  (define selected (deduce/analyze db-admin-goal-directed-selected 1)) "
            "  (define selected-components (ref selected 'goal-directed-selected-components)) "
            "  (define selected-predicates (ref selected 'goal-directed-selected-predicates)) "
            "  (define other-before (deduce/count gd-other-out-s)) "
            "  (define full (deduce/analyze db-admin-goal-directed-selected)) "
            "  (define other-after (deduce/count gd-other-out-s)) "
            "  (and "
            "    (= (ref selected 'goal-directed-execution-path) 'selected-component-closure) "
            "    (= (ref selected 'goal-directed-selector-rule-index) 1) "
            "    (= (ref selected 'goal-directed-selected-component-id) 1) "
            "    (= (length selected-components) 2) "
            "    (= (car selected-components) 0) "
            "    (= (car (cdr selected-components)) 1) "
            "    (= (car selected-predicates) 'gd-reach-s) "
            "    (= (car (cdr selected-predicates)) 'gd-edge-s) "
            "    (= (length selected-predicates) 2) "
            "    (= (ref selected 'incremental-dirty-predicate-count) 4) "
            "    (= (deduce/count gd-reach-s) 3) "
            "    (= other-before 0) "
            "    (= (ref full 'goal-directed-execution-path) 'full-db-fixpoint) "
            "    (= (ref full 'goal-directed-selected-components) nil) "
            "    (= (ref full 'goal-directed-selected-predicates) nil) "
            "    (= (ref full 'incremental-dirty-predicate-count) 2) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze can execute only the selected goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze can execute only the selected goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze can execute only the selected goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-dirty-frontier-selected (deduce 'open 'memory)) "
            "  (define [relation db-admin-dirty-frontier-selected] edge-s (src) (dst)) "
            "  (define [relation db-admin-dirty-frontier-selected] reach-s (src) (dst)) "
            "  (define [relation db-admin-dirty-frontier-selected] other-src-s (src) (dst)) "
            "  (define [relation db-admin-dirty-frontier-selected] other-out-s (src) (dst)) "
            "  (deduce/rule! db-admin-dirty-frontier-selected '(reach-s ?x ?y) '(edge-s ?x ?y)) "
            "  (deduce/rule! db-admin-dirty-frontier-selected '(reach-s ?x ?z) '(edge-s ?x ?y) '(reach-s ?y ?z)) "
            "  (deduce/rule! db-admin-dirty-frontier-selected '(other-out-s ?x ?y) '(other-src-s ?x ?y)) "
            "  (deduce/analyze db-admin-dirty-frontier-selected) "
            "  (deduce/fact! edge-s 1 2) "
            "  (deduce/fact! edge-s 2 3) "
            "  (deduce/fact! other-src-s 9 10) "
            "  (deduce/analyze db-admin-dirty-frontier-selected 1) "
            "  (define reach-stats (deduce/stats reach-s)) "
            "  (define other-stats (deduce/stats other-out-s)) "
            "  (and "
            "    (= (ref reach-stats 'dirty-predicate-count) 2) "
            "    (= (car (ref reach-stats 'dirty-predicates)) 'other-src-s) "
            "    (= (car (cdr (ref reach-stats 'dirty-predicates))) 'other-out-s) "
            "    (= (ref reach-stats 'dirty-self) false) "
            "    (= (ref other-stats 'dirty-predicate-count) 2) "
            "    (= (car (ref other-stats 'dirty-predicates)) 'other-src-s) "
            "    (= (car (cdr (ref other-stats 'dirty-predicates))) 'other-out-s) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats keeps a global dirty frontier while dirty-self stays relation-local");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats keeps a global dirty frontier while dirty-self stays relation-local (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats keeps a global dirty frontier while dirty-self stays relation-local");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-explain-classification (deduce 'open 'memory)) "
            "  (define [relation db-admin-explain-classification] gd-edge-ex (src) (dst)) "
            "  (define [relation db-admin-explain-classification] gd-reach-ex (src) (dst)) "
            "  (deduce/rule! db-admin-explain-classification '(gd-reach-ex ?x ?y) '(gd-edge-ex ?x ?y)) "
            "  (deduce/rule! db-admin-explain-classification '(gd-reach-ex ?x ?z) '(gd-edge-ex ?x ?y) '(gd-reach-ex ?y ?z)) "
            "  (deduce/fact! gd-edge-ex 1 2) "
            "  (deduce/fact! gd-edge-ex 2 3) "
            "  (deduce/query gd-reach-ex (lambda (row) (= (ref row 'src) 1))) "
            "  (define explain (deduce/explain db-admin-explain-classification 1)) "
            "  (define steps (ref explain 'steps)) "
            "  (define step0 (car steps)) "
            "  (define step1 (car (cdr steps))) "
            "  (and "
            "    (= (ref explain 'surface-kind) 'planner-snapshot) "
            "    (= (ref explain 'goal-directed-execution-path) 'selected-component-closure) "
            "    (= (ref explain 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref explain 'last-goal-directed-read-surface) 'query) "
            "    (= (ref explain 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref explain 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (ref explain 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length steps) 2) "
            "    (= (ref (ref step0 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref step1 'counters) 'counter-kind) 'observed)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce explain keeps planner fields separate from last runtime read metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce explain keeps planner fields separate from last runtime read metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce explain keeps planner fields separate from last runtime read metadata");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-analyze-classification (deduce 'open 'memory)) "
            "  (define [relation db-admin-analyze-classification] gd-edge-an (src) (dst)) "
            "  (define [relation db-admin-analyze-classification] gd-reach-an (src) (dst)) "
            "  (deduce/rule! db-admin-analyze-classification '(gd-reach-an ?x ?y) '(gd-edge-an ?x ?y)) "
            "  (deduce/rule! db-admin-analyze-classification '(gd-reach-an ?x ?z) '(gd-edge-an ?x ?y) '(gd-reach-an ?y ?z)) "
            "  (deduce/fact! gd-edge-an 1 2) "
            "  (deduce/fact! gd-edge-an 2 3) "
            "  (deduce/query gd-reach-an (lambda (row) (= (ref row 'src) 1))) "
            "  (define analyze (deduce/analyze db-admin-analyze-classification 1)) "
            "  (define rules (ref analyze 'rule-execution)) "
            "  (define rule0 (car rules)) "
            "  (define rule1 (car (cdr rules))) "
            "  (define rule0-step0 (car (ref rule0 'steps))) "
            "  (define rule1-step0 (car (ref rule1 'steps))) "
            "  (define rule1-step1 (car (cdr (ref rule1 'steps)))) "
            "  (and "
            "    (= (ref analyze 'goal-directed-execution-path) 'selected-component-closure) "
            "    (= (ref analyze 'goal-directed-selector-rule-index) 1) "
            "    (= (ref analyze 'goal-directed-selected-component-id) 1) "
            "    (= (ref analyze 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze 'last-goal-directed-read-relation) 'gd-reach-an) "
            "    (= (ref analyze 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (length rules) 2) "
            "    (= (ref rule0 'rule-index) 0) "
            "    (= (ref rule1 'rule-index) 1) "
            "    (= (ref rule0 'head-predicate) 'gd-reach-an) "
            "    (= (ref rule1 'head-predicate) 'gd-reach-an) "
            "    (= (length (ref rule0 'steps)) 1) "
            "    (= (length (ref rule1 'steps)) 2) "
            "    (= (ref (ref rule0-step0 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref rule1-step0 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref rule1-step1 'counters) 'counter-kind) 'observed)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze keeps selector planner fields separate from last runtime read metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze keeps selector planner fields separate from last runtime read metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze keeps selector planner fields separate from last runtime read metadata");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-conjunctive-order (deduce 'open 'memory)) "
            "  (define [relation db-admin-conjunctive-order] ord-src (id) (k)) "
            "  (define [relation db-admin-conjunctive-order] ord-dim (k) (v)) "
            "  (define [relation db-admin-conjunctive-order] ord-out (id) (v)) "
            "  (deduce/rule! db-admin-conjunctive-order '(ord-out ?id ?v) '(ord-src ?id ?k) '(ord-dim ?k ?v)) "
            "  (deduce/fact! ord-src 1 10) "
            "  (deduce/fact! ord-src 2 20) "
            "  (deduce/fact! ord-dim 10 100) "
            "  (deduce/fact! ord-dim 20 200) "
            "  (deduce/fact! ord-dim 30 300) "
            "  (define explain (deduce/explain db-admin-conjunctive-order 0)) "
            "  (define analyze (deduce/analyze db-admin-conjunctive-order)) "
            "  (define rule0 (car (ref analyze 'rule-execution))) "
            "  (define explain-steps (ref explain 'steps)) "
            "  (define analyze-steps (ref rule0 'steps)) "
            "  (define explain-step0 (car explain-steps)) "
            "  (define explain-step1 (car (cdr explain-steps))) "
            "  (define analyze-step0 (car analyze-steps)) "
            "  (define analyze-step1 (car (cdr analyze-steps))) "
            "  (and "
            "    (= (ref explain 'join-order) '(ord-src ord-dim)) "
            "    (= (ref rule0 'join-order) '(ord-src ord-dim)) "
            "    (= (ref explain-step0 'predicate) 'ord-src) "
            "    (= (ref explain-step1 'predicate) 'ord-dim) "
            "    (= (ref analyze-step0 'predicate) 'ord-src) "
            "    (= (ref analyze-step1 'predicate) 'ord-dim) "
            "    (= (ref explain-step0 'operator) 'full-scan) "
            "    (= (ref explain-step1 'operator) 'index-scan) "
            "    (= (ref analyze-step0 'operator) 'full-scan) "
            "    (= (ref analyze-step1 'operator) 'index-scan) "
            "    (= (ref explain-step0 'selected-index) nil) "
            "    (= (ref explain-step1 'selected-index) 0) "
            "    (= (ref analyze-step0 'selected-index) nil) "
            "    (= (ref analyze-step1 'selected-index) 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce explain/analyze keep conjunctive join order and operator fields aligned");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce explain/analyze keep conjunctive join order and operator fields aligned (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce explain/analyze keep conjunctive join order and operator fields aligned");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-counter-truth (deduce 'open 'memory)) "
            "  (define [relation db-admin-counter-truth] ctr-edge (src) (dst)) "
            "  (define [relation db-admin-counter-truth] ctr-reach (src) (dst)) "
            "  (deduce/rule! db-admin-counter-truth '(ctr-reach ?x ?y) '(ctr-edge ?x ?y)) "
            "  (deduce/rule! db-admin-counter-truth '(ctr-reach ?x ?z) '(ctr-edge ?x ?y) '(ctr-reach ?y ?z)) "
            "  (deduce/fact! ctr-edge 1 2) "
            "  (deduce/fact! ctr-edge 2 5) "
            "  (deduce/fact! ctr-edge 3 4) "
            "  (deduce/fact! ctr-edge 4 5) "
            "  (deduce/fact! ctr-edge 9 10) "
            "  (deduce/fact! ctr-edge 10 5) "
            "  (deduce/match ctr-reach '(ctr-reach ?src 5) 1 'naive) "
            "  (define stats (deduce/stats ctr-reach)) "
            "  (define analyze (deduce/analyze db-admin-counter-truth 1)) "
            "  (define explain (deduce/explain db-admin-counter-truth 1)) "
            "  (define stats-steps (ref stats 'last-goal-directed-read-step-counters)) "
            "  (define analyze-rule1 (car (cdr (ref analyze 'rule-execution)))) "
            "  (define analyze-steps (ref analyze-rule1 'steps)) "
            "  (define explain-steps (ref explain 'steps)) "
            "  (and "
            "    (= (length stats-steps) 2) "
            "    (= (ref (car stats-steps) 'counter-kind) 'observed) "
            "    (= (ref (car (cdr stats-steps)) 'counter-kind) 'observed) "
            "    (= (ref (car stats-steps) 'rows-read) 12) "
            "    (= (ref (car (cdr stats-steps)) 'rows-read) 16) "
            "    (= (length analyze-steps) 2) "
            "    (= (ref (ref (car analyze-steps) 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref (car (cdr analyze-steps)) 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref (car analyze-steps) 'counters) 'rows-read) 18) "
            "    (= (ref (ref (car (cdr analyze-steps)) 'counters) 'rows-read) 54) "
            "    (= (length explain-steps) 2) "
            "    (= (ref (ref (car explain-steps) 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref (car (cdr explain-steps)) 'counters) 'counter-kind) 'observed) "
            "    (= (ref (ref (car explain-steps) 'counters) 'rows-read) 6) "
            "    (= (ref (ref (car (cdr explain-steps)) 'counters) 'rows-read) 54)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats, analyze, and explain keep conjunctive observed counters on their own execution surfaces");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats, analyze, and explain keep conjunctive observed counters on their own execution surfaces (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats, analyze, and explain keep conjunctive observed counters on their own execution surfaces");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-txn-commit-insert (deduce 'open 'memory)) "
            "  (define [relation db-admin-txn-commit-insert] edge-txc (src) (dst)) "
            "  (define [relation db-admin-txn-commit-insert] reach-txc (src) (dst)) "
            "  (deduce/rule! db-admin-txn-commit-insert '(reach-txc ?x ?y) '(edge-txc ?x ?y)) "
            "  (deduce/rule! db-admin-txn-commit-insert '(reach-txc ?x ?z) '(edge-txc ?x ?y) '(reach-txc ?y ?z)) "
            "  (define analyze0 (deduce/analyze db-admin-txn-commit-insert)) "
            "  (define txc (deduce 'block db-admin-txn-commit-insert 'write)) "
            "  (deduce 'fact! txc edge-txc 1 2) "
            "  (deduce 'fact! txc edge-txc 2 3) "
            "  (deduce 'commit txc) "
            "  (define analyze1 (deduce/analyze db-admin-txn-commit-insert)) "
            "  (define db-admin-txn-abort-insert (deduce 'open 'memory)) "
            "  (define [relation db-admin-txn-abort-insert] edge-txa (src) (dst)) "
            "  (define [relation db-admin-txn-abort-insert] reach-txa (src) (dst)) "
            "  (deduce/rule! db-admin-txn-abort-insert '(reach-txa ?x ?y) '(edge-txa ?x ?y)) "
            "  (deduce/rule! db-admin-txn-abort-insert '(reach-txa ?x ?z) '(edge-txa ?x ?y) '(reach-txa ?y ?z)) "
            "  (define analyze2 (deduce/analyze db-admin-txn-abort-insert)) "
            "  (define txa (deduce 'block db-admin-txn-abort-insert 'write)) "
            "  (deduce 'fact! txa edge-txa 1 2) "
            "  (deduce 'fact! txa edge-txa 2 3) "
            "  (deduce 'abort txa) "
            "  (define analyze3 (deduce/analyze db-admin-txn-abort-insert)) "
            "  (define db-admin-txn-commit-delete (deduce 'open 'memory)) "
            "  (define [relation db-admin-txn-commit-delete] edge-tdc (src) (dst)) "
            "  (define [relation db-admin-txn-commit-delete] reach-tdc (src) (dst)) "
            "  (deduce/rule! db-admin-txn-commit-delete '(reach-tdc ?x ?y) '(edge-tdc ?x ?y)) "
            "  (deduce/rule! db-admin-txn-commit-delete '(reach-tdc ?x ?z) '(edge-tdc ?x ?y) '(reach-tdc ?y ?z)) "
            "  (deduce 'fact! edge-tdc 1 2) "
            "  (deduce 'fact! edge-tdc 2 3) "
            "  (define txd (deduce 'block db-admin-txn-commit-delete 'write)) "
            "  (deduce 'retract! txd edge-tdc 2 3) "
            "  (deduce 'commit txd) "
            "  (define analyze4 (deduce/analyze db-admin-txn-commit-delete)) "
            "  (and "
            "    (= (ref analyze0 'incremental-dirty-predicate-count) 0) "
            "    (= (ref analyze0 'incremental-invalidation-mode) 'tracked) "
            "    (= (ref analyze1 'incremental-dirty-predicate-count) 2) "
            "    (= (ref analyze1 'incremental-invalidation-mode) 'tracked) "
            "    (= (car (ref analyze1 'incremental-dirty-predicates)) 'edge-txc) "
            "    (= (car (cdr (ref analyze1 'incremental-dirty-predicates))) 'reach-txc) "
            "    (= (deduce 'count edge-txc) 2) "
            "    (= (ref analyze2 'incremental-dirty-predicate-count) 0) "
            "    (= (ref analyze2 'incremental-invalidation-mode) 'tracked) "
            "    (= (ref analyze3 'incremental-dirty-predicate-count) 0) "
            "    (= (ref analyze3 'incremental-invalidation-mode) 'tracked) "
            "    (= (ref analyze3 'incremental-dirty-predicates) nil) "
            "    (= (deduce 'count edge-txa) 0) "
            "    (= (ref analyze4 'incremental-dirty-predicate-count) 2) "
            "    (= (ref analyze4 'incremental-invalidation-mode) 'full-recompute) "
            "    (= (car (ref analyze4 'incremental-dirty-predicates)) 'edge-tdc) "
            "    (= (car (cdr (ref analyze4 'incremental-dirty-predicates))) 'reach-tdc) "
            "    (= (deduce 'count edge-tdc) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce commit applies deferred mutation logs while abort drops them");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce commit applies deferred mutation logs while abort drops them (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce commit applies deferred mutation logs while abort drops them");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-degraded-analyze (deduce 'open 'memory)) "
            "  (define [relation db-admin-degraded-analyze] edge-dr (src) (dst)) "
            "  (define [relation db-admin-degraded-analyze] reach-dr (src) (dst)) "
            "  (deduce 'fact! edge-dr 1 2) "
            "  (deduce 'fact! edge-dr 2 3) "
            "  (deduce/rule! db-admin-degraded-analyze '(reach-dr ?x ?y) '(edge-dr ?x ?y)) "
            "  (deduce/rule! db-admin-degraded-analyze '(reach-dr ?x ?z) '(edge-dr ?x ?y) '(reach-dr ?y ?z)) "
            "  (deduce/analyze db-admin-degraded-analyze) "
            "  (deduce 'retract! edge-dr 2 3) "
            "  (define stats0 (deduce/stats reach-dr)) "
            "  (define analyze0 (deduce/analyze db-admin-degraded-analyze)) "
            "  (define stats1 (deduce/stats reach-dr)) "
            "  (and "
            "    (= (ref stats0 'full-recompute-required) true) "
            "    (= (ref analyze0 'incremental-invalidation-mode) 'full-recompute) "
            "    (= (ref analyze0 'incremental-dirty-predicate-count) 2) "
            "    (= (car (ref analyze0 'incremental-dirty-predicates)) 'edge-dr) "
            "    (= (car (cdr (ref analyze0 'incremental-dirty-predicates))) 'reach-dr) "
            "    (= (ref stats1 'full-recompute-required) false) "
            "    (= (ref stats1 'dirty-predicate-count) 0) "
            "    (= (deduce 'count reach-dr) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze snapshots degraded full-recompute state before clearing it");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze snapshots degraded full-recompute state before clearing it (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze snapshots degraded full-recompute state before clearing it");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-aggregate-select (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-aggregate-select] gd-edge-a (src) (dst)) "
            "  (define [relation db-admin-goal-directed-aggregate-select] gd-score-a (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-aggregate-select '(gd-score-a ?x (sum ?n)) '(gd-edge-a ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-aggregate-select '(gd-score-a ?x (sum ?n)) '(gd-edge-a ?x ?y) '(gd-score-a ?y ?n)) "
            "  (define payload (handle (deduce/analyze db-admin-goal-directed-aggregate-select 1) (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/analyze-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'aggregates-present)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze rejects goal-directed selector execution for recursive aggregate components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze rejects goal-directed selector execution for recursive aggregate components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze rejects goal-directed selector execution for recursive aggregate components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match] gd-edge-m (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match] gd-reach-m (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match] gd-other-src-m (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match] gd-other-out-m (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match '(gd-reach-m ?x ?y) '(gd-edge-m ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match '(gd-reach-m ?x ?z) '(gd-edge-m ?x ?y) '(gd-reach-m ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match '(gd-other-out-m ?x ?y) '(gd-other-src-m ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match) "
            "  (deduce/fact! gd-edge-m 1 2) "
            "  (deduce/fact! gd-edge-m 2 3) "
            "  (deduce/fact! gd-other-src-m 9 10) "
            "  (define selected (deduce/match gd-reach-m '(gd-reach-m ?x ?y) 1)) "
            "  (define other-stats (deduce/stats gd-other-out-m)) "
            "  (define other-before (deduce/count gd-other-out-m)) "
            "  (deduce/analyze db-admin-goal-directed-match) "
            "  (define other-after (deduce/count gd-other-out-m)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match can execute only the selected goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match can execute only the selected goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match can execute only the selected goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-demand (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-edge-md (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-reach-md (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-other-src-md (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-demand] gd-other-out-md (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand '(gd-reach-md ?x ?y) '(gd-edge-md ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand '(gd-reach-md ?x ?z) '(gd-reach-md ?x ?y) '(gd-edge-md ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-demand '(gd-other-out-md ?x ?y) '(gd-other-src-md ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-demand) "
            "  (deduce/fact! gd-edge-md 1 2) "
            "  (deduce/fact! gd-edge-md 2 3) "
            "  (deduce/fact! gd-edge-md 10 11) "
            "  (deduce/fact! gd-edge-md 11 12) "
            "  (deduce/fact! gd-other-src-md 20 21) "
            "  (define selected (deduce/match gd-reach-md '(gd-reach-md 1 ?y) 1)) "
            "  (define reach-stats (deduce/stats gd-reach-md)) "
            "  (define other-stats (deduce/stats gd-other-out-md)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (every? (lambda (row) (or (= (ref row 'y) 2) (= (ref row 'y) 3))) selected) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'cardinality-estimate) 0) "
            "    (= (ref reach-stats 'distinct-estimate) 0) "
            "    (= (ref other-stats 'dirty-self) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match can run selector-bound recursive reads ephemerally without clearing dirty state");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match can run selector-bound recursive reads ephemerally without clearing dirty state (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match can run selector-bound recursive reads ephemerally without clearing dirty state");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-mismatch (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-mismatch] gd-edge-mm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-mismatch] gd-reach-mm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-mismatch] gd-other-src-mm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-mismatch] gd-other-out-mm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-mismatch '(gd-reach-mm ?x ?y) '(gd-edge-mm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-mismatch '(gd-reach-mm ?x ?z) '(gd-edge-mm ?x ?y) '(gd-reach-mm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-mismatch '(gd-other-out-mm ?x ?y) '(gd-other-src-mm ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-mismatch) "
            "  (define payload "
            "    (handle "
            "      (deduce/match gd-other-out-mm '(gd-other-out-mm ?x ?y) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/match-goal-directed-selector-does-not-produce-relation) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (ref data 'relation) 'gd-other-out-mm)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match rejects goal-directed selector closures that do not produce the requested relation");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match rejects goal-directed selector closures that do not produce the requested relation (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match rejects goal-directed selector closures that do not produce the requested relation");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-aggregate (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-aggregate] gd-edge-ma (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-aggregate] gd-score-ma (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-match-aggregate '(gd-score-ma ?x (sum ?n)) '(gd-edge-ma ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-match-aggregate '(gd-score-ma ?x (sum ?n)) '(gd-edge-ma ?x ?y) '(gd-score-ma ?y ?n)) "
            "  (define payload "
            "    (handle "
            "      (deduce/match gd-score-ma '(gd-score-ma ?x ?n) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/match-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'aggregates-present)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match rejects goal-directed selectors for recursive aggregate components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match rejects goal-directed selectors for recursive aggregate components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match rejects goal-directed selectors for recursive aggregate components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-negated (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-negated] gd-edge-mn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-negated] gd-blocked-mn (node)) "
            "  (define [relation db-admin-goal-directed-match-negated] gd-reach-mn (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-negated '(gd-reach-mn ?x ?y) '(gd-edge-mn ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-negated '(gd-reach-mn ?x ?z) '(gd-edge-mn ?x ?y) '(gd-reach-mn ?y ?z) '(not (gd-blocked-mn ?z))) "
            "  (define payload "
            "    (handle "
            "      (deduce/match gd-reach-mn '(gd-reach-mn ?x ?y) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/match-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce match rejects goal-directed selectors for recursive negated components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce match rejects goal-directed selectors for recursive negated components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce match rejects goal-directed selectors for recursive negated components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-negated-select (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-negated-select] gd-edge-n (src) (dst)) "
            "  (define [relation db-admin-goal-directed-negated-select] gd-blocked-n (node)) "
            "  (define [relation db-admin-goal-directed-negated-select] gd-reach-n (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-negated-select '(gd-reach-n ?x ?y) '(gd-edge-n ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-negated-select '(gd-reach-n ?x ?z) '(gd-edge-n ?x ?y) '(gd-reach-n ?y ?z) '(not (gd-blocked-n ?z))) "
            "  (define payload (handle (deduce/analyze db-admin-goal-directed-negated-select 1) (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/analyze-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze rejects goal-directed selector execution for recursive negated components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze rejects goal-directed selector execution for recursive negated components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze rejects goal-directed selector execution for recursive negated components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query] gd-edge-q (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query] gd-reach-q (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query] gd-other-src-q (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query] gd-other-out-q (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query '(gd-reach-q ?x ?y) '(gd-edge-q ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query '(gd-reach-q ?x ?z) '(gd-edge-q ?x ?y) '(gd-reach-q ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query '(gd-other-out-q ?x ?y) '(gd-other-src-q ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query) "
            "  (deduce/fact! gd-edge-q 1 2) "
            "  (deduce/fact! gd-edge-q 2 3) "
            "  (deduce/fact! gd-other-src-q 9 10) "
            "  (define selected (deduce/query gd-reach-q (lambda (row) true) 1)) "
            "  (define other-stats (deduce/stats gd-other-out-q)) "
            "  (define other-before (deduce/count gd-other-out-q)) "
            "  (deduce/analyze db-admin-goal-directed-query) "
            "  (define other-after (deduce/count gd-other-out-q)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query can execute only the selected goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query can execute only the selected goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query can execute only the selected goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-mismatch (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-mismatch] gd-edge-qm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-mismatch] gd-reach-qm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-mismatch] gd-other-src-qm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-mismatch] gd-other-out-qm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-mismatch '(gd-reach-qm ?x ?y) '(gd-edge-qm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-mismatch '(gd-reach-qm ?x ?z) '(gd-edge-qm ?x ?y) '(gd-reach-qm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-mismatch '(gd-other-out-qm ?x ?y) '(gd-other-src-qm ?x ?y)) "
            "  (define payload "
            "    (handle "
            "      (deduce/query gd-other-out-qm (lambda (row) true) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/query-goal-directed-selector-does-not-produce-relation) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (ref data 'relation) 'gd-other-out-qm)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query rejects goal-directed selector closures that do not produce the requested relation");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query rejects goal-directed selector closures that do not produce the requested relation (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query rejects goal-directed selector closures that do not produce the requested relation");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-aggregate (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-aggregate] gd-edge-qa (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-aggregate] gd-score-qa (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-query-aggregate '(gd-score-qa ?x (sum ?n)) '(gd-edge-qa ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-query-aggregate '(gd-score-qa ?x (sum ?n)) '(gd-edge-qa ?x ?y) '(gd-score-qa ?y ?n)) "
            "  (define payload "
            "    (handle "
            "      (deduce/query gd-score-qa (lambda (row) true) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/query-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'aggregates-present)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query rejects goal-directed selectors for recursive aggregate components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query rejects goal-directed selectors for recursive aggregate components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query rejects goal-directed selectors for recursive aggregate components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-negated (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-negated] gd-edge-qn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-negated] gd-blocked-qn (node)) "
            "  (define [relation db-admin-goal-directed-query-negated] gd-reach-qn (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-negated '(gd-reach-qn ?x ?y) '(gd-edge-qn ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-negated '(gd-reach-qn ?x ?z) '(gd-edge-qn ?x ?y) '(gd-reach-qn ?y ?z) '(not (gd-blocked-qn ?z))) "
            "  (define payload "
            "    (handle "
            "      (deduce/query gd-reach-qn (lambda (row) true) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/query-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query rejects goal-directed selectors for recursive negated components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query rejects goal-directed selectors for recursive negated components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query rejects goal-directed selectors for recursive negated components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-readers (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-readers] gd-edge-r (src) (dst)) "
            "  (define [relation db-admin-goal-directed-readers] gd-reach-r (src) (dst)) "
            "  (define [relation db-admin-goal-directed-readers] gd-other-src-r (src) (dst)) "
            "  (define [relation db-admin-goal-directed-readers] gd-other-out-r (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-readers '(gd-reach-r ?x ?y) '(gd-edge-r ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-readers '(gd-reach-r ?x ?z) '(gd-edge-r ?x ?y) '(gd-reach-r ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-readers '(gd-other-out-r ?x ?y) '(gd-other-src-r ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-readers) "
            "  (deduce/fact! gd-edge-r 1 2) "
            "  (deduce/fact! gd-edge-r 2 3) "
            "  (deduce/fact! gd-other-src-r 9 10) "
            "  (define count-selected (deduce/count gd-reach-r 1)) "
            "  (define scan-selected (deduce/scan gd-reach-r 1)) "
            "  (define range-selected (deduce/scan-range gd-reach-r '(1 2) '(2 3) 1)) "
            "  (define other-stats (deduce/stats gd-other-out-r)) "
            "  (define other-before (deduce/count gd-other-out-r)) "
            "  (deduce/analyze db-admin-goal-directed-readers) "
            "  (define other-after (deduce/count gd-other-out-r)) "
            "  (and "
            "    (= count-selected 3) "
            "    (= (length scan-selected) 3) "
            "    (= (length range-selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce count, scan, and scan-range can execute only the selected goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce count, scan, and scan-range can execute only the selected goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce count, scan, and scan-range can execute only the selected goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-count-mismatch (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-count-mismatch] gd-edge-cm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-mismatch] gd-reach-cm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-mismatch] gd-other-src-cm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-mismatch] gd-other-out-cm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-count-mismatch '(gd-reach-cm ?x ?y) '(gd-edge-cm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-count-mismatch '(gd-reach-cm ?x ?z) '(gd-edge-cm ?x ?y) '(gd-reach-cm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-count-mismatch '(gd-other-out-cm ?x ?y) '(gd-other-src-cm ?x ?y)) "
            "  (define payload "
            "    (handle "
            "      (deduce/count gd-other-out-cm 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/count-goal-directed-selector-does-not-produce-relation) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (ref data 'relation) 'gd-other-out-cm)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce count rejects goal-directed selector closures that do not produce the requested relation");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce count rejects goal-directed selector closures that do not produce the requested relation (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce count rejects goal-directed selector closures that do not produce the requested relation");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-aggregate (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-aggregate] gd-edge-sa (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-aggregate] gd-score-sa (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-scan-aggregate '(gd-score-sa ?x (sum ?n)) '(gd-edge-sa ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-scan-aggregate '(gd-score-sa ?x (sum ?n)) '(gd-edge-sa ?x ?y) '(gd-score-sa ?y ?n)) "
            "  (define payload "
            "    (handle "
            "      (deduce/scan gd-score-sa 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/scan-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'aggregates-present)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce scan rejects goal-directed selectors for recursive aggregate components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce scan rejects goal-directed selectors for recursive aggregate components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce scan rejects goal-directed selectors for recursive aggregate components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-negated (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-negated] gd-edge-srn (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-negated] gd-blocked-srn (node)) "
            "  (define [relation db-admin-goal-directed-scan-range-negated] gd-reach-srn (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-negated '(gd-reach-srn ?x ?y) '(gd-edge-srn ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-negated '(gd-reach-srn ?x ?z) '(gd-edge-srn ?x ?y) '(gd-reach-srn ?y ?z) '(not (gd-blocked-srn ?z))) "
            "  (define payload "
            "    (handle "
            "      (deduce/scan-range gd-reach-srn '(1 2) '(9 9) 1) "
            "      (raise msg msg))) "
            "  (define data (ref payload 'data)) "
            "  (define blockers (ref data 'goal-directed-blockers)) "
            "  (and "
            "    (dict? payload) "
            "    (= (ref payload 'code) 'deduce/scan-range-goal-directed-selector-not-eligible) "
            "    (= (ref data 'rule-index) 1) "
            "    (= (car blockers) 'negated-body-atom)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce scan-range rejects goal-directed selectors for recursive negated components");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce scan-range rejects goal-directed selectors for recursive negated components (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce scan-range rejects goal-directed selectors for recursive negated components");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand] gd-edge-qd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand] gd-reach-qd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand] gd-other-src-qd (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand] gd-other-out-qd (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand '(gd-reach-qd ?x ?y) '(gd-edge-qd ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand '(gd-reach-qd ?x ?z) '(gd-edge-qd ?x ?y) '(gd-reach-qd ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand '(gd-other-out-qd ?x ?y) '(gd-other-src-qd ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand) "
            "  (deduce/fact! gd-edge-qd 1 2) "
            "  (deduce/fact! gd-edge-qd 2 3) "
            "  (deduce/fact! gd-edge-qd 10 11) "
            "  (deduce/fact! gd-other-src-qd 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qd "
            "      (lambda (row) (= (ref row 'dst) 3)) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qd)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qd) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use ephemeral demand execution for simple equality filters");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use ephemeral demand execution for simple equality filters (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use ephemeral demand execution for simple equality filters");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-plain] gd-edge-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-plain] gd-reach-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-plain] gd-other-src-qdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-plain] gd-other-out-qdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-plain '(gd-reach-qdp ?x ?y) '(gd-edge-qdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-plain '(gd-reach-qdp ?x ?z) '(gd-edge-qdp ?x ?y) '(gd-reach-qdp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-plain '(gd-other-out-qdp ?x ?y) '(gd-other-src-qdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-plain) "
            "  (deduce/fact! gd-edge-qdp 1 2) "
            "  (deduce/fact! gd-edge-qdp 2 3) "
            "  (deduce/fact! gd-edge-qdp 10 11) "
            "  (deduce/fact! gd-other-src-qdp 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdp "
            "      (lambda (row) (= (ref row 'dst) 3)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use ephemeral demand execution for simple equality filters");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use ephemeral demand execution for simple equality filters (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use ephemeral demand execution for simple equality filters");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured] gd-edge-qdc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured] gd-reach-qdc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured] gd-other-src-qdc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured] gd-other-out-qdc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured '(gd-reach-qdc ?x ?y) '(gd-edge-qdc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured '(gd-reach-qdc ?x ?z) '(gd-edge-qdc ?x ?y) '(gd-reach-qdc ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured '(gd-other-out-qdc ?x ?y) '(gd-other-src-qdc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured) "
            "  (deduce/fact! gd-edge-qdc 1 2) "
            "  (deduce/fact! gd-edge-qdc 2 3) "
            "  (deduce/fact! gd-edge-qdc 10 11) "
            "  (deduce/fact! gd-other-src-qdc 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdc "
            "        (lambda (row) (= (ref row 'dst) target)) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdc) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use captured constant equality filters in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use captured constant equality filters in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use captured constant equality filters in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-plain] gd-edge-qdcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-plain] gd-reach-qdcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-plain] gd-other-src-qdcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-plain] gd-other-out-qdcp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-plain '(gd-reach-qdcp ?x ?y) '(gd-edge-qdcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-plain '(gd-reach-qdcp ?x ?z) '(gd-edge-qdcp ?x ?y) '(gd-reach-qdcp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-plain '(gd-other-out-qdcp ?x ?y) '(gd-other-src-qdcp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-plain) "
            "  (deduce/fact! gd-edge-qdcp 1 2) "
            "  (deduce/fact! gd-edge-qdcp 2 3) "
            "  (deduce/fact! gd-edge-qdcp 10 11) "
            "  (deduce/fact! gd-other-src-qdcp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdcp "
            "        (lambda (row) (= (ref row 'dst) target))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdcp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use captured constant equality filters in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use captured constant equality filters in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use captured constant equality filters in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper] gd-edge-qdcw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper] gd-reach-qdcw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper] gd-other-src-qdcw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper] gd-other-out-qdcw (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper '(gd-reach-qdcw ?x ?y) '(gd-edge-qdcw ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper '(gd-reach-qdcw ?x ?z) '(gd-edge-qdcw ?x ?y) '(gd-reach-qdcw ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper '(gd-other-out-qdcw ?x ?y) '(gd-other-src-qdcw ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper) "
            "  (deduce/fact! gd-edge-qdcw 1 2) "
            "  (deduce/fact! gd-edge-qdcw 2 3) "
            "  (deduce/fact! gd-edge-qdcw 10 11) "
            "  (deduce/fact! gd-other-src-qdcw 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want (lambda (candidate) (= (ref candidate 'dst) target))) "
            "        (deduce/query gd-reach-qdcw "
            "          (lambda (row) (want row)) "
            "          1 "
            "          'semi-naive)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcw)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdcw) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap one-arg captured closure wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap one-arg captured closure wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap one-arg captured closure wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-plain] gd-edge-qdcwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-plain] gd-reach-qdcwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-plain] gd-other-src-qdcwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-plain] gd-other-out-qdcwp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-plain '(gd-reach-qdcwp ?x ?y) '(gd-edge-qdcwp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-plain '(gd-reach-qdcwp ?x ?z) '(gd-edge-qdcwp ?x ?y) '(gd-reach-qdcwp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-plain '(gd-other-out-qdcwp ?x ?y) '(gd-other-src-qdcwp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-plain) "
            "  (deduce/fact! gd-edge-qdcwp 1 2) "
            "  (deduce/fact! gd-edge-qdcwp 2 3) "
            "  (deduce/fact! gd-edge-qdcwp 10 11) "
            "  (deduce/fact! gd-other-src-qdcwp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want (lambda (candidate) (= (ref candidate 'dst) target))) "
            "        (deduce/query gd-reach-qdcwp "
            "          (lambda (row) (want row)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdcwp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap one-arg captured closure wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap one-arg captured closure wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap one-arg captured closure wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-row-middle (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle] gd-edge-qdcwrm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle] gd-reach-qdcwrm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle] gd-other-src-qdcwrm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle] gd-other-out-qdcwrm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-row-middle '(gd-reach-qdcwrm ?x ?y) '(gd-edge-qdcwrm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-row-middle '(gd-reach-qdcwrm ?x ?z) '(gd-edge-qdcwrm ?x ?y) '(gd-reach-qdcwrm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-row-middle '(gd-other-out-qdcwrm ?x ?y) '(gd-other-src-qdcwrm ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-row-middle) "
            "  (deduce/fact! gd-edge-qdcwrm 1 2) "
            "  (deduce/fact! gd-edge-qdcwrm 2 3) "
            "  (deduce/fact! gd-edge-qdcwrm 10 11) "
            "  (deduce/fact! gd-other-src-qdcwrm 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (delta 0) "
            "        (let (want (lambda (expected candidate shift) (= (ref candidate 'dst) (+ expected shift)))) "
            "          (deduce/query gd-reach-qdcwrm "
            "            (lambda (row) (want target row delta)) "
            "            1 "
            "            'semi-naive))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwrm)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-row-middle)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap preserved-row wrappers with non-leading row and multiple literal args");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap preserved-row wrappers with non-leading row and multiple literal args (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap preserved-row wrappers with non-leading row and multiple literal args");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain] gd-edge-qdcwrmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain] gd-reach-qdcwrmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain] gd-other-src-qdcwrmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain] gd-other-out-qdcwrmp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain '(gd-reach-qdcwrmp ?x ?y) '(gd-edge-qdcwrmp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain '(gd-reach-qdcwrmp ?x ?z) '(gd-edge-qdcwrmp ?x ?y) '(gd-reach-qdcwrmp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain '(gd-other-out-qdcwrmp ?x ?y) '(gd-other-src-qdcwrmp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain) "
            "  (deduce/fact! gd-edge-qdcwrmp 1 2) "
            "  (deduce/fact! gd-edge-qdcwrmp 2 3) "
            "  (deduce/fact! gd-edge-qdcwrmp 10 11) "
            "  (deduce/fact! gd-other-src-qdcwrmp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (delta 0) "
            "        (let (want (lambda (expected candidate shift) (= (ref candidate 'dst) (+ expected shift)))) "
            "          (deduce/query gd-reach-qdcwrmp "
            "            (lambda (row) (want target row delta))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwrmp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-row-middle-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap preserved-row wrappers with non-leading row and multiple literal args");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap preserved-row wrappers with non-leading row and multiple literal args (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap preserved-row wrappers with non-leading row and multiple literal args");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-literal-args (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args] gd-edge-qdcwla (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args] gd-reach-qdcwla (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args] gd-other-src-qdcwla (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args] gd-other-out-qdcwla (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args '(gd-reach-qdcwla ?x ?y) '(gd-edge-qdcwla ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args '(gd-reach-qdcwla ?x ?z) '(gd-edge-qdcwla ?x ?y) '(gd-reach-qdcwla ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args '(gd-other-out-qdcwla ?x ?y) '(gd-other-src-qdcwla ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args) "
            "  (deduce/fact! gd-edge-qdcwla 1 2) "
            "  (deduce/fact! gd-edge-qdcwla 2 3) "
            "  (deduce/fact! gd-edge-qdcwla 10 11) "
            "  (deduce/fact! gd-other-src-qdcwla 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want (lambda (candidate expected) (= (ref candidate 'dst) expected))) "
            "        (deduce/query gd-reach-qdcwla "
            "          (lambda (row) (want row target)) "
            "          1 "
            "          'semi-naive)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwla)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap captured wrappers with closed literal args in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap captured wrappers with closed literal args in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap captured wrappers with closed literal args in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain] gd-edge-qdcwlap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain] gd-reach-qdcwlap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain] gd-other-src-qdcwlap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain] gd-other-out-qdcwlap (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain '(gd-reach-qdcwlap ?x ?y) '(gd-edge-qdcwlap ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain '(gd-reach-qdcwlap ?x ?z) '(gd-edge-qdcwlap ?x ?y) '(gd-reach-qdcwlap ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain '(gd-other-out-qdcwlap ?x ?y) '(gd-other-src-qdcwlap ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain) "
            "  (deduce/fact! gd-edge-qdcwlap 1 2) "
            "  (deduce/fact! gd-edge-qdcwlap 2 3) "
            "  (deduce/fact! gd-edge-qdcwlap 10 11) "
            "  (deduce/fact! gd-other-src-qdcwlap 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want (lambda (candidate expected) (= (ref candidate 'dst) expected))) "
            "        (deduce/query gd-reach-qdcwlap "
            "          (lambda (row) (want row target)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwlap)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap captured wrappers with closed literal args in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap captured wrappers with closed literal args in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap captured wrappers with closed literal args in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-scalar-wrapper (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper] gd-edge-qdcsw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper] gd-reach-qdcsw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper] gd-other-src-qdcsw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper] gd-other-out-qdcsw (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-scalar-wrapper '(gd-reach-qdcsw ?x ?y) '(gd-edge-qdcsw ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-scalar-wrapper '(gd-reach-qdcsw ?x ?z) '(gd-edge-qdcsw ?x ?y) '(gd-reach-qdcsw ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-scalar-wrapper '(gd-other-out-qdcsw ?x ?y) '(gd-other-src-qdcsw ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-scalar-wrapper) "
            "  (deduce/fact! gd-edge-qdcsw 1 2) "
            "  (deduce/fact! gd-edge-qdcsw 2 3) "
            "  (deduce/fact! gd-edge-qdcsw 10 11) "
            "  (deduce/fact! gd-other-src-qdcsw 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (pickdst (lambda (candidate) (ref candidate 'dst))) "
            "        (let (want (lambda (expected actual) (= actual expected))) "
            "          (deduce/query gd-reach-qdcsw "
            "            (lambda (row) (want target (pickdst row))) "
            "            1 "
            "            'semi-naive))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcsw)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-scalar-wrapper)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can rewrite captured comparator wrappers over row-derived scalar args");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can rewrite captured comparator wrappers over row-derived scalar args (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can rewrite captured comparator wrappers over row-derived scalar args");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain] gd-edge-qdcswcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain] gd-reach-qdcswcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain] gd-other-src-qdcswcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain] gd-other-out-qdcswcp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain '(gd-reach-qdcswcp ?x ?y) '(gd-edge-qdcswcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain '(gd-reach-qdcswcp ?x ?z) '(gd-edge-qdcswcp ?x ?y) '(gd-reach-qdcswcp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain '(gd-other-out-qdcswcp ?x ?y) '(gd-other-src-qdcswcp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain) "
            "  (deduce/fact! gd-edge-qdcswcp 1 2) "
            "  (deduce/fact! gd-edge-qdcswcp 2 3) "
            "  (deduce/fact! gd-edge-qdcswcp 10 11) "
            "  (deduce/fact! gd-other-src-qdcswcp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (pickdst (lambda (candidate) (ref candidate 'dst))) "
            "        (let (want0 (lambda (actual expected) (= actual expected))) "
            "          (let (want1 (lambda (expected actual) (want0 actual expected))) "
            "            (deduce/query gd-reach-qdcswcp "
            "              (lambda (row) (want1 target (pickdst row))))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcswcp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-scalar-wrapper-chain-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can follow short captured comparator wrapper chains over row-derived scalar args");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can follow short captured comparator wrapper chains over row-derived scalar args (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can follow short captured comparator wrapper chains over row-derived scalar args");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-selector] gd-edge-qdds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-selector] gd-reach-qdds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-selector] gd-other-src-qdds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-selector] gd-other-out-qdds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-selector '(gd-reach-qdds ?x ?y) '(gd-edge-qdds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-selector '(gd-reach-qdds ?x ?z) '(gd-edge-qdds ?x ?y) '(gd-reach-qdds ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-selector '(gd-other-out-qdds ?x ?y) '(gd-other-src-qdds ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdds 1 2) "
            "  (deduce/fact! gd-edge-qdds 2 3) "
            "  (deduce/fact! gd-edge-qdds 10 11) "
            "  (deduce/fact! gd-other-src-qdds 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdds "
            "      (lambda (row) "
            "        (or "
            "          (= (ref row 'dst) 2) "
            "          (= (ref row 'dst) 3))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qdds)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 4) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 8) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can union disjunctive demand-safe branches in one ephemeral demand run");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can union disjunctive demand-safe branches in one ephemeral demand run (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can union disjunctive demand-safe branches in one ephemeral demand run");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-plain] gd-edge-qddp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-plain] gd-reach-qddp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-plain] gd-other-src-qddp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-plain] gd-other-out-qddp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-plain '(gd-reach-qddp ?x ?y) '(gd-edge-qddp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-plain '(gd-reach-qddp ?x ?z) '(gd-edge-qddp ?x ?y) '(gd-reach-qddp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-plain '(gd-other-out-qddp ?x ?y) '(gd-other-src-qddp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-plain) "
            "  (deduce/fact! gd-edge-qddp 1 2) "
            "  (deduce/fact! gd-edge-qddp 2 3) "
            "  (deduce/fact! gd-edge-qddp 10 11) "
            "  (deduce/fact! gd-other-src-qddp 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qddp "
            "      (lambda (row) "
            "        (or "
            "          (= (ref row 'dst) 3) "
            "          (= (ref row 'dst) 11))))) "
            "  (define reach-stats (deduce/stats gd-reach-qddp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 13) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 17) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can union disjunctive demand-safe branches in one ephemeral demand run");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can union disjunctive demand-safe branches in one ephemeral demand run (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can union disjunctive demand-safe branches in one ephemeral demand run");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-wrapper-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-selector] gd-edge-qddws (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-selector] gd-reach-qddws (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-selector] gd-other-src-qddws (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-selector] gd-other-out-qddws (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-wrapper-selector '(gd-reach-qddws ?x ?y) '(gd-edge-qddws ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-wrapper-selector '(gd-reach-qddws ?x ?z) '(gd-edge-qddws ?x ?y) '(gd-reach-qddws ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-wrapper-selector '(gd-other-out-qddws ?x ?y) '(gd-other-src-qddws ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-wrapper-selector) "
            "  (deduce/fact! gd-edge-qddws 1 2) "
            "  (deduce/fact! gd-edge-qddws 2 3) "
            "  (deduce/fact! gd-edge-qddws 10 11) "
            "  (deduce/fact! gd-other-src-qddws 9 10) "
            "  (define selected "
            "    (let (want2 (lambda (candidate) (= (ref candidate 'dst) 2)) "
            "          want3 (lambda (candidate) (= (ref candidate 'dst) 3))) "
            "      (deduce/query gd-reach-qddws "
            "        (lambda (row) (or (want2 row) (want3 row))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qddws)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-wrapper-selector)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 4) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can union same-position disjunctive branches with whole-filter wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can union same-position disjunctive branches with whole-filter wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can union same-position disjunctive branches with whole-filter wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-wrapper-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-plain] gd-edge-qddwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-plain] gd-reach-qddwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-plain] gd-other-src-qddwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-wrapper-plain] gd-other-out-qddwp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-wrapper-plain '(gd-reach-qddwp ?x ?y) '(gd-edge-qddwp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-wrapper-plain '(gd-reach-qddwp ?x ?z) '(gd-edge-qddwp ?x ?y) '(gd-reach-qddwp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-wrapper-plain '(gd-other-out-qddwp ?x ?y) '(gd-other-src-qddwp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-wrapper-plain) "
            "  (deduce/fact! gd-edge-qddwp 1 2) "
            "  (deduce/fact! gd-edge-qddwp 2 3) "
            "  (deduce/fact! gd-edge-qddwp 10 11) "
            "  (deduce/fact! gd-other-src-qddwp 9 10) "
            "  (define selected "
            "    (let (pickdst (lambda (candidate) (ref candidate 'dst)) "
            "          want2 (lambda (actual) (= actual 2)) "
            "          want3 (lambda (actual) (= actual 3))) "
            "      (deduce/query gd-reach-qddwp "
            "        (lambda (row) (or (want2 (pickdst row)) (want3 (pickdst row))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qddwp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-wrapper-plain)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 4) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can union same-position disjunctive branches with row-column wrappers");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can union same-position disjunctive branches with row-column wrappers (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can union same-position disjunctive branches with row-column wrappers");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-residual-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-selector] gd-edge-qddrs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-selector] gd-reach-qddrs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-selector] gd-other-src-qddrs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-selector] gd-other-out-qddrs (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-residual-selector '(gd-reach-qddrs ?x ?y) '(gd-edge-qddrs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-residual-selector '(gd-reach-qddrs ?x ?z) '(gd-edge-qddrs ?x ?y) '(gd-reach-qddrs ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-residual-selector '(gd-other-out-qddrs ?x ?y) '(gd-other-src-qddrs ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-residual-selector) "
            "  (deduce/fact! gd-edge-qddrs 1 2) "
            "  (deduce/fact! gd-edge-qddrs 2 3) "
            "  (deduce/fact! gd-edge-qddrs 10 11) "
            "  (deduce/fact! gd-other-src-qddrs 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qddrs "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'dst) 2) (> (ref row 'src) 0)) "
            "          (and (= (ref row 'dst) 3) (> (ref row 'src) 1)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qddrs)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-residual-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can union same-position disjunctive branches with residual conjunct filtering");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can union same-position disjunctive branches with residual conjunct filtering (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can union same-position disjunctive branches with residual conjunct filtering");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-residual-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-plain] gd-edge-qddrp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-plain] gd-reach-qddrp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-plain] gd-other-src-qddrp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-residual-plain] gd-other-out-qddrp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-residual-plain '(gd-reach-qddrp ?x ?y) '(gd-edge-qddrp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-residual-plain '(gd-reach-qddrp ?x ?z) '(gd-edge-qddrp ?x ?y) '(gd-reach-qddrp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-residual-plain '(gd-other-out-qddrp ?x ?y) '(gd-other-src-qddrp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-residual-plain) "
            "  (deduce/fact! gd-edge-qddrp 1 2) "
            "  (deduce/fact! gd-edge-qddrp 2 3) "
            "  (deduce/fact! gd-edge-qddrp 10 11) "
            "  (deduce/fact! gd-other-src-qddrp 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qddrp "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'dst) 3) (> (ref row 'src) 1)) "
            "          (and (= (ref row 'dst) 11) (> (ref row 'src) 5)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qddrp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-residual-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 12) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can union same-position disjunctive branches with residual conjunct filtering");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can union same-position disjunctive branches with residual conjunct filtering (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can union same-position disjunctive branches with residual conjunct filtering");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-mixed-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-selector] gd-edge-qddms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-selector] gd-reach-qddms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-selector] gd-other-src-qddms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-selector] gd-other-out-qddms (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-mixed-selector '(gd-reach-qddms ?x ?y) '(gd-edge-qddms ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-mixed-selector '(gd-reach-qddms ?x ?z) '(gd-edge-qddms ?x ?y) '(gd-reach-qddms ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-mixed-selector '(gd-other-out-qddms ?x ?y) '(gd-other-src-qddms ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-mixed-selector) "
            "  (deduce/fact! gd-edge-qddms 1 2) "
            "  (deduce/fact! gd-edge-qddms 2 3) "
            "  (deduce/fact! gd-edge-qddms 10 11) "
            "  (deduce/fact! gd-other-src-qddms 9 10) "
            "  (define selected "
            "    (let (src-target 1 "
            "          dst-target 11 "
            "          want-src (lambda (candidate expected) (= (ref candidate 'src) expected)) "
            "          pickdst (lambda (candidate) (ref candidate 'dst)) "
            "          want-dst (lambda (actual expected) (= actual expected))) "
            "      (deduce/query gd-reach-qddms "
            "        (lambda (row) "
            "          (or "
            "            (want-src row src-target) "
            "            (want-dst (pickdst row) dst-target))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qddms)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-mixed-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can union mixed-position demand-safe branches in separate ephemeral runs");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can union mixed-position demand-safe branches in separate ephemeral runs (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can union mixed-position demand-safe branches in separate ephemeral runs");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-mixed-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-plain] gd-edge-qddmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-plain] gd-reach-qddmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-plain] gd-other-src-qddmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-mixed-plain] gd-other-out-qddmp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-mixed-plain '(gd-reach-qddmp ?x ?y) '(gd-edge-qddmp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-mixed-plain '(gd-reach-qddmp ?x ?z) '(gd-edge-qddmp ?x ?y) '(gd-reach-qddmp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-mixed-plain '(gd-other-out-qddmp ?x ?y) '(gd-other-src-qddmp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-mixed-plain) "
            "  (deduce/fact! gd-edge-qddmp 1 2) "
            "  (deduce/fact! gd-edge-qddmp 2 3) "
            "  (deduce/fact! gd-edge-qddmp 10 11) "
            "  (deduce/fact! gd-other-src-qddmp 9 10) "
            "  (define selected "
            "    (let (src-target 1 "
            "          dst-target 11 "
            "          want-src (lambda (candidate expected) (= (ref candidate 'src) expected)) "
            "          pickdst (lambda (candidate) (ref candidate 'dst)) "
            "          want-dst (lambda (actual expected) (= actual expected))) "
            "      (deduce/query gd-reach-qddmp "
            "        (lambda (row) "
            "          (or "
            "            (want-src row src-target) "
            "            (want-dst (pickdst row) dst-target)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qddmp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-mixed-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can union mixed-position demand-safe branches in separate ephemeral runs");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can union mixed-position demand-safe branches in separate ephemeral runs (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can union mixed-position demand-safe branches in separate ephemeral runs");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-pair-selector-fallback (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-selector-fallback] gd-edge-qdpsf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-selector-fallback] gd-reach-qdpsf (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-selector-fallback '(gd-reach-qdpsf ?x ?y) '(gd-edge-qdpsf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-selector-fallback '(gd-reach-qdpsf ?x ?z) '(gd-edge-qdpsf ?x ?y) '(gd-reach-qdpsf ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-pair-selector-fallback) "
            "  (deduce/fact! gd-edge-qdpsf 1 2) "
            "  (deduce/fact! gd-edge-qdpsf 2 3) "
            "  (deduce/fact! gd-edge-qdpsf 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qdpsf "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 3))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qdpsf)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-pair-selector-fallback)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query keeps recursive multi-position symbolic demand filters on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps recursive multi-position symbolic demand filters on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps recursive multi-position symbolic demand filters on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-pair-plain-fallback (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-plain-fallback] gd-edge-qdppf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-plain-fallback] gd-reach-qdppf (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-plain-fallback '(gd-reach-qdppf ?x ?y) '(gd-edge-qdppf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-plain-fallback '(gd-reach-qdppf ?x ?z) '(gd-edge-qdppf ?x ?y) '(gd-reach-qdppf ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-pair-plain-fallback) "
            "  (deduce/fact! gd-edge-qdppf 1 2) "
            "  (deduce/fact! gd-edge-qdppf 2 3) "
            "  (deduce/fact! gd-edge-qdppf 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qdppf "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 3))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdppf)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-pair-plain-fallback)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps recursive multi-position symbolic demand filters on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps recursive multi-position symbolic demand filters on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps recursive multi-position symbolic demand filters on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback] gd-edge-qddpsf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback] gd-reach-qddpsf (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback '(gd-reach-qddpsf ?x ?y) '(gd-edge-qddpsf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback '(gd-reach-qddpsf ?x ?z) '(gd-edge-qddpsf ?x ?y) '(gd-reach-qddpsf ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback) "
            "  (deduce/fact! gd-edge-qddpsf 1 2) "
            "  (deduce/fact! gd-edge-qddpsf 2 3) "
            "  (deduce/fact! gd-edge-qddpsf 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qddpsf "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 3)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qddpsf)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-selector-fallback)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 14) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query keeps recursive multi-position disjunctive demand branches on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps recursive multi-position disjunctive demand branches on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps recursive multi-position disjunctive demand branches on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback] gd-edge-qddppf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback] gd-reach-qddppf (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback '(gd-reach-qddppf ?x ?y) '(gd-edge-qddppf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback '(gd-reach-qddppf ?x ?z) '(gd-edge-qddppf ?x ?y) '(gd-reach-qddppf ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback) "
            "  (deduce/fact! gd-edge-qddppf 1 2) "
            "  (deduce/fact! gd-edge-qddppf 2 3) "
            "  (deduce/fact! gd-edge-qddppf 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qddppf "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 3)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qddppf)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-plain-fallback)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 14) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps recursive multi-position disjunctive demand branches on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps recursive multi-position disjunctive demand branches on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps recursive multi-position disjunctive demand branches on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-pair-selector-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-selector-swapped] gd-edge-qdpss (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-selector-swapped] gd-reach-qdpss (dst) (src)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-selector-swapped '(gd-reach-qdpss ?y ?x) '(gd-edge-qdpss ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-selector-swapped '(gd-reach-qdpss ?z ?x) '(gd-edge-qdpss ?x ?y) '(gd-reach-qdpss ?z ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-pair-selector-swapped) "
            "  (deduce/fact! gd-edge-qdpss 1 2) "
            "  (deduce/fact! gd-edge-qdpss 2 3) "
            "  (define selected "
            "    (deduce/query gd-reach-qdpss "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 3))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qdpss)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-pair-selector-swapped)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query keeps swapped recursive pair demands on the preserved recursive position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps swapped recursive pair demands on the preserved recursive position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps swapped recursive pair demands on the preserved recursive position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-pair-plain-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-plain-swapped] gd-edge-qdpps (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-pair-plain-swapped] gd-reach-qdpps (dst) (src)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-plain-swapped '(gd-reach-qdpps ?y ?x) '(gd-edge-qdpps ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-pair-plain-swapped '(gd-reach-qdpps ?z ?x) '(gd-edge-qdpps ?x ?y) '(gd-reach-qdpps ?z ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-pair-plain-swapped) "
            "  (deduce/fact! gd-edge-qdpps 1 2) "
            "  (deduce/fact! gd-edge-qdpps 2 3) "
            "  (define selected "
            "    (deduce/query gd-reach-qdpps "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 3))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdpps)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-pair-plain-swapped)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps swapped recursive pair demands on the preserved recursive position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps swapped recursive pair demands on the preserved recursive position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps swapped recursive pair demands on the preserved recursive position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped] gd-edge-qddpss (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped] gd-reach-qddpss (dst) (src)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped '(gd-reach-qddpss ?y ?x) '(gd-edge-qddpss ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped '(gd-reach-qddpss ?z ?x) '(gd-edge-qddpss ?x ?y) '(gd-reach-qddpss ?z ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped) "
            "  (deduce/fact! gd-edge-qddpss 1 2) "
            "  (deduce/fact! gd-edge-qddpss 2 3) "
            "  (deduce/fact! gd-edge-qddpss 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qddpss "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 3)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qddpss)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-selector-swapped)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 14) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query keeps swapped recursive disjunctive demands on the preserved recursive position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps swapped recursive disjunctive demands on the preserved recursive position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps swapped recursive disjunctive demands on the preserved recursive position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped] gd-edge-qddpps (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped] gd-reach-qddpps (dst) (src)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped '(gd-reach-qddpps ?y ?x) '(gd-edge-qddpps ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped '(gd-reach-qddpps ?z ?x) '(gd-edge-qddpps ?x ?y) '(gd-reach-qddpps ?z ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped) "
            "  (deduce/fact! gd-edge-qddpps 1 2) "
            "  (deduce/fact! gd-edge-qddpps 2 3) "
            "  (deduce/fact! gd-edge-qddpps 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qddpps "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 3)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qddpps)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-disjunction-pairs-plain-swapped)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 14) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps swapped recursive disjunctive demands on the preserved recursive position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps swapped recursive disjunctive demands on the preserved recursive position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps swapped recursive disjunctive demands on the preserved recursive position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-sym-pair-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-pair-selector] gd-edge-qdsps (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-pair-selector] gd-sym-qdsps (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-pair-selector '(gd-sym-qdsps ?x ?y) '(gd-edge-qdsps ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-pair-selector '(gd-sym-qdsps ?x ?y) '(gd-sym-qdsps ?y ?x)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-sym-pair-selector) "
            "  (deduce/fact! gd-edge-qdsps 1 2) "
            "  (deduce/fact! gd-edge-qdsps 10 11) "
            "  (define selected "
            "    (deduce/query gd-sym-qdsps "
            "      (lambda (row) (and (= (ref row 'src) 2) (= (ref row 'dst) 1))) "
            "      1 "
            "      'semi-naive)) "
            "  (define sym-stats (deduce/stats gd-sym-qdsps)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-sym-pair-selector)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 2) "
            "    (= (ref (car selected) 'dst) 1) "
            "    (= (ref sym-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref sym-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query falls back on jointly-supported recursive pair permutation demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query falls back on jointly-supported recursive pair permutation demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query falls back on jointly-supported recursive pair permutation demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-stable-pair-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-pair-selector] gd-edge-qdstps (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-pair-selector] gd-stable-qdstps (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-pair-selector '(gd-stable-qdstps ?x ?y) '(gd-edge-qdstps ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-pair-selector '(gd-stable-qdstps ?x ?y) '(gd-stable-qdstps ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-stable-pair-selector) "
            "  (deduce/fact! gd-edge-qdstps 1 2) "
            "  (deduce/fact! gd-edge-qdstps 10 11) "
            "  (define selected "
            "    (deduce/query gd-stable-qdstps "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))) "
            "      1 "
            "      'semi-naive)) "
            "  (define stable-stats (deduce/stats gd-stable-qdstps)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-stable-pair-selector)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref stable-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref stable-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref stable-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref stable-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query preserves jointly-supported same-index recursive pair demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query preserves jointly-supported same-index recursive pair demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query preserves jointly-supported same-index recursive pair demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-stable-pair-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-pair-plain] gd-edge-qdstpp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-pair-plain] gd-stable-qdstpp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-pair-plain '(gd-stable-qdstpp ?x ?y) '(gd-edge-qdstpp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-pair-plain '(gd-stable-qdstpp ?x ?y) '(gd-stable-qdstpp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-stable-pair-plain) "
            "  (deduce/fact! gd-edge-qdstpp 1 2) "
            "  (deduce/fact! gd-edge-qdstpp 10 11) "
            "  (define selected "
            "    (deduce/query gd-stable-qdstpp "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))))) "
            "  (define stable-stats (deduce/stats gd-stable-qdstpp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-stable-pair-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref stable-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref stable-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref stable-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref stable-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query preserves jointly-supported same-index recursive pair demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query preserves jointly-supported same-index recursive pair demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query preserves jointly-supported same-index recursive pair demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-stable-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-disjunction-selector] gd-edge-qdstds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-disjunction-selector] gd-stable-qdstds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-disjunction-selector '(gd-stable-qdstds ?x ?y) '(gd-edge-qdstds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-disjunction-selector '(gd-stable-qdstds ?x ?y) '(gd-stable-qdstds ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-stable-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdstds 1 2) "
            "  (deduce/fact! gd-edge-qdstds 10 11) "
            "  (define selected "
            "    (deduce/query gd-stable-qdstds "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define stable-stats (deduce/stats gd-stable-qdstds)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-stable-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref stable-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref stable-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref stable-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref stable-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query preserves jointly-supported same-index recursive disjunctive demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query preserves jointly-supported same-index recursive disjunctive demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query preserves jointly-supported same-index recursive disjunctive demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-stable-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-disjunction-plain] gd-edge-qdstdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-stable-disjunction-plain] gd-stable-qdstdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-disjunction-plain '(gd-stable-qdstdp ?x ?y) '(gd-edge-qdstdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-stable-disjunction-plain '(gd-stable-qdstdp ?x ?y) '(gd-stable-qdstdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-stable-disjunction-plain) "
            "  (deduce/fact! gd-edge-qdstdp 1 2) "
            "  (deduce/fact! gd-edge-qdstdp 10 11) "
            "  (define selected "
            "    (deduce/query gd-stable-qdstdp "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define stable-stats (deduce/stats gd-stable-qdstdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-stable-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref stable-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref stable-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref stable-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref stable-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref stable-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref stable-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query preserves jointly-supported same-index recursive disjunctive demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query preserves jointly-supported same-index recursive disjunctive demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query preserves jointly-supported same-index recursive disjunctive demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-sym-pair-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-pair-plain] gd-edge-qdspp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-pair-plain] gd-sym-qdspp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-pair-plain '(gd-sym-qdspp ?x ?y) '(gd-edge-qdspp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-pair-plain '(gd-sym-qdspp ?x ?y) '(gd-sym-qdspp ?y ?x)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-sym-pair-plain) "
            "  (deduce/fact! gd-edge-qdspp 1 2) "
            "  (deduce/fact! gd-edge-qdspp 10 11) "
            "  (define selected "
            "    (deduce/query gd-sym-qdspp "
            "      (lambda (row) (and (= (ref row 'src) 2) (= (ref row 'dst) 1))))) "
            "  (define sym-stats (deduce/stats gd-sym-qdspp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-sym-pair-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 2) "
            "    (= (ref (car selected) 'dst) 1) "
            "    (= (ref sym-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref sym-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query falls back on jointly-supported recursive pair permutation demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query falls back on jointly-supported recursive pair permutation demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query falls back on jointly-supported recursive pair permutation demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-sym-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-disjunction-selector] gd-edge-qdsds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-disjunction-selector] gd-sym-qdsds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-disjunction-selector '(gd-sym-qdsds ?x ?y) '(gd-edge-qdsds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-disjunction-selector '(gd-sym-qdsds ?x ?y) '(gd-sym-qdsds ?y ?x)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-sym-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdsds 1 2) "
            "  (deduce/fact! gd-edge-qdsds 10 11) "
            "  (define selected "
            "    (deduce/query gd-sym-qdsds "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 2) (= (ref row 'dst) 1)) "
            "          (and (= (ref row 'src) 11) (= (ref row 'dst) 10)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define sym-stats (deduce/stats gd-sym-qdsds)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-sym-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 13) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 11) "
            "    (= (ref sym-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref sym-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query falls back on jointly-supported recursive disjunctive permutation demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query falls back on jointly-supported recursive disjunctive permutation demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query falls back on jointly-supported recursive disjunctive permutation demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-sym-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-disjunction-plain] gd-edge-qdsdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-sym-disjunction-plain] gd-sym-qdsdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-disjunction-plain '(gd-sym-qdsdp ?x ?y) '(gd-edge-qdsdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-sym-disjunction-plain '(gd-sym-qdsdp ?x ?y) '(gd-sym-qdsdp ?y ?x)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-sym-disjunction-plain) "
            "  (deduce/fact! gd-edge-qdsdp 1 2) "
            "  (deduce/fact! gd-edge-qdsdp 10 11) "
            "  (define selected "
            "    (deduce/query gd-sym-qdsdp "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 2) (= (ref row 'dst) 1)) "
            "          (and (= (ref row 'src) 11) (= (ref row 'dst) 10)))))) "
            "  (define sym-stats (deduce/stats gd-sym-qdsdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-sym-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 13) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 11) "
            "    (= (ref sym-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref sym-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref sym-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query falls back on jointly-supported recursive disjunctive permutation demands");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query falls back on jointly-supported recursive disjunctive permutation demands (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query falls back on jointly-supported recursive disjunctive permutation demands");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-multi-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-multi-selector] gd-edge-qdms (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-multi-selector] gd-path-qdms (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-multi-selector '(gd-path-qdms ?x ?y) '(gd-edge-qdms ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-multi-selector '(gd-path-qdms ?x ?z) '(gd-path-qdms ?x ?y) '(gd-path-qdms ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-multi-selector) "
            "  (deduce/fact! gd-edge-qdms 1 2) "
            "  (deduce/fact! gd-edge-qdms 2 3) "
            "  (deduce/fact! gd-edge-qdms 3 4) "
            "  (define selected "
            "    (deduce/query gd-path-qdms "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 4))) "
            "      1 "
            "      'semi-naive)) "
            "  (define path-stats (deduce/stats gd-path-qdms)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-multi-selector)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 4) "
            "    (= (ref path-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref path-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref path-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref path-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query falls back when demanded positions require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query falls back when demanded positions require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query falls back when demanded positions require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-multi-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-multi-plain] gd-edge-qdmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-multi-plain] gd-path-qdmp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-multi-plain '(gd-path-qdmp ?x ?y) '(gd-edge-qdmp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-multi-plain '(gd-path-qdmp ?x ?z) '(gd-path-qdmp ?x ?y) '(gd-path-qdmp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-multi-plain) "
            "  (deduce/fact! gd-edge-qdmp 1 2) "
            "  (deduce/fact! gd-edge-qdmp 2 3) "
            "  (deduce/fact! gd-edge-qdmp 3 4) "
            "  (define selected "
            "    (deduce/query gd-path-qdmp "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 4))))) "
            "  (define path-stats (deduce/stats gd-path-qdmp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-multi-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 4) "
            "    (= (ref path-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref path-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref path-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref path-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query falls back when demanded positions require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query falls back when demanded positions require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query falls back when demanded positions require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-selector] gd-edge-qdmscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-selector] gd-a-qdmscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-selector] gd-b-qdmscc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-selector '(gd-a-qdmscc ?x ?y) '(gd-edge-qdmscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-selector '(gd-a-qdmscc ?x ?y) '(gd-b-qdmscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-selector '(gd-b-qdmscc ?x ?y) '(gd-a-qdmscc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-selector) "
            "  (deduce/fact! gd-edge-qdmscc 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdmscc "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdmscc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-selector)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query keeps same-index mutual-recursive pair demands on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps same-index mutual-recursive pair demands on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps same-index mutual-recursive pair demands on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-plain] gd-edge-qdmpcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-plain] gd-a-qdmpcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-plain] gd-b-qdmpcc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-plain '(gd-a-qdmpcc ?x ?y) '(gd-edge-qdmpcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-plain '(gd-a-qdmpcc ?x ?y) '(gd-b-qdmpcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-plain '(gd-b-qdmpcc ?x ?y) '(gd-a-qdmpcc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-plain) "
            "  (deduce/fact! gd-edge-qdmpcc 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdmpcc "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))))) "
            "  (define rel-stats (deduce/stats gd-a-qdmpcc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps same-index mutual-recursive pair demands on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps same-index mutual-recursive pair demands on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps same-index mutual-recursive pair demands on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-disjunction-selector] gd-edge-qdmdsscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-disjunction-selector] gd-a-qdmdsscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-disjunction-selector] gd-b-qdmdsscc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-disjunction-selector '(gd-a-qdmdsscc ?x ?y) '(gd-edge-qdmdsscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-disjunction-selector '(gd-a-qdmdsscc ?x ?y) '(gd-b-qdmdsscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-disjunction-selector '(gd-b-qdmdsscc ?x ?y) '(gd-a-qdmdsscc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdmdsscc 1 2) "
            "  (deduce/fact! gd-edge-qdmdsscc 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdmdsscc "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdmdsscc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query keeps same-index mutual-recursive disjunctive demands on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps same-index mutual-recursive disjunctive demands on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps same-index mutual-recursive disjunctive demands on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-disjunction-plain] gd-edge-qdmdspcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-disjunction-plain] gd-a-qdmdspcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-disjunction-plain] gd-b-qdmdspcc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-disjunction-plain '(gd-a-qdmdspcc ?x ?y) '(gd-edge-qdmdspcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-disjunction-plain '(gd-a-qdmdspcc ?x ?y) '(gd-b-qdmdspcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-disjunction-plain '(gd-b-qdmdspcc ?x ?y) '(gd-a-qdmdspcc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-disjunction-plain) "
            "  (deduce/fact! gd-edge-qdmdspcc 1 2) "
            "  (deduce/fact! gd-edge-qdmdspcc 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdmdspcc "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define rel-stats (deduce/stats gd-a-qdmdspcc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps same-index mutual-recursive disjunctive demands on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps same-index mutual-recursive disjunctive demands on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps same-index mutual-recursive disjunctive demands on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-three-cycle-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-selector] gd-edge-qdtcscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-selector] gd-a-qdtcscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-selector] gd-b-qdtcscc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-selector] gd-c-qdtcscc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-selector '(gd-a-qdtcscc ?x ?y) '(gd-edge-qdtcscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-selector '(gd-a-qdtcscc ?x ?y) '(gd-b-qdtcscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-selector '(gd-b-qdtcscc ?x ?y) '(gd-c-qdtcscc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-selector '(gd-c-qdtcscc ?x ?y) '(gd-a-qdtcscc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-three-cycle-selector) "
            "  (deduce/fact! gd-edge-qdtcscc 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdtcscc "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdtcscc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-three-cycle-selector)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query keeps same-index three-cycle recursive pair demands on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query keeps same-index three-cycle recursive pair demands on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query keeps same-index three-cycle recursive pair demands on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-three-cycle-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-plain] gd-edge-qdtcpcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-plain] gd-a-qdtcpcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-plain] gd-b-qdtcpcc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-three-cycle-plain] gd-c-qdtcpcc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-plain '(gd-a-qdtcpcc ?x ?y) '(gd-edge-qdtcpcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-plain '(gd-a-qdtcpcc ?x ?y) '(gd-b-qdtcpcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-plain '(gd-b-qdtcpcc ?x ?y) '(gd-c-qdtcpcc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-three-cycle-plain '(gd-c-qdtcpcc ?x ?y) '(gd-a-qdtcpcc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-three-cycle-plain) "
            "  (deduce/fact! gd-edge-qdtcpcc 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdtcpcc "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))))) "
            "  (define rel-stats (deduce/stats gd-a-qdtcpcc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-three-cycle-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query keeps same-index three-cycle recursive pair demands on bounded ephemeral demand execution");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query keeps same-index three-cycle recursive pair demands on bounded ephemeral demand execution (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query keeps same-index three-cycle recursive pair demands on bounded ephemeral demand execution");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-reordered-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-selector] gd-edge-qdmrs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-selector] gd-a-qdmrs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-selector] gd-b-qdmrs (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-selector '(gd-a-qdmrs ?x ?y) '(gd-edge-qdmrs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-selector '(gd-a-qdmrs ?x ?y) '(gd-b-qdmrs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-selector '(gd-b-qdmrs ?y ?x) '(gd-a-qdmrs ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-selector) "
            "  (deduce/fact! gd-edge-qdmrs 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdmrs "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdmrs)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-selector)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query relaxes transformed mutual-recursive pair demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query relaxes transformed mutual-recursive pair demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query relaxes transformed mutual-recursive pair demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-reordered-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-plain] gd-edge-qdmrp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-plain] gd-a-qdmrp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-plain] gd-b-qdmrp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-plain '(gd-a-qdmrp ?x ?y) '(gd-edge-qdmrp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-plain '(gd-a-qdmrp ?x ?y) '(gd-b-qdmrp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-plain '(gd-b-qdmrp ?y ?x) '(gd-a-qdmrp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-plain) "
            "  (deduce/fact! gd-edge-qdmrp 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdmrp "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))))) "
            "  (define rel-stats (deduce/stats gd-a-qdmrp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query relaxes transformed mutual-recursive pair demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query relaxes transformed mutual-recursive pair demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query relaxes transformed mutual-recursive pair demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector] gd-edge-qdmrds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector] gd-a-qdmrds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector] gd-b-qdmrds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector '(gd-a-qdmrds ?x ?y) '(gd-edge-qdmrds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector '(gd-a-qdmrds ?x ?y) '(gd-b-qdmrds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector '(gd-b-qdmrds ?y ?x) '(gd-a-qdmrds ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdmrds 1 2) "
            "  (deduce/fact! gd-edge-qdmrds 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdmrds "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdmrds)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query relaxes transformed mutual-recursive disjunctive demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query relaxes transformed mutual-recursive disjunctive demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query relaxes transformed mutual-recursive disjunctive demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain] gd-edge-qdmrdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain] gd-a-qdmrdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain] gd-b-qdmrdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain '(gd-a-qdmrdp ?x ?y) '(gd-edge-qdmrdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain '(gd-a-qdmrdp ?x ?y) '(gd-b-qdmrdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain '(gd-b-qdmrdp ?y ?x) '(gd-a-qdmrdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain) "
            "  (deduce/fact! gd-edge-qdmrdp 1 2) "
            "  (deduce/fact! gd-edge-qdmrdp 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdmrdp "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define rel-stats (deduce/stats gd-a-qdmrdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-mutual-reordered-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query relaxes transformed mutual-recursive disjunctive demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query relaxes transformed mutual-recursive disjunctive demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query relaxes transformed mutual-recursive disjunctive demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-three-cycle-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-selector] gd-edge-qdttcs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-selector] gd-a-qdttcs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-selector] gd-b-qdttcs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-selector] gd-c-qdttcs (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-selector '(gd-a-qdttcs ?x ?y) '(gd-edge-qdttcs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-selector '(gd-a-qdttcs ?x ?y) '(gd-b-qdttcs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-selector '(gd-b-qdttcs ?y ?x) '(gd-c-qdttcs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-selector '(gd-c-qdttcs ?x ?y) '(gd-a-qdttcs ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-selector) "
            "  (deduce/fact! gd-edge-qdttcs 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdttcs "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdttcs)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-selector)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query relaxes transformed three-cycle recursive pair demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query relaxes transformed three-cycle recursive pair demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query relaxes transformed three-cycle recursive pair demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-three-cycle-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-plain] gd-edge-qdttcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-plain] gd-a-qdttcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-plain] gd-b-qdttcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-plain] gd-c-qdttcp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-plain '(gd-a-qdttcp ?x ?y) '(gd-edge-qdttcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-plain '(gd-a-qdttcp ?x ?y) '(gd-b-qdttcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-plain '(gd-b-qdttcp ?y ?x) '(gd-c-qdttcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-plain '(gd-c-qdttcp ?x ?y) '(gd-a-qdttcp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-plain) "
            "  (deduce/fact! gd-edge-qdttcp 1 2) "
            "  (define selected "
            "    (deduce/query gd-a-qdttcp "
            "      (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) 2))))) "
            "  (define rel-stats (deduce/stats gd-a-qdttcp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query relaxes transformed three-cycle recursive pair demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query relaxes transformed three-cycle recursive pair demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query relaxes transformed three-cycle recursive pair demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector] gd-edge-qdttcds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector] gd-a-qdttcds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector] gd-b-qdttcds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector] gd-c-qdttcds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector '(gd-a-qdttcds ?x ?y) '(gd-edge-qdttcds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector '(gd-a-qdttcds ?x ?y) '(gd-b-qdttcds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector '(gd-b-qdttcds ?y ?x) '(gd-c-qdttcds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector '(gd-c-qdttcds ?x ?y) '(gd-a-qdttcds ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdttcds 1 2) "
            "  (deduce/fact! gd-edge-qdttcds 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdttcds "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdttcds)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query relaxes transformed three-cycle recursive disjunctive demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query relaxes transformed three-cycle recursive disjunctive demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query relaxes transformed three-cycle recursive disjunctive demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain] gd-edge-qdttcdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain] gd-a-qdttcdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain] gd-b-qdttcdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain] gd-c-qdttcdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain '(gd-a-qdttcdp ?x ?y) '(gd-edge-qdttcdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain '(gd-a-qdttcdp ?x ?y) '(gd-b-qdttcdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain '(gd-b-qdttcdp ?y ?x) '(gd-c-qdttcdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain '(gd-c-qdttcdp ?x ?y) '(gd-a-qdttcdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain) "
            "  (deduce/fact! gd-edge-qdttcdp 1 2) "
            "  (deduce/fact! gd-edge-qdttcdp 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdttcdp "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 1) (= (ref row 'dst) 2)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define rel-stats (deduce/stats gd-a-qdttcdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-three-cycle-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 11) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 13) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref rel-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref rel-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query relaxes transformed three-cycle recursive disjunctive demands to one carried applied position");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query relaxes transformed three-cycle recursive disjunctive demands to one carried applied position (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query relaxes transformed three-cycle recursive disjunctive demands to one carried applied position");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-multiatom-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-selector] gd-edge-qdtmas (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-selector] gd-a-qdtmas (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-selector] gd-b-qdtmas (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-selector '(gd-a-qdtmas ?x ?y) '(gd-edge-qdtmas ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-selector '(gd-a-qdtmas ?x ?z) '(gd-b-qdtmas ?x ?y) '(gd-b-qdtmas ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-selector '(gd-b-qdtmas ?y ?x) '(gd-a-qdtmas ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-selector) "
            "  (deduce/fact! gd-edge-qdtmas 1 2) "
            "  (deduce/fact! gd-edge-qdtmas 2 3) "
            "  (define selected "
            "    (deduce/query gd-a-qdtmas "
            "      (lambda (row) (and (= (ref row 'src) 3) (= (ref row 'dst) 1))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdtmas)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-selector)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 3) "
            "    (= (ref (car selected) 'dst) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query falls back on transformed recursive demands that require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query falls back on transformed recursive demands that require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query falls back on transformed recursive demands that require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-multiatom-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-plain] gd-edge-qdtmap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-plain] gd-a-qdtmap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-plain] gd-b-qdtmap (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-plain '(gd-a-qdtmap ?x ?y) '(gd-edge-qdtmap ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-plain '(gd-a-qdtmap ?x ?z) '(gd-b-qdtmap ?x ?y) '(gd-b-qdtmap ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-plain '(gd-b-qdtmap ?y ?x) '(gd-a-qdtmap ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-plain) "
            "  (deduce/fact! gd-edge-qdtmap 1 2) "
            "  (deduce/fact! gd-edge-qdtmap 2 3) "
            "  (define selected "
            "    (deduce/query gd-a-qdtmap "
            "      (lambda (row) (and (= (ref row 'src) 3) (= (ref row 'dst) 1))))) "
            "  (define rel-stats (deduce/stats gd-a-qdtmap)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 3) "
            "    (= (ref (car selected) 'dst) 1) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query falls back on transformed recursive demands that require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query falls back on transformed recursive demands that require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query falls back on transformed recursive demands that require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector] gd-edge-qdtmds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector] gd-a-qdtmds (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector] gd-b-qdtmds (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector '(gd-a-qdtmds ?x ?y) '(gd-edge-qdtmds ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector '(gd-a-qdtmds ?x ?z) '(gd-b-qdtmds ?x ?y) '(gd-b-qdtmds ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector '(gd-b-qdtmds ?y ?x) '(gd-a-qdtmds ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector) "
            "  (deduce/fact! gd-edge-qdtmds 1 2) "
            "  (deduce/fact! gd-edge-qdtmds 2 3) "
            "  (deduce/fact! gd-edge-qdtmds 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdtmds "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 3) (= (ref row 'dst) 1)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))) "
            "      1 "
            "      'semi-naive)) "
            "  (define rel-stats (deduce/stats gd-a-qdtmds)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-disjunction-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 13) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 12) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query falls back on transformed recursive disjunctive demands that require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query falls back on transformed recursive disjunctive demands that require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query falls back on transformed recursive disjunctive demands that require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain] gd-edge-qdtmdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain] gd-a-qdtmdp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain] gd-b-qdtmdp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain '(gd-a-qdtmdp ?x ?y) '(gd-edge-qdtmdp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain '(gd-a-qdtmdp ?x ?z) '(gd-b-qdtmdp ?x ?y) '(gd-b-qdtmdp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain '(gd-b-qdtmdp ?y ?x) '(gd-a-qdtmdp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain) "
            "  (deduce/fact! gd-edge-qdtmdp 1 2) "
            "  (deduce/fact! gd-edge-qdtmdp 2 3) "
            "  (deduce/fact! gd-edge-qdtmdp 10 11) "
            "  (define selected "
            "    (deduce/query gd-a-qdtmdp "
            "      (lambda (row) "
            "        (or "
            "          (and (= (ref row 'src) 3) (= (ref row 'dst) 1)) "
            "          (and (= (ref row 'src) 10) (= (ref row 'dst) 11)))))) "
            "  (define rel-stats (deduce/stats gd-a-qdtmdp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-transform-multiatom-disjunction-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 13) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'dst))) 0 selected) 12) "
            "    (= (ref rel-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref rel-stats 'last-goal-directed-read-requested-bound-count) 0) "
            "    (= (ref rel-stats 'last-goal-directed-read-applied-bound-count) 0) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query falls back on transformed recursive disjunctive demands that require multiple recursive atoms");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query falls back on transformed recursive disjunctive demands that require multiple recursive atoms (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query falls back on transformed recursive disjunctive demands that require multiple recursive atoms");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second] gd-edge-qdcwlars (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second] gd-reach-qdcwlars (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second] gd-other-src-qdcwlars (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second] gd-other-out-qdcwlars (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second '(gd-reach-qdcwlars ?x ?y) '(gd-edge-qdcwlars ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second '(gd-reach-qdcwlars ?x ?z) '(gd-edge-qdcwlars ?x ?y) '(gd-reach-qdcwlars ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second '(gd-other-out-qdcwlars ?x ?y) '(gd-other-src-qdcwlars ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second) "
            "  (deduce/fact! gd-edge-qdcwlars 1 2) "
            "  (deduce/fact! gd-edge-qdcwlars 2 3) "
            "  (deduce/fact! gd-edge-qdcwlars 10 11) "
            "  (deduce/fact! gd-other-src-qdcwlars 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want (lambda (expected candidate) (= (ref candidate 'dst) expected))) "
            "        (deduce/query gd-reach-qdcwlars "
            "          (lambda (row) (want target row)) "
            "          1 "
            "          'semi-naive)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwlars)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap preserved-row literal-arg wrappers when row is not the first arg");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap preserved-row literal-arg wrappers when row is not the first arg (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap preserved-row literal-arg wrappers when row is not the first arg");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain] gd-edge-qdcwlarsp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain] gd-reach-qdcwlarsp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain] gd-other-src-qdcwlarsp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain] gd-other-out-qdcwlarsp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain '(gd-reach-qdcwlarsp ?x ?y) '(gd-edge-qdcwlarsp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain '(gd-reach-qdcwlarsp ?x ?z) '(gd-edge-qdcwlarsp ?x ?y) '(gd-reach-qdcwlarsp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain '(gd-other-out-qdcwlarsp ?x ?y) '(gd-other-src-qdcwlarsp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain) "
            "  (deduce/fact! gd-edge-qdcwlarsp 1 2) "
            "  (deduce/fact! gd-edge-qdcwlarsp 2 3) "
            "  (deduce/fact! gd-edge-qdcwlarsp 10 11) "
            "  (deduce/fact! gd-other-src-qdcwlarsp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want (lambda (expected candidate) (= (ref candidate 'dst) expected))) "
            "        (deduce/query gd-reach-qdcwlarsp "
            "          (lambda (row) (want target row)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwlarsp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-args-row-second-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap preserved-row literal-arg wrappers when row is not the first arg");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap preserved-row literal-arg wrappers when row is not the first arg (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap preserved-row literal-arg wrappers when row is not the first arg");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain] gd-edge-qdcwlac (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain] gd-reach-qdcwlac (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain] gd-other-src-qdcwlac (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain] gd-other-out-qdcwlac (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain '(gd-reach-qdcwlac ?x ?y) '(gd-edge-qdcwlac ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain '(gd-reach-qdcwlac ?x ?z) '(gd-edge-qdcwlac ?x ?y) '(gd-reach-qdcwlac ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain '(gd-other-out-qdcwlac ?x ?y) '(gd-other-src-qdcwlac ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain) "
            "  (deduce/fact! gd-edge-qdcwlac 1 2) "
            "  (deduce/fact! gd-edge-qdcwlac 2 3) "
            "  (deduce/fact! gd-edge-qdcwlac 10 11) "
            "  (deduce/fact! gd-other-src-qdcwlac 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want0 (lambda (candidate expected) (= (ref candidate 'dst) expected))) "
            "        (let (want1 (lambda (candidate expected) (want0 candidate expected))) "
            "          (deduce/query gd-reach-qdcwlac "
            "            (lambda (row) (want1 row target)) "
            "            1 "
            "            'semi-naive))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwlac)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap preserved-row literal-arg wrapper chains in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap preserved-row literal-arg wrapper chains in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap preserved-row literal-arg wrapper chains in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain] gd-edge-qdcwlacp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain] gd-reach-qdcwlacp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain] gd-other-src-qdcwlacp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain] gd-other-out-qdcwlacp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain '(gd-reach-qdcwlacp ?x ?y) '(gd-edge-qdcwlacp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain '(gd-reach-qdcwlacp ?x ?z) '(gd-edge-qdcwlacp ?x ?y) '(gd-reach-qdcwlacp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain '(gd-other-out-qdcwlacp ?x ?y) '(gd-other-src-qdcwlacp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain) "
            "  (deduce/fact! gd-edge-qdcwlacp 1 2) "
            "  (deduce/fact! gd-edge-qdcwlacp 2 3) "
            "  (deduce/fact! gd-edge-qdcwlacp 10 11) "
            "  (deduce/fact! gd-other-src-qdcwlacp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want0 (lambda (candidate expected) (= (ref candidate 'dst) expected))) "
            "        (let (want1 (lambda (candidate expected) (want0 candidate expected))) "
            "          (deduce/query gd-reach-qdcwlacp "
            "            (lambda (row) (want1 row target))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwlacp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-literal-arg-chain-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap preserved-row literal-arg wrapper chains in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap preserved-row literal-arg wrapper chains in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap preserved-row literal-arg wrapper chains in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-chain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain] gd-edge-qdcwc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain] gd-reach-qdcwc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain] gd-other-src-qdcwc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain] gd-other-out-qdcwc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-chain '(gd-reach-qdcwc ?x ?y) '(gd-edge-qdcwc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-chain '(gd-reach-qdcwc ?x ?z) '(gd-edge-qdcwc ?x ?y) '(gd-reach-qdcwc ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-chain '(gd-other-out-qdcwc ?x ?y) '(gd-other-src-qdcwc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-chain) "
            "  (deduce/fact! gd-edge-qdcwc 1 2) "
            "  (deduce/fact! gd-edge-qdcwc 2 3) "
            "  (deduce/fact! gd-edge-qdcwc 10 11) "
            "  (deduce/fact! gd-other-src-qdcwc 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want0 (lambda (candidate) (= (ref candidate 'dst) target))) "
            "        (let (want1 (lambda (candidate) (want0 candidate))) "
            "          (deduce/query gd-reach-qdcwc "
            "            (lambda (row) (want1 row)) "
            "            1 "
            "            'semi-naive))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-chain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap one-arg captured closure wrapper chains in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap one-arg captured closure wrapper chains in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap one-arg captured closure wrapper chains in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-captured-wrapper-chain-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain-plain] gd-edge-qdcwcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain-plain] gd-reach-qdcwcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain-plain] gd-other-src-qdcwcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-captured-wrapper-chain-plain] gd-other-out-qdcwcp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-chain-plain '(gd-reach-qdcwcp ?x ?y) '(gd-edge-qdcwcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-chain-plain '(gd-reach-qdcwcp ?x ?z) '(gd-edge-qdcwcp ?x ?y) '(gd-reach-qdcwcp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-captured-wrapper-chain-plain '(gd-other-out-qdcwcp ?x ?y) '(gd-other-src-qdcwcp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-chain-plain) "
            "  (deduce/fact! gd-edge-qdcwcp 1 2) "
            "  (deduce/fact! gd-edge-qdcwcp 2 3) "
            "  (deduce/fact! gd-edge-qdcwcp 10 11) "
            "  (deduce/fact! gd-other-src-qdcwcp 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (want0 (lambda (candidate) (= (ref candidate 'dst) target))) "
            "        (let (want1 (lambda (candidate) (want0 candidate))) "
            "          (deduce/query gd-reach-qdcwcp "
            "            (lambda (row) (want1 row))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcwcp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-captured-wrapper-chain-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl (lambda (acc row) (+ acc (ref row 'src))) 0 selected) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap one-arg captured closure wrapper chains in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap one-arg captured closure wrapper chains in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap one-arg captured closure wrapper chains in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-residual (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-residual] gd-edge-qdr (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-residual] gd-reach-qdr (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-residual] gd-other-src-qdr (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-residual] gd-other-out-qdr (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-residual '(gd-reach-qdr ?x ?y) '(gd-edge-qdr ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-residual '(gd-reach-qdr ?x ?z) '(gd-edge-qdr ?x ?y) '(gd-reach-qdr ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-residual '(gd-other-out-qdr ?x ?y) '(gd-other-src-qdr ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-residual) "
            "  (deduce/fact! gd-edge-qdr 1 2) "
            "  (deduce/fact! gd-edge-qdr 2 3) "
            "  (deduce/fact! gd-edge-qdr 10 11) "
            "  (deduce/fact! gd-other-src-qdr 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdr "
            "      (lambda (row) "
            "        (and (= (ref row 'dst) 3) (< (ref row 'src) 2))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qdr)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-residual)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdr) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use equality demand extraction with residual unsupported conjuncts");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use equality demand extraction with residual unsupported conjuncts (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use equality demand extraction with residual unsupported conjuncts");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-projected (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-projected] gd-edge-qdpr (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-projected] gd-reach-qdpr (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-projected '(gd-reach-qdpr ?x ?y) '(gd-edge-qdpr ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-projected '(gd-reach-qdpr ?x ?z) '(gd-edge-qdpr ?x ?y) '(gd-reach-qdpr ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-projected) "
            "  (deduce/fact! gd-edge-qdpr 1 2) "
            "  (deduce/fact! gd-edge-qdpr 2 3) "
            "  (deduce/fact! gd-edge-qdpr 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qdpr "
            "      (lambda (row) "
            "        (and (= (ref row 'src) 1) (= (ref row 'dst) 3))) "
            "      1 "
            "      'semi-naive)) "
            "  (define reach-stats (deduce/stats gd-reach-qdpr)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-projected)) "
            "  (and "
            "    (null? selected) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-applied-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdpr) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref analyze-after 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 2) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 2) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 0) "
            "    (= (car (cdr (ref analyze-after 'last-goal-directed-read-applied-bound-positions))) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can project unsupported equality filters out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can project unsupported equality filters out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can project unsupported equality filters out of the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-projected-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-projected-plain] gd-edge-qdpp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-projected-plain] gd-reach-qdpp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-projected-plain '(gd-reach-qdpp ?x ?y) '(gd-edge-qdpp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-projected-plain '(gd-reach-qdpp ?x ?z) '(gd-edge-qdpp ?x ?y) '(gd-reach-qdpp ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-projected-plain) "
            "  (deduce/fact! gd-edge-qdpp 1 2) "
            "  (deduce/fact! gd-edge-qdpp 2 3) "
            "  (deduce/fact! gd-edge-qdpp 10 11) "
            "  (define selected "
            "    (deduce/query gd-reach-qdpp "
            "      (lambda (row) "
            "        (and (= (ref row 'src) 1) (= (ref row 'dst) 3))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdpp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-projected-plain)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref reach-stats 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdpp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-requested-bound-count) 2) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 2) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-requested-bound-positions)) 0) "
            "    (= (car (cdr (ref analyze-after 'last-goal-directed-read-requested-bound-positions))) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref analyze-after 'last-goal-directed-read-applied-bound-positions)) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can project unsupported equality filters out of the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can project unsupported equality filters out of the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can project unsupported equality filters out of the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-builtin-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-builtin-selector] gd-edge-qdfs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-builtin-selector] gd-reach-qdfs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-builtin-selector] gd-other-src-qdfs (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-builtin-selector] gd-other-out-qdfs (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-builtin-selector '(gd-reach-qdfs ?x ?y) '(gd-edge-qdfs ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-builtin-selector '(gd-reach-qdfs ?x ?z) '(gd-edge-qdfs ?x ?y) '(gd-reach-qdfs ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-builtin-selector '(gd-other-out-qdfs ?x ?y) '(gd-other-src-qdfs ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-builtin-selector) "
            "  (deduce/fact! gd-edge-qdfs 1 2) "
            "  (deduce/fact! gd-edge-qdfs 2 3) "
            "  (deduce/fact! gd-edge-qdfs 10 11) "
            "  (deduce/fact! gd-other-src-qdfs 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdfs "
            "        (lambda (row) (= (ref row 'dst) (+ target 0))) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdfs)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-builtin-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdfs) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use closed builtin numeric expressions in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use closed builtin numeric expressions in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use closed builtin numeric expressions in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-fallback (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-edge-qdf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-reach-qdf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-other-src-qdf (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-other-out-qdf (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-fallback '(gd-reach-qdf ?x ?y) '(gd-edge-qdf ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-fallback '(gd-reach-qdf ?x ?z) '(gd-edge-qdf ?x ?y) '(gd-reach-qdf ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-fallback '(gd-other-out-qdf ?x ?y) '(gd-other-src-qdf ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-fallback) "
            "  (deduce/fact! gd-edge-qdf 1 2) "
            "  (deduce/fact! gd-edge-qdf 2 3) "
            "  (deduce/fact! gd-edge-qdf 10 11) "
            "  (deduce/fact! gd-other-src-qdf 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (deduce/query gd-reach-qdf "
            "        (lambda (row) (= (ref row 'dst) (+ target 0)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdf)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-fallback)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdf)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use closed builtin numeric expressions in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use closed builtin numeric expressions in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use closed builtin numeric expressions in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-selector (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-selector] gd-edge-qdis (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-selector] gd-reach-qdis (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-selector] gd-other-src-qdis (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-selector] gd-other-out-qdis (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-selector '(gd-reach-qdis ?x ?y) '(gd-edge-qdis ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-selector '(gd-reach-qdis ?x ?z) '(gd-edge-qdis ?x ?y) '(gd-reach-qdis ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-selector '(gd-other-out-qdis ?x ?y) '(gd-other-src-qdis ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-selector) "
            "  (deduce/fact! gd-edge-qdis 1 2) "
            "  (deduce/fact! gd-edge-qdis 2 3) "
            "  (deduce/fact! gd-edge-qdis 10 11) "
            "  (deduce/fact! gd-other-src-qdis 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (use-target true) "
            "        (deduce/query gd-reach-qdis "
            "          (lambda (row) "
            "            (if use-target "
            "              (= (ref row 'dst) target) "
            "              (= (ref row 'dst) 11))) "
            "          1 "
            "          'semi-naive)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdis)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-selector)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdis) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can use row-independent if wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can use row-independent if wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can use row-independent if wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-if-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-plain] gd-edge-qdip (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-plain] gd-reach-qdip (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-plain] gd-other-src-qdip (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-if-plain] gd-other-out-qdip (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-plain '(gd-reach-qdip ?x ?y) '(gd-edge-qdip ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-plain '(gd-reach-qdip ?x ?z) '(gd-edge-qdip ?x ?y) '(gd-reach-qdip ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-if-plain '(gd-other-out-qdip ?x ?y) '(gd-other-src-qdip ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-if-plain) "
            "  (deduce/fact! gd-edge-qdip 1 2) "
            "  (deduce/fact! gd-edge-qdip 2 3) "
            "  (deduce/fact! gd-edge-qdip 10 11) "
            "  (deduce/fact! gd-other-src-qdip 9 10) "
            "  (define selected "
            "    (let (target 3) "
            "      (let (use-target true) "
            "        (deduce/query gd-reach-qdip "
            "          (lambda (row) "
            "            (if use-target "
            "              (= (ref row 'dst) target) "
            "              (= (ref row 'dst) 11))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdip)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-if-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdip)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can use row-independent if wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can use row-independent if wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can use row-independent if wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-fallback (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-edge-qdfu (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-reach-qdfu (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-other-src-qdfu (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-fallback] gd-other-out-qdfu (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-fallback '(gd-reach-qdfu ?x ?y) '(gd-edge-qdfu ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-fallback '(gd-reach-qdfu ?x ?z) '(gd-edge-qdfu ?x ?y) '(gd-reach-qdfu ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-fallback '(gd-other-out-qdfu ?x ?y) '(gd-other-src-qdfu ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-fallback) "
            "  (deduce/fact! gd-edge-qdfu 1 2) "
            "  (deduce/fact! gd-edge-qdfu 2 3) "
            "  (deduce/fact! gd-edge-qdfu 10 11) "
            "  (deduce/fact! gd-other-src-qdfu 9 10) "
            "  (define selected "
            "    (let (k 1) "
            "      (let (id (lambda (x) x)) "
            "        (deduce/query gd-reach-qdfu "
            "          (lambda (row) (= (ref row 'src) (id k))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdfu)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-fallback)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'dst)) selected)) 2) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdfu)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce query still falls back to selected closure for dirty closure reads");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce query still falls back to selected closure for dirty closure reads (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce query still falls back to selected closure for dirty closure reads");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-literal-closed (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-literal-closed] gd-edge-qdlc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-literal-closed] gd-reach-qdlc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-literal-closed] gd-other-src-qdlc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-literal-closed] gd-other-out-qdlc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-literal-closed '(gd-reach-qdlc ?x ?y) '(gd-edge-qdlc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-literal-closed '(gd-reach-qdlc ?x ?z) '(gd-edge-qdlc ?x ?y) '(gd-reach-qdlc ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-literal-closed '(gd-other-out-qdlc ?x ?y) '(gd-other-src-qdlc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-literal-closed) "
            "  (deduce/fact! gd-edge-qdlc 1 2) "
            "  (deduce/fact! gd-edge-qdlc 2 3) "
            "  (deduce/fact! gd-edge-qdlc 10 11) "
            "  (deduce/fact! gd-other-src-qdlc 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdlc "
            "      (lambda (row) "
            "        (= (ref row (let (c 'dst) (block c))) "
            "           (let (target 3) (block target)))) "
            "      1)) "
            "  (define reach-stats (deduce/stats gd-reach-qdlc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-literal-closed)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdlc) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can extract closed let and block wrappers inside literal demand expressions");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can extract closed let and block wrappers inside literal demand expressions (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can extract closed let and block wrappers inside literal demand expressions");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-closed (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-closed] gd-edge-qdrc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-closed] gd-reach-qdrc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-closed] gd-other-src-qdrc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-closed] gd-other-out-qdrc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-closed '(gd-reach-qdrc ?x ?y) '(gd-edge-qdrc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-closed '(gd-reach-qdrc ?x ?z) '(gd-edge-qdrc ?x ?y) '(gd-reach-qdrc ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-closed '(gd-other-out-qdrc ?x ?y) '(gd-other-src-qdrc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-closed) "
            "  (deduce/fact! gd-edge-qdrc 1 2) "
            "  (deduce/fact! gd-edge-qdrc 2 3) "
            "  (deduce/fact! gd-edge-qdrc 10 11) "
            "  (deduce/fact! gd-other-src-qdrc 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdrc "
            "      (lambda (row) "
            "        (= (block (let (col 'dst) (ref row col))) 3)) "
            "      1)) "
            "  (define reach-stats (deduce/stats gd-reach-qdrc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-closed)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdrc) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can extract closed let and block wrappers around row column refs");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can extract closed let and block wrappers around row column refs (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can extract closed let and block wrappers around row column refs");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-literal-closed-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-literal-closed-plain] gd-edge-qdlp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-literal-closed-plain] gd-reach-qdlp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-literal-closed-plain] gd-other-src-qdlp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-literal-closed-plain] gd-other-out-qdlp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-literal-closed-plain '(gd-reach-qdlp ?x ?y) '(gd-edge-qdlp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-literal-closed-plain '(gd-reach-qdlp ?x ?z) '(gd-edge-qdlp ?x ?y) '(gd-reach-qdlp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-literal-closed-plain '(gd-other-out-qdlp ?x ?y) '(gd-other-src-qdlp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-literal-closed-plain) "
            "  (deduce/fact! gd-edge-qdlp 1 2) "
            "  (deduce/fact! gd-edge-qdlp 2 3) "
            "  (deduce/fact! gd-edge-qdlp 10 11) "
            "  (deduce/fact! gd-other-src-qdlp 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdlp "
            "      (lambda (row) "
            "        (= (ref row (block (let (c 'dst) c))) "
            "           (block (let (target 3) target)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdlp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-literal-closed-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdlp)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can extract closed let and block wrappers inside literal demand expressions");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can extract closed let and block wrappers inside literal demand expressions (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can extract closed let and block wrappers inside literal demand expressions");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-closed-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-closed-plain] gd-edge-qdcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-closed-plain] gd-reach-qdcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-closed-plain] gd-other-src-qdcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-closed-plain] gd-other-out-qdcp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-closed-plain '(gd-reach-qdcp ?x ?y) '(gd-edge-qdcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-closed-plain '(gd-reach-qdcp ?x ?z) '(gd-edge-qdcp ?x ?y) '(gd-reach-qdcp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-closed-plain '(gd-other-out-qdcp ?x ?y) '(gd-other-src-qdcp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-closed-plain) "
            "  (deduce/fact! gd-edge-qdcp 1 2) "
            "  (deduce/fact! gd-edge-qdcp 2 3) "
            "  (deduce/fact! gd-edge-qdcp 10 11) "
            "  (deduce/fact! gd-other-src-qdcp 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdcp "
            "      (lambda (row) "
            "        (= (let (col (block 'dst)) (ref row col)) 3)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdcp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-closed-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdcp)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can extract closed let and block wrappers around row column refs");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can extract closed let and block wrappers around row column refs (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can extract closed let and block wrappers around row column refs");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-if (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-if] gd-edge-qdri (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-if] gd-reach-qdri (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-if] gd-other-src-qdri (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-if] gd-other-out-qdri (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-if '(gd-reach-qdri ?x ?y) '(gd-edge-qdri ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-if '(gd-reach-qdri ?x ?z) '(gd-edge-qdri ?x ?y) '(gd-reach-qdri ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-if '(gd-other-out-qdri ?x ?y) '(gd-other-src-qdri ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-if) "
            "  (deduce/fact! gd-edge-qdri 1 2) "
            "  (deduce/fact! gd-edge-qdri 2 3) "
            "  (deduce/fact! gd-edge-qdri 10 11) "
            "  (deduce/fact! gd-other-src-qdri 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdri "
            "      (lambda (row) "
            "        (= (if true (ref row 'dst) (ref row 'src)) 3)) "
            "      1)) "
            "  (define reach-stats (deduce/stats gd-reach-qdri)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-if)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdri) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can extract closed if wrappers around row column refs");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can extract closed if wrappers around row column refs (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can extract closed if wrappers around row column refs");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-if-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-if-plain] gd-edge-qdip (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-if-plain] gd-reach-qdip (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-if-plain] gd-other-src-qdip (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-if-plain] gd-other-out-qdip (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-if-plain '(gd-reach-qdip ?x ?y) '(gd-edge-qdip ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-if-plain '(gd-reach-qdip ?x ?z) '(gd-edge-qdip ?x ?y) '(gd-reach-qdip ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-if-plain '(gd-other-out-qdip ?x ?y) '(gd-other-src-qdip ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-if-plain) "
            "  (deduce/fact! gd-edge-qdip 1 2) "
            "  (deduce/fact! gd-edge-qdip 2 3) "
            "  (deduce/fact! gd-edge-qdip 10 11) "
            "  (deduce/fact! gd-other-src-qdip 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdip "
            "      (lambda (row) "
            "        (= (if false (ref row 'src) (ref row 'dst)) 3)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdip)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-if-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdip)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can extract closed if wrappers around row column refs");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can extract closed if wrappers around row column refs (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can extract closed if wrappers around row column refs");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-captured-wrapper (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper] gd-edge-qdrcw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper] gd-reach-qdrcw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper] gd-other-src-qdrcw (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper] gd-other-out-qdrcw (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper '(gd-reach-qdrcw ?x ?y) '(gd-edge-qdrcw ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper '(gd-reach-qdrcw ?x ?z) '(gd-edge-qdrcw ?x ?y) '(gd-reach-qdrcw ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper '(gd-other-out-qdrcw ?x ?y) '(gd-other-src-qdrcw ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper) "
            "  (deduce/fact! gd-edge-qdrcw 1 2) "
            "  (deduce/fact! gd-edge-qdrcw 2 3) "
            "  (deduce/fact! gd-edge-qdrcw 10 11) "
            "  (deduce/fact! gd-other-src-qdrcw 9 10) "
            "  (define selected "
            "    (let (pickdst (lambda (candidate) (ref candidate 'dst))) "
            "      (deduce/query gd-reach-qdrcw "
            "        (lambda (row) (= (pickdst row) 3)) "
            "        1 "
            "        'semi-naive))) "
            "  (define reach-stats (deduce/stats gd-reach-qdrcw)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdrcw) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap one-arg captured row-column wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap one-arg captured row-column wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap one-arg captured row-column wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-captured-wrapper-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-plain] gd-edge-qdrcwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-plain] gd-reach-qdrcwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-plain] gd-other-src-qdrcwp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-plain] gd-other-out-qdrcwp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-plain '(gd-reach-qdrcwp ?x ?y) '(gd-edge-qdrcwp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-plain '(gd-reach-qdrcwp ?x ?z) '(gd-edge-qdrcwp ?x ?y) '(gd-reach-qdrcwp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-plain '(gd-other-out-qdrcwp ?x ?y) '(gd-other-src-qdrcwp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-plain) "
            "  (deduce/fact! gd-edge-qdrcwp 1 2) "
            "  (deduce/fact! gd-edge-qdrcwp 2 3) "
            "  (deduce/fact! gd-edge-qdrcwp 10 11) "
            "  (deduce/fact! gd-other-src-qdrcwp 9 10) "
            "  (define selected "
            "    (let (pickdst (lambda (candidate) (ref candidate 'dst))) "
            "      (deduce/query gd-reach-qdrcwp "
            "        (lambda (row) (= (pickdst row) 3))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdrcwp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdrcwp) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap one-arg captured row-column wrappers in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap one-arg captured row-column wrappers in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap one-arg captured row-column wrappers in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle] gd-edge-qdrcwrm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle] gd-reach-qdrcwrm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle] gd-other-src-qdrcwrm (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle] gd-other-out-qdrcwrm (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle '(gd-reach-qdrcwrm ?x ?y) '(gd-edge-qdrcwrm ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle '(gd-reach-qdrcwrm ?x ?z) '(gd-edge-qdrcwrm ?x ?y) '(gd-reach-qdrcwrm ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle '(gd-other-out-qdrcwrm ?x ?y) '(gd-other-src-qdrcwrm ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle) "
            "  (deduce/fact! gd-edge-qdrcwrm 1 2) "
            "  (deduce/fact! gd-edge-qdrcwrm 2 3) "
            "  (deduce/fact! gd-edge-qdrcwrm 10 11) "
            "  (deduce/fact! gd-other-src-qdrcwrm 9 10) "
            "  (define selected "
            "    (let (use-dst true) "
            "      (let (keep true) "
            "        (let (pick (lambda (which candidate still-dst) (if which (if still-dst (ref candidate 'dst) (ref candidate 'src)) (ref candidate 'src)))) "
            "          (deduce/query gd-reach-qdrcwrm "
            "            (lambda (row) (= (pick use-dst row keep) 3)) "
            "            1 "
            "            'semi-naive))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdrcwrm)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap preserved-row row-column wrappers with non-leading row and multiple literal args");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap preserved-row row-column wrappers with non-leading row and multiple literal args (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap preserved-row row-column wrappers with non-leading row and multiple literal args");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle-plain] gd-edge-qdrcwrmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle-plain] gd-reach-qdrcwrmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle-plain] gd-other-src-qdrcwrmp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle-plain] gd-other-out-qdrcwrmp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle-plain '(gd-reach-qdrcwrmp ?x ?y) '(gd-edge-qdrcwrmp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle-plain '(gd-reach-qdrcwrmp ?x ?z) '(gd-edge-qdrcwrmp ?x ?y) '(gd-reach-qdrcwrmp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle-plain '(gd-other-out-qdrcwrmp ?x ?y) '(gd-other-src-qdrcwrmp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle-plain) "
            "  (deduce/fact! gd-edge-qdrcwrmp 1 2) "
            "  (deduce/fact! gd-edge-qdrcwrmp 2 3) "
            "  (deduce/fact! gd-edge-qdrcwrmp 10 11) "
            "  (deduce/fact! gd-other-src-qdrcwrmp 9 10) "
            "  (define selected "
            "    (let (use-dst true) "
            "      (let (keep true) "
            "        (let (pick (lambda (which candidate still-dst) (if which (if still-dst (ref candidate 'dst) (ref candidate 'src)) (ref candidate 'src)))) "
            "          (deduce/query gd-reach-qdrcwrmp "
            "            (lambda (row) (= (pick use-dst row keep) 3))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdrcwrmp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-row-middle-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap preserved-row row-column wrappers with non-leading row and multiple literal args");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap preserved-row row-column wrappers with non-leading row and multiple literal args (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap preserved-row row-column wrappers with non-leading row and multiple literal args");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args] gd-edge-qdrcwla (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args] gd-reach-qdrcwla (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args] gd-other-src-qdrcwla (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args] gd-other-out-qdrcwla (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args '(gd-reach-qdrcwla ?x ?y) '(gd-edge-qdrcwla ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args '(gd-reach-qdrcwla ?x ?z) '(gd-edge-qdrcwla ?x ?y) '(gd-reach-qdrcwla ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args '(gd-other-out-qdrcwla ?x ?y) '(gd-other-src-qdrcwla ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args) "
            "  (deduce/fact! gd-edge-qdrcwla 1 2) "
            "  (deduce/fact! gd-edge-qdrcwla 2 3) "
            "  (deduce/fact! gd-edge-qdrcwla 10 11) "
            "  (deduce/fact! gd-other-src-qdrcwla 9 10) "
            "  (define selected "
            "    (let (use-dst true) "
            "      (let (pick (lambda (candidate which) (if which (ref candidate 'dst) (ref candidate 'src)))) "
            "        (deduce/query gd-reach-qdrcwla "
            "          (lambda (row) (= (pick row use-dst) 3)) "
            "          1 "
            "          'semi-naive)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdrcwla)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap captured row-column wrappers with closed literal args in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap captured row-column wrappers with closed literal args in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap captured row-column wrappers with closed literal args in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-plain] gd-edge-qdrcwlap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-plain] gd-reach-qdrcwlap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-plain] gd-other-src-qdrcwlap (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-plain] gd-other-out-qdrcwlap (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-plain '(gd-reach-qdrcwlap ?x ?y) '(gd-edge-qdrcwlap ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-plain '(gd-reach-qdrcwlap ?x ?z) '(gd-edge-qdrcwlap ?x ?y) '(gd-reach-qdrcwlap ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-plain '(gd-other-out-qdrcwlap ?x ?y) '(gd-other-src-qdrcwlap ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-plain) "
            "  (deduce/fact! gd-edge-qdrcwlap 1 2) "
            "  (deduce/fact! gd-edge-qdrcwlap 2 3) "
            "  (deduce/fact! gd-edge-qdrcwlap 10 11) "
            "  (deduce/fact! gd-other-src-qdrcwlap 9 10) "
            "  (define selected "
            "    (let (use-dst true) "
            "      (let (pick (lambda (candidate which) (if which (ref candidate 'dst) (ref candidate 'src)))) "
            "        (deduce/query gd-reach-qdrcwlap "
            "          (lambda (row) (= (pick row use-dst) 3)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdrcwlap)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap captured row-column wrappers with closed literal args in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap captured row-column wrappers with closed literal args in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap captured row-column wrappers with closed literal args in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second] gd-edge-qdrcwlars (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second] gd-reach-qdrcwlars (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second] gd-other-src-qdrcwlars (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second] gd-other-out-qdrcwlars (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second '(gd-reach-qdrcwlars ?x ?y) '(gd-edge-qdrcwlars ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second '(gd-reach-qdrcwlars ?x ?z) '(gd-edge-qdrcwlars ?x ?y) '(gd-reach-qdrcwlars ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second '(gd-other-out-qdrcwlars ?x ?y) '(gd-other-src-qdrcwlars ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second) "
            "  (deduce/fact! gd-edge-qdrcwlars 1 2) "
            "  (deduce/fact! gd-edge-qdrcwlars 2 3) "
            "  (deduce/fact! gd-edge-qdrcwlars 10 11) "
            "  (deduce/fact! gd-other-src-qdrcwlars 9 10) "
            "  (define selected "
            "    (let (use-dst true) "
            "      (let (pick (lambda (which candidate) (if which (ref candidate 'dst) (ref candidate 'src)))) "
            "        (deduce/query gd-reach-qdrcwlars "
            "          (lambda (row) (= (pick use-dst row) 3)) "
            "          1 "
            "          'semi-naive)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdrcwlars)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap preserved-row row-column literal-arg wrappers when row is not the first arg");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap preserved-row row-column literal-arg wrappers when row is not the first arg (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap preserved-row row-column literal-arg wrappers when row is not the first arg");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second-plain] gd-edge-qdrcwlarsp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second-plain] gd-reach-qdrcwlarsp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second-plain] gd-other-src-qdrcwlarsp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second-plain] gd-other-out-qdrcwlarsp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second-plain '(gd-reach-qdrcwlarsp ?x ?y) '(gd-edge-qdrcwlarsp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second-plain '(gd-reach-qdrcwlarsp ?x ?z) '(gd-edge-qdrcwlarsp ?x ?y) '(gd-reach-qdrcwlarsp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second-plain '(gd-other-out-qdrcwlarsp ?x ?y) '(gd-other-src-qdrcwlarsp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second-plain) "
            "  (deduce/fact! gd-edge-qdrcwlarsp 1 2) "
            "  (deduce/fact! gd-edge-qdrcwlarsp 2 3) "
            "  (deduce/fact! gd-edge-qdrcwlarsp 10 11) "
            "  (deduce/fact! gd-other-src-qdrcwlarsp 9 10) "
            "  (define selected "
            "    (let (use-dst true) "
            "      (let (pick (lambda (which candidate) (if which (ref candidate 'dst) (ref candidate 'src)))) "
            "        (deduce/query gd-reach-qdrcwlarsp "
            "          (lambda (row) (= (pick use-dst row) 3)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdrcwlarsp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-literal-args-row-second-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap preserved-row row-column literal-arg wrappers when row is not the first arg");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap preserved-row row-column literal-arg wrappers when row is not the first arg (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap preserved-row row-column literal-arg wrappers when row is not the first arg");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain] gd-edge-qdrcwlac (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain] gd-reach-qdrcwlac (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain] gd-other-src-qdrcwlac (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain] gd-other-out-qdrcwlac (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain '(gd-reach-qdrcwlac ?x ?y) '(gd-edge-qdrcwlac ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain '(gd-reach-qdrcwlac ?x ?z) '(gd-edge-qdrcwlac ?x ?y) '(gd-reach-qdrcwlac ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain '(gd-other-out-qdrcwlac ?x ?y) '(gd-other-src-qdrcwlac ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain) "
            "  (deduce/fact! gd-edge-qdrcwlac 1 2) "
            "  (deduce/fact! gd-edge-qdrcwlac 2 3) "
            "  (deduce/fact! gd-edge-qdrcwlac 10 11) "
            "  (deduce/fact! gd-other-src-qdrcwlac 9 10) "
            "  (define selected "
            "    (let (use-dst true) "
            "      (let (pick0 (lambda (candidate which) (if which (ref candidate 'dst) (ref candidate 'src)))) "
            "        (let (pick1 (lambda (candidate which) (pick0 candidate which))) "
            "          (deduce/query gd-reach-qdrcwlac "
            "            (lambda (row) (= (pick1 row use-dst) 3)) "
            "            1 "
            "            'semi-naive))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdrcwlac)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap preserved-row row-column literal-arg wrapper chains in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap preserved-row row-column literal-arg wrapper chains in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap preserved-row row-column literal-arg wrapper chains in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain-plain] gd-edge-qdrcwlacp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain-plain] gd-reach-qdrcwlacp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain-plain] gd-other-src-qdrcwlacp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain-plain] gd-other-out-qdrcwlacp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain-plain '(gd-reach-qdrcwlacp ?x ?y) '(gd-edge-qdrcwlacp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain-plain '(gd-reach-qdrcwlacp ?x ?z) '(gd-edge-qdrcwlacp ?x ?y) '(gd-reach-qdrcwlacp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain-plain '(gd-other-out-qdrcwlacp ?x ?y) '(gd-other-src-qdrcwlacp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain-plain) "
            "  (deduce/fact! gd-edge-qdrcwlacp 1 2) "
            "  (deduce/fact! gd-edge-qdrcwlacp 2 3) "
            "  (deduce/fact! gd-edge-qdrcwlacp 10 11) "
            "  (deduce/fact! gd-other-src-qdrcwlacp 9 10) "
            "  (define selected "
            "    (let (use-dst true) "
            "      (let (pick0 (lambda (candidate which) (if which (ref candidate 'dst) (ref candidate 'src)))) "
            "        (let (pick1 (lambda (candidate which) (pick0 candidate which))) "
            "          (deduce/query gd-reach-qdrcwlacp "
            "            (lambda (row) (= (pick1 row use-dst) 3))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdrcwlacp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-literal-arg-chain-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap preserved-row row-column literal-arg wrapper chains in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap preserved-row row-column literal-arg wrapper chains in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap preserved-row row-column literal-arg wrapper chains in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-captured-wrapper-chain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-chain] gd-edge-qdrcwc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-chain] gd-reach-qdrcwc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-chain] gd-other-src-qdrcwc (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-chain] gd-other-out-qdrcwc (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-chain '(gd-reach-qdrcwc ?x ?y) '(gd-edge-qdrcwc ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-chain '(gd-reach-qdrcwc ?x ?z) '(gd-edge-qdrcwc ?x ?y) '(gd-reach-qdrcwc ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-chain '(gd-other-out-qdrcwc ?x ?y) '(gd-other-src-qdrcwc ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-chain) "
            "  (deduce/fact! gd-edge-qdrcwc 1 2) "
            "  (deduce/fact! gd-edge-qdrcwc 2 3) "
            "  (deduce/fact! gd-edge-qdrcwc 10 11) "
            "  (deduce/fact! gd-other-src-qdrcwc 9 10) "
            "  (define selected "
            "    (let (pick0 (lambda (candidate) (ref candidate 'dst))) "
            "      (let (pick1 (lambda (candidate) (pick0 candidate))) "
            "        (deduce/query gd-reach-qdrcwc "
            "          (lambda (row) (= (pick1 row) 3)) "
            "          1 "
            "          'semi-naive)))) "
            "  (define reach-stats (deduce/stats gd-reach-qdrcwc)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-chain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can unwrap one-arg captured row-column wrapper chains in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can unwrap one-arg captured row-column wrapper chains in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can unwrap one-arg captured row-column wrapper chains in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-captured-wrapper-chain-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-chain-plain] gd-edge-qdrcwcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-chain-plain] gd-reach-qdrcwcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-chain-plain] gd-other-src-qdrcwcp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-captured-wrapper-chain-plain] gd-other-out-qdrcwcp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-chain-plain '(gd-reach-qdrcwcp ?x ?y) '(gd-edge-qdrcwcp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-chain-plain '(gd-reach-qdrcwcp ?x ?z) '(gd-edge-qdrcwcp ?x ?y) '(gd-reach-qdrcwcp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-captured-wrapper-chain-plain '(gd-other-out-qdrcwcp ?x ?y) '(gd-other-src-qdrcwcp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-chain-plain) "
            "  (deduce/fact! gd-edge-qdrcwcp 1 2) "
            "  (deduce/fact! gd-edge-qdrcwcp 2 3) "
            "  (deduce/fact! gd-edge-qdrcwcp 10 11) "
            "  (deduce/fact! gd-other-src-qdrcwcp 9 10) "
            "  (define selected "
            "    (let (pick0 (lambda (candidate) (ref candidate 'dst))) "
            "      (let (pick1 (lambda (candidate) (pick0 candidate))) "
            "        (deduce/query gd-reach-qdrcwcp "
            "          (lambda (row) (= (pick1 row) 3)))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdrcwcp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-captured-wrapper-chain-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) nil)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can unwrap one-arg captured row-column wrapper chains in the demand path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can unwrap one-arg captured row-column wrapper chains in the demand path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can unwrap one-arg captured row-column wrapper chains in the demand path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-boolean (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-boolean] gd-edge-qdrb (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-boolean] gd-reach-qdrb (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-boolean] gd-other-src-qdrb (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-boolean] gd-other-out-qdrb (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-boolean '(gd-reach-qdrb ?x ?y) '(gd-edge-qdrb ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-boolean '(gd-reach-qdrb ?x ?z) '(gd-edge-qdrb ?x ?y) '(gd-reach-qdrb ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-boolean '(gd-other-out-qdrb ?x ?y) '(gd-other-src-qdrb ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-boolean) "
            "  (deduce/fact! gd-edge-qdrb 1 2) "
            "  (deduce/fact! gd-edge-qdrb 2 3) "
            "  (deduce/fact! gd-edge-qdrb 10 11) "
            "  (deduce/fact! gd-other-src-qdrb 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdrb "
            "      (lambda (row) "
            "        (and "
            "          (= (and true (ref row 'dst)) 3) "
            "          (= (or false (ref row 'dst)) 3))) "
            "      1)) "
            "  (define reach-stats (deduce/stats gd-reach-qdrb)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-boolean)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdrb) "
            "    (= (ref analyze-after 'last-goal-directed-read-selector-rule-index) 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] selector-scoped deduce query can extract closed boolean wrappers around row column refs");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] selector-scoped deduce query can extract closed boolean wrappers around row column refs (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] selector-scoped deduce query can extract closed boolean wrappers around row column refs");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-demand-row-boolean-plain (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-boolean-plain] gd-edge-qdbrp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-boolean-plain] gd-reach-qdbrp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-boolean-plain] gd-other-src-qdbrp (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-demand-row-boolean-plain] gd-other-out-qdbrp (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-boolean-plain '(gd-reach-qdbrp ?x ?y) '(gd-edge-qdbrp ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-boolean-plain '(gd-reach-qdbrp ?x ?z) '(gd-edge-qdbrp ?x ?y) '(gd-reach-qdbrp ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-demand-row-boolean-plain '(gd-other-out-qdbrp ?x ?y) '(gd-other-src-qdbrp ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-demand-row-boolean-plain) "
            "  (deduce/fact! gd-edge-qdbrp 1 2) "
            "  (deduce/fact! gd-edge-qdbrp 2 3) "
            "  (deduce/fact! gd-edge-qdbrp 10 11) "
            "  (deduce/fact! gd-other-src-qdbrp 9 10) "
            "  (define selected "
            "    (deduce/query gd-reach-qdbrp "
            "      (lambda (row) "
            "        (and "
            "          (= (and true (ref row 'dst)) 3) "
            "          (= (or false (ref row 'dst)) 3))))) "
            "  (define reach-stats (deduce/stats gd-reach-qdbrp)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-demand-row-boolean-plain)) "
            "  (and "
            "    (= (length selected) 2) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'src)) selected)) 3) "
            "    (= (ref (car selected) 'dst) 3) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-surface) 'query) "
            "    (= (ref reach-stats 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-requested-bound-positions)) 1) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) 1) "
            "    (= (length (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (car (ref reach-stats 'last-goal-directed-read-applied-bound-positions)) 1) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qdbrp)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query can extract closed boolean wrappers around row column refs");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query can extract closed boolean wrappers around row column refs (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query can extract closed boolean wrappers around row column refs");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-auto] gd-edge-qa0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto] gd-reach-qa0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto] gd-other-src-qa0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto] gd-other-out-qa0 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto '(gd-reach-qa0 ?x ?y) '(gd-edge-qa0 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto '(gd-reach-qa0 ?x ?z) '(gd-edge-qa0 ?x ?y) '(gd-reach-qa0 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto '(gd-other-out-qa0 ?x ?y) '(gd-other-src-qa0 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-auto) "
            "  (deduce/fact! gd-edge-qa0 1 2) "
            "  (deduce/fact! gd-edge-qa0 2 3) "
            "  (deduce/fact! gd-other-src-qa0 9 10) "
            "  (define selected (deduce/query gd-reach-qa0 (lambda (row) true))) "
            "  (define other-stats (deduce/stats gd-other-out-qa0)) "
            "  (define other-before (deduce/count gd-other-out-qa0)) "
            "  (deduce/analyze db-admin-goal-directed-query-auto) "
            "  (define other-after (deduce/count gd-other-out-qa0)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] ordinary deduce query auto-executes the target goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] ordinary deduce query auto-executes the target goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] ordinary deduce query auto-executes the target goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-auto-id (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-auto-id] gd-edge-qa1 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto-id] gd-reach-qa1 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto-id] gd-other-src-qa1 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto-id] gd-other-out-qa1 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto-id '(gd-reach-qa1 ?x ?y) '(gd-edge-qa1 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto-id '(gd-reach-qa1 ?x ?z) '(gd-edge-qa1 ?x ?y) '(gd-reach-qa1 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto-id '(gd-other-out-qa1 ?x ?y) '(gd-other-src-qa1 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-auto-id) "
            "  (deduce/fact! gd-edge-qa1 1 2) "
            "  (deduce/fact! gd-edge-qa1 2 3) "
            "  (deduce/fact! gd-other-src-qa1 9 10) "
            "  (define selected "
            "    (let (k 1) "
            "      (let (id (lambda (x) x)) "
            "        (deduce/query gd-reach-qa1 "
            "          (lambda (row) (= (ref row 'src) (id k))))))) "
            "  (define reach-stats (deduce/stats gd-reach-qa1)) "
            "  (define analyze-after (deduce/analyze db-admin-goal-directed-query-auto-id)) "
            "  (define other-after (deduce/count gd-other-out-qa1)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'dst)) selected)) 2) "
            "    (= (ref reach-stats 'dirty-self) true) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-execution-path) 'ephemeral-head-demand-query) "
            "    (= (ref analyze-after 'last-goal-directed-read-fallback-reason) nil) "
            "    (= (ref analyze-after 'last-goal-directed-read-relation) 'gd-reach-qa1) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] ordinary deduce query auto-executes the target goal-directed recursive closure through identity wrapper");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] ordinary deduce query auto-executes the target goal-directed recursive closure through identity wrapper (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] ordinary deduce query auto-executes the target goal-directed recursive closure through identity wrapper");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-edge-ma0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-reach-ma0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-other-src-ma0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-other-out-ma0 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-auto '(gd-reach-ma0 ?x ?y) '(gd-edge-ma0 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-auto '(gd-reach-ma0 ?x ?z) '(gd-edge-ma0 ?x ?y) '(gd-reach-ma0 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-auto '(gd-other-out-ma0 ?x ?y) '(gd-other-src-ma0 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-auto) "
            "  (deduce/fact! gd-edge-ma0 1 2) "
            "  (deduce/fact! gd-edge-ma0 2 3) "
            "  (deduce/fact! gd-other-src-ma0 9 10) "
            "  (define selected (deduce/match gd-reach-ma0 '(gd-reach-ma0 ?x ?y))) "
            "  (define other-stats (deduce/stats gd-other-out-ma0)) "
            "  (define other-before (deduce/count gd-other-out-ma0)) "
            "  (deduce/analyze db-admin-goal-directed-match-auto) "
            "  (define other-after (deduce/count gd-other-out-ma0)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] ordinary deduce match auto-executes the target goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] ordinary deduce match auto-executes the target goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] ordinary deduce match auto-executes the target goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-count-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-edge-ca0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-reach-ca0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-other-src-ca0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-other-out-ca0 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-count-auto '(gd-reach-ca0 ?x ?y) '(gd-edge-ca0 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-count-auto '(gd-reach-ca0 ?x ?z) '(gd-edge-ca0 ?x ?y) '(gd-reach-ca0 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-count-auto '(gd-other-out-ca0 ?x ?y) '(gd-other-src-ca0 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-count-auto) "
            "  (deduce/fact! gd-edge-ca0 1 2) "
            "  (deduce/fact! gd-edge-ca0 2 3) "
            "  (deduce/fact! gd-other-src-ca0 9 10) "
            "  (define count-selected (deduce/count gd-reach-ca0)) "
            "  (define other-stats (deduce/stats gd-other-out-ca0)) "
            "  (define other-before (deduce/count gd-other-out-ca0)) "
            "  (deduce/analyze db-admin-goal-directed-count-auto) "
            "  (define other-after (deduce/count gd-other-out-ca0)) "
            "  (and "
            "    (= count-selected 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] ordinary deduce count auto-executes the target goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] ordinary deduce count auto-executes the target goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] ordinary deduce count auto-executes the target goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-edge-sa0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-reach-sa0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-other-src-sa0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-other-out-sa0 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-auto '(gd-reach-sa0 ?x ?y) '(gd-edge-sa0 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-auto '(gd-reach-sa0 ?x ?z) '(gd-edge-sa0 ?x ?y) '(gd-reach-sa0 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-auto '(gd-other-out-sa0 ?x ?y) '(gd-other-src-sa0 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-auto) "
            "  (deduce/fact! gd-edge-sa0 1 2) "
            "  (deduce/fact! gd-edge-sa0 2 3) "
            "  (deduce/fact! gd-other-src-sa0 9 10) "
            "  (define scan-selected (deduce/scan gd-reach-sa0)) "
            "  (define other-stats (deduce/stats gd-other-out-sa0)) "
            "  (define other-before (deduce/count gd-other-out-sa0)) "
            "  (deduce/analyze db-admin-goal-directed-scan-auto) "
            "  (define other-after (deduce/count gd-other-out-sa0)) "
            "  (and "
            "    (= (length scan-selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] ordinary deduce scan auto-executes the target goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] ordinary deduce scan auto-executes the target goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] ordinary deduce scan auto-executes the target goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-scan-range-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-edge-sra0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-reach-sra0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-other-src-sra0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-other-out-sra0 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-auto '(gd-reach-sra0 ?x ?y) '(gd-edge-sra0 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-auto '(gd-reach-sra0 ?x ?z) '(gd-edge-sra0 ?x ?y) '(gd-reach-sra0 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-auto '(gd-other-out-sra0 ?x ?y) '(gd-other-src-sra0 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-auto) "
            "  (deduce/fact! gd-edge-sra0 1 2) "
            "  (deduce/fact! gd-edge-sra0 2 3) "
            "  (deduce/fact! gd-other-src-sra0 9 10) "
            "  (define range-selected (deduce/scan-range gd-reach-sra0 '(1 2) '(2 3))) "
            "  (define other-stats (deduce/stats gd-other-out-sra0)) "
            "  (define other-before (deduce/count gd-other-out-sra0)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-auto) "
            "  (define other-after (deduce/count gd-other-out-sra0)) "
            "  (and "
            "    (= (length range-selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] ordinary deduce scan-range auto-executes the target goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] ordinary deduce scan-range auto-executes the target goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] ordinary deduce scan-range auto-executes the target goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-clean-target (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-clean-target] gd-edge-cta (src) (dst)) "
            "  (define [relation db-admin-goal-directed-clean-target] gd-reach-cta (src) (dst)) "
            "  (define [relation db-admin-goal-directed-clean-target] gd-edge-ctb (src) (dst)) "
            "  (define [relation db-admin-goal-directed-clean-target] gd-reach-ctb (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-clean-target '(gd-reach-cta ?x ?y) '(gd-edge-cta ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-clean-target '(gd-reach-cta ?x ?z) '(gd-edge-cta ?x ?y) '(gd-reach-cta ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-clean-target '(gd-reach-ctb ?x ?y) '(gd-edge-ctb ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-clean-target '(gd-reach-ctb ?x ?z) '(gd-edge-ctb ?x ?y) '(gd-reach-ctb ?y ?z)) "
            "  (deduce/fact! gd-edge-cta 1 2) "
            "  (deduce/analyze db-admin-goal-directed-clean-target) "
            "  (deduce/fact! gd-edge-ctb 9 10) "
            "  (define stats0 (deduce/stats gd-reach-ctb)) "
            "  (define clean-read (deduce/count gd-reach-cta)) "
            "  (define clean-target-stats (deduce/stats gd-reach-cta)) "
            "  (define stats1 (deduce/stats gd-reach-ctb)) "
            "  (define analyze0 (deduce/analyze db-admin-goal-directed-clean-target)) "
            "  (deduce/analyze db-admin-goal-directed-clean-target) "
            "  (define sibling-count (deduce/count gd-reach-ctb)) "
            "  (and "
            "    (= clean-read 1) "
            "    (= (ref stats0 'dirty-self) true) "
            "    (= (ref stats1 'dirty-self) true) "
            "    (= (ref clean-target-stats 'last-goal-directed-read-execution-path) 'no-op) "
            "    (= (ref clean-target-stats 'last-goal-directed-read-mutation-epoch) (ref clean-target-stats 'mutation-epoch)) "
            "    (= (ref analyze0 'last-goal-directed-read-execution-path) 'no-op) "
            "    (= (ref analyze0 'last-goal-directed-read-relation) 'gd-reach-cta) "
            "    (= sibling-count 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain reads on clean eligible recursive targets do not execute unrelated dirty closures");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain reads on clean eligible recursive targets do not execute unrelated dirty closures (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain reads on clean eligible recursive targets do not execute unrelated dirty closures");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-unrelated-read (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-unrelated-read] gd-edge-ur (src) (dst)) "
            "  (define [relation db-admin-goal-directed-unrelated-read] gd-reach-ur (src) (dst)) "
            "  (define [relation db-admin-goal-directed-unrelated-read] gd-plain-ur (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-unrelated-read '(gd-reach-ur ?x ?y) '(gd-edge-ur ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-unrelated-read '(gd-reach-ur ?x ?z) '(gd-edge-ur ?x ?y) '(gd-reach-ur ?y ?z)) "
            "  (deduce/fact! gd-plain-ur 50 60) "
            "  (deduce/analyze db-admin-goal-directed-unrelated-read) "
            "  (deduce/fact! gd-edge-ur 7 8) "
            "  (define stats0 (deduce/stats gd-reach-ur)) "
            "  (define plain-count (deduce/count gd-plain-ur)) "
            "  (define stats1 (deduce/stats gd-reach-ur)) "
            "  (deduce/analyze db-admin-goal-directed-unrelated-read) "
            "  (define reach-count (deduce/count gd-reach-ur)) "
            "  (and "
            "    (= plain-count 1) "
            "    (= (ref stats0 'dirty-self) true) "
            "    (= (ref stats1 'dirty-self) true) "
            "    (= reach-count 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain reads on unrelated non-recursive targets do not execute dirty recursive closures");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain reads on unrelated non-recursive targets do not execute dirty recursive closures (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain reads on unrelated non-recursive targets do not execute dirty recursive closures");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-negated-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-negated-auto] gd-edge-na1 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-negated-auto] gd-reach-na1 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-negated-auto] gd-edge-na2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-negated-auto] gd-blocked-na2 (node)) "
            "  (define [relation db-admin-goal-directed-negated-auto] gd-reach-na2 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-negated-auto '(gd-reach-na1 ?x ?y) '(gd-edge-na1 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-negated-auto '(gd-reach-na1 ?x ?z) '(gd-edge-na1 ?x ?y) '(gd-reach-na1 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-negated-auto '(gd-reach-na2 ?x ?y) '(gd-edge-na2 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-negated-auto '(gd-reach-na2 ?x ?z) '(gd-edge-na2 ?x ?y) '(gd-reach-na2 ?y ?z) '(not (gd-blocked-na2 ?z))) "
            "  (deduce/analyze db-admin-goal-directed-negated-auto) "
            "  (deduce/fact! gd-edge-na1 3 4) "
            "  (define stats0 (deduce/stats gd-reach-na1)) "
            "  (define blocked-read (deduce/count gd-reach-na2)) "
            "  (define stats1 (deduce/stats gd-reach-na1)) "
            "  (deduce/analyze db-admin-goal-directed-negated-auto) "
            "  (define positive-count (deduce/count gd-reach-na1)) "
            "  (and "
            "    (= blocked-read 0) "
            "    (= (ref stats0 'dirty-self) true) "
            "    (= (ref stats1 'dirty-self) true) "
            "    (= positive-count 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain reads on blocked negated recursive targets do not execute unrelated eligible closures");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain reads on blocked negated recursive targets do not execute unrelated eligible closures (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain reads on blocked negated recursive targets do not execute unrelated eligible closures");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-auto-blocked (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-auto-blocked] gd-edge-ab0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-auto-blocked] gd-score-ab0 (src) (score)) "
            "  (deduce/rule! db-admin-goal-directed-auto-blocked '(gd-score-ab0 ?x (sum ?n)) '(gd-edge-ab0 ?x ?n)) "
            "  (deduce/rule! db-admin-goal-directed-auto-blocked '(gd-score-ab0 ?x (sum ?n)) '(gd-edge-ab0 ?x ?y) '(gd-score-ab0 ?y ?n)) "
            "  (deduce/analyze db-admin-goal-directed-auto-blocked) "
            "  (deduce/fact! gd-edge-ab0 1 2) "
            "  (deduce/fact! gd-edge-ab0 2 3) "
            "  (define before (deduce/count gd-score-ab0)) "
            "  (define stats-before (deduce/stats gd-score-ab0)) "
            "  (deduce/analyze db-admin-goal-directed-auto-blocked) "
            "  (define after (deduce/count gd-score-ab0)) "
            "  (and "
            "    (= before 0) "
            "    (= (ref stats-before 'dirty-self) true) "
            "    (> after 0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] ordinary relation reads do not auto-execute blocked recursive aggregate closures");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] ordinary relation reads do not auto-execute blocked recursive aggregate closures (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] ordinary relation reads do not auto-execute blocked recursive aggregate closures");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-auto] gd-edge-qa2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto] gd-reach-qa2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto] gd-other-src-qa2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-auto] gd-other-out-qa2 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto '(gd-reach-qa2 ?x ?y) '(gd-edge-qa2 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto '(gd-reach-qa2 ?x ?z) '(gd-edge-qa2 ?x ?y) '(gd-reach-qa2 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-query-auto '(gd-other-out-qa2 ?x ?y) '(gd-other-src-qa2 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-query-auto) "
            "  (deduce/fact! gd-edge-qa2 1 2) "
            "  (deduce/fact! gd-edge-qa2 2 3) "
            "  (deduce/fact! gd-other-src-qa2 9 10) "
            "  (define selected (deduce/query gd-reach-qa2 (lambda (row) true))) "
            "  (define reach-stats (deduce/stats gd-reach-qa2)) "
            "  (define other-stats (deduce/stats gd-other-out-qa2)) "
            "  (define other-before (deduce/count gd-other-out-qa2)) "
            "  (deduce/analyze db-admin-goal-directed-query-auto) "
            "  (define other-after (deduce/count gd-other-out-qa2)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= other-before 0) "
            "    (= (foldl + 0 (map (lambda (row) (ref row 'dst)) selected)) 8) "
            "    (= (ref reach-stats 'dirty-self) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref reach-stats 'last-goal-directed-read-requested-bound-count) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-applied-bound-count) nil) "
            "    (= (ref reach-stats 'last-goal-directed-read-mutation-epoch) (ref reach-stats 'mutation-epoch)) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce query auto-executes an eligible dirty goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce query auto-executes an eligible dirty goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce query auto-executes an eligible dirty goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-query-last-read (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-query-last-read] gd-edge-qlr (src) (dst)) "
            "  (define [relation db-admin-goal-directed-query-last-read] gd-reach-qlr (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-query-last-read '(gd-reach-qlr ?x ?y) '(gd-edge-qlr ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-query-last-read '(gd-reach-qlr ?x ?z) '(gd-edge-qlr ?x ?y) '(gd-reach-qlr ?y ?z)) "
            "  (deduce/analyze db-admin-goal-directed-query-last-read) "
            "  (deduce/fact! gd-edge-qlr 1 2) "
            "  (deduce/fact! gd-edge-qlr 2 3) "
            "  (deduce/query gd-reach-qlr (lambda (row) true)) "
            "  (define stats0 (deduce/stats gd-reach-qlr)) "
            "  (define analyze0 (deduce/analyze db-admin-goal-directed-query-last-read)) "
            "  (and "
            "    (= (ref stats0 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref stats0 'last-goal-directed-read-surface) 'query) "
            "    (= (ref stats0 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref stats0 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref stats0 'last-goal-directed-read-mutation-epoch) (ref stats0 'mutation-epoch)) "
            "    (= (ref analyze0 'last-goal-directed-read-execution-path) 'selected-component-closure) "
            "    (= (ref analyze0 'last-goal-directed-read-surface) 'query) "
            "    (= (ref analyze0 'last-goal-directed-read-fallback-reason) 'dirty-closure) "
            "    (= (ref analyze0 'last-goal-directed-read-relation) 'gd-reach-qlr) "
            "    (= (ref analyze0 'last-goal-directed-read-selector-rule-index) nil) "
            "    (= (ref analyze0 'last-goal-directed-read-mutation-epoch) 2)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain selected-closure reads record the last actual goal-directed read path");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain selected-closure reads record the last actual goal-directed read path (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain selected-closure reads record the last actual goal-directed read path");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-full-read (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-full-read] gd-edge-fr0 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-full-read] gd-reach-fr0 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-full-read '(gd-reach-fr0 ?x ?y) '(gd-edge-fr0 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-full-read '(gd-reach-fr0 ?x ?z) '(gd-edge-fr0 ?x ?y) '(gd-reach-fr0 ?y ?z)) "
            "  (deduce/fact! gd-edge-fr0 1 2) "
            "  (deduce/fact! gd-edge-fr0 2 3) "
            "  (deduce/analyze db-admin-goal-directed-full-read) "
            "  (deduce/retract! gd-edge-fr0 2 3) "
            "  (define selected (deduce/query gd-reach-fr0 (lambda (row) true))) "
            "  (define reach-stats (deduce/stats gd-reach-fr0)) "
            "  (define analyze0 (deduce/analyze db-admin-goal-directed-full-read)) "
            "  (and "
            "    (= (length selected) 1) "
            "    (= (ref (car selected) 'src) 1) "
            "    (= (ref (car selected) 'dst) 2) "
            "    (= (ref reach-stats 'dirty-self) false) "
            "    (= (ref reach-stats 'full-recompute-required) false) "
            "    (= (ref reach-stats 'last-goal-directed-read-execution-path) 'full-db-fixpoint) "
            "    (= (ref reach-stats 'last-goal-directed-read-fallback-reason) 'full-recompute-required) "
            "    (= (ref reach-stats 'last-goal-directed-read-mutation-epoch) (ref reach-stats 'mutation-epoch)) "
            "    (= (ref analyze0 'last-goal-directed-read-execution-path) 'full-db-fixpoint) "
            "    (= (ref analyze0 'last-goal-directed-read-fallback-reason) 'full-recompute-required) "
            "    (= (ref analyze0 'last-goal-directed-read-relation) 'gd-reach-fr0)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain derived reads record full-db fixpoint when destructive changes force recompute");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain derived reads record full-db fixpoint when destructive changes force recompute (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain derived reads record full-db fixpoint when destructive changes force recompute");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-match-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-edge-ma2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-reach-ma2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-other-src-ma2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-match-auto] gd-other-out-ma2 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-match-auto '(gd-reach-ma2 ?x ?y) '(gd-edge-ma2 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-match-auto '(gd-reach-ma2 ?x ?z) '(gd-edge-ma2 ?x ?y) '(gd-reach-ma2 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-match-auto '(gd-other-out-ma2 ?x ?y) '(gd-other-src-ma2 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-match-auto) "
            "  (deduce/fact! gd-edge-ma2 1 2) "
            "  (deduce/fact! gd-edge-ma2 2 3) "
            "  (deduce/fact! gd-other-src-ma2 9 10) "
            "  (define selected (deduce/match gd-reach-ma2 '(gd-reach-ma2 ?x ?y))) "
            "  (define other-stats (deduce/stats gd-other-out-ma2)) "
            "  (define other-before (deduce/count gd-other-out-ma2)) "
            "  (deduce/analyze db-admin-goal-directed-match-auto) "
            "  (define other-after (deduce/count gd-other-out-ma2)) "
            "  (and "
            "    (= (length selected) 3) "
            "    (= other-before 0) "
            "    (= (ref other-stats 'dirty-self) true) "
            "    (= other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce match auto-executes an eligible dirty goal-directed recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce match auto-executes an eligible dirty goal-directed recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce match auto-executes an eligible dirty goal-directed recursive closure");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-goal-directed-count-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-edge-ca2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-reach-ca2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-other-src-ca2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-count-auto] gd-other-out-ca2 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-count-auto '(gd-reach-ca2 ?x ?y) '(gd-edge-ca2 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-count-auto '(gd-reach-ca2 ?x ?z) '(gd-edge-ca2 ?x ?y) '(gd-reach-ca2 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-count-auto '(gd-other-out-ca2 ?x ?y) '(gd-other-src-ca2 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-count-auto) "
            "  (deduce/fact! gd-edge-ca2 1 2) "
            "  (deduce/fact! gd-edge-ca2 2 3) "
            "  (deduce/fact! gd-other-src-ca2 9 10) "
            "  (define count-selected (deduce/count gd-reach-ca2)) "
            "  (define count-other-stats (deduce/stats gd-other-out-ca2)) "
            "  (define count-other-before (deduce/count gd-other-out-ca2)) "
            "  (deduce/analyze db-admin-goal-directed-count-auto) "
            "  (define count-other-after (deduce/count gd-other-out-ca2)) "
            "  (define db-admin-goal-directed-scan-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-edge-sa2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-reach-sa2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-other-src-sa2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-auto] gd-other-out-sa2 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-auto '(gd-reach-sa2 ?x ?y) '(gd-edge-sa2 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-auto '(gd-reach-sa2 ?x ?z) '(gd-edge-sa2 ?x ?y) '(gd-reach-sa2 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-auto '(gd-other-out-sa2 ?x ?y) '(gd-other-src-sa2 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-auto) "
            "  (deduce/fact! gd-edge-sa2 1 2) "
            "  (deduce/fact! gd-edge-sa2 2 3) "
            "  (deduce/fact! gd-other-src-sa2 9 10) "
            "  (define scan-selected (deduce/scan gd-reach-sa2)) "
            "  (define scan-other-stats (deduce/stats gd-other-out-sa2)) "
            "  (define scan-other-before (deduce/count gd-other-out-sa2)) "
            "  (deduce/analyze db-admin-goal-directed-scan-auto) "
            "  (define scan-other-after (deduce/count gd-other-out-sa2)) "
            "  (define db-admin-goal-directed-scan-range-auto (deduce 'open 'memory)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-edge-sra2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-reach-sra2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-other-src-sra2 (src) (dst)) "
            "  (define [relation db-admin-goal-directed-scan-range-auto] gd-other-out-sra2 (src) (dst)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-auto '(gd-reach-sra2 ?x ?y) '(gd-edge-sra2 ?x ?y)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-auto '(gd-reach-sra2 ?x ?z) '(gd-edge-sra2 ?x ?y) '(gd-reach-sra2 ?y ?z)) "
            "  (deduce/rule! db-admin-goal-directed-scan-range-auto '(gd-other-out-sra2 ?x ?y) '(gd-other-src-sra2 ?x ?y)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-auto) "
            "  (deduce/fact! gd-edge-sra2 1 2) "
            "  (deduce/fact! gd-edge-sra2 2 3) "
            "  (deduce/fact! gd-other-src-sra2 9 10) "
            "  (define range-selected (deduce/scan-range gd-reach-sra2 '(1 2) '(2 3))) "
            "  (define range-other-stats (deduce/stats gd-other-out-sra2)) "
            "  (define range-other-before (deduce/count gd-other-out-sra2)) "
            "  (deduce/analyze db-admin-goal-directed-scan-range-auto) "
            "  (define range-other-after (deduce/count gd-other-out-sra2)) "
            "  (and "
            "    (= count-selected 3) "
            "    (= count-other-before 0) "
            "    (= (ref count-other-stats 'dirty-self) true) "
            "    (= count-other-after 1) "
            "    (= (length scan-selected) 3) "
            "    (= scan-other-before 0) "
            "    (= (ref scan-other-stats 'dirty-self) true) "
            "    (= scan-other-after 1) "
            "    (= (length range-selected) 3) "
            "    (= range-other-before 0) "
            "    (= (ref range-other-stats 'dirty-self) true) "
            "    (= range-other-after 1)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] plain deduce count, scan, and scan-range auto-execute eligible dirty goal-directed recursive closures");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] plain deduce count, scan, and scan-range auto-execute eligible dirty goal-directed recursive closures (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] plain deduce count, scan, and scan-range auto-execute eligible dirty goal-directed recursive closures");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-check (deduce 'open 'memory)) "
            "  (define [relation db-admin-check] admin-check (id) (check positive amount) (note)) "
            "  (define schema (deduce/schema admin-check)) "
            "  (define analyze (deduce/analyze db-admin-check)) "
            "  (define constraints (ref schema 'constraints)) "
            "  (define first (car constraints)) "
            "  (define columns (ref first 'columns)) "
            "  (and (= (ref schema 'constraint-count) 1) "
            "       (and (= (ref schema 'check-constraint-count) 1) "
            "            (and (= (ref analyze 'check-constraint-count) 1) "
            "                 (and (= (ref analyze 'integrity-constraint-count) 1) "
            "                      (and (= (ref first 'kind) 'check) "
            "                           (and (= (car columns) 'amount) "
            "                                (and (= (ref first 'predicate) 'positive) "
            "                                     (= (ref first 'enforced) false)))))))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce schema/analyze surfaces expose check integrity metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce schema/analyze surfaces expose check integrity metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce schema/analyze surfaces expose check integrity metadata");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-unique (deduce 'open 'memory)) "
            "  (define [relation db-admin-unique] admin-unique (id) (unique email) (index dept)) "
            "  (deduce 'fact! admin-unique 1 \"alice@x\" \"ops\") "
            "  (define schema (deduce/schema admin-unique)) "
            "  (define analyze (deduce/analyze db-admin-unique)) "
            "  (define constraints (ref schema 'constraints)) "
            "  (define first (car constraints)) "
            "  (and (= (ref schema 'constraint-count) 1) "
            "       (and (= (ref analyze 'keyed-relation-count) 0) "
            "            (and (= (ref analyze 'unique-relation-count) 1) "
            "                 (and (= (ref analyze 'integrity-constraint-count) 1) "
            "                      (and (= (ref schema 'unique-column) 'email) "
            "                           (and (= (ref first 'kind) 'unique) "
            "                                (and (= (ref first 'unique) true) "
            "                                     (= (ref first 'enforced) true)))))))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce schema/analyze surfaces expose unique integrity metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce schema/analyze surfaces expose unique integrity metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce schema/analyze surfaces expose unique integrity metadata");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-unique-composite (deduce 'open 'memory)) "
            "  (define [relation db-admin-unique-composite] admin-unique-composite (id) (unique email) (unique dept)) "
            "  (deduce 'fact! admin-unique-composite 1 \"alice@x\" \"ops\") "
            "  (define schema (deduce/schema admin-unique-composite)) "
            "  (define analyze (deduce/analyze db-admin-unique-composite)) "
            "  (define constraints (ref schema 'constraints)) "
            "  (define first (car constraints)) "
            "  (define unique-columns (ref schema 'unique-columns)) "
            "  (define constraint-columns (ref first 'columns)) "
            "  (and (= (ref schema 'constraint-count) 1) "
            "       (and (= (ref analyze 'keyed-relation-count) 0) "
            "            (and (= (ref analyze 'unique-relation-count) 1) "
            "                 (and (= (ref analyze 'integrity-constraint-count) 1) "
            "                      (and (nil? (ref schema 'unique-column)) "
            "                           (and (= (car unique-columns) 'email) "
            "                                (and (= (car (cdr unique-columns)) 'dept) "
            "                                     (and (= (ref first 'kind) 'unique) "
            "                                          (and (= (car constraint-columns) 'email) "
            "                                               (and (= (car (cdr constraint-columns)) 'dept) "
            "                                                    (and (= (ref first 'unique) true) "
            "                                                         (= (ref first 'enforced) true)))))))))))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce schema/analyze surfaces expose composite unique integrity metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce schema/analyze surfaces expose composite unique integrity metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce schema/analyze surfaces expose composite unique integrity metadata");
            }
            (*fail)++;
        }
    }
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-reference (deduce 'open 'memory)) "
            "  (define [relation db-admin-reference] ref-users-admin (key id) (name)) "
            "  (define [relation db-admin-reference] ref-audit-admin (ref ref-users-admin id user-id) (action)) "
            "  (define schema (deduce/schema ref-audit-admin)) "
            "  (define analyze (deduce/analyze db-admin-reference)) "
            "  (define constraints (ref schema 'constraints)) "
            "  (define first (car constraints)) "
            "  (and "
            "    (= (ref schema 'constraint-count) 1) "
            "    (= (ref schema 'reference-constraint-count) 1) "
            "    (= (ref schema 'resolved-reference-constraint-count) 1) "
            "    (= (ref schema 'unresolved-reference-constraint-count) 0) "
            "    (= (ref analyze 'keyed-relation-count) 1) "
            "    (= (ref analyze 'reference-constraint-count) 1) "
            "    (= (ref analyze 'resolved-reference-constraint-count) 1) "
            "    (= (ref analyze 'unresolved-reference-constraint-count) 0) "
            "    (= (ref analyze 'integrity-constraint-count) 2) "
            "    (= (ref first 'kind) 'reference) "
            "    (= (car (ref first 'columns)) 'user-id) "
            "    (= (ref first 'referenced-relation) 'ref-users-admin) "
            "    (= (ref first 'referenced-column) 'id) "
            "    (= (ref first 'target-declared) true) "
            "    (= (ref first 'target-column-declared) true) "
            "    (= (ref first 'target-live) true) "
            "    (= (ref first 'target-unique) true) "
            "    (= (ref first 'resolved) true)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce schema/analyze surfaces expose reference integrity metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce schema/analyze surfaces expose reference integrity metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce schema/analyze surfaces expose reference integrity metadata");
            }
            (*fail)++;
        }
    }
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-reference-missing-rel (deduce 'open 'memory)) "
            "  (define [relation db-admin-reference-missing-rel] ref-audit-admin-missing-rel (ref ref-users-admin-missing-rel id user-id) (action)) "
            "  (define schema (deduce/schema ref-audit-admin-missing-rel)) "
            "  (define analyze (deduce/analyze db-admin-reference-missing-rel)) "
            "  (define first (car (ref schema 'constraints))) "
            "  (and "
            "    (= (ref schema 'reference-constraint-count) 1) "
            "    (= (ref schema 'resolved-reference-constraint-count) 0) "
            "    (= (ref schema 'unresolved-reference-constraint-count) 1) "
            "    (= (ref analyze 'reference-constraint-count) 1) "
            "    (= (ref analyze 'resolved-reference-constraint-count) 0) "
            "    (= (ref analyze 'unresolved-reference-constraint-count) 1) "
            "    (= (ref first 'target-declared) false) "
            "    (= (ref first 'target-column-declared) false) "
            "    (= (ref first 'target-live) false) "
            "    (= (ref first 'target-unique) false) "
            "    (= (ref first 'resolved) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce schema/analyze surfaces expose unresolved missing-target relation metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce schema/analyze surfaces expose unresolved missing-target relation metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce schema/analyze surfaces expose unresolved missing-target relation metadata");
            }
            (*fail)++;
        }
    }
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-reference-missing-col (deduce 'open 'memory)) "
            "  (define [relation db-admin-reference-missing-col] ref-users-admin-missing-col (key id) (name)) "
            "  (define [relation db-admin-reference-missing-col] ref-audit-admin-missing-col (ref ref-users-admin-missing-col missing-id user-id) (action)) "
            "  (define schema (deduce/schema ref-audit-admin-missing-col)) "
            "  (define analyze (deduce/analyze db-admin-reference-missing-col)) "
            "  (define first (car (ref schema 'constraints))) "
            "  (and "
            "    (= (ref schema 'reference-constraint-count) 1) "
            "    (= (ref schema 'resolved-reference-constraint-count) 0) "
            "    (= (ref schema 'unresolved-reference-constraint-count) 1) "
            "    (= (ref analyze 'reference-constraint-count) 1) "
            "    (= (ref analyze 'resolved-reference-constraint-count) 0) "
            "    (= (ref analyze 'unresolved-reference-constraint-count) 1) "
            "    (= (ref first 'target-declared) true) "
            "    (= (ref first 'target-column-declared) false) "
            "    (= (ref first 'target-live) true) "
            "    (= (ref first 'target-unique) false) "
            "    (= (ref first 'resolved) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce schema/analyze surfaces expose unresolved missing-target column metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce schema/analyze surfaces expose unresolved missing-target column metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce schema/analyze surfaces expose unresolved missing-target column metadata");
            }
            (*fail)++;
        }
    }
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-reference-nonunique (deduce 'open 'memory)) "
            "  (define [relation db-admin-reference-nonunique] ref-users-admin-nonunique (id) (email)) "
            "  (define [relation db-admin-reference-nonunique] ref-audit-admin-nonunique (ref ref-users-admin-nonunique email user-email) (action)) "
            "  (define schema (deduce/schema ref-audit-admin-nonunique)) "
            "  (define analyze (deduce/analyze db-admin-reference-nonunique)) "
            "  (define first (car (ref schema 'constraints))) "
            "  (and "
            "    (= (ref schema 'reference-constraint-count) 1) "
            "    (= (ref schema 'resolved-reference-constraint-count) 0) "
            "    (= (ref schema 'unresolved-reference-constraint-count) 1) "
            "    (= (ref analyze 'reference-constraint-count) 1) "
            "    (= (ref analyze 'resolved-reference-constraint-count) 0) "
            "    (= (ref analyze 'unresolved-reference-constraint-count) 1) "
            "    (= (ref first 'target-declared) true) "
            "    (= (ref first 'target-column-declared) true) "
            "    (= (ref first 'target-live) true) "
            "    (= (ref first 'target-unique) false) "
            "    (= (ref first 'resolved) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce schema/analyze surfaces expose unresolved non-unique reference target metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce schema/analyze surfaces expose unresolved non-unique reference target metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce schema/analyze surfaces expose unresolved non-unique reference target metadata");
            }
            (*fail)++;
        }
    }
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-reference-dropped (deduce 'open 'memory)) "
            "  (define [relation db-admin-reference-dropped] ref-users-admin-dropped (key id) (name)) "
            "  (define [relation db-admin-reference-dropped] ref-audit-admin-dropped (ref ref-users-admin-dropped id user-id) (action)) "
            "  (deduce 'drop! ref-users-admin-dropped) "
            "  (define schema (deduce/schema ref-audit-admin-dropped)) "
            "  (define analyze (deduce/analyze db-admin-reference-dropped)) "
            "  (define first (car (ref schema 'constraints))) "
            "  (and "
            "    (= (ref schema 'reference-constraint-count) 1) "
            "    (= (ref schema 'resolved-reference-constraint-count) 0) "
            "    (= (ref schema 'unresolved-reference-constraint-count) 1) "
            "    (= (ref analyze 'reference-constraint-count) 1) "
            "    (= (ref analyze 'resolved-reference-constraint-count) 0) "
            "    (= (ref analyze 'unresolved-reference-constraint-count) 1) "
            "    (= (ref first 'target-declared) true) "
            "    (= (ref first 'target-column-declared) true) "
            "    (= (ref first 'target-live) false) "
            "    (= (ref first 'target-unique) false) "
            "    (= (ref first 'resolved) false)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce schema/analyze surfaces expose unresolved dropped-target reference metadata");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce schema/analyze surfaces expose unresolved dropped-target reference metadata (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce schema/analyze surfaces expose unresolved dropped-target reference metadata");
            }
            (*fail)++;
        }
    }
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-integrity-stats (deduce 'open 'memory)) "
            "  (define [relation db-admin-integrity-stats] admin-key-violations (key id) (name)) "
            "  (define [relation db-admin-integrity-stats] admin-unique-violations (id) (unique email)) "
            "  (define [relation db-admin-integrity-stats] admin-users-violations (key id) (name)) "
            "  (define [relation db-admin-integrity-stats] admin-audit-violations (ref admin-users-violations id user-id) (action)) "
            "  (deduce 'fact! admin-key-violations 1 \"Alice\") "
            "  (deduce 'fact! admin-unique-violations 1 \"alice@x\") "
            "  (handle (deduce 'fact! admin-key-violations 1 \"Bob\") (raise msg msg)) "
            "  (handle (deduce 'fact! admin-unique-violations 2 \"alice@x\") (raise msg msg)) "
            "  (handle (deduce 'fact! admin-audit-violations 42 \"login\") (raise msg msg)) "
            "  (define key-stats (deduce/stats admin-key-violations)) "
            "  (define unique-stats (deduce/stats admin-unique-violations)) "
            "  (define audit-stats (deduce/stats admin-audit-violations)) "
            "  (define analyze (deduce/analyze db-admin-integrity-stats)) "
            "  (and (= (ref key-stats 'integrity-violation-count) 1) "
            "       (and (= (ref key-stats 'key-integrity-violation-count) 1) "
            "            (and (= (ref key-stats 'unique-integrity-violation-count) 0) "
            "                 (and (= (ref key-stats 'reference-integrity-violation-count) 0) "
            "                      (and (= (ref key-stats 'check-integrity-violation-count) 0) "
            "                      (and (= (ref key-stats 'last-integrity-violation-code) 'deduce/integrity-key-conflict) "
            "                           (and (= (ref unique-stats 'integrity-violation-count) 1) "
            "                                (and (= (ref unique-stats 'key-integrity-violation-count) 0) "
            "                                     (and (= (ref unique-stats 'unique-integrity-violation-count) 1) "
            "                                          (and (= (ref unique-stats 'reference-integrity-violation-count) 0) "
            "                                               (and (= (ref unique-stats 'check-integrity-violation-count) 0) "
            "                                               (and (= (ref unique-stats 'last-integrity-violation-code) 'deduce/integrity-unique-conflict) "
            "                                                    (and (= (ref audit-stats 'integrity-violation-count) 1) "
            "                                                         (and (= (ref audit-stats 'key-integrity-violation-count) 0) "
            "                                                              (and (= (ref audit-stats 'unique-integrity-violation-count) 0) "
            "                                                                   (and (= (ref audit-stats 'reference-integrity-violation-count) 1) "
            "                                                                        (and (= (ref audit-stats 'check-integrity-violation-count) 0) "
            "                                                                        (and (= (ref audit-stats 'last-integrity-violation-code) 'deduce/integrity-reference-missing) "
            "                                                                             (and (= (ref analyze 'integrity-violation-count) 3) "
            "                                                                                  (and (= (ref analyze 'key-integrity-violation-count) 1) "
            "                                                                                       (and (= (ref analyze 'unique-integrity-violation-count) 1) "
            "                                                                                            (and (= (ref analyze 'reference-integrity-violation-count) 1) "
            "                                                                                                 (and (= (ref analyze 'check-integrity-violation-count) 0) "
            "                                                                                                 (and (= (ref analyze 'last-integrity-violation-code) 'deduce/integrity-reference-missing) "
            "                                                                                                      (= (ref analyze 'last-integrity-violation-relation) 'admin-audit-violations))))))))))))))))))))))))))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats/analyze surfaces expose integrity violation reporting");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats/analyze surfaces expose integrity violation reporting (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats/analyze surfaces expose integrity violation reporting");
            }
            (*fail)++;
        }
    }
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-integrity-history (deduce 'open 'memory)) "
            "  (define [relation db-admin-integrity-history] history-key (key id) (name)) "
            "  (define [relation db-admin-integrity-history] history-unique (id) (unique email)) "
            "  (define [relation db-admin-integrity-history] history-users (key id) (name)) "
            "  (define [relation db-admin-integrity-history] history-audit (ref history-users id user-id) (action)) "
            "  (deduce 'fact! history-key 1 \"Alice\") "
            "  (deduce 'fact! history-unique 1 \"alice@x\") "
            "  (deduce 'fact! history-users 7 \"Seven\") "
            "  (deduce 'fact! history-audit 7 \"login\") "
            "  (handle (deduce 'fact! history-key 1 \"Bob\") (raise msg msg)) "
            "  (handle (deduce 'fact! history-unique 2 \"alice@x\") (raise msg msg)) "
            "  (handle (deduce 'fact! history-audit 42 \"missing\") (raise msg msg)) "
            "  (handle (deduce 'retract! history-users 7 \"Seven\") (raise msg msg)) "
            "  (define user-stats (deduce/stats history-users)) "
            "  (define analyze (deduce/analyze db-admin-integrity-history)) "
            "  (define user-history (ref user-stats 'recent-integrity-violations)) "
            "  (define db-history (ref analyze 'recent-integrity-violations)) "
            "  (and (= (length user-history) 1) "
            "       (= (ref (car user-history) 'code) 'deduce/integrity-reference-target-in-use) "
            "       (= (ref (car user-history) 'violation-class) 'reference) "
            "       (= (ref (car user-history) 'relation) 'history-users) "
            "       (= (length db-history) 4) "
            "       (= (ref (car db-history) 'code) 'deduce/integrity-reference-target-in-use) "
            "       (= (ref (car (cdr db-history)) 'code) 'deduce/integrity-reference-missing) "
            "       (= (ref (car (cdr (cdr db-history))) 'code) 'deduce/integrity-unique-conflict) "
            "       (= (ref (car (cdr (cdr (cdr db-history)))) 'code) 'deduce/integrity-key-conflict) "
            "       (> (ref (car db-history) 'sequence) (ref (car (cdr db-history)) 'sequence)) "
            "       (> (ref (car (cdr db-history)) 'sequence) (ref (car (cdr (cdr db-history))) 'sequence)) "
            "       (> (ref (car (cdr (cdr db-history))) 'sequence) (ref (car (cdr (cdr (cdr db-history)))) 'sequence))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats/analyze surfaces expose bounded integrity violation history");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats/analyze surfaces expose bounded integrity violation history (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats/analyze surfaces expose bounded integrity violation history");
            }
            (*fail)++;
        }
    }
    {
        EvalResult r = run(
            "(block "
            "  (define db-admin-check-history (deduce 'open 'memory)) "
            "  (define [relation db-admin-check-history] history-check (id) (check int? amount)) "
            "  (handle (deduce 'fact! history-check 1 \"oops\") (raise msg msg)) "
            "  (define stats (deduce/stats history-check)) "
            "  (define analyze (deduce/analyze db-admin-check-history)) "
            "  (define rel-history (ref stats 'recent-integrity-violations)) "
            "  (define db-history (ref analyze 'recent-integrity-violations)) "
            "  (and "
            "    (= (ref stats 'integrity-violation-count) 1) "
            "    (= (ref stats 'check-integrity-violation-count) 1) "
            "    (= (ref stats 'key-integrity-violation-count) 0) "
            "    (= (ref stats 'unique-integrity-violation-count) 0) "
            "    (= (ref stats 'reference-integrity-violation-count) 0) "
            "    (= (ref stats 'last-integrity-violation-code) 'deduce/integrity-check-failed) "
            "    (= (length rel-history) 1) "
            "    (= (ref (car rel-history) 'code) 'deduce/integrity-check-failed) "
            "    (= (ref (car rel-history) 'violation-class) 'check) "
            "    (= (ref (car rel-history) 'relation) 'history-check) "
            "    (= (ref analyze 'integrity-violation-count) 1) "
            "    (= (ref analyze 'check-integrity-violation-count) 1) "
            "    (= (ref analyze 'key-integrity-violation-count) 0) "
            "    (= (ref analyze 'unique-integrity-violation-count) 0) "
            "    (= (ref analyze 'reference-integrity-violation-count) 0) "
            "    (= (ref analyze 'last-integrity-violation-code) 'deduce/integrity-check-failed) "
            "    (= (ref analyze 'last-integrity-violation-relation) 'history-check) "
            "    (= (length db-history) 1) "
            "    (= (ref (car db-history) 'code) 'deduce/integrity-check-failed) "
            "    (= (ref (car db-history) 'violation-class) 'check) "
            "    (= (ref (car db-history) 'relation) 'history-check)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce stats/analyze surfaces expose check integrity violation history");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce stats/analyze surfaces expose check integrity violation history (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce stats/analyze surfaces expose check integrity violation history");
            }
            (*fail)++;
        }
    }
}

fn void run_deduce_aggregate_rule_tests(Interp* interp, int* pass, int* fail) {
    if (deduce_query_filter_try_run_targeted_why_result(interp, pass, fail)) return;

    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = make_symbol(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;

        SymbolId rel_src = interp.symbols.intern("agg_src");
        SymbolId rel_count = interp.symbols.intern("agg_count");
        SymbolId rel_sum = interp.symbols.intern("agg_sum");
        SymbolId rel_min = interp.symbols.intern("agg_min");
        SymbolId rel_max = interp.symbols.intern("agg_max");
        SymbolId col_dept = interp.symbols.intern("dept");
        SymbolId col_amount = interp.symbols.intern("amount");
        SymbolId col_n = interp.symbols.intern("n");
        SymbolId col_total = interp.symbols.intern("total");
        SymbolId col_low = interp.symbols.intern("low");
        SymbolId col_high = interp.symbols.intern("high");
        SymbolId var_dept = interp.symbols.intern("?dept");
        SymbolId var_amount = interp.symbols.intern("?amount");
        SymbolId sym_count = interp.symbols.intern("count");
        SymbolId sym_sum = interp.symbols.intern("sum");
        SymbolId sym_min = interp.symbols.intern("min");
        SymbolId sym_max = interp.symbols.intern("max");

        Value* src = null;
        Value* out_count = null;
        Value* out_sum = null;
        Value* out_min = null;
        Value* out_max = null;
        if (ok) {
            src = setup_deduce_relation_for_db(interp, db_val, rel_src, col_dept, col_amount);
            out_count = setup_deduce_relation_for_db(interp, db_val, rel_count, col_dept, col_n);
            out_sum = setup_deduce_relation_for_db(interp, db_val, rel_sum, col_dept, col_total);
            out_min = setup_deduce_relation_for_db(interp, db_val, rel_min, col_dept, col_low);
            out_max = setup_deduce_relation_for_db(interp, db_val, rel_max, col_dept, col_high);
            ok = src != null && src.tag == FFI_HANDLE &&
                 out_count != null && out_count.tag == FFI_HANDLE &&
                 out_sum != null && out_sum.tag == FFI_HANDLE &&
                 out_min != null && out_min.tag == FFI_HANDLE &&
                 out_max != null && out_max.tag == FFI_HANDLE;
        }

        if (ok) {
            char[][] depts = { "eng", "eng", "sales", "sales" };
            long[] amounts = { 10, 15, 3, 9 };
            for (usz i = 0; i < depts.len; i++) {
                Value*[3] fact_args;
                fact_args[0] = src;
                fact_args[1] = make_string(interp, depts[i]);
                fact_args[2] = make_int(interp, amounts[i]);
                Value* inserted = prim_assert_fact(fact_args[..], null, interp);
                if (inserted == null || inserted.tag == ERROR) {
                    ok = false;
                    break;
                }
            }
        }

        if (ok) {
            Value* count_head =
                make_cons(interp,
                    deduce_rule_test_make_symbol_value(interp, rel_count),
                    make_cons(interp,
                        deduce_rule_test_make_symbol_value(interp, var_dept),
                        make_cons(interp,
                            make_cons(interp,
                                deduce_rule_test_make_symbol_value(interp, sym_count),
                                make_cons(interp,
                                    deduce_rule_test_make_symbol_value(interp, var_amount),
                                    make_nil(interp))),
                            make_nil(interp))));
            Value* sum_head =
                make_cons(interp,
                    deduce_rule_test_make_symbol_value(interp, rel_sum),
                    make_cons(interp,
                        deduce_rule_test_make_symbol_value(interp, var_dept),
                        make_cons(interp,
                            make_cons(interp,
                                deduce_rule_test_make_symbol_value(interp, sym_sum),
                                make_cons(interp,
                                    deduce_rule_test_make_symbol_value(interp, var_amount),
                                    make_nil(interp))),
                            make_nil(interp))));
            Value* min_head =
                make_cons(interp,
                    deduce_rule_test_make_symbol_value(interp, rel_min),
                    make_cons(interp,
                        deduce_rule_test_make_symbol_value(interp, var_dept),
                        make_cons(interp,
                            make_cons(interp,
                                deduce_rule_test_make_symbol_value(interp, sym_min),
                                make_cons(interp,
                                    deduce_rule_test_make_symbol_value(interp, var_amount),
                                    make_nil(interp))),
                            make_nil(interp))));
            Value* max_head =
                make_cons(interp,
                    deduce_rule_test_make_symbol_value(interp, rel_max),
                    make_cons(interp,
                        deduce_rule_test_make_symbol_value(interp, var_dept),
                        make_cons(interp,
                            make_cons(interp,
                                deduce_rule_test_make_symbol_value(interp, sym_max),
                                make_cons(interp,
                                    deduce_rule_test_make_symbol_value(interp, var_amount),
                                    make_nil(interp))),
                            make_nil(interp))));
            Value* body = deduce_rule_test_make_atom2(
                interp,
                rel_src,
                deduce_rule_test_make_symbol_value(interp, var_dept),
                deduce_rule_test_make_symbol_value(interp, var_amount)
            );

            Value*[3] count_rule_args = { db_val, count_head, body };
            Value*[3] sum_rule_args = { db_val, sum_head, body };
            Value*[3] min_rule_args = { db_val, min_head, body };
            Value*[3] max_rule_args = { db_val, max_head, body };
            Value* count_rule_ok = prim_deduce_rule(count_rule_args[..], null, interp);
            Value* sum_rule_ok = prim_deduce_rule(sum_rule_args[..], null, interp);
            Value* min_rule_ok = prim_deduce_rule(min_rule_args[..], null, interp);
            Value* max_rule_ok = prim_deduce_rule(max_rule_args[..], null, interp);
            ok = count_rule_ok != null && count_rule_ok.tag == VOID &&
                 sum_rule_ok != null && sum_rule_ok.tag == VOID &&
                 min_rule_ok != null && min_rule_ok.tag == VOID &&
                 max_rule_ok != null && max_rule_ok.tag == VOID;
        }

        Value* analyzed = null;
        Value* count_rows = null;
        Value* sum_rows = null;
        Value* min_rows = null;
        Value* max_rows = null;
        if (ok) {
            Value*[1] analyze_args;
            analyze_args[0] = db_val;
            analyzed = prim_deduce_analyze(analyze_args[..], null, interp);

            Value*[1] scan_args = { out_count };
            count_rows = prim_deduce_scan(scan_args[..], null, interp);
            scan_args[0] = out_sum;
            sum_rows = prim_deduce_scan(scan_args[..], null, interp);
            scan_args[0] = out_min;
            min_rows = prim_deduce_scan(scan_args[..], null, interp);
            scan_args[0] = out_max;
            max_rows = prim_deduce_scan(scan_args[..], null, interp);

            ok = analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null;
        }

        if (ok) {
            Value* analyze_status = deduce_query_test_dict_get(interp, analyzed, "status");
            Value* eng = make_string(interp, "eng");
            Value* sales = make_string(interp, "sales");
            Value* count_eng_row = deduce_query_test_find_row_by_value(interp, count_rows, "dept", eng);
            Value* count_sales_row = deduce_query_test_find_row_by_value(interp, count_rows, "dept", sales);
            Value* sum_eng_row = deduce_query_test_find_row_by_value(interp, sum_rows, "dept", eng);
            Value* sum_sales_row = deduce_query_test_find_row_by_value(interp, sum_rows, "dept", sales);
            Value* min_eng_row = deduce_query_test_find_row_by_value(interp, min_rows, "dept", eng);
            Value* min_sales_row = deduce_query_test_find_row_by_value(interp, min_rows, "dept", sales);
            Value* max_eng_row = deduce_query_test_find_row_by_value(interp, max_rows, "dept", eng);
            Value* max_sales_row = deduce_query_test_find_row_by_value(interp, max_rows, "dept", sales);

            Value* count_eng = deduce_query_test_dict_get(interp, count_eng_row, "n");
            Value* count_sales = deduce_query_test_dict_get(interp, count_sales_row, "n");
            Value* sum_eng = deduce_query_test_dict_get(interp, sum_eng_row, "total");
            Value* sum_sales = deduce_query_test_dict_get(interp, sum_sales_row, "total");
            Value* min_eng = deduce_query_test_dict_get(interp, min_eng_row, "low");
            Value* min_sales = deduce_query_test_dict_get(interp, min_sales_row, "low");
            Value* max_eng = deduce_query_test_dict_get(interp, max_eng_row, "high");
            Value* max_sales = deduce_query_test_dict_get(interp, max_sales_row, "high");

            ok = analyze_status != null && analyze_status.tag == SYMBOL && analyze_status.sym_val == interp.symbols.intern("ok") &&
                 count_eng != null && count_eng.tag == INT && count_eng.int_val == 2 &&
                 count_sales != null && count_sales.tag == INT && count_sales.int_val == 2 &&
                 sum_eng != null && sum_eng.tag == INT && sum_eng.int_val == 25 &&
                 sum_sales != null && sum_sales.tag == INT && sum_sales.int_val == 12 &&
                 min_eng != null && min_eng.tag == INT && min_eng.int_val == 10 &&
                 min_sales != null && min_sales.tag == INT && min_sales.int_val == 3 &&
                 max_eng != null && max_eng.tag == INT && max_eng.int_val == 15 &&
                 max_sales != null && max_sales.tag == INT && max_sales.int_val == 9;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce aggregate rules execute grouped count/sum/min/max");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce aggregate rules execute grouped count/sum/min/max");
            (*fail)++;
        }
    }
}

fn void run_deduce_recursive_aggregate_rule_tests(Interp* interp, int* pass, int* fail) {
    if (deduce_query_filter_try_run_targeted_why_result(interp, pass, fail)) return;

    Value* db_val = null;
    Value* out_rel = null;
    usz expected_rows = 0;
    bool ok = deduce_query_bench_seed_recursive_aggregate_db(interp, 4, &db_val, &out_rel, &expected_rows);

    Value* analyzed = null;
    Value* rows = null;
    if (ok) {
        Value*[2] analyze_args;
        analyze_args[0] = db_val;
        analyze_args[1] = make_symbol(interp, interp.symbols.intern("semi-naive"));
        analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
        ok = analyzed != null && analyzed.tag == HASHMAP && analyzed.hashmap_val != null;
    }

    if (ok) {
        Value*[1] scan_args;
        scan_args[0] = out_rel;
        rows = prim_deduce_scan(scan_args[..], null, interp);

        Value* status = deduce_query_test_dict_get(interp, analyzed, "status");
        Value* engine = deduce_query_test_dict_get(interp, analyzed, "execution-engine");
        Value* row0 = deduce_query_test_find_row_by_value(interp, rows, "node", make_int(interp, 0));
        Value* row1 = deduce_query_test_find_row_by_value(interp, rows, "node", make_int(interp, 1));
        Value* row2 = deduce_query_test_find_row_by_value(interp, rows, "node", make_int(interp, 2));
        Value* row3 = deduce_query_test_find_row_by_value(interp, rows, "node", make_int(interp, 3));
        Value* total0 = deduce_query_test_dict_get(interp, row0, "total");
        Value* total1 = deduce_query_test_dict_get(interp, row1, "total");
        Value* total2 = deduce_query_test_dict_get(interp, row2, "total");
        Value* total3 = deduce_query_test_dict_get(interp, row3, "total");
        Value*[1] count_args;
        count_args[0] = out_rel;
        Value* row_count = prim_deduce_count(count_args[..], null, interp);

        ok = status != null && status.tag == SYMBOL && status.sym_val == interp.symbols.intern("ok") &&
             engine != null && engine.tag == SYMBOL && engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
             row_count != null && row_count.tag == INT && row_count.int_val == (long)expected_rows &&
             total0 != null && total0.tag == INT && total0.int_val == 4 &&
             total1 != null && total1.tag == INT && total1.int_val == 3 &&
             total2 != null && total2.tag == INT && total2.int_val == 2 &&
             total3 != null && total3.tag == INT && total3.int_val == 1;
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce recursive aggregate sum runs on semi-naive SCC and remains exact");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce recursive aggregate sum runs on semi-naive SCC and remains exact");
        (*fail)++;
    }

    {
        Value* mixed_db = null;
        Value* mixed_score = null;
        Value* mixed_chosen = null;
        usz mixed_expected_rows = 0;
        bool mixed_ok = deduce_query_bench_seed_recursive_mixed_aggregate_db(
            interp,
            4,
            &mixed_db,
            &mixed_score,
            &mixed_chosen,
            &mixed_expected_rows
        );

        Value* mixed_analyzed = null;
        Value* mixed_score_rows = null;
        Value* mixed_chosen_rows = null;
        if (mixed_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = mixed_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("semi-naive"));
            mixed_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            mixed_ok = mixed_analyzed != null && mixed_analyzed.tag == HASHMAP && mixed_analyzed.hashmap_val != null;
        }

        if (mixed_ok) {
            Value*[1] scan_score_args;
            scan_score_args[0] = mixed_score;
            mixed_score_rows = prim_deduce_scan(scan_score_args[..], null, interp);

            Value*[1] scan_chosen_args;
            scan_chosen_args[0] = mixed_chosen;
            mixed_chosen_rows = prim_deduce_scan(scan_chosen_args[..], null, interp);

            Value* status = deduce_query_test_dict_get(interp, mixed_analyzed, "status");
            Value* engine = deduce_query_test_dict_get(interp, mixed_analyzed, "execution-engine");
            Value* score_row0 = deduce_query_test_find_row_by_value(interp, mixed_score_rows, "node", make_int(interp, 0));
            Value* score_row1 = deduce_query_test_find_row_by_value(interp, mixed_score_rows, "node", make_int(interp, 1));
            Value* score_row2 = deduce_query_test_find_row_by_value(interp, mixed_score_rows, "node", make_int(interp, 2));
            Value* score_row3 = deduce_query_test_find_row_by_value(interp, mixed_score_rows, "node", make_int(interp, 3));
            Value* chosen_row0 = deduce_query_test_find_row_by_value(interp, mixed_chosen_rows, "node", make_int(interp, 0));
            Value* score_total0 = deduce_query_test_dict_get(interp, score_row0, "total");
            Value* score_total1 = deduce_query_test_dict_get(interp, score_row1, "total");
            Value* score_total2 = deduce_query_test_dict_get(interp, score_row2, "total");
            Value* score_total3 = deduce_query_test_dict_get(interp, score_row3, "total");
            Value* chosen_total0 = deduce_query_test_dict_get(interp, chosen_row0, "total");
            Value*[1] count_score_args;
            count_score_args[0] = mixed_score;
            Value* score_count = prim_deduce_count(count_score_args[..], null, interp);
            Value*[1] count_chosen_args;
            count_chosen_args[0] = mixed_chosen;
            Value* chosen_count = prim_deduce_count(count_chosen_args[..], null, interp);

            mixed_ok = status != null && status.tag == SYMBOL && status.sym_val == interp.symbols.intern("ok") &&
                       engine != null && engine.tag == SYMBOL && engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                       score_count != null && score_count.tag == INT && score_count.int_val == (long)mixed_expected_rows &&
                       chosen_count != null && chosen_count.tag == INT && chosen_count.int_val == (long)mixed_expected_rows &&
                       score_total0 != null && score_total0.tag == INT && score_total0.int_val == 4 &&
                       score_total1 != null && score_total1.tag == INT && score_total1.int_val == 3 &&
                       score_total2 != null && score_total2.tag == INT && score_total2.int_val == 2 &&
                       score_total3 != null && score_total3.tag == INT && score_total3.int_val == 1 &&
                       chosen_total0 != null && chosen_total0.tag == INT && chosen_total0.int_val == 4;
        }

        if (mixed_ok) {
            emit_pass_literal("[PASS] deduce mixed positive recursive aggregate SCCs run on semi-naive-scc and stay exact");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce mixed positive recursive aggregate SCCs run on semi-naive-scc and stay exact");
            (*fail)++;
        }
    }

    {
        Value* pure_db = null;
        Value* pure_out = null;
        usz pure_expected_rows = 0;
        bool pure_ok = deduce_query_bench_seed_recursive_aggregate_db(
            interp,
            4,
            &pure_db,
            &pure_out,
            &pure_expected_rows
        );

        Value* pure_analyzed = null;
        if (pure_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = pure_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("naive"));
            pure_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            pure_ok = pure_analyzed != null && pure_analyzed.tag == HASHMAP && pure_analyzed.hashmap_val != null;
        }

        if (pure_ok) {
            Value* engine = deduce_query_test_dict_get(interp, pure_analyzed, "execution-engine");
            Value* requested = deduce_query_test_dict_get(interp, pure_analyzed, "requested-execution-engine");
            Value* fallback = deduce_query_test_dict_get(interp, pure_analyzed, "recursive-aggregate-naive-fallback");
            Value* routed = deduce_query_test_dict_get(
                interp,
                pure_analyzed,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value*[1] count_args;
            count_args[0] = pure_out;
            Value* row_count = prim_deduce_count(count_args[..], null, interp);

            pure_ok = engine != null && engine.tag == SYMBOL &&
                      engine.sym_val == interp.symbols.intern("naive-scc") &&
                      requested != null && requested.tag == SYMBOL &&
                      requested.sym_val == interp.symbols.intern("naive-scc") &&
                      fallback != null && fallback.tag == NIL &&
                      routed != null && routed.tag == NIL &&
                      row_count != null && row_count.tag == INT &&
                      row_count.int_val == (long)pure_expected_rows;
        }

        if (pure_ok) {
            emit_pass_literal("[PASS] deduce analyze keeps naive pure positive recursive aggregates off mixed-route diagnostics");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce analyze keeps naive pure positive recursive aggregates off mixed-route diagnostics");
            (*fail)++;
        }
    }

    {
        Value* mixed_db = null;
        Value* mixed_score = null;
        Value* mixed_chosen = null;
        usz mixed_expected_rows = 0;
        bool mixed_ok = deduce_query_bench_seed_recursive_mixed_aggregate_db(
            interp,
            4,
            &mixed_db,
            &mixed_score,
            &mixed_chosen,
            &mixed_expected_rows
        );

        Value* mixed_analyzed = null;
        if (mixed_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = mixed_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("naive"));
            mixed_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            mixed_ok = mixed_analyzed != null && mixed_analyzed.tag == HASHMAP && mixed_analyzed.hashmap_val != null;
        }

        if (mixed_ok) {
            Value* engine = deduce_query_test_dict_get(interp, mixed_analyzed, "execution-engine");
            Value* requested = deduce_query_test_dict_get(interp, mixed_analyzed, "requested-execution-engine");
            Value* fallback = deduce_query_test_dict_get(interp, mixed_analyzed, "recursive-aggregate-naive-fallback");
            Value* routed = deduce_query_test_dict_get(
                interp,
                mixed_analyzed,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value*[1] count_score_args;
            count_score_args[0] = mixed_score;
            Value* score_count = prim_deduce_count(count_score_args[..], null, interp);
            Value*[1] count_chosen_args;
            count_chosen_args[0] = mixed_chosen;
            Value* chosen_count = prim_deduce_count(count_chosen_args[..], null, interp);

            mixed_ok = engine != null && engine.tag == SYMBOL &&
                       engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                       requested != null && requested.tag == SYMBOL &&
                       requested.sym_val == interp.symbols.intern("naive-scc") &&
                       fallback != null && fallback.tag == NIL &&
                       routed != null && routed.tag == SYMBOL &&
                       routed.sym_val == interp.sym_true &&
                       score_count != null && score_count.tag == INT &&
                       score_count.int_val == (long)mixed_expected_rows &&
                       chosen_count != null && chosen_count.tag == INT &&
                       chosen_count.int_val == (long)mixed_expected_rows;
        }

        if (mixed_ok) {
            emit_pass_literal("[PASS] deduce analyze routes naive mixed recursive aggregate SCCs to semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce analyze routes naive mixed recursive aggregate SCCs to semi-naive-scc");
            (*fail)++;
        }
    }

    {
        Value* neg_db = null;
        Value* neg_out = null;
        usz neg_expected_rows = 0;
        bool neg_ok = deduce_query_bench_seed_recursive_aggregate_negated_db(
            interp,
            4,
            &neg_db,
            &neg_out,
            &neg_expected_rows
        );

        Value* neg_analyzed = null;
        Value* neg_rows = null;
        if (neg_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = neg_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("semi-naive"));
            neg_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            neg_ok = neg_analyzed != null && neg_analyzed.tag == HASHMAP && neg_analyzed.hashmap_val != null;
        }

        if (neg_ok) {
            Value*[1] scan_args;
            scan_args[0] = neg_out;
            neg_rows = prim_deduce_scan(scan_args[..], null, interp);

            Value* status = deduce_query_test_dict_get(interp, neg_analyzed, "status");
            Value* engine = deduce_query_test_dict_get(interp, neg_analyzed, "execution-engine");
            Value* row0 = deduce_query_test_find_row_by_value(interp, neg_rows, "node", make_int(interp, 0));
            Value* row1 = deduce_query_test_find_row_by_value(interp, neg_rows, "node", make_int(interp, 1));
            Value* row2 = deduce_query_test_find_row_by_value(interp, neg_rows, "node", make_int(interp, 2));
            Value* row3 = deduce_query_test_find_row_by_value(interp, neg_rows, "node", make_int(interp, 3));
            Value* total0 = deduce_query_test_dict_get(interp, row0, "total");
            Value* total1 = deduce_query_test_dict_get(interp, row1, "total");
            Value* total2 = deduce_query_test_dict_get(interp, row2, "total");
            Value*[1] count_args;
            count_args[0] = neg_out;
            Value* row_count = prim_deduce_count(count_args[..], null, interp);

            neg_ok = status != null && status.tag == SYMBOL && status.sym_val == interp.symbols.intern("ok") &&
                     engine != null && engine.tag == SYMBOL && engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                     row_count != null && row_count.tag == INT && row_count.int_val == (long)neg_expected_rows &&
                     total0 != null && total0.tag == INT && total0.int_val == 3 &&
                     total1 != null && total1.tag == INT && total1.int_val == 2 &&
                     total2 != null && total2.tag == INT && total2.int_val == 1 &&
                     row3 == null;
        }

        if (neg_ok) {
            emit_pass_literal("[PASS] deduce recursive aggregate SCCs with lower-stratum negation run on semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce recursive aggregate SCCs with lower-stratum negation run on semi-naive-scc");
            (*fail)++;
        }
    }

    {
        Value* mixed_neg_db = null;
        Value* mixed_neg_score = null;
        Value* mixed_neg_chosen = null;
        usz expected_score_rows = 0;
        usz expected_chosen_rows = 0;
        bool mixed_neg_ok = deduce_query_bench_seed_recursive_mixed_aggregate_negated_db(
            interp,
            4,
            &mixed_neg_db,
            &mixed_neg_score,
            &mixed_neg_chosen,
            &expected_score_rows,
            &expected_chosen_rows
        );

        Value* mixed_neg_analyzed = null;
        Value* mixed_neg_score_rows = null;
        Value* mixed_neg_chosen_rows = null;
        if (mixed_neg_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = mixed_neg_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("semi-naive"));
            mixed_neg_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            mixed_neg_ok = mixed_neg_analyzed != null && mixed_neg_analyzed.tag == HASHMAP && mixed_neg_analyzed.hashmap_val != null;
        }

        if (mixed_neg_ok) {
            Value*[1] scan_score_args;
            scan_score_args[0] = mixed_neg_score;
            mixed_neg_score_rows = prim_deduce_scan(scan_score_args[..], null, interp);

            Value*[1] scan_chosen_args;
            scan_chosen_args[0] = mixed_neg_chosen;
            mixed_neg_chosen_rows = prim_deduce_scan(scan_chosen_args[..], null, interp);

            Value* status = deduce_query_test_dict_get(interp, mixed_neg_analyzed, "status");
            Value* engine = deduce_query_test_dict_get(interp, mixed_neg_analyzed, "execution-engine");
            Value* score_row0 = deduce_query_test_find_row_by_value(interp, mixed_neg_score_rows, "node", make_int(interp, 0));
            Value* score_row1 = deduce_query_test_find_row_by_value(interp, mixed_neg_score_rows, "node", make_int(interp, 1));
            Value* score_row2 = deduce_query_test_find_row_by_value(interp, mixed_neg_score_rows, "node", make_int(interp, 2));
            Value* score_row3 = deduce_query_test_find_row_by_value(interp, mixed_neg_score_rows, "node", make_int(interp, 3));
            Value* chosen_row0 = deduce_query_test_find_row_by_value(interp, mixed_neg_chosen_rows, "node", make_int(interp, 0));
            Value* chosen_row1 = deduce_query_test_find_row_by_value(interp, mixed_neg_chosen_rows, "node", make_int(interp, 1));
            Value* chosen_row2 = deduce_query_test_find_row_by_value(interp, mixed_neg_chosen_rows, "node", make_int(interp, 2));
            Value* chosen_row3 = deduce_query_test_find_row_by_value(interp, mixed_neg_chosen_rows, "node", make_int(interp, 3));
            Value* score_total0 = deduce_query_test_dict_get(interp, score_row0, "total");
            Value* score_total1 = deduce_query_test_dict_get(interp, score_row1, "total");
            Value* score_total2 = deduce_query_test_dict_get(interp, score_row2, "total");
            Value* score_total3 = deduce_query_test_dict_get(interp, score_row3, "total");
            Value* chosen_total0 = deduce_query_test_dict_get(interp, chosen_row0, "total");
            Value* chosen_total1 = deduce_query_test_dict_get(interp, chosen_row1, "total");
            Value* chosen_total2 = deduce_query_test_dict_get(interp, chosen_row2, "total");
            Value*[1] count_score_args;
            count_score_args[0] = mixed_neg_score;
            Value* score_count = prim_deduce_count(count_score_args[..], null, interp);
            Value*[1] count_chosen_args;
            count_chosen_args[0] = mixed_neg_chosen;
            Value* chosen_count = prim_deduce_count(count_chosen_args[..], null, interp);

            mixed_neg_ok =
                status != null && status.tag == SYMBOL && status.sym_val == interp.symbols.intern("ok") &&
                engine != null && engine.tag == SYMBOL && engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                score_count != null && score_count.tag == INT && score_count.int_val == (long)expected_score_rows &&
                chosen_count != null && chosen_count.tag == INT && chosen_count.int_val == (long)expected_chosen_rows &&
                score_total0 != null && score_total0.tag == INT && score_total0.int_val == 3 &&
                score_total1 != null && score_total1.tag == INT && score_total1.int_val == 2 &&
                score_total2 != null && score_total2.tag == INT && score_total2.int_val == 1 &&
                score_total3 != null && score_total3.tag == INT && score_total3.int_val == 1 &&
                chosen_total0 != null && chosen_total0.tag == INT && chosen_total0.int_val == 3 &&
                chosen_total1 != null && chosen_total1.tag == INT && chosen_total1.int_val == 2 &&
                chosen_total2 != null && chosen_total2.tag == INT && chosen_total2.int_val == 1 &&
                chosen_row3 == null;
        }

        if (mixed_neg_ok) {
            emit_pass_literal("[PASS] deduce mixed recursive aggregate SCCs with lower-stratum negation run on semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce mixed recursive aggregate SCCs with lower-stratum negation run on semi-naive-scc");
            (*fail)++;
        }
    }

    {
        Value* neg_db = null;
        Value* neg_out = null;
        usz neg_expected_rows = 0;
        bool neg_ok = deduce_query_bench_seed_recursive_aggregate_negated_db(
            interp,
            4,
            &neg_db,
            &neg_out,
            &neg_expected_rows
        );

        Value* neg_analyzed = null;
        if (neg_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = neg_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("naive"));
            neg_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            neg_ok = neg_analyzed != null && neg_analyzed.tag == HASHMAP && neg_analyzed.hashmap_val != null;
        }

        if (neg_ok) {
            Value* engine = deduce_query_test_dict_get(interp, neg_analyzed, "execution-engine");
            Value* requested = deduce_query_test_dict_get(interp, neg_analyzed, "requested-execution-engine");
            Value* fallback = deduce_query_test_dict_get(interp, neg_analyzed, "recursive-aggregate-naive-fallback");
            Value* routed = deduce_query_test_dict_get(
                interp,
                neg_analyzed,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value*[1] count_args;
            count_args[0] = neg_out;
            Value* row_count = prim_deduce_count(count_args[..], null, interp);

            neg_ok = engine != null && engine.tag == SYMBOL &&
                     engine.sym_val == interp.symbols.intern("naive-scc") &&
                     requested != null && requested.tag == SYMBOL &&
                     requested.sym_val == interp.symbols.intern("naive-scc") &&
                     fallback != null && fallback.tag == NIL &&
                     routed != null && routed.tag == NIL &&
                     row_count != null && row_count.tag == INT &&
                     row_count.int_val == (long)neg_expected_rows;
        }

        if (neg_ok) {
            emit_pass_literal("[PASS] deduce analyze keeps naive pure negated recursive aggregates off mixed-route diagnostics");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce analyze keeps naive pure negated recursive aggregates off mixed-route diagnostics");
            (*fail)++;
        }
    }

    {
        Value* mixed_neg_db = null;
        Value* mixed_neg_score = null;
        Value* mixed_neg_chosen = null;
        usz expected_score_rows = 0;
        usz expected_chosen_rows = 0;
        bool mixed_neg_ok = deduce_query_bench_seed_recursive_mixed_aggregate_negated_db(
            interp,
            4,
            &mixed_neg_db,
            &mixed_neg_score,
            &mixed_neg_chosen,
            &expected_score_rows,
            &expected_chosen_rows
        );

        Value* mixed_neg_analyzed = null;
        if (mixed_neg_ok) {
            Value*[2] analyze_args;
            analyze_args[0] = mixed_neg_db;
            analyze_args[1] = make_symbol(interp, interp.symbols.intern("naive"));
            mixed_neg_analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            mixed_neg_ok = mixed_neg_analyzed != null && mixed_neg_analyzed.tag == HASHMAP && mixed_neg_analyzed.hashmap_val != null;
        }

        if (mixed_neg_ok) {
            Value* engine = deduce_query_test_dict_get(interp, mixed_neg_analyzed, "execution-engine");
            Value* requested = deduce_query_test_dict_get(interp, mixed_neg_analyzed, "requested-execution-engine");
            Value* fallback = deduce_query_test_dict_get(interp, mixed_neg_analyzed, "recursive-aggregate-naive-fallback");
            Value* routed = deduce_query_test_dict_get(
                interp,
                mixed_neg_analyzed,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value*[1] count_score_args;
            count_score_args[0] = mixed_neg_score;
            Value* score_count = prim_deduce_count(count_score_args[..], null, interp);
            Value*[1] count_chosen_args;
            count_chosen_args[0] = mixed_neg_chosen;
            Value* chosen_count = prim_deduce_count(count_chosen_args[..], null, interp);

            mixed_neg_ok = engine != null && engine.tag == SYMBOL &&
                           engine.sym_val == interp.symbols.intern("semi-naive-scc") &&
                           requested != null && requested.tag == SYMBOL &&
                           requested.sym_val == interp.symbols.intern("naive-scc") &&
                           fallback != null && fallback.tag == NIL &&
                           routed != null && routed.tag == SYMBOL &&
                           routed.sym_val == interp.sym_true &&
                           score_count != null && score_count.tag == INT &&
                           score_count.int_val == (long)expected_score_rows &&
                           chosen_count != null && chosen_count.tag == INT &&
                           chosen_count.int_val == (long)expected_chosen_rows;
        }

        if (mixed_neg_ok) {
            emit_pass_literal("[PASS] deduce analyze routes naive mixed negated recursive aggregate SCCs to semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce analyze routes naive mixed negated recursive aggregate SCCs to semi-naive-scc");
            (*fail)++;
        }
    }
}

fn void run_deduce_query_tests(Interp* interp, int* pass, int* fail) {
    if (test_c_getenv("OMNI_DEDUCE_FORCE_ROW_DICT_OOM") != null) {
        run_deduce_scan_range_tests(interp, pass, fail);
        return;
    }

    if (deduce_query_filter_try_run_targeted_why_result(interp, pass, fail)) return;

    run_deduce_scan_tests(interp, pass, fail);
    run_deduce_scan_range_tests(interp, pass, fail);
    run_deduce_query_filter_tests(interp, pass, fail);
    run_deduce_match_tests(interp, pass, fail);
    run_deduce_empty_relation_stats_tests(interp, pass, fail);
    run_deduce_admin_surface_tests(interp, pass, fail);
    run_deduce_aggregate_rule_tests(interp, pass, fail);
    run_deduce_recursive_aggregate_rule_tests(interp, pass, fail);
    run_deduce_aggregate_benchmarks(interp);
    run_deduce_recursive_aggregate_benchmarks(interp);
    run_deduce_stats_maintenance_benchmarks(interp);
    run_deduce_scan_query_count_benchmarks(interp);
    run_deduce_incremental_mutation_benchmarks(interp);
    run_deduce_selective_join_benchmarks(interp);
    run_deduce_recursive_closure_benchmarks(interp);
    run_deduce_skewed_cardinality_benchmarks(interp);
}
