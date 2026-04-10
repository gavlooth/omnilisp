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

fn bool deduce_query_test_error_matches(EvalResult result, char[] expected) {
    return result.error.has_error && str_starts_with(result.error.message[:256], expected);
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

fn bool deduce_query_filter_contains_token(char* filter, char[] name) {
    if (filter == null) return false;

    usz i = 0;
    while (filter[i] != 0) {
        while (filter[i] == ' ' || filter[i] == ',') i++;
        if (filter[i] == 0) break;

        usz start = i;
        while (filter[i] != 0 && filter[i] != ',') i++;
        usz end = i;
        while (end > start && filter[end - 1] == ' ') end--;

        usz token_len = end - start;
        if (token_len == name.len) {
            bool matches = true;
            for (usz j = 0; j < token_len; j++) {
                if (filter[start + j] != name[j]) {
                    matches = false;
                    break;
                }
            }
            if (matches) return true;
        }

        if (filter[i] == ',') i++;
    }
    return false;
}

bool deduce_query_filter_why_result_ran = false;

fn bool deduce_query_filter_try_run_targeted_why_result(Interp* interp, int* pass, int* fail) {
    char* query_filter = test_c_getenv("OMNI_DEDUCE_QUERY_FILTER");
    bool run_bounded = deduce_query_filter_matches(query_filter, "why-result-bounded");
    if (!run_bounded) return false;
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
        EvalResult r = run("(ref (car (deduce 'match person '(person ?name ?age))) 'name)", interp);
        if (!r.error.has_error && is_string(r.value)) {
            emit_pass_literal("[PASS] deduce 'match ?var binding");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'match ?var binding");
            (*fail)++;
        }
    }

    {
        bool saved_hashmap_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        EvalResult r = run(
            "(handle (deduce 'match person '(person ?name ?age)) "
            "  (raise payload "
            "    (if (dict? payload) "
            "        (symbol->string (ref payload 'code)) "
            "        \"deduce/match-out-of-memory\")))",
            interp
        );
        g_hashmap_force_struct_alloc_fail = saved_hashmap_struct_fail;

        bool ok =
            !r.error.has_error &&
            is_string(r.value) &&
            deduce_name_equals(r.value.str_chars[:r.value.str_len], "deduce/match-out-of-memory");
        if (ok) {
            emit_pass_literal("[PASS] deduce 'match propagates result-dict constructor oom");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'match propagates result-dict constructor oom");
            (*fail)++;
        }
    }

    {
        EvalResult setup = run(
            "(block "
            "  (define db-why-result-oom-seed (deduce 'open 'memory)) "
            "  (define [relation db-why-result-oom-seed] parent-oom-seed (src) (dst)) "
            "  (deduce/fact! parent-oom-seed 1 2))",
            interp
        );
        if (setup.error.has_error) {
            io::printfn(
                "[FAIL] deduce why-result fails closed when seed payload path allocation fails (setup error=%s)",
                (ZString)setup.error.message[:256]
            );
            (*fail)++;
            return;
        }

        bool saved_hashmap_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        EvalResult r = run("(deduce/why-result parent-oom-seed 1 2)", interp);
        g_hashmap_force_struct_alloc_fail = saved_hashmap_struct_fail;

        bool ok = deduce_query_test_error_matches(r, "deduce/why-result: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] deduce why-result fails closed when seed payload path allocation fails");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] deduce why-result fails closed when seed payload path allocation fails (error=%s msg=%s)",
                r.error.has_error ? "yes" : "no",
                r.error.has_error ? (ZString)r.error.message[:256] : (ZString)"<none>"
            );
            (*fail)++;
        }
    }

    {
        EvalResult setup = run(
            "(block "
            "  (define db-why-result-oom-derived (deduce 'open 'memory)) "
            "  (define [relation db-why-result-oom-derived] parent-oom-derived (src) (dst)) "
            "  (define [relation db-why-result-oom-derived materialized] reach-oom-derived (src) (dst)) "
            "  (deduce/rule! db-why-result-oom-derived '(reach-oom-derived ?x ?y) '(parent-oom-derived ?x ?y)) "
            "  (deduce/fact! parent-oom-derived 1 2) "
            "  (deduce/refresh! reach-oom-derived) "
            "  (deduce/fact! parent-oom-derived 2 3))",
            interp
        );
        if (setup.error.has_error) {
            io::printfn(
                "[FAIL] deduce why-result fails closed when derived payload path allocation fails (setup error=%s)",
                (ZString)setup.error.message[:256]
            );
            (*fail)++;
            return;
        }

        bool saved_hashmap_struct_fail = g_hashmap_force_struct_alloc_fail;
        g_hashmap_force_struct_alloc_fail = true;
        EvalResult r = run("(deduce/why-result reach-oom-derived 1 2)", interp);
        g_hashmap_force_struct_alloc_fail = saved_hashmap_struct_fail;

        bool ok = deduce_query_test_error_matches(r, "deduce/why-result: out of memory");
        if (ok) {
            emit_pass_literal("[PASS] deduce why-result fails closed when derived payload path allocation fails");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] deduce why-result fails closed when derived payload path allocation fails (error=%s msg=%s)",
                r.error.has_error ? "yes" : "no",
                r.error.has_error ? (ZString)r.error.message[:256] : (ZString)"<none>"
            );
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
            if (try char[] col_name = io::bprintf(&col_buf, "c%d", (int)i)) {
                col_syms[i] = interp.symbols.intern(col_name);
                define_args[i + 2] = make_symbol(interp, col_syms[i]);
            } else {
                io::printn("[FAIL] deduce 'match preserves >32 bindings (col format)");
                (*fail)++;
                return;
            }

            char[16] var_buf;
            if (try char[] var_name = io::bprintf(&var_buf, "?v%d", (int)i)) {
                var_syms[i] = interp.symbols.intern(var_name);
                key_syms[i] = interp.symbols.intern(var_name[1..]);
            } else {
                io::printn("[FAIL] deduce 'match preserves >32 bindings (var format)");
                (*fail)++;
                return;
            }

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

fn void run_deduce_query_tests(Interp* interp, int* pass, int* fail) {
    if (test_c_getenv("OMNI_DEDUCE_FORCE_ROW_DICT_OOM") != null) {
        run_deduce_scan_range_tests(interp, pass, fail);
        return;
    }

    if (deduce_query_filter_try_run_targeted_why_result(interp, pass, fail)) return;

    char* query_filter = test_c_getenv("OMNI_DEDUCE_QUERY_FILTER");
    bool matched_group_filter = false;
    if (deduce_query_filter_contains_token(query_filter, "scan")) {
        run_deduce_scan_tests(interp, pass, fail);
        matched_group_filter = true;
    }
    if (deduce_query_filter_contains_token(query_filter, "scan-range")) {
        run_deduce_scan_range_tests(interp, pass, fail);
        matched_group_filter = true;
    }
    if (deduce_query_filter_contains_token(query_filter, "query-filter")) {
        run_deduce_query_filter_tests(interp, pass, fail);
        matched_group_filter = true;
    }
    if (deduce_query_filter_contains_token(query_filter, "match")) {
        run_deduce_match_tests(interp, pass, fail);
        matched_group_filter = true;
    }
    if (deduce_query_filter_contains_token(query_filter, "empty-relation-stats")) {
        run_deduce_empty_relation_stats_tests(interp, pass, fail);
        matched_group_filter = true;
    }
    if (deduce_query_filter_contains_token(query_filter, "admin-surface")) {
        run_deduce_admin_surface_tests(interp, pass, fail);
        matched_group_filter = true;
    }
    if (deduce_query_filter_contains_token(query_filter, "admin-surface-demand")) {
        run_deduce_admin_surface_demand_tests(interp, pass, fail);
        matched_group_filter = true;
    }
    if (deduce_query_filter_contains_token(query_filter, "aggregate-rules")) {
        run_deduce_aggregate_rule_tests(interp, pass, fail);
        matched_group_filter = true;
    }
    if (deduce_query_filter_contains_token(query_filter, "recursive-aggregate-rules")) {
        run_deduce_recursive_aggregate_rule_tests(interp, pass, fail);
        matched_group_filter = true;
    }
    if (matched_group_filter) {
        return;
    }

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
