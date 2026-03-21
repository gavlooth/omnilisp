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

fn void run_deduce_empty_relation_stats_tests(Interp* interp, int* pass, int* fail) {
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
            "  (and (= (ref schema 'constraint-count) 1) "
            "       (and (= (ref schema 'reference-constraint-count) 1) "
            "            (and (= (ref schema 'resolved-reference-constraint-count) 1) "
            "                 (and (= (ref schema 'unresolved-reference-constraint-count) 0) "
            "                      (and (= (ref analyze 'keyed-relation-count) 1) "
            "                           (and (= (ref analyze 'reference-constraint-count) 1) "
            "                                (and (= (ref analyze 'resolved-reference-constraint-count) 1) "
            "                                     (and (= (ref analyze 'unresolved-reference-constraint-count) 0) "
            "                                          (and (= (ref analyze 'integrity-constraint-count) 2) "
            "                                               (and (= (ref first 'kind) 'reference) "
            "                                                    (and (= (car (ref first 'columns)) 'user-id) "
            "                                                         (and (= (ref first 'referenced-relation) 'ref-users-admin) "
            "                                                              (and (= (ref first 'referenced-column) 'id) "
            "                                                                   (and (= (ref first 'target-declared) true) "
            "                                                                        (and (= (ref first 'target-column-declared) true) "
            "                                                                             (= (ref first 'resolved) true))))))))))))))))",
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
            "  (and (= (ref schema 'reference-constraint-count) 1) "
            "       (and (= (ref schema 'resolved-reference-constraint-count) 0) "
            "            (and (= (ref schema 'unresolved-reference-constraint-count) 1) "
            "                 (and (= (ref analyze 'reference-constraint-count) 1) "
            "                      (and (= (ref analyze 'resolved-reference-constraint-count) 0) "
            "                           (and (= (ref analyze 'unresolved-reference-constraint-count) 1) "
            "                                (and (= (ref first 'target-declared) false) "
            "                                     (and (= (ref first 'target-column-declared) false) "
            "                                          (= (ref first 'resolved) false)))))))))",
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
            "  (and (= (ref schema 'reference-constraint-count) 1) "
            "       (and (= (ref schema 'resolved-reference-constraint-count) 0) "
            "            (and (= (ref schema 'unresolved-reference-constraint-count) 1) "
            "                 (and (= (ref analyze 'reference-constraint-count) 1) "
            "                      (and (= (ref analyze 'resolved-reference-constraint-count) 0) "
            "                           (and (= (ref analyze 'unresolved-reference-constraint-count) 1) "
            "                                (and (= (ref first 'target-declared) true) "
            "                                     (and (= (ref first 'target-column-declared) false) "
            "                                          (= (ref first 'resolved) false)))))))))",
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
}

fn void run_deduce_aggregate_rule_tests(Interp* interp, int* pass, int* fail) {
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
    run_deduce_scan_query_count_benchmarks(interp);
    run_deduce_incremental_mutation_benchmarks(interp);
    run_deduce_selective_join_benchmarks(interp);
    run_deduce_recursive_closure_benchmarks(interp);
    run_deduce_skewed_cardinality_benchmarks(interp);
}
