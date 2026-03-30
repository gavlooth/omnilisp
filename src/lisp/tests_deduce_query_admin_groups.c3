module lisp;

import std::io;

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
            "    (= (length rows) 3) "
            "    (every? (lambda (row) (= (ref row 'dst) 4)) rows) "
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
            "    (= (dict? top-gd) nil) "
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
            "    (= (ref fact-1-gd 'execution-path) 'no-op) "
            "    (= (ref fact-1-gd 'selector-rule-index) nil) "
            "    (= (ref fact-1-gd 'requested-bound-positions) nil) "
            "    (= (ref fact-1-gd 'applied-bound-positions) nil) "
            "    (= (ref fact-2-gd 'surface) 'query) "
            "    (= (ref fact-2-gd 'execution-path) 'no-op) "
            "    (= (ref fact-2-gd 'selector-rule-index) nil) "
            "    (= (ref fact-2-gd 'requested-bound-positions) nil) "
            "    (= (ref fact-2-gd 'applied-bound-positions) nil)))",
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
            "    (= (ref match-path-gd 'execution-path) 'ephemeral-head-demand-match) "
            "    (= (ref match-path-gd 'selector-rule-index) 1) "
            "    (= (ref match-path-gd 'requested-bound-count) 2) "
            "    (= (length (ref match-path-gd 'requested-bound-positions)) 2) "
            "    (= (car (ref match-path-gd 'requested-bound-positions)) 0) "
            "    (= (car (cdr (ref match-path-gd 'requested-bound-positions))) 1) "
            "    (= (ref match-path-gd 'applied-bound-count) 1) "
            "    (= (length (ref match-path-gd 'applied-bound-positions)) 1) "
            "    (= (car (ref match-path-gd 'applied-bound-positions)) 1) "
            "    (= (dict? scan-path-gd) true) "
            "    (= (ref scan-path-gd 'surface) 'scan) "
            "    (= (ref scan-path-gd 'execution-path) 'selected-component-closure) "
            "    (= (ref scan-path-gd 'selector-rule-index) 1) "
            "    (= (ref scan-path-gd 'fallback-reason) 'dirty-closure) "
            "    (= (ref scan-path-gd 'requested-bound-count) 0) "
            "    (= (ref scan-path-gd 'requested-bound-positions) nil) "
            "    (= (ref scan-path-gd 'applied-bound-count) 0) "
            "    (= (ref scan-path-gd 'applied-bound-positions) nil) "
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
            "  (define mixed-derived-support (ref mixed-derived-path 'support)) "
            "  (define mixed-derived-step (car (reverse mixed-derived-support))) "
            "  (printn (list "
            "    (ref seed 'status) (ref seed 'path-count) (ref seed 'max-depth) (ref seed 'truncated) "
            "    (ref derived 'status) (ref derived 'path-count) (ref derived 'max-depth) (ref derived 'truncated) "
            "    (ref projected-derived 'status) (ref projected-derived 'path-count) "
            "    (ref multi-derived 'status) (ref multi-derived 'path-count) (length multi-derived-support) "
            "    (ref search-derived 'status) (ref search-derived 'path-count) (ref search-derived 'truncated) (length (ref search-derived-path 'support)) "
            "    (ref search-derived-partial 'status) (ref search-derived-partial 'path-count) (ref search-derived-partial 'truncated) (length (ref search-derived-partial-path 'support)) "
            "    (ref mixed-derived 'status) (ref mixed-derived 'path-count) (ref mixed-derived 'truncated) (length mixed-derived-support) "
            "    (ref multi-rule-direct 'status) (ref multi-rule-direct 'path-count) (ref multi-rule-direct 'max-depth) (ref multi-rule-direct 'truncated) (ref (car (ref multi-rule-direct 'paths)) 'rule-index) "
            "    (length (ref (car (ref multi-rule-direct 'paths)) 'support)) "
            "    (ref multi-rule-derived 'status) (ref multi-rule-derived 'path-count) (ref multi-rule-derived 'max-depth) (ref multi-rule-derived 'truncated) (ref (car (ref multi-rule-derived 'paths)) 'rule-index) "
            "    (length (ref (car (ref multi-rule-derived 'paths)) 'support)))) "
            "  (and "
            "    (= (ref seed 'kind) 'why-result) "
            "    (= (ref seed 'status) 'ok) "
            "    (= (ref seed 'surface-kind) 'provenance-snapshot) "
            "    (= (ref seed 'subject-kind) 'row) "
            "    (= (ref (ref seed 'subject) 'relation) 'parent-why) "
            "    (= (length (ref (ref seed 'subject) 'tuple)) 2) "
            "    (= (ref seed 'path-count) 1) "
            "    (= (ref seed 'max-depth) 1) "
            "    (= (ref seed 'truncated) nil) "
            "    (= (length seed-paths) 1) "
            "    (= (ref seed-path 'path-kind) 'seed) "
            "    (= (ref seed-path 'head-predicate) 'parent-why) "
            "    (= (ref seed-path 'rule-index) nil) "
            "    (= (ref seed-path 'truncated) nil) "
            "    (= (length seed-support) 1) "
            "    (= (ref seed-frame 'kind) 'fact) "
            "    (= (ref seed-frame 'predicate) 'parent-why) "
            "    (= (ref missing 'status) 'ok) "
            "    (= (ref missing 'path-count) 1) "
            "    (= (ref missing 'max-depth) 1) "
            "    (= (ref missing 'truncated) nil) "
            "    (= (length (ref missing 'paths)) 1) "
            "    (= (ref (car (ref missing 'paths)) 'path-kind) 'seed) "
            "    (= (ref (car (ref missing 'paths)) 'head-predicate) 'parent-why) "
            "    (= (ref (car (ref missing 'paths)) 'rule-index) nil) "
            "    (= (ref (car (ref missing 'paths)) 'truncated) nil) "
            "    (= (length (ref (car (ref missing 'paths)) 'support)) 1) "
            "    (= (ref (car (ref (car (ref missing 'paths)) 'support)) 'kind) 'fact) "
            "    (= (ref (car (ref (car (ref missing 'paths)) 'support)) 'predicate) 'parent-why) "
            "    (= (ref derived 'status) 'ok) "
            "    (= (ref derived 'path-count) 1) "
            "    (= (ref derived 'max-depth) 2) "
            "    (= (ref derived 'truncated) nil) "
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
            "    (= (ref search-derived 'truncated) nil) "
            "    (= (ref search-derived-path 'path-kind) 'derived) "
            "    (= (length (ref search-derived-path 'support)) 2) "
            "    (= (ref search-derived-partial 'status) 'partial) "
            "    (= (ref search-derived-partial 'path-count) 1) "
            "    (= (ref search-derived-partial 'truncated) true) "
            "    (= (ref search-derived-partial-path 'path-kind) 'derived) "
            "    (= (length (ref search-derived-partial-path 'support)) 2) "
            "    (= (ref mixed-derived 'status) 'ok) "
            "    (= (ref mixed-derived 'path-count) 1) "
            "    (= (ref mixed-derived 'truncated) nil) "
            "    (= (ref mixed-derived-path 'path-kind) 'derived) "
            "    (= (length mixed-derived-support) 3) "
            "    (= (ref mixed-derived-step 'kind) 'rule-step) "
            "    (= (ref mixed-derived-step 'predicate) 'ancestor-why) "
            "    (= (ref multi-rule-direct 'status) 'ok) "
            "    (= (ref multi-rule-direct 'path-count) 1) "
            "    (= (ref multi-rule-direct 'max-depth) 2) "
            "    (= (ref multi-rule-direct 'truncated) nil) "
            "    (= (length (ref multi-rule-direct 'paths)) 1) "
            "    (= (ref (car (ref multi-rule-direct 'paths)) 'rule-index) 5) "
            "    (= (length (ref (car (ref multi-rule-direct 'paths)) 'support)) 1) "
            "    (= (ref multi-rule-derived 'status) 'partial) "
            "    (= (ref multi-rule-derived 'path-count) 1) "
            "    (= (ref multi-rule-derived 'max-depth) 3) "
            "    (= (ref multi-rule-derived 'truncated) true) "
            "    (= (length (ref multi-rule-derived 'paths)) 1) "
            "    (= (ref (car (ref multi-rule-derived 'paths)) 'path-kind) 'derived) "
            "    (= (ref (car (ref multi-rule-derived 'paths)) 'head-predicate) 'reach-why) "
            "    (= (ref (car (ref multi-rule-derived 'paths)) 'rule-index) 6) "
            "    (= (length (ref (car (ref multi-rule-derived 'paths)) 'support)) 3)))",
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

    run_deduce_admin_surface_tail_tests(interp, pass, fail);
}
