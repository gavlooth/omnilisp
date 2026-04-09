module lisp;

import std::io;

fn void run_deduce_admin_surface_metadata_and_goal_directed_tests(Interp* interp, int* pass, int* fail) {
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
}
