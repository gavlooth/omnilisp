module lisp;

import std::io;
import std::core::mem;


fn void run_deduce_rule_validation_tests_more_tail_head(Interp* interp, int* pass, int* fail) {
    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        SymbolId rel_sym = interp.symbols.intern("schema_idx_memory");
        SymbolId col_a = interp.symbols.intern("name");
        SymbolId col_b = interp.symbols.intern("age");
        SymbolId col_c = interp.symbols.intern("email");

        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        if (ok) {
            Value*[5] define_args;
            define_args[0] = db_val;
            define_args[1] = deduce_rule_test_make_symbol_value(interp, rel_sym);
            define_args[2] = deduce_rule_test_make_symbol_value(interp, col_a);
            define_args[3] = deduce_rule_test_make_symbol_value(interp, col_b);
            define_args[4] = deduce_rule_test_make_symbol_value(interp, col_c);
            Value* rel_val = prim_define_relation(define_args[..], null, interp);
            ok = rel_val != null && rel_val.tag == FFI_HANDLE;
        }
        if (ok) {
            DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
            DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
            ok = deduce_rule_test_validate_default_indexes(schema);
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce relation schema indexes (memory-backed)");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce relation schema indexes (memory-backed)");
            (*fail)++;
        }
    }

    {
        uint pid = (uint)deduce_test_getpid();
        usz seq = g_deduce_test_long_path_seq++;
        char[256] db_path_buf;
        char[] db_path;
        if (try char[] rendered_path = io::bprintf(
            &db_path_buf,
            "/tmp/omni-deduce-index-open-%d-%d",
            (int)pid,
            (int)seq
        )) {
            db_path = rendered_path;
        } else {
            io::printn("[FAIL] deduce relation schema indexes (file-backed path)");
            (*fail)++;
            return;
        }

        char[320] cleanup_cmd_buf;
        char[] cleanup_cmd;
        if (try char[] rendered_cleanup = io::bprintf(&cleanup_cmd_buf, "rm -rf %s", (ZString)db_path)) {
            cleanup_cmd = rendered_cleanup;
        } else {
            io::printn("[FAIL] deduce relation schema indexes (file-backed cleanup path)");
            (*fail)++;
            return;
        }
        cleanup_cmd_buf[cleanup_cmd.len] = 0;
        deduce_test_system(&cleanup_cmd_buf[0]);
        defer deduce_test_system(&cleanup_cmd_buf[0]);

        Value*[1] open_args;
        open_args[0] = make_string(interp, db_path);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        SymbolId rel_sym = interp.symbols.intern("schema_idx_file");
        SymbolId col_a = interp.symbols.intern("name");
        SymbolId col_b = interp.symbols.intern("age");
        SymbolId col_c = interp.symbols.intern("email");

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        if (ok) {
            Value*[6] open_named_args;
            open_named_args[0] = db_val;
            open_named_args[1] = deduce_rule_test_make_symbol_value(interp, rel_sym);
            open_named_args[2] = make_string(interp, "schema_idx_file_v1");
            open_named_args[3] = deduce_rule_test_make_symbol_value(interp, col_a);
            open_named_args[4] = deduce_rule_test_make_symbol_value(interp, col_b);
            open_named_args[5] = deduce_rule_test_make_symbol_value(interp, col_c);
            Value* rel_val = prim_deduce_open_named(open_named_args[..], null, interp);
            ok = rel_val != null && rel_val.tag == FFI_HANDLE;
        }
        if (ok) {
            DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
            DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, rel_sym);
            ok = deduce_rule_test_validate_default_indexes(schema);
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce relation schema indexes (file-backed open-named)");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce relation schema indexes (file-backed open-named)");
            (*fail)++;
        }
    }

    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        SymbolId rel_sym = interp.symbols.intern("schema_stats");
        SymbolId col_name = interp.symbols.intern("name");
        SymbolId col_age = interp.symbols.intern("age");

        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;

        Value* rel_val = null;
        if (ok) {
            Value*[4] define_args;
            define_args[0] = db_val;
            define_args[1] = deduce_rule_test_make_symbol_value(interp, rel_sym);
            define_args[2] = deduce_rule_test_make_symbol_value(interp, col_name);
            define_args[3] = deduce_rule_test_make_symbol_value(interp, col_age);
            rel_val = prim_define_relation(define_args[..], null, interp);
            ok = rel_val != null && rel_val.tag == FFI_HANDLE;
        }

        DeduceDb* db = ok ? (DeduceDb*)db_val.ffi_val.lib_handle : null;
        DeduceRelationSchema* schema = ok ? deduce_db_lookup_relation_schema(db, rel_sym) : null;
        ok = ok &&
             schema != null &&
             schema.cardinality_estimate == 0 &&
             schema.distinct_estimate == 0;

        if (ok) {
            Value*[3] fact1;
            fact1[0] = rel_val;
            fact1[1] = make_string(interp, "Alice");
            fact1[2] = make_int(interp, 30);
            Value* r1 = prim_assert_fact(fact1[..], null, interp);

            Value*[3] fact2;
            fact2[0] = rel_val;
            fact2[1] = make_string(interp, "Bob");
            fact2[2] = make_int(interp, 31);
            Value* r2 = prim_assert_fact(fact2[..], null, interp);

            Value*[3] fact2_dup;
            fact2_dup[0] = rel_val;
            fact2_dup[1] = make_string(interp, "Bob");
            fact2_dup[2] = make_int(interp, 31);
            Value* r3 = prim_assert_fact(fact2_dup[..], null, interp);

            ok = r1 != null && r1.tag == VOID &&
                 r2 != null && r2.tag == VOID &&
                 r3 != null && r3.tag == VOID &&
                 schema.cardinality_estimate == 2 &&
                 schema.distinct_estimate == 2;
        }

        if (ok) {
            Value*[3] retract1;
            retract1[0] = rel_val;
            retract1[1] = make_string(interp, "Bob");
            retract1[2] = make_int(interp, 31);
            Value* rr1 = prim_retract_fact(retract1[..], null, interp);

            Value*[3] retract_miss;
            retract_miss[0] = rel_val;
            retract_miss[1] = make_string(interp, "Zoe");
            retract_miss[2] = make_int(interp, 99);
            Value* rr2 = prim_retract_fact(retract_miss[..], null, interp);

            ok = rr1 != null && rr1.tag == VOID &&
                 rr2 != null && rr2.tag == VOID &&
                 schema.cardinality_estimate == 1 &&
                 schema.distinct_estimate == 1;
        }

        if (ok) {
            Value*[1] clear_args;
            clear_args[0] = rel_val;
            Value* cleared = prim_deduce_clear(clear_args[..], null, interp);
            ok = cleared != null &&
                 cleared.tag == VOID &&
                 schema.cardinality_estimate == 0 &&
                 schema.distinct_estimate == 0;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce schema captures baseline cardinality/distinct estimates");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce schema captures baseline cardinality/distinct estimates");
            (*fail)++;
        }
    }

    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        SymbolId rel_sym = interp.symbols.intern("extensional_shadow_rel");
        SymbolId col_id = interp.symbols.intern("id");
        SymbolId col_kind = interp.symbols.intern("kind");
        bool ok = true;

        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);
        Value* rel_val = ok ? setup_deduce_relation_for_db(interp, db_val, rel_sym, col_id, col_kind) : null;
        ok = db_val != null && db_val.tag == FFI_HANDLE && db_val.ffi_val != null &&
             rel_val != null && rel_val.tag == FFI_HANDLE && rel_val.ffi_val != null;

        Relation* rel = ok ? (Relation*)rel_val.ffi_val.lib_handle : null;
        MdbDbi shadow_dbi = 0;
        long shadow_count = -1;
        if (ok) {
            Value*[3] fact1_full;
            fact1_full[0] = rel_val;
            fact1_full[1] = make_int(interp, 1);
            fact1_full[2] = make_string(interp, "seed");
            Value*[3] fact2;
            fact2[0] = rel_val;
            fact2[1] = make_int(interp, 2);
            fact2[2] = make_string(interp, "seed");
            Value* r1 = prim_assert_fact(fact1_full[..], null, interp);
            Value* r2 = prim_assert_fact(fact2[..], null, interp);
            ok = r1 != null && r1.tag != ERROR && r2 != null && r2.tag != ERROR;
        }

        if (ok) {
            MdbTxn* txn = null;
            int rc = mdb_txn_begin(rel.db.env, null, MDB_RDONLY, &txn);
            ok = rc == MDB_SUCCESS && txn != null &&
                 deduce_open_extensional_dbi_for_relation(rel, txn, false, &shadow_dbi, interp);
            if (ok) {
                Value* err = deduce_relation_count_rows(txn, shadow_dbi, &shadow_count, interp);
                ok = err == null && shadow_count == 2;
            }
            if (txn != null) mdb_txn_abort(txn);
        }

        if (ok) {
            Value*[3] retract_args;
            retract_args[0] = rel_val;
            retract_args[1] = make_int(interp, 1);
            retract_args[2] = make_string(interp, "seed");
            Value* retracted = prim_retract_fact(retract_args[..], null, interp);
            ok = retracted != null && retracted.tag != ERROR;
        }

        if (ok) {
            MdbTxn* txn = null;
            int rc = mdb_txn_begin(rel.db.env, null, MDB_RDONLY, &txn);
            ok = rc == MDB_SUCCESS && txn != null &&
                 deduce_open_extensional_dbi_for_relation(rel, txn, false, &shadow_dbi, interp);
            if (ok) {
                Value* err = deduce_relation_count_rows(txn, shadow_dbi, &shadow_count, interp);
                ok = err == null && shadow_count == 1;
            }
            if (txn != null) mdb_txn_abort(txn);
        }

        if (ok) {
            Value*[1] clear_args;
            clear_args[0] = rel_val;
            Value* cleared = prim_deduce_clear(clear_args[..], null, interp);
            ok = cleared != null && cleared.tag != ERROR;
        }

        if (ok) {
            MdbTxn* txn = null;
            int rc = mdb_txn_begin(rel.db.env, null, MDB_RDONLY, &txn);
            ok = rc == MDB_SUCCESS && txn != null &&
                 deduce_open_extensional_dbi_for_relation(rel, txn, false, &shadow_dbi, interp);
            if (ok) {
                Value* err = deduce_relation_count_rows(txn, shadow_dbi, &shadow_count, interp);
                ok = err == null && shadow_count == 0;
            }
            if (txn != null) mdb_txn_abort(txn);
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce extensional shadow mirrors assert retract and clear");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce extensional shadow mirrors assert retract and clear");
            (*fail)++;
        }
    }

    {
        DeduceTupleSupportTable table = {};
        defer deduce_tuple_support_table_release(&table);

        char[1] recursive_key = {'r'};
        char[1] base_key = {'b'};

        bool ok = true;
        long total = -1;
        bool became_present = false;
        bool became_absent = false;

        Value* err = deduce_tuple_support_table_adjust_recursive_count(
            &table,
            &recursive_key[0],
            1,
            1,
            &total,
            &became_present,
            &became_absent,
            interp
        );
        ok = err == null &&
             total == 1 &&
             became_present &&
             !became_absent &&
             deduce_tuple_support_table_total_count(&table, &recursive_key[0], 1, interp) == 1;

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                &recursive_key[0],
                1,
                1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 2 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_total_count(&table, &recursive_key[0], 1, interp) == 2;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                &recursive_key[0],
                1,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 1 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_total_count(&table, &recursive_key[0], 1, interp) == 1;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                &recursive_key[0],
                1,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 0 &&
                 !became_present &&
                 became_absent &&
                 deduce_tuple_support_table_total_count(&table, &recursive_key[0], 1, interp) == 0;
        }

        if (ok) {
            err = deduce_tuple_support_table_seed_base_tuple(
                &table,
                &base_key[0],
                1,
                &total,
                interp
            );
            ok = err == null &&
                 total == 1 &&
                 deduce_tuple_support_table_total_count(&table, &base_key[0], 1, interp) == 1;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                &base_key[0],
                1,
                1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 2 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_total_count(&table, &base_key[0], 1, interp) == 2;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                &base_key[0],
                1,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null &&
                 total == 1 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_total_count(&table, &base_key[0], 1, interp) == 1;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                &base_key[0],
                1,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err != null &&
                 deduce_tuple_support_table_total_count(&table, &base_key[0], 1, interp) == 1;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce tuple support tables track zero-crossing transitions");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce tuple support tables track zero-crossing transitions");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-ana (deduce/open 'memory)) "
            "  (define [relation db-ana] parent (src) (dst)) "
            "  (define [relation db-ana] ancestor (src) (dst)) "
            "  (deduce/fact! parent \"A\" \"B\") "
            "  (deduce/fact! parent \"B\" \"C\") "
            "  (deduce/fact! parent \"C\" \"D\") "
            "  (deduce/rule! db-ana '(ancestor ?x ?y) '(parent ?x ?y)) "
            "  (deduce/rule! db-ana '(ancestor ?x ?z) '(parent ?x ?y) '(ancestor ?y ?z)) "
            "  (define a1 (deduce/analyze db-ana)) "
            "  (define a2 (deduce 'analyze db-ana)) "
            "  (define ok1 (= (deduce/count ancestor) 6)) "
            "  (define ok2 (= a1.mode 'naive-bottom-up)) "
            "  (define ok3 (= a1.status 'ok)) "
            "  (define ok4 (= a1.execution-engine 'semi-naive-scc)) "
            "  (define ok5 (= a2.mode 'naive-bottom-up)) "
            "  (define ok6 (= a2.execution-engine 'semi-naive-scc)) "
            "  (define ok7 (= a1.strata-count 2)) "
            "  (define ok8 (= a1.recursive-strata 1)) "
            "  (define ok9 (>= a1.iterations 1)) "
            "  (define ok10 (= a1.iteration-limit 256)) "
            "  (define ok11 (>= a1.max-component-iterations 1)) "
            "  (and ok1 (and ok2 (and ok3 (and ok4 (and ok5 (and ok6 (and ok7 (and ok8 (and ok9 (and ok10 ok11))))))))))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze runs semi-naive recursive fixpoint and derives closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze runs semi-naive recursive fixpoint and derives closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze runs semi-naive recursive fixpoint and derives closure");
            }
            (*fail)++;
        }
    }

    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        SymbolId rel_src = interp.symbols.intern("analyze_counter_src");
        SymbolId rel_filter = interp.symbols.intern("analyze_counter_filter");
        SymbolId rel_out = interp.symbols.intern("analyze_counter_out");
        SymbolId col_left = interp.symbols.intern("left");
        SymbolId col_right = interp.symbols.intern("right");
        Value* src_rel = null;
        Value* filter_rel = null;
        if (ok) {
            src_rel = setup_deduce_relation_for_db(interp, db_val, rel_src, col_left, col_right);
            filter_rel = setup_deduce_relation_for_db(interp, db_val, rel_filter, col_left, col_right);
            Value* out_rel = setup_deduce_relation_for_db(interp, db_val, rel_out, col_left, col_right);
            ok = src_rel != null && src_rel.tag == FFI_HANDLE &&
                 filter_rel != null && filter_rel.tag == FFI_HANDLE &&
                 out_rel != null && out_rel.tag == FFI_HANDLE;
        }

        if (ok) {
            for (long i = 0; i < 4; i++) {
                Value*[3] fact_args;
                fact_args[0] = src_rel;
                fact_args[1] = make_int(interp, i);
                fact_args[2] = make_int(interp, i + 1000);
                Value* inserted = prim_assert_fact(fact_args[..], null, interp);
                ok = inserted != null && inserted.tag == VOID;
                if (!ok) break;
            }
        }

        if (ok) {
            Value*[3] fact_args;
            fact_args[0] = filter_rel;
            fact_args[1] = make_int(interp, 0);
            fact_args[2] = make_int(interp, 1000);
            Value* inserted = prim_assert_fact(fact_args[..], null, interp);
            ok = inserted != null && inserted.tag == VOID;
        }

        if (ok) {
            SymbolId var_x = interp.symbols.intern("?x");
            SymbolId var_y = interp.symbols.intern("?y");
            Value* head = deduce_rule_test_make_atom2(
                interp,
                rel_out,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_filter = deduce_rule_test_make_atom2(
                interp,
                rel_filter,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );
            Value* body_src = deduce_rule_test_make_atom2(
                interp,
                rel_src,
                deduce_rule_test_make_symbol_value(interp, var_x),
                deduce_rule_test_make_symbol_value(interp, var_y)
            );

            Value*[4] rule_args;
            rule_args[0] = db_val;
            rule_args[1] = head;
            rule_args[2] = body_filter;
            rule_args[3] = body_src;
            Value* rv = prim_deduce_rule(rule_args[..], null, interp);
            ok = rv != null && rv.tag == VOID;
        }

        if (ok) {
            Value*[1] analyze_args;
            analyze_args[0] = db_val;
            Value* analyzed = prim_deduce_analyze(analyze_args[..], null, interp);
            ok = analyzed != null &&
                 analyzed.tag == HASHMAP &&
                 analyzed.hashmap_val != null;
            if (ok) {
                Value* rule_execution = deduce_rule_test_dict_get(interp, analyzed, "rule-execution");
                ok = is_cons(rule_execution) && is_nil(cdr(rule_execution));
                if (ok) {
                    Value* entry = car(rule_execution);
                    Value* head_pred = deduce_rule_test_dict_get(interp, entry, "head-predicate");
                    Value* steps = deduce_rule_test_dict_get(interp, entry, "steps");
                    Value* step0 = is_cons(steps) ? car(steps) : null;
                    Value* step1 = is_cons(steps) && is_cons(cdr(steps)) ? car(cdr(steps)) : null;
                    Value* step0_pred = deduce_rule_test_dict_get(interp, step0, "predicate");
                    Value* step1_pred = deduce_rule_test_dict_get(interp, step1, "predicate");
                    Value* step0_counters = deduce_rule_test_dict_get(interp, step0, "counters");
                    Value* step1_counters = deduce_rule_test_dict_get(interp, step1, "counters");
                    Value* step0_rows_read = deduce_rule_test_dict_get(interp, step0_counters, "rows-read");
                    Value* step0_rows_emitted = deduce_rule_test_dict_get(interp, step0_counters, "rows-emitted");
                    Value* step0_index_hits = deduce_rule_test_dict_get(interp, step0_counters, "index-hits");
                    Value* step0_index_misses = deduce_rule_test_dict_get(interp, step0_counters, "index-misses");
                    Value* step0_join_probes = deduce_rule_test_dict_get(interp, step0_counters, "join-probes");
                    Value* step0_counter_kind = deduce_rule_test_dict_get(interp, step0_counters, "counter-kind");
                    Value* step1_rows_read = deduce_rule_test_dict_get(interp, step1_counters, "rows-read");
                    Value* step1_rows_emitted = deduce_rule_test_dict_get(interp, step1_counters, "rows-emitted");
                    Value* step1_index_hits = deduce_rule_test_dict_get(interp, step1_counters, "index-hits");
                    Value* step1_index_misses = deduce_rule_test_dict_get(interp, step1_counters, "index-misses");
                    Value* step1_join_probes = deduce_rule_test_dict_get(interp, step1_counters, "join-probes");
                    Value* step1_counter_kind = deduce_rule_test_dict_get(interp, step1_counters, "counter-kind");
                    SymbolId sym_observed = interp.symbols.intern("observed");

                    ok = head_pred != null && head_pred.tag == SYMBOL && head_pred.sym_val == rel_out &&
                         is_cons(steps) && is_cons(cdr(steps)) && is_nil(cdr(cdr(steps))) &&
                         step0_pred != null && step0_pred.tag == SYMBOL && step0_pred.sym_val == rel_filter &&
                         step1_pred != null && step1_pred.tag == SYMBOL && step1_pred.sym_val == rel_src &&
                         step0_rows_read != null && step0_rows_read.tag == INT && step0_rows_read.int_val == 1 &&
                         step0_rows_emitted != null && step0_rows_emitted.tag == INT && step0_rows_emitted.int_val == 1 &&
                         step0_index_hits != null && step0_index_hits.tag == INT && step0_index_hits.int_val == 0 &&
                         step0_index_misses != null && step0_index_misses.tag == INT && step0_index_misses.int_val == 0 &&
                         step0_join_probes != null && step0_join_probes.tag == INT && step0_join_probes.int_val == 0 &&
                         step0_counter_kind != null && step0_counter_kind.tag == SYMBOL &&
                         step0_counter_kind.sym_val == sym_observed &&
                         step1_rows_read != null && step1_rows_read.tag == INT && step1_rows_read.int_val == 4 &&
                         step1_rows_emitted != null && step1_rows_emitted.tag == INT && step1_rows_emitted.int_val == 1 &&
                         step1_index_hits != null && step1_index_hits.tag == INT && step1_index_hits.int_val == 0 &&
                         step1_index_misses != null && step1_index_misses.tag == INT && step1_index_misses.int_val == 0 &&
                         step1_join_probes != null && step1_join_probes.tag == INT && step1_join_probes.int_val == 1 &&
                         step1_counter_kind != null && step1_counter_kind.tag == SYMBOL &&
                         step1_counter_kind.sym_val == sym_observed;
                }
            }
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce analyze exposes observed per-step operator counters");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce analyze exposes observed per-step operator counters");
            (*fail)++;
        }
    }

    if (deduce_rule_test_capture_recursive_limit_code(interp, 300, "deduce/analyze-iteration-limit")) {
        emit_pass_literal("[PASS] deduce analyze recursive limit reports stable error code");
        (*pass)++;
    } else {
        io::printn("[FAIL] deduce analyze recursive limit reports stable error code");
        (*fail)++;
    }

    {
        char[32768] limit_err_buf;
        bool built_err = deduce_rule_test_build_recursive_limit_error_expr(
            &limit_err_buf[0],
            limit_err_buf.len,
            300
        );
        if (built_err) {
            test_error_contains(
                interp,
                "deduce analyze recursive limit reports stable error message",
                limit_err_buf[..advanced_test_cstr_len(&limit_err_buf)],
                "recursive stratum iteration limit reached",
                pass,
                fail
            );
        } else {
            io::printn("[FAIL] deduce analyze recursive limit reports stable error message (builder failed)");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-naive (deduce/open 'memory)) "
            "  (define [relation db-naive] parent-n (src) (dst)) "
            "  (define [relation db-naive] ancestor-n (src) (dst)) "
            "  (deduce/fact! parent-n \"A\" \"B\") "
            "  (deduce/fact! parent-n \"B\" \"C\") "
            "  (deduce/fact! parent-n \"C\" \"D\") "
            "  (deduce/rule! db-naive '(ancestor-n ?x ?y) '(parent-n ?x ?y)) "
            "  (deduce/rule! db-naive '(ancestor-n ?x ?z) '(parent-n ?x ?y) '(ancestor-n ?y ?z)) "
            "  (define an (deduce/analyze db-naive 'naive)) "
            "  (define cn (deduce/count ancestor-n)) "
            "  (define db-semi (deduce/open 'memory)) "
            "  (define [relation db-semi] parent-s (src) (dst)) "
            "  (define [relation db-semi] ancestor-s (src) (dst)) "
            "  (deduce/fact! parent-s \"A\" \"B\") "
            "  (deduce/fact! parent-s \"B\" \"C\") "
            "  (deduce/fact! parent-s \"C\" \"D\") "
            "  (deduce/rule! db-semi '(ancestor-s ?x ?y) '(parent-s ?x ?y)) "
            "  (deduce/rule! db-semi '(ancestor-s ?x ?z) '(parent-s ?x ?y) '(ancestor-s ?y ?z)) "
            "  (define as (deduce/analyze db-semi 'semi-naive)) "
            "  (define cs (deduce/count ancestor-s)) "
            "  (define ok1 (= cn cs)) "
            "  (define ok2 (= cn 6)) "
            "  (define ok3 (= an.derived-facts as.derived-facts)) "
            "  (define ok4 (= an.execution-engine 'naive-scc)) "
            "  (define ok5 (= as.execution-engine 'semi-naive-scc)) "
            "  (and ok1 (and ok2 (and ok3 (and ok4 ok5)))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze naive and semi-naive engines produce parity on recursive closure");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze naive and semi-naive engines produce parity on recursive closure (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze naive and semi-naive engines produce parity on recursive closure");
            }
            (*fail)++;
        }
    }

    {
        DeduceTupleSupportTable table = {};
        defer deduce_tuple_support_table_release(&table);

        char[] recursive_key = "recursive-support";
        char[] seeded_key = "seeded-support";
        bool ok = true;
        long total = 0;
        bool became_present = false;
        bool became_absent = false;

        Value* err = deduce_tuple_support_table_adjust_recursive_count(
            &table,
            recursive_key.ptr,
            recursive_key.len,
            1,
            &total,
            &became_present,
            &became_absent,
            interp
        );
        ok = err == null && total == 1 && became_present && !became_absent;

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                recursive_key.ptr,
                recursive_key.len,
                1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null && total == 2 && !became_present && !became_absent;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                recursive_key.ptr,
                recursive_key.len,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null && total == 1 && !became_present && !became_absent;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                recursive_key.ptr,
                recursive_key.len,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null && total == 0 && !became_present && became_absent;
        }

        if (ok) {
            err = deduce_tuple_support_table_seed_base_tuple(
                &table,
                seeded_key.ptr,
                seeded_key.len,
                &total,
                interp
            );
            ok = err == null && total == 1 &&
                 deduce_tuple_support_table_total_count(&table, seeded_key.ptr, seeded_key.len, interp) == 1;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                seeded_key.ptr,
                seeded_key.len,
                1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null && total == 2 && !became_present && !became_absent;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                seeded_key.ptr,
                seeded_key.len,
                -1,
                &total,
                &became_present,
                &became_absent,
                interp
            );
            ok = err == null && total == 1 && !became_present && !became_absent &&
                 deduce_tuple_support_table_total_count(&table, seeded_key.ptr, seeded_key.len, interp) == 1;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce tuple support tables preserve zero-crossing semantics");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce tuple support tables preserve zero-crossing semantics");
            (*fail)++;
        }
    }
}
