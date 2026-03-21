module lisp;

import std::io;
import std::core::mem;

fn bool deduce_rule_test_build_recursive_limit_expr(char* buf, usz cap, usz fact_count, char[] payload_expr) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(block ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (define db-limit (deduce/open 'memory)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (define [relation db-limit] parent-limit (src) (dst)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (define [relation db-limit] ancestor-limit (src) (dst)) ")) return false;
    for (usz i = 0; i + 1 < fact_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " (deduce/fact! parent-limit \"")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
        if (!advanced_test_append(buf, cap, &len, "\" \"")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i + 1)) return false;
        if (!advanced_test_append(buf, cap, &len, "\")")) return false;
    }
    if (!advanced_test_append(buf, cap, &len, " (deduce/rule! db-limit '(ancestor-limit ?x ?y) '(parent-limit ?x ?y)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (deduce/rule! db-limit '(ancestor-limit ?x ?z) '(parent-limit ?x ?y) '(ancestor-limit ?y ?z)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, "(handle (deduce/analyze db-limit) (raise msg ")) return false;
    if (!advanced_test_append(buf, cap, &len, payload_expr)) return false;
    if (!advanced_test_append(buf, cap, &len, "))")) return false;
    if (!advanced_test_append(buf, cap, &len, ")")) return false;
    return true;
}

fn bool deduce_rule_test_build_recursive_limit_error_expr(char* buf, usz cap, usz fact_count) {
    usz len = 0;
    if (!advanced_test_append(buf, cap, &len, "(block ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (define db-limit (deduce/open 'memory)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (define [relation db-limit] parent-limit (src) (dst)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (define [relation db-limit] ancestor-limit (src) (dst)) ")) return false;
    for (usz i = 0; i + 1 < fact_count; i++) {
        if (!advanced_test_append(buf, cap, &len, " (deduce/fact! parent-limit \"")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i)) return false;
        if (!advanced_test_append(buf, cap, &len, "\" \"")) return false;
        if (!advanced_test_append_int(buf, cap, &len, i + 1)) return false;
        if (!advanced_test_append(buf, cap, &len, "\")")) return false;
    }
    if (!advanced_test_append(buf, cap, &len, " (deduce/rule! db-limit '(ancestor-limit ?x ?y) '(parent-limit ?x ?y)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, " (deduce/rule! db-limit '(ancestor-limit ?x ?z) '(parent-limit ?x ?y) '(ancestor-limit ?y ?z)) ")) return false;
    if (!advanced_test_append(buf, cap, &len, "(deduce/analyze db-limit)")) return false;
    if (!advanced_test_append(buf, cap, &len, ")")) return false;
    return true;
}

fn void run_deduce_rule_validation_tests_more_tail(Interp* interp, int* pass, int* fail) {
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
        char[] db_path = io::bprintf(
            &db_path_buf,
            "/tmp/omni-deduce-index-open-%d-%d",
            (int)pid,
            (int)seq
        )!!;

        char[320] cleanup_cmd_buf;
        char[] cleanup_cmd = io::bprintf(&cleanup_cmd_buf, "rm -rf %s", (ZString)db_path)!!;
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
            "  (define ok2 (= ('mode a1) 'naive-bottom-up)) "
            "  (define ok3 (= ('status a1) 'ok)) "
            "  (define ok4 (= ('execution-engine a1) 'semi-naive-scc)) "
            "  (define ok5 (= ('mode a2) 'naive-bottom-up)) "
            "  (define ok6 (= ('execution-engine a2) 'semi-naive-scc)) "
            "  (define ok7 (= ('strata-count a1) 2)) "
            "  (define ok8 (= ('recursive-strata a1) 1)) "
            "  (define ok9 (>= ('iterations a1) 1)) "
            "  (define ok10 (= ('iteration-limit a1) 256)) "
            "  (define ok11 (>= ('max-component-iterations a1) 1)) "
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

    {
        char[32768] limit_code_buf;
        bool built_code = deduce_rule_test_build_recursive_limit_expr(
            &limit_code_buf[0],
            limit_code_buf.len,
            300,
            "(symbol->string (ref msg 'code))"
        );
        if (built_code) {
            test_str_val(
                interp,
                "deduce analyze recursive limit reports stable error code",
                limit_code_buf[..advanced_test_cstr_len(&limit_code_buf)],
                "deduce/analyze-iteration-limit",
                pass,
                fail
            );
        } else {
            io::printn("[FAIL] deduce analyze recursive limit reports stable error code (builder failed)");
            (*fail)++;
        }
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
            "  (define ok3 (= ('derived-facts an) ('derived-facts as))) "
            "  (define ok4 (= ('execution-engine an) 'naive-scc)) "
            "  (define ok5 (= ('execution-engine as) 'semi-naive-scc)) "
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

    {
        DeduceAggregateGroups groups = {};
        defer deduce_aggregate_groups_release(&groups);

        DeduceRuleTermKind[2] head_kinds = {
            DEDUCE_RULE_TERM_VARIABLE,
            DEDUCE_RULE_TERM_AGGREGATE
        };
        DeduceAggregateOpKind[2] aggregate_ops = {
            DEDUCE_AGGREGATE_NONE,
            DEDUCE_AGGREGATE_COUNT
        };
        uint[2] head_var_ids = { 0, 1 };
        Value*[2] head_literals = { null, null };
        DeduceRuleSignature sig = {
            .head_term_kinds = &head_kinds[0],
            .head_term_aggregate_ops = &aggregate_ops[0],
            .head_term_var_ids = &head_var_ids[0],
            .head_term_literals = &head_literals[0],
            .head_term_count = 2,
        };

        bool[2] var_bound = { true, true };
        Value*[2] var_values = {
            make_string(interp, "g"),
            make_int(interp, 7)
        };
        DeduceAggregateGroup* group = null;
        bool ok = deduce_aggregate_group_find_or_add(
            &groups,
            &sig,
            var_bound[..],
            var_values[..],
            &group,
            interp
        ) == null && group != null;

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                var_values[..],
                false,
                interp
            ) == null &&
            group.support_count == 1 &&
            group.term_states[1].initialized &&
            group.term_states[1].count_value == 1;
        }

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                var_values[..],
                false,
                interp
            ) == null &&
            group.support_count == 2 &&
            group.term_states[1].count_value == 2;
        }

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                var_values[..],
                true,
                interp
            ) == null &&
            group.support_count == 1 &&
            group.term_states[1].initialized &&
            group.term_states[1].count_value == 1;
        }

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                var_values[..],
                true,
                interp
            ) == null &&
            group.support_count == 0 &&
            !group.term_states[1].initialized &&
            group.term_states[1].count_value == 0;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce aggregate count groups support signed add/remove transitions");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce aggregate count groups support signed add/remove transitions");
            (*fail)++;
        }
    }

    {
        DeduceAggregateGroups groups = {};
        defer deduce_aggregate_groups_release(&groups);

        DeduceRuleTermKind[2] head_kinds = {
            DEDUCE_RULE_TERM_VARIABLE,
            DEDUCE_RULE_TERM_AGGREGATE
        };
        DeduceAggregateOpKind[2] aggregate_ops = {
            DEDUCE_AGGREGATE_NONE,
            DEDUCE_AGGREGATE_MIN
        };
        uint[2] head_var_ids = { 0, 1 };
        Value*[2] head_literals = { null, null };
        DeduceRuleSignature sig = {
            .head_term_kinds = &head_kinds[0],
            .head_term_aggregate_ops = &aggregate_ops[0],
            .head_term_var_ids = &head_var_ids[0],
            .head_term_literals = &head_literals[0],
            .head_term_count = 2,
        };

        bool[2] var_bound = { true, true };
        Value*[2] low_values = {
            make_string(interp, "g"),
            make_int(interp, 2)
        };
        Value*[2] high_values = {
            make_string(interp, "g"),
            make_int(interp, 5)
        };
        DeduceAggregateGroup* group = null;
        bool ok = deduce_aggregate_group_find_or_add(
            &groups,
            &sig,
            var_bound[..],
            low_values[..],
            &group,
            interp
        ) == null && group != null;

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                high_values[..],
                false,
                interp
            ) == null &&
            deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                low_values[..],
                false,
                interp
            ) == null &&
            group.support_count == 2 &&
            group.term_states[1].initialized &&
            group.term_states[1].extremum_value != null &&
            group.term_states[1].extremum_value.tag == INT &&
            group.term_states[1].extremum_value.int_val == 2;
        }

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                low_values[..],
                true,
                interp
            ) == null &&
            group.support_count == 1 &&
            group.term_states[1].initialized &&
            group.term_states[1].extremum_value != null &&
            group.term_states[1].extremum_value.tag == INT &&
            group.term_states[1].extremum_value.int_val == 5;
        }

        if (ok) {
            ok = deduce_aggregate_group_note_binding_with_polarity(
                group,
                &sig,
                var_bound[..],
                high_values[..],
                true,
                interp
            ) == null &&
            group.support_count == 0 &&
            !group.term_states[1].initialized &&
            group.term_states[1].extremum_value == null;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce aggregate min groups recompute extrema on removals");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce aggregate min groups recompute extrema on removals");
            (*fail)++;
        }
    }

    {
        Value* db_naive = null;
        Value* out_naive = null;
        usz expected_naive = 0;
        Value* db_semi = null;
        Value* out_semi = null;
        usz expected_semi = 0;
        bool ok = deduce_query_bench_seed_recursive_aggregate_db(interp, 4, &db_naive, &out_naive, &expected_naive);
        if (ok) {
            ok = deduce_query_bench_seed_recursive_aggregate_db(interp, 4, &db_semi, &out_semi, &expected_semi) &&
                 expected_naive == expected_semi;
        }

        Value* analyzed_naive = null;
        Value* analyzed_semi = null;
        if (ok) {
            Value*[2] analyze_naive_args;
            analyze_naive_args[0] = db_naive;
            analyze_naive_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("naive"));
            analyzed_naive = prim_deduce_analyze(analyze_naive_args[..], null, interp);

            Value*[2] analyze_semi_args;
            analyze_semi_args[0] = db_semi;
            analyze_semi_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("semi-naive"));
            analyzed_semi = prim_deduce_analyze(analyze_semi_args[..], null, interp);

            ok = analyzed_naive != null && analyzed_naive.tag == HASHMAP && analyzed_naive.hashmap_val != null &&
                 analyzed_semi != null && analyzed_semi.tag == HASHMAP && analyzed_semi.hashmap_val != null;
        }

        if (ok) {
            Value* engine_naive = deduce_rule_test_dict_get(interp, analyzed_naive, "execution-engine");
            Value* requested_naive = deduce_rule_test_dict_get(interp, analyzed_naive, "requested-execution-engine");
            Value* fallback_naive = deduce_rule_test_dict_get(interp, analyzed_naive, "recursive-aggregate-naive-fallback");
            Value* routed_naive = deduce_rule_test_dict_get(
                interp,
                analyzed_naive,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value* engine_semi = deduce_rule_test_dict_get(interp, analyzed_semi, "execution-engine");
            Value* requested_semi = deduce_rule_test_dict_get(interp, analyzed_semi, "requested-execution-engine");
            Value* fallback_semi = deduce_rule_test_dict_get(interp, analyzed_semi, "recursive-aggregate-naive-fallback");
            Value* routed_semi = deduce_rule_test_dict_get(
                interp,
                analyzed_semi,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value*[1] count_naive_args;
            count_naive_args[0] = out_naive;
            Value* count_naive = prim_deduce_count(count_naive_args[..], null, interp);
            Value*[1] count_semi_args;
            count_semi_args[0] = out_semi;
            Value* count_semi = prim_deduce_count(count_semi_args[..], null, interp);

            ok = engine_naive != null && engine_naive.tag == SYMBOL &&
                 engine_naive.sym_val == interp.symbols.intern("naive-scc") &&
                 requested_naive != null && requested_naive.tag == SYMBOL &&
                 requested_naive.sym_val == interp.symbols.intern("naive-scc") &&
                 fallback_naive != null && fallback_naive.tag == NIL &&
                 routed_naive != null && routed_naive.tag == NIL &&
                 engine_semi != null && engine_semi.tag == SYMBOL &&
                 engine_semi.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 requested_semi != null && requested_semi.tag == SYMBOL &&
                 requested_semi.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 fallback_semi != null && fallback_semi.tag == NIL &&
                 routed_semi != null && routed_semi.tag == NIL &&
                 count_naive != null && count_naive.tag == INT && count_naive.int_val == 4 &&
                 count_semi != null && count_semi.tag == INT && count_semi.int_val == 4;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce recursive aggregate-only SCCs keep pure engine selectors off mixed-route diagnostics");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce recursive aggregate-only SCCs keep pure engine selectors off mixed-route diagnostics");
            (*fail)++;
        }
    }

    {
        DeduceTupleSupportTable table = {};
        defer deduce_tuple_support_table_release(&table);

        char[] recursive_key = "support-recursive-only";
        char[] base_key = "support-base-seeded";

        bool ok = true;
        bool became_present = false;
        bool became_absent = false;
        long total = -1;

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
        ok = err == null &&
             total == 1 &&
             became_present &&
             !became_absent &&
             deduce_tuple_support_table_total_count(&table, recursive_key.ptr, recursive_key.len, interp) == 1;

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
            ok = err == null &&
                 total == 2 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_total_count(&table, recursive_key.ptr, recursive_key.len, interp) == 2;
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
            ok = err == null &&
                 total == 1 &&
                 !became_present &&
                 !became_absent &&
                 deduce_tuple_support_table_total_count(&table, recursive_key.ptr, recursive_key.len, interp) == 1;
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
            ok = err == null &&
                 total == 0 &&
                 !became_present &&
                 became_absent &&
                 deduce_tuple_support_table_total_count(&table, recursive_key.ptr, recursive_key.len, interp) == 0;
        }

        if (ok) {
            err = deduce_tuple_support_table_seed_base_tuple(&table, base_key.ptr, base_key.len, &total, interp);
            ok = err == null &&
                 total == 1 &&
                 deduce_tuple_support_table_lookup_count(table.base_counts, base_key.ptr, base_key.len, interp) == 1 &&
                 deduce_tuple_support_table_lookup_count(table.recursive_counts, base_key.ptr, base_key.len, interp) == 0;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                base_key.ptr,
                base_key.len,
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
                 deduce_tuple_support_table_lookup_count(table.base_counts, base_key.ptr, base_key.len, interp) == 1 &&
                 deduce_tuple_support_table_lookup_count(table.recursive_counts, base_key.ptr, base_key.len, interp) == 1;
        }

        if (ok) {
            err = deduce_tuple_support_table_adjust_recursive_count(
                &table,
                base_key.ptr,
                base_key.len,
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
                 deduce_tuple_support_table_lookup_count(table.base_counts, base_key.ptr, base_key.len, interp) == 1 &&
                 deduce_tuple_support_table_lookup_count(table.recursive_counts, base_key.ptr, base_key.len, interp) == 0;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce tuple support tables preserve zero-crossing transitions");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce tuple support tables preserve zero-crossing transitions");
            (*fail)++;
        }
    }

    {
        DeduceSemiNaiveRuleExec exec = {
            .aggregate_component_mode = true,
            .anchor_delta_is_removal = false,
            .interp = interp,
        };

        bool[] var_bound = mem::new_array(bool, 2);
        Value*[] var_values = mem::new_array(Value*, 2);
        bool ok = var_bound.ptr != null && var_values.ptr != null;
        defer if (var_bound.ptr != null) mem::free(var_bound.ptr);
        defer if (var_values.ptr != null) mem::free(var_values.ptr);

        if (ok) {
            for (usz i = 0; i < var_bound.len; i++) {
                var_bound[i] = false;
                var_values[i] = null;
            }
            var_bound[0] = true;
            var_bound[1] = true;
            var_values[0] = make_int(interp, 7);
            var_values[1] = make_string(interp, "alpha");

            bool already_seen = false;
            Value* err = deduce_seminaive_mark_current_proof_seen(&exec, var_bound, var_values, &already_seen);
            ok = err == null && !already_seen;

            if (ok) {
                err = deduce_seminaive_mark_current_proof_seen(&exec, var_bound, var_values, &already_seen);
                ok = err == null && already_seen;
            }

            if (ok) {
                exec.anchor_delta_is_removal = true;
                err = deduce_seminaive_mark_current_proof_seen(&exec, var_bound, var_values, &already_seen);
                ok = err == null && !already_seen;
            }

            if (ok) {
                exec.anchor_delta_is_removal = false;
                var_values[1] = make_string(interp, "beta");
                err = deduce_seminaive_mark_current_proof_seen(&exec, var_bound, var_values, &already_seen);
                ok = err == null && !already_seen;
            }
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce seminaive proof dedupe keys normalized bindings and polarity");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce seminaive proof dedupe keys normalized bindings and polarity");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-naive (deduce/open 'memory)) "
            "  (define [relation db-naive] tail-mixed-naive-edge (src) (dst)) "
            "  (define [relation db-naive] tail-mixed-naive-unit (node) (weight)) "
            "  (define [relation db-naive] tail-mixed-naive-score (node) (total)) "
            "  (define [relation db-naive] tail-mixed-naive-chosen (node) (total)) "
            "  (deduce/rule! db-naive '(tail-mixed-naive-chosen ?x ?n) '(tail-mixed-naive-score ?x ?n)) "
            "  (deduce/rule! db-naive '(tail-mixed-naive-score ?x (sum ?n)) '(tail-mixed-naive-unit ?x ?n)) "
            "  (deduce/rule! db-naive '(tail-mixed-naive-score ?x (sum ?n)) '(tail-mixed-naive-edge ?x ?y) '(tail-mixed-naive-chosen ?y ?n)) "
            "  (deduce/fact! tail-mixed-naive-unit 0 1) "
            "  (deduce/fact! tail-mixed-naive-edge 0 1) "
            "  (deduce/fact! tail-mixed-naive-unit 1 1) "
            "  (deduce/fact! tail-mixed-naive-edge 1 2) "
            "  (deduce/fact! tail-mixed-naive-unit 2 1) "
            "  (deduce/fact! tail-mixed-naive-edge 2 3) "
            "  (deduce/fact! tail-mixed-naive-unit 3 1) "
            "  (define db-semi (deduce/open 'memory)) "
            "  (define [relation db-semi] tail-mixed-semi-edge (src) (dst)) "
            "  (define [relation db-semi] tail-mixed-semi-unit (node) (weight)) "
            "  (define [relation db-semi] tail-mixed-semi-score (node) (total)) "
            "  (define [relation db-semi] tail-mixed-semi-chosen (node) (total)) "
            "  (deduce/rule! db-semi '(tail-mixed-semi-chosen ?x ?n) '(tail-mixed-semi-score ?x ?n)) "
            "  (deduce/rule! db-semi '(tail-mixed-semi-score ?x (sum ?n)) '(tail-mixed-semi-unit ?x ?n)) "
            "  (deduce/rule! db-semi '(tail-mixed-semi-score ?x (sum ?n)) '(tail-mixed-semi-edge ?x ?y) '(tail-mixed-semi-chosen ?y ?n)) "
            "  (deduce/fact! tail-mixed-semi-unit 0 1) "
            "  (deduce/fact! tail-mixed-semi-edge 0 1) "
            "  (deduce/fact! tail-mixed-semi-unit 1 1) "
            "  (deduce/fact! tail-mixed-semi-edge 1 2) "
            "  (deduce/fact! tail-mixed-semi-unit 2 1) "
            "  (deduce/fact! tail-mixed-semi-edge 2 3) "
            "  (deduce/fact! tail-mixed-semi-unit 3 1) "
            "  (define a-naive (deduce/analyze db-naive 'naive)) "
            "  (define a-semi (deduce/analyze db-semi 'semi-naive)) "
            "  (define ok1 (= ('execution-engine a-naive) 'semi-naive-scc)) "
            "  (define ok2 (= ('requested-execution-engine a-naive) 'naive-scc)) "
            "  (define ok3 (= ('recursive-aggregate-naive-fallback a-naive) nil)) "
            "  (define ok4 (= ('naive-recursive-aggregate-mixed-routed-to-seminaive a-naive) true)) "
            "  (define ok5 (= ('execution-engine a-semi) 'semi-naive-scc)) "
            "  (define ok6 (= ('requested-execution-engine a-semi) 'semi-naive-scc)) "
            "  (define ok7 (= ('recursive-aggregate-naive-fallback a-semi) nil)) "
            "  (define ok8 (= ('naive-recursive-aggregate-mixed-routed-to-seminaive a-semi) nil)) "
            "  (define ok9 (= (deduce/count tail-mixed-semi-score) 4)) "
            "  (define ok10 (= (deduce/count tail-mixed-semi-chosen) 4)) "
            "  (and ok1 (and ok2 (and ok3 (and ok4 (and ok5 (and ok6 (and ok7 (and ok8 (and ok9 ok10))))))))))"
            ,
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce mixed positive recursive aggregate SCCs run on semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce mixed positive recursive aggregate SCCs run on semi-naive-scc");
            (*fail)++;
        }
    }

    {
        Value* db_naive = null;
        Value* out_naive = null;
        usz expected_naive = 0;
        Value* db_semi = null;
        Value* out_semi = null;
        usz expected_semi = 0;
        bool ok = deduce_query_bench_seed_recursive_aggregate_negated_db(interp, 4, &db_naive, &out_naive, &expected_naive);
        if (ok) {
            ok = deduce_query_bench_seed_recursive_aggregate_negated_db(interp, 4, &db_semi, &out_semi, &expected_semi) &&
                 expected_naive == expected_semi;
        }

        Value* analyzed_naive = null;
        Value* analyzed_semi = null;
        if (ok) {
            Value*[2] analyze_naive_args;
            analyze_naive_args[0] = db_naive;
            analyze_naive_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("naive"));
            analyzed_naive = prim_deduce_analyze(analyze_naive_args[..], null, interp);

            Value*[2] analyze_semi_args;
            analyze_semi_args[0] = db_semi;
            analyze_semi_args[1] = deduce_rule_test_make_symbol_value(interp, interp.symbols.intern("semi-naive"));
            analyzed_semi = prim_deduce_analyze(analyze_semi_args[..], null, interp);

            ok = analyzed_naive != null && analyzed_naive.tag == HASHMAP && analyzed_naive.hashmap_val != null &&
                 analyzed_semi != null && analyzed_semi.tag == HASHMAP && analyzed_semi.hashmap_val != null;
        }

        if (ok) {
            Value* engine_naive = deduce_rule_test_dict_get(interp, analyzed_naive, "execution-engine");
            Value* requested_naive = deduce_rule_test_dict_get(interp, analyzed_naive, "requested-execution-engine");
            Value* fallback_naive = deduce_rule_test_dict_get(interp, analyzed_naive, "recursive-aggregate-naive-fallback");
            Value* routed_naive = deduce_rule_test_dict_get(
                interp,
                analyzed_naive,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value* engine_semi = deduce_rule_test_dict_get(interp, analyzed_semi, "execution-engine");
            Value* requested_semi = deduce_rule_test_dict_get(interp, analyzed_semi, "requested-execution-engine");
            Value* fallback_semi = deduce_rule_test_dict_get(interp, analyzed_semi, "recursive-aggregate-naive-fallback");
            Value* routed_semi = deduce_rule_test_dict_get(
                interp,
                analyzed_semi,
                "naive-recursive-aggregate-mixed-routed-to-seminaive"
            );
            Value*[1] count_naive_args;
            count_naive_args[0] = out_naive;
            Value* count_naive = prim_deduce_count(count_naive_args[..], null, interp);
            Value*[1] count_semi_args;
            count_semi_args[0] = out_semi;
            Value* count_semi = prim_deduce_count(count_semi_args[..], null, interp);

            ok = engine_naive != null && engine_naive.tag == SYMBOL &&
                 engine_naive.sym_val == interp.symbols.intern("naive-scc") &&
                 requested_naive != null && requested_naive.tag == SYMBOL &&
                 requested_naive.sym_val == interp.symbols.intern("naive-scc") &&
                 fallback_naive != null && fallback_naive.tag == NIL &&
                 routed_naive != null && routed_naive.tag == NIL &&
                 engine_semi != null && engine_semi.tag == SYMBOL &&
                 engine_semi.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 requested_semi != null && requested_semi.tag == SYMBOL &&
                 requested_semi.sym_val == interp.symbols.intern("semi-naive-scc") &&
                 fallback_semi != null && fallback_semi.tag == NIL &&
                 routed_semi != null && routed_semi.tag == NIL &&
                 count_naive != null && count_naive.tag == INT && count_naive.int_val == (long)expected_naive &&
                 count_semi != null && count_semi.tag == INT && count_semi.int_val == (long)expected_semi;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce negated recursive aggregate SCCs keep pure engine selectors off mixed-route diagnostics");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce negated recursive aggregate SCCs keep pure engine selectors off mixed-route diagnostics");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-naive (deduce/open 'memory)) "
            "  (define [relation db-naive] tail-mixed-neg-naive-edge (src) (dst)) "
            "  (define [relation db-naive] tail-mixed-neg-naive-unit (node) (weight)) "
            "  (define [relation db-naive] tail-mixed-neg-naive-blocked (node)) "
            "  (define [relation db-naive] tail-mixed-neg-naive-score (node) (total)) "
            "  (define [relation db-naive] tail-mixed-neg-naive-chosen (node) (total)) "
            "  (deduce/rule! db-naive '(tail-mixed-neg-naive-chosen ?x ?n) '(tail-mixed-neg-naive-score ?x ?n) '(not (tail-mixed-neg-naive-blocked ?x))) "
            "  (deduce/rule! db-naive '(tail-mixed-neg-naive-score ?x (sum ?n)) '(tail-mixed-neg-naive-unit ?x ?n)) "
            "  (deduce/rule! db-naive '(tail-mixed-neg-naive-score ?x (sum ?n)) '(tail-mixed-neg-naive-edge ?x ?y) '(tail-mixed-neg-naive-chosen ?y ?n)) "
            "  (deduce/fact! tail-mixed-neg-naive-unit 0 1) "
            "  (deduce/fact! tail-mixed-neg-naive-edge 0 1) "
            "  (deduce/fact! tail-mixed-neg-naive-unit 1 1) "
            "  (deduce/fact! tail-mixed-neg-naive-edge 1 2) "
            "  (deduce/fact! tail-mixed-neg-naive-unit 2 1) "
            "  (deduce/fact! tail-mixed-neg-naive-edge 2 3) "
            "  (deduce/fact! tail-mixed-neg-naive-unit 3 1) "
            "  (deduce/fact! tail-mixed-neg-naive-blocked 3) "
            "  (define db-semi (deduce/open 'memory)) "
            "  (define [relation db-semi] tail-mixed-neg-semi-edge (src) (dst)) "
            "  (define [relation db-semi] tail-mixed-neg-semi-unit (node) (weight)) "
            "  (define [relation db-semi] tail-mixed-neg-semi-blocked (node)) "
            "  (define [relation db-semi] tail-mixed-neg-semi-score (node) (total)) "
            "  (define [relation db-semi] tail-mixed-neg-semi-chosen (node) (total)) "
            "  (deduce/rule! db-semi '(tail-mixed-neg-semi-chosen ?x ?n) '(tail-mixed-neg-semi-score ?x ?n) '(not (tail-mixed-neg-semi-blocked ?x))) "
            "  (deduce/rule! db-semi '(tail-mixed-neg-semi-score ?x (sum ?n)) '(tail-mixed-neg-semi-unit ?x ?n)) "
            "  (deduce/rule! db-semi '(tail-mixed-neg-semi-score ?x (sum ?n)) '(tail-mixed-neg-semi-edge ?x ?y) '(tail-mixed-neg-semi-chosen ?y ?n)) "
            "  (deduce/fact! tail-mixed-neg-semi-unit 0 1) "
            "  (deduce/fact! tail-mixed-neg-semi-edge 0 1) "
            "  (deduce/fact! tail-mixed-neg-semi-unit 1 1) "
            "  (deduce/fact! tail-mixed-neg-semi-edge 1 2) "
            "  (deduce/fact! tail-mixed-neg-semi-unit 2 1) "
            "  (deduce/fact! tail-mixed-neg-semi-edge 2 3) "
            "  (deduce/fact! tail-mixed-neg-semi-unit 3 1) "
            "  (deduce/fact! tail-mixed-neg-semi-blocked 3) "
            "  (define a-naive (deduce/analyze db-naive 'naive)) "
            "  (define a-semi (deduce/analyze db-semi 'semi-naive)) "
            "  (define ok1 (= ('execution-engine a-naive) 'semi-naive-scc)) "
            "  (define ok2 (= ('requested-execution-engine a-naive) 'naive-scc)) "
            "  (define ok3 (= ('recursive-aggregate-naive-fallback a-naive) nil)) "
            "  (define ok4 (= ('naive-recursive-aggregate-mixed-routed-to-seminaive a-naive) true)) "
            "  (define ok5 (= ('execution-engine a-semi) 'semi-naive-scc)) "
            "  (define ok6 (= ('requested-execution-engine a-semi) 'semi-naive-scc)) "
            "  (define ok7 (= ('recursive-aggregate-naive-fallback a-semi) nil)) "
            "  (define ok8 (= ('naive-recursive-aggregate-mixed-routed-to-seminaive a-semi) nil)) "
            "  (define ok9 (= (deduce/count tail-mixed-neg-semi-score) 4)) "
            "  (define ok10 (= (deduce/count tail-mixed-neg-semi-chosen) 3)) "
            "  (and ok1 (and ok2 (and ok3 (and ok4 (and ok5 (and ok6 (and ok7 (and ok8 (and ok9 ok10))))))))))"
            ,
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce mixed negated recursive aggregate SCCs run on semi-naive-scc");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce mixed negated recursive aggregate SCCs run on semi-naive-scc");
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-strata (deduce/open 'memory)) "
            "  (define [relation db-strata] base-neg (x)) "
            "  (define [relation db-strata] deny-neg (x)) "
            "  (define [relation db-strata] allow-neg (x)) "
            "  (deduce/fact! base-neg \"A\") "
            "  (deduce/rule! db-strata '(deny-neg ?x) '(base-neg ?x)) "
            "  (deduce/rule! db-strata '(allow-neg ?x) '(base-neg ?x) '(not (deny-neg ?x))) "
            "  (define a (deduce/analyze db-strata)) "
            "  (define ok1 (= (deduce/count deny-neg) 1)) "
            "  (define ok2 (= (deduce/count allow-neg) 0)) "
            "  (define ok3 (>= ('stratum-count a) 2)) "
            "  (and ok1 (and ok2 ok3)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze enforces negation stratum order");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze enforces negation stratum order (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze enforces negation stratum order");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-mixed (deduce/open 'memory)) "
            "  (define [relation db-mixed] edge-m (src) (dst)) "
            "  (define [relation db-mixed] reach-m (src) (dst)) "
            "  (define [relation db-mixed] banned-m (node)) "
            "  (define [relation db-mixed] reach-ok-m (src) (dst)) "
            "  (deduce/fact! edge-m \"A\" \"B\") "
            "  (deduce/fact! edge-m \"B\" \"C\") "
            "  (deduce/fact! banned-m \"C\") "
            "  (deduce/rule! db-mixed '(reach-m ?x ?y) '(edge-m ?x ?y)) "
            "  (deduce/rule! db-mixed '(reach-m ?x ?z) '(edge-m ?x ?y) '(reach-m ?y ?z)) "
            "  (deduce/rule! db-mixed '(reach-ok-m ?x ?y) '(reach-m ?x ?y) '(not (banned-m ?y))) "
            "  (define a (deduce/analyze db-mixed)) "
            "  (define ok1 (= (deduce/count reach-m) 3)) "
            "  (define ok2 (= (deduce/count reach-ok-m) 1)) "
            "  (define ok3 (>= ('stratum-count a) 2)) "
            "  (and ok1 (and ok2 ok3)))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze handles mixed recursive and negated stratum boundaries");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze handles mixed recursive and negated stratum boundaries (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze handles mixed recursive and negated stratum boundaries");
            }
            (*fail)++;
        }
    }

    {
        EvalResult r = run(
            "(block "
            "  (define db-inc (deduce/open 'memory)) "
            "  (define [relation db-inc] base-inc (x)) "
            "  (define [relation db-inc] derived-inc (x)) "
            "  (deduce/rule! db-inc '(derived-inc ?x) '(base-inc ?x)) "
            "  (define a0 (deduce/analyze db-inc)) "
            "  (deduce/fact! base-inc \"A\") "
            "  (define a1 (deduce/analyze db-inc)) "
            "  (define tx1 (deduce 'block db-inc 'write)) "
            "  (deduce/fact! tx1 base-inc \"B\") "
            "  (deduce 'commit tx1) "
            "  (define a2 (deduce/analyze db-inc)) "
            "  (define tx2 (deduce 'block db-inc 'write)) "
            "  (deduce/fact! tx2 base-inc \"C\") "
            "  (deduce 'abort tx2) "
            "  (define a3 (deduce/analyze db-inc)) "
            "  (define ok1 (= ('incremental-dependency-edges a0) 1)) "
            "  (define ok2 (= ('incremental-dirty-predicate-count a0) 0)) "
            "  (define ok3 (= ('incremental-invalidation-mode a0) 'tracked)) "
            "  (define ok4 (>= ('incremental-dirty-predicate-count a1) 2)) "
            "  (define ok5 (= ('incremental-invalidation-mode a1) 'tracked)) "
            "  (define ok6 (= ('incremental-dirty-predicate-count a2) 2)) "
            "  (define ok7 (= ('incremental-invalidation-mode a2) 'tracked)) "
            "  (define ok8 (= (deduce/count base-inc) 2)) "
            "  (define ok8a (= (deduce/count derived-inc) 2)) "
            "  (define ok9 (= ('incremental-dirty-predicate-count a3) 0)) "
            "  (define ok10 (= ('incremental-invalidation-mode a3) 'tracked)) "
            "  (define ok11 (= (deduce/count derived-inc) 2)) "
            "  (and ok1 "
            "       (and ok2 "
            "            (and ok3 "
            "                 (and ok4 "
            "                      (and ok5 "
            "                           (and ok6 "
            "                                (and ok7 "
            "                                     (and ok8 "
            "                                          (and ok8a "
            "                                               (and ok9 "
            "                                                    (and ok10 ok11))))))))))))",
            interp
        );
        if (!r.error.has_error && is_symbol(r.value) && r.value.sym_val == interp.sym_true) {
            emit_pass_literal("[PASS] deduce analyze reports transaction-safe incremental commit and abort boundaries");
            (*pass)++;
        } else {
            if (r.error.has_error) {
                io::printfn("[FAIL] deduce analyze reports transaction-safe incremental commit and abort boundaries (error: %s)", (ZString)r.error.message[:256]);
            } else {
                io::printn("[FAIL] deduce analyze reports transaction-safe incremental commit and abort boundaries");
            }
            (*fail)++;
        }
    }
}
