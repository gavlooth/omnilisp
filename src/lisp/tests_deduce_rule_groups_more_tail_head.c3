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
}
