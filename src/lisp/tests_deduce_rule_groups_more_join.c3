module lisp;

import std::io;
import std::core::mem;

fn void run_deduce_rule_validation_tests_more_join(Interp* interp, int* pass, int* fail) {
    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        SymbolId rel_outer_sym = interp.symbols.intern("join_outer_adaptive_idx");
        SymbolId rel_inner_sym = interp.symbols.intern("join_inner_adaptive_idx");
        SymbolId col_a = interp.symbols.intern("a");
        SymbolId col_b = interp.symbols.intern("b");
        Value* outer_rel_val = null;
        Value* inner_rel_val = null;
        if (ok) {
            outer_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_outer_sym, col_a, col_b);
            inner_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_inner_sym, col_a, col_b);
            ok = outer_rel_val != null && outer_rel_val.tag == FFI_HANDLE &&
                 inner_rel_val != null && inner_rel_val.tag == FFI_HANDLE;
        }

        if (ok) {
            Value*[3] o1;
            o1[0] = outer_rel_val;
            o1[1] = make_string(interp, "A");
            o1[2] = make_string(interp, "B");
            Value*[3] o2;
            o2[0] = outer_rel_val;
            o2[1] = make_string(interp, "B");
            o2[2] = make_string(interp, "C");
            Value*[3] o3;
            o3[0] = outer_rel_val;
            o3[1] = make_string(interp, "C");
            o3[2] = make_string(interp, "D");
            Value*[3] i1;
            i1[0] = inner_rel_val;
            i1[1] = make_string(interp, "A");
            i1[2] = make_string(interp, "B");
            Value*[3] i2;
            i2[0] = inner_rel_val;
            i2[1] = make_string(interp, "B");
            i2[2] = make_string(interp, "C");
            Value*[3] i3;
            i3[0] = inner_rel_val;
            i3[1] = make_string(interp, "C");
            i3[2] = make_string(interp, "D");
            Value* r1 = prim_assert_fact(o1[..], null, interp);
            Value* r2 = prim_assert_fact(o2[..], null, interp);
            Value* r3 = prim_assert_fact(o3[..], null, interp);
            Value* r4 = prim_assert_fact(i1[..], null, interp);
            Value* r5 = prim_assert_fact(i2[..], null, interp);
            Value* r6 = prim_assert_fact(i3[..], null, interp);
            ok = r1 != null && r1.tag == VOID &&
                 r2 != null && r2.tag == VOID &&
                 r3 != null && r3.tag == VOID &&
                 r4 != null && r4.tag == VOID &&
                 r5 != null && r5.tag == VOID &&
                 r6 != null && r6.tag == VOID;
        }

        if (ok) {
            Relation* outer_rel = (Relation*)outer_rel_val.ffi_val.lib_handle;
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            usz[2] outer_cols = { 0, 1 };
            usz[2] inner_cols = { 0, 1 };
            long join_count = 0;
            DeduceJoinExecStats stats = {};
            bool joined = deduce_adaptive_join_count(
                outer_rel,
                inner_rel,
                outer_cols[..],
                inner_cols[..],
                {},
                3,
                70,
                &join_count,
                &stats,
                interp
            );
            ok = joined &&
                stats.used_index &&
                !stats.used_hash &&
                !stats.fallback_applied &&
                stats.index_hits == 3 &&
                stats.index_misses == 0 &&
                join_count == 3;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce adaptive join keeps index");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce adaptive join keeps index");
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
        SymbolId rel_outer_sym = interp.symbols.intern("join_outer_adaptive_default_route");
        SymbolId rel_inner_sym = interp.symbols.intern("join_inner_adaptive_default_route");
        SymbolId col_a = interp.symbols.intern("a");
        SymbolId col_b = interp.symbols.intern("b");
        Value* outer_rel_val = null;
        Value* inner_rel_val = null;
        if (ok) {
            outer_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_outer_sym, col_a, col_b);
            inner_rel_val = setup_deduce_relation_for_db(interp, db_val, rel_inner_sym, col_a, col_b);
            ok = outer_rel_val != null && outer_rel_val.tag == FFI_HANDLE &&
                 inner_rel_val != null && inner_rel_val.tag == FFI_HANDLE;
        }

        if (ok) {
            for (long i = 0; i < 24; i++) {
                char[2] key_buf;
                key_buf[0] = (char)('A' + i);
                key_buf[1] = 0;
                Value*[3] fact;
                fact[0] = outer_rel_val;
                fact[1] = make_string(interp, key_buf[:1]);
                fact[2] = make_int(interp, i);
                Value* inserted = prim_assert_fact(fact[..], null, interp);
                ok = inserted != null && inserted.tag == VOID;
                if (!ok) break;
            }
        }

        if (ok) {
            for (long i = 0; i < 64; i++) {
                Value*[3] fact;
                fact[0] = inner_rel_val;
                fact[1] = make_string(interp, "A");
                fact[2] = make_int(interp, i);
                Value* inserted = prim_assert_fact(fact[..], null, interp);
                ok = inserted != null && inserted.tag == VOID;
                if (!ok) break;
            }
        }

        if (ok) {
            Relation* outer_rel = (Relation*)outer_rel_val.ffi_val.lib_handle;
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            usz[1] outer_cols = { 0 };
            usz[1] inner_cols = { 0 };
            long join_count = 0;
            DeduceJoinExecStats stats = {};
            bool joined = deduce_adaptive_join_count(
                outer_rel,
                inner_rel,
                outer_cols[..],
                inner_cols[..],
                {},
                0,
                0,
                &join_count,
                &stats,
                interp
            );
            ok = joined &&
                stats.used_hash &&
                !stats.used_index &&
                !stats.fallback_applied &&
                stats.outer_rows == 24 &&
                stats.rows_emitted == 64 &&
                join_count == 64;
            if (!ok) {
                io::printfn(
                    "[FAIL-DETAIL] deduce adaptive join defaults to hash on low-selectivity shapes: joined=%d used_hash=%d used_index=%d fallback=%d outer_rows=%d rows_emitted=%d join_count=%d",
                    joined ? 1 : 0,
                    stats.used_hash ? 1 : 0,
                    stats.used_index ? 1 : 0,
                    stats.fallback_applied ? 1 : 0,
                    (int)stats.outer_rows,
                    (int)stats.rows_emitted,
                    (int)join_count
                );
            }
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce adaptive join defaults to hash on low-selectivity shapes");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce adaptive join defaults to hash on low-selectivity shapes");
            (*fail)++;
        }
    }
    run_deduce_rule_validation_tests_more_tail(interp, pass, fail);
}
