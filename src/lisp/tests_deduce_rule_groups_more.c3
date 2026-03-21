module lisp;

import std::io;
import std::core::mem;

fn void run_deduce_rule_validation_tests_more(Interp* interp, int* pass, int* fail) {
    {
        SymbolId sym_memory = interp.symbols.intern("memory");
        Value*[1] open_args;
        open_args[0] = deduce_rule_test_make_symbol_value(interp, sym_memory);
        Value* db_val = prim_deduce_open(open_args[..], null, interp);

        bool ok = db_val != null &&
                  db_val.tag == FFI_HANDLE &&
                  db_val.ffi_val != null &&
                  db_val.ffi_val.lib_handle != null;
        SymbolId rel_outer_sym = interp.symbols.intern("join_outer_exact");
        SymbolId rel_inner_sym = interp.symbols.intern("join_inner_exact");
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
            o2[1] = make_string(interp, "A");
            o2[2] = make_string(interp, "C");
            Value*[3] o3;
            o3[0] = outer_rel_val;
            o3[1] = make_string(interp, "B");
            o3[2] = make_string(interp, "D");
            Value*[3] i1;
            i1[0] = inner_rel_val;
            i1[1] = make_string(interp, "A");
            i1[2] = make_string(interp, "B");
            Value*[3] i2;
            i2[0] = inner_rel_val;
            i2[1] = make_string(interp, "B");
            i2[2] = make_string(interp, "D");
            Value* r1 = prim_assert_fact(o1[..], null, interp);
            Value* r2 = prim_assert_fact(o2[..], null, interp);
            Value* r3 = prim_assert_fact(o3[..], null, interp);
            Value* r4 = prim_assert_fact(i1[..], null, interp);
            Value* r5 = prim_assert_fact(i2[..], null, interp);
            ok = r1 != null && r1.tag == VOID &&
                 r2 != null && r2.tag == VOID &&
                 r3 != null && r3.tag == VOID &&
                 r4 != null && r4.tag == VOID &&
                 r5 != null && r5.tag == VOID;
        }

        if (ok) {
            Relation* outer_rel = (Relation*)outer_rel_val.ffi_val.lib_handle;
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            usz[2] outer_cols = { 0, 1 };
            usz[2] inner_cols = { 0, 1 };
            long join_count = 0;
            DeduceJoinExecStats stats = {};
            bool joined = deduce_index_nested_loop_join_count(
                outer_rel,
                inner_rel,
                outer_cols[..],
                inner_cols[..],
                {},
                &join_count,
                &stats,
                interp
            );
            bool join_ok = joined &&
                stats.used_index &&
                stats.outer_rows == 3 &&
                stats.inner_probes == 3 &&
                stats.rows_emitted == 2 &&
                join_count == 2;
            ok = join_ok;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce index nested-loop join exact probe");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce index nested-loop join exact probe");
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
        SymbolId rel_outer_sym = interp.symbols.intern("join_outer_lit");
        SymbolId rel_inner_sym = interp.symbols.intern("join_inner_lit");
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
            o2[1] = make_string(interp, "A");
            o2[2] = make_string(interp, "C");
            Value*[3] o3;
            o3[0] = outer_rel_val;
            o3[1] = make_string(interp, "B");
            o3[2] = make_string(interp, "D");
            Value*[3] i1;
            i1[0] = inner_rel_val;
            i1[1] = make_string(interp, "A");
            i1[2] = make_string(interp, "B");
            Value*[3] i2;
            i2[0] = inner_rel_val;
            i2[1] = make_string(interp, "A");
            i2[2] = make_string(interp, "C");
            Value* r1 = prim_assert_fact(o1[..], null, interp);
            Value* r2 = prim_assert_fact(o2[..], null, interp);
            Value* r3 = prim_assert_fact(o3[..], null, interp);
            Value* r4 = prim_assert_fact(i1[..], null, interp);
            Value* r5 = prim_assert_fact(i2[..], null, interp);
            ok = r1 != null && r1.tag == VOID &&
                 r2 != null && r2.tag == VOID &&
                 r3 != null && r3.tag == VOID &&
                 r4 != null && r4.tag == VOID &&
                 r5 != null && r5.tag == VOID;
        }

        Value*[] static_vals = {};
        if (ok) {
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            if (inner_rel == null || inner_rel.col_count != 2) {
                ok = false;
            } else {
                static_vals = mem::new_array(Value*, inner_rel.col_count);
                if (static_vals.ptr == null) {
                    ok = false;
                } else {
                    for (usz i = 0; i < static_vals.len; i++) static_vals[i] = null;
                    static_vals[0] = make_string(interp, "A");
                }
            }
        }
        defer if (static_vals.ptr != null) mem::free(static_vals.ptr);

        if (ok) {
            Relation* outer_rel = (Relation*)outer_rel_val.ffi_val.lib_handle;
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            usz[1] outer_cols = { 1 };
            usz[1] inner_cols = { 1 };
            long join_count = 0;
            DeduceJoinExecStats stats = {};
            bool joined = deduce_index_nested_loop_join_count(
                outer_rel,
                inner_rel,
                outer_cols[..],
                inner_cols[..],
                static_vals,
                &join_count,
                &stats,
                interp
            );
            bool join_ok = joined &&
                stats.used_index &&
                stats.outer_rows == 3 &&
                stats.inner_probes == 3 &&
                stats.rows_emitted == 2 &&
                join_count == 2;
            ok = join_ok;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce index nested-loop join literal+bound probe");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce index nested-loop join literal+bound probe");
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
        SymbolId rel_outer_sym = interp.symbols.intern("join_outer_hash");
        SymbolId rel_inner_sym = interp.symbols.intern("join_inner_hash");
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
            o2[1] = make_string(interp, "A");
            o2[2] = make_string(interp, "C");
            Value*[3] o3;
            o3[0] = outer_rel_val;
            o3[1] = make_string(interp, "B");
            o3[2] = make_string(interp, "D");
            Value*[3] i1;
            i1[0] = inner_rel_val;
            i1[1] = make_string(interp, "A");
            i1[2] = make_string(interp, "X1");
            Value*[3] i2;
            i2[0] = inner_rel_val;
            i2[1] = make_string(interp, "A");
            i2[2] = make_string(interp, "X2");
            Value*[3] i3;
            i3[0] = inner_rel_val;
            i3[1] = make_string(interp, "B");
            i3[2] = make_string(interp, "X3");
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
            usz[1] outer_cols = { 0 };
            usz[1] inner_cols = { 0 };
            long join_count = 0;
            DeduceJoinExecStats stats = {};
            bool joined = deduce_hash_join_count(
                outer_rel,
                inner_rel,
                outer_cols[..],
                inner_cols[..],
                {},
                &join_count,
                &stats,
                interp
            );
            ok = joined &&
                !stats.used_index &&
                stats.outer_rows == 3 &&
                stats.inner_probes == 3 &&
                stats.rows_emitted == 5 &&
                join_count == 5;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce hash join multi-match probe");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce hash join multi-match probe");
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
        SymbolId rel_outer_sym = interp.symbols.intern("join_outer_hash_lit");
        SymbolId rel_inner_sym = interp.symbols.intern("join_inner_hash_lit");
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
            o2[1] = make_string(interp, "A");
            o2[2] = make_string(interp, "C");
            Value*[3] o3;
            o3[0] = outer_rel_val;
            o3[1] = make_string(interp, "B");
            o3[2] = make_string(interp, "D");
            Value*[3] i1;
            i1[0] = inner_rel_val;
            i1[1] = make_string(interp, "A");
            i1[2] = make_string(interp, "B");
            Value*[3] i2;
            i2[0] = inner_rel_val;
            i2[1] = make_string(interp, "A");
            i2[2] = make_string(interp, "C");
            Value*[3] i3;
            i3[0] = inner_rel_val;
            i3[1] = make_string(interp, "A");
            i3[2] = make_string(interp, "D");
            Value*[3] i4;
            i4[0] = inner_rel_val;
            i4[1] = make_string(interp, "B");
            i4[2] = make_string(interp, "D");
            Value* r1 = prim_assert_fact(o1[..], null, interp);
            Value* r2 = prim_assert_fact(o2[..], null, interp);
            Value* r3 = prim_assert_fact(o3[..], null, interp);
            Value* r4 = prim_assert_fact(i1[..], null, interp);
            Value* r5 = prim_assert_fact(i2[..], null, interp);
            Value* r6 = prim_assert_fact(i3[..], null, interp);
            Value* r7 = prim_assert_fact(i4[..], null, interp);
            ok = r1 != null && r1.tag == VOID &&
                 r2 != null && r2.tag == VOID &&
                 r3 != null && r3.tag == VOID &&
                 r4 != null && r4.tag == VOID &&
                 r5 != null && r5.tag == VOID &&
                 r6 != null && r6.tag == VOID &&
                 r7 != null && r7.tag == VOID;
        }

        Value*[] static_vals = {};
        if (ok) {
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            if (inner_rel == null || inner_rel.col_count != 2) {
                ok = false;
            } else {
                static_vals = mem::new_array(Value*, inner_rel.col_count);
                if (static_vals.ptr == null) {
                    ok = false;
                } else {
                    for (usz i = 0; i < static_vals.len; i++) static_vals[i] = null;
                    static_vals[0] = make_string(interp, "A");
                }
            }
        }
        defer if (static_vals.ptr != null) mem::free(static_vals.ptr);

        if (ok) {
            Relation* outer_rel = (Relation*)outer_rel_val.ffi_val.lib_handle;
            Relation* inner_rel = (Relation*)inner_rel_val.ffi_val.lib_handle;
            usz[1] outer_cols = { 1 };
            usz[1] inner_cols = { 1 };
            long join_count = 0;
            DeduceJoinExecStats stats = {};
            bool joined = deduce_hash_join_count(
                outer_rel,
                inner_rel,
                outer_cols[..],
                inner_cols[..],
                static_vals,
                &join_count,
                &stats,
                interp
            );
            ok = joined &&
                !stats.used_index &&
                stats.outer_rows == 3 &&
                stats.inner_probes == 3 &&
                stats.rows_emitted == 3 &&
                join_count == 3;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce hash join literal+bound probe");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce hash join literal+bound probe");
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
        SymbolId rel_outer_sym = interp.symbols.intern("join_outer_adaptive_hash");
        SymbolId rel_inner_sym = interp.symbols.intern("join_inner_adaptive_hash");
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
            o2[2] = make_string(interp, "B");
            Value*[3] o3;
            o3[0] = outer_rel_val;
            o3[1] = make_string(interp, "C");
            o3[2] = make_string(interp, "B");
            Value*[3] o4;
            o4[0] = outer_rel_val;
            o4[1] = make_string(interp, "D");
            o4[2] = make_string(interp, "B");
            Value*[3] o5;
            o5[0] = outer_rel_val;
            o5[1] = make_string(interp, "E");
            o5[2] = make_string(interp, "B");
            Value*[3] i1;
            i1[0] = inner_rel_val;
            i1[1] = make_string(interp, "A");
            i1[2] = make_string(interp, "X");
            Value* r1 = prim_assert_fact(o1[..], null, interp);
            Value* r2 = prim_assert_fact(o2[..], null, interp);
            Value* r3 = prim_assert_fact(o3[..], null, interp);
            Value* r4 = prim_assert_fact(o4[..], null, interp);
            Value* r5 = prim_assert_fact(o5[..], null, interp);
            Value* r6 = prim_assert_fact(i1[..], null, interp);
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
                4,
                70,
                &join_count,
                &stats,
                interp
            );
            ok = joined &&
                stats.used_hash &&
                !stats.used_index &&
                stats.fallback_applied &&
                stats.outer_rows == 5 &&
                join_count == 1;
        }

        if (ok) {
            emit_pass_literal("[PASS] deduce adaptive join fallback to hash");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce adaptive join fallback to hash");
            (*fail)++;
        }
    }

    run_deduce_rule_validation_tests_more_join(interp, pass, fail);
}
