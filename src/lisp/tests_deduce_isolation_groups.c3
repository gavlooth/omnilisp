module lisp;


import std::core::mem;
import std::io;

fn void run_deduce_memory_open_isolation_test(Interp* interp, int* pass, int* fail) {
    EvalResult ra = run("(deduce 'open 'memory)", interp);
    EvalResult rb = run("(deduce 'open 'memory)", interp);
    bool open_ok = !ra.error.has_error &&
                   !rb.error.has_error &&
                   ra.value != null && rb.value != null &&
                   ra.value.tag == FFI_HANDLE &&
                   rb.value.tag == FFI_HANDLE &&
                   ra.value.ffi_val != null &&
                   rb.value.ffi_val != null &&
                   ra.value.ffi_val != rb.value.ffi_val;
    if (!open_ok) {
        io::printn("[FAIL] deduce memory open isolation (open)");
        (*fail)++;
        return;
    }

    SymbolId rel_a_sym = interp.symbols.intern("person_a_iso");
    SymbolId rel_b_sym = interp.symbols.intern("person_b_iso");
    SymbolId name_sym = interp.symbols.intern("name");
    SymbolId age_sym = interp.symbols.intern("age");

    Value* rel_a = setup_deduce_relation_for_db(interp, ra.value, rel_a_sym, name_sym, age_sym);
    Value* rel_b = setup_deduce_relation_for_db(interp, rb.value, rel_b_sym, name_sym, age_sym);
    bool rel_ok = rel_a != null && rel_b != null &&
                  rel_a.tag == FFI_HANDLE && rel_b.tag == FFI_HANDLE &&
                  rel_a.ffi_val != null && rel_b.ffi_val != null;
    if (!rel_ok) {
        io::printn("[FAIL] deduce memory open isolation (relation setup)");
        (*fail)++;
        return;
    }

    env_define_with_barrier(interp.global_env, rel_a_sym, rel_a, interp);
    env_define_with_barrier(interp.global_env, rel_b_sym, rel_b, interp);

    EvalResult ins = run("(deduce 'fact! person_a_iso \"Alice\" 30)", interp);
    EvalResult ca = run("(deduce 'count person_a_iso)", interp);
    EvalResult cb = run("(deduce 'count person_b_iso)", interp);

    bool count_ok = !ins.error.has_error &&
                    !ca.error.has_error &&
                    !cb.error.has_error &&
                    is_int(ca.value) && ca.value.int_val == 1 &&
                    is_int(cb.value) && cb.value.int_val == 0;

    if (count_ok) {
        emit_pass_literal("[PASS] deduce memory open isolation");
        (*pass)++;
    } else {
        io::printfn("[FAIL] deduce memory open isolation (ins_err=%s count_a=%d count_b=%d)",
            ins.error.has_error ? "yes" : "no",
            is_int(ca.value) ? (int)ca.value.int_val : -1,
            is_int(cb.value) ? (int)cb.value.int_val : -1);
        (*fail)++;
    }
}

fn void run_deduce_reopen_stress_test(Interp* interp, int* pass, int* fail) {
    bool ok = true;
    isz failed_iter = -1;

    for (usz i = 0; i < 64; i++) {
        EvalResult r = run(
            "(block "
            "  (define ddb-reopen (deduce 'open 'memory)) "
            "  ddb-reopen)",
            interp
        );
        Value* h = r.value;
        if (r.error.has_error || h == null || h.tag != FFI_HANDLE || h.ffi_val == null) {
            ok = false;
            failed_iter = (isz)i;
            break;
        }
    }

    if (ok) {
        emit_pass_literal("[PASS] deduce repeated open/rebind");
        (*pass)++;
    } else {
        io::printfn("[FAIL] deduce repeated open/rebind (iter %d)", (int)failed_iter);
        (*fail)++;
    }
}

fn void run_deduce_large_relation_schema_test(Interp* interp, int* pass, int* fail) {
    EvalResult open = run("(deduce 'open 'memory)", interp);
    if (open.error.has_error || open.value == null || open.value.tag != FFI_HANDLE || open.value.ffi_val == null) {
        io::printn("[FAIL] deduce relation schema >16 columns (open)");
        (*fail)++;
        return;
    }

    const usz COL_COUNT = 20;
    SymbolId[] col_syms = mem::new_array(SymbolId, COL_COUNT);
    Value*[] define_args = mem::new_array(Value*, COL_COUNT + 2);
    Value*[] fact_args = mem::new_array(Value*, COL_COUNT + 1);
    if (col_syms.ptr == null || define_args.ptr == null || fact_args.ptr == null) {
        if (col_syms.ptr != null) mem::free(col_syms.ptr);
        if (define_args.ptr != null) mem::free(define_args.ptr);
        if (fact_args.ptr != null) mem::free(fact_args.ptr);
        io::printn("[FAIL] deduce relation schema >16 columns (alloc)");
        (*fail)++;
        return;
    }
    defer mem::free(col_syms.ptr);
    defer mem::free(define_args.ptr);
    defer mem::free(fact_args.ptr);

    col_syms[0] = interp.symbols.intern("c0");
    col_syms[1] = interp.symbols.intern("c1");
    col_syms[2] = interp.symbols.intern("c2");
    col_syms[3] = interp.symbols.intern("c3");
    col_syms[4] = interp.symbols.intern("c4");
    col_syms[5] = interp.symbols.intern("c5");
    col_syms[6] = interp.symbols.intern("c6");
    col_syms[7] = interp.symbols.intern("c7");
    col_syms[8] = interp.symbols.intern("c8");
    col_syms[9] = interp.symbols.intern("c9");
    col_syms[10] = interp.symbols.intern("c10");
    col_syms[11] = interp.symbols.intern("c11");
    col_syms[12] = interp.symbols.intern("c12");
    col_syms[13] = interp.symbols.intern("c13");
    col_syms[14] = interp.symbols.intern("c14");
    col_syms[15] = interp.symbols.intern("c15");
    col_syms[16] = interp.symbols.intern("c16");
    col_syms[17] = interp.symbols.intern("c17");
    col_syms[18] = interp.symbols.intern("c18");
    col_syms[19] = interp.symbols.intern("c19");

    define_args[0] = open.value;
    define_args[1] = make_symbol(interp, interp.symbols.intern("wide_person"));
    for (usz i = 0; i < COL_COUNT; i++) {
        define_args[i + 2] = make_symbol(interp, col_syms[i]);
    }

    Value* rel_val = prim_define_relation(define_args, null, interp);
    if (rel_val == null || rel_val.tag != FFI_HANDLE || rel_val.ffi_val == null) {
        io::printn("[FAIL] deduce relation schema >16 columns (define)");
        (*fail)++;
        return;
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    fact_args[0] = rel_val;
    for (usz i = 0; i < COL_COUNT; i++) {
        fact_args[i + 1] = make_int(interp, (long)i);
    }

    Value* insert = prim_assert_fact(fact_args, null, interp);
    Value* rows = relation_scan_all(rel, interp);
    Value* row = is_cons(rows) ? car(rows) : null;
    Value* last = row != null && row.tag == HASHMAP
        ? hashmap_get(row.hashmap_val, make_symbol(interp, col_syms[COL_COUNT - 1]))
        : null;

    bool ok = rel != null &&
              rel.col_count == COL_COUNT &&
              rel.columns != null &&
              rel.columns[COL_COUNT - 1] == col_syms[COL_COUNT - 1] &&
              insert != null &&
              insert.tag != ERROR &&
              row != null &&
              row.tag == HASHMAP &&
              row.hashmap_val != null &&
              row.hashmap_val.count == COL_COUNT &&
              last != null &&
              last.tag == INT &&
              last.int_val == (long)(COL_COUNT - 1);

    if (ok) {
        emit_pass_literal("[PASS] deduce relation schema preserves >16 columns");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce relation schema preserves >16 columns (cols=%d last=%d count=%d)",
            rel != null ? (int)rel.col_count : -1,
            last != null && last.tag == INT ? (int)last.int_val : -1,
            row != null && row.tag == HASHMAP && row.hashmap_val != null ? (int)row.hashmap_val.count : -1
        );
        (*fail)++;
    }
}

fn void run_deduce_long_relation_name_isolation_test(Interp* interp, int* pass, int* fail) {
    EvalResult open = run("(deduce 'open 'memory)", interp);
    if (open.error.has_error || open.value == null || open.value.tag != FFI_HANDLE || open.value.ffi_val == null) {
        io::printn("[FAIL] deduce long relation name isolation (open)");
        (*fail)++;
        return;
    }

    SymbolId rel_a_name = deduce_test_make_long_relation_name(interp, 'a');
    SymbolId rel_b_name = deduce_test_make_long_relation_name(interp, 'b');
    SymbolId name_sym = interp.symbols.intern("name");
    SymbolId age_sym = interp.symbols.intern("age");

    Value* rel_a = setup_deduce_relation_for_db(interp, open.value, rel_a_name, name_sym, age_sym);
    Value* rel_b = setup_deduce_relation_for_db(interp, open.value, rel_b_name, name_sym, age_sym);
    if (rel_a == null || rel_b == null || rel_a.tag != FFI_HANDLE || rel_b.tag != FFI_HANDLE ||
        rel_a.ffi_val == null || rel_b.ffi_val == null) {
        io::printn("[FAIL] deduce long relation name isolation (define)");
        (*fail)++;
        return;
    }

    Value*[3] fact_a;
    fact_a[0] = rel_a;
    fact_a[1] = make_string(interp, "Alice");
    fact_a[2] = make_int(interp, 30);
    Value* insert_a = prim_assert_fact(fact_a[..], null, interp);

    Value*[1] count_args_a;
    count_args_a[0] = rel_a;
    Value*[1] count_args_b;
    count_args_b[0] = rel_b;
    Value* count_a_after_a = prim_deduce_count(count_args_a[..], null, interp);
    Value* count_b_after_a = prim_deduce_count(count_args_b[..], null, interp);

    Value*[3] fact_b;
    fact_b[0] = rel_b;
    fact_b[1] = make_string(interp, "Bob");
    fact_b[2] = make_int(interp, 25);
    Value* insert_b = prim_assert_fact(fact_b[..], null, interp);

    Value* count_a_final = prim_deduce_count(count_args_a[..], null, interp);
    Value* count_b_final = prim_deduce_count(count_args_b[..], null, interp);

    bool ok = insert_a != null && insert_a.tag != ERROR &&
              insert_b != null && insert_b.tag != ERROR &&
              count_a_after_a != null && count_a_after_a.tag == INT && count_a_after_a.int_val == 1 &&
              count_b_after_a != null && count_b_after_a.tag == INT && count_b_after_a.int_val == 0 &&
              count_a_final != null && count_a_final.tag == INT && count_a_final.int_val == 1 &&
              count_b_final != null && count_b_final.tag == INT && count_b_final.int_val == 1;

    if (ok) {
        emit_pass_literal("[PASS] deduce long relation name isolation");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce long relation name isolation (a1=%d b1=%d af=%d bf=%d)",
            count_a_after_a != null && count_a_after_a.tag == INT ? (int)count_a_after_a.int_val : -1,
            count_b_after_a != null && count_b_after_a.tag == INT ? (int)count_b_after_a.int_val : -1,
            count_a_final != null && count_a_final.tag == INT ? (int)count_a_final.int_val : -1,
            count_b_final != null && count_b_final.tag == INT ? (int)count_b_final.int_val : -1
        );
        (*fail)++;
    }
}

fn void run_deduce_long_open_path_isolation_test(Interp* interp, int* pass, int* fail) {
    Value* path_a = deduce_test_make_long_open_path(interp, 'a');
    Value* path_b = deduce_test_make_long_open_path(interp, 'b');
    Value*[1] open_args_a;
    Value*[1] open_args_b;
    open_args_a[0] = path_a;
    open_args_b[0] = path_b;

    Value* db_a = prim_deduce_open(open_args_a[..], null, interp);
    Value* db_b = prim_deduce_open(open_args_b[..], null, interp);
    if (db_a == null || db_b == null || db_a.tag != FFI_HANDLE || db_b.tag != FFI_HANDLE ||
        db_a.ffi_val == null || db_b.ffi_val == null) {
        io::printn("[FAIL] deduce long open path isolation (open)");
        (*fail)++;
        return;
    }

    SymbolId rel_name = interp.symbols.intern("path_iso_rel");
    SymbolId name_sym = interp.symbols.intern("name");
    SymbolId age_sym = interp.symbols.intern("age");
    Value* rel_a = setup_deduce_relation_for_db(interp, db_a, rel_name, name_sym, age_sym);
    Value* rel_b = setup_deduce_relation_for_db(interp, db_b, rel_name, name_sym, age_sym);
    if (rel_a == null || rel_b == null || rel_a.tag != FFI_HANDLE || rel_b.tag != FFI_HANDLE ||
        rel_a.ffi_val == null || rel_b.ffi_val == null) {
        io::printn("[FAIL] deduce long open path isolation (define)");
        (*fail)++;
        return;
    }

    Value*[3] fact_a;
    fact_a[0] = rel_a;
    fact_a[1] = make_string(interp, "Alice");
    fact_a[2] = make_int(interp, 31);
    Value* insert_a = prim_assert_fact(fact_a[..], null, interp);

    Value*[1] count_args_a;
    count_args_a[0] = rel_a;
    Value*[1] count_args_b;
    count_args_b[0] = rel_b;
    Value* count_a_after_a = prim_deduce_count(count_args_a[..], null, interp);
    Value* count_b_after_a = prim_deduce_count(count_args_b[..], null, interp);

    Value*[3] fact_b;
    fact_b[0] = rel_b;
    fact_b[1] = make_string(interp, "Bob");
    fact_b[2] = make_int(interp, 22);
    Value* insert_b = prim_assert_fact(fact_b[..], null, interp);

    Value* count_a_final = prim_deduce_count(count_args_a[..], null, interp);
    Value* count_b_final = prim_deduce_count(count_args_b[..], null, interp);

    bool ok = insert_a != null && insert_a.tag != ERROR &&
              insert_b != null && insert_b.tag != ERROR &&
              count_a_after_a != null && count_a_after_a.tag == INT && count_a_after_a.int_val == 1 &&
              count_b_after_a != null && count_b_after_a.tag == INT && count_b_after_a.int_val == 0 &&
              count_a_final != null && count_a_final.tag == INT && count_a_final.int_val == 1 &&
              count_b_final != null && count_b_final.tag == INT && count_b_final.int_val == 1;

    if (ok) {
        emit_pass_literal("[PASS] deduce long open path isolation");
        (*pass)++;
    } else {
        io::printfn(
            "[FAIL] deduce long open path isolation (a1=%d b1=%d af=%d bf=%d)",
            count_a_after_a != null && count_a_after_a.tag == INT ? (int)count_a_after_a.int_val : -1,
            count_b_after_a != null && count_b_after_a.tag == INT ? (int)count_b_after_a.int_val : -1,
            count_a_final != null && count_a_final.tag == INT ? (int)count_a_final.int_val : -1,
            count_b_final != null && count_b_final.tag == INT ? (int)count_b_final.int_val : -1
        );
        (*fail)++;
    }
}
