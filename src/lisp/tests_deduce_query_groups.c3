module lisp;


import std::core::mem;
import std::io;

fn void run_deduce_scan_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run("(length (deduce 'scan person))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 2) {
            emit_pass_literal("[PASS] deduce 'fact! + 'scan");
            (*pass)++;
        } else {
            io::printfn("[FAIL] deduce 'fact! + 'scan (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }

    setup(interp, "(deduce 'retract! person \"Bob\" 25)");
    {
        EvalResult r = run("(length (deduce 'scan person))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 1) {
            emit_pass_literal("[PASS] deduce 'retract!");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'retract!");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("('name (car (deduce 'scan person)))", interp);
        if (!r.error.has_error && is_string(r.value)) {
            emit_pass_literal("[PASS] deduce 'scan dict result");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'scan dict result");
            (*fail)++;
        }
    }
}

fn void run_deduce_scan_range_tests(Interp* interp, int* pass, int* fail) {
    setup(interp,
        "(begin "
        "  (define scan-range-db (deduce 'open 'memory)) "
        "  (define [relation scan-range-db] scan_range_person (name) (age) (email)) "
        "  (deduce 'fact! scan_range_person \"Alice\" 20 \"a@x\") "
        "  (deduce 'fact! scan_range_person \"Bob\" 30 \"b@x\") "
        "  (deduce 'fact! scan_range_person \"Carol\" 40 \"c@x\") "
        "  (deduce 'fact! scan_range_person \"Dora\" 50 \"d@x\") "
        ")");

    {
        EvalResult r = run("(length (deduce 'scan-range scan_range_person '(\"Bob\" 30 \"b@x\") '(\"Carol\" 40 \"c@x\")))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 2) {
            emit_pass_literal("[PASS] deduce 'scan-range bounded count");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] deduce 'scan-range bounded count (got %d)",
                is_int(r.value) ? (int)r.value.int_val : -1
            );
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(length (deduce 'scan-range scan_range_person '(\"Zed\" 99 \"z@x\") '(\"Zoe\" 100 \"z@x\")))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 0) {
            emit_pass_literal("[PASS] deduce 'scan-range empty");
            (*pass)++;
        } else {
            io::printfn(
                "[FAIL] deduce 'scan-range empty (got %d)",
                is_int(r.value) ? (int)r.value.int_val : -1
            );
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(deduce 'scan-range scan_range_person \"bad\" '(\"Carol\" 40 \"c@x\"))", interp);
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce 'scan-range lower bound not list");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'scan-range lower bound not list");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(deduce 'scan-range scan_range_person '(\"Bob\" 30) '(\"Carol\" 40 \"c@x\"))", interp);
        if (r.error.has_error) {
            emit_pass_literal("[PASS] deduce 'scan-range lower bound arity");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'scan-range lower bound arity");
            (*fail)++;
        }
    }
}

fn void run_deduce_query_filter_tests(Interp* interp, int* pass, int* fail) {
    {
        EvalResult r = run("(deduce 'count person)", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 1) {
            emit_pass_literal("[PASS] deduce 'count");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'count");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(length (deduce 'query person (lambda (row) (> ('age row) 28))))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val >= 1) {
            emit_pass_literal("[PASS] deduce 'query filter");
            (*pass)++;
        } else {
            io::printfn("[FAIL] deduce 'query filter (got %d)", is_int(r.value) ? (int)r.value.int_val : -1);
            (*fail)++;
        }
    }

    {
        EvalResult r = run("(length (deduce 'query person (lambda (row) false)))", interp);
        if (!r.error.has_error && is_int(r.value) && r.value.int_val == 0) {
            emit_pass_literal("[PASS] deduce 'query false predicate");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'query false predicate");
            (*fail)++;
        }
    }

    {
        EvalResult r = run("('name (car (deduce 'query person (lambda (row) (> ('age row) 28)))))", interp);
        if (!r.error.has_error && is_string(r.value)) {
            emit_pass_literal("[PASS] deduce 'query dict result");
            (*pass)++;
        } else {
            io::printn("[FAIL] deduce 'query dict result");
            (*fail)++;
        }
    }
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
            if (insert == null || insert.tag != NIL) {
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
        if (insert == null || insert.tag != NIL) {
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

fn void run_deduce_query_tests(Interp* interp, int* pass, int* fail) {
    run_deduce_scan_tests(interp, pass, fail);
    run_deduce_scan_range_tests(interp, pass, fail);
    run_deduce_query_filter_tests(interp, pass, fail);
    run_deduce_match_tests(interp, pass, fail);
}
