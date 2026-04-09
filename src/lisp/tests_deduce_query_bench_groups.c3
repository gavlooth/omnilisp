module lisp;

import std::io;

fn bool deduce_bench_seed_relation(
    Interp* interp,
    Value** rel_out,
    usz* query_expected_out
) {
    if (interp == null || rel_out == null || query_expected_out == null) return false;
    *rel_out = null;
    *query_expected_out = 0;

    Value*[1] open_args;
    open_args[0] = make_symbol(interp, interp.symbols.intern("memory"));
    Value* db = prim_deduce_open(open_args[..], null, interp);
    if (db == null || db.tag != FFI_HANDLE || db.ffi_val == null) {
        io::printfn(
            "[deduce-debug] recursive-negated seed failed: db open (tag=%d msg=%s)",
            db != null ? (int)db.tag : -1,
            (ZString)(db != null && db.tag == ERROR ? db.str_chars[:db.str_len] : "<none>")
        );
        return false;
    }

    SymbolId rel_name = interp.symbols.intern("bench_person");
    SymbolId id_sym = interp.symbols.intern("id");
    SymbolId score_sym = interp.symbols.intern("score");
    Value* rel = setup_deduce_relation_for_db(interp, db, rel_name, id_sym, score_sym);
    if (rel == null || rel.tag != FFI_HANDLE || rel.ffi_val == null) return false;

    usz query_expected = 0;
    for (usz i = 0; i < DEDUCE_BENCH_CORPUS_ROWS; i++) {
        long score = (long)(i % 100);
        if (score > 50) query_expected++;

        Value*[3] fact_args;
        fact_args[0] = rel;
        fact_args[1] = make_int(interp, (long)i);
        fact_args[2] = make_int(interp, score);
        Value* inserted = prim_assert_fact(fact_args[..], null, interp);
        if (inserted == null || inserted.tag == ERROR) return false;
    }

    *rel_out = rel;
    *query_expected_out = query_expected;
    return true;
}

fn Value* deduce_query_bench_make_symbol_value(Interp* interp, SymbolId sym) {
    Value* v = interp.alloc_value();
    v.tag = SYMBOL;
    v.sym_val = sym;
    return v;
}

fn Value* deduce_query_bench_make_atom2(Interp* interp, SymbolId predicate, Value* a, Value* b) {
    Value* pred = deduce_query_bench_make_symbol_value(interp, predicate);
    return make_cons(interp, pred, make_cons(interp, a, make_cons(interp, b, make_nil(interp))));
}

fn Value* deduce_query_bench_make_atom1(Interp* interp, SymbolId predicate, Value* a) {
    Value* pred = deduce_query_bench_make_symbol_value(interp, predicate);
    return make_cons(interp, pred, make_cons(interp, a, make_nil(interp)));
}

fn Value* deduce_query_bench_setup_unary_relation(
    Interp* interp,
    Value* db_val,
    SymbolId rel_name,
    SymbolId col1
) {
    if (interp == null || db_val == null || db_val.tag != FFI_HANDLE || db_val.ffi_val == null) return null;

    Value* name_sym = interp.alloc_value();
    name_sym.tag = SYMBOL;
    name_sym.sym_val = rel_name;

    Value* c1_sym = interp.alloc_value();
    c1_sym.tag = SYMBOL;
    c1_sym.sym_val = col1;

    Value*[3] args;
    args[0] = db_val;
    args[1] = name_sym;
    args[2] = c1_sym;
    return prim_define_relation(args[..], null, interp);
}

fn Value* deduce_query_bench_dict_get(Interp* interp, Value* dict, char[] key) {
    if (dict == null || dict.tag != HASHMAP || dict.hashmap_val == null) return null;
    Value* key_val = make_symbol(interp, interp.symbols.intern(key));
    return hashmap_get(dict.hashmap_val, key_val);
}

fn usz deduce_query_bench_list_length(Value* rows) @inline {
    usz count = 0;
    Value* cur = rows;
    while (is_cons(cur)) {
        count++;
        cur = cdr(cur);
    }
    return count;
}

fn bool deduce_query_bench_value_is_symbol(Value* value, SymbolId expected) @inline {
    return value != null && value.tag == SYMBOL && value.sym_val == expected;
}

fn bool deduce_query_bench_value_is_int(Value* value, long expected) @inline {
    return value != null && value.tag == INT && value.int_val == expected;
}

fn bool deduce_query_bench_value_is_true(Interp* interp, Value* value) @inline {
    return value != null && value.tag == SYMBOL && value.sym_val == interp.sym_true;
}

