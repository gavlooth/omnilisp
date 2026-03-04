module lisp;

import std::core::mem;

// ============================================================
// Relation definition + query
// ============================================================
fn Value* prim_define_relation(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return raise_error(interp, "__define-relation: expected (cons db '(name col...))");

    // arg 0: (cons db (name col1 col2 ...))
    Value* pair = args[0];
    // fault: lisp::TYPE_MISMATCH
    if (!is_cons(pair)) return raise_error(interp, "__define-relation: expected cons pair");
    Value* db_val = car(pair);
    if (db_val == null || db_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return raise_error(interp, "__define-relation: car must be a deduce-open handle");
    }
    if (db_val.ffi_val == null) return raise_error(interp, "__define-relation: database handle is closed");
    DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;

    Value* spec = cdr(pair);
    // fault: lisp::TYPE_MISMATCH
    if (!is_cons(spec)) return raise_error(interp, "__define-relation: expected (name col...) in cdr");
    // fault: lisp::EXPECTED_SYMBOL
    if (car(spec).tag != SYMBOL) return raise_error(interp, "__define-relation: first element must be relation name");
    SymbolId rel_name = car(spec).sym_val;

    Relation* rel = (Relation*)mem::malloc(Relation.sizeof);
    rel.name = rel_name;
    rel.db = db;
    rel.col_count = 0;
    rel.key_col = 0;
    rel.index_col = 0;

    Value* cols = cdr(spec);
    while (is_cons(cols) && rel.col_count < MAX_RELATION_COLS) {
        if (car(cols).tag == SYMBOL) {
            rel.columns[rel.col_count++] = car(cols).sym_val;
        }
        cols = cdr(cols);
    }

    // Open LMDB named database for this relation
    char[] name_str = interp.symbols.get_name(rel_name);
    char[64] dbi_name;  // buffer: C-interop (keep) — null-terminated for mdb_dbi_open
    usz nlen = name_str.len < 63 ? name_str.len : 63;
    for (usz i = 0; i < nlen; i++) dbi_name[i] = name_str[i];
    dbi_name[nlen] = 0;

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS) {
        mem::free(rel);
        // fault: lisp::WRITE_FAILED
        return raise_error(interp, "__define-relation: txn begin failed");
    }

    rc = mdb_dbi_open(txn, &dbi_name, MDB_CREATE, &rel.dbi);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        mem::free(rel);
        // fault: lisp::WRITE_FAILED
        return raise_error(interp, "__define-relation: dbi open failed");
    }

    mdb_txn_commit(txn);

    Value* v = make_ffi_handle_ex(interp, (void*)rel, "deduce-relation", null, true, false);
    if (v == null) {
        mem::free(rel);
        return raise_error(interp, "__define-relation: out of memory");
    }
    return v;
}

// ============================================================
// (deduce-query relation filter-fn) → filtered list of dicts
// filter-fn takes a dict, returns truthy/nil
// ============================================================

fn Value* prim_deduce_query(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2) return raise_error(interp, "deduce-query: expected (deduce-query relation filter-fn)");

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return raise_error(interp, "deduce-query: first argument must be a relation");
    }
    if (rel_val.ffi_val == null) return raise_error(interp, "deduce-query: relation handle is closed");
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;

    Value* filter_fn = args[1];

    // Scan all tuples and filter
    Value* all = relation_scan_all(rel, interp);
    Value* result = make_nil(interp);
    Value*[256] matches;
    usz match_count = 0;

    Value* curr = all;
    while (is_cons(curr) && match_count < 256) {
        Value* row = car(curr);
        // Apply filter function to row
        Value* keep = jit_apply_value(filter_fn, row, interp);
        if (keep != null && keep.tag != NIL) {
            matches[match_count++] = row;
        }
        curr = cdr(curr);
    }

    // Build result list in order
    for (isz i = (isz)match_count - 1; i >= 0; i--) {
        result = make_cons(interp, matches[(usz)i], result);
    }
    return result;
}
