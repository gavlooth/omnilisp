module lisp;

import std::core::mem;

// ============================================================
// Relation definition + query
// ============================================================
fn Value* deduce_relation_extract_spec(
    Value* pair,
    Interp* interp,
    DeduceDb** db_out,
    SymbolId* rel_name_out,
    Value** cols_out
) {
    // arg 0: (cons db (name col1 col2 ...))
    // fault: lisp::TYPE_MISMATCH
    if (!is_cons(pair)) return raise_error(interp, "__define-relation: expected cons pair");

    Value* db_val = car(pair);
    if (db_val == null || db_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return raise_error(interp, "__define-relation: car must be a deduce-open handle");
    }
    if (db_val.ffi_val == null) return raise_error(interp, "__define-relation: database handle is closed");
    *db_out = (DeduceDb*)db_val.ffi_val.lib_handle;

    Value* spec = cdr(pair);
    // fault: lisp::TYPE_MISMATCH
    if (!is_cons(spec)) return raise_error(interp, "__define-relation: expected (name col...) in cdr");
    // fault: lisp::EXPECTED_SYMBOL
    if (car(spec).tag != SYMBOL) return raise_error(interp, "__define-relation: first element must be relation name");

    *rel_name_out = car(spec).sym_val;
    *cols_out = cdr(spec);
    return null;
}

fn Relation* deduce_relation_alloc(DeduceDb* db, SymbolId rel_name) {
    Relation* rel = (Relation*)mem::malloc(Relation.sizeof);
    rel.name = rel_name;
    rel.db = db;
    rel.col_count = 0;
    rel.key_col = 0;
    rel.index_col = 0;
    return rel;
}

fn void deduce_relation_collect_columns(Relation* rel, Value* cols) {
    Value* curr = cols;
    while (is_cons(curr) && rel.col_count < MAX_RELATION_COLS) {
        Value* col = car(curr);
        if (col.tag == SYMBOL) {
            rel.columns[rel.col_count++] = col.sym_val;
        }
        curr = cdr(curr);
    }
}

enum DeduceRelationOpenStatus {
    REL_OPEN_OK,
    REL_OPEN_TXN_BEGIN_FAILED,
    REL_OPEN_DBI_FAILED,
}

fn DeduceRelationOpenStatus deduce_relation_open_dbi(Relation* rel, Interp* interp) {
    // Open LMDB named database for this relation.
    char[] name_str = interp.symbols.get_name(rel.name);
    char[64] dbi_name;  // buffer: C-interop (keep) — null-terminated for mdb_dbi_open
    usz nlen = name_str.len < 63 ? name_str.len : 63;
    for (usz i = 0; i < nlen; i++) dbi_name[i] = name_str[i];
    dbi_name[nlen] = 0;

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS) return REL_OPEN_TXN_BEGIN_FAILED;

    rc = mdb_dbi_open(txn, &dbi_name, MDB_CREATE, &rel.dbi);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        return REL_OPEN_DBI_FAILED;
    }

    mdb_txn_commit(txn);
    return REL_OPEN_OK;
}

fn Value* prim_define_relation(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return raise_error(interp, "__define-relation: expected (cons db '(name col...))");

    DeduceDb* db = null;
    SymbolId rel_name = 0;
    Value* cols = null;
    Value* parse_err = deduce_relation_extract_spec(args[0], interp, &db, &rel_name, &cols);
    if (parse_err != null) return parse_err;

    Relation* rel = deduce_relation_alloc(db, rel_name);
    deduce_relation_collect_columns(rel, cols);
    DeduceRelationOpenStatus open_status = deduce_relation_open_dbi(rel, interp);
    if (open_status != REL_OPEN_OK) {
        mem::free(rel);
        if (open_status == REL_OPEN_TXN_BEGIN_FAILED) {
            // fault: lisp::WRITE_FAILED
            return raise_error(interp, "__define-relation: txn begin failed");
        }
        // fault: lisp::WRITE_FAILED
        return raise_error(interp, "__define-relation: dbi open failed");
    }

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
