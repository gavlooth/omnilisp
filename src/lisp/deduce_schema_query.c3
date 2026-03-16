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
    if (!is_cons(pair)) return deduce_raise(interp, "deduce/define-relation-expected-cons-pair", "__define-relation: expected cons pair");

    Value* db_val = car(pair);
    if (db_val == null || db_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return deduce_raise(interp, "deduce/define-relation-db-not-handle", "__define-relation: car must be a deduce-open handle");
    }
    if (db_val.ffi_val == null || db_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/define-relation-db-handle-closed", "__define-relation: database handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(db_val, "deduce-db")) {
        return deduce_raise(interp, "deduce/define-relation-db-not-deduce-db", "__define-relation: car must be a deduce database handle");
    }
    *db_out = (DeduceDb*)db_val.ffi_val.lib_handle;

    Value* spec = cdr(pair);
    // fault: lisp::TYPE_MISMATCH
    if (!is_cons(spec)) return deduce_raise(interp, "deduce/define-relation-expected-name-columns-list", "__define-relation: expected (name col...) in cdr");
    // fault: lisp::EXPECTED_SYMBOL
    if (car(spec).tag != SYMBOL) return deduce_raise(interp, "deduce/define-relation-name-not-symbol", "__define-relation: first element must be relation name");

    *rel_name_out = car(spec).sym_val;
    *cols_out = cdr(spec);
    return null;
}

fn Relation* deduce_relation_alloc(DeduceDb* db, SymbolId rel_name) {
    Relation* rel = (Relation*)mem::malloc(Relation.sizeof);
    if (rel == null) return null;
    rel.name = rel_name;
    rel.columns = null;
    rel.column_key_values = null;
    rel.db = db;
    rel.col_count = 0;
    rel.col_capacity = 0;
    rel.dbi = 0;
    rel.key_col = 0;
    rel.index_col = 0;
    rel.dropped = false;
    return rel;
}

fn bool deduce_relation_ensure_column_capacity(Relation* rel, usz needed) {
    if (needed <= rel.col_capacity) return true;

    usz new_cap = rel.col_capacity > 0 ? rel.col_capacity : 4;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    SymbolId* new_columns = (SymbolId*)mem::realloc(rel.columns, SymbolId.sizeof * new_cap);
    if (new_columns == null) return false;
    rel.columns = new_columns;
    rel.col_capacity = new_cap;
    return true;
}

fn Value* deduce_relation_collect_columns(Relation* rel, Value* cols, Interp* interp) {
    Value* curr = cols;
    while (is_cons(curr)) {
        Value* col = car(curr);
        if (col.tag == SYMBOL) {
            if (!deduce_relation_ensure_column_capacity(rel, rel.col_count + 1)) {
                return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
            }
            rel.columns[rel.col_count++] = col.sym_val;
        }
        curr = cdr(curr);
    }
    return null;
}

fn Value* deduce_relation_collect_columns_from_args(Relation* rel, Value*[] args, usz start, Interp* interp) {
    SymbolId sym_role_key = interp.symbols.intern("__relation-role-key");
    SymbolId sym_role_index = interp.symbols.intern("__relation-role-index");

    for (usz i = start; i < args.len;) {
        bool has_key_role = false;
        bool has_index_role = false;
        while (i < args.len) {
            Value* marker = args[i];
            if (marker == null || marker.tag != SYMBOL) break;
            if (marker.sym_val == sym_role_key) {
                has_key_role = true;
                i++;
                continue;
            }
            if (marker.sym_val == sym_role_index) {
                has_index_role = true;
                i++;
                continue;
            }
            break;
        }

        if (i >= args.len) {
            return deduce_raise(interp, "deduce/define-relation-column-not-symbol", "__define-relation: column arguments must be symbols");
        }

        Value* col = args[i];
        if (col == null || col.tag != SYMBOL) {
            return deduce_raise(interp, "deduce/define-relation-column-not-symbol", "__define-relation: column arguments must be symbols");
        }
        if (!deduce_relation_ensure_column_capacity(rel, rel.col_count + 1)) {
            return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
        }
        rel.columns[rel.col_count++] = col.sym_val;
        if (has_key_role) rel.key_col = col.sym_val;
        if (has_index_role) rel.index_col = col.sym_val;
        i++;
    }
    return null;
}

enum DeduceRelationOpenStatus {
    REL_OPEN_OK,
    REL_OPEN_OUT_OF_MEMORY,
    REL_OPEN_TXN_BEGIN_FAILED,
    REL_OPEN_DBI_FAILED,
}

fn DeduceRelationOpenStatus deduce_relation_open_dbi(Relation* rel, Interp* interp) {
    // Open LMDB named database for this relation.
    char[] name_str = interp.symbols.get_name(rel.name);
    return deduce_relation_open_dbi_with_name(rel, name_str);
}

fn DeduceRelationOpenStatus deduce_relation_open_dbi_with_name(Relation* rel, char[] name_str) {
    char* dbi_name = deduce_copy_c_string(name_str);
    if (dbi_name == null) return REL_OPEN_OUT_OF_MEMORY;
    defer mem::free(dbi_name);

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(rel.db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS) return REL_OPEN_TXN_BEGIN_FAILED;

    rc = mdb_dbi_open(txn, dbi_name, MDB_CREATE, &rel.dbi);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        return REL_OPEN_DBI_FAILED;
    }

    mdb_txn_commit(txn);
    return REL_OPEN_OK;
}

fn Value* prim_define_relation(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 1) return deduce_raise(interp, "deduce/define-relation-expected-spec", "__define-relation: expected (cons db '(name col...))");

    DeduceDb* db = null;
    SymbolId rel_name = 0;
    Value* cols = null;
    bool direct_form = args.len >= 2 &&
                       args[0] != null && args[0].tag == FFI_HANDLE &&
                       args[1] != null && args[1].tag == SYMBOL;
    if (direct_form) {
        if (args[0].ffi_val == null || args[0].ffi_val.lib_handle == null) {
            return deduce_raise(interp, "deduce/define-relation-db-handle-closed", "__define-relation: database handle is closed");
        }
        if (!deduce_ffi_handle_name_equals(args[0], "deduce-db")) {
            return deduce_raise(interp, "deduce/define-relation-db-not-deduce-db", "__define-relation: database handle must be a deduce database handle");
        }
        db = (DeduceDb*)args[0].ffi_val.lib_handle;
        rel_name = args[1].sym_val;
    } else {
        Value* parse_err = deduce_relation_extract_spec(args[0], interp, &db, &rel_name, &cols);
        if (parse_err != null) return parse_err;
    }

    Relation* rel = deduce_relation_alloc(db, rel_name);
    if (rel == null) return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    if (direct_form) {
        Value* col_err = deduce_relation_collect_columns_from_args(rel, args, 2, interp);
        if (col_err != null) {
            deduce_relation_free(rel);
            return col_err;
        }
    } else {
        Value* col_err = deduce_relation_collect_columns(rel, cols, interp);
        if (col_err != null) {
            deduce_relation_free(rel);
            return col_err;
        }
    }
    DeduceRelationOpenStatus open_status = deduce_relation_open_dbi(rel, interp);
    if (open_status != REL_OPEN_OK) {
        deduce_relation_free(rel);
        if (open_status == REL_OPEN_OUT_OF_MEMORY) {
            return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
        }
        if (open_status == REL_OPEN_TXN_BEGIN_FAILED) {
            // fault: lisp::WRITE_FAILED
            return deduce_raise(interp, "deduce/define-relation-txn-open-failed", "__define-relation: txn open failed");
        }
        // fault: lisp::WRITE_FAILED
        return deduce_raise(interp, "deduce/define-relation-dbi-open-failed", "__define-relation: dbi open failed");
    }

    DeduceSchemaRegisterStatus schema_status =
        deduce_db_register_relation_schema(db, rel_name, rel.columns, rel.col_count, rel.key_col, rel.index_col);
    if (schema_status != DEDUCE_SCHEMA_REGISTER_OK) {
        deduce_relation_free(rel);
        if (schema_status == DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT) {
            return deduce_raise(interp, "deduce/relation-arity-conflict", "__define-relation: relation name already registered with different arity");
        }
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }

    Value* v = make_ffi_handle_ex(interp, (void*)rel, "deduce-relation", &deduce_relation_finalizer, true, false);
    if (v == null) {
        deduce_relation_free(rel);
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }
    return v;
}

// (deduce 'open-named db relation-name db-name col...) -> relation handle
fn Value* prim_deduce_open_named(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 4) {
        return deduce_raise(interp, "deduce/open-named-expected-db-relation-dbname-columns", "deduce-open-named: expected (deduce 'open-named db relation-name db-name col...)");
    }

    Value* db_val = args[0];
    if (!deduce_ffi_handle_name_equals(db_val, "deduce-db")) {
        return deduce_raise(interp, "deduce/open-named-db-not-handle", "deduce-open-named: first argument must be a deduce database handle");
    }
    if (db_val.ffi_val == null || db_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/open-named-db-handle-closed", "deduce-open-named: database handle is closed");
    }
    DeduceDb* db = (DeduceDb*)db_val.ffi_val.lib_handle;
    if (!db.open || db.env == null) {
        return deduce_raise(interp, "deduce/open-named-db-not-open", "deduce-open-named: database is not open");
    }

    Value* rel_name_v = args[1];
    if (rel_name_v == null || rel_name_v.tag != SYMBOL) {
        return deduce_raise(interp, "deduce/open-named-relation-name-not-symbol", "deduce-open-named: relation-name must be a symbol");
    }

    char[] db_name = {};
    Value* db_name_v = args[2];
    if (db_name_v == null) {
        return deduce_raise(interp, "deduce/open-named-db-name-invalid", "deduce-open-named: db-name must be a string or symbol");
    }
    if (db_name_v.tag == STRING) {
        db_name = db_name_v.str_chars[:db_name_v.str_len];
    } else if (db_name_v.tag == SYMBOL) {
        db_name = interp.symbols.get_name(db_name_v.sym_val);
    } else {
        return deduce_raise(interp, "deduce/open-named-db-name-invalid", "deduce-open-named: db-name must be a string or symbol");
    }
    if (db_name.len == 0) {
        return deduce_raise(interp, "deduce/open-named-db-name-empty", "deduce-open-named: db-name cannot be empty");
    }

    Relation* rel = deduce_relation_alloc(db, rel_name_v.sym_val);
    if (rel == null) {
        return deduce_raise(interp, "deduce/open-named-out-of-memory", "deduce-open-named: out of memory");
    }

    Value* col_err = deduce_relation_collect_columns_from_args(rel, args, 3, interp);
    if (col_err != null) {
        deduce_relation_free(rel);
        return col_err;
    }

    DeduceRelationOpenStatus open_status = deduce_relation_open_dbi_with_name(rel, db_name);
    if (open_status != REL_OPEN_OK) {
        deduce_relation_free(rel);
        if (open_status == REL_OPEN_OUT_OF_MEMORY) {
            return deduce_raise(interp, "deduce/open-named-out-of-memory", "deduce-open-named: out of memory");
        }
        if (open_status == REL_OPEN_TXN_BEGIN_FAILED) {
            return deduce_raise(interp, "deduce/open-named-txn-open-failed", "deduce-open-named: txn open failed");
        }
        return deduce_raise(interp, "deduce/open-named-dbi-open-failed", "deduce-open-named: dbi open failed");
    }

    DeduceSchemaRegisterStatus schema_status =
        deduce_db_register_relation_schema(db, rel_name_v.sym_val, rel.columns, rel.col_count, rel.key_col, rel.index_col);
    if (schema_status != DEDUCE_SCHEMA_REGISTER_OK) {
        deduce_relation_free(rel);
        if (schema_status == DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT) {
            return deduce_raise(interp, "deduce/relation-arity-conflict", "deduce-open-named: relation name already registered with different arity");
        }
        return deduce_raise(interp, "deduce/open-named-out-of-memory", "deduce-open-named: out of memory");
    }

    Value* rel_v = make_ffi_handle_ex(interp, (void*)rel, "deduce-relation", &deduce_relation_finalizer, true, false);
    if (rel_v == null) {
        deduce_relation_free(rel);
        return deduce_raise(interp, "deduce/open-named-out-of-memory", "deduce-open-named: out of memory");
    }
    return rel_v;
}

// ============================================================
// (deduce-query relation filter-fn) → filtered list of dicts
// filter-fn takes a dict, returns truthy/nil
// ============================================================

fn Value* prim_deduce_query(Value*[] args, Env* env, Interp* interp) {
    // fault: lisp::ARITY_MISMATCH
    if (args.len < 2) return deduce_raise(interp, "deduce/query-expected-relation-filter", "deduce-query: expected (deduce-query relation filter-fn)");

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        // fault: lisp::TYPE_MISMATCH
        return deduce_raise(interp, "deduce/query-relation-not-handle", "deduce-query: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/query-relation-handle-closed", "deduce-query: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/query-relation-not-deduce-relation", "deduce-query: first argument must be a deduce relation handle");
    }
    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) return deduce_raise(interp, "deduce/query-database-not-open", "deduce-query: database not open");
    if (rel.dropped) return deduce_raise(interp, "deduce/query-relation-dropped", "deduce-query: relation is dropped");

    Value* filter_fn = args[1];

    if (!deduce_relation_ensure_column_key_values(rel, interp, null)) {
        return make_nil(interp);
    }

    DeduceTupleIterator it = {};
    if (!deduce_tuple_iterator_open(&it, rel, interp, null, 0, false, {}, {}, false)) {
        return make_nil(interp);
    }
    defer deduce_tuple_iterator_close(&it);

    uint dict_capacity = deduce_relation_row_dict_capacity(rel);
    Value* filtered_rev = make_nil(interp);
    while (deduce_tuple_iterator_next(&it)) {
        Value* row = deduce_relation_materialize_row_dict(rel, it.vals, interp, dict_capacity);
        Value* keep = jit_apply_value(filter_fn, row, interp);
        if (keep != null && !is_falsy(keep, interp)) {
            filtered_rev = make_cons(interp, row, filtered_rev);
        }
    }
    return deduce_reverse_list(filtered_rev, interp);
}
