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
    rel.unique_col = 0;
    rel.unique_cols = null;
    rel.unique_col_count = 0;
    rel.unique_col_capacity = 0;
    rel.reference_constraints = null;
    rel.reference_constraint_count = 0;
    rel.reference_constraint_capacity = 0;
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

fn bool deduce_relation_ensure_unique_column_capacity(Relation* rel, usz needed) {
    if (needed <= rel.unique_col_capacity) return true;

    usz new_cap = rel.unique_col_capacity > 0 ? rel.unique_col_capacity : 2;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    SymbolId* new_columns = (SymbolId*)mem::realloc(rel.unique_cols, SymbolId.sizeof * new_cap);
    if (new_columns == null) return false;
    rel.unique_cols = new_columns;
    rel.unique_col_capacity = new_cap;
    return true;
}

fn bool deduce_relation_ensure_reference_constraint_capacity(Relation* rel, usz needed) {
    if (needed <= rel.reference_constraint_capacity) return true;

    usz new_cap = rel.reference_constraint_capacity > 0 ? rel.reference_constraint_capacity : 2;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    DeduceReferenceConstraint* next =
        (DeduceReferenceConstraint*)mem::realloc(rel.reference_constraints, DeduceReferenceConstraint.sizeof * new_cap);
    if (next == null) return false;
    rel.reference_constraints = next;
    rel.reference_constraint_capacity = new_cap;
    return true;
}

fn Value* deduce_relation_note_unique_column(Relation* rel, SymbolId col, Interp* interp) {
    if (rel == null || col == (SymbolId)0) return null;
    for (usz i = 0; i < rel.unique_col_count; i++) {
        if (rel.unique_cols != null && rel.unique_cols[i] == col) {
            rel.unique_col = rel.unique_col_count == 1 ? rel.unique_cols[0] : (SymbolId)0;
            return null;
        }
    }
    if (!deduce_relation_ensure_unique_column_capacity(rel, rel.unique_col_count + 1)) {
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }
    rel.unique_cols[rel.unique_col_count++] = col;
    rel.unique_col = rel.unique_col_count == 1 ? rel.unique_cols[0] : (SymbolId)0;
    return null;
}

fn Value* deduce_relation_note_reference_constraint(
    Relation* rel,
    SymbolId local_col,
    SymbolId target_relation,
    SymbolId target_column,
    Interp* interp
) {
    if (rel == null || local_col == (SymbolId)0 || target_relation == (SymbolId)0 || target_column == (SymbolId)0) {
        return null;
    }
    for (usz i = 0; i < rel.reference_constraint_count; i++) {
        if (rel.reference_constraints == null || rel.reference_constraints[i].local_column != local_col) continue;
        rel.reference_constraints[i].target_relation = target_relation;
        rel.reference_constraints[i].target_column = target_column;
        return null;
    }
    if (!deduce_relation_ensure_reference_constraint_capacity(rel, rel.reference_constraint_count + 1)) {
        return deduce_raise(interp, "deduce/define-relation-out-of-memory", "__define-relation: out of memory");
    }
    rel.reference_constraints[rel.reference_constraint_count++] = {
        .local_column = local_col,
        .target_relation = target_relation,
        .target_column = target_column
    };
    return null;
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
    SymbolId sym_role_unique = interp.symbols.intern("__relation-role-unique");
    SymbolId sym_role_index = interp.symbols.intern("__relation-role-index");
    SymbolId sym_role_ref = interp.symbols.intern("__relation-role-ref");

    for (usz i = start; i < args.len;) {
        bool has_key_role = false;
        bool has_unique_role = false;
        bool has_index_role = false;
        bool has_ref_role = false;
        SymbolId ref_target_relation = 0;
        SymbolId ref_target_column = 0;
        while (i < args.len) {
            Value* marker = args[i];
            if (marker == null || marker.tag != SYMBOL) break;
            if (marker.sym_val == sym_role_key) {
                has_key_role = true;
                i++;
                continue;
            }
            if (marker.sym_val == sym_role_unique) {
                has_unique_role = true;
                i++;
                continue;
            }
            if (marker.sym_val == sym_role_index) {
                has_index_role = true;
                i++;
                continue;
            }
            if (marker.sym_val == sym_role_ref) {
                if (i + 2 >= args.len ||
                    args[i + 1] == null || args[i + 1].tag != SYMBOL ||
                    args[i + 2] == null || args[i + 2].tag != SYMBOL) {
                    return deduce_raise(interp, "deduce/define-relation-reference-invalid", "__define-relation: ref role expects target relation and target column symbols");
                }
                has_ref_role = true;
                ref_target_relation = args[i + 1].sym_val;
                ref_target_column = args[i + 2].sym_val;
                i += 3;
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
        if (has_unique_role) {
            Value* unique_err = deduce_relation_note_unique_column(rel, col.sym_val, interp);
            if (unique_err != null) return unique_err;
        }
        if (has_ref_role) {
            Value* ref_err = deduce_relation_note_reference_constraint(rel, col.sym_val, ref_target_relation, ref_target_column, interp);
            if (ref_err != null) return ref_err;
        }
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
        deduce_db_register_relation_schema(
            db,
            rel_name,
            rel.columns,
            rel.col_count,
            rel.key_col,
            rel.unique_cols,
            rel.unique_col_count,
            rel.reference_constraints,
            rel.reference_constraint_count,
            rel.index_col
        );
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
        deduce_db_register_relation_schema(
            db,
            rel_name_v.sym_val,
            rel.columns,
            rel.col_count,
            rel.key_col,
            rel.unique_cols,
            rel.unique_col_count,
            rel.reference_constraints,
            rel.reference_constraint_count,
            rel.index_col
        );
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

fn Value* deduce_relation_schema_columns_list(DeduceRelationSchema* schema, Interp* interp) {
    if (schema == null || schema.columns == null || schema.arity == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < schema.arity; i++) {
        rev = make_cons(interp, make_symbol(interp, schema.columns[i]), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* deduce_relation_index_columns_list(
    DeduceRelationSchema* schema,
    DeduceIndexDescriptor* index,
    Interp* interp
) {
    if (schema == null || index == null || index.column_positions == null || index.column_count == 0) {
        return make_nil(interp);
    }

    Value* rev = make_nil(interp);
    for (usz i = 0; i < index.column_count; i++) {
        usz column_pos = index.column_positions[i];
        if (column_pos >= schema.arity || schema.columns == null) {
            rev = make_cons(interp, make_nil(interp), rev);
            continue;
        }
        rev = make_cons(interp, make_symbol(interp, schema.columns[column_pos]), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* deduce_relation_schema_role_column(SymbolId column, Interp* interp) {
    if (column == (SymbolId)0) return make_nil(interp);
    return make_symbol(interp, column);
}

fn Value* deduce_relation_schema_role_columns(SymbolId* columns, usz count, Interp* interp) {
    if (columns == null || count == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < count; i++) {
        rev = make_cons(interp, make_symbol(interp, columns[i]), rev);
    }
    return explain_reverse_list(rev, interp);
}

fn bool deduce_relation_schema_unique_equivalent_to_key(DeduceRelationSchema* schema) @inline {
    return schema != null &&
        schema.key_column != (SymbolId)0 &&
        schema.unique_column_count == 1 &&
        schema.unique_columns != null &&
        schema.unique_columns[0] == schema.key_column;
}

fn long deduce_relation_schema_constraint_count(DeduceRelationSchema* schema) @inline {
    if (schema == null) return 0;
    long count = 0;
    if (schema.key_column != (SymbolId)0) count++;
    if (schema.unique_column_count > 0 && !deduce_relation_schema_unique_equivalent_to_key(schema)) count++;
    count += (long)schema.reference_constraint_count;
    return count;
}

fn void deduce_reference_constraint_resolution(
    DeduceDb* db,
    DeduceReferenceConstraint* ref_constraint,
    bool* target_declared_out,
    bool* target_column_declared_out
) {
    bool target_declared = false;
    bool target_column_declared = false;
    if (db != null && ref_constraint != null) {
        DeduceRelationSchema* target_schema = deduce_db_lookup_relation_schema(db, ref_constraint.target_relation);
        if (target_schema != null) {
            target_declared = true;
            usz ignored = 0;
            target_column_declared =
                deduce_find_column_position(target_schema.columns, target_schema.arity, ref_constraint.target_column, &ignored);
        }
    }
    if (target_declared_out != null) *target_declared_out = target_declared;
    if (target_column_declared_out != null) *target_column_declared_out = target_column_declared;
}

fn long deduce_relation_schema_resolved_reference_constraint_count(
    DeduceDb* db,
    DeduceRelationSchema* schema
) {
    if (db == null || schema == null || schema.reference_constraints == null || schema.reference_constraint_count == 0) {
        return 0;
    }
    long count = 0;
    for (usz i = 0; i < schema.reference_constraint_count; i++) {
        bool target_declared = false;
        bool target_column_declared = false;
        deduce_reference_constraint_resolution(db, &schema.reference_constraints[i], &target_declared, &target_column_declared);
        if (target_declared && target_column_declared) count++;
    }
    return count;
}

fn Value* deduce_relation_schema_constraints_payload(
    DeduceDb* db,
    DeduceRelationSchema* schema,
    Interp* interp
) {
    if (schema == null) return make_nil(interp);

    Value* rev = make_nil(interp);

    if (schema.unique_column_count > 0 && !deduce_relation_schema_unique_equivalent_to_key(schema)) {
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
        }
        Value* columns = deduce_relation_schema_role_columns(schema.unique_columns, schema.unique_column_count, interp);
        explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("unique"));
        explain_dict_set(entry, interp, "columns", columns);
        explain_dict_set(entry, interp, "unique", explain_bool_value(interp, true));
        explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, true));
        rev = make_cons(interp, entry, rev);
    }

    if (schema.reference_constraint_count > 0 && schema.reference_constraints != null) {
        for (usz i = 0; i < schema.reference_constraint_count; i++) {
            DeduceReferenceConstraint* ref = &schema.reference_constraints[i];
            Value* entry = make_hashmap(interp, 10);
            if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
                return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
            }
            bool target_declared = false;
            bool target_column_declared = false;
            deduce_reference_constraint_resolution(db, ref, &target_declared, &target_column_declared);
            Value* columns = make_cons(interp, make_symbol(interp, ref.local_column), make_nil(interp));
            explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("reference"));
            explain_dict_set(entry, interp, "columns", columns);
            explain_dict_set_symbol(entry, interp, "referenced-relation", ref.target_relation);
            explain_dict_set_symbol(entry, interp, "referenced-column", ref.target_column);
            explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, true));
            explain_dict_set(entry, interp, "target-declared", explain_bool_value(interp, target_declared));
            explain_dict_set(entry, interp, "target-column-declared", explain_bool_value(interp, target_column_declared));
            explain_dict_set(entry, interp, "resolved", explain_bool_value(interp, target_declared && target_column_declared));
            rev = make_cons(interp, entry, rev);
        }
    }

    if (schema.key_column != (SymbolId)0) {
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
        }
        Value* columns = make_cons(interp, make_symbol(interp, schema.key_column), make_nil(interp));
        explain_dict_set_symbol(entry, interp, "kind", interp.symbols.intern("key"));
        explain_dict_set(entry, interp, "columns", columns);
        explain_dict_set(entry, interp, "unique", explain_bool_value(interp, true));
        explain_dict_set(entry, interp, "enforced", explain_bool_value(interp, true));
        rev = make_cons(interp, entry, rev);
    }

    return rev == null ? make_nil(interp) : explain_reverse_list(rev, interp);
}

fn Value* deduce_relation_schema_payload(
    Relation* rel,
    DeduceRelationSchema* schema,
    Interp* interp
) {
    Value* out = make_hashmap(interp, 24);
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) {
        return deduce_raise(interp, "deduce/schema-out-of-memory", "deduce/schema: out of memory");
    }

    long resolved_reference_constraint_count =
        deduce_relation_schema_resolved_reference_constraint_count(rel != null ? rel.db : null, schema);
    long unresolved_reference_constraint_count =
        schema != null ? (long)schema.reference_constraint_count - resolved_reference_constraint_count : 0;

    Value* constraints = deduce_relation_schema_constraints_payload(rel != null ? rel.db : null, schema, interp);
    if (constraints != null && constraints.tag == ERROR) return constraints;

    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-schema"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set(out, interp, "relation", make_symbol(interp, rel.name));
    explain_dict_set(out, interp, "columns", deduce_relation_schema_columns_list(schema, interp));
    explain_dict_set(out, interp, "arity", make_int(interp, (long)schema.arity));
    explain_dict_set(out, interp, "key-column", deduce_relation_schema_role_column(schema.key_column, interp));
    explain_dict_set(out, interp, "unique-column", deduce_relation_schema_role_column(schema.unique_column, interp));
    explain_dict_set(out, interp, "unique-columns", deduce_relation_schema_role_columns(schema.unique_columns, schema.unique_column_count, interp));
    explain_dict_set(out, interp, "reference-constraint-count", make_int(interp, (long)schema.reference_constraint_count));
    explain_dict_set(out, interp, "resolved-reference-constraint-count", make_int(interp, resolved_reference_constraint_count));
    explain_dict_set(out, interp, "unresolved-reference-constraint-count", make_int(interp, unresolved_reference_constraint_count));
    explain_dict_set(out, interp, "index-column", deduce_relation_schema_role_column(schema.index_column, interp));
    explain_dict_set(out, interp, "index-count", make_int(interp, (long)schema.index_count));
    explain_dict_set(out, interp, "constraint-count", make_int(interp, deduce_relation_schema_constraint_count(schema)));
    explain_dict_set(out, interp, "constraints", constraints);
    explain_dict_set(out, interp, "cardinality-estimate", make_int(interp, (long)schema.cardinality_estimate));
    explain_dict_set(out, interp, "distinct-estimate", make_int(interp, (long)schema.distinct_estimate));
    explain_dict_set(out, interp, "dropped", explain_bool_value(interp, rel.dropped));
    explain_dict_set(out, interp, "database-open", explain_bool_value(interp, rel.db != null && rel.db.open));
    return out;
}

fn Value* deduce_relation_indexes_payload(
    Relation* rel,
    DeduceRelationSchema* schema,
    Interp* interp
) {
    (void)rel;
    if (schema == null || schema.indexes == null || schema.index_count == 0) return make_nil(interp);

    Value* rev = make_nil(interp);
    for (usz i = 0; i < schema.index_count; i++) {
        DeduceIndexDescriptor* index = &schema.indexes[i];
        Value* entry = make_hashmap(interp, 8);
        if (entry == null || entry.tag != HASHMAP || entry.hashmap_val == null) {
            return deduce_raise(interp, "deduce/indexes-out-of-memory", "deduce/indexes: out of memory");
        }
        explain_dict_set(entry, interp, "index-id", make_int(interp, (long)i));
        explain_dict_set(entry, interp, "columns", deduce_relation_index_columns_list(schema, index, interp));
        explain_dict_set(entry, interp, "column-count", make_int(interp, (long)index.column_count));
        explain_dict_set(entry, interp, "unique", explain_bool_value(interp, index.unique));
        rev = make_cons(interp, entry, rev);
    }
    return explain_reverse_list(rev, interp);
}

fn Value* prim_deduce_schema(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/schema-expected-relation", "deduce/schema: expected (deduce/schema relation)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/schema-relation-not-handle", "deduce/schema: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/schema-relation-handle-closed", "deduce/schema: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/schema-relation-not-deduce-relation", "deduce/schema: first argument must be a deduce relation handle");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/schema-database-not-open", "deduce/schema: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/schema-relation-not-found", "deduce/schema: relation schema is not registered");
    }

    return deduce_relation_schema_payload(rel, schema, interp);
}

fn Value* prim_deduce_indexes(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/indexes-expected-relation", "deduce/indexes: expected (deduce/indexes relation)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/indexes-relation-not-handle", "deduce/indexes: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/indexes-relation-handle-closed", "deduce/indexes: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/indexes-relation-not-deduce-relation", "deduce/indexes: first argument must be a deduce relation handle");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/indexes-database-not-open", "deduce/indexes: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/indexes-relation-not-found", "deduce/indexes: relation schema is not registered");
    }

    return deduce_relation_indexes_payload(rel, schema, interp);
}

fn Value* prim_deduce_stats(Value*[] args, Env* env, Interp* interp) {
    (void)env;
    if (args.len != 1) {
        return deduce_raise(interp, "deduce/stats-expected-relation", "deduce/stats: expected (deduce/stats relation)");
    }

    Value* rel_val = args[0];
    if (rel_val == null || rel_val.tag != FFI_HANDLE) {
        return deduce_raise(interp, "deduce/stats-relation-not-handle", "deduce/stats: first argument must be a relation");
    }
    if (rel_val.ffi_val == null || rel_val.ffi_val.lib_handle == null) {
        return deduce_raise(interp, "deduce/stats-relation-handle-closed", "deduce/stats: relation handle is closed");
    }
    if (!deduce_ffi_handle_name_equals(rel_val, "deduce-relation")) {
        return deduce_raise(interp, "deduce/stats-relation-not-deduce-relation", "deduce/stats: first argument must be a deduce relation handle");
    }

    Relation* rel = (Relation*)rel_val.ffi_val.lib_handle;
    if (rel.db == null || !rel.db.open) {
        return deduce_raise(interp, "deduce/stats-database-not-open", "deduce/stats: database is not open");
    }

    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) {
        return deduce_raise(interp, "deduce/stats-relation-not-found", "deduce/stats: relation schema is not registered");
    }

    Value* out = make_hashmap(interp, 16);
    if (out == null || out.tag != HASHMAP || out.hashmap_val == null) {
        return deduce_raise(interp, "deduce/stats-out-of-memory", "deduce/stats: out of memory");
    }

    explain_dict_set_symbol(out, interp, "kind", interp.symbols.intern("deduce-stats"));
    explain_dict_set_symbol(out, interp, "status", interp.symbols.intern("ok"));
    explain_dict_set(out, interp, "relation", make_symbol(interp, rel.name));
    explain_dict_set(out, interp, "cardinality-estimate", make_int(interp, (long)schema.cardinality_estimate));
    explain_dict_set(out, interp, "distinct-estimate", make_int(interp, (long)schema.distinct_estimate));
    explain_dict_set(out, interp, "dirty-predicate-count", make_int(interp, (long)rel.db.incremental_dirty_count));
    explain_dict_set(out, interp, "mutation-epoch", make_int(interp, (long)rel.db.incremental_mutation_epoch));
    explain_dict_set(out, interp, "full-recompute-required", explain_bool_value(interp, rel.db.incremental_full_recompute_required));
    explain_dict_set(out, interp, "dropped", explain_bool_value(interp, rel.dropped));
    return out;
}
