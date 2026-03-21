module lisp;

import std::core::mem;

// ============================================================
// LMDB extern declarations (no wrapper)
// ============================================================
alias MdbEnv = void;
alias MdbTxn = void;
alias MdbCursor = void;
alias MdbDbi = uint;

struct MdbVal {
    usz   mv_size;
    void* mv_data;
}

struct DeduceIndexDescriptor {
    usz* column_positions;
    usz  column_count;
    bool unique;
}

struct DeduceReferenceConstraint {
    SymbolId local_column;
    SymbolId target_relation;
    SymbolId target_column;
}

struct DeduceRelationSchema {
    SymbolId               name;
    SymbolId*              columns;
    usz                    arity;
    SymbolId               key_column;
    SymbolId               unique_column;
    SymbolId*              unique_columns;
    usz                    unique_column_count;
    DeduceReferenceConstraint* reference_constraints;
    usz                    reference_constraint_count;
    SymbolId               index_column;
    DeduceIndexDescriptor* indexes;
    usz                    index_count;
    ulong                  cardinality_estimate;
    ulong                  distinct_estimate;
}

struct DeduceRuleSignature {
    SymbolId             head_predicate;
    SymbolId*            body_predicates;
    bool*                body_negated;
    ulong*               body_bound_masks;
    int*                 body_selected_indexes;
    bool*                body_filter_pushdown;
    ulong*               body_projection_masks;
    DeduceRuleTermKind*  head_term_kinds;
    DeduceAggregateOpKind* head_term_aggregate_ops;
    bool*                head_term_is_variable;
    uint*                head_term_var_ids;
    Value**              head_term_literals;
    usz                  head_term_count;
    usz                  aggregate_projection_count;
    usz*                 body_term_offsets;
    usz*                 body_term_counts;
    bool*                body_term_is_variable;
    uint*                body_term_var_ids;
    Value**              body_term_literals;
    usz                  body_term_total_count;
    uint                 max_var_id;
    usz                  body_count;
}

enum DeduceSchemaRegisterStatus {
    DEDUCE_SCHEMA_REGISTER_OK,
    DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY,
    DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT,
}

enum DeduceRuleRegisterStatus {
    DEDUCE_RULE_REGISTER_OK,
    DEDUCE_RULE_REGISTER_OUT_OF_MEMORY,
}

extern fn int mdb_env_create(MdbEnv** env) @extern("mdb_env_create");
extern fn int mdb_env_open(MdbEnv* env, char* path, uint flags, uint mode) @extern("mdb_env_open");
extern fn int mdb_env_set_mapsize(MdbEnv* env, usz size) @extern("mdb_env_set_mapsize");
extern fn int mdb_env_set_maxdbs(MdbEnv* env, uint dbs) @extern("mdb_env_set_maxdbs");
extern fn void mdb_env_close(MdbEnv* env) @extern("mdb_env_close");
extern fn int mdb_txn_begin(MdbEnv* env, MdbTxn* parent, uint flags, MdbTxn** txn) @extern("mdb_txn_begin");
extern fn int mdb_txn_commit(MdbTxn* txn) @extern("mdb_txn_commit");
extern fn void mdb_txn_abort(MdbTxn* txn) @extern("mdb_txn_abort");
extern fn int mdb_dbi_open(MdbTxn* txn, char* name, uint flags, MdbDbi* dbi) @extern("mdb_dbi_open");
extern fn int mdb_drop(MdbTxn* txn, MdbDbi dbi, int del) @extern("mdb_drop");
extern fn int mdb_put(MdbTxn* txn, MdbDbi dbi, MdbVal* key, MdbVal* data, uint flags) @extern("mdb_put");
extern fn int mdb_get(MdbTxn* txn, MdbDbi dbi, MdbVal* key, MdbVal* data) @extern("mdb_get");
extern fn int mdb_del(MdbTxn* txn, MdbDbi dbi, MdbVal* key, MdbVal* data) @extern("mdb_del");
extern fn int mdb_cursor_open(MdbTxn* txn, MdbDbi dbi, MdbCursor** cursor) @extern("mdb_cursor_open");
extern fn int mdb_cursor_get(MdbCursor* cursor, MdbVal* key, MdbVal* data, int op) @extern("mdb_cursor_get");
extern fn void mdb_cursor_close(MdbCursor* cursor) @extern("mdb_cursor_close");
extern fn int c_getpid() @extern("getpid");

const uint MDB_CREATE = 0x40000;
const uint MDB_NOSUBDIR = 0x4000;
const uint MDB_RDONLY = 0x20000;
const int MDB_SUCCESS = 0;
const int MDB_NOTFOUND = -30798;
const int MDB_FIRST = 0;
const int MDB_NEXT = 8;
const int MDB_SET_RANGE = 17;

// ============================================================
// Deduce Database Handle
// ============================================================

struct DeduceDb {
    MdbEnv*               env;
    bool                  open;
    DeduceRelationSchema* relation_schemas;
    usz                   relation_schema_count;
    usz                   relation_schema_capacity;
    DeduceRuleSignature*  rule_signatures;
    usz                   rule_signature_count;
    usz                   rule_signature_capacity;
    SymbolId*             incremental_dirty_predicates;
    usz                   incremental_dirty_count;
    usz                   incremental_dirty_capacity;
    ulong                 incremental_mutation_epoch;
    bool                  incremental_full_recompute_required;
}

const char[] DEDUCE_HEX_DIGITS = "0123456789abcdef";
const char[] DEDUCE_EXTENSIONAL_DBI_SUFFIX = "__deduce_extensional";
usz g_deduce_memory_open_seq = 0;

fn Value* deduce_raise(Interp* interp, char[] code, char[] message, Value* data = null) @inline {
    return raise_error_with_payload_names(interp, "deduce", code, message, data);
}

fn bool deduce_ffi_handle_name_equals(Value* v, char[] expected) @inline {
    if (v == null || v.tag != FFI_HANDLE || v.ffi_val == null) return false;
    if (v.ffi_val.name_len != expected.len) return false;
    for (usz i = 0; i < expected.len; i++) {
        if (v.ffi_val.lib_name[i] != expected[i]) return false;
    }
    return true;
}

fn bool deduce_name_equals(char[] name, char[] expected) @inline {
    if (name.len != expected.len) return false;
    for (usz i = 0; i < name.len; i++) {
        if (name[i] != expected[i]) return false;
    }
    return true;
}

fn bool deduce_rule_signature_has_aggregates(DeduceRuleSignature* sig) @inline {
    return sig != null && sig.aggregate_projection_count > 0;
}

fn usz deduce_format_usz_hex(char[] out, usz value) {
    if (out.len == 0) return 0;
    if (value == 0) {
        out[0] = '0';
        return 1;
    }

    char[2 * usz.sizeof] rev;
    usz count = 0;
    usz cur = value;
    while (cur != 0 && count < rev.len) {
        usz nibble = cur & 0xF;
        rev[count] = DEDUCE_HEX_DIGITS[nibble];
        count++;
        cur >>= 4;
    }

    for (usz i = 0; i < count; i++) {
        out[i] = rev[count - 1 - i];
    }
    return count;
}

fn bool deduce_append_slice(char[256]* out, usz* idx, char[] src) {
    if (*idx + src.len >= out.len) return false;
    for (usz i = 0; i < src.len; i++) {
        (*out)[*idx + i] = src[i];
    }
    *idx += src.len;
    return true;
}

fn bool deduce_append_hex(char[256]* out, usz* idx, usz value) {
    char[2 * usz.sizeof] hex_buf;
    usz hlen = deduce_format_usz_hex(&hex_buf, value);
    if (*idx + hlen >= out.len) return false;
    for (usz i = 0; i < hlen; i++) {
        (*out)[*idx + i] = hex_buf[i];
    }
    *idx += hlen;
    return true;
}

fn bool deduce_build_memory_path(char[256]* out, DeduceDb* db) {
    if (out == null || db == null) return false;
    usz idx = 0;
    if (!deduce_append_slice(out, &idx, "/tmp/deduce-")) return false;
    if (!deduce_append_hex(out, &idx, (usz)c_getpid())) return false;
    if (idx + 1 >= out.len) return false;
    (*out)[idx++] = '-';
    g_deduce_memory_open_seq++;
    if (!deduce_append_hex(out, &idx, g_deduce_memory_open_seq)) return false;
    if (idx + 1 >= out.len) return false;
    (*out)[idx++] = '-';
    if (!deduce_append_hex(out, &idx, (usz)db)) return false;
    if (idx >= out.len) return false;
    (*out)[idx] = 0;
    return true;
}

fn char* deduce_copy_c_string(char[] src) {
    char* out = (char*)mem::malloc(src.len + 1);
    if (out == null) return null;
    for (usz i = 0; i < src.len; i++) out[i] = src[i];
    out[src.len] = 0;
    return out;
}

fn char* deduce_copy_extensional_dbi_name(char[] base_name) {
    usz total_len = base_name.len + DEDUCE_EXTENSIONAL_DBI_SUFFIX.len;
    char* out = (char*)mem::malloc(total_len + 1);
    if (out == null) return null;
    for (usz i = 0; i < base_name.len; i++) out[i] = base_name[i];
    for (usz i = 0; i < DEDUCE_EXTENSIONAL_DBI_SUFFIX.len; i++) {
        out[base_name.len + i] = DEDUCE_EXTENSIONAL_DBI_SUFFIX[i];
    }
    out[total_len] = 0;
    return out;
}

fn bool deduce_open_extensional_dbi_for_relation(
    Relation* rel,
    MdbTxn* txn,
    bool create,
    MdbDbi* dbi_out,
    Interp* interp
) {
    if (rel == null || rel.db == null || !rel.db.open || txn == null || dbi_out == null || interp == null) return false;
    char[] name_str = interp.symbols.get_name(rel.name);
    char* dbi_name = deduce_copy_extensional_dbi_name(name_str);
    if (dbi_name == null) return false;
    defer mem::free(dbi_name);

    MdbDbi dbi = 0;
    int rc = mdb_dbi_open(txn, dbi_name, create ? MDB_CREATE : 0, &dbi);
    if (rc != MDB_SUCCESS) return false;
    *dbi_out = dbi;
    return true;
}

fn bool deduce_open_extensional_dbi_for_predicate(
    DeduceDb* db,
    SymbolId predicate,
    MdbTxn* txn,
    bool create,
    MdbDbi* dbi_out,
    Interp* interp
) {
    if (db == null || !db.open || predicate == (SymbolId)0 || txn == null || dbi_out == null || interp == null) return false;
    char[] name_str = interp.symbols.get_name(predicate);
    char* dbi_name = deduce_copy_extensional_dbi_name(name_str);
    if (dbi_name == null) return false;
    defer mem::free(dbi_name);

    MdbDbi dbi = 0;
    int rc = mdb_dbi_open(txn, dbi_name, create ? MDB_CREATE : 0, &dbi);
    if (rc != MDB_SUCCESS) return false;
    *dbi_out = dbi;
    return true;
}

fn void deduce_db_finalizer(void* handle) {
    if (handle == null) return;
    DeduceDb* db = (DeduceDb*)handle;
    if (db.rule_signatures != null) {
        for (usz i = 0; i < db.rule_signature_count; i++) {
            if (db.rule_signatures[i].body_predicates != null) {
                mem::free(db.rule_signatures[i].body_predicates);
                db.rule_signatures[i].body_predicates = null;
            }
            if (db.rule_signatures[i].body_negated != null) {
                mem::free(db.rule_signatures[i].body_negated);
                db.rule_signatures[i].body_negated = null;
            }
            if (db.rule_signatures[i].body_bound_masks != null) {
                mem::free(db.rule_signatures[i].body_bound_masks);
                db.rule_signatures[i].body_bound_masks = null;
            }
            if (db.rule_signatures[i].body_selected_indexes != null) {
                mem::free(db.rule_signatures[i].body_selected_indexes);
                db.rule_signatures[i].body_selected_indexes = null;
            }
            if (db.rule_signatures[i].body_filter_pushdown != null) {
                mem::free(db.rule_signatures[i].body_filter_pushdown);
                db.rule_signatures[i].body_filter_pushdown = null;
            }
            if (db.rule_signatures[i].body_projection_masks != null) {
                mem::free(db.rule_signatures[i].body_projection_masks);
                db.rule_signatures[i].body_projection_masks = null;
            }
            if (db.rule_signatures[i].head_term_kinds != null) {
                mem::free(db.rule_signatures[i].head_term_kinds);
                db.rule_signatures[i].head_term_kinds = null;
            }
            if (db.rule_signatures[i].head_term_aggregate_ops != null) {
                mem::free(db.rule_signatures[i].head_term_aggregate_ops);
                db.rule_signatures[i].head_term_aggregate_ops = null;
            }
            if (db.rule_signatures[i].head_term_is_variable != null) {
                mem::free(db.rule_signatures[i].head_term_is_variable);
                db.rule_signatures[i].head_term_is_variable = null;
            }
            if (db.rule_signatures[i].head_term_var_ids != null) {
                mem::free(db.rule_signatures[i].head_term_var_ids);
                db.rule_signatures[i].head_term_var_ids = null;
            }
            if (db.rule_signatures[i].head_term_literals != null) {
                mem::free(db.rule_signatures[i].head_term_literals);
                db.rule_signatures[i].head_term_literals = null;
            }
            if (db.rule_signatures[i].body_term_offsets != null) {
                mem::free(db.rule_signatures[i].body_term_offsets);
                db.rule_signatures[i].body_term_offsets = null;
            }
            if (db.rule_signatures[i].body_term_counts != null) {
                mem::free(db.rule_signatures[i].body_term_counts);
                db.rule_signatures[i].body_term_counts = null;
            }
            if (db.rule_signatures[i].body_term_is_variable != null) {
                mem::free(db.rule_signatures[i].body_term_is_variable);
                db.rule_signatures[i].body_term_is_variable = null;
            }
            if (db.rule_signatures[i].body_term_var_ids != null) {
                mem::free(db.rule_signatures[i].body_term_var_ids);
                db.rule_signatures[i].body_term_var_ids = null;
            }
            if (db.rule_signatures[i].body_term_literals != null) {
                mem::free(db.rule_signatures[i].body_term_literals);
                db.rule_signatures[i].body_term_literals = null;
            }
            db.rule_signatures[i].head_term_count = 0;
            db.rule_signatures[i].aggregate_projection_count = 0;
            db.rule_signatures[i].body_term_total_count = 0;
            db.rule_signatures[i].max_var_id = 0;
            db.rule_signatures[i].body_count = 0;
            db.rule_signatures[i].head_predicate = 0;
        }
        mem::free(db.rule_signatures);
        db.rule_signatures = null;
    }
    db.rule_signature_count = 0;
    db.rule_signature_capacity = 0;

    if (db.incremental_dirty_predicates != null) {
        mem::free(db.incremental_dirty_predicates);
        db.incremental_dirty_predicates = null;
    }
    db.incremental_dirty_count = 0;
    db.incremental_dirty_capacity = 0;
    db.incremental_mutation_epoch = 0;
    db.incremental_full_recompute_required = false;

    if (db.relation_schemas != null) {
        for (usz i = 0; i < db.relation_schema_count; i++) {
            DeduceRelationSchema* schema = &db.relation_schemas[i];
            if (schema.columns != null) {
                mem::free(schema.columns);
                schema.columns = null;
            }
            if (schema.unique_columns != null) {
                mem::free(schema.unique_columns);
                schema.unique_columns = null;
            }
            if (schema.reference_constraints != null) {
                mem::free(schema.reference_constraints);
                schema.reference_constraints = null;
            }
            if (schema.indexes != null) {
                for (usz j = 0; j < schema.index_count; j++) {
                    if (schema.indexes[j].column_positions != null) {
                        mem::free(schema.indexes[j].column_positions);
                        schema.indexes[j].column_positions = null;
                    }
                    schema.indexes[j].column_count = 0;
                    schema.indexes[j].unique = false;
                }
                mem::free(schema.indexes);
                schema.indexes = null;
            }
            schema.index_count = 0;
            schema.cardinality_estimate = 0;
            schema.distinct_estimate = 0;
            schema.key_column = (SymbolId)0;
            schema.unique_column = (SymbolId)0;
            schema.unique_column_count = 0;
            schema.reference_constraint_count = 0;
            schema.index_column = (SymbolId)0;
            schema.arity = 0;
            schema.name = (SymbolId)0;
        }
        mem::free(db.relation_schemas);
        db.relation_schemas = null;
    }
    db.relation_schema_count = 0;
    db.relation_schema_capacity = 0;

    if (db.env != null && db.open) {
        mdb_env_close(db.env);
        db.env = null;
        db.open = false;
    }
}

fn bool deduce_index_columns_equal(DeduceIndexDescriptor* desc, usz[] positions) {
    if (desc == null) return false;
    if (desc.column_count != positions.len) return false;
    for (usz i = 0; i < positions.len; i++) {
        if (desc.column_positions == null || desc.column_positions[i] != positions[i]) return false;
    }
    return true;
}

fn bool deduce_schema_add_index(
    DeduceIndexDescriptor* indexes,
    usz max_indexes,
    usz* index_count,
    usz[] positions,
    bool unique
) {
    if (positions.len == 0) return true;
    if (*index_count >= max_indexes) return false;

    for (usz i = 0; i < *index_count; i++) {
        if (!deduce_index_columns_equal(&indexes[i], positions)) continue;
        if (unique && !indexes[i].unique) indexes[i].unique = true;
        return true;
    }

    usz* cols = (usz*)mem::malloc(usz.sizeof * positions.len);
    if (cols == null) return false;
    for (usz i = 0; i < positions.len; i++) cols[i] = positions[i];

    indexes[*index_count].column_positions = cols;
    indexes[*index_count].column_count = positions.len;
    indexes[*index_count].unique = unique;
    (*index_count)++;
    return true;
}

fn bool deduce_find_column_position(SymbolId* columns, usz arity, SymbolId name, usz* pos_out) {
    if (pos_out == null || name == (SymbolId)0) return false;
    if (columns == null) return false;
    for (usz i = 0; i < arity; i++) {
        if (columns[i] != name) continue;
        *pos_out = i;
        return true;
    }
    return false;
}

fn void deduce_release_index_descriptors(DeduceIndexDescriptor* indexes, usz count) {
    if (indexes == null) return;
    for (usz i = 0; i < count; i++) {
        if (indexes[i].column_positions != null) {
            mem::free(indexes[i].column_positions);
            indexes[i].column_positions = null;
        }
        indexes[i].column_count = 0;
        indexes[i].unique = false;
    }
}

fn bool deduce_db_ensure_relation_schema_capacity(DeduceDb* db, usz needed) {
    if (db == null) return false;
    if (needed <= db.relation_schema_capacity) return true;

    usz new_cap = db.relation_schema_capacity > 0 ? db.relation_schema_capacity : 8;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    DeduceRelationSchema* next =
        (DeduceRelationSchema*)mem::realloc(db.relation_schemas, DeduceRelationSchema.sizeof * new_cap);
    if (next == null) return false;
    db.relation_schemas = next;
    db.relation_schema_capacity = new_cap;
    return true;
}
