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

struct DeduceRelationSchema {
    SymbolId               name;
    SymbolId*              columns;
    usz                    arity;
    DeduceIndexDescriptor* indexes;
    usz                    index_count;
    ulong                  cardinality_estimate;
    ulong                  distinct_estimate;
}

struct DeduceRuleSignature {
    SymbolId  head_predicate;
    SymbolId* body_predicates;
    bool*     body_negated;
    ulong*    body_bound_masks;
    int*      body_selected_indexes;
    bool*     body_filter_pushdown;
    ulong*    body_projection_masks;
    bool*     head_term_is_variable;
    uint*     head_term_var_ids;
    Value**   head_term_literals;
    usz       head_term_count;
    usz*      body_term_offsets;
    usz*      body_term_counts;
    bool*     body_term_is_variable;
    uint*     body_term_var_ids;
    Value**   body_term_literals;
    usz       body_term_total_count;
    uint      max_var_id;
    usz       body_count;
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

fn DeduceSchemaRegisterStatus deduce_db_register_relation_schema(
    DeduceDb* db,
    SymbolId name,
    SymbolId* columns,
    usz arity,
    SymbolId key_col,
    SymbolId index_col
) {
    if (db == null || name == (SymbolId)0) return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    if (arity > 0 && columns == null) return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;

    for (usz i = 0; i < db.relation_schema_count; i++) {
        DeduceRelationSchema* entry = &db.relation_schemas[i];
        if (entry.name != name) continue;
        if (entry.arity != arity) return DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT;
        for (usz c = 0; c < arity; c++) {
            if (entry.columns == null || entry.columns[c] != columns[c]) {
                return DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT;
            }
        }
        return DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT;
    }

    usz needed = db.relation_schema_count + 1;
    if (!deduce_db_ensure_relation_schema_capacity(db, needed)) {
        return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
    }

    DeduceRelationSchema* schema = &db.relation_schemas[db.relation_schema_count];
    schema.name = name;
    schema.columns = null;
    schema.arity = arity;
    schema.indexes = null;
    schema.index_count = 0;
    schema.cardinality_estimate = 0;
    schema.distinct_estimate = 0;

    if (arity > 0) {
        schema.columns = (SymbolId*)mem::malloc(SymbolId.sizeof * arity);
        if (schema.columns == null) return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        for (usz i = 0; i < arity; i++) schema.columns[i] = columns[i];
    }

    usz max_indexes = arity + 3;
    if (max_indexes > 0) {
        schema.indexes = (DeduceIndexDescriptor*)mem::malloc(DeduceIndexDescriptor.sizeof * max_indexes);
        if (schema.indexes == null) {
            if (schema.columns != null) {
                mem::free(schema.columns);
                schema.columns = null;
            }
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < max_indexes; i++) {
            schema.indexes[i].column_positions = null;
            schema.indexes[i].column_count = 0;
            schema.indexes[i].unique = false;
        }
    }

    if (arity > 0) {
        usz[] full_positions = mem::new_array(usz, arity);
        if (full_positions.ptr == null) {
            deduce_release_index_descriptors(schema.indexes, schema.index_count);
            if (schema.indexes != null) mem::free(schema.indexes);
            if (schema.columns != null) mem::free(schema.columns);
            schema.indexes = null;
            schema.columns = null;
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }
        defer mem::free(full_positions.ptr);
        for (usz i = 0; i < arity; i++) full_positions[i] = i;

        if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, full_positions, true)) {
            deduce_release_index_descriptors(schema.indexes, schema.index_count);
            if (schema.indexes != null) mem::free(schema.indexes);
            if (schema.columns != null) mem::free(schema.columns);
            schema.indexes = null;
            schema.columns = null;
            return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
        }

        for (usz i = 0; i < arity; i++) {
            usz[1] single = { i };
            if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, single[..], false)) {
                deduce_release_index_descriptors(schema.indexes, schema.index_count);
                if (schema.indexes != null) mem::free(schema.indexes);
                if (schema.columns != null) mem::free(schema.columns);
                schema.indexes = null;
                schema.columns = null;
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }

        usz key_pos = 0;
        if (deduce_find_column_position(columns, arity, key_col, &key_pos)) {
            usz[1] key_single = { key_pos };
            if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, key_single[..], true)) {
                deduce_release_index_descriptors(schema.indexes, schema.index_count);
                if (schema.indexes != null) mem::free(schema.indexes);
                if (schema.columns != null) mem::free(schema.columns);
                schema.indexes = null;
                schema.columns = null;
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }

        usz idx_pos = 0;
        if (deduce_find_column_position(columns, arity, index_col, &idx_pos)) {
            usz[1] idx_single = { idx_pos };
            if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, idx_single[..], false)) {
                deduce_release_index_descriptors(schema.indexes, schema.index_count);
                if (schema.indexes != null) mem::free(schema.indexes);
                if (schema.columns != null) mem::free(schema.columns);
                schema.indexes = null;
                schema.columns = null;
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }

        if (deduce_find_column_position(columns, arity, key_col, &key_pos) &&
            deduce_find_column_position(columns, arity, index_col, &idx_pos) &&
            key_pos != idx_pos) {
            usz[2] key_index_pair = { key_pos, idx_pos };
            if (!deduce_schema_add_index(schema.indexes, max_indexes, &schema.index_count, key_index_pair[..], false)) {
                deduce_release_index_descriptors(schema.indexes, schema.index_count);
                if (schema.indexes != null) mem::free(schema.indexes);
                if (schema.columns != null) mem::free(schema.columns);
                schema.indexes = null;
                schema.columns = null;
                return DEDUCE_SCHEMA_REGISTER_OUT_OF_MEMORY;
            }
        }
    }

    db.relation_schema_count++;
    return DEDUCE_SCHEMA_REGISTER_OK;
}

fn DeduceRelationSchema* deduce_db_lookup_relation_schema(DeduceDb* db, SymbolId name) {
    if (db == null || name == (SymbolId)0) return null;
    for (usz i = 0; i < db.relation_schema_count; i++) {
        if (db.relation_schemas[i].name != name) continue;
        return &db.relation_schemas[i];
    }
    return null;
}

fn DeduceRelationSchema* deduce_relation_lookup_schema(Relation* rel) {
    if (rel == null || rel.db == null) return null;
    return deduce_db_lookup_relation_schema(rel.db, rel.name);
}

fn void deduce_relation_schema_set_estimates(Relation* rel, ulong cardinality, ulong distinct_estimate) {
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) return;
    schema.cardinality_estimate = cardinality;
    if (distinct_estimate > cardinality) {
        schema.distinct_estimate = cardinality;
    } else {
        schema.distinct_estimate = distinct_estimate;
    }
}

fn void deduce_relation_schema_note_insert(Relation* rel) {
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) return;
    if (schema.cardinality_estimate < ulong.max) schema.cardinality_estimate++;
    if (schema.distinct_estimate < schema.cardinality_estimate) {
        schema.distinct_estimate = schema.cardinality_estimate;
    }
}

fn void deduce_relation_schema_note_delete(Relation* rel) {
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) return;
    if (schema.cardinality_estimate > 0) schema.cardinality_estimate--;
    if (schema.distinct_estimate > schema.cardinality_estimate) {
        schema.distinct_estimate = schema.cardinality_estimate;
    }
}

fn void deduce_relation_schema_note_clear(Relation* rel) {
    deduce_relation_schema_set_estimates(rel, 0, 0);
}

fn bool deduce_symbol_id_slice_contains(SymbolId[] values, SymbolId target) @inline {
    if (target == (SymbolId)0) return false;
    for (usz i = 0; i < values.len; i++) {
        if (values[i] == target) return true;
    }
    return false;
}

fn bool deduce_db_ensure_incremental_dirty_capacity(DeduceDb* db, usz needed) {
    if (db == null) return false;
    if (needed <= db.incremental_dirty_capacity) return true;

    usz new_cap = db.incremental_dirty_capacity > 0 ? db.incremental_dirty_capacity : 8;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    SymbolId* next = (SymbolId*)mem::realloc(db.incremental_dirty_predicates, SymbolId.sizeof * new_cap);
    if (next == null) return false;
    db.incremental_dirty_predicates = next;
    db.incremental_dirty_capacity = new_cap;
    return true;
}

fn bool deduce_db_mark_incremental_dirty_predicate(DeduceDb* db, SymbolId predicate) {
    if (db == null || predicate == (SymbolId)0) return false;
    for (usz i = 0; i < db.incremental_dirty_count; i++) {
        if (db.incremental_dirty_predicates != null && db.incremental_dirty_predicates[i] == predicate) {
            return true;
        }
    }
    usz needed = db.incremental_dirty_count + 1;
    if (!deduce_db_ensure_incremental_dirty_capacity(db, needed)) return false;
    db.incremental_dirty_predicates[db.incremental_dirty_count] = predicate;
    db.incremental_dirty_count++;
    return true;
}

fn bool deduce_db_mark_dirty_with_rule_dependents(DeduceDb* db, SymbolId seed_predicate) {
    if (db == null || seed_predicate == (SymbolId)0) return false;
    usz queue_cap = db.rule_signature_count + db.relation_schema_count + 1;
    if (queue_cap == 0) queue_cap = 1;
    SymbolId[] queue = mem::new_array(SymbolId, queue_cap);
    if (queue.ptr == null) return false;
    defer mem::free(queue.ptr);

    usz queue_len = 1;
    usz queue_idx = 0;
    queue[0] = seed_predicate;

    while (queue_idx < queue_len) {
        SymbolId current = queue[queue_idx++];
        if (!deduce_db_mark_incremental_dirty_predicate(db, current)) return false;

        for (usz r = 0; r < db.rule_signature_count; r++) {
            DeduceRuleSignature* sig = &db.rule_signatures[r];
            bool depends_on_current = false;
            for (usz b = 0; b < sig.body_count; b++) {
                if (sig.body_predicates != null && sig.body_predicates[b] == current) {
                    depends_on_current = true;
                    break;
                }
            }
            if (!depends_on_current) continue;
            SymbolId head = sig.head_predicate;
            if (head == (SymbolId)0) continue;
            if (deduce_symbol_id_slice_contains(queue[..queue_len], head)) continue;
            if (queue_len >= queue.len) return false;
            queue[queue_len++] = head;
        }
    }
    return true;
}

fn void deduce_db_note_mutation(Relation* rel, bool destructive_change) {
    if (rel == null || rel.db == null) return;
    DeduceDb* db = rel.db;
    if (db.incremental_mutation_epoch < ulong.max) db.incremental_mutation_epoch++;
    if (destructive_change) db.incremental_full_recompute_required = true;
    if (!deduce_db_mark_dirty_with_rule_dependents(db, rel.name)) {
        db.incremental_full_recompute_required = true;
    }
}

fn void deduce_db_note_untracked_write_commit(DeduceDb* db) {
    if (db == null) return;
    if (db.incremental_mutation_epoch < ulong.max) db.incremental_mutation_epoch++;
    db.incremental_full_recompute_required = true;
}

fn void deduce_db_reset_incremental_tracking(DeduceDb* db) {
    if (db == null) return;
    db.incremental_dirty_count = 0;
    db.incremental_full_recompute_required = false;
}

fn void deduce_db_dependency_edge_counts(DeduceDb* db, long* total_out, long* negated_out) {
    if (total_out != null) *total_out = 0;
    if (negated_out != null) *negated_out = 0;
    if (db == null) return;

    long total = 0;
    long negated = 0;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        for (usz b = 0; b < sig.body_count; b++) {
            total++;
            if (sig.body_negated != null && sig.body_negated[b]) negated++;
        }
    }
    if (total_out != null) *total_out = total;
    if (negated_out != null) *negated_out = negated;
}

fn bool deduce_db_lookup_relation_arity(DeduceDb* db, SymbolId name, usz* arity_out) {
    if (arity_out == null) return false;
    DeduceRelationSchema* schema = deduce_db_lookup_relation_schema(db, name);
    if (schema == null) return false;
    *arity_out = schema.arity;
    return true;
}

fn bool deduce_index_column_bound(ulong bound_mask, usz col_pos) @inline {
    usz width = (usz)(ulong.sizeof * 8);
    if (col_pos >= width) return false;
    return (bound_mask & ((ulong)1 << col_pos)) != 0;
}

fn int deduce_schema_select_index_for_bound_mask(DeduceRelationSchema* schema, ulong bound_mask) {
    if (schema == null || schema.indexes == null || schema.index_count == 0) return -1;

    int best_index = -1;
    int best_score = -1;
    for (usz i = 0; i < schema.index_count; i++) {
        DeduceIndexDescriptor* idx = &schema.indexes[i];
        if (idx.column_positions == null || idx.column_count == 0) continue;

        usz prefix_len = 0;
        for (usz c = 0; c < idx.column_count; c++) {
            if (!deduce_index_column_bound(bound_mask, idx.column_positions[c])) break;
            prefix_len++;
        }
        if (prefix_len == 0) continue;

        int score = (int)(prefix_len * 128);
        score -= (int)idx.column_count;
        if (idx.unique) score += 4;
        if (score > best_score) {
            best_score = score;
            best_index = (int)i;
        }
    }
    return best_index;
}

fn bool deduce_db_ensure_rule_signature_capacity(DeduceDb* db, usz needed) {
    if (db == null) return false;
    if (needed <= db.rule_signature_capacity) return true;

    usz new_cap = db.rule_signature_capacity > 0 ? db.rule_signature_capacity : 8;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    DeduceRuleSignature* next =
        (DeduceRuleSignature*)mem::realloc(db.rule_signatures, DeduceRuleSignature.sizeof * new_cap);
    if (next == null) return false;
    db.rule_signatures = next;
    db.rule_signature_capacity = new_cap;
    return true;
}

fn void deduce_rule_signature_release_allocations(DeduceRuleSignature* sig) {
    if (sig == null) return;
    if (sig.body_predicates != null) {
        mem::free(sig.body_predicates);
        sig.body_predicates = null;
    }
    if (sig.body_negated != null) {
        mem::free(sig.body_negated);
        sig.body_negated = null;
    }
    if (sig.body_bound_masks != null) {
        mem::free(sig.body_bound_masks);
        sig.body_bound_masks = null;
    }
    if (sig.body_selected_indexes != null) {
        mem::free(sig.body_selected_indexes);
        sig.body_selected_indexes = null;
    }
    if (sig.body_filter_pushdown != null) {
        mem::free(sig.body_filter_pushdown);
        sig.body_filter_pushdown = null;
    }
    if (sig.body_projection_masks != null) {
        mem::free(sig.body_projection_masks);
        sig.body_projection_masks = null;
    }
    if (sig.head_term_is_variable != null) {
        mem::free(sig.head_term_is_variable);
        sig.head_term_is_variable = null;
    }
    if (sig.head_term_var_ids != null) {
        mem::free(sig.head_term_var_ids);
        sig.head_term_var_ids = null;
    }
    if (sig.head_term_literals != null) {
        mem::free(sig.head_term_literals);
        sig.head_term_literals = null;
    }
    if (sig.body_term_offsets != null) {
        mem::free(sig.body_term_offsets);
        sig.body_term_offsets = null;
    }
    if (sig.body_term_counts != null) {
        mem::free(sig.body_term_counts);
        sig.body_term_counts = null;
    }
    if (sig.body_term_is_variable != null) {
        mem::free(sig.body_term_is_variable);
        sig.body_term_is_variable = null;
    }
    if (sig.body_term_var_ids != null) {
        mem::free(sig.body_term_var_ids);
        sig.body_term_var_ids = null;
    }
    if (sig.body_term_literals != null) {
        mem::free(sig.body_term_literals);
        sig.body_term_literals = null;
    }
    sig.head_term_count = 0;
    sig.body_term_total_count = 0;
    sig.max_var_id = 0;
}

fn DeduceRuleRegisterStatus deduce_db_add_rule_signature(
    DeduceDb* db,
    SymbolId head_predicate,
    SymbolId[] body_predicates,
    bool[] body_negated,
    ulong[] body_bound_masks,
    int[] body_selected_indexes,
    bool[] body_filter_pushdown,
    ulong[] body_projection_masks,
    bool[] head_term_is_variable,
    uint[] head_term_var_ids,
    Value*[] head_term_literals,
    usz[] body_term_offsets,
    usz[] body_term_counts,
    bool[] body_term_is_variable,
    uint[] body_term_var_ids,
    Value*[] body_term_literals,
    uint max_var_id
) {
    if (db == null ||
        body_predicates.len != body_negated.len ||
        body_predicates.len != body_bound_masks.len ||
        body_predicates.len != body_selected_indexes.len ||
        body_predicates.len != body_filter_pushdown.len ||
        body_predicates.len != body_projection_masks.len ||
        head_term_is_variable.len != head_term_var_ids.len ||
        head_term_is_variable.len != head_term_literals.len ||
        body_term_offsets.len != body_term_counts.len ||
        body_term_is_variable.len != body_term_var_ids.len ||
        body_term_is_variable.len != body_term_literals.len) {
        return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
    }
    for (usz i = 0; i < body_term_counts.len; i++) {
        if (i >= body_term_offsets.len) return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        if (body_term_offsets[i] + body_term_counts[i] > body_term_is_variable.len) {
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
    }

    usz needed = db.rule_signature_count + 1;
    if (!deduce_db_ensure_rule_signature_capacity(db, needed)) {
        return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
    }

    DeduceRuleSignature* dst = &db.rule_signatures[db.rule_signature_count];
    dst.head_predicate = head_predicate;
    dst.body_count = body_predicates.len;
    dst.body_predicates = null;
    dst.body_negated = null;
    dst.body_bound_masks = null;
    dst.body_selected_indexes = null;
    dst.body_filter_pushdown = null;
    dst.body_projection_masks = null;
    dst.head_term_is_variable = null;
    dst.head_term_var_ids = null;
    dst.head_term_literals = null;
    dst.head_term_count = head_term_is_variable.len;
    dst.body_term_offsets = null;
    dst.body_term_counts = null;
    dst.body_term_is_variable = null;
    dst.body_term_var_ids = null;
    dst.body_term_literals = null;
    dst.body_term_total_count = body_term_is_variable.len;
    dst.max_var_id = max_var_id;

    if (body_predicates.len > 0) {
        dst.body_predicates = (SymbolId*)mem::malloc(SymbolId.sizeof * body_predicates.len);
        if (dst.body_predicates == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_negated = (bool*)mem::malloc(bool.sizeof * body_negated.len);
        if (dst.body_negated == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_bound_masks = (ulong*)mem::malloc(ulong.sizeof * body_bound_masks.len);
        if (dst.body_bound_masks == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_selected_indexes = (int*)mem::malloc(int.sizeof * body_selected_indexes.len);
        if (dst.body_selected_indexes == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_filter_pushdown = (bool*)mem::malloc(bool.sizeof * body_filter_pushdown.len);
        if (dst.body_filter_pushdown == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_projection_masks = (ulong*)mem::malloc(ulong.sizeof * body_projection_masks.len);
        if (dst.body_projection_masks == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < body_predicates.len; i++) {
            dst.body_predicates[i] = body_predicates[i];
            dst.body_negated[i] = body_negated[i];
            dst.body_bound_masks[i] = body_bound_masks[i];
            dst.body_selected_indexes[i] = body_selected_indexes[i];
            dst.body_filter_pushdown[i] = body_filter_pushdown[i];
            dst.body_projection_masks[i] = body_projection_masks[i];
        }
    }

    if (head_term_is_variable.len > 0) {
        dst.head_term_is_variable = (bool*)mem::malloc(bool.sizeof * head_term_is_variable.len);
        if (dst.head_term_is_variable == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.head_term_var_ids = (uint*)mem::malloc(uint.sizeof * head_term_var_ids.len);
        if (dst.head_term_var_ids == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.head_term_literals = (Value**)mem::malloc(Value*.sizeof * head_term_literals.len);
        if (dst.head_term_literals == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < head_term_is_variable.len; i++) {
            dst.head_term_is_variable[i] = head_term_is_variable[i];
            dst.head_term_var_ids[i] = head_term_var_ids[i];
            dst.head_term_literals[i] = head_term_literals[i];
        }
    }

    if (body_term_counts.len > 0) {
        dst.body_term_offsets = (usz*)mem::malloc(usz.sizeof * body_term_offsets.len);
        if (dst.body_term_offsets == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_term_counts = (usz*)mem::malloc(usz.sizeof * body_term_counts.len);
        if (dst.body_term_counts == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < body_term_counts.len; i++) {
            dst.body_term_offsets[i] = body_term_offsets[i];
            dst.body_term_counts[i] = body_term_counts[i];
        }
    }

    if (body_term_is_variable.len > 0) {
        dst.body_term_is_variable = (bool*)mem::malloc(bool.sizeof * body_term_is_variable.len);
        if (dst.body_term_is_variable == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_term_var_ids = (uint*)mem::malloc(uint.sizeof * body_term_var_ids.len);
        if (dst.body_term_var_ids == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        dst.body_term_literals = (Value**)mem::malloc(Value*.sizeof * body_term_literals.len);
        if (dst.body_term_literals == null) {
            deduce_rule_signature_release_allocations(dst);
            return DEDUCE_RULE_REGISTER_OUT_OF_MEMORY;
        }
        for (usz i = 0; i < body_term_is_variable.len; i++) {
            dst.body_term_is_variable[i] = body_term_is_variable[i];
            dst.body_term_var_ids[i] = body_term_var_ids[i];
            dst.body_term_literals[i] = body_term_literals[i];
        }
    }

    db.rule_signature_count++;
    return DEDUCE_RULE_REGISTER_OK;
}

struct DeduceTxnMutationEntry {
    Relation* relation;
    ulong     inserted_count;
    ulong     deleted_count;
    bool      destructive_change;
}

struct DeduceTxn {
    DeduceDb* db;
    MdbTxn*   txn;
    bool      open;
    bool      read_only;
    bool      mutation_pending;
    DeduceTxnMutationEntry* mutation_entries;
    usz       mutation_entry_count;
    usz       mutation_entry_capacity;
}

fn bool deduce_txn_ensure_mutation_capacity(DeduceTxn* txn, usz needed) {
    if (txn == null) return false;
    if (needed <= txn.mutation_entry_capacity) return true;

    usz new_cap = txn.mutation_entry_capacity > 0 ? txn.mutation_entry_capacity : 4;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    DeduceTxnMutationEntry* next =
        (DeduceTxnMutationEntry*)mem::realloc(txn.mutation_entries, DeduceTxnMutationEntry.sizeof * new_cap);
    if (next == null) return false;
    txn.mutation_entries = next;
    txn.mutation_entry_capacity = new_cap;
    return true;
}

fn DeduceTxnMutationEntry* deduce_txn_lookup_or_add_mutation_entry(DeduceTxn* txn, Relation* rel) {
    if (txn == null || rel == null) return null;
    for (usz i = 0; i < txn.mutation_entry_count; i++) {
        DeduceTxnMutationEntry* entry = &txn.mutation_entries[i];
        if (entry.relation == rel) return entry;
    }

    usz needed = txn.mutation_entry_count + 1;
    if (!deduce_txn_ensure_mutation_capacity(txn, needed)) return null;

    DeduceTxnMutationEntry* added = &txn.mutation_entries[txn.mutation_entry_count];
    added.relation = rel;
    added.inserted_count = 0;
    added.deleted_count = 0;
    added.destructive_change = false;
    txn.mutation_entry_count++;
    return added;
}

fn bool deduce_txn_log_insert(DeduceTxn* txn, Relation* rel) {
    DeduceTxnMutationEntry* entry = deduce_txn_lookup_or_add_mutation_entry(txn, rel);
    if (entry == null) return false;
    if (entry.inserted_count < ulong.max) entry.inserted_count++;
    return true;
}

fn bool deduce_txn_log_delete(DeduceTxn* txn, Relation* rel) {
    DeduceTxnMutationEntry* entry = deduce_txn_lookup_or_add_mutation_entry(txn, rel);
    if (entry == null) return false;
    if (entry.deleted_count < ulong.max) entry.deleted_count++;
    entry.destructive_change = true;
    return true;
}

fn void deduce_relation_schema_apply_mutation_delta(Relation* rel, ulong inserted_count, ulong deleted_count) {
    DeduceRelationSchema* schema = deduce_relation_lookup_schema(rel);
    if (schema == null) return;

    ulong cardinality = schema.cardinality_estimate;
    if (inserted_count > 0) {
        if (ulong.max - cardinality < inserted_count) {
            cardinality = ulong.max;
        } else {
            cardinality += inserted_count;
        }
    }
    if (deleted_count > 0) {
        if (deleted_count >= cardinality) {
            cardinality = 0;
        } else {
            cardinality -= deleted_count;
        }
    }
    schema.cardinality_estimate = cardinality;
    schema.distinct_estimate = cardinality;
}

fn void deduce_txn_apply_mutation_log(DeduceTxn* txn) {
    if (txn == null || txn.mutation_entries == null || txn.mutation_entry_count == 0) return;
    for (usz i = 0; i < txn.mutation_entry_count; i++) {
        DeduceTxnMutationEntry* entry = &txn.mutation_entries[i];
        Relation* rel = entry.relation;
        if (rel == null || rel.db == null) continue;
        deduce_relation_schema_apply_mutation_delta(rel, entry.inserted_count, entry.deleted_count);
        bool destructive = entry.destructive_change || entry.deleted_count > 0;
        deduce_db_note_mutation(rel, destructive);
    }
}

fn void deduce_txn_clear_mutation_log(DeduceTxn* txn) {
    if (txn == null) return;
    if (txn.mutation_entries != null) {
        mem::free(txn.mutation_entries);
        txn.mutation_entries = null;
    }
    txn.mutation_entry_count = 0;
    txn.mutation_entry_capacity = 0;
}

fn void deduce_txn_finalize(DeduceTxn* txn) @inline {
    if (txn == null) return;
    if (txn.open && txn.txn != null) {
        mdb_txn_abort(txn.txn);
    }
    txn.txn = null;
    txn.open = false;
    txn.db = null;
    txn.mutation_pending = false;
    deduce_txn_clear_mutation_log(txn);
}

fn void deduce_txn_finalizer(void* handle) {
    if (handle == null) return;
    DeduceTxn* txn = (DeduceTxn*)handle;
    deduce_txn_finalize(txn);
}
