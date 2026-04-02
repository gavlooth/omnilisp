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

struct DeduceCheckConstraint {
    SymbolId local_column;
    SymbolId predicate;
}

const usz DEDUCE_GOAL_DIRECTED_READ_SUBJECT_KEY_LIMIT = 8;

struct DeduceGoalDirectedReadSubjectKeyCapture {
    char*[DEDUCE_GOAL_DIRECTED_READ_SUBJECT_KEY_LIMIT] keys;
    usz[DEDUCE_GOAL_DIRECTED_READ_SUBJECT_KEY_LIMIT] key_lens;
    usz count;
    bool complete;
}

enum DeduceIntegrityViolationClass {
    DEDUCE_INTEGRITY_VIOLATION_KEY,
    DEDUCE_INTEGRITY_VIOLATION_UNIQUE,
    DEDUCE_INTEGRITY_VIOLATION_REFERENCE,
    DEDUCE_INTEGRITY_VIOLATION_CHECK,
}

struct DeduceIntegrityViolationEntry {
    ulong                         sequence;
    SymbolId                      relation;
    SymbolId                      code;
    DeduceIntegrityViolationClass violation_class;
}

struct DeducePersistedRelationMetaV1 {
    char version;
    char flags;
}

struct DeducePersistedRelationMetaV2 {
    char version;
    char flags;
    char stale_reason;
    char reserved;
    ulong materialized_refresh_count;
    ulong materialized_last_refresh_mutation_epoch;
    ulong materialized_last_stale_mutation_epoch;
}

struct DeducePersistedRuleCatalogRecordV1 {
    char  version;
    char  reserved0;
    char  reserved1;
    char  reserved2;
    char  reserved3;
    char  reserved4;
    char  reserved5;
    char  reserved6;
    ulong body_count;
    ulong head_name_len;
}

struct DeducePersistedRuleCatalogBodyAtomV1 {
    char  negated;
    char  reserved0;
    char  reserved1;
    char  reserved2;
    char  reserved3;
    char  reserved4;
    char  reserved5;
    char  reserved6;
    ulong name_len;
}

struct DeducePersistedRuleSignatureRecordV1 {
    char     version;
    char     reserved0;
    char     reserved1;
    char     reserved2;
    char     reserved3;
    char     reserved4;
    char     reserved5;
    char     reserved6;
    SymbolId head_predicate;
    ulong    body_count;
    ulong    head_term_count;
    ulong    body_term_total_count;
    uint     max_var_id;
    uint     reserved7;
    ulong    head_literal_blob_len;
    ulong    body_literal_blob_len;
}

const usz DEDUCE_INTEGRITY_HISTORY_LIMIT = 8;
const char DEDUCE_PERSISTED_RELATION_META_VERSION = 2;
const char DEDUCE_PERSISTED_RULE_CATALOG_VERSION = 1;
const char DEDUCE_PERSISTED_RULE_SIGNATURE_VERSION = 1;

// Test seam: force relation metadata persistence failure paths.
bool g_deduce_force_materialized_meta_persist_fail = false;
const char DEDUCE_PERSISTED_RELATION_META_FLAG_MATERIALIZED = 1 << 0;
const char DEDUCE_PERSISTED_RELATION_META_FLAG_MANUAL_REFRESH = 1 << 1;
const char DEDUCE_PERSISTED_RELATION_META_FLAG_ON_READ_REFRESH = 1 << 2;

enum DeduceMaterializedStaleReason {
    DEDUCE_MATERIALIZED_STALE_NONE,
    DEDUCE_MATERIALIZED_STALE_NEVER_REFRESHED,
    DEDUCE_MATERIALIZED_STALE_RULE_SET_CHANGE,
    DEDUCE_MATERIALIZED_STALE_DEPENDENCY_DIRTY,
    DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED,
}

struct DeduceRelationSchema {
    SymbolId               name;
    SymbolId*              columns;
    usz                    arity;
    bool                   inferred_columns;
    bool                   dropped;
    bool                   materialized_view;
    SymbolId               key_column;
    SymbolId               unique_column;
    SymbolId*              unique_columns;
    usz                    unique_column_count;
    DeduceReferenceConstraint* reference_constraints;
    usz                    reference_constraint_count;
    DeduceCheckConstraint* check_constraints;
    usz                    check_constraint_count;
    SymbolId               index_column;
    DeduceIndexDescriptor* indexes;
    usz                    index_count;
    ulong                  cardinality_estimate;
    ulong                  distinct_estimate;
    ulong                  integrity_violation_count;
    ulong                  key_integrity_violation_count;
    ulong                  unique_integrity_violation_count;
    ulong                  reference_integrity_violation_count;
    ulong                  check_integrity_violation_count;
    SymbolId               last_integrity_violation_code;
    SymbolId               materialized_refresh_policy;
    ulong                  materialized_refresh_count;
    ulong                  materialized_last_refresh_mutation_epoch;
    ulong                  materialized_last_stale_mutation_epoch;
    DeduceMaterializedStaleReason materialized_last_stale_reason;
    DeduceDeltaTupleSet    maintained_pending_insertions;
    SymbolId               last_goal_directed_read_execution_path;
    SymbolId               last_goal_directed_read_surface;
    SymbolId               last_goal_directed_read_fallback_reason;
    long                   last_goal_directed_read_selector_rule_index;
    ulong                  last_goal_directed_read_mutation_epoch;
    ulong                  last_goal_directed_read_requested_bound_count;
    usz*                   last_goal_directed_read_requested_bound_positions;
    ulong                  last_goal_directed_read_applied_bound_count;
    usz*                   last_goal_directed_read_applied_bound_positions;
    DeduceGoalDirectedReadSubjectKeyCapture last_goal_directed_read_subject_keys;
    DeduceObservedStepCounters* last_goal_directed_read_step_counters;
    usz                    last_goal_directed_read_step_count;
    DeduceIntegrityViolationEntry[DEDUCE_INTEGRITY_HISTORY_LIMIT] integrity_violation_history;
    usz                    integrity_violation_history_count;
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

struct DeducePersistedRuleCatalogEntry {
    SymbolId  head_predicate;
    SymbolId* body_predicates;
    bool*     body_negated;
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
    DeducePersistedRuleCatalogEntry* persisted_rule_catalog;
    usz                   persisted_rule_catalog_count;
    usz                   persisted_rule_catalog_capacity;
    SymbolId*             incremental_dirty_predicates;
    usz                   incremental_dirty_count;
    usz                   incremental_dirty_capacity;
    ulong                 incremental_mutation_epoch;
    bool                  incremental_full_recompute_required;
    ulong                 integrity_violation_count;
    ulong                 key_integrity_violation_count;
    ulong                 unique_integrity_violation_count;
    ulong                 reference_integrity_violation_count;
    ulong                 check_integrity_violation_count;
    SymbolId              last_integrity_violation_code;
    SymbolId              last_integrity_violation_relation;
    SymbolId              last_goal_directed_read_execution_path;
    SymbolId              last_goal_directed_read_surface;
    SymbolId              last_goal_directed_read_relation;
    SymbolId              last_goal_directed_read_fallback_reason;
    long                  last_goal_directed_read_selector_rule_index;
    ulong                 last_goal_directed_read_mutation_epoch;
    ulong                 last_goal_directed_read_requested_bound_count;
    usz*                  last_goal_directed_read_requested_bound_positions;
    ulong                 last_goal_directed_read_applied_bound_count;
    usz*                  last_goal_directed_read_applied_bound_positions;
    DeduceObservedStepCounters* last_goal_directed_read_step_counters;
    usz                   last_goal_directed_read_step_count;
    DeduceIntegrityViolationEntry[DEDUCE_INTEGRITY_HISTORY_LIMIT] integrity_violation_history;
    usz                   integrity_violation_history_count;
    ulong                 integrity_violation_sequence;
}

const char[] DEDUCE_HEX_DIGITS = "0123456789abcdef";
const char[] DEDUCE_EXTENSIONAL_DBI_SUFFIX = "__deduce_extensional";
const char[] DEDUCE_RELATION_META_DBI_NAME = "__deduce_relation_meta";
const char[] DEDUCE_RULE_CATALOG_DBI_NAME = "__deduce_rule_catalog";
const char[] DEDUCE_RULE_SIGNATURE_DBI_NAME = "__deduce_rule_signatures";
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

fn bool deduce_open_relation_meta_dbi(
    DeduceDb* db,
    MdbTxn* txn,
    bool create,
    MdbDbi* dbi_out
) {
    if (db == null || !db.open || txn == null || dbi_out == null) return false;
    char* dbi_name = deduce_copy_c_string(DEDUCE_RELATION_META_DBI_NAME);
    if (dbi_name == null) return false;
    defer mem::free(dbi_name);

    MdbDbi dbi = 0;
    int rc = mdb_dbi_open(txn, dbi_name, create ? MDB_CREATE : 0, &dbi);
    if (rc != MDB_SUCCESS) return false;
    *dbi_out = dbi;
    return true;
}

fn bool deduce_open_rule_catalog_dbi(
    DeduceDb* db,
    MdbTxn* txn,
    bool create,
    MdbDbi* dbi_out
) {
    if (db == null || !db.open || txn == null || dbi_out == null) return false;
    char* dbi_name = deduce_copy_c_string(DEDUCE_RULE_CATALOG_DBI_NAME);
    if (dbi_name == null) return false;
    defer mem::free(dbi_name);

    MdbDbi dbi = 0;
    int rc = mdb_dbi_open(txn, dbi_name, create ? MDB_CREATE : 0, &dbi);
    if (rc != MDB_SUCCESS) return false;
    *dbi_out = dbi;
    return true;
}

fn bool deduce_open_rule_signature_dbi(
    DeduceDb* db,
    MdbTxn* txn,
    bool create,
    MdbDbi* dbi_out
) {
    if (db == null || !db.open || txn == null || dbi_out == null) return false;
    char* dbi_name = deduce_copy_c_string(DEDUCE_RULE_SIGNATURE_DBI_NAME);
    if (dbi_name == null) return false;
    defer mem::free(dbi_name);

    MdbDbi dbi = 0;
    int rc = mdb_dbi_open(txn, dbi_name, create ? MDB_CREATE : 0, &dbi);
    if (rc != MDB_SUCCESS) return false;
    *dbi_out = dbi;
    return true;
}

fn void deduce_persisted_rule_catalog_entry_release(DeducePersistedRuleCatalogEntry* entry) {
    if (entry == null) return;
    if (entry.body_predicates != null) {
        mem::free(entry.body_predicates);
        entry.body_predicates = null;
    }
    if (entry.body_negated != null) {
        mem::free(entry.body_negated);
        entry.body_negated = null;
    }
    entry.head_predicate = (SymbolId)0;
    entry.body_count = 0;
}

fn void deduce_db_clear_persisted_rule_catalog(DeduceDb* db) {
    if (db == null || db.persisted_rule_catalog == null) {
        if (db != null) {
            db.persisted_rule_catalog_count = 0;
            db.persisted_rule_catalog_capacity = 0;
        }
        return;
    }
    for (usz i = 0; i < db.persisted_rule_catalog_count; i++) {
        deduce_persisted_rule_catalog_entry_release(&db.persisted_rule_catalog[i]);
    }
    mem::free(db.persisted_rule_catalog);
    db.persisted_rule_catalog = null;
    db.persisted_rule_catalog_count = 0;
    db.persisted_rule_catalog_capacity = 0;
}

fn bool deduce_db_ensure_persisted_rule_catalog_capacity(DeduceDb* db, usz needed) {
    if (db == null) return false;
    if (needed <= db.persisted_rule_catalog_capacity) return true;

    usz new_cap = db.persisted_rule_catalog_capacity > 0 ? db.persisted_rule_catalog_capacity : 8;
    while (new_cap < needed) {
        if (new_cap > usz.max / 2) {
            new_cap = needed;
            break;
        }
        new_cap *= 2;
    }

    DeducePersistedRuleCatalogEntry* next =
        (DeducePersistedRuleCatalogEntry*)mem::realloc(
            db.persisted_rule_catalog,
            DeducePersistedRuleCatalogEntry.sizeof * new_cap
        );
    if (next == null) return false;
    db.persisted_rule_catalog = next;
    db.persisted_rule_catalog_capacity = new_cap;
    return true;
}

fn bool deduce_db_append_persisted_rule_catalog_entry(
    DeduceDb* db,
    SymbolId head_predicate,
    SymbolId[] body_predicates,
    bool[] body_negated
) {
    if (db == null || body_predicates.len != body_negated.len) return false;
    usz needed = db.persisted_rule_catalog_count + 1;
    if (!deduce_db_ensure_persisted_rule_catalog_capacity(db, needed)) return false;

    DeducePersistedRuleCatalogEntry* entry = &db.persisted_rule_catalog[db.persisted_rule_catalog_count];
    entry.head_predicate = head_predicate;
    entry.body_predicates = null;
    entry.body_negated = null;
    entry.body_count = body_predicates.len;

    if (body_predicates.len > 0) {
        entry.body_predicates = (SymbolId*)mem::malloc(SymbolId.sizeof * body_predicates.len);
        if (entry.body_predicates == null) {
            deduce_persisted_rule_catalog_entry_release(entry);
            return false;
        }
        entry.body_negated = (bool*)mem::malloc(bool.sizeof * body_negated.len);
        if (entry.body_negated == null) {
            deduce_persisted_rule_catalog_entry_release(entry);
            return false;
        }
        for (usz i = 0; i < body_predicates.len; i++) {
            entry.body_predicates[i] = body_predicates[i];
            entry.body_negated[i] = body_negated[i];
        }
    }

    db.persisted_rule_catalog_count++;
    return true;
}

fn usz deduce_rule_catalog_record_size(DeduceRuleSignature* sig, Interp* interp) {
    if (sig == null || interp == null || sig.head_predicate == (SymbolId)0) return 0;
    char[] head_name = interp.symbols.get_name(sig.head_predicate);
    usz total = DeducePersistedRuleCatalogRecordV1.sizeof + head_name.len;
    for (usz i = 0; i < sig.body_count; i++) {
        if (sig.body_predicates == null) return 0;
        char[] body_name = interp.symbols.get_name(sig.body_predicates[i]);
        total += DeducePersistedRuleCatalogBodyAtomV1.sizeof + body_name.len;
    }
    return total;
}

fn void deduce_write_u64_le(char* out, ulong value) @inline {
    if (out == null) return;
    for (usz i = 0; i < ulong.sizeof; i++) {
        out[i] = (char)((value >> (i * 8)) & 0xFF);
    }
}

fn bool deduce_value_supported_by_tuple_codec(Value* v) @inline {
    if (v == null || v.tag == NIL) return true;
    return v.tag == INT ||
        v.tag == DOUBLE ||
        v.tag == STRING ||
        v.tag == SYMBOL ||
        v.tag == TIME_POINT;
}

fn bool deduce_rule_signature_literals_supported(DeduceRuleSignature* sig) {
    if (sig == null) return false;
    for (usz i = 0; i < sig.head_term_count; i++) {
        if (sig.head_term_literals == null) break;
        if (!deduce_value_supported_by_tuple_codec(sig.head_term_literals[i])) return false;
    }
    for (usz i = 0; i < sig.body_term_total_count; i++) {
        if (sig.body_term_literals == null) break;
        if (!deduce_value_supported_by_tuple_codec(sig.body_term_literals[i])) return false;
    }
    return true;
}

fn usz? deduce_tuple_encoded_size(Value*[] vals) {
    usz total = 0;
    for (usz i = 0; i < vals.len; i++) {
        Value* v = vals[i];
        if (v == null || v.tag == NIL) {
            total += 1;
        } else if (v.tag == INT || v.tag == DOUBLE) {
            total += 9;
        } else if (v.tag == STRING) {
            usz slen = v.str_len;
            if (slen > 0xFFFF) slen = 0xFFFF;
            total += 3 + slen;
        } else if (v.tag == SYMBOL) {
            total += 3;
        } else if (v.tag == TIME_POINT) {
            total += 34;
        } else {
            return WRITE_FAILED~;
        }
    }
    return total;
}

fn bool deduce_encode_rule_catalog_key(ulong ordinal, char[ulong.sizeof]* out) {
    if (out == null) return false;
    deduce_write_u64_le(&(*out)[0], ordinal);
    return true;
}

fn usz deduce_rule_signature_record_size(
    DeduceRuleSignature* sig,
    usz head_literal_blob_len,
    usz body_literal_blob_len
) {
    if (sig == null) return 0;
    return DeducePersistedRuleSignatureRecordV1.sizeof +
        (SymbolId.sizeof * sig.body_count) +
        (bool.sizeof * sig.body_count) +
        (ulong.sizeof * sig.body_count) +
        (int.sizeof * sig.body_count) +
        (bool.sizeof * sig.body_count) +
        (ulong.sizeof * sig.body_count) +
        (DeduceRuleTermKind.sizeof * sig.head_term_count) +
        (DeduceAggregateOpKind.sizeof * sig.head_term_count) +
        (bool.sizeof * sig.head_term_count) +
        (uint.sizeof * sig.head_term_count) +
        head_literal_blob_len +
        (usz.sizeof * sig.body_count) +
        (usz.sizeof * sig.body_count) +
        (bool.sizeof * sig.body_term_total_count) +
        (uint.sizeof * sig.body_term_total_count) +
        body_literal_blob_len;
}

fn bool deduce_copy_bytes_into_record(char[] out, usz* cursor, void* src, usz len) {
    if (out.ptr == null || cursor == null) return false;
    if (*cursor + len > out.len) return false;
    if (len > 0 && src == null) return false;
    if (len > 0) mem::copy(out.ptr + *cursor, src, len);
    *cursor += len;
    return true;
}

fn bool deduce_encode_rule_signature_record(
    DeduceRuleSignature* sig,
    Interp* interp,
    char[] out
) {
    if (sig == null || interp == null) return false;
    if (!deduce_rule_signature_literals_supported(sig)) return false;

    usz? head_literal_blob_len_opt = deduce_tuple_encoded_size(sig.head_term_literals[0:sig.head_term_count]);
    if (catch err = head_literal_blob_len_opt) return false;
    usz? body_literal_blob_len_opt = deduce_tuple_encoded_size(sig.body_term_literals[0:sig.body_term_total_count]);
    if (catch err = body_literal_blob_len_opt) return false;
    usz head_literal_blob_len = head_literal_blob_len_opt;
    usz body_literal_blob_len = body_literal_blob_len_opt;
    usz needed = deduce_rule_signature_record_size(sig, head_literal_blob_len, body_literal_blob_len);
    if (needed == 0 || out.len < needed) return false;

    DeducePersistedRuleSignatureRecordV1 header = {
        .version = DEDUCE_PERSISTED_RULE_SIGNATURE_VERSION,
        .reserved0 = 0,
        .reserved1 = 0,
        .reserved2 = 0,
        .reserved3 = 0,
        .reserved4 = 0,
        .reserved5 = 0,
        .reserved6 = 0,
        .head_predicate = sig.head_predicate,
        .body_count = sig.body_count,
        .head_term_count = sig.head_term_count,
        .body_term_total_count = sig.body_term_total_count,
        .max_var_id = sig.max_var_id,
        .reserved7 = 0,
        .head_literal_blob_len = head_literal_blob_len,
        .body_literal_blob_len = body_literal_blob_len,
    };

    usz cursor = 0;
    if (!deduce_copy_bytes_into_record(out, &cursor, &header, DeducePersistedRuleSignatureRecordV1.sizeof)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_predicates, SymbolId.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_negated, bool.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_bound_masks, ulong.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_selected_indexes, int.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_filter_pushdown, bool.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_projection_masks, ulong.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.head_term_kinds, DeduceRuleTermKind.sizeof * sig.head_term_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.head_term_aggregate_ops, DeduceAggregateOpKind.sizeof * sig.head_term_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.head_term_is_variable, bool.sizeof * sig.head_term_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.head_term_var_ids, uint.sizeof * sig.head_term_count)) return false;

    if (head_literal_blob_len > 0) {
        usz? encoded_head_literals = encode_tuple(sig.head_term_literals[0:sig.head_term_count], out.ptr + cursor, head_literal_blob_len);
        if (catch err = encoded_head_literals) return false;
        if (encoded_head_literals != head_literal_blob_len) return false;
    }
    cursor += head_literal_blob_len;

    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_term_offsets, usz.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_term_counts, usz.sizeof * sig.body_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_term_is_variable, bool.sizeof * sig.body_term_total_count)) return false;
    if (!deduce_copy_bytes_into_record(out, &cursor, sig.body_term_var_ids, uint.sizeof * sig.body_term_total_count)) return false;

    if (body_literal_blob_len > 0) {
        usz? encoded_body_literals = encode_tuple(sig.body_term_literals[0:sig.body_term_total_count], out.ptr + cursor, body_literal_blob_len);
        if (catch err = encoded_body_literals) return false;
        if (encoded_body_literals != body_literal_blob_len) return false;
    }
    cursor += body_literal_blob_len;
    return cursor == needed;
}

fn bool deduce_record_slice(char[] data, usz* cursor, usz len, char[]* out_slice) {
    if (cursor == null || out_slice == null) return false;
    if (*cursor + len > data.len) return false;
    if (len == 0) {
        *out_slice = {};
        return true;
    }
    *out_slice = (((char*)data.ptr) + *cursor)[:len];
    *cursor += len;
    return true;
}

fn bool deduce_encode_rule_catalog_record(
    DeduceRuleSignature* sig,
    Interp* interp,
    char[] out
) {
    if (sig == null || interp == null) return false;
    usz needed = deduce_rule_catalog_record_size(sig, interp);
    if (needed == 0 || out.len < needed) return false;

    char[] head_name = interp.symbols.get_name(sig.head_predicate);
    DeducePersistedRuleCatalogRecordV1 header = {
        .version = DEDUCE_PERSISTED_RULE_CATALOG_VERSION,
        .reserved0 = 0,
        .reserved1 = 0,
        .reserved2 = 0,
        .reserved3 = 0,
        .reserved4 = 0,
        .reserved5 = 0,
        .reserved6 = 0,
        .body_count = sig.body_count,
        .head_name_len = head_name.len
    };
    mem::copy(out.ptr, &header, DeducePersistedRuleCatalogRecordV1.sizeof);
    usz cursor = DeducePersistedRuleCatalogRecordV1.sizeof;
    if (head_name.len > 0) {
        mem::copy(out.ptr + cursor, head_name.ptr, head_name.len);
        cursor += head_name.len;
    }

    for (usz i = 0; i < sig.body_count; i++) {
        char[] body_name = interp.symbols.get_name(sig.body_predicates[i]);
        DeducePersistedRuleCatalogBodyAtomV1 atom = {
            .negated = sig.body_negated != null && sig.body_negated[i] ? (char)1 : (char)0,
            .reserved0 = 0,
            .reserved1 = 0,
            .reserved2 = 0,
            .reserved3 = 0,
            .reserved4 = 0,
            .reserved5 = 0,
            .reserved6 = 0,
            .name_len = body_name.len
        };
        mem::copy(out.ptr + cursor, &atom, DeducePersistedRuleCatalogBodyAtomV1.sizeof);
        cursor += DeducePersistedRuleCatalogBodyAtomV1.sizeof;
        if (body_name.len > 0) {
            mem::copy(out.ptr + cursor, body_name.ptr, body_name.len);
            cursor += body_name.len;
        }
    }
    return cursor == needed;
}

fn Value* deduce_db_restore_persisted_rule_catalog(DeduceDb* db, Interp* interp) {
    if (db == null || !db.open || interp == null) return null;

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/rule-catalog-restore-txn-open-failed",
            "deduce/open: persisted rule catalog txn open failed");
    }
    defer mdb_txn_abort(txn);

    MdbDbi rule_catalog_dbi = 0;
    if (!deduce_open_rule_catalog_dbi(db, txn, false, &rule_catalog_dbi)) return null;

    MdbCursor* cursor = null;
    rc = mdb_cursor_open(txn, rule_catalog_dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/rule-catalog-restore-cursor-open-failed",
            "deduce/open: persisted rule catalog cursor open failed");
    }
    defer mdb_cursor_close(cursor);

    deduce_db_clear_persisted_rule_catalog(db);

    MdbVal key = {};
    MdbVal data = {};
    usz record_index = 0;
    for (int op = MDB_FIRST; ; op = MDB_NEXT) {
        rc = mdb_cursor_get(cursor, &key, &data, op);
        if (rc == MDB_NOTFOUND) break;
        if (rc != MDB_SUCCESS || data.mv_data == null || data.mv_size < DeducePersistedRuleCatalogRecordV1.sizeof) {
            deduce_db_clear_persisted_rule_catalog(db);
            return deduce_raise(interp, "deduce/rule-catalog-restore-read-failed",
                "deduce/open: persisted rule catalog read failed");
        }

        DeducePersistedRuleCatalogRecordV1 header = {};
        mem::copy(&header, data.mv_data, DeducePersistedRuleCatalogRecordV1.sizeof);
        if (header.version != DEDUCE_PERSISTED_RULE_CATALOG_VERSION) {
            deduce_db_clear_persisted_rule_catalog(db);
            return deduce_raise(interp, "deduce/rule-catalog-restore-invalid-version",
                "deduce/open: persisted rule catalog has unsupported version");
        }

        usz cursor_offset = DeducePersistedRuleCatalogRecordV1.sizeof;
        if (cursor_offset + header.head_name_len > data.mv_size) {
            deduce_db_clear_persisted_rule_catalog(db);
            return deduce_raise(interp, "deduce/rule-catalog-restore-invalid-record",
                "deduce/open: persisted rule catalog record is truncated");
        }

        char[] head_name = ((char*)data.mv_data + cursor_offset)[:header.head_name_len];
        SymbolId head_predicate = interp.symbols.intern(head_name);
        cursor_offset += header.head_name_len;

        SymbolId[] body_predicates = {};
        bool[] body_negated = {};
        if (header.body_count > 0) {
            body_predicates = mem::new_array(SymbolId, header.body_count);
            body_negated = mem::new_array(bool, header.body_count);
            if (body_predicates.ptr == null || body_negated.ptr == null) {
                if (body_predicates.ptr != null) mem::free(body_predicates.ptr);
                if (body_negated.ptr != null) mem::free(body_negated.ptr);
                deduce_db_clear_persisted_rule_catalog(db);
                return deduce_raise(interp, "deduce/rule-catalog-restore-out-of-memory",
                    "deduce/open: persisted rule catalog restore out of memory");
            }
        }
        defer {
            if (body_predicates.ptr != null) mem::free(body_predicates.ptr);
            if (body_negated.ptr != null) mem::free(body_negated.ptr);
        }

        for (usz i = 0; i < header.body_count; i++) {
            if (cursor_offset + DeducePersistedRuleCatalogBodyAtomV1.sizeof > data.mv_size) {
                deduce_db_clear_persisted_rule_catalog(db);
                return deduce_raise(interp, "deduce/rule-catalog-restore-invalid-record",
                    "deduce/open: persisted rule catalog record is truncated");
            }
            DeducePersistedRuleCatalogBodyAtomV1 atom = {};
            mem::copy(
                &atom,
                (char*)data.mv_data + cursor_offset,
                DeducePersistedRuleCatalogBodyAtomV1.sizeof
            );
            cursor_offset += DeducePersistedRuleCatalogBodyAtomV1.sizeof;
            if (cursor_offset + atom.name_len > data.mv_size) {
                deduce_db_clear_persisted_rule_catalog(db);
                return deduce_raise(interp, "deduce/rule-catalog-restore-invalid-record",
                    "deduce/open: persisted rule catalog record is truncated");
            }
            char[] body_name = ((char*)data.mv_data + cursor_offset)[:atom.name_len];
            body_predicates[i] = interp.symbols.intern(body_name);
            body_negated[i] = atom.negated != 0;
            cursor_offset += atom.name_len;
        }
        if (cursor_offset != data.mv_size) {
            deduce_db_clear_persisted_rule_catalog(db);
            return deduce_raise(interp, "deduce/rule-catalog-restore-invalid-record",
                "deduce/open: persisted rule catalog record has trailing data");
        }

        if (!deduce_db_append_persisted_rule_catalog_entry(db, head_predicate, body_predicates, body_negated)) {
            deduce_db_clear_persisted_rule_catalog(db);
            return deduce_raise(interp, "deduce/rule-catalog-restore-out-of-memory",
                "deduce/open: persisted rule catalog restore out of memory");
        }
    }

    return null;
}

fn Value* deduce_db_persist_rule_catalog(DeduceDb* db, Interp* interp) {
    if (db == null || !db.open || interp == null) {
        return deduce_raise(interp, "deduce/rule-catalog-invalid-state",
            "deduce: invalid persisted rule catalog state");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/rule-catalog-txn-open-failed",
            "deduce: persisted rule catalog txn open failed");
    }

    MdbDbi rule_catalog_dbi = 0;
    if (!deduce_open_rule_catalog_dbi(db, txn, true, &rule_catalog_dbi)) {
        mdb_txn_abort(txn);
        return deduce_raise(interp, "deduce/rule-catalog-open-failed",
            "deduce: persisted rule catalog dbi open failed");
    }

    rc = mdb_drop(txn, rule_catalog_dbi, 0);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        return deduce_raise(interp, "deduce/rule-catalog-clear-failed",
            "deduce: persisted rule catalog clear failed");
    }

    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        usz record_size = deduce_rule_catalog_record_size(sig, interp);
        if (record_size == 0) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-catalog-encode-failed",
                "deduce: persisted rule catalog encode failed");
        }
        char* record_buf = (char*)mem::malloc(record_size);
        if (record_buf == null) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-catalog-out-of-memory",
                "deduce: persisted rule catalog out of memory");
        }
        defer mem::free(record_buf);

        if (!deduce_encode_rule_catalog_record(sig, interp, record_buf[:record_size])) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-catalog-encode-failed",
                "deduce: persisted rule catalog encode failed");
        }

        char[ulong.sizeof] key_buf = {};
        deduce_encode_rule_catalog_key(i, &key_buf);
        MdbVal key = {
            .mv_size = key_buf.len,
            .mv_data = &key_buf[0]
        };
        MdbVal data = {
            .mv_size = record_size,
            .mv_data = record_buf
        };
        rc = mdb_put(txn, rule_catalog_dbi, &key, &data, 0);
        if (rc != MDB_SUCCESS) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-catalog-write-failed",
                "deduce: persisted rule catalog write failed");
        }
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/rule-catalog-commit-failed",
            "deduce: persisted rule catalog commit failed");
    }

    return deduce_db_restore_persisted_rule_catalog(db, interp);
}

fn Value* deduce_db_restore_persisted_rule_signatures(DeduceDb* db, Interp* interp) {
    if (db == null || !db.open || interp == null) return null;

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/rule-signature-restore-txn-open-failed",
            "deduce/open: persisted rule signature txn open failed");
    }
    defer mdb_txn_abort(txn);

    MdbDbi rule_signature_dbi = 0;
    if (!deduce_open_rule_signature_dbi(db, txn, false, &rule_signature_dbi)) return null;

    MdbCursor* cursor = null;
    rc = mdb_cursor_open(txn, rule_signature_dbi, &cursor);
    if (rc != MDB_SUCCESS || cursor == null) {
        return deduce_raise(interp, "deduce/rule-signature-restore-cursor-open-failed",
            "deduce/open: persisted rule signature cursor open failed");
    }
    defer mdb_cursor_close(cursor);

    MdbVal key = {};
    MdbVal data = {};
    usz record_index = 0;
    for (int op = MDB_FIRST; ; op = MDB_NEXT) {
        rc = mdb_cursor_get(cursor, &key, &data, op);
        if (rc == MDB_NOTFOUND) break;
        if (rc != MDB_SUCCESS || data.mv_data == null || data.mv_size < DeducePersistedRuleSignatureRecordV1.sizeof) {
            return deduce_raise(interp, "deduce/rule-signature-restore-read-failed",
                "deduce/open: persisted rule signature read failed");
        }

        char[] record = ((char*)data.mv_data)[:data.mv_size];
        DeducePersistedRuleSignatureRecordV1 header = {};
        mem::copy(&header, record.ptr, DeducePersistedRuleSignatureRecordV1.sizeof);
        if (header.version != DEDUCE_PERSISTED_RULE_SIGNATURE_VERSION) {
            return deduce_raise(interp, "deduce/rule-signature-restore-invalid-version",
                "deduce/open: persisted rule signature has unsupported version");
        }

        usz cursor_offset = DeducePersistedRuleSignatureRecordV1.sizeof;
        char[] body_predicates_bytes = {};
        char[] body_negated_bytes = {};
        char[] body_bound_masks_bytes = {};
        char[] body_selected_indexes_bytes = {};
        char[] body_filter_pushdown_bytes = {};
        char[] body_projection_masks_bytes = {};
        char[] head_term_kinds_bytes = {};
        char[] head_term_aggregate_ops_bytes = {};
        char[] head_term_is_variable_bytes = {};
        char[] head_term_var_ids_bytes = {};
        char[] head_literal_bytes = {};
        char[] body_term_offsets_bytes = {};
        char[] body_term_counts_bytes = {};
        char[] body_term_is_variable_bytes = {};
        char[] body_term_var_ids_bytes = {};
        char[] body_literal_bytes = {};
        if (!deduce_record_slice(record, &cursor_offset, SymbolId.sizeof * header.body_count, &body_predicates_bytes) ||
            !deduce_record_slice(record, &cursor_offset, bool.sizeof * header.body_count, &body_negated_bytes) ||
            !deduce_record_slice(record, &cursor_offset, ulong.sizeof * header.body_count, &body_bound_masks_bytes) ||
            !deduce_record_slice(record, &cursor_offset, int.sizeof * header.body_count, &body_selected_indexes_bytes) ||
            !deduce_record_slice(record, &cursor_offset, bool.sizeof * header.body_count, &body_filter_pushdown_bytes) ||
            !deduce_record_slice(record, &cursor_offset, ulong.sizeof * header.body_count, &body_projection_masks_bytes) ||
            !deduce_record_slice(record, &cursor_offset, DeduceRuleTermKind.sizeof * header.head_term_count, &head_term_kinds_bytes) ||
            !deduce_record_slice(record, &cursor_offset, DeduceAggregateOpKind.sizeof * header.head_term_count, &head_term_aggregate_ops_bytes) ||
            !deduce_record_slice(record, &cursor_offset, bool.sizeof * header.head_term_count, &head_term_is_variable_bytes) ||
            !deduce_record_slice(record, &cursor_offset, uint.sizeof * header.head_term_count, &head_term_var_ids_bytes) ||
            !deduce_record_slice(record, &cursor_offset, header.head_literal_blob_len, &head_literal_bytes) ||
            !deduce_record_slice(record, &cursor_offset, usz.sizeof * header.body_count, &body_term_offsets_bytes) ||
            !deduce_record_slice(record, &cursor_offset, usz.sizeof * header.body_count, &body_term_counts_bytes) ||
            !deduce_record_slice(record, &cursor_offset, bool.sizeof * header.body_term_total_count, &body_term_is_variable_bytes) ||
            !deduce_record_slice(record, &cursor_offset, uint.sizeof * header.body_term_total_count, &body_term_var_ids_bytes) ||
            !deduce_record_slice(record, &cursor_offset, header.body_literal_blob_len, &body_literal_bytes) ||
            cursor_offset != record.len) {
            return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                "deduce/open: persisted rule signature record is malformed");
        }

        if (record_index >= db.persisted_rule_catalog_count) {
            return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                "deduce/open: persisted rule signature record has no matching rule catalog entry");
        }
        DeducePersistedRuleCatalogEntry* catalog_entry = &db.persisted_rule_catalog[record_index];
        if (catalog_entry.body_count != header.body_count) {
            return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                "deduce/open: persisted rule signature body count does not match rule catalog");
        }

        SymbolId[] body_predicates = mem::new_array(SymbolId, header.body_count);
        bool[] body_negated = mem::new_array(bool, header.body_count);
        ulong[] body_bound_masks = mem::new_array(ulong, header.body_count);
        int[] body_selected_indexes = mem::new_array(int, header.body_count);
        bool[] body_filter_pushdown = mem::new_array(bool, header.body_count);
        ulong[] body_projection_masks = mem::new_array(ulong, header.body_count);
        DeduceRuleTermKind[] head_term_kinds = mem::new_array(DeduceRuleTermKind, header.head_term_count);
        DeduceAggregateOpKind[] head_term_aggregate_ops = mem::new_array(DeduceAggregateOpKind, header.head_term_count);
        bool[] head_term_is_variable = mem::new_array(bool, header.head_term_count);
        uint[] head_term_var_ids = mem::new_array(uint, header.head_term_count);
        Value*[] head_term_literals = mem::new_array(Value*, header.head_term_count);
        usz[] body_term_offsets = mem::new_array(usz, header.body_count);
        usz[] body_term_counts = mem::new_array(usz, header.body_count);
        bool[] body_term_is_variable = mem::new_array(bool, header.body_term_total_count);
        uint[] body_term_var_ids = mem::new_array(uint, header.body_term_total_count);
        Value*[] body_term_literals = mem::new_array(Value*, header.body_term_total_count);
        if ((header.body_count > 0 &&
                (body_predicates.ptr == null || body_negated.ptr == null || body_bound_masks.ptr == null ||
                 body_selected_indexes.ptr == null || body_filter_pushdown.ptr == null ||
                 body_projection_masks.ptr == null || body_term_offsets.ptr == null ||
                 body_term_counts.ptr == null)) ||
            (header.head_term_count > 0 &&
                (head_term_kinds.ptr == null || head_term_aggregate_ops.ptr == null ||
                 head_term_is_variable.ptr == null || head_term_var_ids.ptr == null ||
                 head_term_literals.ptr == null)) ||
            (header.body_term_total_count > 0 &&
                (body_term_is_variable.ptr == null || body_term_var_ids.ptr == null ||
                 body_term_literals.ptr == null))) {
            if (body_predicates.ptr != null) mem::free(body_predicates.ptr);
            if (body_negated.ptr != null) mem::free(body_negated.ptr);
            if (body_bound_masks.ptr != null) mem::free(body_bound_masks.ptr);
            if (body_selected_indexes.ptr != null) mem::free(body_selected_indexes.ptr);
            if (body_filter_pushdown.ptr != null) mem::free(body_filter_pushdown.ptr);
            if (body_projection_masks.ptr != null) mem::free(body_projection_masks.ptr);
            if (head_term_kinds.ptr != null) mem::free(head_term_kinds.ptr);
            if (head_term_aggregate_ops.ptr != null) mem::free(head_term_aggregate_ops.ptr);
            if (head_term_is_variable.ptr != null) mem::free(head_term_is_variable.ptr);
            if (head_term_var_ids.ptr != null) mem::free(head_term_var_ids.ptr);
            if (head_term_literals.ptr != null) mem::free(head_term_literals.ptr);
            if (body_term_offsets.ptr != null) mem::free(body_term_offsets.ptr);
            if (body_term_counts.ptr != null) mem::free(body_term_counts.ptr);
            if (body_term_is_variable.ptr != null) mem::free(body_term_is_variable.ptr);
            if (body_term_var_ids.ptr != null) mem::free(body_term_var_ids.ptr);
            if (body_term_literals.ptr != null) mem::free(body_term_literals.ptr);
            return deduce_raise(interp, "deduce/rule-signature-restore-out-of-memory",
                "deduce/open: persisted rule signature restore out of memory");
        }
        defer {
            if (body_predicates.ptr != null) mem::free(body_predicates.ptr);
            if (body_negated.ptr != null) mem::free(body_negated.ptr);
            if (body_bound_masks.ptr != null) mem::free(body_bound_masks.ptr);
            if (body_selected_indexes.ptr != null) mem::free(body_selected_indexes.ptr);
            if (body_filter_pushdown.ptr != null) mem::free(body_filter_pushdown.ptr);
            if (body_projection_masks.ptr != null) mem::free(body_projection_masks.ptr);
            if (head_term_kinds.ptr != null) mem::free(head_term_kinds.ptr);
            if (head_term_aggregate_ops.ptr != null) mem::free(head_term_aggregate_ops.ptr);
            if (head_term_is_variable.ptr != null) mem::free(head_term_is_variable.ptr);
            if (head_term_var_ids.ptr != null) mem::free(head_term_var_ids.ptr);
            if (head_term_literals.ptr != null) mem::free(head_term_literals.ptr);
            if (body_term_offsets.ptr != null) mem::free(body_term_offsets.ptr);
            if (body_term_counts.ptr != null) mem::free(body_term_counts.ptr);
            if (body_term_is_variable.ptr != null) mem::free(body_term_is_variable.ptr);
            if (body_term_var_ids.ptr != null) mem::free(body_term_var_ids.ptr);
            if (body_term_literals.ptr != null) mem::free(body_term_literals.ptr);
        }

        if (header.body_count > 0) {
            for (usz i = 0; i < header.body_count; i++) {
                body_predicates[i] = catalog_entry.body_predicates[i];
                bool catalog_negated = catalog_entry.body_negated != null && catalog_entry.body_negated[i];
                bool encoded_negated = ((bool*)body_negated_bytes.ptr)[i];
                if (catalog_negated != encoded_negated) {
                    return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                        "deduce/open: persisted rule signature negation flags do not match rule catalog");
                }
                body_negated[i] = catalog_negated;
            }
            mem::copy(body_bound_masks.ptr, body_bound_masks_bytes.ptr, body_bound_masks_bytes.len);
            mem::copy(body_selected_indexes.ptr, body_selected_indexes_bytes.ptr, body_selected_indexes_bytes.len);
            mem::copy(body_filter_pushdown.ptr, body_filter_pushdown_bytes.ptr, body_filter_pushdown_bytes.len);
            mem::copy(body_projection_masks.ptr, body_projection_masks_bytes.ptr, body_projection_masks_bytes.len);
            mem::copy(body_term_offsets.ptr, body_term_offsets_bytes.ptr, body_term_offsets_bytes.len);
            mem::copy(body_term_counts.ptr, body_term_counts_bytes.ptr, body_term_counts_bytes.len);
        }
        if (header.head_term_count > 0) {
            mem::copy(head_term_kinds.ptr, head_term_kinds_bytes.ptr, head_term_kinds_bytes.len);
            mem::copy(head_term_aggregate_ops.ptr, head_term_aggregate_ops_bytes.ptr, head_term_aggregate_ops_bytes.len);
            mem::copy(head_term_is_variable.ptr, head_term_is_variable_bytes.ptr, head_term_is_variable_bytes.len);
            mem::copy(head_term_var_ids.ptr, head_term_var_ids_bytes.ptr, head_term_var_ids_bytes.len);
            usz? decoded_head_literals = decode_tuple(head_literal_bytes.ptr, head_literal_bytes.len, head_term_literals, interp);
            if (catch err = decoded_head_literals) {
                return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                    "deduce/open: persisted rule signature head literal decode failed");
            }
            if (decoded_head_literals != header.head_term_count) {
                return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                    "deduce/open: persisted rule signature head literal decode failed");
            }
        }
        if (header.body_term_total_count > 0) {
            mem::copy(body_term_is_variable.ptr, body_term_is_variable_bytes.ptr, body_term_is_variable_bytes.len);
            mem::copy(body_term_var_ids.ptr, body_term_var_ids_bytes.ptr, body_term_var_ids_bytes.len);
            usz? decoded_body_literals = decode_tuple(body_literal_bytes.ptr, body_literal_bytes.len, body_term_literals, interp);
            if (catch err = decoded_body_literals) {
                return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                    "deduce/open: persisted rule signature body literal decode failed");
            }
            if (decoded_body_literals != header.body_term_total_count) {
                return deduce_raise(interp, "deduce/rule-signature-restore-invalid-record",
                    "deduce/open: persisted rule signature body literal decode failed");
            }
        }

        DeduceRuleRegisterStatus register_status =
            deduce_db_add_rule_signature(
                db,
                catalog_entry.head_predicate,
                body_predicates,
                body_negated,
                body_bound_masks,
                body_selected_indexes,
                body_filter_pushdown,
                body_projection_masks,
                head_term_kinds,
                head_term_aggregate_ops,
                head_term_is_variable,
                head_term_var_ids,
                head_term_literals,
                body_term_offsets,
                body_term_counts,
                body_term_is_variable,
                body_term_var_ids,
                body_term_literals,
                header.max_var_id
            );
        if (register_status != DEDUCE_RULE_REGISTER_OK) {
            return deduce_raise(interp, "deduce/rule-signature-restore-out-of-memory",
                "deduce/open: persisted rule signature restore out of memory");
        }
        Value* schema_err = deduce_db_ensure_restored_rule_signature_schemas(
            db,
            &db.rule_signatures[db.rule_signature_count - 1],
            interp
        );
        if (schema_err != null) return schema_err;
        record_index++;
    }

    return null;
}

fn Value* deduce_db_persist_rule_signatures(DeduceDb* db, Interp* interp) {
    if (db == null || !db.open || interp == null) {
        return deduce_raise(interp, "deduce/rule-signature-invalid-state",
            "deduce: invalid persisted rule signature state");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/rule-signature-txn-open-failed",
            "deduce: persisted rule signature txn open failed");
    }

    MdbDbi rule_signature_dbi = 0;
    if (!deduce_open_rule_signature_dbi(db, txn, true, &rule_signature_dbi)) {
        mdb_txn_abort(txn);
        return deduce_raise(interp, "deduce/rule-signature-open-failed",
            "deduce: persisted rule signature dbi open failed");
    }

    rc = mdb_drop(txn, rule_signature_dbi, 0);
    if (rc != MDB_SUCCESS) {
        mdb_txn_abort(txn);
        return deduce_raise(interp, "deduce/rule-signature-clear-failed",
            "deduce: persisted rule signature clear failed");
    }

    bool can_persist_executable_signatures = true;
    for (usz i = 0; i < db.rule_signature_count; i++) {
        if (!deduce_rule_signature_literals_supported(&db.rule_signatures[i])) {
            can_persist_executable_signatures = false;
            break;
        }
    }
    if (!can_persist_executable_signatures) {
        rc = mdb_txn_commit(txn);
        if (rc != MDB_SUCCESS) {
            return deduce_raise(interp, "deduce/rule-signature-commit-failed",
                "deduce: persisted rule signature commit failed");
        }
        return null;
    }

    for (usz i = 0; i < db.rule_signature_count; i++) {
        DeduceRuleSignature* sig = &db.rule_signatures[i];
        usz? head_literal_blob_len_opt = deduce_tuple_encoded_size(sig.head_term_literals[0:sig.head_term_count]);
        if (catch err = head_literal_blob_len_opt) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-signature-encode-failed",
                "deduce: persisted rule signature encode failed");
        }
        usz? body_literal_blob_len_opt = deduce_tuple_encoded_size(sig.body_term_literals[0:sig.body_term_total_count]);
        if (catch err = body_literal_blob_len_opt) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-signature-encode-failed",
                "deduce: persisted rule signature encode failed");
        }
        usz record_size = deduce_rule_signature_record_size(sig, head_literal_blob_len_opt, body_literal_blob_len_opt);
        if (record_size == 0) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-signature-encode-failed",
                "deduce: persisted rule signature encode failed");
        }
        char* record_buf = (char*)mem::malloc(record_size);
        if (record_buf == null) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-signature-out-of-memory",
                "deduce: persisted rule signature out of memory");
        }
        defer mem::free(record_buf);

        if (!deduce_encode_rule_signature_record(sig, interp, record_buf[:record_size])) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-signature-encode-failed",
                "deduce: persisted rule signature encode failed");
        }

        char[ulong.sizeof] key_buf = {};
        deduce_encode_rule_catalog_key(i, &key_buf);
        MdbVal key = {
            .mv_size = key_buf.len,
            .mv_data = &key_buf[0]
        };
        MdbVal data = {
            .mv_size = record_size,
            .mv_data = record_buf
        };
        rc = mdb_put(txn, rule_signature_dbi, &key, &data, 0);
        if (rc != MDB_SUCCESS) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/rule-signature-write-failed",
                "deduce: persisted rule signature write failed");
        }
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/rule-signature-commit-failed",
            "deduce: persisted rule signature commit failed");
    }
    return null;
}

fn usz deduce_db_catalog_rule_count(DeduceDb* db) @inline {
    if (db == null) return 0;
    if (db.rule_signature_count > 0) return db.rule_signature_count;
    return db.persisted_rule_catalog_count;
}

fn Value* deduce_db_ensure_restored_rule_predicate_schema(
    DeduceDb* db,
    SymbolId predicate,
    usz arity,
    Interp* interp
) {
    if (db == null || predicate == (SymbolId)0 || interp == null) return null;

    DeduceRelationSchema* existing = deduce_db_lookup_relation_schema(db, predicate);
    if (existing != null) {
        if (existing.arity != arity) {
            return deduce_raise(interp, "deduce/rule-signature-restore-schema-conflict",
                "deduce/open: restored rule signature predicate arity conflicts with registered relation schema");
        }
        return null;
    }

    bool materialized_view = false;
    SymbolId materialized_refresh_policy = (SymbolId)0;
    ulong materialized_refresh_count = 0;
    ulong materialized_last_refresh_mutation_epoch = 0;
    ulong materialized_last_stale_mutation_epoch = 0;
    DeduceMaterializedStaleReason materialized_last_stale_reason = DEDUCE_MATERIALIZED_STALE_NONE;
    (void)deduce_relation_meta_apply_persisted_state(
        db,
        predicate,
        &materialized_view,
        &materialized_refresh_policy,
        &materialized_refresh_count,
        &materialized_last_refresh_mutation_epoch,
        &materialized_last_stale_mutation_epoch,
        &materialized_last_stale_reason,
        interp
    );

    DeduceSchemaRegisterStatus status = deduce_db_register_inferred_relation_schema(
        db,
        predicate,
        arity,
        materialized_view,
        materialized_refresh_policy,
        materialized_refresh_count,
        materialized_last_refresh_mutation_epoch,
        materialized_last_stale_mutation_epoch,
        materialized_last_stale_reason,
        interp
    );
    if (status == DEDUCE_SCHEMA_REGISTER_OK) return null;
    if (status == DEDUCE_SCHEMA_REGISTER_ARITY_CONFLICT) {
        return deduce_raise(interp, "deduce/rule-signature-restore-schema-conflict",
            "deduce/open: restored rule signature predicate arity conflicts with registered relation schema");
    }
    return deduce_raise(interp, "deduce/rule-signature-restore-out-of-memory",
        "deduce/open: restored rule signature schema registration ran out of memory");
}

fn Value* deduce_db_ensure_restored_rule_signature_schemas(
    DeduceDb* db,
    DeduceRuleSignature* sig,
    Interp* interp
) {
    if (db == null || sig == null || interp == null) return null;

    Value* head_err = deduce_db_ensure_restored_rule_predicate_schema(
        db,
        sig.head_predicate,
        sig.head_term_count,
        interp
    );
    if (head_err != null) return head_err;

    if (sig.body_predicates == null || sig.body_term_counts == null) return null;
    for (usz i = 0; i < sig.body_count; i++) {
        Value* body_err = deduce_db_ensure_restored_rule_predicate_schema(
            db,
            sig.body_predicates[i],
            sig.body_term_counts[i],
            interp
        );
        if (body_err != null) return body_err;
    }
    return null;
}

fn void deduce_db_catalog_dependency_edge_counts(DeduceDb* db, long* total_out, long* negated_out) {
    if (total_out != null) *total_out = 0;
    if (negated_out != null) *negated_out = 0;
    if (db == null) return;
    if (db.rule_signature_count > 0) {
        deduce_db_dependency_edge_counts(db, total_out, negated_out);
        return;
    }

    long total = 0;
    long negated = 0;
    for (usz i = 0; i < db.persisted_rule_catalog_count; i++) {
        DeducePersistedRuleCatalogEntry* entry = &db.persisted_rule_catalog[i];
        for (usz b = 0; b < entry.body_count; b++) {
            total++;
            if (entry.body_negated != null && entry.body_negated[b]) negated++;
        }
    }
    if (total_out != null) *total_out = total;
    if (negated_out != null) *negated_out = negated;
}

fn bool deduce_relation_meta_apply_persisted_state(
    DeduceDb* db,
    SymbolId relation_name,
    bool* materialized_view_io,
    SymbolId* materialized_refresh_policy_io,
    ulong* materialized_refresh_count_io,
    ulong* materialized_last_refresh_mutation_epoch_io,
    ulong* materialized_last_stale_mutation_epoch_io,
    DeduceMaterializedStaleReason* materialized_last_stale_reason_io,
    Interp* interp
) {
    if (db == null || !db.open || relation_name == (SymbolId)0 || interp == null) return false;
    if (materialized_view_io == null || materialized_refresh_policy_io == null) return false;
    if (materialized_refresh_count_io == null ||
        materialized_last_refresh_mutation_epoch_io == null ||
        materialized_last_stale_mutation_epoch_io == null ||
        materialized_last_stale_reason_io == null) {
        return false;
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, MDB_RDONLY, &txn);
    if (rc != MDB_SUCCESS || txn == null) return false;
    defer mdb_txn_abort(txn);

    MdbDbi meta_dbi = 0;
    if (!deduce_open_relation_meta_dbi(db, txn, false, &meta_dbi)) return false;

    char[] relation_name_str = interp.symbols.get_name(relation_name);
    MdbVal key = {
        .mv_size = relation_name_str.len,
        .mv_data = relation_name_str.ptr
    };
    MdbVal data = {};
    rc = mdb_get(txn, meta_dbi, &key, &data);
    if (rc != MDB_SUCCESS || data.mv_data == null || data.mv_size < DeducePersistedRelationMetaV1.sizeof) {
        return false;
    }

    DeducePersistedRelationMetaV1 meta_v1 = {};
    mem::copy(&meta_v1, data.mv_data, DeducePersistedRelationMetaV1.sizeof);
    if (meta_v1.version == 1) {
        if ((meta_v1.flags & DEDUCE_PERSISTED_RELATION_META_FLAG_MATERIALIZED) == 0) return false;
        *materialized_view_io = true;
        if ((meta_v1.flags & DEDUCE_PERSISTED_RELATION_META_FLAG_MANUAL_REFRESH) != 0) {
            *materialized_refresh_policy_io = interp.symbols.intern("manual");
        }
        *materialized_refresh_count_io = 0;
        *materialized_last_refresh_mutation_epoch_io = 0;
        *materialized_last_stale_mutation_epoch_io = 0;
        *materialized_last_stale_reason_io = DEDUCE_MATERIALIZED_STALE_NONE;
        return true;
    }

    if (meta_v1.version != DEDUCE_PERSISTED_RELATION_META_VERSION ||
        data.mv_size < DeducePersistedRelationMetaV2.sizeof) {
        return false;
    }

    DeducePersistedRelationMetaV2 meta_v2 = {};
    mem::copy(&meta_v2, data.mv_data, DeducePersistedRelationMetaV2.sizeof);
    if ((meta_v2.flags & DEDUCE_PERSISTED_RELATION_META_FLAG_MATERIALIZED) == 0) return false;

    *materialized_view_io = true;
    if ((meta_v2.flags & DEDUCE_PERSISTED_RELATION_META_FLAG_MANUAL_REFRESH) != 0) {
        *materialized_refresh_policy_io = interp.symbols.intern("manual");
    } else if ((meta_v2.flags & DEDUCE_PERSISTED_RELATION_META_FLAG_ON_READ_REFRESH) != 0) {
        *materialized_refresh_policy_io = interp.symbols.intern("on-read");
    }
    *materialized_refresh_count_io = meta_v2.materialized_refresh_count;
    *materialized_last_refresh_mutation_epoch_io = meta_v2.materialized_last_refresh_mutation_epoch;
    *materialized_last_stale_mutation_epoch_io = meta_v2.materialized_last_stale_mutation_epoch;
    if (meta_v2.stale_reason <= (char)DeduceMaterializedStaleReason.DEDUCE_MATERIALIZED_STALE_FULL_RECOMPUTE_REQUIRED) {
        *materialized_last_stale_reason_io = (DeduceMaterializedStaleReason)meta_v2.stale_reason;
    } else {
        *materialized_last_stale_reason_io = DeduceMaterializedStaleReason.DEDUCE_MATERIALIZED_STALE_NONE;
    }
    return true;
}

fn Value* deduce_relation_meta_persist_state(
    DeduceDb* db,
    SymbolId relation_name,
    bool materialized_view,
    SymbolId materialized_refresh_policy,
    ulong materialized_refresh_count,
    ulong materialized_last_refresh_mutation_epoch,
    ulong materialized_last_stale_mutation_epoch,
    DeduceMaterializedStaleReason materialized_last_stale_reason,
    bool delete_entry,
    Interp* interp
) {
    if (db == null || !db.open || relation_name == (SymbolId)0 || interp == null) {
        return deduce_raise(interp, "deduce/materialized-meta-invalid-state",
            "deduce: invalid relation metadata persistence state");
    }
    if (g_deduce_force_materialized_meta_persist_fail) {
        return deduce_raise(interp, "deduce/materialized-meta-forced-failed",
            "deduce: forced materialized relation metadata persistence failure");
    }

    MdbTxn* txn = null;
    int rc = mdb_txn_begin(db.env, null, 0, &txn);
    if (rc != MDB_SUCCESS || txn == null) {
        return deduce_raise(interp, "deduce/materialized-meta-txn-open-failed",
            "deduce: materialized relation metadata txn open failed");
    }

    MdbDbi meta_dbi = 0;
    bool meta_open = deduce_open_relation_meta_dbi(db, txn, !delete_entry, &meta_dbi);
    if (!meta_open) {
        mdb_txn_abort(txn);
        if (delete_entry) return null;
        return deduce_raise(interp, "deduce/materialized-meta-open-failed",
            "deduce: materialized relation metadata dbi open failed");
    }

    char[] relation_name_str = interp.symbols.get_name(relation_name);
    MdbVal key = {
        .mv_size = relation_name_str.len,
        .mv_data = relation_name_str.ptr
    };
    if (delete_entry) {
        rc = mdb_del(txn, meta_dbi, &key, null);
        if (rc != MDB_SUCCESS && rc != MDB_NOTFOUND) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/materialized-meta-delete-failed",
                "deduce: materialized relation metadata delete failed");
        }
    } else {
        DeducePersistedRelationMetaV2 meta = {
            .version = DEDUCE_PERSISTED_RELATION_META_VERSION,
            .flags = 0,
            .stale_reason = (char)materialized_last_stale_reason,
            .reserved = 0,
            .materialized_refresh_count = materialized_refresh_count,
            .materialized_last_refresh_mutation_epoch = materialized_last_refresh_mutation_epoch,
            .materialized_last_stale_mutation_epoch = materialized_last_stale_mutation_epoch,
        };
        if (materialized_view) meta.flags |= DEDUCE_PERSISTED_RELATION_META_FLAG_MATERIALIZED;
        if (materialized_refresh_policy != (SymbolId)0) {
            char[] policy_name = interp.symbols.get_name(materialized_refresh_policy);
            if (deduce_name_equals(policy_name, "manual")) {
                meta.flags |= DEDUCE_PERSISTED_RELATION_META_FLAG_MANUAL_REFRESH;
            } else if (deduce_name_equals(policy_name, "on-read")) {
                meta.flags |= DEDUCE_PERSISTED_RELATION_META_FLAG_ON_READ_REFRESH;
            }
        }
        MdbVal data = {
            .mv_size = DeducePersistedRelationMetaV2.sizeof,
            .mv_data = &meta
        };
        rc = mdb_put(txn, meta_dbi, &key, &data, 0);
        if (rc != MDB_SUCCESS) {
            mdb_txn_abort(txn);
            return deduce_raise(interp, "deduce/materialized-meta-write-failed",
                "deduce: materialized relation metadata write failed");
        }
    }

    rc = mdb_txn_commit(txn);
    if (rc != MDB_SUCCESS) {
        return deduce_raise(interp, "deduce/materialized-meta-commit-failed",
            "deduce: materialized relation metadata commit failed");
    }
    return null;
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
    deduce_db_clear_persisted_rule_catalog(db);

    if (db.incremental_dirty_predicates != null) {
        mem::free(db.incremental_dirty_predicates);
        db.incremental_dirty_predicates = null;
    }
    db.incremental_dirty_count = 0;
    db.incremental_dirty_capacity = 0;
    db.incremental_mutation_epoch = 0;
    db.incremental_full_recompute_required = false;
    db.integrity_violation_count = 0;
    db.key_integrity_violation_count = 0;
    db.unique_integrity_violation_count = 0;
    db.reference_integrity_violation_count = 0;
    db.check_integrity_violation_count = 0;
    db.last_integrity_violation_code = (SymbolId)0;
    db.last_integrity_violation_relation = (SymbolId)0;
    db.last_goal_directed_read_fallback_reason = (SymbolId)0;
    if (db.last_goal_directed_read_requested_bound_positions != null) {
        mem::free(db.last_goal_directed_read_requested_bound_positions);
        db.last_goal_directed_read_requested_bound_positions = null;
    }
    if (db.last_goal_directed_read_applied_bound_positions != null) {
        mem::free(db.last_goal_directed_read_applied_bound_positions);
        db.last_goal_directed_read_applied_bound_positions = null;
    }
    if (db.last_goal_directed_read_step_counters != null) {
        mem::free(db.last_goal_directed_read_step_counters);
        db.last_goal_directed_read_step_counters = null;
    }
    db.last_goal_directed_read_requested_bound_count = 0;
    db.last_goal_directed_read_applied_bound_count = 0;
    db.last_goal_directed_read_step_count = 0;
    db.integrity_violation_history_count = 0;
    db.integrity_violation_sequence = 0;

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
            if (schema.check_constraints != null) {
                mem::free(schema.check_constraints);
                schema.check_constraints = null;
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
            schema.integrity_violation_count = 0;
            schema.key_integrity_violation_count = 0;
            schema.unique_integrity_violation_count = 0;
            schema.reference_integrity_violation_count = 0;
            schema.check_integrity_violation_count = 0;
            schema.last_integrity_violation_code = (SymbolId)0;
            schema.last_goal_directed_read_fallback_reason = (SymbolId)0;
            if (schema.last_goal_directed_read_requested_bound_positions != null) {
                mem::free(schema.last_goal_directed_read_requested_bound_positions);
                schema.last_goal_directed_read_requested_bound_positions = null;
            }
            if (schema.last_goal_directed_read_applied_bound_positions != null) {
                mem::free(schema.last_goal_directed_read_applied_bound_positions);
                schema.last_goal_directed_read_applied_bound_positions = null;
            }
            for (usz key_index = 0; key_index < schema.last_goal_directed_read_subject_keys.count; key_index++) {
                if (schema.last_goal_directed_read_subject_keys.keys[key_index] != null) {
                    mem::free(schema.last_goal_directed_read_subject_keys.keys[key_index]);
                    schema.last_goal_directed_read_subject_keys.keys[key_index] = null;
                }
                schema.last_goal_directed_read_subject_keys.key_lens[key_index] = 0;
            }
            if (schema.last_goal_directed_read_step_counters != null) {
                mem::free(schema.last_goal_directed_read_step_counters);
                schema.last_goal_directed_read_step_counters = null;
            }
            deduce_delta_set_release(&schema.maintained_pending_insertions);
            schema.last_goal_directed_read_requested_bound_count = 0;
            schema.last_goal_directed_read_applied_bound_count = 0;
            schema.last_goal_directed_read_subject_keys.count = 0;
            schema.last_goal_directed_read_subject_keys.complete = true;
            schema.last_goal_directed_read_step_count = 0;
            schema.integrity_violation_history_count = 0;
            schema.key_column = (SymbolId)0;
            schema.unique_column = (SymbolId)0;
            schema.unique_column_count = 0;
            schema.reference_constraint_count = 0;
            schema.check_constraint_count = 0;
            schema.index_column = (SymbolId)0;
            schema.arity = 0;
            schema.dropped = false;
            schema.name = (SymbolId)0;
            schema.last_goal_directed_read_fallback_reason = (SymbolId)0;
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
