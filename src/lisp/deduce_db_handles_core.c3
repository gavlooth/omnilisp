module lisp;

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

const usz DEDUCE_INTEGRITY_HISTORY_LIMIT = 8;
const char DEDUCE_PERSISTED_RELATION_META_VERSION = 2;

// Test seam: force relation metadata persistence failure paths.
bool g_deduce_force_materialized_meta_persist_fail = false;
// Test seam: force relation metadata reads to fail closed.
bool g_deduce_force_materialized_meta_read_fail = false;
// Test seam: persist an invalid stale reason byte for corruption handling coverage.
bool g_deduce_force_materialized_meta_invalid_stale_reason = false;
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
