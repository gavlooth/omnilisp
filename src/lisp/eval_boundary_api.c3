module lisp;

import main;
import std::io;

extern fn char* boundary_c_getenv(char* name) @extern("getenv");

struct BoundaryInterpState {
    main::ScopeRegion* current_scope;
    main::ScopeRegion* releasing_scope;
}

struct BoundarySession {
    main::ScopeRegion* current_scope;
    main::ScopeRegion* releasing_scope;
    bool active;
}

struct BoundaryScopeSwap {
    main::ScopeRegion* saved_scope;
    bool active;
}

struct BoundaryReleasingScopeSwap {
    main::ScopeRegion* saved_releasing_scope;
    bool active;
}

enum BoundaryTxnState : char {
    BOUNDARY_TXN_INIT,
    BOUNDARY_TXN_OPEN,
    BOUNDARY_TXN_COMMITTED,
    BOUNDARY_TXN_ABORTED,
    BOUNDARY_TXN_CLOSED,
}

const usz BOUNDARY_TXN_STATE_COUNT =
    (usz)BoundaryTxnState.BOUNDARY_TXN_CLOSED + 1;

enum BoundaryTxnAction : char {
    BOUNDARY_TXN_ACTION_COMMIT,
    BOUNDARY_TXN_ACTION_ABORT,
    BOUNDARY_TXN_ACTION_CLOSE,
}

struct BoundaryTxn {
    BoundarySession session;
    BoundaryTxnState state;
}

enum BoundaryCommitOutcome : char {
    BOUNDARY_COMMIT_NULL_RESULT,
    BOUNDARY_COMMIT_REUSED_IN_TARGET_CHAIN,
    BOUNDARY_COMMIT_DESTINATION_CONS_BUILT,
    BOUNDARY_COMMIT_DESTINATION_PARTIAL_BUILT,
    BOUNDARY_COMMIT_DESTINATION_ITERATOR_BUILT,
    BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED,
    BOUNDARY_COMMIT_MIXED_DESTINATION_PROMOTED,
    BOUNDARY_COMMIT_SPLICED_RELEASING_SCOPE,
    BOUNDARY_COMMIT_FALLBACK_COPIED,
    BOUNDARY_COMMIT_PASSTHROUGH,
}

enum BoundaryReturnProvenance : char {
    BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN,
    BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP,
    BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE,
    BOUNDARY_RETURN_MIXED_UNCERTAIN,
}

const usz BOUNDARY_RETURN_PROVENANCE_COUNT =
    (usz)BoundaryReturnProvenance.BOUNDARY_RETURN_MIXED_UNCERTAIN + 1;

struct BoundaryReturnClassification {
    BoundaryReturnProvenance provenance;
    bool in_target_scope_chain;
    bool in_releasing_scope;
}

struct BoundaryCommitEscapeResult {
    Value* value;
    BoundaryCommitOutcome outcome;
    bool releasing_scope_spliced;
    bool reused_in_target_chain;
    bool fallback_copied;
}

enum BoundaryScopeTransferReason : char {
    BOUNDARY_SCOPE_TRANSFER_OK,
    BOUNDARY_SCOPE_TRANSFER_NULL_PARENT,
    BOUNDARY_SCOPE_TRANSFER_NULL_CHILD,
    BOUNDARY_SCOPE_TRANSFER_SAME_SCOPE,
    BOUNDARY_SCOPE_TRANSFER_NOT_IMMEDIATE_CHILD,
    BOUNDARY_SCOPE_TRANSFER_CHILD_REFCOUNT_NOT_ONE,
    BOUNDARY_SCOPE_TRANSFER_OWNER_THREAD_MISMATCH,
    BOUNDARY_SCOPE_TRANSFER_PARENT_ESCAPE_LANE_INVALID,
    BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID,
    BOUNDARY_SCOPE_TRANSFER_CHILD_ESCAPE_LANE_INVALID,
}

struct BoundaryScopeTransferResult {
    bool allowed;
    BoundaryScopeTransferReason reason;
}

const usz BOUNDARY_SCOPE_TRANSFER_REASON_COUNT =
    (usz)BoundaryScopeTransferReason.BOUNDARY_SCOPE_TRANSFER_CHILD_ESCAPE_LANE_INVALID + 1;

enum BoundaryPromotionAbortReason : char {
    BOUNDARY_PROMOTION_ABORT_BUDGET_EXHAUSTED,
    BOUNDARY_PROMOTION_ABORT_PRE_ABORTED_CTX,
}

const usz BOUNDARY_PROMOTION_ABORT_REASON_COUNT =
    (usz)BoundaryPromotionAbortReason.BOUNDARY_PROMOTION_ABORT_PRE_ABORTED_CTX + 1;

enum BoundaryCopyFallbackReason : char {
    BOUNDARY_COPY_FALLBACK_BUDGET_ABORT,
    BOUNDARY_COPY_FALLBACK_SPLICE_REJECTED,
    BOUNDARY_COPY_FALLBACK_RELEASING_SCOPE_OWNED,
    BOUNDARY_COPY_FALLBACK_MIXED_UNCERTAIN_PROVENANCE,
}

const usz BOUNDARY_COPY_FALLBACK_REASON_COUNT =
    (usz)BoundaryCopyFallbackReason.BOUNDARY_COPY_FALLBACK_MIXED_UNCERTAIN_PROVENANCE + 1;

const char[][] BOUNDARY_SCOPE_TRANSFER_REASON_NAMES = {
    "ok",
    "null-parent",
    "null-child",
    "same-scope",
    "not-immediate-child",
    "child-refcount-not-one",
    "owner-thread-mismatch",
    "parent-escape-lane-invalid",
    "child-temp-lane-invalid",
    "child-escape-lane-invalid",
};

const char[][] BOUNDARY_PROMOTION_ABORT_REASON_NAMES = {
    "budget-exhausted",
    "pre-aborted-ctx",
};

const char[][] BOUNDARY_COPY_FALLBACK_REASON_NAMES = {
    "budget-abort",
    "splice-rejected",
    "releasing-scope-owned",
    "mixed-uncertain-provenance",
};

const char[][] BOUNDARY_RETURN_PROVENANCE_NAMES = {
    "reusable-in-target-chain",
    "owned-by-releasing-temp",
    "owned-by-releasing-escape",
    "mixed-uncertain",
};

const char[][] BOUNDARY_COPY_SITE_NAMES = {
    "generic",
    "promote-to-root",
    "copy-env-to-scope",
    "run-jit-result",
    "repl-line-result",
    "prim-resume",
    "cons-barrier-car",
    "cons-barrier-cdr",
    "env-promote-for-target",
    "jit-single-scope-return",
    "jit-call-scope-step2",
    "jit-tco-frame-copy",
    "jit-eval-set",
    "jit-eval-define",
    "jit-env-extend-root",
};

const char[][] BOUNDARY_TXN_STATE_NAMES = {
    "init",
    "open",
    "committed",
    "aborted",
    "closed",
};

$assert(BOUNDARY_SCOPE_TRANSFER_REASON_NAMES.len == BOUNDARY_SCOPE_TRANSFER_REASON_COUNT);
$assert(BOUNDARY_PROMOTION_ABORT_REASON_NAMES.len == BOUNDARY_PROMOTION_ABORT_REASON_COUNT);
$assert(BOUNDARY_COPY_FALLBACK_REASON_NAMES.len == BOUNDARY_COPY_FALLBACK_REASON_COUNT);
$assert(BOUNDARY_RETURN_PROVENANCE_NAMES.len == BOUNDARY_RETURN_PROVENANCE_COUNT);
$assert(BOUNDARY_COPY_SITE_NAMES.len == COPY_SITE_COUNT);
$assert(BOUNDARY_TXN_STATE_NAMES.len == BOUNDARY_TXN_STATE_COUNT);

struct BoundaryDecisionStats {
    usz splice_attempted;
    usz splice_succeeded;
    usz[BOUNDARY_SCOPE_TRANSFER_REASON_COUNT] splice_failed_by_reason;
    usz promotion_attempted;
    usz[BOUNDARY_PROMOTION_ABORT_REASON_COUNT] promotion_aborted_by_reason;
    usz copy_fallback_total;
    usz[BOUNDARY_COPY_FALLBACK_REASON_COUNT] copy_fallback_by_reason;
    usz[COPY_SITE_COUNT] copy_fallback_by_site;
}

BoundaryDecisionStats g_boundary_decision_stats;

// Instrumentation policy:
// - macros are used only for telemetry/trace scaffolding
// - ownership transitions and boundary control flow stay in functions
enum BoundaryInstrumentationMode : char {
    BOUNDARY_INSTRUMENTATION_OFF,
    BOUNDARY_INSTRUMENTATION_COUNTERS,
    BOUNDARY_INSTRUMENTATION_TRACE,
    BOUNDARY_INSTRUMENTATION_BENCHMARK,
}

// Compile-time instrumentation mode gate:
// - OFF: zero-overhead counters/trace path
// - COUNTERS: lightweight counters only (default)
// - TRACE: counters + human-readable trace events (env-gated)
// - BENCHMARK: counters + compact benchmark event stream (env-gated)
const BoundaryInstrumentationMode BOUNDARY_INSTRUMENTATION_MODE =
    BOUNDARY_INSTRUMENTATION_COUNTERS;

const bool BOUNDARY_TELEMETRY_ENABLED = BOUNDARY_INSTRUMENTATION_MODE != BOUNDARY_INSTRUMENTATION_OFF;
const bool BOUNDARY_TRACE_ENABLED = BOUNDARY_INSTRUMENTATION_MODE == BOUNDARY_INSTRUMENTATION_TRACE;
const bool BOUNDARY_BENCHMARK_ENABLED = BOUNDARY_INSTRUMENTATION_MODE == BOUNDARY_INSTRUMENTATION_BENCHMARK;

fn bool boundary_trace_runtime_enabled() @inline {
    return BOUNDARY_TRACE_ENABLED && boundary_c_getenv("OMNI_BOUNDARY_TRACE") != null;
}

fn bool boundary_benchmark_runtime_enabled() @inline {
    return BOUNDARY_BENCHMARK_ENABLED && boundary_c_getenv("OMNI_BOUNDARY_BENCHMARK_TRACE") != null;
}

macro boundary_telemetry_inc_splice_attempted() {
    if (BOUNDARY_TELEMETRY_ENABLED) g_boundary_decision_stats.splice_attempted++;
}

macro boundary_telemetry_inc_splice_succeeded() {
    if (BOUNDARY_TELEMETRY_ENABLED) g_boundary_decision_stats.splice_succeeded++;
}

macro boundary_telemetry_inc_splice_failed(reason) {
    if (BOUNDARY_TELEMETRY_ENABLED) {
        usz reason_idx = (usz)(reason);
        if (reason_idx < BOUNDARY_SCOPE_TRANSFER_REASON_COUNT) {
            g_boundary_decision_stats.splice_failed_by_reason[reason_idx]++;
        }
    }
}

macro boundary_telemetry_inc_promotion_attempted() {
    if (BOUNDARY_TELEMETRY_ENABLED) g_boundary_decision_stats.promotion_attempted++;
}

macro boundary_telemetry_inc_promotion_aborted(reason) {
    if (BOUNDARY_TELEMETRY_ENABLED) {
        usz reason_idx = (usz)(reason);
        if (reason_idx < BOUNDARY_PROMOTION_ABORT_REASON_COUNT) {
            g_boundary_decision_stats.promotion_aborted_by_reason[reason_idx]++;
        }
    }
}

macro boundary_telemetry_inc_copy_fallback_total() {
    if (BOUNDARY_TELEMETRY_ENABLED) g_boundary_decision_stats.copy_fallback_total++;
}

macro boundary_telemetry_inc_copy_fallback_reason(reason) {
    if (BOUNDARY_TELEMETRY_ENABLED) {
        usz reason_idx = (usz)(reason);
        if (reason_idx < BOUNDARY_COPY_FALLBACK_REASON_COUNT) {
            g_boundary_decision_stats.copy_fallback_by_reason[reason_idx]++;
        }
    }
}

macro boundary_trace_splice_event(allowed, reason) {
    if (boundary_trace_runtime_enabled()) {
        io::eprintfn(
            "[boundary][trace] splice allowed=%d reason=%s",
            allowed ? 1 : 0,
            boundary_scope_transfer_reason_name(reason)
        );
    }
}

macro boundary_benchmark_splice_event(allowed, reason) {
    if (boundary_benchmark_runtime_enabled()) {
        io::eprintfn(
            "[boundary][bench] splice allowed=%d reason=%d",
            allowed ? 1 : 0,
            (int)(reason)
        );
    }
}

macro boundary_trace_promotion_abort(reason) {
    if (boundary_trace_runtime_enabled()) {
        io::eprintfn(
            "[boundary][trace] promotion abort reason=%s",
            boundary_promotion_abort_reason_name(reason)
        );
    }
}

macro boundary_benchmark_promotion_abort(reason) {
    if (boundary_benchmark_runtime_enabled()) {
        io::eprintfn(
            "[boundary][bench] promotion abort reason=%d",
            (int)(reason)
        );
    }
}

macro boundary_trace_copy_fallback(reason, site) {
    if (boundary_trace_runtime_enabled()) {
        io::eprintfn(
            "[boundary][trace] copy fallback reason=%s site=%s",
            boundary_copy_fallback_reason_name(reason),
            boundary_copy_site_name(site)
        );
    }
}

macro boundary_benchmark_copy_fallback(reason, site) {
    if (boundary_benchmark_runtime_enabled()) {
        io::eprintfn(
            "[boundary][bench] copy fallback reason=%d site=%d",
            (int)(reason),
            (int)(site)
        );
    }
}

fn void boundary_reset_decision_stats() {
    g_boundary_decision_stats = {};
}

fn void boundary_restore_decision_stats(BoundaryDecisionStats snapshot) @inline {
    g_boundary_decision_stats = snapshot;
}

fn BoundaryDecisionStats boundary_decision_stats_snapshot() @inline {
    return g_boundary_decision_stats;
}

fn void boundary_note_splice_result(BoundaryScopeTransferResult result) @inline {
    boundary_telemetry_inc_splice_attempted();
    boundary_trace_splice_event(result.allowed, result.reason);
    boundary_benchmark_splice_event(result.allowed, result.reason);
    if (result.allowed) {
        boundary_telemetry_inc_splice_succeeded();
        return;
    }
    boundary_telemetry_inc_splice_failed(result.reason);
}

fn void boundary_note_promotion_attempt() @inline {
    boundary_telemetry_inc_promotion_attempted();
}

fn void boundary_note_promotion_abort(BoundaryPromotionAbortReason reason) @inline {
    boundary_trace_promotion_abort(reason);
    boundary_benchmark_promotion_abort(reason);
    boundary_telemetry_inc_promotion_aborted(reason);
}

fn void boundary_note_copy_fallback(BoundaryCopyFallbackReason reason, uint copy_site) @inline {
    boundary_telemetry_inc_copy_fallback_total();
    boundary_trace_copy_fallback(reason, copy_site);
    boundary_benchmark_copy_fallback(reason, copy_site);
    boundary_telemetry_inc_copy_fallback_reason(reason);
    if (copy_site < COPY_SITE_COUNT) {
        g_boundary_decision_stats.copy_fallback_by_site[(usz)copy_site]++;
    }
}

fn bool boundary_scope_chain_contains(main::ScopeRegion* scope, main::ScopeRegion* ancestor) @inline {
    if (scope == null || ancestor == null) return false;
    main::ScopeRegion* cur = scope;
    while (cur != null) {
        if (cur == ancestor) return true;
        cur = cur.parent;
    }
    return false;
}

fn char[] boundary_scope_transfer_reason_name(BoundaryScopeTransferReason reason) @inline {
    usz idx = (usz)reason;
    if (idx < BOUNDARY_SCOPE_TRANSFER_REASON_NAMES.len) {
        return BOUNDARY_SCOPE_TRANSFER_REASON_NAMES[idx];
    }
    return "unknown";
}

fn char[] boundary_promotion_abort_reason_name(BoundaryPromotionAbortReason reason) @inline {
    usz idx = (usz)reason;
    if (idx < BOUNDARY_PROMOTION_ABORT_REASON_NAMES.len) {
        return BOUNDARY_PROMOTION_ABORT_REASON_NAMES[idx];
    }
    return "unknown";
}

fn char[] boundary_copy_fallback_reason_name(BoundaryCopyFallbackReason reason) @inline {
    usz idx = (usz)reason;
    if (idx < BOUNDARY_COPY_FALLBACK_REASON_NAMES.len) {
        return BOUNDARY_COPY_FALLBACK_REASON_NAMES[idx];
    }
    return "unknown";
}

fn char[] boundary_return_provenance_name(BoundaryReturnProvenance provenance) @inline {
    usz idx = (usz)provenance;
    if (idx < BOUNDARY_RETURN_PROVENANCE_NAMES.len) {
        return BOUNDARY_RETURN_PROVENANCE_NAMES[idx];
    }
    return "unknown";
}

fn char[] boundary_copy_site_name(uint site) @inline {
    if (site < BOUNDARY_COPY_SITE_NAMES.len) {
        return BOUNDARY_COPY_SITE_NAMES[(usz)site];
    }
    return "unknown";
}

fn bool boundary_scope_optional_lane_shape_valid(char* bump, char* limit, main::ScopeChunk* chunks) @inline {
    if (chunks == null) return bump == null && limit == null;
    if (bump == null || limit == null) return false;
    return bump <= limit;
}

fn bool boundary_scope_required_lane_shape_valid(char* bump, char* limit, main::ScopeChunk* chunks) @inline {
    if (chunks == null) return false;
    if (bump == null || limit == null) return false;
    return bump <= limit;
}

fn BoundaryScopeTransferResult boundary_scope_transfer_ok() @inline {
    return { .allowed = true, .reason = BOUNDARY_SCOPE_TRANSFER_OK };
}

fn bool boundary_graph_audit_enabled() @inline {
    return BOUNDARY_TRACE_ENABLED && boundary_c_getenv("OMNI_BOUNDARY_GRAPH_AUDIT") != null;
}

const usz BOUNDARY_GRAPH_AUDIT_MAX_NODES = 8192;

fn bool boundary_graph_audit_value_seen(Value** seen, usz seen_len, Value* v) @inline {
    for (usz i = 0; i < seen_len; i++) {
        if (seen[i] == v) return true;
    }
    return false;
}

fn bool boundary_graph_audit_env_seen(Env** seen, usz seen_len, Env* env) @inline {
    for (usz i = 0; i < seen_len; i++) {
        if (seen[i] == env) return true;
    }
    return false;
}

fn bool boundary_graph_audit_push_value(Value** stack, usz* stack_len, Value* v) @inline {
    if (v == null) return true;
    if (*stack_len >= BOUNDARY_GRAPH_AUDIT_MAX_NODES) return false;
    stack[*stack_len] = v;
    (*stack_len)++;
    return true;
}

fn bool boundary_graph_audit_push_env(Env** stack, usz* stack_len, Env* env) @inline {
    if (env == null) return true;
    if (*stack_len >= BOUNDARY_GRAPH_AUDIT_MAX_NODES) return false;
    stack[*stack_len] = env;
    (*stack_len)++;
    return true;
}

fn bool boundary_value_points_into_temp(Value* v, main::ScopeRegion* target_scope) @inline {
    if (v == null || target_scope == null) return false;
    for (main::ScopeRegion* s = target_scope; s != null; s = s.parent) {
        if (main::is_in_scope((void*)v, s) && v.scope_gen == s.generation) {
            return true;
        }
    }
    return false;
}

fn bool boundary_env_points_into_temp(Env* env, main::ScopeRegion* target_scope) @inline {
    if (env == null || target_scope == null) return false;
    for (main::ScopeRegion* s = target_scope; s != null; s = s.parent) {
        if (main::is_in_scope((void*)env, s) && env.scope_gen == s.generation) {
            return true;
        }
    }
    return false;
}

fn bool boundary_graph_audit_escape_reachability(Value* root, main::ScopeRegion* target_scope) @noinline {
    if (root == null || target_scope == null) return true;

    Value** value_stack = (Value**)mem::malloc(Value*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    Value** seen_values = (Value**)mem::malloc(Value*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    Env** env_stack = (Env**)mem::malloc(Env*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    Env** seen_envs = (Env**)mem::malloc(Env*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    if (value_stack == null || seen_values == null || env_stack == null || seen_envs == null) {
        if (value_stack != null) mem::free(value_stack);
        if (seen_values != null) mem::free(seen_values);
        if (env_stack != null) mem::free(env_stack);
        if (seen_envs != null) mem::free(seen_envs);
        io::eprintfn("[boundary][graph-audit] escape reachability allocation failure");
        return false;
    }
    defer {
        mem::free(value_stack);
        mem::free(seen_values);
        mem::free(env_stack);
        mem::free(seen_envs);
    }

    usz value_stack_len = 0;
    usz seen_value_len = 0;
    usz env_stack_len = 0;
    usz seen_env_len = 0;

    if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, root)) {
        io::eprintfn("[boundary][graph-audit] escape reachability overflow while seeding root");
        return false;
    }

    while (value_stack_len > 0 || env_stack_len > 0) {
        while (value_stack_len > 0) {
            Value* cur = value_stack[--value_stack_len];
            if (cur == null) continue;
            if (boundary_graph_audit_value_seen(seen_values, seen_value_len, cur)) continue;
            if (seen_value_len >= BOUNDARY_GRAPH_AUDIT_MAX_NODES) {
                io::eprintfn("[boundary][graph-audit] escape reachability overflow in value visited set");
                return false;
            }
            seen_values[seen_value_len++] = cur;

            if (boundary_value_points_into_temp(cur, target_scope)) {
                io::eprintfn(
                    "[boundary][graph-audit] violation: reachable value points into TEMP tag=%d scope_gen=%d target_escape_gen=%d",
                    (int)cur.tag,
                    (long)cur.scope_gen,
                    (long)target_scope.escape_generation
                );
                return false;
            }

            // Explicit traversed edge set for reachability audit:
            // - CONS: car/cdr
            // - ARRAY: items[i]
            // - HASHMAP: entry.key / entry.value
            // - CLOSURE: captured env + env parent chain bindings
            // - PARTIAL_PRIM: first_arg / second_arg
            // - METHOD_TABLE: entry implementations + fallback
            // - MODULE: module env + env parent chain bindings
            // - ITERATOR: iterator thunk value
            // - INSTANCE: instance fields (Value slots)
            // Opaque wrappers intentionally excluded:
            // - FFI_HANDLE, COROUTINE, CONTINUATION, PRIMITIVE, TYPE_INFO
            // and any other non-Omni payload that does not own Value edges.
            switch (cur.tag) {
                case CONS:
                    if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, cur.cons_val.car) ||
                        !boundary_graph_audit_push_value(value_stack, &value_stack_len, cur.cons_val.cdr)) {
                        io::eprintfn("[boundary][graph-audit] escape reachability overflow while pushing CONS edges");
                        return false;
                    }
                    break;
                case ARRAY:
                    if (cur.array_val != null) {
                        for (usz i = 0; i < cur.array_val.length; i++) {
                            if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, cur.array_val.items[i])) {
                                io::eprintfn("[boundary][graph-audit] escape reachability overflow while pushing ARRAY edges");
                                return false;
                            }
                        }
                    }
                    break;
                case HASHMAP:
                    if (cur.hashmap_val != null && cur.hashmap_val.entries != null) {
                        for (uint i = 0; i < cur.hashmap_val.capacity; i++) {
                            Value* key = cur.hashmap_val.entries[i].key;
                            if (key == null) continue;
                            Value* val = cur.hashmap_val.entries[i].value;
                            if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, key) ||
                                !boundary_graph_audit_push_value(value_stack, &value_stack_len, val)) {
                                io::eprintfn("[boundary][graph-audit] escape reachability overflow while pushing HASHMAP edges");
                                return false;
                            }
                        }
                    }
                    break;
                case CLOSURE:
                    if (cur.closure_val != null &&
                        !boundary_graph_audit_push_env(env_stack, &env_stack_len, cur.closure_val.env)) {
                        io::eprintfn("[boundary][graph-audit] escape reachability overflow while pushing CLOSURE env");
                        return false;
                    }
                    break;
                case PARTIAL_PRIM:
                    if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, cur.partial_val.first_arg) ||
                        !boundary_graph_audit_push_value(value_stack, &value_stack_len, cur.partial_val.second_arg)) {
                        io::eprintfn("[boundary][graph-audit] escape reachability overflow while pushing PARTIAL_PRIM edges");
                        return false;
                    }
                    break;
                case METHOD_TABLE:
                    if (cur.method_table_val != null) {
                        MethodTable* mt = cur.method_table_val;
                        for (usz i = 0; i < mt.entry_count; i++) {
                            if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, mt.entries[i].implementation)) {
                                io::eprintfn("[boundary][graph-audit] escape reachability overflow while pushing METHOD_TABLE entry");
                                return false;
                            }
                        }
                        if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, mt.fallback)) {
                            io::eprintfn("[boundary][graph-audit] escape reachability overflow while pushing METHOD_TABLE fallback");
                            return false;
                        }
                    }
                    break;
                case MODULE:
                    if (cur.module_val != null &&
                        !boundary_graph_audit_push_env(env_stack, &env_stack_len, cur.module_val.env)) {
                        io::eprintfn("[boundary][graph-audit] escape reachability overflow while pushing MODULE env");
                        return false;
                    }
                    break;
                case ITERATOR:
                    if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, cur.iterator_val)) {
                        io::eprintfn("[boundary][graph-audit] escape reachability overflow while pushing ITERATOR edge");
                        return false;
                    }
                    break;
                case INSTANCE:
                    if (cur.instance_val != null && cur.instance_val.fields != null) {
                        for (usz i = 0; i < cur.instance_val.field_count; i++) {
                            if (!boundary_graph_audit_push_value(
                                    value_stack,
                                    &value_stack_len,
                                    &cur.instance_val.fields[i])) {
                                io::eprintfn("[boundary][graph-audit] escape reachability overflow while pushing INSTANCE field");
                                return false;
                            }
                        }
                    }
                    break;
                default:
                    break;
            }
        }

        while (env_stack_len > 0) {
            Env* env = env_stack[--env_stack_len];
            if (env == null) continue;
            if (boundary_graph_audit_env_seen(seen_envs, seen_env_len, env)) continue;
            if (seen_env_len >= BOUNDARY_GRAPH_AUDIT_MAX_NODES) {
                io::eprintfn("[boundary][graph-audit] escape reachability overflow in env visited set");
                return false;
            }
            seen_envs[seen_env_len++] = env;

            if (boundary_env_points_into_temp(env, target_scope)) {
                io::eprintfn(
                    "[boundary][graph-audit] violation: reachable env points into TEMP scope_gen=%d target_escape_gen=%d",
                    (long)env.scope_gen,
                    (long)target_scope.escape_generation
                );
                return false;
            }

            if (!boundary_graph_audit_push_env(env_stack, &env_stack_len, env.parent)) {
                io::eprintfn("[boundary][graph-audit] escape reachability overflow while pushing env parent");
                return false;
            }

            for (usz i = 0; i < env.binding_count; i++) {
                if (!boundary_graph_audit_push_value(value_stack, &value_stack_len, env.bindings[i].value)) {
                    io::eprintfn("[boundary][graph-audit] escape reachability overflow while pushing env binding");
                    return false;
                }
            }
        }
    }
    return true;
}

fn void boundary_debug_graph_audit_committed_escape_root(
    BoundaryCommitEscapeResult committed,
    main::ScopeRegion* target_scope
) @noinline {
    if (!boundary_graph_audit_enabled()) return;
    if (committed.value == null || target_scope == null) return;
    if (committed.value.scope_gen != target_scope.escape_generation) return;

    if (!boundary_graph_audit_escape_reachability(committed.value, target_scope)) {
        io::eprintfn(
            "[boundary][graph-audit] committed escape root reachability violation outcome=%d",
            (int)committed.outcome
        );
    }
}

fn bool boundary_verbose_telemetry_enabled() @inline {
    return
        (BOUNDARY_TRACE_ENABLED || BOUNDARY_BENCHMARK_ENABLED) &&
        boundary_c_getenv("OMNI_BOUNDARY_VERBOSE_TELEMETRY") != null;
}

fn void boundary_dump_decision_stats_verbose(char[] label = "runtime") @noinline {
    if (!boundary_verbose_telemetry_enabled()) return;
    BoundaryDecisionStats stats = boundary_decision_stats_snapshot();

    io::eprintfn(
        "[boundary][telemetry][%s] splice_attempted=%d splice_succeeded=%d promotion_attempted=%d copy_fallback_total=%d",
        (ZString)label,
        (long)stats.splice_attempted,
        (long)stats.splice_succeeded,
        (long)stats.promotion_attempted,
        (long)stats.copy_fallback_total
    );

    for (usz i = 0; i < BOUNDARY_SCOPE_TRANSFER_REASON_COUNT; i++) {
        io::eprintfn(
            "[boundary][telemetry][%s] splice_fail reason=%s count=%d",
            (ZString)label,
            boundary_scope_transfer_reason_name((BoundaryScopeTransferReason)i),
            (long)stats.splice_failed_by_reason[i]
        );
    }
    for (usz i = 0; i < BOUNDARY_PROMOTION_ABORT_REASON_COUNT; i++) {
        io::eprintfn(
            "[boundary][telemetry][%s] promotion_abort reason=%s count=%d",
            (ZString)label,
            boundary_promotion_abort_reason_name((BoundaryPromotionAbortReason)i),
            (long)stats.promotion_aborted_by_reason[i]
        );
    }
    for (usz i = 0; i < BOUNDARY_COPY_FALLBACK_REASON_COUNT; i++) {
        io::eprintfn(
            "[boundary][telemetry][%s] copy_fallback reason=%s count=%d",
            (ZString)label,
            boundary_copy_fallback_reason_name((BoundaryCopyFallbackReason)i),
            (long)stats.copy_fallback_by_reason[i]
        );
    }
    for (usz i = 0; i < COPY_SITE_COUNT; i++) {
        io::eprintfn(
            "[boundary][telemetry][%s] copy_fallback_site site=%s count=%d",
            (ZString)label,
            boundary_copy_site_name((uint)i),
            (long)stats.copy_fallback_by_site[i]
        );
    }
}

fn void boundary_debug_graph_audit_report_scope_transfer(
    main::ScopeRegion* parent,
    main::ScopeRegion* child,
    BoundaryScopeTransferReason reason
) @noinline {
    if (!boundary_graph_audit_enabled()) return;
    bool parent_non_null = parent != null;
    bool child_non_null = child != null;
    bool same_scope = parent_non_null && child_non_null && parent == child;
    bool immediate_child = parent_non_null && child_non_null && child.parent == parent;
    bool refcount_one = child_non_null && child.refcount == 1;
    bool owner_thread_match = parent_non_null &&
        child_non_null &&
        parent.owner_thread_token != null &&
        child.owner_thread_token != null &&
        parent.owner_thread_token == child.owner_thread_token;
    bool parent_escape_lane_valid = parent_non_null &&
        boundary_scope_optional_lane_shape_valid(parent.escape_bump, parent.escape_limit, parent.escape_chunks);
    bool child_temp_lane_valid = child_non_null &&
        boundary_scope_required_lane_shape_valid(child.bump, child.limit, child.chunks);
    bool child_escape_lane_valid = child_non_null &&
        boundary_scope_optional_lane_shape_valid(child.escape_bump, child.escape_limit, child.escape_chunks);

    io::eprintfn(
        "[boundary][graph-audit] splice-reject reason=%s parent_non_null=%d child_non_null=%d same_scope=%d immediate_child=%d refcount_one=%d owner_thread_match=%d parent_escape_lane_valid=%d child_temp_lane_valid=%d child_escape_lane_valid=%d",
        boundary_scope_transfer_reason_name(reason),
        parent_non_null ? 1 : 0,
        child_non_null ? 1 : 0,
        same_scope ? 1 : 0,
        immediate_child ? 1 : 0,
        refcount_one ? 1 : 0,
        owner_thread_match ? 1 : 0,
        parent_escape_lane_valid ? 1 : 0,
        child_temp_lane_valid ? 1 : 0,
        child_escape_lane_valid ? 1 : 0
    );
}

fn BoundaryScopeTransferResult boundary_scope_transfer_reject(
    main::ScopeRegion* parent,
    main::ScopeRegion* child,
    BoundaryScopeTransferReason reason
) @noinline {
    boundary_debug_graph_audit_report_scope_transfer(parent, child, reason);
    return { .allowed = false, .reason = reason };
}

fn BoundaryScopeTransferResult boundary_check_scope_transfer(main::ScopeRegion* parent, main::ScopeRegion* child) @inline {
    if (parent == null) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_NULL_PARENT);
    }
    if (child == null) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_NULL_CHILD);
    }
    if (parent == child) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_SAME_SCOPE);
    }
    if (child.parent != parent) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_NOT_IMMEDIATE_CHILD);
    }
    if (child.refcount != 1) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_CHILD_REFCOUNT_NOT_ONE);
    }
    if (parent.owner_thread_token == null ||
        child.owner_thread_token == null ||
        parent.owner_thread_token != child.owner_thread_token) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_OWNER_THREAD_MISMATCH);
    }
    if (!boundary_scope_optional_lane_shape_valid(parent.escape_bump, parent.escape_limit, parent.escape_chunks)) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_PARENT_ESCAPE_LANE_INVALID);
    }
    if (!boundary_scope_required_lane_shape_valid(child.bump, child.limit, child.chunks)) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID);
    }
    if (!boundary_scope_optional_lane_shape_valid(child.escape_bump, child.escape_limit, child.escape_chunks)) {
        return boundary_scope_transfer_reject(parent, child, BOUNDARY_SCOPE_TRANSFER_CHILD_ESCAPE_LANE_INVALID);
    }
    return boundary_scope_transfer_ok();
}

fn void boundary_assert_interp_scope_chain(Interp* interp) @inline {
    if (interp == null) return;
    assert(interp.root_scope != null, "boundary invariant: root_scope must not be null");
    assert(interp.current_scope != null, "boundary invariant: current_scope must not be null");
}

fn void boundary_assert_saved_state(BoundaryInterpState saved) @inline {
    assert(saved.current_scope != null, "boundary invariant: saved current_scope must not be null");
}

fn void boundary_assert_session_state(BoundarySession* session) @inline {
    assert(session != null, "boundary session: null session");
    assert(session.current_scope != null, "boundary session: saved current_scope must not be null");
}

fn void boundary_assert_scope_swap_state(BoundaryScopeSwap* swap) @inline {
    assert(swap != null, "boundary scope swap: null swap");
    assert(swap.saved_scope != null, "boundary scope swap: saved scope must not be null");
}

fn void boundary_assert_releasing_scope_swap_state(BoundaryReleasingScopeSwap* swap) @inline {
    assert(swap != null, "boundary releasing-scope swap: null swap");
}

<*
@require interp != null : "boundary_scope_swap_begin: null interp"
@require target_scope != null : "boundary_scope_swap_begin: null target_scope"
@ensure return.active : "boundary_scope_swap_begin: returned inactive swap"
@ensure return.saved_scope != null : "boundary_scope_swap_begin: missing saved scope"
*>
fn BoundaryScopeSwap boundary_scope_swap_begin(Interp* interp, main::ScopeRegion* target_scope) @inline {
    boundary_assert_interp_scope_chain(interp);
    assert(target_scope != null, "boundary scope swap: null target_scope");
    BoundaryScopeSwap swap = {
        .saved_scope = interp.current_scope,
        .active = true,
    };
    interp.current_scope = target_scope;
    boundary_assert_interp_scope_chain(interp);
    return swap;
}

<*
@require interp != null : "boundary_scope_swap_end: null interp"
@require swap != null : "boundary_scope_swap_end: null swap"
@require swap.active : "boundary_scope_swap_end: swap must be active"
@require swap.saved_scope != null : "boundary_scope_swap_end: missing saved scope"
@ensure !swap.active : "boundary_scope_swap_end: swap must be inactive after restore"
@ensure interp.current_scope == swap.saved_scope : "boundary_scope_swap_end: current_scope restore mismatch"
*>
fn void boundary_scope_swap_end(Interp* interp, BoundaryScopeSwap* swap) @inline {
    boundary_assert_scope_swap_state(swap);
    assert(swap.active, "boundary scope swap: end on inactive swap");
    interp.current_scope = swap.saved_scope;
    swap.active = false;
    boundary_assert_interp_scope_chain(interp);
}

<*
@require interp != null : "boundary_releasing_scope_swap_begin: null interp"
@require releasing_scope != null : "boundary_releasing_scope_swap_begin: null releasing_scope"
@ensure return.active : "boundary_releasing_scope_swap_begin: returned inactive swap"
*>
fn BoundaryReleasingScopeSwap boundary_releasing_scope_swap_begin(
    Interp* interp,
    main::ScopeRegion* releasing_scope
) @inline {
    boundary_assert_interp_scope_chain(interp);
    BoundaryReleasingScopeSwap swap = {
        .saved_releasing_scope = interp.releasing_scope,
        .active = true,
    };
    interp.releasing_scope = releasing_scope;
    return swap;
}

<*
@require interp != null : "boundary_releasing_scope_swap_end: null interp"
@require swap != null : "boundary_releasing_scope_swap_end: null swap"
@require swap.active : "boundary_releasing_scope_swap_end: swap must be active"
@ensure !swap.active : "boundary_releasing_scope_swap_end: swap must be inactive after restore"
@ensure interp.releasing_scope == swap.saved_releasing_scope : "boundary_releasing_scope_swap_end: releasing_scope restore mismatch"
*>
fn void boundary_releasing_scope_swap_end(
    Interp* interp,
    BoundaryReleasingScopeSwap* swap
) @inline {
    boundary_assert_releasing_scope_swap_state(swap);
    assert(swap.active, "boundary releasing-scope swap: end on inactive swap");
    interp.releasing_scope = swap.saved_releasing_scope;
    swap.active = false;
    boundary_assert_interp_scope_chain(interp);
}

fn void boundary_assert_txn_state(BoundaryTxn* txn) @inline {
    assert(txn != null, "boundary txn: null txn");
}

fn bool boundary_txn_transition_allowed(BoundaryTxnState state, BoundaryTxnAction action) @inline {
    switch (action) {
        case BOUNDARY_TXN_ACTION_COMMIT:
            return state == BOUNDARY_TXN_OPEN;
        case BOUNDARY_TXN_ACTION_ABORT:
            return state == BOUNDARY_TXN_OPEN;
        case BOUNDARY_TXN_ACTION_CLOSE:
            return state == BOUNDARY_TXN_OPEN ||
                   state == BOUNDARY_TXN_COMMITTED ||
                   state == BOUNDARY_TXN_ABORTED;
    }
}

<*
@require interp != null : "boundary_txn_begin: null interp"
@ensure return.state == BOUNDARY_TXN_OPEN : "boundary_txn_begin: txn not opened"
@ensure return.session.active : "boundary_txn_begin: session should be active"
*>
fn BoundaryTxn boundary_txn_begin(Interp* interp) @inline {
    BoundaryTxn txn = {};
    txn.session = boundary_session_begin(interp);
    txn.state = BOUNDARY_TXN_OPEN;
    return txn;
}

fn void boundary_txn_assert_open(BoundaryTxn* txn) @inline {
    boundary_assert_txn_state(txn);
    assert(txn.state == BOUNDARY_TXN_OPEN, "boundary txn: use-after-close or invalid transition");
}

<*
@require txn != null : "boundary_txn_mark_committed: null txn"
@require txn.state == BOUNDARY_TXN_OPEN : "boundary_txn_mark_committed: txn must be open"
@ensure txn.state == BOUNDARY_TXN_COMMITTED : "boundary_txn_mark_committed: txn not committed"
*>
fn void boundary_txn_mark_committed(BoundaryTxn* txn) @inline {
    boundary_txn_assert_open(txn);
    assert(
        boundary_txn_transition_allowed(txn.state, BOUNDARY_TXN_ACTION_COMMIT),
        "boundary txn: invalid commit transition"
    );
    txn.state = BOUNDARY_TXN_COMMITTED;
}

<*
@require txn != null : "boundary_txn_mark_aborted: null txn"
@require txn.state == BOUNDARY_TXN_OPEN : "boundary_txn_mark_aborted: txn must be open"
@ensure txn.state == BOUNDARY_TXN_ABORTED : "boundary_txn_mark_aborted: txn not aborted"
*>
fn void boundary_txn_mark_aborted(BoundaryTxn* txn) @inline {
    boundary_txn_assert_open(txn);
    assert(
        boundary_txn_transition_allowed(txn.state, BOUNDARY_TXN_ACTION_ABORT),
        "boundary txn: invalid abort transition"
    );
    txn.state = BOUNDARY_TXN_ABORTED;
}

<*
@require interp != null : "boundary_txn_close: null interp"
@require txn != null : "boundary_txn_close: null txn"
@require txn.state != BOUNDARY_TXN_INIT : "boundary_txn_close: close from init"
@require txn.state != BOUNDARY_TXN_CLOSED : "boundary_txn_close: double-close"
@ensure txn.state == BOUNDARY_TXN_CLOSED : "boundary_txn_close: txn should be closed"
@ensure !txn.session.active : "boundary_txn_close: session should be inactive"
*>
fn void boundary_txn_close(Interp* interp, BoundaryTxn* txn) @inline {
    boundary_assert_txn_state(txn);
    assert(
        boundary_txn_transition_allowed(txn.state, BOUNDARY_TXN_ACTION_CLOSE),
        "boundary txn: invalid close transition"
    );
    boundary_session_end(interp, &txn.session);
    txn.state = BOUNDARY_TXN_CLOSED;
}

<*
@require interp != null : "boundary_save_interp_state: null interp"
*>
fn BoundaryInterpState boundary_save_interp_state(Interp* interp) @inline {
    boundary_assert_interp_scope_chain(interp);
    return {
        .current_scope = interp.current_scope,
        .releasing_scope = interp.releasing_scope,
    };
}

<*
@require interp != null : "boundary_restore_interp_state: null interp"
@ensure interp.current_scope == saved.current_scope : "boundary_restore_interp_state: current_scope restore mismatch"
@ensure interp.releasing_scope == saved.releasing_scope : "boundary_restore_interp_state: releasing_scope restore mismatch"
*>
fn void boundary_restore_interp_state(Interp* interp, BoundaryInterpState saved) @inline {
    boundary_assert_saved_state(saved);
    interp.current_scope = saved.current_scope;
    interp.releasing_scope = saved.releasing_scope;
    boundary_assert_interp_scope_chain(interp);
}

<*
@require interp != null : "boundary_session_begin: null interp"
*>
fn BoundarySession boundary_session_begin(Interp* interp) @inline {
    boundary_assert_interp_scope_chain(interp);
    return {
        .current_scope = interp.current_scope,
        .releasing_scope = interp.releasing_scope,
        .active = true,
    };
}

<*
@require interp != null : "boundary_session_end: null interp"
@require session != null : "boundary_session_end: null session"
@require session.active : "boundary_session_end: session must be active"
@ensure !session.active : "boundary_session_end: session should be inactive after restore"
@ensure interp.current_scope == session.current_scope : "boundary_session_end: current_scope restore mismatch"
@ensure interp.releasing_scope == session.releasing_scope : "boundary_session_end: releasing_scope restore mismatch"
*>
fn void boundary_session_end(Interp* interp, BoundarySession* session) @inline {
    boundary_assert_session_state(session);
    assert(session.active, "boundary session: end on inactive session");
    interp.current_scope = session.current_scope;
    interp.releasing_scope = session.releasing_scope;
    session.active = false;
    assert(interp.current_scope == session.current_scope, "boundary session: current_scope restore mismatch");
    assert(interp.releasing_scope == session.releasing_scope, "boundary session: releasing_scope restore mismatch");
    boundary_assert_interp_scope_chain(interp);
}

<*
@require interp != null : "boundary_ptr_in_target_scope_chain: null interp"
*>
fn bool boundary_ptr_in_target_scope_chain(void* ptr, Interp* interp) @inline {
    if (ptr == null) return false;
    return in_target_scope_chain(ptr, interp);
}

fn bool boundary_ptr_in_scope(void* ptr, main::ScopeRegion* scope) @inline {
    if (ptr == null || scope == null) return false;
    return main::is_in_scope(ptr, scope);
}

fn bool boundary_ptr_in_scope_chain(void* ptr, main::ScopeRegion* scope) @inline {
    if (ptr == null || scope == null) return false;
    main::ScopeRegion* cur = scope;
    while (cur != null) {
        if (boundary_ptr_in_scope(ptr, cur)) return true;
        cur = cur.parent;
    }
    return false;
}

<*
@require interp != null : "boundary_value_in_releasing_scope: null interp"
*>
fn bool boundary_value_in_releasing_scope(Value* v, Interp* interp) @inline {
    if (v == null || interp.releasing_scope == null) return false;
    return boundary_ptr_in_scope((void*)v, interp.releasing_scope);
}

<*
@require interp != null : "boundary_can_reuse_value: null interp"
*>
fn bool boundary_can_reuse_value(Value* v, Interp* interp) @inline {
    if (v == null) return false;
    boundary_assert_interp_scope_chain(interp);
    if (boundary_value_in_releasing_scope(v, interp)) return false;
    return boundary_ptr_in_target_scope_chain((void*)v, interp);
}

fn BoundaryReturnClassification boundary_classify_return_value(
    Value* v,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    BoundaryReturnClassification classification = {
        .provenance = BOUNDARY_RETURN_MIXED_UNCERTAIN,
        .in_target_scope_chain = false,
        .in_releasing_scope = false,
    };

    if (v == null || interp == null || target_scope == null || releasing_scope == null) {
        return classification;
    }

    classification.in_releasing_scope = boundary_ptr_in_scope((void*)v, releasing_scope);
    classification.in_target_scope_chain = boundary_ptr_in_scope_chain((void*)v, target_scope);

    if (!classification.in_releasing_scope && classification.in_target_scope_chain) {
        classification.provenance = BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN;
        return classification;
    }

    if (classification.in_releasing_scope) {
        if (v.scope_gen == releasing_scope.generation) {
            classification.provenance = BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP;
            return classification;
        }
        if (v.scope_gen == releasing_scope.escape_generation) {
            classification.provenance = BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE;
            return classification;
        }
    }

    classification.provenance = BOUNDARY_RETURN_MIXED_UNCERTAIN;
    return classification;
}

// =============================================================================
// SECTION 2.4: FRAME PUSH/POP (audited boundary facade)
// =============================================================================

<*
@require interp != null : "boundary_copy_to_parent: null interp"
*>
fn Value* boundary_copy_to_parent(Value* v, Interp* interp) @inline {
    return copy_to_parent(v, interp);
}

<*
@require interp != null : "boundary_enter_scope: null interp"
@require target_scope != null : "boundary_enter_scope: null target_scope"
*>
fn main::ScopeRegion* boundary_enter_scope(Interp* interp, main::ScopeRegion* target_scope) @inline {
    BoundaryScopeSwap swap = boundary_scope_swap_begin(interp, target_scope);
    return swap.saved_scope;
}

<*
@require interp != null : "boundary_leave_scope: null interp"
*>
fn void boundary_leave_scope(Interp* interp, main::ScopeRegion* saved_scope) @inline {
    interp.current_scope = saved_scope;
    boundary_assert_interp_scope_chain(interp);
}

<*
@require interp != null : "boundary_push_child_scope: null interp"
*>
fn main::ScopeRegion* boundary_push_child_scope(Interp* interp, main::ScopeRegion** saved_scope_out = null) @inline {
    boundary_assert_interp_scope_chain(interp);
    main::ScopeRegion* saved_scope = interp.current_scope;
    if (saved_scope_out != null) {
        *saved_scope_out = saved_scope;
    }
    main::ScopeRegion* child_scope = main::scope_create(saved_scope);
    if (child_scope == null) {
        return null;
    }
    interp.current_scope = child_scope;
    return child_scope;
}

<*
@require interp != null : "boundary_pop_child_scope: null interp"
*>
fn void boundary_pop_child_scope(
    Interp* interp,
    main::ScopeRegion* saved_scope,
    main::ScopeRegion* child_scope
) @inline {
    interp.current_scope = saved_scope;
    boundary_assert_interp_scope_chain(interp);
    if (child_scope != null) {
        main::scope_release(child_scope);
    }
}

<*
@require interp != null : "boundary_copy_to_parent_site: null interp"
*>
fn Value* boundary_copy_to_parent_site(Value* v, Interp* interp, uint site) @inline {
    return copy_to_parent_site(v, interp, site);
}

<*
@require interp != null : "boundary_copy_to_parent_site_ctx: null interp"
*>
fn Value* boundary_copy_to_parent_site_ctx(Value* v, Interp* interp, uint site, PromotionContext* ctx) @inline {
    return copy_to_parent_site_ctx(v, interp, site, ctx);
}

<*
@require interp != null : "boundary_copy_to_scope_site: null interp"
*>
fn Value* boundary_copy_to_scope_site(
    Value* v,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    uint site
) @inline {
    if (v == null || target_scope == null) return v;
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    return boundary_copy_to_parent_site(v, interp, site);
}

<*
@require interp != null : "boundary_alloc_value_in_scope: null interp"
@require target_scope != null : "boundary_alloc_value_in_scope: null target_scope"
*>
fn Value* boundary_alloc_value_in_scope(Interp* interp, main::ScopeRegion* target_scope, bool register_dtor = false) @inline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    Value* v = interp.alloc_value();
    if (register_dtor) {
        main::scope_register_dtor(target_scope, (void*)v, &scope_dtor_value);
    }
    return v;
}

<*
@require interp != null : "boundary_alloc_value_in_root: null interp"
*>
fn Value* boundary_alloc_value_in_root(Interp* interp, bool register_dtor = false) @inline {
    return boundary_alloc_value_in_scope(interp, interp.root_scope, register_dtor);
}

<*
@require interp != null : "boundary_make_env_in_scope: null interp"
@require target_scope != null : "boundary_make_env_in_scope: null target_scope"
*>
fn Env* boundary_make_env_in_scope(Interp* interp, main::ScopeRegion* target_scope, Env* parent) @inline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    return make_env(interp, parent);
}

<*
@require interp != null : "boundary_make_env_in_root: null interp"
*>
fn Env* boundary_make_env_in_root(Interp* interp, Env* parent) @inline {
    return boundary_make_env_in_scope(interp, interp.root_scope, parent);
}

<*
@require interp != null : "boundary_env_extend_in_scope: null interp"
@require target_scope != null : "boundary_env_extend_in_scope: null target_scope"
@require env != null : "boundary_env_extend_in_scope: null env"
*>
fn Env* boundary_env_extend_in_scope(
    Interp* interp,
    main::ScopeRegion* target_scope,
    Env* env,
    SymbolId name,
    Value* value
) @inline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    return env.extend(interp, name, value);
}

<*
@require interp != null : "boundary_env_extend_in_root: null interp"
@require env != null : "boundary_env_extend_in_root: null env"
*>
fn Env* boundary_env_extend_in_root(Interp* interp, Env* env, SymbolId name, Value* value) @inline {
    return boundary_env_extend_in_scope(interp, interp.root_scope, env, name, value);
}

<*
@require interp != null : "boundary_promote_to_escape: null interp"
*>
fn Value* boundary_promote_to_escape(Value* v, Interp* interp) @inline {
    return promote_to_escape(v, interp);
}

<*
@require interp != null : "boundary_promote_to_root_site: null interp"
*>
fn Value* boundary_promote_to_root_site(Value* v, Interp* interp, uint site) @inline {
    return promote_to_root_site(v, interp, site);
}

<*
@require interp != null : "boundary_promote_to_root: null interp"
*>
fn Value* boundary_promote_to_root(Value* v, Interp* interp) @inline {
    return promote_to_root(v, interp);
}

<*
@require interp != null : "boundary_copy_value_if_owned_by_scope: null interp"
*>
fn Value* boundary_copy_value_if_owned_by_scope(Value* v, Interp* interp, main::ScopeRegion* source_scope, uint site) @inline {
    return copy_value_if_owned_by_scope(v, interp, source_scope, site);
}

<*
@require interp != null : "boundary_copy_from_releasing_scope: null interp"
*>
fn Value* boundary_copy_from_releasing_scope(Value* v, Interp* interp, main::ScopeRegion* releasing_scope, uint site) @inline {
    if (releasing_scope == null) return v;
    BoundaryReleasingScopeSwap swap = boundary_releasing_scope_swap_begin(interp, releasing_scope);
    defer boundary_releasing_scope_swap_end(interp, &swap);
    return boundary_copy_value_if_owned_by_scope(v, interp, releasing_scope, site);
}

<*
@require interp != null : "boundary_copy_env_to_scope: null interp"
*>
fn Env* boundary_copy_env_to_scope(Env* env, Interp* interp, usz depth = 0, PromotionContext* ctx = null) @inline {
    return copy_env_to_scope(env, interp, depth, ctx);
}

<*
@require interp != null : "boundary_copy_env_to_target_scope: null interp"
@require target_scope != null : "boundary_copy_env_to_target_scope: null target_scope"
*>
fn Env* boundary_copy_env_to_target_scope(
    Env* env,
    Interp* interp,
    main::ScopeRegion* target_scope,
    usz depth = 0,
    PromotionContext* ctx = null
) @inline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    return boundary_copy_env_to_scope(env, interp, depth, ctx);
}

fn BoundaryScopeTransferResult boundary_try_scope_splice_escapes_result(main::ScopeRegion* parent, main::ScopeRegion* child) @inline {
    BoundaryScopeTransferResult result = boundary_check_scope_transfer(parent, child);
    if (result.allowed) {
        main::scope_splice_escapes(parent, child);
    }
    boundary_note_splice_result(result);
    return result;
}

<*
@require parent != null : "boundary_try_scope_splice_escapes: null parent"
@require child != null : "boundary_try_scope_splice_escapes: null child"
@require child.parent == parent : "boundary_try_scope_splice_escapes: child must be immediate child of parent"
*>
fn bool boundary_try_scope_splice_escapes(main::ScopeRegion* parent, main::ScopeRegion* child) @inline {
    return boundary_try_scope_splice_escapes_result(parent, child).allowed;
}

fn bool boundary_is_scope_transfer_legal(main::ScopeRegion* parent, main::ScopeRegion* child) @inline {
    return boundary_check_scope_transfer(parent, child).allowed;
}

<*
@require interp != null : "boundary_should_copy_env: null interp"
*>
fn bool boundary_should_copy_env(Value* v, Interp* interp) @inline {
    if (v == null) return false;
    return !boundary_can_reuse_value(v, interp);
}

fn bool boundary_should_promote(Value* v, PromotionContext* ctx, main::ScopeRegion* source_scope) @inline {
    if (v == null || ctx == null || source_scope == null) return false;
    if (ctx.aborted) return false;
    return v.scope_gen == source_scope.escape_generation;
}

fn bool boundary_cons_escape_child_edge_safe(Value* edge, main::ScopeRegion* target_scope) @inline {
    if (edge == null || target_scope == null) return true;
    if (!boundary_ptr_in_scope((void*)edge, target_scope)) return true;
    return edge.scope_gen != target_scope.generation;
}

<*
@require list != null : "boundary_build_destination_cons_escape: null list"
@require interp != null : "boundary_build_destination_cons_escape: null interp"
@require target_scope != null : "boundary_build_destination_cons_escape: null target_scope"
@require releasing_scope != null : "boundary_build_destination_cons_escape: null releasing_scope"
@ensure return == list || return.scope_gen == target_scope.escape_generation : "boundary_build_destination_cons_escape: expected ESCAPE-lane cons head"
*>
fn Value* boundary_build_destination_cons_escape(
    Value* list,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx = null
) {
    if (list.tag != CONS) return list;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;

    Value* head = interp.alloc_value_escape();
    head.tag = CONS;
    Value* current_new = head;
    Value* current_old = list;

    while (true) {
        Value* routed_car = boundary_copy_to_parent_site_ctx(
            current_old.cons_val.car,
            interp,
            COPY_SITE_CONS_BARRIER_CAR,
            ctx
        );
        current_new.cons_val.car = boundary_promote_to_escape(routed_car, interp);
        if (ctx != null && ctx.aborted) return list;
        assert(
            boundary_cons_escape_child_edge_safe(current_new.cons_val.car, target_scope),
            "boundary destination-cons: ESCAPE car must not point into target TEMP lane"
        );

        Value* old_cdr = current_old.cons_val.cdr;
        bool extend_spine = old_cdr != null &&
            old_cdr.tag == CONS &&
            old_cdr.scope_gen == releasing_scope.generation &&
            boundary_ptr_in_scope((void*)old_cdr, releasing_scope);
        if (extend_spine) {
            Value* next_new = interp.alloc_value_escape();
            next_new.tag = CONS;
            current_new.cons_val.cdr = next_new;
            current_new = next_new;
            current_old = old_cdr;
            continue;
        }

        Value* routed_cdr = boundary_copy_to_parent_site_ctx(
            old_cdr,
            interp,
            COPY_SITE_CONS_BARRIER_CDR,
            ctx
        );
        current_new.cons_val.cdr = boundary_promote_to_escape(routed_cdr, interp);
        if (ctx != null && ctx.aborted) return list;
        assert(
            boundary_cons_escape_child_edge_safe(current_new.cons_val.cdr, target_scope),
            "boundary destination-cons: ESCAPE cdr must not point into target TEMP lane"
        );
        break;
    }

    return head;
}

<*
@require partial != null : "boundary_build_destination_partial_escape: null partial"
@require interp != null : "boundary_build_destination_partial_escape: null interp"
@require target_scope != null : "boundary_build_destination_partial_escape: null target_scope"
@require releasing_scope != null : "boundary_build_destination_partial_escape: null releasing_scope"
@ensure return == partial || return.scope_gen == target_scope.escape_generation : "boundary_build_destination_partial_escape: expected ESCAPE-lane partial wrapper"
*>
fn Value* boundary_build_destination_partial_escape(
    Value* partial,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx = null
) {
    if (partial.tag != PARTIAL_PRIM) return partial;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;

    Value* routed_first = boundary_copy_to_parent_site_ctx(
        partial.partial_val.first_arg,
        interp,
        COPY_SITE_GENERIC,
        ctx
    );
    Value* escaped_first = boundary_promote_to_escape(routed_first, interp);
    if (ctx != null && ctx.aborted) return partial;
    assert(
        boundary_cons_escape_child_edge_safe(escaped_first, target_scope),
        "boundary destination-partial: first arg must not point into target TEMP lane"
    );

    Value* routed_second = boundary_copy_to_parent_site_ctx(
        partial.partial_val.second_arg,
        interp,
        COPY_SITE_GENERIC,
        ctx
    );
    Value* escaped_second = boundary_promote_to_escape(routed_second, interp);
    if (ctx != null && ctx.aborted) return partial;
    assert(
        boundary_cons_escape_child_edge_safe(escaped_second, target_scope),
        "boundary destination-partial: second arg must not point into target TEMP lane"
    );

    Value* result = interp.alloc_value_escape();
    result.tag = PARTIAL_PRIM;
    result.partial_val.func = partial.partial_val.func;
    result.partial_val.remaining = partial.partial_val.remaining;
    result.partial_val.first_arg = escaped_first;
    result.partial_val.second_arg = escaped_second;
    return result;
}

<*
@require iterator != null : "boundary_build_destination_iterator_escape: null iterator"
@require interp != null : "boundary_build_destination_iterator_escape: null interp"
@require target_scope != null : "boundary_build_destination_iterator_escape: null target_scope"
@require releasing_scope != null : "boundary_build_destination_iterator_escape: null releasing_scope"
@ensure return == iterator || return.scope_gen == target_scope.escape_generation : "boundary_build_destination_iterator_escape: expected ESCAPE-lane iterator wrapper"
*>
fn Value* boundary_build_destination_iterator_escape(
    Value* iterator,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    PromotionContext* ctx = null
) {
    if (iterator.tag != ITERATOR) return iterator;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;

    Value* routed_inner = boundary_copy_to_parent_site_ctx(
        iterator.iterator_val,
        interp,
        COPY_SITE_GENERIC,
        ctx
    );
    Value* escaped_inner = boundary_promote_to_escape(routed_inner, interp);
    if (ctx != null && ctx.aborted) return iterator;
    assert(
        boundary_cons_escape_child_edge_safe(escaped_inner, target_scope),
        "boundary destination-iterator: iterator payload must not point into target TEMP lane"
    );

    Value* result = interp.alloc_value_escape();
    result.tag = ITERATOR;
    result.iterator_val = escaped_inner;
    return result;
}

<*
@require error_value != null : "boundary_build_destination_error_escape: null error value"
@require interp != null : "boundary_build_destination_error_escape: null interp"
@require target_scope != null : "boundary_build_destination_error_escape: null target_scope"
@ensure return == error_value || return.scope_gen == target_scope.escape_generation : "boundary_build_destination_error_escape: expected ESCAPE-lane error wrapper"
*>
fn Value* boundary_build_destination_error_escape(
    Value* error_value,
    Interp* interp,
    main::ScopeRegion* target_scope
) {
    if (error_value.tag != ERROR) return error_value;
    Value* result = interp.alloc_value_escape();
    result.tag = ERROR;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);

    usz len = error_value.str_len;
    usz cap = len + 1;
    if (cap < 32) cap = 32;
    result.str_chars = (char*)mem::malloc(cap);
    for (usz i = 0; i < len; i++) {
        result.str_chars[i] = error_value.str_chars[i];
    }
    result.str_chars[len] = 0;
    result.str_len = len;
    return result;
}

<*
@require interp != null : "boundary_commit_escape: null interp"
@require target_scope != null : "boundary_commit_escape: null target_scope"
@require releasing_scope != null : "boundary_commit_escape: null releasing_scope"
*>
fn BoundaryCommitEscapeResult boundary_commit_escape(
    Value* result,
    Value* original_result,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    uint copy_site,
    PromotionContext* ctx = null
) {
    BoundaryCommitEscapeResult committed = {
        .value = result,
        .outcome = BOUNDARY_COMMIT_NULL_RESULT,
        .releasing_scope_spliced = false,
        .reused_in_target_chain = false,
        .fallback_copied = false,
    };
    if (result == null) return committed;
    committed.outcome = BOUNDARY_COMMIT_PASSTHROUGH;
    defer boundary_debug_graph_audit_committed_escape_root(committed, target_scope);

    BoundaryTxn txn = boundary_txn_begin(interp);
    defer boundary_txn_close(interp, &txn);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;

    BoundaryReturnClassification classification =
        boundary_classify_return_value(result, interp, target_scope, releasing_scope);

    boundary_txn_assert_open(&txn);
    if (classification.provenance == BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN) {
        committed.value = result;
        committed.outcome = BOUNDARY_COMMIT_REUSED_IN_TARGET_CHAIN;
        committed.reused_in_target_chain = true;
        boundary_txn_mark_committed(&txn);
        return committed;
    }

    if (classification.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP ||
        classification.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE) {
        PromotionContext local_ctx = {};
        PromotionContext* active_ctx = ctx;
        bool owns_ctx = false;
        if (active_ctx == null) {
            boundary_promotion_context_begin(interp, &local_ctx);
            active_ctx = &local_ctx;
            owns_ctx = true;
        } else if (active_ctx.aborted) {
            // A pre-aborted caller context should not force boundary finalize
            // into immediate fallback. Use an isolated boundary context so
            // destination routing and splice legality can still run.
            boundary_promotion_context_begin(interp, &local_ctx, PROMOTION_CTX_BUDGET_RETURN);
            active_ctx = &local_ctx;
            owns_ctx = true;
        }
        defer {
            if (owns_ctx) boundary_promotion_context_end(interp, active_ctx);
        }

        bool splice_rejected_for_commit = false;
        bool requires_escape_promotion =
            classification.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP;

        if (requires_escape_promotion) {
            if (result.tag == ERROR) {
                // Stack-overflow/error unwinds must avoid disjoint promotion
                // routing that mutates promotion-context budget on deep stacks.
                Value* destination_error = boundary_build_destination_error_escape(
                    result,
                    interp,
                    target_scope
                );
                if (destination_error != null &&
                    destination_error != result &&
                    destination_error.scope_gen == target_scope.escape_generation) {
                    committed.value = destination_error;
                    committed.outcome = BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED;
                    boundary_txn_mark_committed(&txn);
                    return committed;
                }
            } else if (result.tag == CONS) {
                Value* destination_cons = boundary_build_destination_cons_escape(
                    result,
                    interp,
                    target_scope,
                    releasing_scope,
                    active_ctx
                );
                if (!active_ctx.aborted &&
                    destination_cons != null &&
                    destination_cons != result &&
                    destination_cons.scope_gen == target_scope.escape_generation) {
                    committed.value = destination_cons;
                    committed.outcome = BOUNDARY_COMMIT_DESTINATION_CONS_BUILT;
                    boundary_txn_mark_committed(&txn);
                    return committed;
                }
            } else if (result.tag == PARTIAL_PRIM) {
                Value* destination_partial = boundary_build_destination_partial_escape(
                    result,
                    interp,
                    target_scope,
                    releasing_scope,
                    active_ctx
                );
                if (!active_ctx.aborted &&
                    destination_partial != null &&
                    destination_partial != result &&
                    destination_partial.scope_gen == target_scope.escape_generation) {
                    committed.value = destination_partial;
                    committed.outcome = BOUNDARY_COMMIT_DESTINATION_PARTIAL_BUILT;
                    boundary_txn_mark_committed(&txn);
                    return committed;
                }
            } else if (result.tag == ITERATOR) {
                Value* destination_iterator = boundary_build_destination_iterator_escape(
                    result,
                    interp,
                    target_scope,
                    releasing_scope,
                    active_ctx
                );
                if (!active_ctx.aborted &&
                    destination_iterator != null &&
                    destination_iterator != result &&
                    destination_iterator.scope_gen == target_scope.escape_generation) {
                    committed.value = destination_iterator;
                    committed.outcome = BOUNDARY_COMMIT_DESTINATION_ITERATOR_BUILT;
                    boundary_txn_mark_committed(&txn);
                    return committed;
                }
            } else {
                // For remaining TEMP-owned survivor tags, route directly into the
                // destination scope's ESCAPE lane instead of promoting in the
                // releasing scope and relying on splice legality.
                Value* destination_direct = boundary_promote_to_escape(result, interp);
                if (!active_ctx.aborted &&
                    destination_direct != null &&
                    destination_direct != result &&
                    destination_direct.scope_gen == target_scope.escape_generation) {
                    committed.value = destination_direct;
                    committed.outcome = BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED;
                    boundary_txn_mark_committed(&txn);
                    return committed;
                }
            }
        }

        if (releasing_scope.refcount == 1) {
            bool pre_aborted_ctx = active_ctx.aborted;
            if (requires_escape_promotion) {
                boundary_note_promotion_attempt();
                BoundaryScopeSwap promote_scope = boundary_scope_swap_begin(interp, releasing_scope);
                defer boundary_scope_swap_end(interp, &promote_scope);
                result = boundary_promote_to_escape(result, interp);
                classification = boundary_classify_return_value(result, interp, target_scope, releasing_scope);
            }

            // Budget-abort fallback: preserve copy-site tagging from caller.
            if (active_ctx.aborted) {
                if (classification.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE) {
                    BoundaryScopeTransferResult splice_result =
                        boundary_try_scope_splice_escapes_result(target_scope, releasing_scope);
                    if (splice_result.allowed) {
                        committed.value = result;
                        committed.outcome = BOUNDARY_COMMIT_SPLICED_RELEASING_SCOPE;
                        committed.releasing_scope_spliced = true;
                        boundary_txn_mark_committed(&txn);
                        return committed;
                    }
                    splice_rejected_for_commit = true;
                }
                BoundaryPromotionAbortReason abort_reason =
                    BOUNDARY_PROMOTION_ABORT_BUDGET_EXHAUSTED;
                if (pre_aborted_ctx || active_ctx.abort_reason != PROMOTION_ABORT_BUDGET_EXHAUSTED) {
                    abort_reason = BOUNDARY_PROMOTION_ABORT_PRE_ABORTED_CTX;
                }
                boundary_note_promotion_abort(
                    abort_reason
                );
                Value* fallback_source = original_result == null ? result : original_result;
                Value* fallback = boundary_copy_from_releasing_scope(
                    fallback_source,
                    interp,
                    releasing_scope,
                    copy_site
                );
                boundary_note_copy_fallback(BOUNDARY_COPY_FALLBACK_BUDGET_ABORT, copy_site);
                committed.value = fallback;
                committed.outcome = BOUNDARY_COMMIT_FALLBACK_COPIED;
                committed.fallback_copied = fallback != fallback_source;
                boundary_txn_mark_aborted(&txn);
                return committed;
            }

            if (classification.provenance == BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE &&
                boundary_should_promote(result, active_ctx, releasing_scope)) {
                BoundaryScopeTransferResult splice_result =
                    boundary_try_scope_splice_escapes_result(target_scope, releasing_scope);
                if (splice_result.allowed) {
                    committed.value = result;
                    committed.outcome = BOUNDARY_COMMIT_SPLICED_RELEASING_SCOPE;
                    committed.releasing_scope_spliced = true;
                    boundary_txn_mark_committed(&txn);
                    return committed;
                }
                splice_rejected_for_commit = true;
            }
        }

        Value* copied = boundary_copy_from_releasing_scope(result, interp, releasing_scope, copy_site);
        boundary_note_copy_fallback(
            splice_rejected_for_commit
                ? BOUNDARY_COPY_FALLBACK_SPLICE_REJECTED
                : BOUNDARY_COPY_FALLBACK_RELEASING_SCOPE_OWNED,
            copy_site
        );
        committed.value = copied;
        committed.outcome = BOUNDARY_COMMIT_FALLBACK_COPIED;
        committed.fallback_copied = copied != result;
        boundary_txn_mark_committed(&txn);
        return committed;
    }

    // Unknown or mixed provenance: first try destination ESCAPE promotion under
    // explicit promotion budget/context, then fall back to boundary copy.
    PromotionContext mixed_local_ctx = {};
    PromotionContext* mixed_ctx = ctx;
    bool owns_mixed_ctx = false;
    if (mixed_ctx == null) {
        boundary_promotion_context_begin(interp, &mixed_local_ctx);
        mixed_ctx = &mixed_local_ctx;
        owns_mixed_ctx = true;
    }
    defer {
        if (owns_mixed_ctx) boundary_promotion_context_end(interp, mixed_ctx);
    }

    Value* mixed_destination = boundary_promote_to_escape(result, interp);
    if (!mixed_ctx.aborted &&
        mixed_destination != null &&
        mixed_destination != result &&
        mixed_destination.scope_gen == target_scope.escape_generation) {
        committed.value = mixed_destination;
        committed.outcome = BOUNDARY_COMMIT_MIXED_DESTINATION_PROMOTED;
        boundary_txn_mark_committed(&txn);
        return committed;
    }

    // Fallback copy remains for unresolved/aborted mixed provenance routes.
    Value* fallback_source = original_result == null ? result : original_result;
    Value* copied = boundary_copy_to_scope_site(
        fallback_source,
        interp,
        target_scope,
        releasing_scope,
        copy_site
    );
    boundary_note_copy_fallback(
        BOUNDARY_COPY_FALLBACK_MIXED_UNCERTAIN_PROVENANCE,
        copy_site
    );
    committed.value = copied;
    committed.outcome = BOUNDARY_COMMIT_FALLBACK_COPIED;
    committed.fallback_copied = copied != fallback_source;
    boundary_txn_mark_committed(&txn);
    return committed;
}

<*
@require interp != null : "boundary_promotion_context_begin: null interp"
@require ctx != null : "boundary_promotion_context_begin: null ctx"
*>
fn void boundary_promotion_context_begin(Interp* interp, PromotionContext* ctx, usz budget = usz.max) @inline {
    promotion_context_begin(interp, ctx, budget);
}

<*
@require interp != null : "boundary_promotion_context_end: null interp"
@require ctx != null : "boundary_promotion_context_end: null ctx"
*>
fn void boundary_promotion_context_end(Interp* interp, PromotionContext* ctx) @inline {
    promotion_context_end(interp, ctx);
}
