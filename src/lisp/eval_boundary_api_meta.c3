module lisp;

import main;

extern fn char* boundary_c_getenv(char* name) @extern("getenv");

fn bool boundary_env_flag_enabled(char* name) @inline {
    if (name == null) return false;
    char* value = boundary_c_getenv(name);
    if (value == null) return false;

    while (*value != 0 && (*value == ' ' || *value == '\t' || *value == '\n' || *value == '\r')) {
        value++;
    }
    if (*value == 0) return false;

    if (value[0] == '1') return true;
    if (value[0] == 't' || value[0] == 'T') return true;
    if (value[0] == 'y' || value[0] == 'Y') return true;
    if ((value[0] == 'o' || value[0] == 'O') &&
        (value[1] == 'n' || value[1] == 'N')) {
        return true;
    }

    return false;
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
    BOUNDARY_COMMIT_FALLBACK_DISALLOWED,
    BOUNDARY_COMMIT_PASSTHROUGH,
}

const usz BOUNDARY_COMMIT_OUTCOME_COUNT =
    (usz)BoundaryCommitOutcome.BOUNDARY_COMMIT_PASSTHROUGH + 1;

enum BoundaryCommitFault : char {
    BOUNDARY_COMMIT_FAULT_NONE,
    BOUNDARY_COMMIT_FAULT_DESTINATION_ERROR_BUILD,
    BOUNDARY_COMMIT_FAULT_DESTINATION_CONS_BUILD,
    BOUNDARY_COMMIT_FAULT_DESTINATION_PARTIAL_BUILD,
    BOUNDARY_COMMIT_FAULT_DESTINATION_ITERATOR_BUILD,
    BOUNDARY_COMMIT_FAULT_DESTINATION_DIRECT_PROMOTION,
    BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED,
    BOUNDARY_COMMIT_FAULT_SPLICE_REJECTED,
    BOUNDARY_COMMIT_FAULT_MIXED_DESTINATION_PROMOTION,
    BOUNDARY_COMMIT_FAULT_FALLBACK_DISALLOWED,
}

const usz BOUNDARY_COMMIT_FAULT_COUNT =
    (usz)BoundaryCommitFault.BOUNDARY_COMMIT_FAULT_FALLBACK_DISALLOWED + 1;

enum BoundaryCommitFallbackRoute : char {
    BOUNDARY_COMMIT_FALLBACK_ROUTE_DISALLOWED,
}

const usz BOUNDARY_COMMIT_FALLBACK_ROUTE_COUNT =
    (usz)BoundaryCommitFallbackRoute.BOUNDARY_COMMIT_FALLBACK_ROUTE_DISALLOWED + 1;

enum BoundaryReturnProvenance : char {
    BOUNDARY_RETURN_REUSABLE_IN_TARGET_CHAIN,
    BOUNDARY_RETURN_OWNED_BY_RELEASING_TEMP,
    BOUNDARY_RETURN_OWNED_BY_RELEASING_ESCAPE,
    BOUNDARY_RETURN_MIXED_UNCERTAIN,
}

const usz BOUNDARY_RETURN_PROVENANCE_COUNT =
    (usz)BoundaryReturnProvenance.BOUNDARY_RETURN_MIXED_UNCERTAIN + 1;

enum BoundaryEnvCopyFault : char {
    BOUNDARY_ENV_COPY_FAULT_NONE,
    BOUNDARY_ENV_COPY_FAULT_PROMOTION_CTX_ALLOC,
    BOUNDARY_ENV_COPY_FAULT_DEPTH_LIMIT,
    BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC,
    BOUNDARY_ENV_COPY_FAULT_BINDING_BUFFER_ALLOC,
    BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY,
    BOUNDARY_ENV_COPY_FAULT_PARENT_COPY,
}

const usz BOUNDARY_ENV_COPY_FAULT_COUNT =
    (usz)BoundaryEnvCopyFault.BOUNDARY_ENV_COPY_FAULT_PARENT_COPY + 1;

enum BoundaryCopyFault : char {
    BOUNDARY_COPY_FAULT_NONE,
    BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE,
    BOUNDARY_COPY_FAULT_CONTEXT_ABORTED,
    BOUNDARY_COPY_FAULT_CLOSURE_TYPE_SIG_COPY,
    BOUNDARY_COPY_FAULT_NULL_RESULT,
}

const usz BOUNDARY_COPY_FAULT_COUNT =
    (usz)BoundaryCopyFault.BOUNDARY_COPY_FAULT_NULL_RESULT + 1;

struct BoundaryEnvCopyResult {
    Env* env;
    BoundaryEnvCopyFault fault_code;
    usz depth;
}

struct BoundaryCopyResult {
    Value* value;
    BoundaryCopyFault fault_code;
}

struct BoundaryReturnClassification {
    BoundaryReturnProvenance provenance;
    bool in_target_scope_chain;
    bool in_releasing_scope;
}

struct BoundaryCommitEscapeResult {
    Value* value;
    BoundaryCommitOutcome outcome;
    BoundaryCommitFault fault_code;
    bool releasing_scope_spliced;
    bool reused_in_target_chain;
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

const char[][] BOUNDARY_COMMIT_OUTCOME_NAMES = {
    "null-result",
    "reused-in-target-chain",
    "destination-cons-built",
    "destination-partial-built",
    "destination-iterator-built",
    "destination-direct-promoted",
    "mixed-destination-promoted",
    "spliced-releasing-scope",
    "fallback-disallowed",
    "passthrough",
};

const char[][] BOUNDARY_COMMIT_FAULT_NAMES = {
    "none",
    "destination-error-build",
    "destination-cons-build",
    "destination-partial-build",
    "destination-iterator-build",
    "destination-direct-promotion",
    "promotion-aborted",
    "splice-rejected",
    "mixed-destination-promotion",
    "fallback-disallowed",
};

const char[][] BOUNDARY_COMMIT_FALLBACK_ROUTE_NAMES = {
    "fallback-disallowed",
};

const char[][] BOUNDARY_PROMOTION_ABORT_REASON_NAMES = {
    "budget-exhausted",
    "pre-aborted-ctx",
};

const char[][] BOUNDARY_RETURN_PROVENANCE_NAMES = {
    "reusable-in-target-chain",
    "owned-by-releasing-temp",
    "owned-by-releasing-escape",
    "mixed-uncertain",
};

const char[][] BOUNDARY_ENV_COPY_FAULT_NAMES = {
    "none",
    "promotion-ctx-alloc",
    "depth-limit",
    "env-alloc",
    "binding-buffer-alloc",
    "binding-value-copy",
    "parent-copy",
};

const char[][] BOUNDARY_COPY_FAULT_NAMES = {
    "none",
    "missing-scope-provenance",
    "context-aborted",
    "closure-type-sig-copy",
    "null-result",
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

$assert(BOUNDARY_COMMIT_OUTCOME_NAMES.len == BOUNDARY_COMMIT_OUTCOME_COUNT);
$assert(BOUNDARY_COMMIT_FAULT_NAMES.len == BOUNDARY_COMMIT_FAULT_COUNT);
$assert(BOUNDARY_COMMIT_FALLBACK_ROUTE_NAMES.len == BOUNDARY_COMMIT_FALLBACK_ROUTE_COUNT);
$assert(BOUNDARY_SCOPE_TRANSFER_REASON_NAMES.len == BOUNDARY_SCOPE_TRANSFER_REASON_COUNT);
$assert(BOUNDARY_PROMOTION_ABORT_REASON_NAMES.len == BOUNDARY_PROMOTION_ABORT_REASON_COUNT);
$assert(BOUNDARY_RETURN_PROVENANCE_NAMES.len == BOUNDARY_RETURN_PROVENANCE_COUNT);
$assert(BOUNDARY_ENV_COPY_FAULT_NAMES.len == BOUNDARY_ENV_COPY_FAULT_COUNT);
$assert(BOUNDARY_COPY_FAULT_NAMES.len == BOUNDARY_COPY_FAULT_COUNT);
$assert(BOUNDARY_COPY_SITE_NAMES.len == COPY_SITE_COUNT);

fn char[] boundary_commit_outcome_name(BoundaryCommitOutcome outcome) @inline {
    usz idx = (usz)outcome;
    if (idx < BOUNDARY_COMMIT_OUTCOME_NAMES.len) {
        return BOUNDARY_COMMIT_OUTCOME_NAMES[idx];
    }
    return "unknown";
}

fn char[] boundary_commit_fault_name(BoundaryCommitFault fault_code) @inline {
    usz idx = (usz)fault_code;
    if (idx < BOUNDARY_COMMIT_FAULT_NAMES.len) {
        return BOUNDARY_COMMIT_FAULT_NAMES[idx];
    }
    return "unknown";
}

fn BoundaryEnvCopyResult boundary_env_copy_ok(Env* env, usz depth = 0) @inline {
    return {
        .env = env,
        .fault_code = BOUNDARY_ENV_COPY_FAULT_NONE,
        .depth = depth,
    };
}

fn BoundaryEnvCopyResult boundary_env_copy_fail(BoundaryEnvCopyFault fault_code, usz depth = 0) @inline {
    return {
        .env = null,
        .fault_code = fault_code,
        .depth = depth,
    };
}

fn bool boundary_env_copy_result_ok(BoundaryEnvCopyResult result) @inline {
    return result.fault_code == BOUNDARY_ENV_COPY_FAULT_NONE;
}

fn BoundaryCopyResult boundary_copy_ok(Value* value) @inline {
    return {
        .value = value,
        .fault_code = BOUNDARY_COPY_FAULT_NONE,
    };
}

fn BoundaryCopyResult boundary_copy_fail(BoundaryCopyFault fault_code) @inline {
    return {
        .value = null,
        .fault_code = fault_code,
    };
}

fn bool boundary_copy_result_ok(BoundaryCopyResult result) @inline {
    return result.fault_code == BOUNDARY_COPY_FAULT_NONE;
}

fn char[] boundary_env_copy_fault_name(BoundaryEnvCopyFault fault_code) @inline {
    usz idx = (usz)fault_code;
    if (idx < BOUNDARY_ENV_COPY_FAULT_NAMES.len) {
        return BOUNDARY_ENV_COPY_FAULT_NAMES[idx];
    }
    return "unknown";
}

fn char[] boundary_copy_fault_name(BoundaryCopyFault fault_code) @inline {
    usz idx = (usz)fault_code;
    if (idx < BOUNDARY_COPY_FAULT_NAMES.len) {
        return BOUNDARY_COPY_FAULT_NAMES[idx];
    }
    return "unknown";
}

fn char[] boundary_copy_fault_message(BoundaryCopyFault fault_code) @inline {
    switch (fault_code) {
        case BOUNDARY_COPY_FAULT_NONE:
            return "boundary: copy succeeded";
        case BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE:
            return "boundary: copy_to_parent_site_ctx requires target/releasing scope provenance";
        case BOUNDARY_COPY_FAULT_CONTEXT_ABORTED:
            return "boundary: copy_to_parent_site_ctx aborted";
        case BOUNDARY_COPY_FAULT_CLOSURE_TYPE_SIG_COPY:
            return "boundary: closure type signature copy failed in copy_to_parent";
        case BOUNDARY_COPY_FAULT_NULL_RESULT:
            return "boundary: copy_to_parent_site_ctx returned null for non-null input";
    }
}
