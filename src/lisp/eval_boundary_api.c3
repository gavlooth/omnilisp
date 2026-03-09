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

fn usz boundary_env_usize_or_default(char* name, usz default_value) @inline {
    if (name == null) return default_value;
    char* value = boundary_c_getenv(name);
    if (value == null) return default_value;
    while (*value != 0 && (*value == ' ' || *value == '\t' || *value == '\n' || *value == '\r')) {
        value++;
    }
    if (*value == 0) return default_value;

    usz result = 0;
    bool has_digits = false;
    for (char* cursor = value; *cursor != 0; cursor++) {
        if (*cursor < '0' || *cursor > '9') return default_value;
        has_digits = true;
        result = result * 10 + (usz)(*cursor - '0');
    }
    if (!has_digits) return default_value;
    return result;
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
        case BOUNDARY_COPY_FAULT_NULL_RESULT:
            return "boundary: copy_to_parent_site_ctx returned null for non-null input";
    }
}

// =============================================================================
// SECTION 2.4: FRAME PUSH/POP (audited boundary facade)
// =============================================================================

<*
@require interp != null : "boundary_enter_scope: null interp"
@require target_scope != null : "boundary_enter_scope: null target_scope"
*>
fn main::ScopeRegion* boundary_enter_scope(Interp* interp, main::ScopeRegion* target_scope) @inline {
    main::ScopeRegion* saved_scope = interp.current_scope;
    interp.current_scope = target_scope;
    boundary_assert_interp_scope_chain(interp);
    return saved_scope;
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
@require parent != null : "boundary_scope_splice_escapes: null parent"
@require child != null : "boundary_scope_splice_escapes: null child"
*>
fn void boundary_scope_splice_escapes(main::ScopeRegion* parent, main::ScopeRegion* child) @inline {
    main::scope_splice_escapes(parent, child);
}

<*
@require interp != null : "boundary_copy_to_parent_site_ctx: null interp"
*>
fn BoundaryCopyResult boundary_copy_to_parent_site_ctx_checked(
    Value* v,
    Interp* interp,
    uint site,
    PromotionContext* ctx
) @nodiscard @inline {
    if (v == null) return boundary_copy_ok(null);
    if (interp.current_scope == null || interp.releasing_scope == null) {
        (void)site;
        return boundary_copy_fail(BOUNDARY_COPY_FAULT_MISSING_SCOPE_PROVENANCE);
    }
    if (ctx != null) {
        Value* cached = promotion_context_lookup(ctx, v);
        if (cached != null) return boundary_copy_ok(cached);
    }
    bool can_track = promotion_context_consume(ctx, 1);
    Value* copied = boundary_copy_to_parent_site(v, interp, site);
    if (copied == null) {
        return boundary_copy_fail(BOUNDARY_COPY_FAULT_NULL_RESULT);
    }
    if (can_track && ctx != null && copied != null) {
        promotion_context_remember(ctx, v, copied, interp);
    }
    return boundary_copy_ok(copied);
}

<*
@require interp != null : "boundary_copy_to_parent_site_ctx: null interp"
*>
fn Value* boundary_copy_to_parent_site_ctx(Value* v, Interp* interp, uint site, PromotionContext* ctx) @nodiscard @inline {
    BoundaryCopyResult copy_result =
        boundary_copy_to_parent_site_ctx_checked(v, interp, site, ctx);
    if (!boundary_copy_result_ok(copy_result)) {
        return raise_error(interp, boundary_copy_fault_message(copy_result.fault_code));
    }
    return copy_result.value;
}

<*
@require interp != null : "boundary_alloc_value_in_scope: null interp"
@require target_scope != null : "boundary_alloc_value_in_scope: null target_scope"
*>
fn Value* boundary_alloc_value_in_scope_impl(Interp* interp, main::ScopeRegion* target_scope, bool register_dtor = false) @noinline {
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
@require interp != null : "boundary_alloc_value_in_scope: null interp"
@require target_scope != null : "boundary_alloc_value_in_scope: null target_scope"
*>
fn Value* boundary_alloc_value_in_scope(Interp* interp, main::ScopeRegion* target_scope, bool register_dtor = false) @inline {
    return boundary_alloc_value_in_scope_impl(interp, target_scope, register_dtor);
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
fn Env* boundary_make_env_in_scope_impl(Interp* interp, main::ScopeRegion* target_scope, Env* parent) @noinline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    return make_env(interp, parent);
}

<*
@require interp != null : "boundary_make_env_in_scope: null interp"
@require target_scope != null : "boundary_make_env_in_scope: null target_scope"
*>
fn Env* boundary_make_env_in_scope(Interp* interp, main::ScopeRegion* target_scope, Env* parent) @inline {
    return boundary_make_env_in_scope_impl(interp, target_scope, parent);
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
fn Env* boundary_env_extend_in_scope_impl(
    Interp* interp,
    main::ScopeRegion* target_scope,
    Env* env,
    SymbolId name,
    Value* value
) @noinline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    return env.extend(interp, name, value);
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
    return boundary_env_extend_in_scope_impl(interp, target_scope, env, name, value);
}

<*
@require interp != null : "boundary_env_extend_in_root: null interp"
@require env != null : "boundary_env_extend_in_root: null env"
*>
fn Env* boundary_env_extend_in_root(Interp* interp, Env* env, SymbolId name, Value* value) @inline {
    Env* box = boundary_env_extend_in_scope(interp, interp.root_scope, env, name, value);
    if (box != null) {
        box.lifetime_kind = ENV_LIFETIME_ROOT_PERSISTENT_MUTABLE_BOX;
    }
    return box;
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
@require interp != null : "boundary_copy_env_to_scope: null interp"
*>
fn Env* boundary_copy_env_to_scope(Env* env, Interp* interp, usz depth = 0, PromotionContext* ctx = null) @inline {
    return copy_env_to_scope_checked(env, interp, depth, ctx).env;
}

<*
@require interp != null : "boundary_copy_env_to_scope_checked: null interp"
*>
fn BoundaryEnvCopyResult boundary_copy_env_to_scope_checked(
    Env* env,
    Interp* interp,
    usz depth = 0,
    PromotionContext* ctx = null
) @nodiscard @inline {
    return copy_env_to_scope_checked(env, interp, depth, ctx);
}

<*
@require interp != null : "boundary_copy_env_to_target_scope: null interp"
@require target_scope != null : "boundary_copy_env_to_target_scope: null target_scope"
*>
fn Env* boundary_copy_env_to_target_scope_impl(
    Env* env,
    Interp* interp,
    main::ScopeRegion* target_scope,
    usz depth = 0,
    PromotionContext* ctx = null
) @noinline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    // Env-copy boundaries release from the caller's active scope into target_scope.
    interp.releasing_scope = session.current_scope;
    return boundary_copy_env_to_scope_checked(env, interp, depth, ctx).env;
}

<*
@require interp != null : "boundary_copy_env_to_target_scope_checked: null interp"
@require target_scope != null : "boundary_copy_env_to_target_scope_checked: null target_scope"
*>
fn BoundaryEnvCopyResult boundary_copy_env_to_target_scope_checked(
    Env* env,
    Interp* interp,
    main::ScopeRegion* target_scope,
    usz depth = 0,
    PromotionContext* ctx = null
) @nodiscard @inline {
    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = session.current_scope;
    return boundary_copy_env_to_scope_checked(env, interp, depth, ctx);
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
) @nodiscard @inline {
    return boundary_copy_env_to_target_scope_impl(env, interp, target_scope, depth, ctx);
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
