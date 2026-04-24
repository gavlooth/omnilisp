module lisp;

import main;

struct BoundaryCommitClosureNormalizeStage {
    bool active;
    Value* closure;
    Env* prior_env;
    main::ScopeRegion* prior_env_scope;
    main::ScopeRegion* normalized_env_scope;
}

fn bool boundary_commit_closure_env_needs_normalize(Value* value) @inline {
    if (value == null || value.tag != CLOSURE || value.closure_val == null) return false;
    if (value.closure_val.env == null || value.closure_val.env_scope == null) return false;
    return !boundary_ptr_in_scope((void*)value.closure_val.env, value.closure_val.env_scope);
}

fn Value* boundary_commit_normalize_closure_env(
    Value* value,
    Interp* interp,
    PromotionContext* ctx = null,
    BoundaryCommitClosureNormalizeStage* stage = null
) {
    if (stage != null) *stage = {};
    if (!boundary_commit_closure_env_needs_normalize(value)) return value;

    main::ScopeRegion* replacement_env_scope = main::scope_create(null);
    if (replacement_env_scope == null) {
        return runtime_raise(interp, "runtime/out-of-memory", "boundary: failed to allocate normalized closure env scope");
    }

    BoundaryEnvCopyResult copied = boundary_copy_env_to_target_scope_checked(
        value.closure_val.env,
        interp,
        replacement_env_scope,
        0,
        ctx
    );
    if (!boundary_env_copy_result_ok(copied) || copied.env == null) {
        main::scope_release(replacement_env_scope);
        return runtime_raise(interp, "boundary/copy-fault", "boundary: failed to normalize closure env scope");
    }

    main::ScopeRegion* prior_env_scope = value.closure_val.env_scope;
    if (stage != null) {
        stage.active = true;
        stage.closure = value;
        stage.prior_env = value.closure_val.env;
        stage.prior_env_scope = prior_env_scope;
        stage.normalized_env_scope = replacement_env_scope;
    } else if (prior_env_scope != null) {
        main::scope_release(prior_env_scope);
    }
    value.closure_val.env = copied.env;
    value.closure_val.env_scope = replacement_env_scope;
    return value;
}

fn void boundary_commit_normalized_closure_finalize(
    BoundaryCommitClosureNormalizeStage* stage
) {
    if (stage == null || !stage.active) return;
    if (stage.prior_env_scope != null) {
        main::scope_release(stage.prior_env_scope);
    }
    *stage = {};
}

fn void boundary_commit_normalized_closure_rollback(
    BoundaryCommitClosureNormalizeStage* stage
) {
    if (stage == null || !stage.active) return;
    if (stage.closure != null &&
        stage.closure.tag == CLOSURE &&
        stage.closure.closure_val != null) {
        if (stage.closure.closure_val.env_scope == stage.normalized_env_scope &&
            stage.normalized_env_scope != null) {
            main::scope_release(stage.normalized_env_scope);
        }
        stage.closure.closure_val.env = stage.prior_env;
        stage.closure.closure_val.env_scope = stage.prior_env_scope;
    } else if (stage.normalized_env_scope != null) {
        main::scope_release(stage.normalized_env_scope);
    }
    *stage = {};
}

fn BoundaryTransplantProof boundary_commit_build_splice_escape_root_proof(
    Value* result,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope,
    Env* global_env = null
) @inline {
    BoundaryTransplantProof proof =
        boundary_build_transplant_root_proof(target_scope, releasing_scope, result, global_env);
    proof.root_owned_by_child_escape =
        result != null &&
        releasing_scope != null &&
        result.scope_gen == releasing_scope.escape_generation;
    if (proof.transfer.allowed && !proof.root_owned_by_child_escape) {
        proof.transfer.allowed = false;
        proof.transfer.reason = BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID;
        proof.reachable_temp_edges_safe = false;
    }
    if (proof.transfer.allowed) {
        proof.source_escape_graph_safe = boundary_graph_audit_escape_reachability(result, releasing_scope);
        if (!proof.source_escape_graph_safe) {
            proof.transfer.allowed = false;
            proof.transfer.reason = BOUNDARY_SCOPE_TRANSFER_CHILD_TEMP_LANE_INVALID;
            proof.reachable_temp_edges_safe = false;
        }
    }
    return proof;
}
