module lisp;

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

fn char[] boundary_plan_route_name(BoundaryPlanRoute route) @inline {
    usz idx = (usz)route;
    if (idx < BOUNDARY_PLAN_ROUTE_NAMES.len) {
        return BOUNDARY_PLAN_ROUTE_NAMES[idx];
    }
    return "unknown";
}

fn char[] boundary_plan_reason_name(BoundaryPlanReason reason) @inline {
    usz idx = (usz)reason;
    if (idx < BOUNDARY_PLAN_REASON_NAMES.len) {
        return BOUNDARY_PLAN_REASON_NAMES[idx];
    }
    return "unknown";
}

fn char[] boundary_value_tag_name(ValueTag tag) @inline {
    usz idx = (usz)tag;
    if (idx < BOUNDARY_VALUE_TAG_NAMES.len) {
        return BOUNDARY_VALUE_TAG_NAMES[idx];
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
        case BOUNDARY_COPY_FAULT_DTOR_REGISTRATION:
            return "boundary: failed to register copied value destructor";
        case BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD:
            return "boundary: refusing primitive copy with opaque user_data";
        case BOUNDARY_COPY_FAULT_NULL_RESULT:
            return "boundary: copy_to_parent_site_ctx returned null for non-null input";
    }
}
