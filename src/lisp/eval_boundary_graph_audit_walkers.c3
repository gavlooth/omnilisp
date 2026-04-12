module lisp;

import main;

fn BoundaryGraphAuditResult boundary_graph_audit_visit_env(
    Env* env,
    Value** value_stack,
    usz* value_stack_len,
    Env** env_stack,
    usz* env_stack_len,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* forbidden_scope,
    Env* global_env,
    bool emit,
    usz root_tag,
    usz root_scope_gen
) {
    if (env == global_env || env.is_root_persistent_box()) {
        return {
            .ok = true,
            .reason = BOUNDARY_GRAPH_AUDIT_OK,
            .root_tag = root_tag,
            .violating_edge_tag = BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
            .root_scope_gen = root_scope_gen,
            .violating_scope_gen = 0,
            .target_scope_gen = (usz)target_scope.generation,
            .target_escape_gen = (usz)target_scope.escape_generation
        };
    }

    if (boundary_env_points_into_temp(env, target_scope, forbidden_scope, global_env)) {
        return boundary_graph_audit_result(
            emit,
            BOUNDARY_GRAPH_AUDIT_REACHABLE_TEMP_ENV,
            target_scope,
            root_tag,
            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_ENV,
            root_scope_gen,
            (usz)env.scope_gen
        );
    }

    if (!boundary_graph_audit_push_env(env_stack, env_stack_len, env.parent)) {
        return boundary_graph_audit_result(
            emit,
            BOUNDARY_GRAPH_AUDIT_PUSH_ENV_OVERFLOW,
            target_scope,
            root_tag,
            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
            root_scope_gen,
            0
        );
    }

    for (usz i = 0; i < env.binding_count; i++) {
        if (!boundary_graph_audit_push_value(value_stack, value_stack_len, env.bindings[i].value)) {
            return boundary_graph_audit_result(
                emit,
                BOUNDARY_GRAPH_AUDIT_PUSH_VALUE_OVERFLOW,
                target_scope,
                root_tag,
                BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                root_scope_gen,
                0
            );
        }
    }

    return {
        .ok = true,
        .reason = BOUNDARY_GRAPH_AUDIT_OK,
        .root_tag = root_tag,
        .violating_edge_tag = BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
        .root_scope_gen = root_scope_gen,
        .violating_scope_gen = 0,
        .target_scope_gen = (usz)target_scope.generation,
        .target_escape_gen = (usz)target_scope.escape_generation
    };
}
