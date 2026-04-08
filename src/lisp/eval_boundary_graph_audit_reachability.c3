module lisp;

import main;

fn BoundaryGraphAuditResult boundary_graph_audit_escape_reachability_result(
    Value* root,
    main::ScopeRegion* target_scope,
    bool emit = false
) @noinline {
    if (root == null || target_scope == null) {
        return {
            .ok = true,
            .reason = BOUNDARY_GRAPH_AUDIT_OK,
            .root_tag = root != null ? (usz)root.tag : BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
            .violating_edge_tag = BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
            .root_scope_gen = root != null ? (usz)root.scope_gen : 0,
            .violating_scope_gen = 0,
            .target_scope_gen = target_scope != null ? (usz)target_scope.generation : 0,
            .target_escape_gen = target_scope != null ? (usz)target_scope.escape_generation : 0
        };
    }

    usz root_tag = (usz)root.tag;
    usz root_scope_gen = (usz)root.scope_gen;

    if (emit && !boundary_graph_audit_enabled()) {
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

    Value** value_stack = (Value**)mem::malloc(Value*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    Value** seen_values = (Value**)mem::malloc(Value*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    Env** env_stack = (Env**)mem::malloc(Env*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    Env** seen_envs = (Env**)mem::malloc(Env*.sizeof * BOUNDARY_GRAPH_AUDIT_MAX_NODES);
    if (value_stack == null || seen_values == null || env_stack == null || seen_envs == null) {
        if (value_stack != null) mem::free(value_stack);
        if (seen_values != null) mem::free(seen_values);
        if (env_stack != null) mem::free(env_stack);
        if (seen_envs != null) mem::free(seen_envs);
        return boundary_graph_audit_result(
            emit,
            BOUNDARY_GRAPH_AUDIT_ALLOCATION_FAILED,
            target_scope,
            root_tag,
            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
            root_scope_gen,
            0
        );
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
        return boundary_graph_audit_result(
            emit,
            BOUNDARY_GRAPH_AUDIT_SEED_OVERFLOW,
            target_scope,
            root_tag,
            BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
            root_scope_gen,
            0
        );
    }

    while (value_stack_len > 0 || env_stack_len > 0) {
        while (value_stack_len > 0) {
            Value* cur = value_stack[--value_stack_len];
            if (cur == null) continue;
            if (boundary_graph_audit_value_seen(seen_values, seen_value_len, cur)) continue;
            if (seen_value_len >= BOUNDARY_GRAPH_AUDIT_MAX_NODES) {
                return boundary_graph_audit_result(
                    emit,
                    BOUNDARY_GRAPH_AUDIT_VALUE_SET_OVERFLOW,
                    target_scope,
                    root_tag,
                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                    root_scope_gen,
                    0
                );
            }
            seen_values[seen_value_len++] = cur;

            BoundaryGraphAuditResult step = boundary_graph_audit_visit_value(
                cur,
                value_stack,
                &value_stack_len,
                env_stack,
                &env_stack_len,
                target_scope,
                emit,
                root_tag,
                root_scope_gen
            );
            if (!step.ok) {
                return step;
            }
        }

        while (env_stack_len > 0) {
            Env* env = env_stack[--env_stack_len];
            if (env == null) continue;
            if (boundary_graph_audit_env_seen(seen_envs, seen_env_len, env)) continue;
            if (seen_env_len >= BOUNDARY_GRAPH_AUDIT_MAX_NODES) {
                return boundary_graph_audit_result(
                    emit,
                    BOUNDARY_GRAPH_AUDIT_ENV_SET_OVERFLOW,
                    target_scope,
                    root_tag,
                    BOUNDARY_GRAPH_AUDIT_EDGE_TAG_NONE,
                    root_scope_gen,
                    0
                );
            }
            seen_envs[seen_env_len++] = env;

            BoundaryGraphAuditResult step = boundary_graph_audit_visit_env(
                env,
                value_stack,
                &value_stack_len,
                env_stack,
                &env_stack_len,
                target_scope,
                emit,
                root_tag,
                root_scope_gen
            );
            if (!step.ok) {
                return step;
            }
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
