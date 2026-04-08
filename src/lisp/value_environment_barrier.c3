module lisp;

import main;

fn main::ScopeRegion* env_barrier_resolve_target_scope(Env* target, Interp* interp) @inline {
    if (target == null || interp == null) return null;

    // Detached copied/normalized closure envs carry explicit owner scope authority.
    if (target.owner_scope != null &&
        (target.owner_scope.generation == target.scope_gen ||
         target.owner_scope.escape_generation == target.scope_gen)) {
        return target.owner_scope;
    }

    // Fallback for legacy/stale frames that do not carry owner scope authority.
    for (main::ScopeRegion* s = interp.current_scope; s != null; s = s.parent) {
        if (s.generation == target.scope_gen || s.escape_generation == target.scope_gen) return s;
    }
    if (interp.root_scope != null &&
        (interp.root_scope.generation == target.scope_gen ||
         interp.root_scope.escape_generation == target.scope_gen)) {
        return interp.root_scope;
    }
    return null;
}

// Barrier-aware env write helper for dual-lane rollout.
// If target env is in current scope's ESCAPE lane, promote TEMP values first.
fn Value* promote_for_env_target(Env* target, Value* value, Interp* interp) {
    if (target == null || interp == null || interp.current_scope == null) return value;
    if (value == null) return null;

    main::ScopeRegion* target_scope = env_barrier_resolve_target_scope(target, interp);
    if (target_scope == null) {
        // Fail closed: unknown target scope authority must never retain raw TEMP values.
        if (interp.root_scope == null) return value;
        target_scope = interp.root_scope;
    }

    // Intra-scope writes keep existing fast paths.
    if (target_scope == interp.current_scope) {
        if (target.scope_gen == target_scope.escape_generation) {
            return boundary_promote_to_escape(value, interp);
        }
        if (target.scope_gen == target_scope.generation) {
            return value;
        }
    }

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = session.current_scope;

    // Detached ESCAPE-lane env target: promote directly into target ESCAPE lane.
    if (target.scope_gen == target_scope.escape_generation) {
        return boundary_promote_to_escape(value, interp);
    }

    // Detached TEMP-lane env target: retain only if value is already in target scope.
    if (target.scope_gen == target_scope.generation &&
        boundary_ptr_in_scope((void*)value, target_scope)) {
        return value;
    }

    if (COPY_SITE_ENV_PROMOTE_FOR_TARGET < COPY_SITE_COUNT) {
        g_copy_to_parent_stats.site_calls[COPY_SITE_ENV_PROMOTE_FOR_TARGET]++;
    }
    CopyParentRoute route = copy_parent_route_for_tag(value.tag);
    return copy_to_parent_by_route(value, interp, route);
}

fn bool env_barrier_promote_failed(Value* original, Value* stored) @inline {
    if (stored == null) return true;
    return original != null && original.tag != ERROR && stored.tag == ERROR;
}

fn Value* env_promote_for_target_result(Env* env, SymbolId _name, Value* value, Interp* interp, char[] failure_msg) {
    (void)_name;
    if (g_env_force_barrier_promote_failure) {
        return raise_error(interp, failure_msg);
    }

    Value* stored = promote_for_env_target(env, value, interp);
    if (env_barrier_promote_failed(value, stored)) {
        return stored != null ? stored : raise_error(interp, failure_msg);
    }
    return stored;
}

fn Value* env_define_with_barrier_result(Env* env, SymbolId name, Value* value, Interp* interp) {
    if (env == null) return raise_error(interp, "out of memory while defining binding");
    Value* stored = env_promote_for_target_result(env, name, value, interp, "out of memory while defining binding");
    if (stored != null && stored.tag == ERROR && value != stored) {
        return stored;
    }
    if (!env.define(name, stored)) {
        return raise_error(interp, "out of memory while defining binding");
    }
    return null;
}

fn bool env_define_with_barrier_checked(Env* env, SymbolId name, Value* value, Interp* interp) @nodiscard {
    return env_define_with_barrier_result(env, name, value, interp) == null;
}

fn void env_define_with_barrier(Env* env, SymbolId name, Value* value, Interp* interp) {
    assert(
        env_define_with_barrier_checked(env, name, value, interp),
        "env_define_with_barrier: binding define failed"
    );
}

// Barrier-aware env set helper for dual-lane rollout.
// Walks parent chain and applies TEMP->ESCAPE promotion at the actual write site.
fn Value* env_set_with_barrier_result(Env* env, SymbolId name, Value* value, Interp* interp, bool* found_out) {
    if (found_out != null) *found_out = false;
    Env* cur = env;
    while (cur != null) {
        if (cur.hash_table != null) {
            usz idx = cur.hash_lookup(name);
            if (idx != ~(usz)0) {
                if (found_out != null) *found_out = true;
                Value* stored = env_promote_for_target_result(cur, name, value, interp, "out of memory while mutating binding");
                if (stored != null && stored.tag == ERROR && value != stored) {
                    return stored;
                }
                cur.bindings[idx].value = stored;
                return null;
            }
        } else {
            for (usz i = 0; i < cur.binding_count; i++) {
                if (cur.bindings[i].name == name) {
                    if (found_out != null) *found_out = true;
                    Value* stored = env_promote_for_target_result(cur, name, value, interp, "out of memory while mutating binding");
                    if (stored != null && stored.tag == ERROR && value != stored) {
                        return stored;
                    }
                    cur.bindings[i].value = stored;
                    return null;
                }
            }
        }
        cur = cur.parent;
    }
    return null;
}

fn bool env_set_with_barrier_checked(Env* env, SymbolId name, Value* value, Interp* interp, bool* found_out = null) @nodiscard {
    return env_set_with_barrier_result(env, name, value, interp, found_out) == null;
}

fn void? env_set_with_barrier(Env* env, SymbolId name, Value* value, Interp* interp) {
    bool found = false;
    if (!env_set_with_barrier_checked(env, name, value, interp, &found)) {
        return BINDING_WRITE_FAILED~;
    }
    if (!found) return UNBOUND_VARIABLE~;
    return;
}
