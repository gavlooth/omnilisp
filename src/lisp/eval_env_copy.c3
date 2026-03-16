module lisp;

import main;

fn void copy_env_invariant_fail(char[] msg) @noinline {
    boundary_invariant_fail("copy-env", msg);
}

fn bool copy_env_value_copy_failure(Value* src, Value* copied) @inline {
    return src != null && copied == null;
}

<*
@require interp != null : "copy_env_begin_owned_promotion_ctx: null interp"
@require interp.current_scope != null : "copy_env_begin_owned_promotion_ctx: null current_scope"
@ensure return == null || interp.active_promotion_ctx == return : "copy_env_begin_owned_promotion_ctx: active context must point at returned ctx"
*>
fn PromotionContext* copy_env_begin_owned_promotion_ctx(Interp* interp) @inline {
    PromotionContext* local_ctx = (PromotionContext*)interp.current_scope.alloc(PromotionContext.sizeof);
    if (local_ctx == null) return null;
    boundary_promotion_context_begin(interp, local_ctx);
    return local_ctx;
}

<*
@require interp != null : "copy_env_end_owned_promotion_ctx: null interp"
@require ctx != null : "copy_env_end_owned_promotion_ctx: null ctx"
@ensure interp.active_promotion_ctx == saved_active_ctx : "copy_env_end_owned_promotion_ctx: active context restore mismatch"
*>
fn void copy_env_end_owned_promotion_ctx(Interp* interp, PromotionContext* ctx, PromotionContext* saved_active_ctx) @inline {
    boundary_promotion_context_end(interp, ctx);
    if (interp.active_promotion_ctx != saved_active_ctx) {
        copy_env_invariant_fail("copy_env_end_owned_promotion_ctx: active context restore mismatch");
    }
}

fn bool copy_env_should_reuse_value(Value* v, Interp* interp) @inline {
    return !boundary_should_copy_env(v, interp);
}

fn BoundaryCopyResult copy_env_copy_by_boundary_policy(Value* v, Interp* interp, PromotionContext* ctx) @inline {
    if (copy_env_should_reuse_value(v, interp)) return boundary_copy_ok(v);
    return boundary_copy_to_parent_site_ctx_checked(v, interp, COPY_SITE_COPY_ENV_TO_SCOPE, ctx);
}

fn Value* copy_env_copy_time_point(Value* v, Interp* interp) {
    Value* result = interp.alloc_value();
    result.tag = TIME_POINT;
    result.time_point_val = v.time_point_val;
    return result;
}

fn Value* copy_env_value_fast(Value* v, Interp* interp, PromotionContext* ctx = null) {
    if (v == null) return null;
    switch (v.tag) {
        case NIL:
            return make_nil(interp);
        case VOID:
            return make_void(interp);
        case INT:
            return make_int(interp, v.int_val);
        case DOUBLE:
            return make_double(interp, v.double_val);
        case SYMBOL:
            return make_symbol(interp, v.sym_val);
        case STRING:
            return make_string(interp, v.str_chars[:v.str_len]);
        case ERROR:
            return make_error(interp, v.str_chars[:v.str_len]);
        case PRIMITIVE:
        case HASHMAP:
        case SET:
        case ARRAY:
        case METHOD_TABLE:
        case TYPE_INFO:
        case MODULE:
            // Root-lifetime or external-lifetime objects can be shared.
            return v;
        case TIME_POINT:
            return copy_env_copy_time_point(v, interp);
        case INSTANCE:
        case FFI_HANDLE: {
            BoundaryCopyResult copied = copy_env_copy_by_boundary_policy(v, interp, ctx);
            if (!boundary_copy_result_ok(copied)) return null;
            return copied.value;
        }
        default:
        {
            BoundaryCopyResult copied = copy_env_copy_by_boundary_policy(v, interp, ctx);
            if (!boundary_copy_result_ok(copied)) return null;
            return copied.value;
        }
    }
}

fn bool copy_env_should_clone_closure(Value* val, Interp* interp) @inline {
    if (val == null || val.tag != CLOSURE || val.closure_val.env_scope == null) return false;
    if (copy_env_should_reuse_value(val, interp)) return false;
    return val.closure_val.scope_gen != interp.current_scope.generation;
}

fn bool copy_env_invalid_closure_alias_in_releasing_scope(Value* val, Interp* interp) @inline {
    if (val == null || interp == null || interp.releasing_scope == null) return false;
    if (val.tag != CLOSURE || val.closure_val == null) return false;
    // A closure without env_scope has no detached ownership envelope; if that wrapper
    // still belongs to releasing_scope, env-copy must fail closed instead of aliasing.
    if (val.closure_val.env_scope != null) return false;
    return boundary_ptr_in_scope((void*)val, interp.releasing_scope);
}

fn Value* copy_env_clone_closure_if_needed(Value* val, Interp* interp) {
    if (copy_env_invalid_closure_alias_in_releasing_scope(val, interp)) return null;
    if (!copy_env_should_clone_closure(val, interp)) return val;
    Value* result = interp.alloc_value();
    // Reuse the shared closure-clone payload path so env-copy and return-boundary
    // closure relocation keep the same retain/copy semantics.
    if (!copy_parent_clone_closure_payload(result, val, interp, null)) return null;
    main::scope_register_dtor(interp.current_scope, (void*)result, &scope_dtor_closure);
    return result;
}

fn Value* copy_env_clone_iterator_if_needed(Value* val, Interp* interp) {
    if (val == null || val.tag != ITERATOR) return val;
    if (boundary_ptr_in_target_scope_chain_with_hint((void*)val, interp, val.scope_gen)) return val;

    Value* inner = val.iterator_val;
    if (copy_env_invalid_closure_alias_in_releasing_scope(inner, interp)) return null;
    if (inner != null && inner.tag == CLOSURE && inner.closure_val.env_scope != null) {
        Value* cloned = copy_env_clone_closure_if_needed(inner, interp);
        if (cloned == null || cloned.tag != CLOSURE) {
            copy_env_invariant_fail("copy_env_clone_iterator_if_needed: expected cloned closure payload");
        }
        inner = cloned;
    }

    Value* iter_result = interp.alloc_value();
    iter_result.tag = ITERATOR;
    iter_result.iterator_val = inner;
    return iter_result;
}

fn Value* copy_env_binding_value(Value* val, Interp* interp, PromotionContext* ctx) {
    if (val != null && val.tag == CLOSURE) {
        return copy_env_clone_closure_if_needed(val, interp);
    }
    if (val != null && val.tag == ITERATOR) {
        return copy_env_clone_iterator_if_needed(val, interp);
    }
    return copy_env_value_fast(val, interp, ctx);
}

fn BoundaryEnvCopyResult copy_env_alloc_frame(Env* src, Interp* interp, usz depth) {
    Env* dst = interp.alloc_env();
    if (dst == null) {
        return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_ENV_ALLOC, depth);
    }
    dst.binding_count = src.binding_count;
    dst.parent = null;
    dst.hash_table = null;
    dst.hash_capacity = 0;
    dst.lifetime_kind = ENV_LIFETIME_LOCAL_SCOPE_FRAME;
    dst.scope_gen = interp.current_scope.generation;

    if (src.binding_count <= 4) {
        dst.is_inline = true;
        dst.bindings = &dst.inline_bindings[0];
        dst.capacity = 4;
        return boundary_env_copy_ok(dst, depth);
    }

    dst.is_inline = false;
    dst.bindings = (Binding*)mem::malloc(Binding.sizeof * src.binding_count);
    if (dst.bindings == null) {
        return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_BINDING_BUFFER_ALLOC, depth);
    }
    dst.capacity = src.binding_count;
    return boundary_env_copy_ok(dst, depth);
}

fn bool copy_env_copy_bindings(
    Env* src,
    Env* dst,
    Interp* interp,
    PromotionContext* ctx
) {
    for (usz i = 0; i < src.binding_count; i++) {
        dst.bindings[i].name = src.bindings[i].name;
        Value* copied = copy_env_binding_value(src.bindings[i].value, interp, ctx);
        if (copy_env_value_copy_failure(src.bindings[i].value, copied)) {
            return false;
        }
        dst.bindings[i].value = copied;
    }
    return true;
}

fn bool copy_env_is_terminal_frame(Env* env, Interp* interp, usz depth) @inline {
    if (env == null || env == interp.global_env) return true;
    return depth >= 256;
}

fn bool copy_env_parent_needs_rewrite(Env* parent, main::ScopeRegion* releasing_scope) @inline {
    if (parent == null || releasing_scope == null) return false;
    if (parent.scope_gen != releasing_scope.generation &&
        parent.scope_gen != releasing_scope.escape_generation) {
        return false;
    }
    return boundary_ptr_in_scope((void*)parent, releasing_scope);
}

fn bool copy_env_can_rewrite_persistent_box_in_place(Env* env, Interp* interp) @inline {
    if (env == null || interp == null || interp.root_scope == null) return false;
    // In-place parent rewrites are only valid for true root-owned persistent boxes.
    if (!main::is_in_scope((void*)env, interp.root_scope)) return false;
    if (interp.releasing_scope != null && boundary_ptr_in_scope((void*)env, interp.releasing_scope)) return false;
    return true;
}

fn void copy_env_cleanup_partial_frame(Env* env) @inline {
    if (env == null) return;
    if (!env.is_inline && env.bindings != null) {
        mem::free(env.bindings);
        env.bindings = null;
    }
    if (env.hash_table != null) {
        mem::free(env.hash_table);
        env.hash_table = null;
    }
}

fn BoundaryEnvCopyResult copy_env_rewrite_persistent_parent(
    Env* env,
    Interp* interp,
    usz depth,
    PromotionContext* active_ctx
) {
    if (!copy_env_can_rewrite_persistent_box_in_place(env, interp)) {
        return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_PARENT_COPY, depth);
    }

    if (!copy_env_parent_needs_rewrite(env.parent, interp.releasing_scope)) {
        return boundary_env_copy_ok(env, depth);
    }

    // Root-persistent mutable boxes keep identity, but parent rewrites must avoid
    // borrowing lifetime from an ephemeral target scope.
    main::ScopeRegion* saved_scope = interp.current_scope;
    interp.current_scope = interp.root_scope;
    defer interp.current_scope = saved_scope;

    BoundaryEnvCopyResult parent_result = copy_env_to_scope_inner_checked(env.parent, interp, depth + 1, active_ctx);
    if (!boundary_env_copy_result_ok(parent_result)) {
        return parent_result;
    }
    env.parent = parent_result.env;
    return boundary_env_copy_ok(env, depth);
}

fn BoundaryEnvCopyResult copy_env_materialize_frame(Env* env, Interp* interp, usz depth, PromotionContext* active_ctx) {
    BoundaryEnvCopyResult alloc_result = copy_env_alloc_frame(env, interp, depth);
    if (!boundary_env_copy_result_ok(alloc_result)) return alloc_result;
    Env* new_env = alloc_result.env;

    if (!copy_env_copy_bindings(env, new_env, interp, active_ctx)) {
        copy_env_cleanup_partial_frame(new_env);
        return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY, depth);
    }

    BoundaryEnvCopyResult parent_result = copy_env_to_scope_inner_checked(env.parent, interp, depth + 1, active_ctx);
    if (!boundary_env_copy_result_ok(parent_result)) {
        copy_env_cleanup_partial_frame(new_env);
        return parent_result;
    }
    new_env.parent = parent_result.env;
    if (env.hash_table != null) {
        new_env.build_hash_table();
    }
    main::scope_register_dtor(interp.current_scope, (void*)new_env, &scope_dtor_env);
    return boundary_env_copy_ok(new_env, depth);
}

fn BoundaryEnvCopyResult copy_env_to_scope_inner_checked(
    Env* env,
    Interp* interp,
    usz depth,
    PromotionContext* active_ctx
) {
    if (copy_env_is_terminal_frame(env, interp, depth)) {
        if (depth >= 256) {
            return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_DEPTH_LIMIT, depth);
        }
        return boundary_env_copy_ok(env, depth);
    }

    // Root-persistent mutable-box env nodes are kept as-is to preserve sharing
    // between JIT stack slots and closure captures.
    // Their parent chain still needs fixing up since it may point to child-scope envs.
    if (env.is_root_persistent_box()) {
        return copy_env_rewrite_persistent_parent(env, interp, depth, active_ctx);
    }
    return copy_env_materialize_frame(env, interp, depth, active_ctx);
}

/**
 * Copy an env chain into interp.current_scope (caller must set this to the target scope,
 * typically a closure's env_scope). Uses boundary copy routing for binding values.
 * Stops at global_env (already in root-scope lifetime). Skips root-persistent mutable-box env nodes.
 * Replaces deep_copy_env — allocates into any scope, not just root_scope.
 */
fn BoundaryEnvCopyResult copy_env_to_scope_checked(
    Env* env,
    Interp* interp,
    usz depth = 0,
    PromotionContext* ctx = null
) {
    PromotionContext* saved_active_ctx = interp.active_promotion_ctx;
    PromotionContext* active_ctx = ctx;
    bool owns_ctx = false;
    if (active_ctx == null) active_ctx = interp.active_promotion_ctx;
    if (depth == 0 && active_ctx == null) {
        // Keep boundary context in scope memory (not stack local) so JIT-driven
        // env-copy paths use the same deterministic region lifetime model.
        active_ctx = copy_env_begin_owned_promotion_ctx(interp);
        if (active_ctx == null) {
            return boundary_env_copy_fail(BOUNDARY_ENV_COPY_FAULT_PROMOTION_CTX_ALLOC, depth);
        }
        owns_ctx = true;
    }
    defer {
        if (owns_ctx) copy_env_end_owned_promotion_ctx(interp, active_ctx, saved_active_ctx);
    }
    return copy_env_to_scope_inner_checked(env, interp, depth, active_ctx);
}

// Continuations, effects, index — all handled by JIT (jit.c3)

// =============================================================================
