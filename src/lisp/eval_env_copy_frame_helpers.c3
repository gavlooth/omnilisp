module lisp;

import std::io;
import main;

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
    dst.owner_scope = interp.current_scope;
    dst.owner_escape_lane = false;

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
    main::ScopeRegion* target_scope = interp.current_scope;
    for (usz i = 0; i < src.binding_count; i++) {
        dst.bindings[i].name = src.bindings[i].name;
        if (copy_env_binding_trace_enabled()) {
            Value* binding_value = src.bindings[i].value;
            io::printfn(
                "OMNI_ENV_COPY_TRACE frame=%p depth_count=%d index=%d name=%d value=%p in_current=%d in_releasing=%d in_root=%d src_scope_gen=%d target_gen=%d releasing_gen=%d",
                (void*)src,
                (int)src.binding_count,
                (int)i,
                (int)src.bindings[i].name,
                (void*)binding_value,
                binding_value != null && interp.current_scope != null && boundary_ptr_in_scope((void*)binding_value, interp.current_scope) ? 1 : 0,
                binding_value != null && interp.releasing_scope != null && boundary_ptr_in_scope((void*)binding_value, interp.releasing_scope) ? 1 : 0,
                binding_value != null && interp.root_scope != null && boundary_ptr_in_scope((void*)binding_value, interp.root_scope) ? 1 : 0,
                (int)src.scope_gen,
                interp.current_scope != null ? (int)interp.current_scope.generation : -1,
                interp.releasing_scope != null ? (int)interp.releasing_scope.generation : -1
            );
        }
        Value* copied = copy_env_binding_value(src.bindings[i].value, interp, ctx);
        if (copy_env_value_copy_failure(src.bindings[i].value, copied)) {
            copy_env_rollback_materialized_bindings(src, dst, target_scope, i);
            return false;
        }
        dst.bindings[i].value = copied;
    }
    return true;
}

fn bool copy_env_is_materialized_target_value(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope
) @inline {
    if (src == null || copied == null || copied == src || target_scope == null) return false;
    return boundary_ptr_in_scope((void*)copied, target_scope);
}

fn bool copy_env_iterator_payload_was_materialized(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope
) @inline {
    if (src == null || copied == null || src.tag != ITERATOR || copied.tag != ITERATOR) return false;
    if (target_scope == null) return false;
    Value* src_inner = src.iterator_val;
    Value* copied_inner = copied.iterator_val;
    if (copied_inner == null || copied_inner == src_inner) return false;
    return boundary_ptr_in_scope((void*)copied_inner, target_scope);
}

fn void copy_env_rollback_iterator_payload(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope
) @inline {
    if (!copy_env_iterator_payload_was_materialized(src, copied, target_scope)) return;
    Value* inner = copied.iterator_val;
    Value* src_inner = src.iterator_val;
    if (src_inner == null || inner == null) return;
    copy_env_rollback_materialized_value(src_inner, inner, target_scope);
}

fn void copy_env_rollback_materialized_cons(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope
) @inline {
    if (src == null || copied == null || src.tag != CONS || copied.tag != CONS) return;
    copy_env_rollback_materialized_value(
        src.cons_val.car,
        copied.cons_val.car,
        target_scope
    );
    if (src.cons_val.cdr != src.cons_val.car ||
        copied.cons_val.cdr != copied.cons_val.car) {
        copy_env_rollback_materialized_value(
            src.cons_val.cdr,
            copied.cons_val.cdr,
            target_scope
        );
    }
}

fn void copy_env_rollback_materialized_array(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope
) @inline {
    if (src == null || copied == null || src.tag != ARRAY || copied.tag != ARRAY) return;
    Array* src_arr = src.array_val;
    Array* copied_arr = copied.array_val;
    if (src_arr == null || copied_arr == null || src_arr.items == null || copied_arr.items == null) return;
    usz limit = src_arr.length;
    if (copied_arr.length < limit) limit = copied_arr.length;
    for (usz i = 0; i < limit; i++) {
        Value* copied_item = copied_arr.items[i];
        if (boundary_array_copy_value_seen_before(copied_arr, i, copied_item)) continue;
        copy_env_rollback_materialized_value(
            src_arr.items[i],
            copied_item,
            target_scope
        );
    }
}

fn void copy_env_rollback_materialized_hashmap(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope
) @inline {
    if (src == null || copied == null) return;
    if ((src.tag != HASHMAP && src.tag != SET) || copied.tag != src.tag) return;
    HashMap* src_map = src.hashmap_val;
    HashMap* copied_map = copied.hashmap_val;
    if (src_map == null || copied_map == null || src_map.entries == null || copied_map.entries == null) return;
    uint limit = src_map.capacity;
    if (copied_map.capacity < limit) limit = copied_map.capacity;
    for (uint i = 0; i < limit; i++) {
        Value* copied_key = copied_map.entries[i].key;
        if (!boundary_hashmap_copy_value_seen_before(copied_map, i, copied_key)) {
            copy_env_rollback_materialized_value(
                src_map.entries[i].key,
                copied_key,
                target_scope
            );
        }
        Value* copied_value = copied_map.entries[i].value;
        if (!boundary_hashmap_copy_value_seen_before(copied_map, i, copied_value, true)) {
            copy_env_rollback_materialized_value(
                src_map.entries[i].value,
                copied_value,
                target_scope
            );
        }
    }
}

fn void copy_env_rollback_materialized_method_table(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope
) @inline {
    if (src == null || copied == null || src.tag != METHOD_TABLE || copied.tag != METHOD_TABLE) return;
    MethodTable* src_mt = src.method_table_val;
    MethodTable* copied_mt = copied.method_table_val;
    if (src_mt == null || copied_mt == null || src_mt.entries == null || copied_mt.entries == null) return;
    usz limit = src_mt.entry_count;
    if (copied_mt.entry_count < limit) limit = copied_mt.entry_count;
    for (usz i = 0; i < limit; i++) {
        Value* copied_impl = copied_mt.entries[i].implementation;
        if (boundary_method_table_impl_seen_before(copied_mt, i, copied_impl)) continue;
        copy_env_rollback_materialized_value(
            src_mt.entries[i].implementation,
            copied_impl,
            target_scope
        );
    }
    if (!boundary_method_table_impl_seen_before(copied_mt, limit, copied_mt.fallback)) {
        copy_env_rollback_materialized_value(
            src_mt.fallback,
            copied_mt.fallback,
            target_scope
        );
    }
}

fn void copy_env_cancel_materialized_target_dtor(
    Value* copied,
    main::ScopeRegion* target_scope
) @inline {
    if (copied == null || target_scope == null) return;
    if (copied.tag == CLOSURE) {
        main::scope_cancel_dtor(target_scope, (void*)copied, &scope_dtor_closure);
        return;
    }
    main::scope_cancel_dtor(target_scope, (void*)copied, &scope_dtor_value);
}

fn void copy_env_rollback_materialized_value(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope
) @inline {
    if (!copy_env_is_materialized_target_value(src, copied, target_scope)) return;
    // Roll back nested closure-retain side effects produced by iterator-copy.
    copy_env_rollback_iterator_payload(src, copied, target_scope);
    if (src != null &&
        copied != null &&
        src.tag == PARTIAL_PRIM &&
        copied.tag == PARTIAL_PRIM) {
        copy_env_rollback_materialized_value(
            src.partial_val.first_arg,
            copied.partial_val.first_arg,
            target_scope
        );
        if (src.partial_val.second_arg != src.partial_val.first_arg ||
            copied.partial_val.second_arg != copied.partial_val.first_arg) {
            copy_env_rollback_materialized_value(
                src.partial_val.second_arg,
                copied.partial_val.second_arg,
                target_scope
            );
        }
        return;
    }
    if (src != null &&
        copied != null &&
        src.tag == CONS &&
        copied.tag == CONS) {
        copy_env_rollback_materialized_cons(src, copied, target_scope);
        return;
    }
    if (src != null &&
        copied != null &&
        src.tag == ARRAY &&
        copied.tag == ARRAY) {
        copy_env_rollback_materialized_array(src, copied, target_scope);
        copy_env_cancel_materialized_target_dtor(copied, target_scope);
        scope_dtor_value((void*)copied);
        return;
    }
    if (src != null &&
        copied != null &&
        (src.tag == HASHMAP || src.tag == SET) &&
        copied.tag == src.tag) {
        copy_env_rollback_materialized_hashmap(src, copied, target_scope);
        copy_env_cancel_materialized_target_dtor(copied, target_scope);
        scope_dtor_value((void*)copied);
        return;
    }
    if (src != null &&
        copied != null &&
        src.tag == METHOD_TABLE &&
        copied.tag == METHOD_TABLE) {
        copy_env_rollback_materialized_method_table(src, copied, target_scope);
        copy_env_cancel_materialized_target_dtor(copied, target_scope);
        scope_dtor_value((void*)copied);
        return;
    }
    if (src != null &&
        copied != null &&
        src.tag == CONTINUATION &&
        copied.tag == CONTINUATION &&
        copied.continuation_boundary_owned) {
        copy_env_cancel_materialized_target_dtor(copied, target_scope);
        scope_dtor_value((void*)copied);
        return;
    }
    if (copied.tag == CLOSURE) {
        copy_env_cancel_materialized_target_dtor(copied, target_scope);
        scope_dtor_closure((void*)copied);
        return;
    }
    copy_env_cancel_materialized_target_dtor(copied, target_scope);
    scope_dtor_value((void*)copied);
}

fn void copy_env_rollback_materialized_bindings(
    Env* src,
    Env* dst,
    main::ScopeRegion* target_scope,
    usz count
) @inline {
    if (src == null || dst == null || target_scope == null) return;
    if (count > src.binding_count) count = src.binding_count;
    for (usz i = 0; i < count; i++) {
        copy_env_rollback_materialized_value(
            src.bindings[i].value,
            dst.bindings[i].value,
            target_scope
        );
    }
}

fn bool copy_env_is_terminal_frame(Env* env, Interp* interp, usz depth) @inline {
    if (env == null || env == interp.global_env) return true;
    return depth >= 256;
}

fn bool copy_env_parent_needs_rewrite(
    Env* parent,
    main::ScopeRegion* releasing_scope,
    main::ScopeRegion* root_scope
) @inline {
    if (parent == null) return false;
    if (root_scope != null && main::is_in_scope((void*)parent, root_scope)) return false;
    // Root-persistent mutable boxes must not retain any non-root transient env
    // parent, even when that parent was allocated in an earlier child scope and
    // is no longer in the immediate releasing scope.
    (void)releasing_scope;
    return true;
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
