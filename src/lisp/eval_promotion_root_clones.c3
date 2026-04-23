module lisp;

import std::core::mem;
import main;

usz g_method_table_partial_cleanup_call_count = 0;
usz g_method_table_partial_cleanup_entry_total = 0;

fn void method_table_partial_cleanup_reset() {
    g_method_table_partial_cleanup_call_count = 0;
    g_method_table_partial_cleanup_entry_total = 0;
}

fn usz method_table_partial_cleanup_calls() @inline {
    return g_method_table_partial_cleanup_call_count;
}

fn usz method_table_partial_cleanup_entries() @inline {
    return g_method_table_partial_cleanup_entry_total;
}

fn void method_table_free_partial_heap(MethodTable* mt, usz copied_entries) {
    if (mt == null) return;
    g_method_table_partial_cleanup_call_count++;
    g_method_table_partial_cleanup_entry_total += copied_entries;
    if (mt.entries != null) {
        usz limit = copied_entries;
        if (limit > mt.entry_count) limit = mt.entry_count;
        for (usz i = 0; i < limit; i++) {
            method_signature_destroy_heap(&mt.entries[i].sig);
        }
        mem::free(mt.entries);
        mt.entries = null;
    }
    mem::free(mt);
}

fn void boundary_cancel_materialized_dtor(
    Value* copied,
    main::ScopeRegion* target_scope,
    bool escape_lane
) @inline {
    if (copied == null || target_scope == null) return;
    if (copied.tag == CLOSURE) {
        if (escape_lane) {
            main::scope_cancel_dtor_escape(target_scope, (void*)copied, &scope_dtor_closure);
        } else {
            main::scope_cancel_dtor(target_scope, (void*)copied, &scope_dtor_closure);
        }
        return;
    }
    if (escape_lane) {
        main::scope_cancel_dtor_escape(target_scope, (void*)copied, &scope_dtor_value);
    } else {
        main::scope_cancel_dtor(target_scope, (void*)copied, &scope_dtor_value);
    }
}

fn void boundary_cleanup_materialized_cons(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope,
    bool escape_lane
) @inline {
    if (src == null || copied == null || src.tag != CONS || copied.tag != CONS) return;
    boundary_cleanup_materialized_value(src.cons_val.car, copied.cons_val.car, target_scope, escape_lane);
    if (src.cons_val.cdr != src.cons_val.car ||
        copied.cons_val.cdr != copied.cons_val.car) {
        boundary_cleanup_materialized_value(src.cons_val.cdr, copied.cons_val.cdr, target_scope, escape_lane);
    }
}

fn void boundary_cleanup_materialized_array(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope,
    bool escape_lane
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
        boundary_cleanup_materialized_value(src_arr.items[i], copied_item, target_scope, escape_lane);
    }
}

fn void boundary_cleanup_materialized_hashmap(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope,
    bool escape_lane
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
            boundary_cleanup_materialized_value(src_map.entries[i].key, copied_key, target_scope, escape_lane);
        }
        Value* copied_value = copied_map.entries[i].value;
        if (!boundary_hashmap_copy_value_seen_before(copied_map, i, copied_value, true)) {
            boundary_cleanup_materialized_value(src_map.entries[i].value, copied_value, target_scope, escape_lane);
        }
    }
}

fn void boundary_cleanup_materialized_method_table(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope,
    bool escape_lane
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
        boundary_cleanup_materialized_value(src_mt.entries[i].implementation, copied_impl, target_scope, escape_lane);
    }
    if (!boundary_method_table_impl_seen_before(copied_mt, limit, copied_mt.fallback)) {
        boundary_cleanup_materialized_value(src_mt.fallback, copied_mt.fallback, target_scope, escape_lane);
    }
}

fn void boundary_cleanup_materialized_partial_prim(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope,
    bool escape_lane
) @inline {
    if (src == null || copied == null || src.tag != PARTIAL_PRIM || copied.tag != PARTIAL_PRIM) return;
    boundary_cleanup_materialized_value(
        src.partial_val.first_arg,
        copied.partial_val.first_arg,
        target_scope,
        escape_lane
    );
    if (src.partial_val.second_arg != src.partial_val.first_arg ||
        copied.partial_val.second_arg != copied.partial_val.first_arg) {
        boundary_cleanup_materialized_value(
            src.partial_val.second_arg,
            copied.partial_val.second_arg,
            target_scope,
            escape_lane
        );
    }
}

fn void boundary_cleanup_materialized_iterator(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope,
    bool escape_lane
) @inline {
    if (src == null || copied == null || src.tag != ITERATOR || copied.tag != ITERATOR) return;
    boundary_cleanup_materialized_value(
        src.iterator_val,
        copied.iterator_val,
        target_scope,
        escape_lane
    );
}

fn void boundary_cleanup_materialized_value(
    Value* src,
    Value* copied,
    main::ScopeRegion* target_scope,
    bool escape_lane = false
) @inline {
    if (copied == null || copied == src) return;
    if (src != null &&
        copied != null &&
        src.tag == CONS &&
        copied.tag == CONS) {
        boundary_cleanup_materialized_cons(src, copied, target_scope, escape_lane);
        return;
    }
    if (src != null &&
        copied != null &&
        src.tag == ARRAY &&
        copied.tag == ARRAY) {
        boundary_cleanup_materialized_array(src, copied, target_scope, escape_lane);
        boundary_cancel_materialized_dtor(copied, target_scope, escape_lane);
        scope_dtor_value((void*)copied);
        return;
    }
    if (src != null &&
        copied != null &&
        (src.tag == HASHMAP || src.tag == SET) &&
        copied.tag == src.tag) {
        boundary_cleanup_materialized_hashmap(src, copied, target_scope, escape_lane);
        boundary_cancel_materialized_dtor(copied, target_scope, escape_lane);
        scope_dtor_value((void*)copied);
        return;
    }
    if (src != null &&
        copied != null &&
        src.tag == METHOD_TABLE &&
        copied.tag == METHOD_TABLE) {
        boundary_cleanup_materialized_method_table(src, copied, target_scope, escape_lane);
        boundary_cancel_materialized_dtor(copied, target_scope, escape_lane);
        scope_dtor_value((void*)copied);
        return;
    }
    if (src != null &&
        copied != null &&
        src.tag == PARTIAL_PRIM &&
        copied.tag == PARTIAL_PRIM) {
        boundary_cleanup_materialized_partial_prim(src, copied, target_scope, escape_lane);
        boundary_cancel_materialized_dtor(copied, target_scope, escape_lane);
        scope_dtor_value((void*)copied);
        return;
    }
    if (src != null &&
        copied != null &&
        src.tag == ITERATOR &&
        copied.tag == ITERATOR) {
        boundary_cleanup_materialized_iterator(src, copied, target_scope, escape_lane);
        boundary_cancel_materialized_dtor(copied, target_scope, escape_lane);
        scope_dtor_value((void*)copied);
        return;
    }
    if (src != null &&
        copied != null &&
        src.tag == TENSOR &&
        copied.tag == TENSOR) {
        boundary_cleanup_materialized_tensor(src, copied, target_scope, escape_lane);
        boundary_cancel_materialized_dtor(copied, target_scope, escape_lane);
        scope_dtor_value((void*)copied);
        return;
    }
    if (copied.tag == CLOSURE) {
        boundary_cancel_materialized_dtor(copied, target_scope, escape_lane);
        scope_dtor_closure((void*)copied);
        return;
    }
    if (copied.tag == CONTINUATION && copied.continuation_boundary_owned) {
        boundary_cancel_materialized_dtor(copied, target_scope, escape_lane);
        scope_dtor_value((void*)copied);
        return;
    }
    boundary_cancel_materialized_dtor(copied, target_scope, escape_lane);
    scope_dtor_value((void*)copied);
}

fn bool boundary_array_copy_value_seen_before(Array* dst, usz upto, Value* candidate) @inline {
    if (dst == null || dst.items == null || candidate == null) return false;
    for (usz i = 0; i < upto; i++) {
        if (dst.items[i] == candidate) return true;
    }
    return false;
}

fn void boundary_cleanup_partial_array_copy(
    Array* src,
    Array* dst,
    usz copied_items,
    main::ScopeRegion* target_scope = null,
    bool escape_lane = false
) {
    if (dst == null) return;
    if (dst.items != null && src != null && src.items != null) {
        if (copied_items > src.length) copied_items = src.length;
        for (usz i = 0; i < copied_items; i++) {
            Value* copied = dst.items[i];
            if (boundary_array_copy_value_seen_before(dst, i, copied)) continue;
            boundary_cleanup_materialized_value(src.items[i], copied, target_scope, escape_lane);
        }
        mem::free(dst.items);
        dst.items = null;
    }
    mem::free(dst);
}

fn bool boundary_hashmap_copy_value_seen_before(
    HashMap* dst,
    uint upto_index,
    Value* candidate,
    bool include_current_key = false
) @inline {
    if (dst == null || dst.entries == null || candidate == null) return false;
    for (uint j = 0; j < upto_index; j++) {
        if (dst.entries[j].key == candidate || dst.entries[j].value == candidate) return true;
    }
    return include_current_key && dst.entries[upto_index].key == candidate;
}

fn void boundary_cleanup_partial_hashmap_copy(
    HashMap* src,
    HashMap* dst,
    uint processed_slots,
    main::ScopeRegion* target_scope = null,
    bool escape_lane = false
) {
    if (dst == null) return;
    if (dst.entries != null && src != null && src.entries != null) {
        if (processed_slots > src.capacity) processed_slots = src.capacity;
        for (uint i = 0; i < processed_slots; i++) {
            Value* copied_key = dst.entries[i].key;
            if (!boundary_hashmap_copy_value_seen_before(dst, i, copied_key)) {
                boundary_cleanup_materialized_value(src.entries[i].key, copied_key, target_scope, escape_lane);
            }
            Value* copied_value = dst.entries[i].value;
            if (!boundary_hashmap_copy_value_seen_before(dst, i, copied_value, true)) {
                boundary_cleanup_materialized_value(src.entries[i].value, copied_value, target_scope, escape_lane);
            }
        }
        mem::free(dst.entries);
        dst.entries = null;
    }
    mem::free(dst);
}

fn bool boundary_method_table_impl_seen_before(MethodTable* dst, usz upto, Value* candidate) @inline {
    if (dst == null || dst.entries == null || candidate == null) return false;
    for (usz i = 0; i < upto; i++) {
        if (dst.entries[i].implementation == candidate) return true;
    }
    return false;
}

fn void boundary_cleanup_partial_method_table_copy(
    MethodTable* src,
    MethodTable* dst,
    usz copied_entries,
    bool cleanup_fallback = false,
    main::ScopeRegion* target_scope = null,
    bool escape_lane = false
) {
    if (dst == null) return;
    g_method_table_partial_cleanup_call_count++;
    g_method_table_partial_cleanup_entry_total += copied_entries;
    if (dst.entries != null && src != null && src.entries != null) {
        if (copied_entries > src.entry_count) copied_entries = src.entry_count;
        for (usz i = 0; i < copied_entries; i++) {
            if (!boundary_method_table_impl_seen_before(dst, i, dst.entries[i].implementation)) {
                boundary_cleanup_materialized_value(src.entries[i].implementation, dst.entries[i].implementation, target_scope, escape_lane);
            }
            method_signature_destroy_heap(&dst.entries[i].sig);
        }
        if (cleanup_fallback && !boundary_method_table_impl_seen_before(dst, copied_entries, dst.fallback)) {
            boundary_cleanup_materialized_value(src.fallback, dst.fallback, target_scope, escape_lane);
        }
        mem::free(dst.entries);
        dst.entries = null;
    }
    mem::free(dst);
}

fn Value* root_store_clone_method_table_to_scope(
    Value* src,
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) {
    if (src == null || interp == null || target_scope == null) return null;

    BoundarySession session = boundary_session_begin(interp);
    defer boundary_session_end(interp, &session);
    interp.current_scope = target_scope;
    interp.releasing_scope = releasing_scope;
    PromotionContext copy_ctx = {};
    boundary_promotion_context_begin(interp, &copy_ctx);
    defer boundary_promotion_context_end(interp, &copy_ctx);

    MethodTable* src_mt = src.method_table_val;
    if (src_mt == null) {
        Value* result = boundary_alloc_value_in_current_escape(interp);
        if (result == null) return null;
        result.tag = METHOD_TABLE;
        result.method_table_val = null;
        main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
        return result;
    }

    MethodTable* dst_mt = (MethodTable*)mem::malloc(MethodTable.sizeof);
    if (dst_mt == null) return null;
    dst_mt.name = src_mt.name;
    dst_mt.entry_count = src_mt.entry_count;
    dst_mt.capacity = src_mt.capacity;
    if (dst_mt.capacity < dst_mt.entry_count) dst_mt.capacity = dst_mt.entry_count;
    dst_mt.entries = null;
    dst_mt.fallback = null;

    if (dst_mt.capacity > 0) {
        if (dst_mt.capacity > usz.max / MethodEntry.sizeof) {
            mem::free(dst_mt);
            return null;
        }
        dst_mt.entries = (MethodEntry*)mem::malloc(MethodEntry.sizeof * dst_mt.capacity);
        if (dst_mt.entries == null) {
            mem::free(dst_mt);
            return null;
        }
    }

    usz copied_entries = 0;
    for (usz i = 0; i < dst_mt.entry_count; i++) {
        method_signature_init_empty(&dst_mt.entries[i].sig);
        dst_mt.entries[i].implementation = null;
        if (!method_signature_copy_to_heap(&dst_mt.entries[i].sig, &src_mt.entries[i].sig)) {
            boundary_cleanup_partial_method_table_copy(src_mt, dst_mt, copied_entries, false, target_scope);
            return null;
        }
        BoundaryCopyResult impl_copy = boundary_copy_to_parent_site_ctx_checked(
            src_mt.entries[i].implementation,
            interp,
            COPY_SITE_PROMOTE_TO_ROOT,
            &copy_ctx
        );
        Value* copied_impl = null;
        if (!boundary_copy_result_ok(impl_copy)) {
            copied_impl = boundary_raise(interp, "boundary/copy-fault", (String)boundary_copy_fault_message(impl_copy.fault_code));
        } else {
            copied_impl = impl_copy.value;
        }
        if (copied_impl != null && copied_impl.tag == ERROR) {
            boundary_cleanup_partial_method_table_copy(src_mt, dst_mt, copied_entries, false, target_scope);
            return copied_impl;
        }
        dst_mt.entries[i].implementation = copied_impl;
        copied_entries++;
    }

    BoundaryCopyResult fallback_copy = boundary_copy_to_parent_site_ctx_checked(
        src_mt.fallback,
        interp,
        COPY_SITE_PROMOTE_TO_ROOT,
        &copy_ctx
    );
    Value* copied_fallback = null;
    if (!boundary_copy_result_ok(fallback_copy)) {
        copied_fallback = boundary_raise(interp, "boundary/copy-fault", (String)boundary_copy_fault_message(fallback_copy.fault_code));
    } else {
        copied_fallback = fallback_copy.value;
    }
    if (copied_fallback != null && copied_fallback.tag == ERROR) {
        boundary_cleanup_partial_method_table_copy(src_mt, dst_mt, copied_entries, false, target_scope);
        return copied_fallback;
    }
    dst_mt.fallback = copied_fallback;

    Value* result = boundary_alloc_value_in_current_escape(interp);
    if (result == null) {
        boundary_cleanup_partial_method_table_copy(
            src_mt,
            dst_mt,
            dst_mt.entry_count,
            true,
            target_scope
        );
        return null;
    }
    result.tag = METHOD_TABLE;
    result.method_table_val = dst_mt;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
    return result;
}
