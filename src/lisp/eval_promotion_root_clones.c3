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

fn void boundary_cleanup_materialized_value(Value* src, Value* copied) @inline {
    if (copied == null || copied == src) return;
    if (copied.tag == CLOSURE) {
        scope_dtor_closure((void*)copied);
        return;
    }
    scope_dtor_value((void*)copied);
}

fn bool boundary_array_copy_value_seen_before(Array* dst, usz upto, Value* candidate) @inline {
    if (dst == null || dst.items == null || candidate == null) return false;
    for (usz i = 0; i < upto; i++) {
        if (dst.items[i] == candidate) return true;
    }
    return false;
}

fn void boundary_cleanup_partial_array_copy(Array* src, Array* dst, usz copied_items) {
    if (dst == null) return;
    if (dst.items != null && src != null && src.items != null) {
        if (copied_items > src.length) copied_items = src.length;
        for (usz i = 0; i < copied_items; i++) {
            Value* copied = dst.items[i];
            if (boundary_array_copy_value_seen_before(dst, i, copied)) continue;
            boundary_cleanup_materialized_value(src.items[i], copied);
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

fn void boundary_cleanup_partial_hashmap_copy(HashMap* src, HashMap* dst, uint processed_slots) {
    if (dst == null) return;
    if (dst.entries != null && src != null && src.entries != null) {
        if (processed_slots > src.capacity) processed_slots = src.capacity;
        for (uint i = 0; i < processed_slots; i++) {
            Value* copied_key = dst.entries[i].key;
            if (!boundary_hashmap_copy_value_seen_before(dst, i, copied_key)) {
                boundary_cleanup_materialized_value(src.entries[i].key, copied_key);
            }
            Value* copied_value = dst.entries[i].value;
            if (!boundary_hashmap_copy_value_seen_before(dst, i, copied_value, true)) {
                boundary_cleanup_materialized_value(src.entries[i].value, copied_value);
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
    bool cleanup_fallback = false
) {
    if (dst == null) return;
    g_method_table_partial_cleanup_call_count++;
    g_method_table_partial_cleanup_entry_total += copied_entries;
    if (dst.entries != null && src != null && src.entries != null) {
        if (copied_entries > src.entry_count) copied_entries = src.entry_count;
        for (usz i = 0; i < copied_entries; i++) {
            if (!boundary_method_table_impl_seen_before(dst, i, dst.entries[i].implementation)) {
                boundary_cleanup_materialized_value(src.entries[i].implementation, dst.entries[i].implementation);
            }
            method_signature_destroy_heap(&dst.entries[i].sig);
        }
        if (cleanup_fallback && !boundary_method_table_impl_seen_before(dst, copied_entries, dst.fallback)) {
            boundary_cleanup_materialized_value(src.fallback, dst.fallback);
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

    Value* result = interp.alloc_value_escape();
    result.tag = METHOD_TABLE;
    result.method_table_val = null;

    MethodTable* src_mt = src.method_table_val;
    if (src_mt == null) {
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
            boundary_cleanup_partial_method_table_copy(src_mt, dst_mt, copied_entries);
            return null;
        }
        Value* copied_impl = boundary_copy_to_parent_site_ctx(
            src_mt.entries[i].implementation,
            interp,
            COPY_SITE_PROMOTE_TO_ROOT,
            &copy_ctx
        );
        if (copied_impl != null && copied_impl.tag == ERROR) {
            boundary_cleanup_partial_method_table_copy(src_mt, dst_mt, copied_entries);
            return copied_impl;
        }
        dst_mt.entries[i].implementation = copied_impl;
        copied_entries++;
    }

    Value* copied_fallback = boundary_copy_to_parent_site_ctx(
        src_mt.fallback,
        interp,
        COPY_SITE_PROMOTE_TO_ROOT,
        &copy_ctx
    );
    if (copied_fallback != null && copied_fallback.tag == ERROR) {
        boundary_cleanup_partial_method_table_copy(src_mt, dst_mt, copied_entries);
        return copied_fallback;
    }
    dst_mt.fallback = copied_fallback;

    result.method_table_val = dst_mt;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
    return result;
}
