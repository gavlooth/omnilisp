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
        if (!method_signature_copy_to_heap(&dst_mt.entries[i].sig, &src_mt.entries[i].sig)) {
            method_table_free_partial_heap(dst_mt, copied_entries);
            return null;
        }
        Value* copied_impl = boundary_copy_to_parent_site_ctx(
            src_mt.entries[i].implementation,
            interp,
            COPY_SITE_PROMOTE_TO_ROOT,
            &copy_ctx
        );
        if (copied_impl != null && copied_impl.tag == ERROR) {
            method_table_free_partial_heap(dst_mt, copied_entries + 1);
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
        method_table_free_partial_heap(dst_mt, copied_entries);
        return copied_fallback;
    }
    dst_mt.fallback = copied_fallback;

    result.method_table_val = dst_mt;
    main::scope_register_dtor_escape(target_scope, (void*)result, &scope_dtor_value);
    return result;
}
