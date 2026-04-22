module lisp;

import std::io;
import main;

bool g_boundary_force_destination_error_escape_alloc_fail = false;

struct BoundaryDestinationBuilderMemoScope {
    PromotionMemoEntry* saved_memo_head;
    usz saved_scope_chain_cache_cursor;
    PromotionContextScopeChainCacheEntry[ PROMOTION_CONTEXT_SCOPE_CHAIN_CACHE_SIZE ] saved_scope_chain_cache;
    bool active;
}

fn BoundaryDestinationBuilderMemoScope boundary_destination_builder_memo_scope_begin(PromotionContext* ctx) @inline {
    BoundaryDestinationBuilderMemoScope saved = {
        .saved_memo_head = null,
        .saved_scope_chain_cache_cursor = 0,
        .active = ctx != null,
    };
    if (ctx == null) return saved;
    saved.saved_memo_head = ctx.memo_head;
    saved.saved_scope_chain_cache_cursor = ctx.scope_chain_cache_cursor;
    for (usz i = 0; i < PROMOTION_CONTEXT_SCOPE_CHAIN_CACHE_SIZE; i++) {
        saved.saved_scope_chain_cache[i] = ctx.scope_chain_cache[i];
    }
    return saved;
}

fn void boundary_destination_builder_memo_scope_end(
    PromotionContext* ctx,
    BoundaryDestinationBuilderMemoScope saved
) @inline {
    // Destination builders allocate through a temporary build scope. Any memo
    // entries or scope-chain cache probes remembered while routing nested
    // children are builder-local and must not survive after the builder
    // returns or aborts.
    if (ctx == null || !saved.active) return;
    ctx.memo_head = saved.saved_memo_head;
    ctx.scope_chain_cache_cursor = saved.saved_scope_chain_cache_cursor;
    for (usz i = 0; i < PROMOTION_CONTEXT_SCOPE_CHAIN_CACHE_SIZE; i++) {
        ctx.scope_chain_cache[i] = saved.saved_scope_chain_cache[i];
    }
}

fn Value* boundary_promote_to_escape_ctx(
    Value* routed,
    Interp* interp,
    PromotionContext* ctx = null
) @inline {
    if (ctx == null || interp == null || interp.active_promotion_ctx == ctx) {
        return boundary_promote_to_escape(routed, interp);
    }

    PromotionContext* saved_active_ctx = interp.active_promotion_ctx;
    interp.active_promotion_ctx = ctx;
    defer interp.active_promotion_ctx = saved_active_ctx;
    return boundary_promote_to_escape(routed, interp);
}

fn Value* boundary_fallback_disallowed_error(Interp* interp, BoundaryCommitFault fault_code) @noinline {
    char[192] msg_buf;
    if (try msg = io::bprintf(
        &msg_buf,
        "boundary: traversal fallback disallowed for commit path (%s)",
        (ZString)boundary_commit_fault_name(fault_code)
    )) {
        return boundary_raise(interp, "boundary/copy-fault", (String)msg);
    }
    return boundary_raise(interp, "boundary/invalid-state", "boundary: traversal fallback disallowed for commit path");
}

fn Value* boundary_destination_promote_routed_escape(
    Value* routed,
    Interp* interp,
    main::ScopeRegion* target_scope,
    PromotionContext* ctx = null
) @inline {
    if (routed == null) return null;
    if (ctx != null && ctx.aborted) return routed;
    if (routed.scope_gen == target_scope.escape_generation) return routed;
    return boundary_promote_to_escape_ctx(routed, interp, ctx);
}

fn Value* boundary_destination_bubble_child_fault(
    Value* source_child,
    Value* promoted_child,
    Value* fallback_value,
    Interp* interp,
    main::ScopeRegion* target_scope,
    PromotionContext* ctx = null
) {
    if (source_child == null || source_child.tag == ERROR) return null;
    if (promoted_child == null || promoted_child.tag != ERROR) return null;

    Value* destination_error = boundary_build_destination_error_escape(
        promoted_child,
        interp,
        target_scope,
        ctx
    );
    if (destination_error != null &&
        destination_error.tag == ERROR &&
        destination_error.scope_gen == target_scope.escape_generation) {
        return destination_error;
    }

    boundary_mark_active_promotion_ctx_aborted(interp, ctx);
    return fallback_value;
}

fn bool boundary_destination_iterator_inner_needs_detach(
    Value* inner,
    main::ScopeRegion* target_scope
) @inline {
    if (inner == null || target_scope == null) return false;
    if (boundary_ptr_in_scope((void*)inner, target_scope) &&
        inner.scope_gen == target_scope.generation) {
        return true;
    }
    if (inner.tag == CLOSURE &&
        boundary_closure_payload_in_scope(inner, target_scope)) {
        return true;
    }
    return false;
}

fn Value* boundary_destination_detach_iterator_inner(
    Value* inner,
    Interp* interp,
    main::ScopeRegion* target_scope,
    PromotionContext* ctx = null
) {
    if (!boundary_destination_iterator_inner_needs_detach(inner, target_scope)) {
        return inner;
    }

    main::ScopeRegion* saved_releasing_scope = interp.releasing_scope;
    interp.releasing_scope = target_scope;
    defer interp.releasing_scope = saved_releasing_scope;
    BoundaryCopyResult copy_result = boundary_copy_to_parent_site_ctx_checked(
        inner,
        interp,
        COPY_SITE_GENERIC,
        ctx
    );
    if (!boundary_copy_result_ok(copy_result)) {
        return boundary_raise(interp, "boundary/copy-fault", (String)boundary_copy_fault_message(copy_result.fault_code));
    }
    return copy_result.value;
}

fn main::ScopeRegion* boundary_destination_build_scope_begin(
    Interp* interp,
    main::ScopeRegion* target_scope,
    main::ScopeRegion* releasing_scope
) @inline {
    main::ScopeRegion* build_scope = main::scope_create(target_scope);
    if (build_scope == null) return null;
    interp.current_scope = build_scope;
    interp.releasing_scope = releasing_scope;
    return build_scope;
}

fn void boundary_destination_build_scope_abort(main::ScopeRegion** build_scope_io) @inline {
    if (build_scope_io == null) return;
    if (*build_scope_io == null) return;
    main::scope_release(*build_scope_io);
    *build_scope_io = null;
}

fn void boundary_destination_build_scope_commit(
    main::ScopeRegion** build_scope_io,
    main::ScopeRegion* target_scope
) @inline {
    if (build_scope_io == null) return;
    main::ScopeRegion* build_scope = *build_scope_io;
    if (build_scope == null || target_scope == null) return;
    boundary_destination_build_scope_splice(target_scope, build_scope);
    *build_scope_io = null;
}
