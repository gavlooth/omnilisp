module lisp;

import std::io;
import main;

// =============================================================================
// SECTION 2.4: FRAME PUSH/POP (promotion context + scoped copy)
// =============================================================================
fn void promotion_context_begin(Interp* interp, PromotionContext* ctx, usz budget = usz.max) {
    if (interp == null || ctx == null) return;
    interp.promotion_epoch_counter++;
    if (interp.promotion_epoch_counter == 0) interp.promotion_epoch_counter = 1;
    ctx.epoch = interp.promotion_epoch_counter;
    ctx.budget = budget;
    ctx.aborted = false;
    ctx.abort_reason = PROMOTION_ABORT_NONE;
    ctx.memo_head = null;
    ctx.prev = interp.active_promotion_ctx;
    promotion_context_reset_scope_chain_cache(ctx);
    interp.active_promotion_ctx = ctx;
}

fn void promotion_context_end(Interp* interp, PromotionContext* ctx) {
    if (interp == null || ctx == null) return;
    if (interp.active_promotion_ctx == ctx) {
        interp.active_promotion_ctx = ctx.prev;
    }
}

fn bool promotion_context_consume(PromotionContext* ctx, usz units = 1) @inline {
    if (ctx == null) return true;
    if (ctx.aborted) return false;
    if (ctx.budget == usz.max) return true;
    if (ctx.budget < units) {
        ctx.aborted = true;
        ctx.abort_reason = PROMOTION_ABORT_BUDGET_EXHAUSTED;
        return false;
    }
    ctx.budget -= units;
    return true;
}

fn Value* promotion_context_lookup(PromotionContext* ctx, Value* src) @inline {
    if (ctx == null || src == null) return null;
    PromotionMemoEntry* e = ctx.memo_head;
    while (e != null) {
        if (e.src == src) return e.dst;
        e = e.next;
    }
    return null;
}

fn void promotion_context_reset_scope_chain_cache(PromotionContext* ctx) @inline {
    if (ctx == null) return;
    ctx.scope_chain_cache_cursor = 0;
    for (usz i = 0; i < PROMOTION_CONTEXT_SCOPE_CHAIN_CACHE_SIZE; i++) {
        ctx.scope_chain_cache[i].ptr = null;
        ctx.scope_chain_cache[i].target_scope = null;
        ctx.scope_chain_cache[i].target_generation = 0;
        ctx.scope_chain_cache[i].target_escape_generation = 0;
        ctx.scope_chain_cache[i].pinned_generation = 0;
        ctx.scope_chain_cache[i].in_scope_chain = false;
        ctx.scope_chain_cache[i].used = false;
    }
}

fn bool promotion_context_lookup_scope_chain_cache(
    PromotionContext* ctx,
    void* ptr,
    main::ScopeRegion* target_scope,
    uint pinned_generation,
    bool* out_result
) @inline {
    if (ctx == null || ptr == null || target_scope == null || out_result == null) return false;
    uint target_generation = target_scope.generation;
    uint target_escape_generation = target_scope.escape_generation;
    foreach (PromotionContextScopeChainCacheEntry e : ctx.scope_chain_cache) {
        if (!e.used) continue;
        if (e.ptr != ptr) continue;
        if (e.target_scope != target_scope) continue;
        if (e.target_generation != target_generation) continue;
        if (e.target_escape_generation != target_escape_generation) continue;
        if (e.pinned_generation != pinned_generation) continue;
        *out_result = e.in_scope_chain;
        return true;
    }
    return false;
}

fn void promotion_context_remember_scope_chain_cache(
    PromotionContext* ctx,
    void* ptr,
    main::ScopeRegion* target_scope,
    uint pinned_generation,
    bool in_scope_chain
) @inline {
    if (ctx == null || ptr == null || target_scope == null) return;

    usz slot = ctx.scope_chain_cache_cursor;
    ctx.scope_chain_cache_cursor++;
    if (ctx.scope_chain_cache_cursor >= PROMOTION_CONTEXT_SCOPE_CHAIN_CACHE_SIZE) {
        ctx.scope_chain_cache_cursor = 0;
    }

    PromotionContextScopeChainCacheEntry* e = &ctx.scope_chain_cache[slot];
    e.ptr = ptr;
    e.target_scope = target_scope;
    e.target_generation = target_scope.generation;
    e.target_escape_generation = target_scope.escape_generation;
    e.pinned_generation = pinned_generation;
    e.in_scope_chain = in_scope_chain;
    e.used = true;
}

fn void promotion_context_remember(PromotionContext* ctx, Value* src, Value* dst, Interp* interp) {
    if (ctx == null || src == null || dst == null || interp == null || interp.current_scope == null) return;
    if (ctx.aborted) return;
    PromotionMemoEntry* e = (PromotionMemoEntry*)interp.current_scope.alloc(PromotionMemoEntry.sizeof);
    e.src = src;
    e.dst = dst;
    e.next = ctx.memo_head;
    ctx.memo_head = e;
}
