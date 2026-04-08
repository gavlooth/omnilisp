module lisp;

import std::core::mem;
import main;

fn Value* coroutine_ctor_type_raise(Interp* interp, char[] message) @inline {
    return raise_error_with_payload_names(interp, "type", "type/arg-mismatch", message, null);
}

// ============================================================================
// COROUTINE PRIMITIVES
// ============================================================================

struct CoroutineThunkState {
    Value*  thunk;
    Interp* interp;
}

fn void coroutine_thunk_entry(void* arg) {
    CoroutineThunkState* state = (CoroutineThunkState*)arg;
    Value* result = jit_apply_value(state.thunk, null, state.interp);
    main::g_current_stack_ctx.result = result;
}

fn CoroutineThunkState* coroutine_alloc_thunk_state(Value* thunk, Interp* interp) {
    CoroutineThunkState* state = (CoroutineThunkState*)mem::malloc(CoroutineThunkState.sizeof);
    if (state == null) return null;
    state.thunk = thunk;
    state.interp = interp;
    return state;
}

fn Value* prim_coroutine_p(Value*[] args, Env* env, Interp* interp) {
    if (args[0].tag == COROUTINE) return make_symbol(interp, interp.sym_true);
    return make_nil(interp);
}

fn Value* prim_coroutine_require_thunk(Value*[] args, Value** thunk_out, Interp* interp) {
    if (args.len < 1) return coroutine_ctor_type_raise(interp, "Coroutine: expected closure thunk");
    Value* thunk = args[0];
    if (!is_closure(thunk)) {
        return coroutine_ctor_type_raise(interp, "Coroutine: expected closure thunk");
    }
    *thunk_out = thunk;
    return null;
}

fn Value* prim_coroutine_prepare_thunk(Value* thunk, Interp* interp) {
    // Phase 4b: Promote thunk to root_scope so it survives call_scope release.
    // The coroutine may be resumed after the creating scope is freed.
    thunk = boundary_promote_to_root(thunk, interp);
    if (thunk != null && thunk.tag == CLOSURE) {
        // Precompile coroutine body before first execution on switched stack.
        jit_warm_expr_cache(thunk.closure_val.body, interp);
    }
    return thunk;
}

fn Value* prim_coroutine_create_ctx(Value* thunk, main::StackCtx** ctx_out, Interp* interp) {
    main::StackCtx* ctx = main::stack_ctx_create(&interp.stack_ctx_pool);
    if (ctx == null) return raise_error(interp, "coroutine: failed to create context");

    // Store thunk state on the context's user_data (lives as long as the context)
    CoroutineThunkState* state = coroutine_alloc_thunk_state(thunk, interp);
    if (state == null) {
        main::stack_ctx_destroy(ctx, &interp.stack_ctx_pool);
        return raise_error(interp, "coroutine: out of memory");
    }
    ctx.user_data = state;

    main::stack_ctx_init(ctx, &coroutine_thunk_entry, state);
    *ctx_out = ctx;
    return null;
}

fn Value* prim_coroutine(Value*[] args, Env* env, Interp* interp) {
    Value* thunk = null;
    Value* parse_err = prim_coroutine_require_thunk(args, &thunk, interp);
    if (parse_err != null) return parse_err;

    thunk = prim_coroutine_prepare_thunk(thunk, interp);

    main::StackCtx* ctx = null;
    Value* ctx_err = prim_coroutine_create_ctx(thunk, &ctx, interp);
    if (ctx_err != null) return ctx_err;

    return make_coroutine(interp, ctx);
}

fn bool coroutine_ctx_matches_interp(main::StackCtx* ctx, Interp* interp) @inline {
    if (ctx == null || interp == null) return false;
    if (ctx.owner_pool != &interp.stack_ctx_pool) return false;
    CoroutineThunkState* state = (CoroutineThunkState*)ctx.user_data;
    if (state != null && state.interp != interp) return false;
    return true;
}

fn void coroutine_ctx_cleanup(Value* coroutine_val, main::StackCtx* ctx, Interp* interp) {
    if (ctx.user_data != null) {
        mem::free(ctx.user_data);
        ctx.user_data = null;
    }
    coroutine_val.coroutine_val = null;
    main::StackPool* owner_pool = ctx.owner_pool;
    if (owner_pool == null) owner_pool = &interp.stack_ctx_pool;
    main::stack_ctx_destroy(ctx, owner_pool);
}

fn void coroutine_clear_yield_boundary_state(Interp* interp) @inline {
    interp.yield_value = null;
    interp.yield_scope = null;
}
