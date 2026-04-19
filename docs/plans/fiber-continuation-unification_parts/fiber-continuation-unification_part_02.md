# fiber-continuation-unification Part 02

Source: `docs/plans/fiber-continuation-unification.md`

#### 3.1.6 Verification

Run all 927 tests with `use_fiber_continuations = true`. The critical tests are:

- "shift aborts" (line 1209)
- "shift k resumes" (line 1210)
- "shift discard k" (line 1508)
- "shift k mul" (line 1509)
- "shift use k result" (line 1511)
- "multi-shift sum" (line 1690)
- "multi-shot k twice" (line 1693)
- "multi-shot k thrice" (line 1694)
- "multi-shot let bindings" (line 1695)
- "multi-shot conditional" (line 1696)
- "TCO in reset body" (line 1705)
- "TCO tail-recursive k use" (line 1711)

If any fail, set `use_fiber_continuations = false` to verify existing path still works, then debug the fiber path.

---

### PHASE 2: Wire Fibers into handle/signal/resolve — COMPLETE

**Goal**: Implement fiber-based algebraic effects. This is the most complex phase.
**Status**: Complete. 986 tests pass (0 failures). Key design decisions:
- `jit_handle_impl_fiber`: runs body on coro, loop dispatches signals to clauses
- `jit_signal_impl_fiber`: suspends coro, stores signal info in HandleFiberState (same save/restore pattern as shift)
- `jit_exec_resolve` fiber path: single-shot resume (no clone), reinstalls handler for multi-signal
- `raise_pending` flag must be captured before interp state restore (coro sets it, restore overwrites it)
- I/O fast path for unhandled signals handled inline in `jit_signal_impl_fiber`
- HandleFiberState stores handler_copy for reinstallation by resolve

#### 3.2.1 Implement `jit_handle_impl_fiber`

The handle body runs on a fiber. When a signal fires, the fiber suspends (context_switch back to handle caller). The handler clause runs in the parent context. `resolve` resumes the fiber.

```c3
struct HandleFiberState {
    Expr*          body;
    Env*           env;
    Interp*        interp;
    Value*         result;
    bool           signaled;
    SymbolId       signal_tag;
    Value*         signal_arg;
    Fiber*         fiber;        // The body's fiber
}

fn void handle_fiber_entry(void* arg) {
    HandleFiberState* state = (HandleFiberState*)arg;
    Value* v = jit_eval(state.body, state.env, state.interp);
    state.result = v;
    state.signaled = false;
    // Fiber trampoline will mark COMPLETED and switch back to parent
}

fn EvalResult jit_handle_impl_fiber(Expr* expr, Env* env, Interp* interp) {
    // Parse handler clauses (same as existing lines 1037-1050)
    // ...

    Fiber* fiber = fiber_create(&interp.fiber_pool, DEFAULT_FIBER_STACK_SIZE);

    HandleFiberState state;
    state.body = expr.handle.body;
    state.env = env;
    state.interp = interp;
    state.result = null;
    state.signaled = false;
    state.fiber = fiber;

    fiber_init_context(fiber, &handle_fiber_entry, &state);

    // Push handler BEFORE running fiber (so signal can find it)
    // But the handler stack entry needs to know about the fiber
    if (interp.handler_count >= interp.handler_capacity) interp.grow_handler_stack();
    usz my_idx = interp.handler_count;
    EffectHandler* h = &interp.handler_stack[my_idx];
    // ... copy clauses same as existing ...
    h.fiber = fiber;  // NEW FIELD on EffectHandler
    h.fiber_state = &state;  // NEW FIELD
    interp.handler_count++;

    FiberContext parent_ctx;
    fiber.parent_ctx = &parent_ctx;
    Fiber* saved_fiber = interp.current_fiber;
    interp.current_fiber = fiber;

    // Run the body fiber
    fiber_context_switch(&parent_ctx, &fiber.ctx);

    interp.current_fiber = saved_fiber;

    while (state.signaled) {
        // Signal fired -- dispatch to handler clause
        state.signaled = false;

        // Find matching clause
        EffectClause* clause = null;
        for (usz j = 0; j < h.clause_count; j++) {
            if ((uint)h.tags[j] == (uint)state.signal_tag) {
                clause = &h.clauses[j];
                break;
            }
        }

        if (clause == null) {
            // No matching clause -- propagate (or I/O fast path)
            // ... (same logic as existing unhandled signal path)
        }

        // Create continuation wrapping the suspended fiber
        Continuation* k = (Continuation*)mem::malloc(Continuation.sizeof);
        k.data = null;
        k.fiber = fiber;
        k.is_fiber_based = true;
        Value* k_val = make_continuation(interp, k);

        // Pop handler (handler ran, body is suspended)
        interp.handler_count = my_idx;

        // Bind k and arg, evaluate clause
        Env* clause_env = env.extend(interp, clause.k_name, k_val);
        clause_env = clause_env.extend(interp, clause.arg_name, state.signal_arg);

        // Also bind __k for resolve/with-continuation
        clause_env = clause_env.extend(interp, interp.sym__k, k_val);

        Value* handler_v = jit_eval(clause.handler_body, clause_env, interp);

        // If handler_v calls resolve internally, the fiber may have resumed and completed
        if (fiber.status == COMPLETED) {
            // resolve was called, fiber ran to completion
            // handler_v is the result of the handler clause body
            mem::free(k);
            fiber_destroy(fiber, &interp.fiber_pool);
            if (handler_v != null && handler_v.tag == ERROR) {
                return eval_error(handler_v.str_val.chars[:handler_v.str_val.len]);
            }
            return eval_ok(handler_v);
        }

        if (fiber.status == SUSPENDED) {
            // Handler didn't call resolve -- handler "aborted" (discarded k)
            // The handler's return value IS the handle expression's result
            mem::free(k);
            fiber_destroy(fiber, &interp.fiber_pool);
            if (handler_v != null && handler_v.tag == ERROR) {
                return eval_error(handler_v.str_val.chars[:handler_v.str_val.len]);
            }
            return eval_ok(handler_v);
        }

        // If fiber is still RUNNING somehow, something went wrong
        break;
    }

    // Normal completion (no signal)
    interp.handler_count = my_idx;
    Value* v = state.result;
    fiber_destroy(fiber, &interp.fiber_pool);
    if (v != null && v.tag == ERROR) {
        return eval_error(v.str_val.chars[:v.str_val.len]);
    }
    return eval_ok(v);
}
```

#### 3.2.2 Modify `jit_perform_impl` for fiber path

When on a fiber, `signal` suspends the fiber instead of doing the longjmp dance:

```c3
fn EvalResult jit_perform_impl_fiber(Expr* expr, Env* env, Interp* interp, RegisterContext* signal_save) {
    Value* arg_v = jit_eval(expr.perform.arg, env, interp);
    if (arg_v != null && arg_v.tag == ERROR) {
        return eval_error(arg_v.str_val.chars[:arg_v.str_val.len]);
    }

    SymbolId tag = expr.perform.tag;

    // Type check (same as existing lines 1157-1178)
    // ...

    // Check if we're on a fiber
    Fiber* current = interp.current_fiber;
    if (current != null && current.status == RUNNING) {
        // Find handler by walking handler stack
        for (isz i = (isz)interp.handler_count - 1; i >= 0; i--) {
            EffectHandler* h = &interp.handler_stack[(usz)i];
            for (usz j = 0; j < h.clause_count; j++) {
                if ((uint)h.clauses[j].effect_tag == (uint)tag) {
                    // Found handler. Suspend fiber, return to handler.
                    HandleFiberState* state = (HandleFiberState*)h.fiber_state;
                    state.signaled = true;
                    state.signal_tag = tag;
                    state.signal_arg = arg_v;

                    current.status = SUSPENDED;
                    // Switch back to parent (handle caller)
                    fiber_context_switch(&current.ctx, (FiberContext*)current.parent_ctx);

                    // Resumed! Return the resolve value
                    return eval_ok(interp.resume_value);
                }
            }
            if (h.strict_mode) break;
        }

        // No handler found -- I/O fast path (same as existing lines 1277-1303)
        // ...
    }

    // Fallback to existing non-fiber path
    return jit_perform_impl(expr, env, interp, signal_save);
}
```

#### 3.2.3 Modify `jit_exec_resolve` for fiber path

```c3
fn Value* jit_exec_resolve_fiber(Interp* interp, Expr* expr, Env* env) {
    Value* val = jit_eval(expr.resolve.value, env, interp);
    if (val != null && val.tag == ERROR) return val;

    Value* k_val = env.lookup(interp.sym__k);
    if (k_val == null) k_val = interp.global_env.lookup(interp.sym__k);
    if (k_val == null || k_val.tag != CONTINUATION) {
        return make_error(interp, "resolve: not inside a handler clause");
    }

    if (k_val.cont_val.is_fiber_based && k_val.cont_val.fiber != null) {
        Fiber* fiber = k_val.cont_val.fiber;
        if (fiber.status != SUSPENDED) {
            return make_error(interp, "resolve: continuation already completed");
        }

        // Resume the fiber with val
        interp.resume_value = val;
        fiber.status = RUNNING;

        // Reinstall handler so body can signal again
        // (same handler reinstallation as existing line 3104-3108)
        // ...

        FiberContext parent_ctx;
        fiber.parent_ctx = &parent_ctx;
        Fiber* saved = interp.current_fiber;
        interp.current_fiber = fiber;

        fiber_context_switch(&parent_ctx, &fiber.ctx);

        interp.current_fiber = saved;

        // Fiber either completed or signaled again
        // Return result to handler clause
        if (fiber.status == COMPLETED) {
            return (Value*)fiber.result;
        }
        // If signaled again, we need to re-enter the handle loop
        // This is handled by the handle_impl_fiber's while(state.signaled) loop
        return val;  // Will be overridden by handle loop
    }

    // Fallback to existing path
    // ... (existing jit_exec_resolve code)
}
```

**Critical subtlety**: When `resolve` resumes the fiber and the fiber signals again, control must return to the `jit_handle_impl_fiber`'s while loop, not to the resolve caller. This requires careful design:

- Option A: `resolve` context-switches into fiber, fiber signals again, fiber context-switches back to its parent_ctx. If parent_ctx was set to resolve's context, resolve returns with signaled=true, and resolve's caller (the handler clause) needs to somehow re-enter handle's dispatch loop. This is awkward.

- Option B (preferred): When `resolve` resumes the fiber, it sets the fiber's `parent_ctx` back to the original handle caller's context. So when the fiber signals again, it switches directly back to `jit_handle_impl_fiber`'s while loop. `resolve` returns the last signal's result back to the handler clause. But wait -- resolve needs to return a value to the handler clause body.

Actually, the correct semantics for multi-signal patterns (like `with-trampoline`) are:

```
(handle (+ (signal a 1) (signal b 2))
  (a x (resolve x))
  (b x (resolve x)))
```

1. Body runs, hits `(signal a 1)`, suspends.
2. Handle dispatches to clause `a`, creates k, evaluates `(resolve x)`.
3. `resolve` resumes fiber with value 1. Body continues: `(+ 1 (signal b 2))`.
4. Body hits `(signal b 2)`, suspends again.
5. Handle dispatches to clause `b`, creates new k, evaluates `(resolve x)`.
6. `resolve` resumes fiber with value 2. Body completes: `(+ 1 2) = 3`.
7. Body fiber completes. Handle returns 3.

So `resolve` must:
1. Resume the fiber.
2. If the fiber completes, the handle expression should return the body's result.
3. If the fiber signals again, the handle should dispatch to the next clause.

The cleanest way: after `resolve` resumes the fiber and it completes or re-signals, control must return to `jit_handle_impl_fiber`'s loop. So `resolve` should NOT context-switch itself -- it should tell the handle loop to resume, then the handler clause body's evaluation completes (resolve returns whatever), and the handle loop checks the fiber state.

**Revised approach**: `resolve` does a context_switch into the fiber from within the handler clause. When the fiber suspends or completes, it switches back. `resolve` then returns. Control returns to the handler clause body, which returns to `jit_handle_impl_fiber`. The handle loop checks: did fiber complete? Return body result. Did fiber signal again? Loop and dispatch.

This actually works because `jit_handle_impl_fiber` calls `jit_eval(clause.handler_body, ...)` which eventually calls `resolve` which switches into fiber. When fiber suspends, resolve returns, clause body returns, handle loop checks fiber state.

#### 3.2.4 Add `EffectHandler` fiber fields

In value.c3, modify `EffectHandler` (line 1723):

```c3
struct EffectHandler {
    SymbolId[MAX_EFFECT_CLAUSES] tags;
    EffectClause[MAX_EFFECT_CLAUSES] clauses;
    usz clause_count;
    Env* handler_env;
    Expr* body_expr;
    Env*  body_env;
    bool  strict_mode;
    // NEW: Fiber integration
    main::Fiber* fiber;     // The body's fiber (null for non-fiber handlers)
    void* fiber_state;      // HandleFiberState* (null for non-fiber)
}
```

#### 3.2.5 Verification

Same test suite. Critical effect tests:

- "handle + perform + resume" (line 1215)
- "handle abort" (line 1216)
- "effect modify" (line 1515)
- "multi-perform 2 sum" (line 1677)
- "multi-perform 3 sum" (line 1678)
- "multi-perform doubles" (line 1679)
- "multi-perform abort" (line 1680)
- "signal/resolve basic" (line 1683)
- "multi-shot effect" (line 1697)
- "with-continuation basic" (line 1700)
- "with-trampoline passthrough" (line 1717)
- "with-trampoline single bounce" (line 1718)
- I/O fast path tests (all print/println/read-file tests)

---
