# fiber-continuation-unification Part 01

Source: `docs/plans/fiber-continuation-unification.md`

Now I have a comprehensive understanding of the entire codebase. Let me compile the detailed implementation plan.

---

# Detailed Implementation Plan: Unified Fiber-Based Continuation System for Omni Lisp

## 1. Current Architecture Analysis

### What Exists Today

**Replay-based mechanism (active, in `jit.c3`):**
- `jit_handle_impl` (line 1034): Sets up effect handlers on a stack, uses `context_capture`/`context_restore` as setjmp/longjmp for non-local exit when a signal fires.
- `jit_perform_impl` (line 1142): On signal, creates a `CapturedCont` storing the handler body expression + environment + replay metadata, evaluates the handler clause, then `context_restore` jumps back to `jit_handle_impl`.
- `jit_exec_perform` (line 909): Wrapper that sets up a `RegisterContext signal_save` save point for direct resume (single-shot optimization).
- `jit_exec_resolve` (line 3083): On `resolve`, either longjmps back to signal_save (single-shot fast path) or falls through to `jit_apply_continuation` (replay-based multi-shot).
- `jit_apply_continuation` (line 1312): Replay mechanism -- reinstalls handler on stack, sets up prior_results arrays, re-evaluates the entire handle body from scratch with `cont_substituting` flag, fast-forwarding through previously-resolved signals.
- `jit_reset_impl` / `jit_shift_impl` (lines 924/958): Same replay pattern for shift/reset.

**Key data structures:**
- `CapturedCont` (value.c3 line 1738): ~200 bytes, stores body expr/env, handler copy, replay counters (perform_index, prior_results, shift_index, shift_prior_results), signal_save pointer, region pinning.
- `EffectHandler` (value.c3 line 1723): Tags, clauses, env, body expr.
- `Interp` fields (value.c3 lines 1941-1984): `reset_depth`, `shift_target_depth`, `shift_result`, `handler_stack`, `handler_count`, `handle_jmp[64]`, `active_effect_cc[64]`, `effect_result`, replay counters, reset body/env stacks.
- `Continuation` (value.c3 line 251): Currently a thin wrapper with a single `void* data` field pointing to `CapturedCont`.

**Unused scaffolding:**
- `continuation.c3`: Rich PromptStack/PromptFrame/Continuation structs -- never called from jit.c3 or eval.c3.
- `delimited.c3`: Full reset/shift/resume API built on continuation.c3 -- also unused by the interpreter.
- `context.c3` lower section: `capture_continuation_context`/`restore_continuation_context` (stack segment copy) -- exists but only the longjmp-style `context_capture`/`context_restore` are used.

### Problems with Current Approach

1. **O(N^2) multi-shot**: Each continuation invocation re-evaluates the entire body, fast-forwarding through N-1 previous signals. N invocations cost O(N^2) total.
2. **Side-effect re-execution**: Replay re-runs the body, meaning I/O and mutations execute again during fast-forward (the prior_results array only remembers signal return values, not side effects).
3. **Fragile state tracking**: The `cont_substituting`, `cont_is_effect`, `effect_perform_counter`, `shift_counter`, and prior_results arrays form a complex state machine that's hard to reason about.
4. **No real fibers**: Test files exist for fiber/yield/resume/spawn/join API but none of it is implemented.

## 2. Target Architecture

Replace replay with real stack-captured fibers. The new design has four layers:

```
Layer 4: User-facing fibers  (Omni Lisp: fiber, yield, resume)
Layer 3: Algebraic effects    (C3: handle/signal/resolve - unchanged syntax)
Layer 2: Delimited continuations (C3: reset/shift - unchanged syntax)
Layer 1: Fiber primitives     (C3/assembly: context_switch, stack alloc)
```

The key insight: `reset` and `handle` each run their body on a **fiber** (a separately-allocated stack). `shift` and `signal` **suspend** that fiber (context_switch back to parent). The fiber itself IS the continuation. One-shot resume: context_switch back. Multi-shot resume: clone the fiber's stack, fixup RBP chain, context_switch to clone.

## 3. Phase-by-Phase Implementation Plan

---

### PHASE 0: Preparation (Low Risk, No Behavior Change) — COMPLETE

**Goal**: Create the new fiber infrastructure files, build them, verify compilation. No existing code is modified.
**Status**: Complete. Stack engine in `src/stack_engine.c3` with 7 passing tests.

#### 3.0.1 Create `/home/heefoo/Documents/code/Omni/src/fiber.c3`

This file contains all Layer 1 primitives.

**Data Structures:**

```c3
module main;

import std::io;

// --- Extern declarations for mmap/munmap/mprotect ---
extern fn void* mmap(void* addr, usz length, int prot, int flags,
                     int fd, long offset) @extern("mmap");
extern fn int munmap(void* addr, usz length) @extern("munmap");
extern fn int mprotect(void* addr, usz length, int prot) @extern("mprotect");

const int PROT_NONE  = 0x0;
const int PROT_READ  = 0x1;
const int PROT_WRITE = 0x2;
const int MAP_PRIVATE   = 0x02;
const int MAP_ANONYMOUS = 0x20;
const usz PAGE_SIZE = 4096;

// --- Fiber Context (72 bytes, callee-saved + RSP + RIP + MXCSR + x87CW) ---
struct FiberContext {
    ulong rbx;       // offset  0
    ulong rbp;       // offset  8
    ulong r12;       // offset 16
    ulong r13;       // offset 24
    ulong r14;       // offset 32
    ulong r15;       // offset 40
    ulong rsp;       // offset 48
    ulong rip;       // offset 56
    uint  mxcsr;     // offset 64
    uint  x87cw;     // offset 68
}
// Total: 72 bytes, 8-byte aligned

// --- Fiber Status ---
enum FiberStatus : char {
    READY,       // Created but not yet started
    RUNNING,     // Currently executing
    SUSPENDED,   // Yielded/shifted, can be resumed
    COMPLETED,   // Returned normally
    DEAD         // Error or invalidated
}

// --- Fiber Stack ---
struct FiberStack {
    void* base;       // mmap'd region (includes guard page)
    usz   total_size; // Total mmap'd size (guard + usable)
    usz   usable_size;// Usable stack size (total - guard page)
    void* stack_top;  // Top of usable stack (base + total_size on x86_64)
}

const usz DEFAULT_FIBER_STACK_SIZE = 65536;  // 64KB
const usz GUARD_PAGE_SIZE = PAGE_SIZE;       // 4KB guard page

// --- Fiber ---
struct Fiber {
    FiberContext  ctx;        // Saved register state
    FiberStack   stack;      // Owned stack
    FiberStatus  status;
    void*        result;     // Return value when COMPLETED
    void*        parent_ctx; // Pointer to parent FiberContext (for symmetric switch-back)
    uint         id;         // Unique identifier
    Fiber*       pool_next;  // For free-list in FiberPool
}

// --- Fiber Pool (free-list of pre-allocated stacks) ---
struct FiberPool {
    Fiber* free_list;  // Singly-linked list of available fibers
    usz    pool_size;  // Number of fibers in pool
    usz    max_pool;   // Maximum pool size before freeing
    uint   next_id;    // Monotonic ID counter
}

const usz FIBER_POOL_MAX = 64;  // Keep up to 64 fibers cached
```

**Functions:**

```c3
// --- Stack allocation ---
fn FiberStack fiber_stack_alloc(usz size);
    // mmap(null, size + GUARD_PAGE_SIZE, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)
    // mprotect(base, GUARD_PAGE_SIZE, PROT_NONE)  -- guard page at bottom
    // stack_top = base + size + GUARD_PAGE_SIZE

fn void fiber_stack_free(FiberStack* stack);
    // munmap(stack.base, stack.total_size)

// --- Context switch (assembly, ~20 instructions) ---
fn void fiber_context_switch(FiberContext* old_ctx, FiberContext* new_ctx) @naked;
    // Save: rbx,rbp,r12-r15,rsp to old_ctx
    // Save: rip (return address from stack) to old_ctx
    // stmxcsr [old_ctx+64], fnstcw [old_ctx+68]
    // Restore: rbx,rbp,r12-r15,rsp from new_ctx
    // ldmxcsr [new_ctx+64], fldcw [new_ctx+68]
    // Load new_ctx.rip into rcx, jmp rcx  (NOT ret, to avoid CET shadow stack)

// --- Fiber lifecycle ---
fn Fiber* fiber_create(FiberPool* pool, usz stack_size);
    // Pop from free_list if available, else allocate new
    // Allocate stack, initialize status=READY

fn void fiber_init_context(Fiber* fiber, void* entry_fn, void* arg);
    // Set up initial stack frame:
    //   fiber.ctx.rsp = stack_top - 16 (aligned)
    //   [fiber.ctx.rsp] = &fiber_entry_trampoline  (return address)
    //   fiber.ctx.rip = entry_fn
    //   fiber.ctx.rbp = fiber.ctx.rsp
    //   Push arg as first argument (in rdi position via trampoline)

fn void fiber_entry_trampoline() @naked;
    // Called when fiber starts for the first time via context_switch
    // rdi = arg (the Fiber*), set up by init_context
    // Calls the actual entry function
    // When entry function returns, marks fiber as COMPLETED
    // context_switch back to parent

fn void fiber_destroy(Fiber* fiber, FiberPool* pool);
    // If pool has room, return to free_list (reset status, keep stack)
    // Otherwise, free stack and free fiber

// --- Stack cloning (for multi-shot) ---
fn Fiber* fiber_clone(Fiber* source, FiberPool* pool);
    // Allocate new fiber with same stack size
    // memcpy source.stack usable region to new.stack usable region
    // Copy source.ctx to new.ctx
    // Fixup: adjust rsp, rbp in new.ctx by (new_base - old_base)
    // Fixup: walk RBP chain on new stack, adjusting each frame pointer
    // new.status = SUSPENDED

// --- Pool management ---
fn void fiber_pool_init(FiberPool* pool);
fn void fiber_pool_shutdown(FiberPool* pool);
```

**Assembly for `fiber_context_switch`:**

```c3
fn void fiber_context_switch(FiberContext* old_ctx, FiberContext* new_ctx) @naked {
    asm {
        // Save callee-saved registers to old_ctx (rdi)
        movq [$rdi],      $rbx;      // offset 0:  rbx
        movq [$rdi + 8],  $rbp;      // offset 8:  rbp
        movq [$rdi + 16], $r12;      // offset 16: r12
        movq [$rdi + 24], $r13;      // offset 24: r13
        movq [$rdi + 32], $r14;      // offset 32: r14
        movq [$rdi + 40], $r15;      // offset 40: r15

        // Save RSP (caller's RSP = current RSP + 8, since call pushed return addr)
        movq $rax, $rsp;
        addq $rax, 8;
        movq [$rdi + 48], $rax;      // offset 48: rsp

        // Save RIP (return address, currently at [rsp])
        movq $rax, [$rsp];
        movq [$rdi + 56], $rax;      // offset 56: rip

        // Save MXCSR and x87 control word
        stmxcsr [$rdi + 64];         // offset 64: mxcsr
        fnstcw  [$rdi + 68];         // offset 68: x87cw

        // --- Restore from new_ctx (rsi) ---

        // Restore callee-saved registers
        movq $rbx, [$rsi];           // rbx
        movq $rbp, [$rsi + 8];       // rbp
        movq $r12, [$rsi + 16];      // r12
        movq $r13, [$rsi + 24];      // r13
        movq $r14, [$rsi + 32];      // r14
        movq $r15, [$rsi + 40];      // r15

        // Restore RSP
        movq $rsp, [$rsi + 48];

        // Restore MXCSR and x87 control word
        ldmxcsr [$rsi + 64];
        fldcw   [$rsi + 68];

        // Jump to saved RIP (NOT ret, avoids CET shadow stack issues)
        movq $rcx, [$rsi + 56];
        jmpq *$rcx;
    }
}
```

**Note on C3 inline assembly**: The existing `context.c3` uses `stmxcsr`/`ldmxcsr`/`fnstcw`/`fldcw` -- need to verify C3 supports these mnemonics. If not, they can be encoded as raw bytes. The existing code does NOT save MXCSR/x87CW, so this is an improvement. If C3 does not support `jmpq *$rcx`, use the existing pattern: `pushq $rcx; ret;` (which works but is CET-unfriendly -- acceptable for now since Omni doesn't use CET).

#### 3.0.2 Update `/home/heefoo/Documents/code/Omni/project.json`

No change needed -- the `"sources": ["src"]` directive already includes all `.c3` files under `src/`. The new `fiber.c3` in `src/` will be automatically compiled.

#### 3.0.3 Add fiber unit tests to `fiber.c3`

```c3
fn void run_fiber_tests() {
    // Test 1: Stack allocation and free
    // Test 2: Context switch between main and a fiber
    // Test 3: Fiber that returns a value
    // Test 4: Fiber pool alloc/return
    // Test 5: Fiber clone and RBP fixup
}
```

Call `run_fiber_tests()` from `entry.c3` line 660, right after `run_context_tests()`.

#### 3.0.4 Verification

Build with `c3c build`. Run `./build/main`. All existing 927 tests must pass. New fiber tests run and pass. No existing code is touched.

---

### PHASE 1: Wire Fibers into reset/shift (Parallel Implementation) — COMPLETE

**Goal**: Implement fiber-based reset/shift alongside the existing replay mechanism, switched by a flag. The existing code path remains as fallback.
**Status**: Complete. 986 tests pass (0 failures). Key lessons learned:
- `jit_shift_impl_fiber` MUST return `Value*` (not `EvalResult`) — EvalResult triggers System V ABI hidden return pointer that goes stale after `coro_clone`
- ALL coro boundaries (reset→coro, shift suspend/resume, apply→clone) must save/restore interp state (`eval_depth`, `jit_env`, `match_env`, `flags`, TCO fields, `reset_depth`, `handler_count`)
- `coro_trampoline` must re-read `g_current_coro` after entry returns (stale after clone)

#### 3.1.1 Add fiber fields to `Interp` struct

In `/home/heefoo/Documents/code/Omni/src/lisp/value.c3`, add to the `Interp` struct (after line 1984, before the closing `}`):

```c3
    // Fiber-based continuation system
    main::FiberPool fiber_pool;
    bool use_fiber_continuations;  // Toggle: true = fiber-based, false = replay
```

In `Interp.init()` (around line 2033), add:

```c3
    main::fiber_pool_init(&self.fiber_pool);
    self.use_fiber_continuations = true;  // Enable fiber system
```

In `Interp.destroy()` (around line 2271), add:

```c3
    main::fiber_pool_shutdown(&self.fiber_pool);
```

#### 3.1.2 New `Continuation` struct

Replace the thin `Continuation` struct in value.c3 (line 251-253) with:

```c3
struct Continuation {
    void* data;           // Historical: opaque pointer to CapturedCont (replay path)
    main::Fiber* fiber;   // New: fiber backing this continuation (null if replay)
    bool is_fiber_based;  // true = use fiber, false = use replay path
}
```

The `Value.cont_val` pointer still points to `Continuation*`. The `make_continuation` function is unchanged. All existing code that accesses `k_val.cont_val.data` continues to work since the field is still there.

#### 3.1.3 Implement fiber-based `jit_reset_impl_fiber` and `jit_shift_impl_fiber`

In `/home/heefoo/Documents/code/Omni/src/lisp/jit.c3`, add new functions (after `jit_shift_impl`, around line 1031):

```c3
// --- Fiber-based reset/shift ---

// State passed into a reset fiber
struct ResetFiberState {
    Expr*  body;
    Env*   env;
    Interp* interp;
    Value* result;      // Set when fiber completes or shifts
    bool   shifted;     // True if a shift occurred
    Value* shift_value; // Value returned by shift body
}

// Entry point for the reset fiber
fn void reset_fiber_entry(void* arg) {
    ResetFiberState* state = (ResetFiberState*)arg;
    Value* v = jit_eval(state.body, state.env, state.interp);
    state.result = v;
    state.shifted = false;
    // Fiber will be marked COMPLETED by the trampoline
}

fn EvalResult jit_reset_impl_fiber(Expr* expr, Env* env, Interp* interp) {
    // Allocate a fiber for the body
    Fiber* fiber = fiber_create(&interp.fiber_pool, DEFAULT_FIBER_STACK_SIZE);

    ResetFiberState state;
    state.body = expr.reset.body;
    state.env = env;
    state.interp = interp;
    state.result = null;
    state.shifted = false;
    state.shift_value = null;

    // Store state pointer where the fiber can find it
    // (via Interp field or fiber user_data)
    Interp.current_reset_fiber_state = &state;  // new Interp field

    fiber_init_context(fiber, &reset_fiber_entry, &state);

    // Context switch: run the fiber
    FiberContext main_ctx;
    fiber.parent_ctx = &main_ctx;
    fiber_context_switch(&main_ctx, &fiber.ctx);

    // Returned here: either fiber completed or shifted
    if (state.shifted) {
        // shift occurred: the shift body already ran and set shift_value
        fiber_destroy(fiber, &interp.fiber_pool);
        return eval_ok(state.shift_value);
    }

    // Normal completion
    Value* v = state.result;
    fiber_destroy(fiber, &interp.fiber_pool);
    if (v != null && v.tag == ERROR) {
        return eval_error(v.str_val.chars[:v.str_val.len]);
    }
    return eval_ok(v);
}
```

For `shift`, the fiber is suspended (not destroyed). The continuation captures the fiber:

```c3
fn EvalResult jit_shift_impl_fiber(Expr* expr, Env* env, Interp* interp) {
    // Get the current fiber (we must be inside a reset fiber)
    // The fiber's parent_ctx points to the reset caller
    Fiber* current_fiber = interp.current_fiber;  // new Interp field
    if (current_fiber == null) {
        return eval_error("shift outside of reset");
    }

    // Create continuation wrapping the fiber
    Continuation* k = (Continuation*)mem::malloc(Continuation.sizeof);
    k.data = null;
    k.fiber = current_fiber;
    k.is_fiber_based = true;
    current_fiber.status = SUSPENDED;

    Value* k_val = make_continuation(interp, k);

    // Bind k in environment
    Env* shift_env = env.extend(interp, expr.shift.k_name, k_val);

    // Switch back to parent (reset caller) to evaluate shift body there
    // But shift body needs k, so we evaluate it HERE before switching
    // Actually: shift body runs in the PARENT context, not the fiber.
    // So we need to switch back, then parent evaluates shift body.

    // Store shift body info for parent
    ResetFiberState* state = interp.current_reset_fiber_state;
    state.shifted = true;
    state.shift_k = k_val;
    state.shift_body = expr.shift.body;
    state.shift_env = shift_env;

    // Suspend: switch back to parent
    fiber_context_switch(&current_fiber.ctx, (FiberContext*)current_fiber.parent_ctx);

    // If we ever resume here, return the resume value
    return eval_ok(interp.resume_value);  // new Interp field set by resume
}
```

**IMPORTANT DESIGN DECISION**: In the standard shift/reset semantics, `shift` captures the continuation and the shift's body runs in the **parent** (reset) context. The captured continuation, when invoked, resumes the fiber. So the control flow is:

1. `(reset BODY)` -- creates fiber, switches to it
2. `BODY` runs on fiber until it hits `(shift k SHIFT-BODY)`
3. Fiber suspends, control returns to reset caller
4. Reset caller evaluates `SHIFT-BODY` in the parent context (with `k` bound)
5. If `SHIFT-BODY` calls `(k val)`, fiber resumes with `val`
6. `SHIFT-BODY`'s return value becomes the return value of `(reset ...)`

This means `jit_shift_impl_fiber` needs to:
- Save the shift body expression and environment into the `ResetFiberState`
- Context-switch back to the parent
- The parent (in `jit_reset_impl_fiber`) then evaluates the shift body
- If `k` is invoked, it context-switches back into the fiber

Revised `jit_reset_impl_fiber`:

```c3
fn EvalResult jit_reset_impl_fiber(Expr* expr, Env* env, Interp* interp) {
    Fiber* fiber = fiber_create(&interp.fiber_pool, DEFAULT_FIBER_STACK_SIZE);

    ResetFiberState state;
    state.body = expr.reset.body;
    state.env = env;
    state.interp = interp;
    state.result = null;
    state.shifted = false;

    fiber_init_context(fiber, &reset_fiber_entry, &state);

    FiberContext parent_ctx;
    fiber.parent_ctx = &parent_ctx;

    // Track current fiber for shift detection
    Fiber* saved_fiber = interp.current_fiber;
    interp.current_fiber = fiber;
    interp.current_reset_fiber_state = &state;

    // Run the fiber
    fiber_context_switch(&parent_ctx, &fiber.ctx);

    // Back in parent. Either fiber completed or shifted.
    interp.current_fiber = saved_fiber;

    if (state.shifted) {
        // Evaluate shift body in parent context
        Value* shift_result = jit_eval(state.shift_body, state.shift_env, interp);

        // Fiber is now either still suspended (k was called) or abandoned (k was discarded)
        if (fiber.status == COMPLETED) {
            // k was called and fiber ran to completion
            fiber_destroy(fiber, &interp.fiber_pool);
        } else if (fiber.status == SUSPENDED) {
            // k was never called -- fiber is abandoned
            fiber_destroy(fiber, &interp.fiber_pool);
        }

        if (shift_result != null && shift_result.tag == ERROR) {
            return eval_error(shift_result.str_val.chars[:shift_result.str_val.len]);
        }
        return eval_ok(shift_result);
    }

    // Normal completion
    Value* v = state.result;
    fiber_destroy(fiber, &interp.fiber_pool);
    if (v != null && v.tag == ERROR) {
        return eval_error(v.str_val.chars[:v.str_val.len]);
    }
    return eval_ok(v);
}
```

#### 3.1.4 Implement fiber-based `jit_apply_continuation` for shift continuations

In `jit_apply_continuation` (jit.c3 line 1312), add a branch at the top:

```c3
fn EvalResult jit_apply_continuation(Value* k_val, Value* arg, Interp* interp) {
    if (k_val.cont_val == null) {
        return eval_error("cannot resume continuation: null continuation");
    }

    // NEW: Fiber-based continuation path
    if (k_val.cont_val.is_fiber_based && k_val.cont_val.fiber != null) {
        return jit_apply_fiber_continuation(k_val, arg, interp);
    }

    // EXISTING: Replay-based path (unchanged)
    CapturedCont* cc = (CapturedCont*)k_val.cont_val.data;
    // ... rest of existing code ...
}

fn EvalResult jit_apply_fiber_continuation(Value* k_val, Value* arg, Interp* interp) {
    Fiber* fiber = k_val.cont_val.fiber;

    if (fiber.status != SUSPENDED) {
        return eval_error("cannot resume completed/dead continuation");
    }

    // One-shot: resume directly
    interp.resume_value = arg;
    fiber.status = RUNNING;

    FiberContext parent_ctx;
    fiber.parent_ctx = &parent_ctx;
    Fiber* saved_fiber = interp.current_fiber;
    interp.current_fiber = fiber;

    fiber_context_switch(&parent_ctx, &fiber.ctx);

    interp.current_fiber = saved_fiber;

    if (fiber.status == COMPLETED) {
        Value* result = (Value*)fiber.result;
        // Don't destroy -- caller may hold reference
        if (result != null && result.tag == ERROR) {
            return eval_error(result.str_val.chars[:result.str_val.len]);
        }
        return eval_ok(result);
    }

    // Fiber suspended again (nested shift) -- handle recursively via reset state
    return eval_ok(interp.resume_value);
}
```

For **multi-shot** (calling `k` more than once), the second call must clone:

```c3
fn EvalResult jit_apply_fiber_continuation(Value* k_val, Value* arg, Interp* interp) {
    Fiber* fiber = k_val.cont_val.fiber;

    if (fiber.status == COMPLETED || fiber.status == DEAD) {
        // Already used and completed -- need clone for multi-shot
        return eval_error("continuation already completed");
    }

    Fiber* target;
    if (fiber.status == SUSPENDED) {
        // First invocation: use original fiber
        target = fiber;
        // Mark original as used so next call knows to clone
        // Actually, for proper multi-shot, ALWAYS clone:
        target = fiber_clone(fiber, &interp.fiber_pool);
    } else {
        return eval_error("cannot resume running fiber");
    }

    interp.resume_value = arg;
    target.status = RUNNING;

    FiberContext parent_ctx;
    target.parent_ctx = &parent_ctx;
    Fiber* saved_fiber = interp.current_fiber;
    interp.current_fiber = target;

    fiber_context_switch(&parent_ctx, &target.ctx);

    interp.current_fiber = saved_fiber;

    Value* result = (Value*)target.result;
    fiber_destroy(target, &interp.fiber_pool);  // Clone is disposable

    if (result != null && result.tag == ERROR) {
        return eval_error(result.str_val.chars[:result.str_val.len]);
    }
    return eval_ok(result);
}
```

**Optimization**: For the common one-shot case, we can avoid cloning by tracking whether this is the first invocation. If `k` is called exactly once, use the original fiber directly. If called again, clone. This requires a `bool first_resume` flag on the Continuation.

#### 3.1.5 Wire the flag into dispatch

In `jit_exec_reset` (jit.c3 line 891) and `jit_exec_shift` (line 897), check the flag:

```c3
fn Value* jit_exec_reset(Interp* interp, Expr* expr, Env* env) {
    EvalResult r;
    if (interp.use_fiber_continuations) {
        r = jit_reset_impl_fiber(expr, env, interp);
    } else {
        r = jit_reset_impl(expr, env, interp);
    }
    if (r.error.has_error) return make_error(interp, r.error.message[:256]);
    return r.value;
}
```

Same pattern for shift.
