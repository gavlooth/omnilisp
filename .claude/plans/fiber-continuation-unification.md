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
    void* data;           // Legacy: opaque pointer to CapturedCont (replay path)
    main::Fiber* fiber;   // New: fiber backing this continuation (null if replay)
    bool is_fiber_based;  // true = use fiber, false = use legacy replay
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

### PHASE 3: Implement User-Facing Fibers

**Goal**: Implement `fiber`, `yield`, `resume` primitives that the test files expect.

#### 3.3.1 Add `FIBER` ValueTag

In value.c3, add to `ValueTag` enum (after line 36):

```c3
    FIBER,        // User-facing fiber (wraps Fiber*)
```

Add to the `Value` union:

```c3
    main::Fiber*  fiber_val;     // User-facing fiber
```

Add constructor:

```c3
fn Value* make_fiber(Interp* interp, main::Fiber* f) @inline {
    Value* v = interp.alloc_value();
    v.tag = FIBER;
    v.fiber_val = f;
    return v;
}
```

#### 3.3.2 Register `fiber`, `resume`, `yield` primitives

In eval.c3 `register_primitives()`, add:

```c3
    { "fiber",  &prim_fiber,  1 },   // (fiber thunk) -> fiber value
    { "resume", &prim_resume, -1 },  // (resume fiber [value]) -> value
```

In primitives.c3, implement:

```c3
fn Value* prim_fiber(Value*[] args, Env* env, Interp* interp) {
    Value* thunk = args[0];
    if (thunk.tag != CLOSURE) {
        return raise_error(interp, "fiber: argument must be a closure");
    }

    Fiber* fiber = fiber_create(&interp.fiber_pool, DEFAULT_FIBER_STACK_SIZE);
    // Set up fiber to call the thunk
    // fiber_init_context with a wrapper that calls jit_apply_value(thunk, nil, interp)
    FiberThunkState* state = (FiberThunkState*)mem::malloc(FiberThunkState.sizeof);
    state.thunk = thunk;
    state.interp = interp;
    state.result = null;

    fiber_init_context(fiber, &fiber_thunk_entry, state);
    fiber.user_data = state;

    return make_fiber(interp, fiber);
}

fn Value* prim_resume(Value*[] args, Env* env, Interp* interp) {
    if (args.len < 1) return raise_error(interp, "resume: expected fiber argument");
    Value* fiber_val = args[0];
    if (fiber_val.tag != FIBER) {
        return raise_error(interp, "resume: argument must be a fiber");
    }

    Fiber* fiber = fiber_val.fiber_val;
    Value* resume_val = args.len > 1 ? args[1] : make_nil(interp);

    if (fiber.status == COMPLETED) {
        return raise_error(interp, "resume: fiber already completed");
    }

    interp.resume_value = resume_val;
    fiber.status = RUNNING;

    FiberContext parent_ctx;
    fiber.parent_ctx = &parent_ctx;
    Fiber* saved = interp.current_fiber;
    interp.current_fiber = fiber;

    fiber_context_switch(&parent_ctx, &fiber.ctx);

    interp.current_fiber = saved;

    if (fiber.status == COMPLETED) {
        return (Value*)fiber.result;
    }
    // Fiber yielded
    return interp.yield_value;  // Set by yield
}
```

**yield** can be a primitive or implemented via signals. Since the test files show `yield` inside fibers, implement as a primitive:

```c3
fn Value* prim_yield(Value*[] args, Env* env, Interp* interp) {
    Fiber* current = interp.current_fiber;
    if (current == null) {
        return raise_error(interp, "yield: not inside a fiber");
    }

    Value* yield_val = args.len > 0 ? args[0] : make_nil(interp);
    interp.yield_value = yield_val;
    current.status = SUSPENDED;

    fiber_context_switch(&current.ctx, (FiberContext*)current.parent_ctx);

    // Resumed with a value
    return interp.resume_value;
}
```

Register in primitives:

```c3
    { "yield",  &prim_yield,  -1 },  // (yield [value]) -> resume-value
```

Note: This conflicts with the existing `yield` macro in stdlib.lisp (line 159): `(define [macro] yield ([val] (shift k (cons val k))))`. The macro needs to be removed or renamed to `stream-yield` once real fibers exist, since `yield` will become a primitive. But since the macro is for generators (returns `(cons val k)`), and the primitive is for fibers (suspends and returns resume value), they have different semantics. The safest approach: keep the macro as `stream-yield` for generator patterns, and make `yield` the fiber primitive.

#### 3.3.3 Verification

Run the test files:
- `tests/test_yield.lisp`
- `tests/test_nested_fiber.lisp`

The `tests/test_with_fibers.lisp` uses `with-fibers`, `spawn`, `join`, and `chan` which require a scheduler -- defer to Phase 4.

---

### PHASE 4: Cleanup and Remove Replay Mechanism

**Goal**: Remove the replay code path, simplify the Interp struct, delete unused scaffolding.

#### 3.4.1 Remove from `Interp` struct (value.c3)

Delete these fields:
- `reset_depth`, `shift_target_depth`, `shift_result` (lines 1942-1944)
- `handle_jmp[64]` (line 1955)
- `active_effect_cc[64]` (line 1958)
- `cont_substitution`, `cont_effect_tag` (lines 1964-1965)
- `effect_perform_counter`, `effect_prior_results`, `effect_prior_capacity`, `cont_target_perform_index` (lines 1967-1970)
- `shift_counter`, `shift_prior_results`, `shift_prior_capacity`, `cont_target_shift_index` (lines 1972-1975)
- `reset_body_stack`, `reset_env_stack`, `reset_stack_capacity` (lines 1978-1980)

Remove from `InterpFlags`:
- `shift_occurred` (bit 2)
- `effect_occurred` (bit 3)
- `cont_substituting` (bit 4)
- `cont_is_effect` (bit 5)

#### 3.4.2 Delete replay functions from `jit.c3`

- `jit_reset_impl` (lines 924-956)
- `jit_shift_impl` (lines 958-1030)
- The replay logic in `jit_perform_impl` (lines 1180-1194)
- `jit_apply_continuation` replay path (lines 1312-1395)

#### 3.4.3 Simplify `CapturedCont`

Either delete entirely (if all continuations are fiber-based) or reduce to a minimal struct for backward compatibility.

#### 3.4.4 Delete or gut unused files

- `/home/heefoo/Documents/code/Omni/src/continuation.c3`: Delete or reduce to just the `PromptTag`/`ContinuationId` typedefs if still referenced.
- `/home/heefoo/Documents/code/Omni/src/delimited.c3`: Delete entirely (its tests call the unused reset_begin/shift_capture API).
- `/home/heefoo/Documents/code/Omni/src/context.c3`: Keep `context_capture`/`context_restore` (still useful as lightweight setjmp/longjmp). Remove `capture_continuation_context`/`restore_continuation_context` and `stack_segment_*` functions.

#### 3.4.5 Update entry.c3

Remove `run_delimited_tests()` call (line 668) since the test functions in delimited.c3 will be deleted.

#### 3.4.6 Verification

All 927 existing tests pass with `use_fiber_continuations` flag removed (fiber path is the only path).

---

### PHASE 5: Scheduler for `with-fibers` / `spawn` / `join`

**Goal**: Implement cooperative scheduler for the `with-fibers` API.

This is a separate feature beyond the core continuation unification and can be deferred. Brief outline:

- `FiberScheduler` struct with run queue (circular buffer of Fiber*)
- `spawn` adds fiber to run queue
- `join` suspends current fiber until target completes
- `with-fibers` creates a scheduler scope, runs until all fibers complete
- `chan` / `send` / `recv` for inter-fiber communication (bounded channel with blocking)

---

## 4. Risk Analysis

### High Risk: Stack Pointer Arithmetic on Cloned Fibers

**Problem**: When cloning a fiber stack for multi-shot, all RBP (frame pointer) values on the stack are absolute addresses pointing into the original stack. After memcpy to a new stack at a different address, these pointers are dangling.

**Mitigation**: Walk the RBP chain on the cloned stack and adjust each frame pointer by `(new_base - old_base)`. This is the `fixup_rbp_chain` function. However, if any C3 compiler-generated code stores absolute stack addresses in local variables (not just RBP chain), those would also need fixing. This is fundamentally hard to detect.

**Fallback**: If multi-shot cloning proves too fragile, keep the replay mechanism as a fallback for multi-shot only. Single-shot (the common case) would always use fibers.

### Medium Risk: Interpreter Pointer Validity After Context Switch

**Problem**: The Interp struct is on the main thread's stack (allocated via `Interp.init()`). When executing on a fiber's stack, all access to `interp` goes through a pointer, which is fine. But any `RegisterContext`-style save points that reference the main stack become invalid when we context-switch to a fiber. The existing `handle_jmp[64]` array in Interp saves main-stack contexts.

**Mitigation**: In the fiber-based path, we do NOT use `handle_jmp` at all. The fiber suspension IS the non-local exit mechanism. The save/restore via `context_capture`/`context_restore` in the existing code is completely replaced by `fiber_context_switch`.

### Medium Risk: GC / Region Integration

**Problem**: Fibers hold references to Values, Envs, and Exprs that are allocated in regions. If a region is destroyed while a fiber is suspended, the fiber holds dangling pointers.

**Mitigation**: Fiber stacks hold C3-level stack frames that reference heap/region-allocated objects. As long as the region outlives the fiber, this is safe. Since `handle` and `reset` create and destroy fibers within their dynamic extent, and regions are hierarchical (child outlives parent), this should be safe. For user-facing fibers (Phase 3), fibers that outlive their creating region are a risk -- document that fibers must not escape their region scope.

### Medium Risk: C3 Inline Assembly Limitations

**Problem**: The `fiber_context_switch` function uses x86_64 instructions like `stmxcsr`, `ldmxcsr`, `fnstcw`, `fldcw`, and `jmpq *$rcx`. The existing `context.c3` does NOT use MXCSR/x87CW saving, so we don't know if C3 supports these instructions.

**Mitigation**: Start with the minimal version (same instructions as existing `context_capture`/`context_restore` which already work). Add MXCSR/x87CW later if needed. For the `jmp *rcx`, use `pushq $rcx; ret;` (same pattern as existing `context_restore` line 180-181).

### Low Risk: Signal Handling and Stack Guard Pages

**Problem**: The guard page at the bottom of each fiber stack will cause SIGSEGV on stack overflow. The application needs to handle this signal gracefully or detect it.

**Mitigation**: For Phase 0-2, don't install a SIGSEGV handler. Stack overflows within fibers will crash, same as stack overflows on the main stack. In a future phase, install `sigaltstack` + `SIGSEGV` handler to detect fiber stack overflow and convert to an Omni error.

### Low Risk: I/O Fast Path Preservation

**Problem**: The I/O fast path in `jit_perform_impl` (lines 1277-1303) checks for `io/print`, `io/println`, etc. when no handler is found. This must continue to work on fiber stacks.

**Mitigation**: The I/O fast path doesn't use longjmp/context_restore -- it just calls the raw primitive and returns. It works identically on a fiber stack. No change needed.

### Low Risk: JIT Cache Interaction

**Problem**: JIT-compiled code calls `jit_exec_reset`, `jit_exec_shift`, `jit_exec_handle`, `jit_exec_perform`, `jit_exec_resolve` via function pointers emitted by the JIT compiler (see `jit_compile_reset` at line 3064). These functions need to work on fiber stacks.

**Mitigation**: Since these are regular C3 functions called via function pointer, they work on any stack. The JIT-compiled code just does `call <address>` which pushes a return address and jumps. This is stack-agnostic.

---

## 5. File Change Summary

### New Files

| File | Purpose |
|------|---------|
| `src/fiber.c3` | Layer 1: FiberContext, fiber_context_switch, FiberStack, FiberPool, fiber_create/destroy/clone, run_fiber_tests |

### Modified Files

| File | Lines Affected | Nature of Change |
|------|----------------|------------------|
| `src/lisp/value.c3` | Lines 251-253 (Continuation struct), 594-616 (Value union), 1723-1731 (EffectHandler), 1775-1783 (InterpFlags), 1788-1989 (Interp), 1991-2034 (Interp.init), 2261-2276 (Interp.destroy) | Add fiber fields to Continuation, EffectHandler, Interp; add FIBER ValueTag |
| `src/lisp/jit.c3` | Lines 891-907 (exec wrappers), ~1031 (new fiber impls), 1034-1140 (handle_impl), 1142-1310 (perform_impl), 1312-1395 (apply_continuation), 3083-3122 (exec_resolve) | Add fiber-path alternatives for reset/shift/handle/signal/resolve; modify dispatch |
| `src/lisp/eval.c3` | `register_primitives` section (~line 1770) | Add fiber, resume, yield primitives |
| `src/lisp/primitives.c3` | End of file | Add prim_fiber, prim_resume, prim_yield implementations |
| `src/lisp/tests.c3` | End of run_advanced_tests (~line 1718) | Add fiber-specific tests |
| `src/entry.c3` | Line 660 | Add run_fiber_tests() call |
| `stdlib/stdlib.lisp` | Line 159 | Rename `yield` macro to `stream-yield` |

### Files to Delete (Phase 4)

| File | Reason |
|------|--------|
| `src/continuation.c3` | Replaced by fiber.c3; PromptStack/PromptFrame never used by interpreter |
| `src/delimited.c3` | Replaced by fiber-based implementation; never used by interpreter |

### Files to Gut (Phase 4)

| File | Change |
|------|--------|
| `src/context.c3` | Keep `context_capture`/`context_restore` and `get_stack_pointer`/`get_frame_pointer`. Remove `StackSegment`, `ContinuationContext`, stack_segment_*, capture/restore_continuation_context |

---

## 6. Implementation Sequencing

```
Phase 0 (1 day):  Create fiber.c3, build, verify compilation + unit tests
Phase 1 (2 days): Fiber-based reset/shift with toggle flag
Phase 2 (3 days): Fiber-based handle/signal/resolve with toggle flag
Phase 3 (1 day):  User-facing fiber/yield/resume primitives
Phase 4 (1 day):  Remove replay mechanism, delete unused files
Phase 5 (later):  Scheduler for with-fibers/spawn/join
```

Phases 1 and 2 are the critical ones. The toggle flag (`use_fiber_continuations`) provides a safety net: if anything breaks, flip the flag and all existing tests pass on the old path while the fiber path is debugged.

---

## 7. Deferred Work (must be done, not yet scheduled)

### D1: MXCSR + x87 FPU Control Word Save/Restore
**What**: `stack_context_switch` currently skips saving MXCSR (SSE control/status) and x87 FPU control word. These are callee-saved per SysV ABI.
**Why deferred**: C3 inline asm doesn't support `stmxcsr`/`ldmxcsr`/`fnstcw`/`fldcw`. Need to either encode as raw bytes or create a small C helper file (e.g., `csrc/context_helpers.c`) that exports these as functions callable from C3.
**Risk if not done**: If any C library called from Omni (e.g., libtorch, libm) changes FPU rounding mode, the change could leak across coroutine switches. Low probability but hard to debug.
**When**: Before Phase 2 (effects on fibers), since effect handlers may call FFI code that modifies FPU state.
**How**: Create `csrc/fpu_save.c` with `void fpu_save(uint32_t* mxcsr, uint16_t* x87cw)` and `void fpu_restore(uint32_t mxcsr, uint16_t x87cw)`, link via C3's extern mechanism.

### D2: Stack Overflow Detection (SIGSEGV handler + sigaltstack)
**What**: When a coroutine overflows its 64KB stack, it hits the guard page and crashes with SIGSEGV. Currently this is an unrecoverable crash.
**Why deferred**: Requires installing a signal handler on an alternate stack (`sigaltstack`), catching SIGSEGV, determining if the faulting address is in a guard page, and converting to an Omni error.
**Risk if not done**: Deeply recursive Omni code on a coroutine stack will segfault instead of producing a helpful error message.
**When**: After Phase 3 (user-facing fibers), before any production use. Users writing recursive code in fibers will hit this.
**How**: Install `sigaltstack` at startup. Register SIGSEGV handler. In handler: check if fault address falls in any active coroutine's guard page region. If yes, mark coro as DEAD and longjmp to a recovery point. If no, re-raise the signal.

### D3: Cooperative Scheduler (with-fibers / spawn / join)
**What**: The test files `tests/test_with_fibers.lisp` expect a scheduler that can run multiple fibers cooperatively: `with-fibers` creates a scope, `spawn` adds a fiber to the run queue, `join` waits for a fiber to complete.
**Why deferred**: Requires a run queue, round-robin scheduling, and blocking semantics. Independent of the core continuation unification.
**When**: Phase 5, after all core phases are complete and stable.
**How**: `FiberScheduler` struct with circular buffer run queue. `spawn` adds to queue. `join` suspends current coro until target completes (tracked via completion callbacks). `with-fibers` runs the scheduler loop until all fibers complete.

### D4: CSP Channels (chan / send / recv)
**What**: `tests/test_with_fibers.lisp` also expects buffered channels for inter-fiber communication.
**Why deferred**: Depends on the scheduler (D3). Channels need blocking send/recv which requires the ability to suspend a fiber and wake it when the channel is ready.
**When**: After D3 (scheduler).
**How**: `Channel` struct with bounded ring buffer. `send` writes to buffer or suspends if full. `recv` reads from buffer or suspends if empty. Suspended fibers are added to per-channel wait queues.

---

### Critical Files for Implementation
- `/home/heefoo/Documents/code/Omni/src/lisp/jit.c3` - Contains all effect/continuation dispatch: jit_handle_impl, jit_perform_impl, jit_apply_continuation, jit_exec_resolve. This is where the fiber-based alternatives must be wired in alongside the existing replay mechanism.
- `/home/heefoo/Documents/code/Omni/src/lisp/value.c3` - Contains Continuation struct (line 251), CapturedCont (line 1738), EffectHandler (line 1723), Interp struct with all replay state (lines 1941-1984). All data structures need fiber fields added.
- `/home/heefoo/Documents/code/Omni/src/context.c3` - Contains the existing x86_64 assembly context_capture/context_restore (lines 106-183) that serve as the reference for writing fiber_context_switch. Also contains StackSegment and stack copy utilities to be repurposed for fiber stack cloning.
- `/home/heefoo/Documents/code/Omni/src/continuation.c3` - Contains the unused PromptStack/PromptFrame/Continuation scaffolding. Must be understood to determine what, if anything, to preserve versus replace with the fiber implementation.
- `/home/heefoo/Documents/code/Omni/src/lisp/tests.c3` - Contains all effect/continuation tests (lines 1507-1718) that define the behavioral contract the fiber implementation must satisfy. Every existing test must continue passing.