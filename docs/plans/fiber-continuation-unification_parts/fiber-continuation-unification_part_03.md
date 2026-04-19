# fiber-continuation-unification Part 03

Source: `docs/plans/fiber-continuation-unification.md`

### PHASE 3: Implement User-Facing Fibers — COMPLETE

**Goal**: Implement `fiber`, `yield`, `resume` primitives that the test files expect.
**Status**: Complete. 994 tests pass (0 failures). Primitives: `fiber`, `resume`, `yield`, `fiber?`.
- `yield` macro renamed to `stream-yield` in stdlib (fiber primitive takes precedence)
- test_yield.lisp and test_nested_fiber.lisp (Test 1, 3) pass
- test_nested_fiber.lisp Test 2 (`with-fibers`/`spawn`/`join`) deferred to Phase 5 (scheduler)

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

Note: This conflicts with the historical `yield` macro in stdlib (now `stream-yield`), canonical form: `(define [macro] stream-yield (syntax-match ([val] (template (shift k (cons (insert val) k))))))`. `yield` should be reserved for the fiber primitive, while `stream-yield` keeps generator semantics (`(cons val k)`). This preserves semantic separation between generator streams and suspend/resume fibers.

#### 3.3.3 Verification

Run the test files:
- `tests/test_yield.lisp`
- `tests/test_nested_fiber.lisp`

The `tests/test_with_fibers.lisp` uses `with-fibers`, `spawn`, `join`, and `chan` which require a scheduler -- defer to Phase 4.

---

### PHASE 4: Cleanup and Remove Replay Mechanism — COMPLETE

**Goal**: Remove the replay code path, simplify the Interp struct, delete unused scaffolding.
**Status**: Complete. All replay-path code removed. 878 unified + 77 compiler tests pass, 0 failures.

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

Either delete entirely (if all continuations are fiber-based) or reduce to a minimal struct if still required.

#### 3.4.4 Delete or gut unused files

- `/home/heefoo/Documents/code/Omni/src/continuation.c3`: Delete or reduce to just the `PromptTag`/`ContinuationId` typedefs if still referenced.
- `/home/heefoo/Documents/code/Omni/src/delimited.c3`: Delete entirely (its tests call the unused reset_begin/shift_capture API).
- `/home/heefoo/Documents/code/Omni/src/context.c3`: Keep `context_capture`/`context_restore` (still useful as lightweight setjmp/longjmp). Remove `capture_continuation_context`/`restore_continuation_context` and `stack_segment_*` functions.

#### 3.4.5 Update entry.c3

Remove `run_delimited_tests()` call (line 668) since the test functions in delimited.c3 will be deleted.

#### 3.4.6 Verification

All 927 existing tests pass with `use_fiber_continuations` flag removed (fiber path is the only path).

---

### PHASE 5: Scheduler for `with-fibers` / `spawn` / `join` — DEFERRED (NOT NEEDED)

**Status**: Deferred indefinitely. The existing `fiber`/`resume`/`yield`/`fiber?` primitives provide sufficient building blocks. A cooperative scheduler (`with-fibers`/`spawn`/`join`) and channels (`chan`/`send`/`recv`) can be built in userland if ever needed. The effects + fibers pattern covers the use cases that channels would address.

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
Phase 5 (deferred): Scheduler not needed — existing primitives sufficient
```

Phases 1 and 2 are the critical ones. The toggle flag (`use_fiber_continuations`) provides a safety net: if anything breaks, flip the flag and all existing tests pass on the old path while the fiber path is debugged.

---

## 7. Deferred Work (must be done, not yet scheduled)

### D1: MXCSR + x87 FPU Control Word Save/Restore — COMPLETE
**Implementation**: Created `csrc/stack_helpers.c` with `fpu_save()`/`fpu_restore()` using GCC inline asm for stmxcsr/ldmxcsr/fnstcw/fldcw. Called from `coro_switch_to`, `coro_resume`, `coro_suspend` wrapper functions via C3 extern. FPU state saved into StackContext (offset 64: mxcsr, offset 68: x87cw) before each context switch and restored after.

### D2: Stack Overflow Detection (SIGSEGV handler + sigaltstack) — COMPLETE
**Implementation**: `csrc/stack_helpers.c` provides sigaltstack + SIGSEGV handler. Guard pages registered per-coro in `coro_create`, unregistered in `coro_destroy`/pool shutdown. `stack_guard_protected_switch()` wraps sigsetjmp + context switch — if coro overflows, siglongjmp recovers. Recovery stack supports nested switches (MAX_RECOVERY_DEPTH=64). Coro marked CORO_DEAD on overflow. All jit.c3 call sites and prim_resume check CORO_DEAD and return "stack overflow" errors.

### D3: Cooperative Scheduler (with-fibers / spawn / join) — NOT IMPLEMENTING
**Decision**: Not needed. The existing `fiber`/`resume`/`yield`/`fiber?` primitives provide sufficient building blocks. A cooperative scheduler can be built in userland if ever needed. The effects + fibers pattern covers the scheduling use cases.

### D4: CSP Channels (chan / send / recv) — NOT IMPLEMENTING
**Decision**: Not needed. The effects + fibers pattern covers inter-fiber communication use cases without dedicated channel primitives.

---

### Critical Files for Implementation
- `/home/heefoo/Documents/code/Omni/src/lisp/jit.c3` - Contains all effect/continuation dispatch: jit_handle_impl, jit_perform_impl, jit_apply_continuation, jit_exec_resolve. This is where the fiber-based alternatives must be wired in alongside the existing replay mechanism.
- `/home/heefoo/Documents/code/Omni/src/lisp/value.c3` - Contains Continuation struct (line 251), CapturedCont (line 1738), EffectHandler (line 1723), Interp struct with all replay state (lines 1941-1984). All data structures need fiber fields added.
- `/home/heefoo/Documents/code/Omni/src/context.c3` - Contains the existing x86_64 assembly context_capture/context_restore (lines 106-183) that serve as the reference for writing fiber_context_switch. Also contains StackSegment and stack copy utilities to be repurposed for fiber stack cloning.
- `/home/heefoo/Documents/code/Omni/src/continuation.c3` - Contains the unused PromptStack/PromptFrame/Continuation scaffolding. Must be understood to determine what, if anything, to preserve versus replace with the fiber implementation.
- `/home/heefoo/Documents/code/Omni/src/lisp/tests.c3` - Contains all effect/continuation tests (lines 1507-1718) that define the behavioral contract the fiber implementation must satisfy. Every existing test must continue passing.
