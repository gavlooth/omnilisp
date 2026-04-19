# DESTINATION_ARENA_PLAN Part 01

Source: `memory/DESTINATION_ARENA_PLAN.md`

# Architectural Plan: Destination-Passing Style (Scratch vs. Destination Arenas)

> Status note (2026-03-07): this document is architecture intent and deep
> rationale. For current validated runtime status, `memory/CHANGELOG.md` is the
> source of truth.

**Date**: March 3, 2026
**Target**: Omni Lisp Runtime
**Goal**: Eliminate $O(N)$ deep copies (`copy_to_parent`) and $O(1)$ memory leaks (`scope_adopt`) when returning values from functions, while strictly preserving Region-Based Reference Counting (RC) and forbidding Garbage Collection.

---

## 1. The Core Problem

Currently, the Omni runtime allocates both **temporary calculations** (garbage) and **return values** (escaping data) into the same `ScopeRegion` (Bump Allocator chunk).

When a function call completes, the runtime faces a dilemma:
* **The `scope_adopt` Leak**: If it merges the chunk into the parent scope (O(1)), it leaks all the temporaries into the parent's lifetime.
* **The `copy_to_parent` Bottleneck**: To avoid leaking, it must allocate a new copy of the return value in the parent scope and perform a recursive deep copy (O(N)), which thrashes CPU cache and burns cycles.
* **The RC/Closure Conflict**: If a closure captures the child scope (`RC > 1`), we cannot `scope_adopt` at all, or we break the closure's memory isolation.

## 2. The Solution: Destination-Passing Arenas

We will generalize an optimization already partially present in the codebase (`interp.escape_scope`).

Instead of a function allocating its return value in its own scope and then copying it up, the **caller passes down its own scope as the Destination Arena**.
* The **Scratch Arena** (`current_scope`): Used by the callee for all intermediate, temporary allocations.
* The **Destination Arena** (`dest_scope`): The caller's scope, used *only* for the final return value.

When the callee finishes:
* The return value is already perfectly constructed in the parent's arena.
* The callee's Scratch Arena is dropped via `scope_release`.
    * If `RC == 1`: The Scratch Arena is instantly destroyed (O(1)). All temporaries are vaporized.
    * If `RC > 1`: A closure captured it. The Scratch Arena survives as an independent heap island.
* **Result**: Zero deep copies. Zero leaked temporaries. Perfect RC compliance.

---

## 3. Implementation Phases

### Phase 1: Architectural Foundation (Scope & Interp updates)
**Target Files**: `src/scope_region.c3`, `src/lisp/value.c3`, `src/lisp/jit_common.c3`

1. **Rename/Generalize `escape_scope`**:
   The current `interp.escape_scope` is used specifically for TCO loop accumulators. This must be renamed/generalized to `interp.dest_scope` (Destination Scope).
2. **Context Passing**:
   Ensure that at every function entry point (`apply_closure`, JIT function boundaries), the interpreter explicitly sets:
   * `interp.dest_scope = interp.current_scope` (The parent becomes the destination)
   * `interp.current_scope = scope_create(parent)` (A fresh scratch arena is born)
3. **Restore Semantics**:
   Ensure `dest_scope` is properly restored when popping the call stack, just as `current_scope` is.

### Phase 2: Tail-Position Allocation
**Target Files**: `src/lisp/eval.c3`, `src/lisp/primitives.c3`

To know *when* to allocate into `dest_scope`, the evaluator must track **Tail Position**.
1. **Tail Position Flag**:
   When `eval_expr` evaluates the last expression in a `begin` block, or the return expression of a `lambda`, it should flag that allocation is in "tail position".
2. **Targeted Allocation**:
   Constructors like `make_cons`, `make_string`, `make_int` etc., need to allocate from `dest_scope` instead of `current_scope` when building the final return value.
   * *Alternative Approach*: Instead of a strict AST tail-position flag, functions that build complex return structures can explicitly switch `interp.current_scope = interp.dest_scope` just before constructing the final return object.

### Phase 3: Eradicating `copy_to_parent`
**Target Files**: `src/lisp/eval.c3`, `src/lisp/jit_jit_helper_functions.c3`

1. **Remove O(N) Copies**:
   Search for all instances of `copy_to_parent(result, interp)`. Because `result` was constructed directly in `dest_scope`, these calls become a no-op.
2. **Simplify Function Returns**:
   ```c
   // OLD WAY
   Value* result = eval_body();
   Value* copied = copy_to_parent(result, interp);
   scope_release(call_scope);
   return copied;

   // NEW WAY
   Value* result = eval_body(); // Allocated directly into dest_scope
   scope_release(call_scope);   // Instantly vaporizes temporaries
   return result;               // Already safe!
   ```

### Phase 4: Deprecation and Cleanup
**Target Files**: `src/scope_region.c3`, `src/lisp/value.c3`

1. **Deprecate `scope_adopt`**:
   Remove `main::scope_adopt` from `scope_region.c3`. It is no longer necessary and removes the risk of Region Leaks.
2. **Remove `make_cons_escape`**:
   The complex accumulator logic in `value.c3` (`make_cons_escape`) can be removed. The new generalized Destination Arena naturally handles O(1) list construction without special-case TCO logic.
3. **Deprecate `copy_to_parent`**:
   Remove the entire `copy_to_parent` graph-traversal function from `eval.c3`.

---

## 4. Edge Cases & Considerations

* **Nested Calls `(f (g x))`**: When evaluating arguments (like `(g x)`), `g`'s destination arena is the scratch arena of `f`. This is correct, because the result of `g` is a temporary to `f`. The nesting naturally routes memory to the exact lifetime that requires it.
* **Scalars**: `INT`, `DOUBLE`, and `BOOL` are so small that it is equally fast to copy them by value as it is to pre-allocate them. The primary target for destination-allocation is heap objects (`CONS`, `STRING`, `CLOSURE`, `ARRAY`).
* **Closures Returning Closures**: If a function returns a closure, the closure object itself is allocated in the `dest_scope`, but its `env_scope` RC is incremented. This works perfectly under the new model without dangling pointers.

---

## 5. Revision (Dialectical Addendum)

### Antithesis
The plan is directionally strong but over-claims in a few places for the current Omni runtime architecture:

1. **`copy_to_parent` cannot be fully removed yet**
   It is currently used as a boundary-promotion primitive for more than return values (global define/set promotion, coroutine yield crossings, REPL scope boundaries, persistent mutable boxes).

2. **`scope_adopt` cannot be immediately removed**
   Today it is still a key O(1) mechanism for selective return/lifetime transfer and TCO-related scope flow. Removing it prematurely risks both performance regressions and correctness regressions.

3. **Tail-position alone is insufficient for safe destination allocation**
   Allocation site is not enough; pointer provenance must be safe transitively. A destination-allocated wrapper that still points into scratch memory is invalid.

4. **Stack-context boundaries must be first-class in the plan**
   Reset/shift/handle/coroutine suspend-resume paths require explicit save/restore semantics for scope-routing state.

### Synthesis
Adopt Destination Arenas as the primary strategy, but with a constrained rollout that preserves RC guarantees and ASAN-clean behavior:

1. **Reframe `copy_to_parent` as `promote_across_boundary` conceptually**
   Keep it for mandatory lifetime crossings; reduce it in return paths where destination construction is proven safe.

2. **Retain `scope_adopt` as a controlled optimization**
   Narrow usage to validated cases while destination routing coverage expands. Remove only after empirical parity.

3. **Roll out by object class and boundary type**
   - Phase A: `CONS`/list accumulators and known fold/reverse/map pipelines.
   - Phase B: selected aggregate constructors (`STRING`/`ARRAY`) where destination ownership is explicit.
   - Phase C: closure/env-sensitive returns only under strict provenance checks.

4. **Codify safety invariants**
   - Any value returned to caller scope must be fully reachable from caller/root scopes.
   - No destination object may retain references to releasing scratch scope unless independently RC-retained.
   - Suspend/resume paths must restore scope-routing state deterministically.

5. **Measure before deleting**
   Add counters for:
   - `copy_to_parent` calls by tag/path,
   - `scope_adopt` bytes/dtor transfer,
   - retained scope lifetimes after call boundaries.
   Use these metrics to decide which copy/adopt paths to eliminate next.

This synthesis preserves the no-GC, no-stop-the-world model while still moving toward the intended zero-copy return architecture.

## 6. The Final Synthesis (Structural Provenance & Unification)

### The Dialectical Progression
* **Thesis:** Implement pure Destination Arenas. Destroy the Scratch Arena instantly. Zero deep copies.
* **Antithesis:** Pure Destination Arenas cause dangling pointers if destination objects contain references to scratch memory. A hybrid model (keeping `copy_to_parent` and `scope_adopt`) is needed for safety and edge cases (coroutines, mutability).
* **Anti-Antithesis:** A hybrid memory model creates an untestable, exploding state-space. If we dynamically check provenance and fall back to `copy_to_parent` anyway, we haven't solved the $O(N)$ performance problem; we've just hidden it and made the runtime permanently brittle.

### The Final Synthesis: Static Provenance via IR and Unified Allocation
To achieve O(1) returns *without* the runtime complexity of a hybrid model, we must solve the Pointer Provenance problem **structurally**, rather than dynamically. We cannot rely on runtime heuristics or piecemeal rollouts.

**1. The "Immediate / Pointer" Bipartition (Eliminating Provenance Checks)**
The root of the provenance problem is that `Value` can be a pointer to a heap object (like a String) living in a doomed Scratch Arena.
To fix this, we enforce a strict bipartition in the runtime:
* **Immediates** (`INT`, `DOUBLE`, `BOOL`, short strings via SSO): These have no provenance. They are copied by value in O(1) time. They can be freely passed between scratch and destination scopes.
* **Heap Objects** (`CONS`, long `STRING`, `CLOSURE`): These *must* be allocated in their final destination arena from the moment of birth.

**2. Compile-Time Escape Analysis (The JIT's Responsibility)**
We move the burden of deciding "which arena" from the runtime evaluator to the Compiler/JIT.
* The JIT performs a basic escape analysis pass on the AST.
* If a heap object is created and bound to a local variable that never escapes, the JIT emits a `make_cons_scratch` opcode.
* If a heap object is created and returned, passed to an external function, or captured, the JIT emits a `make_cons_dest` opcode.
* **The Result:** The runtime does zero dynamic provenance checking. It blindly executes the opcode, and the architecture guarantees no dangling pointers exist when the scratch arena is destroyed.

**3. The Continuation Boundary Protocol**
To handle the complex lifecycle of Delimited Continuations (`shift`/`reset`) and global mutability:
* Continuations and global `set!` operations are explicitly defined as **Arena Escapes**.
* Whenever a value crosses these strict boundaries, the JIT emits a specialized `deep_copy_to_root` or `copy_to_continuation` instruction.
* This isolates the $O(N)$ penalty to explicit, heavy control-flow operations, rather than penalizing standard function returns.

**4. The Clean Break (No Hybrid Rollout)**
Instead of a Phase A/B/C rollout that leaves the codebase in "Permanent Beta", the migration is done as a single, unified cutover:
1. Implement the Escape Analysis pass in the JIT/Compiler.
2. Introduce the `_dest` and `_scratch` variants of all heap constructors.
3. Switch the evaluator to use the new opcodes.
4. **Delete** `scope_adopt` and the implicit `copy_to_parent` entirely in the same PR.

This synthesis provides the extreme performance of the original Thesis, addresses the safety concerns of the Antithesis, and avoids the runtime complexity of the hybrid model by shifting the workload to the compiler.

---

## 7. Revision II (Dialectical Continuation)

### Antithesis II
Section 6 is conceptually strong, but it assumes a level of static certainty that does not fully hold in Omni today:

1. **Pure compile-time escape analysis is incomplete for dynamic Lisp surfaces**
   Macros, runtime `eval`, module loading, FFI callbacks, and effect handlers can generate or route values in ways that are not fully knowable at JIT compile time.

2. **A hard immediate/pointer split is a major runtime redesign**
   The proposed bipartition is not a local optimization. It changes core `Value` semantics, constructor contracts, and likely ABI-level behavior across interpreter/JIT/AOT paths.

3. **Single-PR cutover is operationally high risk**
   Replacing allocation policy, removing fallback primitives, and deleting current safety rails in one migration step increases regression blast radius beyond production-safe bounds.

4. **Eliminating all dynamic checks is not realistic for boundary crossings**
   Continuations, coroutines, and global mutation are inherently dynamic boundaries. Some explicit runtime boundary handling remains necessary even with better compile-time provenance.

### Synthesis II
Unify toward destination allocation as the default architecture, while retaining explicit boundary mechanisms where dynamic behavior requires them:

1. **Adopt a two-layer provenance model**
   - **Static-first**: JIT/AOT emit destination-aware constructors where analysis is confident.
   - **Boundary-safe runtime**: keep explicit promotion at dynamic boundaries (`eval`, continuation resume, coroutine yield, global store).

2. **Narrow, not abolish, promotion primitives**
   Retain `copy_to_parent` (or a renamed boundary primitive) only for dynamic escape boundaries; remove it from normal return paths once destination construction is verified.

3. **Constrain `scope_adopt` to audited hot paths**
   Keep it as an optimization tool under strict invariants and telemetry, then deprecate case-by-case as destination construction coverage reaches parity.

4. **Use staged cutovers with hard acceptance gates**
   - Stage 1: instrument copy/adopt traffic and retained-lifetime inflation.
   - Stage 2: enable destination constructors for list-heavy and tail-heavy paths.
   - Stage 3: expand to aggregates and closure-bearing returns.
   - Stage 4: delete dead fallback paths only after ASAN and stress suites remain clean for each stage.

5. **Define non-negotiable production criteria**
   - No stop-the-world GC introduced.
   - RC/lifetime invariants preserved under suspend/resume.
   - ASAN clean on unified + compiler test suites.
   - No statistically significant throughput regression on existing hot benchmarks.

This continuation preserves the long-term destination-arena objective while avoiding a brittle all-at-once rewrite in a dynamic runtime.

---

## 8. Revision III (Dialectical Edge: The Illusion of Safe Boundaries)

### Antithesis III
Synthesis II strikes a pragmatic operational balance, but it relies on a fundamental architectural contradiction that will eventually manifest as memory unsafety or severe performance cliffs:

1. **The "Two-Layer" model guarantees a "Worst of Both Worlds" edge case.**
   If the JIT emits a destination-aware constructor (because it statically *thinks* the value is a local return), but that value is subsequently passed into a dynamic boundary (like `eval` or a coroutine yield) that the JIT couldn't see, the boundary primitive (`promote_across_boundary`) now has to deep-copy an object that is *already* in the destination arena. We pay the cost of static routing *and* the $O(N)$ penalty of dynamic copying, while risking double-frees if the boundary logic assumes it is copying from scratch memory.

2. **ASAN is a testing tool, not a formal verification.**
   Relying on "ASAN clean test suites" (Synthesis II #4) to validate a Two-Layer provenance model in a dynamic Lisp with coroutines is a false sense of security. State-space coverage in dynamic languages with effects is notoriously poor. If the structural invariants are complex enough to require a two-layer model, ASAN will eventually miss a runtime edge case where a scratch pointer escapes into a destination closure.

3. **`scope_adopt` telemetry will just prove it must be deleted.**
   Constraining `scope_adopt` to "audited hot paths" (Synthesis II #3) ignores the fundamental math of bump allocators: *any* adoption leaks temporaries. Telemetry will simply show that hot paths are exactly where temporary leakage is most disastrous (causing OOM in tight loops).

### Synthesis III
To eliminate the contradiction of the Two-Layer model without reverting to the impossible pure-static model, we must change the physical memory structure itself, rather than just the routing logic. We move from Destination Arenas to **Segmented Capabilities**.

1. **The "Linear Value" Guarantee for Cross-Boundary Safety**
   Instead of trying to guess whether a value belongs in Scratch or Destination, we introduce a new class of memory: **The Linear Object**.
   * A Linear Object is allocated in a specialized `transfer_scope`.
   * It is guaranteed to contain *no pointers* to any other scope (all its children must also be Linear Objects or Immediates).
   * **The Rule:** Only Linear Objects are allowed to cross dynamic boundaries (coroutines, `eval`, `set!`). If a standard Scratch or Destination object attempts to cross a boundary, it is dynamically converted into a Linear Object.

2. **Kill `scope_adopt` Immediately**
   Do not stage its deprecation. The math of bump allocators means `scope_adopt` is fundamentally incompatible with long-running, memory-stable hot paths. Replace it entirely with Destination Arenas for known returns, and Linear Object transfers for unknown boundaries.

3. **The "Read-Only" Scratch Fallback**
   To solve the JIT's blind spots without expensive deep copies: if a dynamic boundary is hit and a value in the Scratch Arena must escape, do not copy it. Instead, **freeze the Scratch Arena**. Its RC is incremented, and it becomes a read-only floating island.
   * *Why this works:* It trades an $O(N)$ deep copy for an $O(1)$ memory leak (the temporaries trapped in the frozen arena). But unlike `scope_adopt`, this leak is **bounded** by the RC lifecycle of the escaped object, not permanently stapled to the parent!

By introducing Linear Objects for safe boundary crossing and Frozen Scratch Arenas as the ultimate $O(1)$ fallback, we achieve the dynamic safety of Synthesis II without paying the $O(N)$ deep-copy penalty or the permanent memory-leak penalty of `scope_adopt`.

---

## 9. Revision IV (Engineering Review of Section 8)

### Scope
This review evaluates Section 8 against Omni's non-negotiables:
- No stop-the-world garbage collection.
- Strict RC/lifetime ownership.
- Predictable reclamation and bounded memory growth in hot paths.

### Decision Summary
Section 8 is **rejected as written** for implementation.
Its core proposals ("Linear Objects" and "Frozen Scratch Arenas") increase architectural complexity and weaken predictable memory behavior under dynamic workloads.

### Concrete Runtime Anchors
The following code paths are the current ground truth and must remain valid through any redesign:
- `scope_adopt` behavior and chunk transfer: `src/scope_region.c3::scope_adopt`.
- boundary promotion primitive: `src/lisp/eval.c3::copy_to_parent`.
- list escape optimization currently in production: `src/lisp/value.c3::make_cons_escape`, `src/lisp/value.c3::make_cons`.
- call-scope orchestration and adopt/copy branching: `src/lisp/jit_jit_helper_functions.c3::jit_eval_in_single_scope`, `src/lisp/jit_jit_helper_functions.c3::jit_eval_in_call_scope`.
- top-level eval boundary crossing: `src/lisp/eval.c3::run`.
- coroutine yield boundary crossing: `src/lisp/primitives.c3::prim_resume`.
- global mutation/definition promotion: `src/lisp/jit_jit_helper_functions.c3::jit_eval_set`, `src/lisp/jit_jit_helper_functions.c3::jit_eval_define`.

### Technical Critique
1. **Frozen Scratch Arenas are lifetime inflation by design.**
   Even if RC-bounded, frozen scratch regions retain unrelated temporaries until the escaped value dies. This breaks deterministic memory density under sustained hot-loop traffic.

2. **Linear Objects create a second memory semantics stack.**
   Introducing a transfer-only class requires duplicated invariants across interpreter/JIT/AOT/effects/coroutines/FFI. This is a large semantic tax for uncertain net gain.

3. **Boundary conversion is still runtime deep-copy pressure.**
   Section 8 shifts copy cost location; it does not remove it. In dynamic code paths (`eval`, continuations, coroutines), conversion remains O(N) in the worst case.

4. **Freeze semantics conflict with mutable runtime behavior.**
   Mutable environments and `set!` make "read-only frozen island" semantics fragile and implementation-heavy, especially once closures cross dynamic boundaries.

### Acceptance/Reject Criteria (Explicit)
Any destination-arena evolution is acceptable only if all are true:
- ASAN clean on unified + compiler suites.
- No unbounded retained-memory growth in long-running loop benchmarks.
- No regression in continuation/coroutine correctness.
- No new memory class with independent lifecycle semantics.

Any proposal is rejected if any one is true:
- Requires freezing scratch scopes as a normal boundary mechanism.
- Requires dual object regimes for regular value flow.
- Relies on hidden boundary conversions without explicit observability.
