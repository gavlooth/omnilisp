# ESCAPE_SCOPE_EXPLORATIONS Part 01

Source: `memory/ESCAPE_SCOPE_EXPLORATIONS.md`

# Escape-Scope Optimization — Explorations

Status: **Exploration 1 IMPLEMENTED (Session 63)** — scope_gen stamp in make_cons. Remaining explorations for future work.

## Current State (Session 63)

Three-tier hierarchy: parent_scope → result_scope → call_scope. `make_cons` uses O(1) `scope_gen` stamp comparison for escape detection. `copy_to_parent` retains `is_in_scope()` chunk walk (because `scope_adopt` moves chunks without updating stamps). All 1065 tests pass.

---

## Exploration 1: Scope Generation Stamp (eliminate chunk-list walk)

### Original idea: Pointer bit-tagging (REJECTED)

Encode `ScopeRegion.generation % 8` in low 3 bits of `Value*`. Rejected because:
- Every pointer consumer needs tag stripping — massive surface area for bugs
- Continuations/coroutines copy stack frames containing tagged pointers — need fixup after clone
- 3 bits = 8 generations, frequent collisions in deep nesting

### Refined idea: `scope_gen` field in Value struct (PREFERRED)

**What**: Add a `uint scope_gen` field to the `Value` struct. Stamp it at allocation time with the allocating scope's generation. Replace `is_in_scope()` chunk-list walk with a single `uint == uint` comparison.

**Key insight**: Value struct has 7 bytes of padding between `tag` (1 byte, `char`) and the union (8-byte aligned). A `uint` (4 bytes) fits in this padding with **zero size increase** — Value stays 40 bytes.

**Struct layout**:
```
Current:  tag(1) + [pad 7] + union(32) = 40 bytes
With gen: tag(1) + [pad 3] + scope_gen(4) + union(32) = 40 bytes  ← SAME SIZE
```

**Where it connects**:
- `value.c3:595` — Add `uint scope_gen;` field after `tag` in `Value` struct
- `value.c3` `alloc_value()` — Stamp `v.scope_gen = interp.current_scope.generation` at allocation
- `value.c3:787-803` — `make_cons` hot path: `is_in_scope(cdr, escape_scope)` becomes `cdr.scope_gen == interp.escape_scope.generation`
- `eval.c3` `copy_to_parent` — skip check: `v.scope_gen != interp.releasing_scope.generation` → return as-is
- `scope_region.c3` — `is_in_scope()` still available as fallback but no longer on hot path

**Code sketch**:
```c3
// alloc_value — stamp generation
fn Value* alloc_value(Interp* self) {
    Value* v = ...bump alloc...;
    v.scope_gen = self.current_scope.generation;
    return v;
}

// make_cons — O(1) check, no chunk walk
fn Value* make_cons(Interp* interp, Value* car, Value* cdr) @inline {
    if (interp.escape_scope != null &&
        interp.current_scope != interp.escape_scope &&
        (cdr.tag == NIL || cdr.scope_gen == interp.escape_scope.generation)) {
        return make_cons_escape(interp, car, cdr);
    }
    // ...normal path...
}

// copy_to_parent — O(1) skip
fn Value* copy_to_parent(Value* v, Interp* interp) {
    if (interp.releasing_scope != null &&
        v.scope_gen != interp.releasing_scope.generation) {
        return v;  // not in dying scope, skip
    }
    // ...existing copy logic...
}
```

**Tradeoffs**:
- Pro: O(1) check — single `uint == uint` comparison, no pointer manipulation, no chunk walking
- Pro: Zero size increase — fits in existing struct padding
- Pro: No pointer bit-stuffing — no tag stripping needed anywhere, zero invasiveness
- Pro: Works at all dereference sites (make_cons, copy_to_parent) with same mechanism
- Con: False positives if two unrelated scopes reuse the same generation number (freelist recycling). **But false positives are safe** — they cause unnecessary escape-allocation, which is correct behavior, just slightly wasteful
- Con: False negatives impossible as long as escape_scope.generation is set correctly
- Risk: Generation counter must be monotonically increasing (not recycled). Currently `scope_create` increments it on recycle, which is correct. If generation wraps at `UINT_MAX` (~4 billion), collisions become possible but astronomically unlikely.

**Verdict**: **IMPLEMENTED (Session 63)**. O(1) hot path in make_cons, zero struct size increase. `copy_to_parent` still uses `is_in_scope` because `scope_adopt` moves chunks between scopes without updating stamps — values adopted from child scope retain child's generation but physically live in parent's chunks. Global monotonic counter (`g_scope_generation_counter`) ensures unique generations (initial per-scope counter caused collisions → segfault on fib).

---

## Exploration 2: JIT Accumulator Hint (skip two-scope setup)

**What**: Statically detect accumulator patterns in the JIT/parser. When a named-let binding starts as `'()` and the recursive call wraps it in `cons`, set a hint flag on the expression. `jit_eval_in_call_scope` only creates the two-scope hierarchy when the hint is present — otherwise uses a cheaper single-scope path.

**Where it connects**:
- `jit.c3:363-418` — `jit_eval_in_call_scope` currently ALWAYS creates result_scope + call_scope (2 scope_create calls + teardown)
- `jit.c3:338` — TCO trampoline scope creation
- Expression structs in `value.c3` — need a flag bit on `ExprLet` or `ExprLambda`
- Parser — pattern recognition during named-let parsing

**Tradeoffs**:
- Pro: Zero-risk — hint is advisory, runtime check in make_cons still catches missed patterns
- Pro: Saves 1 scope_create + scope_release per non-accumulator function call (~15 instructions each)
- Con: Only helps non-accumulator calls (accumulator calls need the two-scope anyway)
- Con: Static detection misses dynamic patterns (HOFs like `foldl` with lambda accumulators)

**Verdict**: Lowest-hanging fruit. Additive, safe, measurable. Should be first to implement.

---

## Exploration 3: Arena Zero-Copy Return (eliminate Step 2 copy)

**What**: Instead of copying the result from result_scope to caller's scope (Step 2, jit.c3:407-415), adopt result_scope's chunks into saved_scope via `scope_adopt()`. Result stays in place — O(1) instead of O(n).

**Where it connects**:
- `jit.c3:407-418` — Step 2 currently copies result then releases result_scope
- `scope_region.c3:370+` — `scope_adopt()` already implemented for Step 1

**Tradeoffs**:
- Pro: Eliminates the final O(n) copy entirely
- Con: result_scope contains ALL temporaries from function body, not just the accumulator. Adopting dumps garbage into parent.
- Possible heuristic: adopt when `result_scope.alloc_bytes` is small (dominated by accumulator); copy otherwise
- Con: Adopted garbage lives until parent scope dies — could extend lifetimes significantly for long-running parent scopes

**Verdict**: High reward but needs careful heuristic. Profile to see how much garbage the result_scope typically holds vs. the actual result.

---

## Exploration 4: Escape Binding Flag (narrow check surface)

**What**: Mark env bindings that are accumulators with an `ESCAPE` flag. `make_cons` only triggers escape path when `cdr` comes from an escape-flagged binding. Non-accumulator cons (argument lists, data construction) skip the check entirely.

**Where it connects**:
- `value.c3` — `Env` struct needs a flag per binding (or use a separate bitset)
- `value.c3:787` — `make_cons` condition narrows from "every cons" to "accumulator cons"
- `jit.c3` — let-binding evaluation sets flag when initial value is `'()` or nil

**Tradeoffs**:
- Pro: Narrows escape check from every cons to ~1-2 per loop iteration
- Con: Need to propagate flag through env lookups and into value metadata
- Con: Closures capturing accumulator bindings complicate flag propagation
- Risk: Medium — flag must be precise or we regress to O(n²)

**Verdict**: Good in theory, messy in practice. Closures and HOFs make binding-level tracking fragile. Revisit if profiling shows non-accumulator cons checks are significant overhead.

---

---

## Exploration 5: Dual Refcount — Selective Deferred Free (eliminate copy_to_parent)

**Status**: Validated via 4-round dialectic reasoning (deepseek thesis, groq antithesis, zai synthesis). Converged: dual refcount preferred over flat DAG.

**What**: Add a `value_rc` field to ScopeRegion alongside the existing `refcount` (renamed `structural_rc`). When a function returns a large result, skip `copy_to_parent` entirely — instead bump `value_rc` on the child scope and return the pointer directly. The child scope stays alive until both `structural_rc` AND `value_rc` reach zero.

**The core insight**: `copy_to_parent` exists because child scopes die at function return. If we keep the child scope alive (deferred free), there's nothing to copy. The dual refcount ensures the scope still dies deterministically — `structural_rc` drops when the call stack unwinds, `value_rc` drops when the caller's scope dies (taking the escaped values with it).

**Selective threshold**: Only defer for large scopes. Small scopes still copy (cheap, frees temporaries immediately). This avoids the space leak where a scope with 10MB of temporaries stays alive because one integer escaped.

**Struct changes**:
```c3
struct ScopeRegion {
    uint structural_rc;  // renamed from refcount — tracks parent→child hierarchy
    uint value_rc;       // NEW — tracks escaped value references to this scope
    // ... rest unchanged
}
```

**Function return path** (`jit_eval_in_call_scope`):
```c3
if (result_scope.alloc_bytes < DEFERRED_FREE_THRESHOLD) {
    // Small scope: copy result, free immediately (existing behavior)
    interp.releasing_scope = result_scope;
    result = copy_to_parent(result, interp);
    interp.releasing_scope = null;
    scope_release(result_scope);
} else {
    // Large scope: skip copy, defer free
    result_scope.value_rc++;
    // result points directly into result_scope — no copy needed
    // result_scope stays alive until value_rc drops to 0
}
```

**Scope release logic**:
```c3
fn void scope_release(ScopeRegion* scope) {
    if (--scope.structural_rc > 0) return;
    if (scope.value_rc > 0) return;  // still referenced by escaped values
    // ... existing cleanup (run dtors, free chunks, recycle)
}

fn void scope_value_release(ScopeRegion* scope) {
    if (--scope.value_rc > 0) return;
    if (scope.structural_rc > 0) return;  // still in hierarchy
    // ... existing cleanup
}
```

**Where it connects**:
- `scope_region.c3` — ScopeRegion struct: rename `refcount` → `structural_rc`, add `value_rc`
- `scope_region.c3` — `scope_release`: check both RCs before freeing
- `scope_region.c3` — New `scope_value_release`: called when caller's scope dies
- `jit.c3:363-418` — `jit_eval_in_call_scope`: selective copy vs defer based on `alloc_bytes`
- `eval.c3` — `copy_to_parent`: unchanged (still used for small scopes)
- All existing `scope_release` call sites: unchanged (structural_rc semantics preserved)

**Interaction with existing features**:

| Feature | Impact | Notes |
|---|---|---|
| TCO recycling | Safe | structural_rc drops at bounce; value_rc keeps scope if values escaped |
| Closures | Safe | Closure env_scope has its own RC; dual RC is orthogonal |
| Continuations | Medium risk | stack_ctx_clone copies values but not scopes; cloned values still point to original scope → value_rc must account for clones |
| scope_adopt | Safe | Adoption transfers structural ownership; value_rc stays independent |
| Escape-scope (make_cons) | No change | Escape cons goes to result_scope as before |
| scope_gen stamps | Compatible | Deferred scopes keep their generation; stamps remain valid |

**Key risk — space leak**:
A function returning a single integer from a scope with 10MB of temporaries keeps the entire scope alive. Mitigated by the threshold: only scopes above `DEFERRED_FREE_THRESHOLD` (e.g., 4KB) are deferred. Below that, copy is cheap and frees temporaries immediately.

**Why Flat DAG was rejected**:
Dialectic explored "one scope per call, no hierarchy, DAG with shared ownership." Fatal flaw: TCO + flat scopes = unbounded memory growth. A 1M-iteration loop retains 1M scopes. The dual refcount preserves hierarchical cleanup while selectively deferring only when beneficial.

**Open questions**:
- What should `DEFERRED_FREE_THRESHOLD` be? Needs profiling. Candidate: 4KB (one chunk).
- How does `value_rc` decrement? When the caller's scope dies, it needs to walk its "deferred children" list and decrement their `value_rc`. This requires tracking which child scopes were deferred — possibly a linked list on the parent scope.
- Should deferred scopes participate in scope_adopt? Probably not — they're detached from the hierarchy once structural_rc drops.

**Tradeoffs**:
- Pro: Eliminates O(n) copy for large results (lists, arrays built in loops)
- Pro: Deterministic — both RCs are precise, no GC needed
- Pro: Selective — small scopes still copy (no waste), large scopes defer (no copy)
- Con: Adds `value_rc` field (4 bytes) to ScopeRegion — negligible
- Con: Need to track deferred children for value_rc decrement — adds a linked list per parent scope
- Con: Space leak risk for large scopes with small results (mitigated by threshold)
- Con: Continuations need careful handling (cloned values reference original scope)

**Verdict**: Most promising path to eliminating copy_to_parent. The threshold-based selective approach avoids the space leak problem while targeting the expensive cases. Implement after profiling confirms copy_to_parent is a measurable bottleneck.

---

## Exploration 6: Closure Scratch Arena + RC (eliminate malloc/dtor overhead)

**Status**: Validated via 4-round dialectic reasoning (deepseek thesis, groq antithesis, zai synthesis). Converged: Variant C (per-scope arena + batch promote) preferred. Variant B rejected (dangling pointers with continuations).

**What**: Replace per-closure `malloc` + individual `ScopeDtor` registration with bump-allocated Closure structs in scope arenas. Keep refcounting for shared ownership. Batch-scan at scope death replaces individual destructor calls.

**The problem**: Closures are the ONLY value type that uses `malloc` for the Closure struct and registers a 24-byte `ScopeDtor` for cleanup. Every other heap-backed type (string, array, hashmap) also has this overhead, but closures are created far more frequently in HOF-heavy code. A `(map (lambda (x) (+ x 1)) lst)` creates n ephemeral closures — each one mallocs, registers a dtor, and frees at scope death.

### Variant A: Pool/Slab Allocator (NOT EXPLORED DEEPLY)

Pre-allocate fixed-size Closure slabs, free-list reuse. Standard approach but doesn't integrate with scope-region model.

### Variant B: Optimistic Scratch Arena (REJECTED)

**What**: Allocate Closure struct directly in call_scope via bump alloc. If closure escapes (captured by another closure, returned, stored in data structure), promote to malloc'd copy.

**Why rejected**: **Fatal flaw with continuations/coroutines.** `stack_ctx_clone` copies the entire stack (including pointers to Closure structs). If the scratch arena is freed when the scope dies, the cloned stack holds dangling pointers. Use-after-free risk is unacceptable — the same class of bug that caused the coroutine yield segfault (Session 62, P1 fix).

Concrete failure scenario:
```
1. (reset (lambda () (let (f (lambda (x) x)) (shift k (k f)))))
2. shift captures continuation → stack_ctx_clone copies stack
3. Cloned stack has pointer to Closure in call_scope's arena
4. call_scope dies → arena freed
5. Invoke continuation → cloned stack dereferences dangling Closure* → SEGFAULT
```

### Variant C: Per-Scope Arena + RC + Batch Promote (PREFERRED)

**What**: Bump-allocate Closure structs in the current scope's arena (not malloc). Keep refcount on Closure for shared ownership. At scope death, batch-scan all Closures in the scope: if `rc > 1`, promote (malloc + copy) to parent scope; if `rc == 1`, just free (the scope arena handles it).

**Key insight**: Most closures are ephemeral (rc stays 1, never shared). Only closures that escape (captured, returned, stored) get promoted. The batch scan at scope death replaces n individual dtor calls with a single sweep.

**Allocation path** (replaces `make_closure`):
```c3
fn Value* make_closure(Interp* interp, ...) {
    Value* v = interp.alloc_value();  // bump alloc in current_scope
    v.tag = CLOSURE;
    // Closure struct also bump-allocated in current_scope (NOT malloc)
    Closure* c = (Closure*)interp.current_scope.alloc(Closure.sizeof);
    c.refcount = 1;
    c.env = ...;
    c.params = ...;
    c.body = ...;
    c.env_scope = ...;
    v.closure = c;
    // NO dtor registration — batch scan handles cleanup
    return v;
}
```

**Scope death path** (replaces individual dtors):
```c3
fn void scope_release(ScopeRegion* scope) {
    // ... existing RC check ...
    // Batch scan: walk all Values in scope's chunks
    // For each CLOSURE value with rc > 1: promote Closure struct to parent
    // For each CLOSURE value with rc == 1: nothing to do (arena frees it)
    batch_promote_closures(scope, scope.parent);
    // ... existing chunk free ...
}

fn void batch_promote_closures(ScopeRegion* dying, ScopeRegion* target) {
    // Walk chunks, find CLOSURE values, check refcount
    for each Value* v in dying.chunks {
        if (v.tag == CLOSURE && v.closure.refcount > 1) {
            // Malloc a copy, update all references
            Closure* promoted = (Closure*)mem::malloc(Closure.sizeof);
            *promoted = *v.closure;
            v.closure = promoted;
            // Register dtor on target scope for the promoted copy
            scope_register_dtor(target, promoted, &closure_dtor);
        }
    }
}
```

**Interaction with existing features**:

| Feature | Impact | Notes |
|---|---|---|
| copy_to_parent CLOSURE case | Simplifies | Currently shares Closure* via refcount bump. Same mechanism, just Closure lives in arena instead of malloc |
| Continuations (stack_ctx_clone) | Safe | Cloned stack points to Closure in scope arena. If scope dies, batch_promote copies shared Closures to parent. Cloned stack's scope is kept alive by structural_rc from the continuation. |
| Coroutines | Safe | Coroutine thunk promoted to root_scope via promote_to_root (existing). Batch promote handles it. |
| env_scope (closure env ownership) | Unchanged | env_scope is a ScopeRegion*, not a Closure field that needs arena placement |
| let-rec weak self-reference | Unchanged | rc stays 1, no promotion needed, arena frees it |
| TCO recycling | Safe | Ephemeral closures (rc=1) in call_scope freed at bounce. Shared closures promoted by batch scan before scope release. |

**Why this works with continuations** (the key safety argument):
When `stack_ctx_clone` copies a stack, the continuation holds a reference to the scope containing the Closure. The scope's `structural_rc` is bumped (or the continuation keeps the scope alive via its own reference chain). So the arena holding the Closure stays alive as long as any continuation references it. When the last continuation dies, the scope's RC drops and `batch_promote_closures` runs — but by then, no live references exist to the arena Closures, so no promotion is needed.

**Tradeoffs**:
- Pro: Eliminates per-closure malloc — bump alloc is ~3 instructions vs ~50 for malloc
- Pro: Eliminates per-closure ScopeDtor (24 bytes each) — batch scan replaces n dtors
- Pro: Cache-friendly — Closures in same scope are contiguous in memory
- Pro: Ephemeral closures (vast majority) have zero cleanup cost — arena freed in bulk
- Pro: Keeps RC for correctness — shared Closures still get precise lifetime management
- Con: Batch scan at scope death adds O(values_in_scope) sweep — but this replaces O(closures_in_scope) dtor calls, net comparable
- Con: Promoted Closures still malloc — but only for shared closures (rare in typical code)
- Con: Walking chunks to find CLOSURE values requires type-tag scanning — could add a per-scope closure list for O(1) access
- Risk: If a scope has many Values but few Closures, the batch scan wastes time. Mitigable with a per-scope closure count or linked list.

**Optimization — per-scope closure list**:
Instead of scanning all Values, maintain a singly-linked list of Closure* per scope. Bump-allocate a `ClosureNode { Closure* c; ClosureNode* next; }` alongside each Closure. At scope death, walk the list (O(closures)) not the chunks (O(values)).

**Verdict**: Best path to eliminating closure malloc overhead. The batch-promote approach preserves RC safety while getting bump-allocation speed for the common case (ephemeral closures). Implement after Exploration 2 (JIT Accumulator Hint) to reduce per-call overhead first.

---

---
