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

## Exploration 7: Inline Bindings + Escape-Scope Env (eliminate copy_tco_env_chain overhead)

**Status**: Validated via 4-round dialectic reasoning (groq thesis, deepseek antithesis, zai synthesis). Converged: A+B (inline bindings + escape-scope allocation) as safe core. C (mutate-in-place) conditionally when closure capture provably absent.

**What**: Eliminate the per-bounce malloc + dtor overhead in `copy_tco_env_chain` by (1) embedding small binding arrays directly in the Env struct (inline bindings), and (2) allocating Env frames in escape_scope so they survive TCO bounces without copying.

**The problem**: At every TCO bounce, `copy_tco_env_chain` (jit.c3:425-449):
- Walks env chain recursively from current env to global_env
- For each non-persistent Env frame: `make_env()` → bump-alloc Env + `malloc(Binding.sizeof * capacity)` + register ScopeDtor
- Copies each binding value via `copy_to_parent` (already O(1) with scope-aware skip)
- Rebuilds hash table if present (`malloc` + rehash)
- For 1M-iteration loop with 2 bindings: 1M make_env calls + 1M mallocs + 1M dtors

### Sub-approach A: Env in escape_scope

**What**: Allocate Env frames in `result_scope` (escape_scope) instead of `call_scope`. They survive bounces — `copy_tco_env_chain` can skip frames already in a surviving scope.

**How**: In `copy_tco_env_chain`, add scope-aware check (analogous to `copy_to_parent`):
```c3
fn Env* copy_tco_env_chain(Env* src, Interp* interp) {
    if (src == null || src == interp.global_env) return src;
    if (src.persistent) { ... }

    // NEW: if env frame is NOT in releasing scope, it's already safe — skip copy
    if (interp.releasing_scope != null &&
        !main::is_in_scope((void*)src, interp.releasing_scope)) {
        return src;  // Env survives bounce, no copy needed
    }

    // ... existing copy logic ...
}
```

**Limitation**: Only helps if the Env frame was allocated in escape_scope to begin with. Currently all Env frames go in `current_scope` (= call_scope). Need to selectively allocate loop-binding Env frames in escape_scope.

**Where to allocate in escape_scope**: In `jit_eval_let_rec` (named-let), after creating the initial env frame for loop bindings, allocate it in escape_scope:
```c3
// Before creating named-let env frame, switch to escape_scope
main::ScopeRegion* saved = interp.current_scope;
interp.current_scope = interp.escape_scope;
Env* loop_env = make_env(interp, closure_env);
interp.current_scope = saved;
```

**Tradeoffs**:
- Pro: Env frame survives all bounces — `copy_tco_env_chain` skips it via is_in_scope check
- Pro: Zero code change to Env struct
- Con: Bindings array still malloc'd — the dtor lives in escape_scope (freed at function return, not at each bounce). Acceptable: one malloc per function call, not per bounce.
- Con: Only helps named-let loops where we know which env frame is the loop binding

### Sub-approach B: Inline bindings

**What**: For small Env frames (≤4 bindings), embed the Binding array directly in the Env struct. No malloc, no dtor. Entire Env is one bump allocation.

**Struct change**:
```c3
const ENV_INLINE_CAPACITY = 4;  // Covers ~95% of named-let frames

struct Env {
    Binding*      bindings;        // Points to inline_storage OR malloc'd array
    usz           binding_count;
    usz           capacity;
    Env*          parent;
    EnvHashEntry* hash_table;
    usz           hash_capacity;
    bool          persistent;
    Binding       inline_storage[ENV_INLINE_CAPACITY];  // NEW: embedded bindings
}
```

**Allocation path**:
```c3
fn Env* make_env(Interp* interp, Env* parent) {
    Env* env = interp.alloc_env();
    env.bindings = &env.inline_storage[0];  // Point to inline storage
    env.binding_count = 0;
    env.capacity = ENV_INLINE_CAPACITY;
    env.parent = parent;
    env.hash_table = null;
    env.hash_capacity = 0;
    env.persistent = false;
    // NO malloc, NO dtor registration for small frames
    return env;
}
```

**Growth path** (when bindings exceed inline capacity):
```c3
fn void Env.define(Env* self, SymbolId name, Value* value) {
    if (self.binding_count >= self.capacity) {
        usz new_cap = self.capacity * 2;
        Binding* new_bindings = (Binding*)mem::malloc(Binding.sizeof * new_cap);
        mem::copy(new_bindings, self.bindings, Binding.sizeof * self.binding_count);
        if (self.bindings != &self.inline_storage[0]) {
            mem::free(self.bindings);  // Free old malloc'd array (not inline)
        }
        self.bindings = new_bindings;
        self.capacity = new_cap;
        // Register dtor NOW (first time we malloc)
        // ... scope_register_dtor ...
    }
    self.bindings[self.binding_count] = { .name = name, .value = value };
    self.binding_count++;
}
```

**Tradeoffs**:
- Pro: Zero malloc + zero dtor for frames with ≤4 bindings (vast majority of named-let)
- Pro: General optimization — benefits ALL Env frames, not just TCO loops
- Pro: No semantic risk — pure allocation optimization
- Con: Env struct grows by `4 * 16 = 64 bytes` (from ~56 to ~120 bytes). Acceptable for bump allocation.
- Con: `make_env` in `copy_tco_env_chain` still allocates a new Env struct per bounce (bump alloc, cheap but not zero)

### Sub-approach C: Env reuse / mutate-in-place (CONDITIONAL)

**What**: Allocate loop Env in escape_scope on first iteration. On subsequent bounces, don't copy the Env frame — just update the `Value*` pointers in the existing bindings.

**The closure capture problem**: If a lambda inside the loop captures the env frame, and the loop mutates it on the next bounce, the closure sees the wrong values. This **breaks lexical scoping**.

Example:
```lisp
(let loop (xs '(1 2 3) acc '())
  (if (null? xs) acc
      (loop (cdr xs)
            (cons (lambda () (car xs))  ; captures env with xs bound
                  acc))))
;; If env is mutated in-place, all closures see xs = '() (final value)
```

**When it's safe**: Named-let where the loop body creates NO closures (no `lambda` in body). The JIT/parser can detect this statically — if ExprLet body contains no ExprLambda nodes, set a `no_capture` flag.

**How**:
```c3
// In TCO trampoline, if env frame has no_capture flag:
if (loop_env.no_capture && is_in_scope(loop_env, interp.escape_scope)) {
    // Safe to mutate in-place — just update binding values
    for (usz i = 0; i < loop_env.binding_count; i++) {
        loop_env.bindings[i].value = copy_to_parent(new_values[i], interp);
    }
    // Zero allocation, zero copy of env frame
} else {
    // Fall back to copy_tco_env_chain (with A+B optimizations)
}
```

**Tradeoffs**:
- Pro: Zero allocation after first iteration — absolute minimum cost per bounce
- Pro: Static detection is conservative and safe (false negatives OK — falls back to copy)
- Con: Only applies to lambda-free loop bodies (common for simple loops, not for HOF patterns)
- Con: Undecidable in general (dynamic lambda creation via eval, macros) — static check must be conservative
- Risk: If static check misses a closure creation path → silent semantic bug. Must be bulletproof.

### Combined Strategy: A+B+10 — "Slot-Backed Env" (the unified design)

**Status**: Refined via 4-round dialectic on "Slot-Backed Env" unifying idea (groq/deepseek/zai). The dialectic debated whether value promotion from call_scope to escape_scope creates a "second codepath." Resolution: **it does not** — the promotion IS the existing copy_to_parent loop, just targeting escape_scope instead of fresh call_scope. ~8 lines of new code in copy_tco_env_chain.

**The key insight from dialectic round 4**: The "value promotion problem" is not a new codepath. `copy_tco_env_chain` already runs `copy_to_parent` per binding value. The only change: when the Env survives (is in escape_scope), skip the frame copy and update values in-place, targeting escape_scope. Everything else is existing code.

**Implementation: 3 pieces that compose**:

**Piece 1 — Exploration 10 (scope_gen on Env)**: Add `uint scope_gen` to Env. Stamp at allocation. O(1) check in copy_tco_env_chain: `src.scope_gen != releasing_scope.generation` → Env survives.

**Piece 2 — Exploration 7A (escape-scope env)**: Allocate named-let loop Env in escape_scope. It survives bounces.

**Piece 3 — In-place value update in copy_tco_env_chain**: When Env survives (piece 1 detects), update binding values to escape_scope via copy_to_parent (piece 2 ensures this is the right target).

**The optimized copy_tco_env_chain** (showing the complete function):
```c3
fn Env* copy_tco_env_chain(Env* src, Interp* interp) {
    if (src == null || src == interp.global_env) return src;
    if (src.persistent) {
        src.parent = copy_tco_env_chain(src.parent, interp);
        return src;
    }

    // NEW: Env survives the bounce (in escape_scope or parent scope).
    // Skip frame copy — just promote binding values to escape_scope.
    if (interp.releasing_scope != null &&
        interp.escape_scope != null &&
        src.scope_gen != interp.releasing_scope.generation) {
        main::ScopeRegion* saved = interp.current_scope;
        interp.current_scope = interp.escape_scope;  // values land in escape_scope
        for (usz i = 0; i < src.binding_count; i++) {
            src.bindings[i].value = copy_to_parent(src.bindings[i].value, interp);
        }
        interp.current_scope = saved;
        src.parent = copy_tco_env_chain(src.parent, interp);
        return src;  // same Env, updated values, no alloc
    }

    // Normal: copy entire env frame (existing code, unchanged)
    Env* parent_copy = copy_tco_env_chain(src.parent, interp);
    Env* dst = make_env(interp, parent_copy);
    for (usz i = 0; i < src.binding_count; i++) {
        Value* v = copy_to_parent(src.bindings[i].value, interp);
        dst.define(src.bindings[i].name, v);
    }
    if (src.hash_table != null) dst.build_hash_table();
    return dst;
}
```

**What's eliminated per bounce** (for escape-scope Env):
- `make_env` (bump alloc Env struct)
- `malloc(Binding[] array)` + `scope_register_dtor`
- `dst.define()` × n (name copying into new frame)
- `build_hash_table()` (if present)

**What's kept** (same as before):
- `copy_to_parent` per binding value (most skip via scope-aware check — O(1) each)

**What's new**: ~8 lines in copy_tco_env_chain + allocate named-let Env in escape_scope

**Optional Piece 4 — Exploration 7B (inline bindings)**: Eliminates malloc+dtor for the Env's Binding array. Combined with A+10, the loop Env is a single bump allocation in escape_scope with zero heap backing. Benefits ALL Env frames, not just TCO loops.

**Optional Piece 5 — Sub-approach C (mutate-in-place)**: For lambda-free loop bodies, skip even copy_to_parent — just write Value* pointers directly. Requires static no_capture check from parser. Only saves ~n×copy_to_parent cost (already O(1) per binding with scope-aware skip).

**Piece 6 — C1 safety: detect-and-fall-back for shift-in-loop**:
Graph-of-thoughts (13 nodes, groq/deepseek/zai) converged on **Idea 6** (score 0.9, terminal solution): if `shift` appears inside a named-let body, DON'T allocate the loop Env in escape_scope — fall back to normal call_scope + copy_tco_env_chain. Parser statically scans body for shift expression nodes.
- Zero cost for common case (most loops don't use shift)
- Correct for edge case (loops with shift use proven env chain copying)
- Zero new mechanisms — just a conditional at Env allocation time

**Piece 7 — C7 safety: continuation holds scope reference**:
Graph-of-thoughts (7 nodes, groq/deepseek/zai) converged on **Idea 1** (score 0.8, two independent terminal solutions): when shift captures a continuation, bump `escape_scope.refcount`. When StackCtx is freed, decrement. escape_scope lives as long as any continuation references it.
```c3
// In jit_shift_impl, after stack_ctx_clone:
if (interp.escape_scope != null) {
    ctx.captured_escape_scope = interp.escape_scope;
    main::scope_addref(interp.escape_scope);
}
// In StackCtx cleanup (dtor or explicit free):
if (ctx.captured_escape_scope != null) {
    main::scope_release(ctx.captured_escape_scope);
}
```
- Adds `ScopeRegion* captured_escape_scope` field to StackCtx
- Composable with existing RC lifecycle

**How Pieces 6+7 compose**: With detect-and-fall-back (Piece 6), loops with `shift` use call_scope for their Env. So C1 (shared mutable Env) never occurs — the Env is copied per bounce as before. C7 (escape_scope lifetime) protects the escape_scope for accumulator cons cells that ARE in escape_scope. The two solutions are orthogonal and cover each other's gaps.

**Interaction with all 7 concerns** (re-evaluated with unified design + Pieces 6-7):

| Concern | Status | How it's handled |
|---|---|---|
| C1 Multi-shot continuations | **Safe (Piece 6)** | Loops with `shift` fall back to call_scope Env. No shared mutable state. |
| C2 Closure capture | **Safe** | Closures capture normal Env. copy_to_parent snapshots bindings. Existing mechanism. |
| C3 set! | **Safe** | set! writes to Env.set → modifies binding in escape_scope. Parent chain for outer vars. |
| C4 Self-tail-call detection | **Not needed** | Env survives bounce; copy_tco_env_chain skips it. Existing TCO works. |
| C5 Two codepaths | **Eliminated** | One codepath with a branch. Env struct unchanged. |
| C6 Nested named-let | **Safe** | Each level has own Env in own escape_scope. Cross-level transparent. |
| C7 escape_scope lifetime | **Safe (Piece 7)** | Continuation bumps escape_scope.refcount. Scope lives until last ref dies. |

**Net effect on `(let loop (xs lst acc '()) ...)` with n iterations**:

| Cost | Before | After A+10 | After A+B+10 | After A+B+10+C |
|------|--------|------------|--------------|----------------|
| Env allocs per bounce | 1 make_env + 1 malloc | 0 | 0 | 0 |
| Dtors per bounce | 1 | 0 | 0 | 0 |
| Binding copies per bounce | 2 copy_to_parent | 2 copy_to_parent | 2 copy_to_parent | 2 pointer writes |
| Total per-bounce cost | ~100 instructions | ~10 instructions | ~10 instructions | ~4 instructions |

**Verdict**: The unified A+10 design is the clear winner — 8 lines of code, eliminates ~90% of per-bounce cost, zero semantic risk, ONE codepath, composes with everything. B (inline bindings) is a general optimization that helps A+10 further (removes malloc+dtor for the initial Env creation). C (mutate-in-place) is a micro-optimization for lambda-free loops.

**This design largely obsoletes Exploration 9 (Loop Register Slots)**. Register slots offered zero-alloc bounces but at the cost of a second codepath, closure materialization, continuation problems, and JIT changes. The unified A+10 achieves ~90% of the benefit with ~5% of the complexity.

---

## Exploration 8: Separated Name-Value Storage (halve copy cost)

**Status**: Proposed. Not yet validated via dialectic.

**What**: Split `Binding = { SymbolId name, Value* value }` into two parallel arrays: an immutable `SymbolId[]` (allocated once per function, shared across bounces) and a mutable `Value*[]` (the only thing that changes per bounce). Copying becomes a `memcpy` of just the value pointers.

**The insight**: In `(let loop (xs lst acc '()) ...)`, the names `[xs, acc]` are identical every iteration. Currently `copy_tco_env_chain` copies both name and value for each binding (32 bytes per binding). If names are separated, we copy only values (16 bytes per binding — 50% reduction).

**Struct changes**:
```c3
struct Env {
    SymbolId*     names;          // Shared, immutable — allocated once
    Value**       values;         // Mutable — copied or updated per bounce
    usz           binding_count;
    usz           capacity;
    Env*          parent;
    EnvHashEntry* hash_table;
    usz           hash_capacity;
    bool          persistent;
    bool          names_shared;   // true = don't free names array (shared with original)
}
```

**Copy path** (in `copy_tco_env_chain`):
```c3
// Instead of copying each Binding:
Env* dst = make_env(interp, parent_copy);
dst.names = src.names;          // Share name array (no copy)
dst.names_shared = true;
dst.binding_count = src.binding_count;
mem::copy(dst.values, src.values, Value*.sizeof * src.binding_count);  // memcpy values only
```

**Where it connects**:
- `value.c3:1236-1244` — Env struct: split `Binding*` into `SymbolId*` + `Value**`
- `value.c3:1246-1257` — `make_env`: allocate both arrays (or share names)
- `value.c3` — `Env.define`, `Env.lookup`, `Env.set`: update to use parallel arrays
- `jit.c3:425-449` — `copy_tco_env_chain`: share names, memcpy values
- All env access sites: change `bindings[i].name` → `names[i]`, `bindings[i].value` → `values[i]`

**Tradeoffs**:
- Pro: 50% less data copied per bounce (names never copied after first iteration)
- Pro: Name array is immutable — safe to share, no COW complexity
- Pro: `memcpy` of contiguous `Value*[]` is vectorizable (SIMD-friendly)
- Con: Refactor of ALL Env access sites (define, lookup, set, hash table) — significant surface area
- Con: Two mallocs instead of one (names + values), unless names are inline too
- Con: `Env.define` for dynamic additions needs to grow both arrays in sync
- Risk: Hash table indexes into parallel arrays — need to ensure index consistency

**Interaction with features**:

| Feature | Impact | Notes |
|---|---|---|
| Closures | Safe | Capture Env with shared names array; names immutable |
| Continuations | Safe | Env copied by copy_to_parent; shared names are in escape_scope or parent |
| set! | Safe | Only modifies values array, names untouched |
| Nested named-let | Safe | Each level has its own Env with its own names |
| HOFs | Safe | Lambda envs capture parent; shared names from parent level survive |

**Verdict**: Clean optimization, halves copy cost. High refactor surface (all env access sites). Best combined with Exploration 7B (inline values array for small frames) — if values are inline AND names are shared, per-bounce cost becomes: zero alloc + memcpy of 2-4 pointers.

---

## Exploration 9: Loop Register Slots (eliminate env chains for named-let)

**Status**: Validated via 4-round dialectic (groq/deepseek/zai, confidence 0.8). Viable for common case; continuations are the hard edge case.

**What**: Compile named-let to use pre-allocated `Value*` slots instead of Env chains. The JIT recognizes named-let at parse time, counts the loop bindings, and allocates a `Value*[n]` array once in escape_scope. Each TCO bounce writes new values directly into the slots. Variable lookups use compile-time slot indices instead of env chain walks.

**The insight**: Named-let has a fixed, known set of bindings. The env chain is overkill — a flat indexed array is sufficient and eliminates all env infrastructure (Env struct, Binding array, parent chain, hash table, copy_tco_env_chain).

**How named-let currently works** (jit.c3:610-666):
```
(let loop (xs lst acc '()) body)
  → desugars to (let ^rec (loop (lambda (xs acc) body)) (loop lst '()))
  → jit_eval_let_rec creates rec_env with loop=closure
  → body (loop lst '()) triggers jit_apply → jit_eval_in_call_scope
  → TCO bounces recycle call_scope, copy_tco_env_chain copies env chain
```

**Proposed new path**:
```
(let loop (xs lst acc '()) body)
  → parser sets flag: expr.let_expr.is_named_loop = true
  → parser stores binding names: [xs, acc] and init exprs: [lst, '()]
  → jit_eval_named_loop:
      1. Allocate slot array Value*[2] in escape_scope
      2. Evaluate init exprs, store in slots: slot[0]=lst, slot[1]='()
      3. Create a LoopEnv that wraps slots + names for lookup
      4. Enter TCO trampoline with body expr
      5. At bounce: evaluate new args, write directly into slots
      6. No copy_tco_env_chain — slots persist in escape_scope
```

**New structures**:
```c3
struct LoopSlots {
    Value**   slots;        // Value*[n] allocated in escape_scope
    SymbolId* names;        // name[i] corresponds to slot[i]
    usz       count;
}

// ExprLet extension (or new ExprNamedLoop):
struct ExprLet {
    SymbolId name;
    Expr*    init;
    Expr*    body;
    bool     is_recursive;
    // NEW for named-let:
    SymbolId* loop_params;   // [xs, acc] — the binding names
    Expr**    loop_inits;    // [lst, '()] — init expressions
    usz       loop_param_count;
}
```

**JIT implementation** (`jit_eval_named_loop`):
```c3
fn Value* jit_eval_named_loop(Interp* interp, Expr* expr, Env* env) {
    usz n = expr.let_expr.loop_param_count;

    // Allocate slots in escape_scope (persist across bounces)
    main::ScopeRegion* saved = interp.current_scope;
    interp.current_scope = interp.escape_scope;
    Value** slots = (Value**)interp.current_scope.alloc(Value*.sizeof * n);
    interp.current_scope = saved;

    // Evaluate init exprs into slots
    for (usz i = 0; i < n; i++) {
        slots[i] = jit_eval(expr.let_expr.loop_inits[i], env, interp);
    }

    // Create loop env that wraps slots for variable lookup
    Env* loop_env = make_loop_env(interp, env, expr.let_expr.loop_params, slots, n);

    // TCO trampoline
    Expr* body = expr.let_expr.body;
    while (true) {
        Value* result = jit_eval(body, loop_env, interp);

        if (!interp.flags.jit_tco_bounce) return result;

        // TCO bounce — update slots directly (no env copy!)
        interp.flags.jit_tco_bounce = false;
        // interp.jit_tco_args contains new values for each slot
        for (usz i = 0; i < n; i++) {
            slots[i] = interp.jit_tco_args[i];  // Direct write, no copy
        }

        // Recycle call_scope as before (for temporaries)
        // ... existing scope recycling logic ...
    }
}
```

**Where it connects**:
- Parser: Detect named-let pattern, populate `loop_params`/`loop_inits` on ExprLet
- `jit.c3:610-666` — `jit_eval_let_rec`: dispatch to `jit_eval_named_loop` when `is_named_loop`
- `jit.c3:425-449` — `copy_tco_env_chain`: NOT CALLED for named-loop bounces
- `jit.c3:326-353` — TCO trampoline: new path for named-loop that writes to slots instead of copying env
- `value.c3` — New `make_loop_env` that wraps slot array as an Env-compatible structure for lookups
- TCO bounce mechanism: need to pass new arg values through interp (new field or reuse existing)

**The closure problem and solution**:
If a lambda is created inside the loop body, it needs a real Env to capture. Solution: **lazy Env materialization**. The LoopEnv is a lightweight wrapper around slots. Only when a closure captures it, materialize a real Env from the current slot values:
```c3
fn Env* materialize_loop_env(Interp* interp, LoopSlots* slots, Env* parent) {
    // Create a snapshot Env from current slot values
    Env* env = make_env(interp, parent);
    for (usz i = 0; i < slots.count; i++) {
        env.define(slots.names[i], slots[i]);
    }
    return env;  // Closure captures this snapshot, not the live slots
}
```
This means closures see a **snapshot** of the loop state at capture time (correct lexical semantics), while the loop continues to mutate slots (no aliasing problem).

**Interaction with features**:

| Feature | Impact | Notes |
|---|---|---|
| Closures | Safe (with lazy materialization) | Closure captures snapshot Env, not live slots |
| Continuations | Medium risk | Continuation captures stack with slot pointer; if escape_scope alive, slots valid |
| Coroutines | Safe | Coroutine runs in own scope; slots in escape_scope survive |
| set! | Needs handling | `set!` on loop var → write to slot directly. Need slot-aware set! path. |
| Nested named-let | Safe | Each named-let has its own LoopSlots in its own escape_scope |
| HOFs | Safe | HOF receives closure with snapshot Env |
| General TCO (tail-call to different fn) | Falls back | Only named-let uses slots; general TCO uses existing env chain copy |

**Tradeoffs**:
- Pro: **Zero per-bounce allocation** — slots persist, direct pointer writes
- Pro: **Zero env chain walking** — no copy_tco_env_chain at all for named-let
- Pro: O(1) variable lookup via compile-time slot index (vs O(n) env chain search)
- Pro: Extreme cache locality — slots are contiguous Value* array
- Con: Significant JIT changes — new codepath for named-let, TCO bounce mechanism, arg passing
- Con: Lazy Env materialization adds complexity for closure-containing loops
- Con: set! on loop vars needs special handling (write to slot, not env)
- Con: Parser changes to detect named-let and store loop params separately
- Risk: Two codepaths (slots for named-let, env for general TCO) — maintenance burden

**Per-bounce cost comparison**:

| Operation | Current | With Slots |
|---|---|---|
| Env alloc | make_env + malloc + dtor | 0 |
| Env chain walk | O(depth) recursive | 0 |
| Binding copies | n × copy_to_parent | n × pointer write |
| Scope recycle | scope_create + release | scope_create + release (unchanged) |
| **Total** | **~100 instructions** | **~n×2 instructions** (n=2-4 typically) |

**Dialectic findings** (4-round groq/deepseek/zai, confidence 0.8):

The core tension: the optimization's zero-alloc goal conflicts with its escape hatch (snapshot materialization). Every time a closure or continuation is captured inside the loop, you pay allocation cost — partially negating the optimization.

| Concern | Verdict | Detail |
|---|---|---|
| Closure capture | Solvable | Snapshot materialization is correct lexical semantics. Adds alloc only when lambdas present in body. |
| set! interaction | Solvable | Slot-aware set! for loop vars, parent chain for outer vars. Straightforward. |
| Self vs general tail-call | Solvable | Parser tags self-recursive calls. General tail-calls fall back. |
| Nested named-let | Solvable | Each level has own slots + escape_scope. Cross-level tail-calls fall back. |
| Lookup chain | Solvable | LoopEnv checks slots first, chains to parent. Standard. |
| Two codepaths | Valid concern | Maintenance burden real but bounded — named-let is well-defined. |
| **Multi-shot continuations** | **Hard problem** | Cloned stack shares mutable slot array. Both original and clone see same slots → shared mutable state. Need snapshot at `shift` time, which re-introduces allocation. |

Key antithesis insight (round 4): Snapshotting for continuations undermines zero-alloc. But **most named-let loops don't use continuations** — `reverse`, `map`, `filter`, `foldl`, `range`, `fib`, `fact` are all lambda-free, continuation-free. The common case gets true zero-alloc; edge cases fall back gracefully.

**Verdict**: Worth implementing for the **common case** (lambda-free, continuation-free loop bodies) where it delivers true zero per-bounce allocation. Loops with closures or continuations fall back to snapshot/copy. Highest reward, highest implementation cost. Best candidate after Exploration 7 (A+B) proves the simpler optimizations.

---

## Exploration 10: Scope-Gen Stamps on Env (O(1) skip in copy_tco_env_chain)

**Status**: Proposed. Straightforward extension of Exploration 1 pattern — no dialectic needed.

**What**: Add a `uint scope_gen` field to the Env struct (same pattern as Exploration 1 for Values). `copy_tco_env_chain` uses stamp comparison instead of `is_in_scope()` chunk walk to decide whether to skip an Env frame.

**The insight**: `copy_tco_env_chain` already has the scope-aware skip logic via `is_in_scope(ptr, releasing_scope)`. But `is_in_scope` walks the chunk list — O(chunks) per check. A generation stamp makes it O(1).

**Struct change**:
```c3
struct Env {
    Binding*      bindings;
    usz           binding_count;
    usz           capacity;
    Env*          parent;
    EnvHashEntry* hash_table;
    usz           hash_capacity;
    bool          persistent;
    uint          scope_gen;      // NEW: scope generation stamp
}
```

**Allocation path**:
```c3
fn Env* make_env(Interp* interp, Env* parent) {
    Env* env = interp.alloc_env();
    env.scope_gen = interp.current_scope.generation;  // Stamp at allocation
    // ... rest unchanged ...
}
```

**Skip check in copy_tco_env_chain**:
```c3
fn Env* copy_tco_env_chain(Env* src, Interp* interp) {
    if (src == null || src == interp.global_env) return src;
    if (src.persistent) {
        src.parent = copy_tco_env_chain(src.parent, interp);
        return src;
    }

    // NEW: O(1) scope-gen check instead of is_in_scope chunk walk
    if (interp.releasing_scope != null &&
        src.scope_gen != interp.releasing_scope.generation) {
        // Env is NOT in the dying scope — safe to keep as-is
        // Still need to check parent chain though (parent may be in dying scope)
        src.parent = copy_tco_env_chain(src.parent, interp);
        return src;
    }

    // ... existing copy logic (Env IS in dying scope, must copy) ...
}
```

**Where it connects**:
- `value.c3:1236-1244` — Env struct: add `uint scope_gen` field
- `value.c3:1246-1257` — `make_env`: stamp `scope_gen`
- `jit.c3:425-449` — `copy_tco_env_chain`: replace is_in_scope with stamp check
- `value.c3` — `alloc_env()`: no change needed (Env bumped from current_scope, stamp set in make_env)

**Interaction with scope_adopt**:
Same caveat as Exploration 1 for Values: `scope_adopt` moves chunks between scopes without updating stamps. However, Env frames are NOT typically subject to scope_adopt — only cons cells (via make_cons_escape) get adopted. Env frames stay in the scope where they were allocated. **So scope_gen on Env is safe for copy_tco_env_chain**, unlike copy_to_parent for Values.

**Interaction with features**:

| Feature | Impact | Notes |
|---|---|---|
| Closures | Safe | Stamp is read-only metadata, doesn't affect env semantics |
| Continuations | Safe | Cloned stacks reference same Env; stamp unchanged |
| scope_adopt | Safe for Env | Env frames aren't adopted (only cons cells are) |
| Escape-scope env (7A) | Composable | Env in escape_scope has escape_scope's generation → stamp check correctly skips |

**Tradeoffs**:
- Pro: O(1) check per Env frame (vs O(chunks) for is_in_scope)
- Pro: Tiny change — add one field, stamp in make_env, one check in copy_tco_env_chain
- Pro: Composable with ALL other explorations
- Pro: No scope_adopt issue (unlike Values in copy_to_parent)
- Con: Only speeds up the "should I copy?" check — doesn't eliminate the copy itself
- Con: Env struct grows by 4 bytes (negligible for bump allocation)

**Verdict**: Smallest, safest optimization. Pure speedup of existing logic. Should be implemented alongside ANY other exploration as a foundation. Highest ROI per line of code changed.

---

## Priority Order

**Tier 1 — Implement next (high reward, low risk)**:
1. **Exploration 1** (Scope Generation Stamps) — **IMPLEMENTED (Session 63)**, O(1) make_cons check
2. **Exploration 10+7A** (Scope-Gen on Env + Escape-Scope Env) — **THE UNIFIED DESIGN**. ~8 lines in copy_tco_env_chain, eliminates ~90% of per-bounce cost. One codepath. Zero semantic risk. Composes with everything.
3. **Exploration 7B** (Inline Bindings) — general optimization, eliminates malloc+dtor for all small Env frames. Composable with #2.

**Tier 2 — Implement after Tier 1 proven**:
4. **Exploration 2** (JIT Accumulator Hint) — skip two-scope setup for non-accumulator calls
5. **Exploration 6** (Closure Scratch Arena + RC) — eliminates closure malloc, high frequency win

**Tier 3 — Profile-driven, implement if bottleneck confirmed**:
6. **Exploration 5** (Dual Refcount / Deferred Free) — eliminates copy_to_parent for large scopes
7. **Exploration 7C** (Mutate-in-Place) — micro-optimization for lambda-free loops, saves ~n×copy_to_parent

**Tier 4 — Largely obsoleted or niche**:
8. **Exploration 9** (Loop Register Slots) — obsoleted by unified 10+7A design (90% of benefit, 5% of complexity)
9. **Exploration 8** (Separated Name-Value Storage) — high refactor surface, modest gain
10. **Exploration 3** (Arena Zero-Copy) — subsumed by Exploration 5
11. **Exploration 4** (Escape Binding Flag) — elegant but fragile with closures
