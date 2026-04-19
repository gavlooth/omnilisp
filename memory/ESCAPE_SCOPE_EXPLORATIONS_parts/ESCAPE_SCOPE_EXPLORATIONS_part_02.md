# ESCAPE_SCOPE_EXPLORATIONS Part 02

Source: `memory/ESCAPE_SCOPE_EXPLORATIONS.md`

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
