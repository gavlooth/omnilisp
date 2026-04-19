# ESCAPE_SCOPE_EXPLORATIONS Part 03

Source: `memory/ESCAPE_SCOPE_EXPLORATIONS.md`

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
