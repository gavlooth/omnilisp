# Remaining Scope-Region Optimizations

## Completed

- **Exploration 7B: Inline Bindings** — Session 65. Env frames <=4 bindings use inline array, no malloc/dtor.
- **Exploration 2: JIT Accumulator Hint** — Session 65. `jit_eval_in_single_scope` for non-loop calls.
- **Exploration 6: Closure Scratch Arena** — Session 65. Closures bump-allocated in scope arena.
- **Exploration 5: Scope Adoption at Return** — Session 66. Conditional `scope_adopt` for CONS returns (O(n)→O(1)). Scalar returns use O(1) copy + release (no garbage). Replaced failed dual-refcount approach.
- **Exploration 7C: Mutate-in-Place** — Session 65. Evaluated and skipped (incorrect assumption about TCO env chain).

## Status

All planned scope-region optimizations are complete or evaluated. No remaining items.
