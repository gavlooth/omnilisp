# Deduce Incremental Delta Propagation Design (2026-03-13)

Purpose: define a concrete, dependency-aware incremental maintenance model for
`deduce/fact!` and `deduce/retract!` mutations so post-v1 Deduce avoids
full-program recompute when safe.

## Scope

This design now covers O7.8 items 1 through 3:

- dependency-aware invalidation/propagation graph for derived predicates.
- transaction-safe mutation boundaries for `deduce 'block` write transactions.
- mutation-heavy benchmark coverage for tracked vs `full-recompute` invalidation.

## Runtime Model

### 1. Mutation Epoch and Dirty Predicate Set

- `DeduceDb` tracks:
  - `incremental_mutation_epoch`
  - `incremental_dirty_predicates[]`
  - `incremental_full_recompute_required`
- each committed mutation increments the epoch.
- each mutation marks the seed predicate dirty.

### 2. Dependency-Aware Dirty Expansion

When predicate `P` changes, the engine traverses rule dependencies:

- rule edge: `head <- body_predicate`
- if body contains `P`, then `head` is dirty.
- traversal continues transitively until closure.
- both positive and negated dependencies are included in invalidation expansion.

Current helper implementation:

- `deduce_db_mark_dirty_with_rule_dependents(...)`
- BFS over installed `DeduceRuleSignature` graph.

### 3. Invalidation Modes

- `tracked`:
  - dependency-aware dirty propagation succeeded.
  - used for auto-commit mutation paths.
- `full-recompute`:
  - used when dependency tracking fails (for example allocation pressure), or
  - transaction write logging degraded and commit had to fall back to conservative
    invalidation.

Current helper implementation:

- `deduce_db_note_mutation(...)`
- `deduce_db_note_untracked_write_commit(...)`

## Analyze Diagnostics Contract (Incremental Slice)

`deduce/analyze` now reports incremental pre-run diagnostics:

- `incremental-dependency-edges`
- `incremental-dirty-predicate-count`
- `incremental-invalidation-mode` (`tracked` or `full-recompute`)

After a successful `deduce/analyze`, dirty tracking is reset (epoch is retained)
because analyze performs a full fixpoint recompute today.

## Transaction Boundary Status (O7.8 Item 2)

Implemented boundary contract:

1. `DeduceTxn` now stores per-relation mutation logs (`insert`/`delete` counts
   plus destructive marker).
2. On successful commit, schema estimate deltas and dependency invalidation are
   applied from the committed log.
3. On abort (or finalizer-driven abort), pending mutation logs are discarded.
4. Mixed-relation transaction logs are applied deterministically in log order.

Conservative fallback remains:

- if mutation logging cannot be recorded during write (allocation pressure),
  transaction state marks untracked mutation and commit flips invalidation mode
  to `full-recompute`.

## Benchmark Status (O7.8 Item 3)

Implemented benchmark contract:

1. Added env-gated mutation benchmark lane:
   - `run_deduce_incremental_mutation_benchmarks(...)`
   - gate: `OMNI_DEDUCE_BENCH=1`
2. Added tracked lane (`fact!` only) and destructive lane (`retract!` + `fact!`)
   with per-iteration `deduce/analyze` calls.
3. Added mode-hit diagnostics in benchmark summary output to prove expected
   invalidation mode behavior under load (`tracked` vs `full-recompute`).

Baseline capture:

- `docs/plans/deduce-incremental-mutation-benchmark-baseline-2026-03-13.md`

## Correctness Notes

- Invalidations are monotone and conservative: over-invalidating is allowed;
  under-invalidating is not.
- Negated dependencies are included, because retractions/inserts can affect
  negation truth values.
- Tracking failure degrades to `full-recompute` mode rather than risking stale
  derived state.
