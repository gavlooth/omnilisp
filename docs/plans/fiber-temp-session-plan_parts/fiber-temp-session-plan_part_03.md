# fiber-temp-session-plan Part 03

Source: `docs/plans/fiber-temp-session-plan.md`

## Wave B: Regression Lock + Performance Stabilization (Sessions 7-9)

### Session 7: Teardown Regression Pack

Goal:
- Lock bug class with deterministic regression cases.

Likely files:
- `src/lisp/tests_tests.c3`
- `src/lisp/tests_escape_scope_tests.c3`
- `src/stack_engine.c3` tests

Work:
- Add tests for:
  - suspend then destroy before natural return
  - clone/discard ordering permutations
  - mixed JIT/eval transitions

Commits:
1. `tests: add teardown and clone boundary regression pack`

Validation:
- Global rule + ASAN.

### Session 8: Stress + Flake Check

Goal:
- Ensure tests are stable across repetition and order variation.

Likely files:
- test files only (plus minor harness hooks if needed)

Work:
- Run repeated targeted tests and tighten nondeterministic assertions.

Commits:
1. `tests: stabilize continuation/scheduler stress assertions`

Validation:
- Global rule + ASAN + repeated targeted runs.

### Session 9: Defer Overhead Stabilization

Goal:
- Reduce hot-path cost without changing API/semantics.

Likely files:
- `src/stack_engine.c3`
- `src/lisp/jit_jit_compiler.c3`
- `src/lisp/jit_jit_eval_scopes.c3`

Work:
- Add counters/bench hooks.
- Optimize internal defer storage strategy (implementation detail only).

Commits:
1. `perf: optimize defer substrate internals with no API change`
2. `perf/tests: add lightweight no-regression checks`

Validation:
- Global rule + ASAN.

---

## Wave C: Fiber-Temp Design + Flagged Plumbing (Sessions 10-13)

### Session 10: Fiber-Temp Design Freeze

Goal:
- Convert RnD concept into implementation-spec with explicit guardrails.

Likely files:
- `memory/DESTINATION_ARENA_PLAN.md`
- `docs/plans/fiber-temp-teardown-revision-summary.md`
- `memory/CHANGELOG.md`

Work:
- Define:
  - eligibility for TEMP routing
  - non-eligible classes
  - clone/suspend constraints
  - rollback strategy

Commits:
1. `docs: freeze fiber-temp design constraints and proof obligations`

Validation:
- Global rule only.

### Session 11: Chunk Pool + Feature Flag Skeleton

Goal:
- Introduce fiber-temp plumbing behind flag, default OFF.

Likely files:
- `src/stack_engine.c3`
- `src/scope_region.c3`
- `src/lisp/value_interp_state.c3`

Work:
- Add chunk metadata/lifecycle primitives.
- Add feature flag path with no behavior change when OFF.

Commits:
1. `runtime: add fiber-temp chunk pool skeleton behind feature flag`

Validation:
- Global rule + ASAN.

### Session 12: TEMP Backing Integration (Flag OFF/ON parity basics)

Goal:
- Connect temp backing context to stack/fiber lifecycle with safe default behavior.

Likely files:
- `src/stack_engine.c3`
- `src/scope_region.c3`
- `src/lisp/value_interp_state.c3`

Work:
- Wire plumbing paths and ensure OFF mode parity.
- Add basic ON-mode sanity tests.

Commits:
1. `runtime: wire fiber-temp backing lifecycle under guarded mode`

Validation:
- Global rule + ASAN.

### Session 13: Eligibility-Based Routing

Goal:
- Route only safe TEMP allocations to fiber-temp-backed path.

Likely files:
- `src/scope_region.c3`
- `src/lisp/value_constructors.c3`
- `src/lisp/eval_promotion_copy.c3`
- `src/lisp/eval_promotion_escape.c3`

Work:
- Add routing eligibility checks.
- Keep non-eligible allocations on deterministic ESCAPE path.

Commits:
1. `runtime: add eligibility-based TEMP routing for fiber-temp mode`

Validation:
- Global rule + ASAN + targeted continuation tests.

---

## Wave D: Clone/Scheduler Safety + Rollout Decision (Sessions 14-16)

### Session 14: Suspend/Clone Safety for Fiber-Temp

Goal:
- Ensure clone/suspend correctness in fiber-temp mode.

Likely files:
- `src/stack_engine.c3`
- `src/scope_region.c3`
- `src/lisp/scheduler_io_fiber_core.c3`
- `src/lisp/scheduler_wakeup_io.c3`

Work:
- Implement safe freeze/share (or equivalent) semantics for temp backing during clone/suspend.
- Ensure destroy reclaims correctly.

Commits:
1. `runtime: harden fiber-temp clone/suspend semantics`

Validation:
- Global rule + ASAN + targeted continuation/scheduler stress.

### Session 15: Scheduler/Thread Boundary Audit

Goal:
- Verify thread-boundary correctness with fiber-temp and defer substrate.

Likely files:
- `src/lisp/scheduler_thread_tasks.c3`
- `src/lisp/scheduler_offload_worker.c3`
- `src/lisp/scheduler_tcp_async_bridge.c3`
- `src/lisp/scheduler_primitives.c3`
- test files

Work:
- Audit callback mutation boundaries.
- Add/expand stress tests for offload wakeups and teardown races.

Commits:
1. `scheduler: enforce boundary-safe teardown and wakeup invariants`
2. `tests: add scheduler-thread teardown stress coverage`

Validation:
- Global rule + ASAN + repeated scheduler stress runs.

### Session 16: Rollout Decision + Cleanup

Goal:
- Decide production posture and clean dead paths only if gates are satisfied.

Likely files:
- `memory/CHANGELOG.md`
- `memory/DESTINATION_ARENA_PLAN.md`
- runtime files touched in prior sessions

Work:
- Decide default flag policy (likely keep OFF initially).
- Remove dead fallback code only where parity is proven.
- Publish final operations/invariant note.

Commits:
1. `docs/runtime: publish fiber-temp readiness decision and invariant status`
2. `cleanup: remove proven-dead fallback paths (if any)`

Validation:
- Full global + ASAN + targeted stress matrix.

---

## Deliverable Checkpoints

Checkpoint A (after Session 6):
- Defer substrate integrated into boundary paths; no known teardown leak path unresolved.

Checkpoint B (after Session 9):
- Regressions locked; defer overhead acceptable.

Checkpoint C (after Session 13):
- Fiber-temp plumbing and eligibility routing functional behind feature flag.

Checkpoint D (after Session 16):
- Production rollout decision with evidence and rollback plan.

## Notes on Throughput

- If sessions are run in parallel branches, merge order must still follow dependency chain:
  - Sessions 1-6 serial required.
  - Sessions 7-9 can partially overlap.
  - Sessions 10-16 should be mostly serial due to shared core files.
- Prefer one active high-risk runtime session at a time to minimize conflict and hidden regressions.
