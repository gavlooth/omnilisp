# Concurrency Boundary and Shared Handle Plan

Status: complete  
As of: 2026-03-09

## Immediate Execution Snapshot

- [x] Phase 0 complete: baseline ASAN blocker fixed and revalidated.
- [x] Phase 1 complete: explicit local/sendable/shared vocabulary reflected across docs.
- [x] Phase 2 complete: one bridge API added and every worker scheduling boundary routed through it.
- [x] Phase 3 complete: worker/scheduler boundaries no longer depend on direct `SharedBlob*` transport.
- [x] Phase 3 complete: byte payloads at concurrency boundaries use `SharedHandle(kind=BLOB)`.
- [x] Phase 4 complete: shared handle registry with generation-validated resolution and explicit destroy/retire entry points.
- [x] Phase 5 complete: one-way projection gate from shared handles into local `Value*` materialization.
- [x] Phase 6 complete: domain abstraction added with fiber-domain affinity guard and domain-level bridge wrappers.
- [x] Phase 7 complete: deferred shared-object reclamation policy validated with full normal + ASAN runs.

Current position (per current repo docs): **Phase 7 complete**

Use this as a hard gate: no phase may advance while upstream item(s) above
remain unchecked.

## Purpose

This plan replaces the earlier concurrency checklist with a concrete step-by-step
execution plan for evolving Omni from the current narrow shared-payload model
toward a future-safe concurrency model without:

- rewriting fibers first,
- introducing a second ownership system for language values,
- blurring thread-affine runtime state with cross-thread shared state,
- relying on root pinning or ad hoc exceptions as correctness fixes.

The plan is intentionally conservative. It assumes the current architecture is
mostly correct and should be extended through one explicit boundary model rather
than through piecemeal hacks.

## Current Reality

Today Omni already has the core pieces of a workable concurrency story:

- `ScopeRegion` is the ownership authority for language values.
- `StackCtx` / fibers are thread-affine and scheduler-owned.
- libuv callbacks and fiber scheduling run on the scheduler thread.
- worker threads are used only for offload jobs and thread-task completions.
- `SharedBlob` is a legacy transport object and the replacement target for byte-sharing migration.
- `SharedBlob` is the legacy transport object that must disappear from the production
  worker/scheduler boundary once parity for
  `SharedHandle(kind=BLOB)` is reached.

This means the current model is already simple in one critical sense:

- local language/runtime state stays local,
- only narrow completion payloads cross threads.

That simplicity must be preserved.

## Problem Statement

The current model is good for one-shot worker payloads, but it is not yet a
future-proof concurrency boundary.

Why:

- `SharedBlob` is transport-specific and should be replaced, not elevated into
  the final model.
- there is no single named bridge API that makes local/shared crossings explicit,
- future persistent shared async objects would likely cause ad hoc exceptions if
  added directly to the current runtime,
- if Omni later adds more concurrency surface without a single boundary model,
  the runtime will become fragile.

The goal is to define that boundary now, then evolve behind it in controlled
phases.

## Core Forward-Compatible Mechanism

This plan adopts the following mechanism as the architectural center.

### 1. Define Two Worlds Explicitly

#### Local world

- `Value*`
- `Env*`
- `ScopeRegion*`
- `StackCtx*`
- closures
- continuations
- per-fiber scheduler state
- libuv callback state owned by the scheduler thread

These never cross threads.

#### Shared world

- copied bytes/scalars today,
- later, a small `SharedHandle` registry for long-lived async/service objects.

### 2. Make One Bridge API the Only Legal Crossing

The runtime must grow a single named bridge surface. Conceptually:

- `publish_sendable(local) -> Sendable`
- `publish_shared(local) -> SharedHandle`
- `consume_sendable(sendable, interp) -> Value*`
- `project_shared(handle, interp) -> Value*` or local view

No other code gets to pass raw runtime pointers across the boundary.

### 3. Keep the First Shared Object Rule Brutally Narrow

A shared object must not own arbitrary Omni `Value` graphs.

If it needs language-level data:

- store serialized/copied payloads, or
- store stable external state,
- then materialize local `Value` projections on the scheduler thread.

This avoids the worst failure mode: a hidden second ownership model for language
values.

### 4. Delay Deferred Reclamation Until It Is Actually Needed

For one-shot worker completions, current ownership transfer is already simpler
than shared handles.

Deferred reclamation should not be introduced until Omni has:

- persistent shared identities,
- repeated shared lookups,
- long-lived objects that truly outlive one completion cycle.

### 5. Add Domain Before Migration

If future Omni needs more concurrency, introduce a scheduler/domain abstraction
first, but keep fibers owned by one domain.

Cross-domain traffic should still go through `Sendable` or `SharedHandle`, not
migrated `StackCtx*`.

## Non-Negotiable Constraints

These rules are hard gates for every phase below.

- `ScopeRegion` remains the primary ownership authority for language values.
- No per-type RC lifetime model for `Value`, `Env`, `Closure`, `Instance`, or
  arbitrary language graph nodes.
- No raw `Value*`, `Env*`, `ScopeRegion*`, or `StackCtx*` may cross a worker or
  future domain boundary.
- Fibers remain owner-thread objects unless a future runtime redesign is
  explicitly approved.
- Shared objects must not directly expose local runtime pointers.
- Root pinning is not a general correctness strategy.
- Boundary APIs must stay the source of truth for thread/domain crossing.

## What This Plan Does Not Do

- It does not replace current return/env/promotion boundary logic.
- It does not rewrite fibers around handles.
- It does not convert ordinary evaluator values into handle-managed objects.
- It does not add tracing GC.
- It does not introduce deferred reclamation for one-shot offload results.

## Type Policy Matrix

Use this matrix as the default classification for runtime objects.

| Runtime object | Policy | Reason |
|---|---|---|
| `Value*` ordinary values | local only | scope-owned language values |
| `Env*` | local only | closure/env graph tied to boundary logic |
| `ScopeRegion*` | local only | ownership authority + thread affinity |
| `StackCtx*` | local only | owner-thread stack engine object |
| `COROUTINE` wrapper | local share only | root wrapper over thread-affine `StackCtx*` |
| `Continuation*` | local share only | runtime control object, not sendable |
| `Instance*` | local share only | owner-scope tethered by current model |
| `FFI_HANDLE` | explicit opaque exception | foreign-resource wrapper only |
| `Module*` / module env | scheduler-thread share only | runtime metadata, not worker payload |
| `MethodTable*` | scheduler-thread share only | runtime metadata, not worker payload |
| `Pending*` scheduler slots | local only | scheduler bookkeeping |
| `OffloadWork` | sendable | copied/scalar payload form |
| `OffloadCompletion` | sendable return payload | one-shot handoff back to scheduler |
| `SharedBlob` | legacy transport target | removed from production worker/scheduler boundaries |
| `SharedHandle(kind=BLOB)` | active shared-handle target | handle-backed shared bytes replacing direct `SharedBlob` usage |
| future channels / mailboxes / promises | shared-handle candidate | persistent shared identity |
| future shared services / registries | shared-handle candidate | repeated cross-boundary access |

## Phased Plan

## Phase 0: Stabilize the Current Baseline

Do not build new concurrency ownership machinery on top of an unstable baseline.

### Tasks

- Fix the current ASAN blocker in memory/runtime.
- Re-run normal + ASAN baseline validation.
- Keep `memory/CHANGELOG.md` and `docs/areas/memory-runtime.md` aligned.
- Audit existing direct `scope.refcount++` lifetime bumps and route them through
  helper paths where required by current ownership policy.

### Exit Criteria

- current memory/runtime baseline is green enough for targeted ownership work,
- no unresolved ownership bugs are being papered over with new abstractions.

## Phase 1: Codify the Two-World Model in Docs and Names

Before adding new runtime structures, make the distinction explicit in code and
documentation.

### Tasks

- Add runtime terminology for:
  - `Local` objects,
  - `Sendable` payloads,
  - `Shared` handle-managed objects.
- Document that worker-thread crossings must use `Sendable` or `SharedHandle`
  only.
- Update concurrency docs to say fibers and libuv remain scheduler-thread-owned.
- Record `SharedBlob` as a legacy transport object to be replaced by
  `SharedHandle(kind=BLOB)`, not as the long-term model.

### Candidate Files

- `docs/reference/09-concurrency-ffi.md`
- `docs/areas/memory-runtime.md`
- `docs/plans/concurrency-hybrid-memory-checklist.md`

### Exit Criteria

- the codebase has one shared vocabulary for local vs sendable vs shared.

## Phase 2: Introduce the Bridge API Surface

This is the most important phase. It defines the only legal crossing.

### Tasks

- Add a boundary module or API surface dedicated to concurrency crossing.
- Define the conceptual operations:
  - `publish_sendable(local) -> Sendable`
  - `publish_shared(local) -> SharedHandle`
  - `consume_sendable(sendable, interp) -> Value*`
  - `project_shared(handle, interp) -> Value*` or local view
- Keep the first implementation narrow:
  - bytes,
  - strings as copied bytes,
  - plain scalars,
  - explicit foreign opaque resources where justified.
- Ensure all worker queue publish/consume paths go through this bridge.

### Important Rule

At this phase, `publish_shared` may exist as API shape before it has broad type
coverage. That is acceptable. The shape matters before the full implementation.

### Candidate Files

- `src/lisp/scheduler_state_offload.c3`
- `src/lisp/scheduler_offload_worker.c3`
- `src/lisp/scheduler_wakeup_io.c3`
- new boundary/shared runtime file(s) if needed

### Exit Criteria

- no cross-thread runtime path bypasses the bridge API,
- raw runtime pointers are not published directly to worker threads.

## Phase 3: Replace `SharedBlob` with Handle-Backed Shared Bytes

`SharedBlob` must not survive as a production concurrency-boundary transport.
This phase replaces direct blob transport with `SharedHandle(kind=BLOB)`.

### Tasks

- Introduce `SharedHandle(kind=BLOB)` as the first concrete shared-handle kind.
- Add bridge operations for byte publication and consumption through
  `SharedHandle`, not through direct `SharedBlob*` exposure.
- Route worker input and completion payloads through the bridge surface so bytes
  are published/consumed as shared handles with `SharedBlob` removed from the
  boundary interfaces.
- Keep local `Value` materialization on the scheduler thread.
- If a short compatibility shim is needed, confine it to the bridge layer only.
- Delete direct `SharedBlob` use from production worker/scheduler interfaces.
- Enforce no new production boundary callsites that pass or retain `SharedBlob*`
  directly.

### Exit Criteria

- production worker/scheduler boundaries no longer depend on direct
  `SharedBlob*` transport,
- blob transport is represented by `SharedHandle(kind=BLOB)` at the concurrency
  boundary.

## Phase 4: Add `SharedHandle` Registry for Persistent Shared Objects

Only after the bridge exists should Omni add a true shared-object abstraction.

### Tasks

- Introduce a narrow `SharedHandle` type.
- Add a shared-object registry/table owned by the runtime, not by local scopes.
- Define `SharedHandleKind` for a small initial set of shareable object
  classes.
- Add generation validation to handle resolution.
- Add explicit destroy/retire entry points for shared objects.

### First Allowed Shared Kinds

Keep the initial set brutally small. Good candidates:

- blob-backed shared byte objects,
- long-lived async completion objects if they become persistent,
- opaque foreign shared resources that do not own Omni `Value` graphs,
- future channels / mailboxes / promises if introduced.

### Not Allowed Yet

- arbitrary `Value` trees,
- `Env*`,
- closures,
- continuations,
- scopes,
- stack contexts,
- instances that embed language values and would create a second lifetime model.

### Exit Criteria

- persistent shared object support exists without changing ownership of ordinary
  language values.

## Phase 5: Keep Projection One-Way Into Local Runtime Values

This phase prevents handle creep into the evaluator.

### Tasks

- Define projection rules:
  - `SharedHandle` resolves to stable external/shared state,
  - scheduler thread materializes local `Value` views or wrappers,
  - local code works with ordinary `Value*` after projection.
- Avoid making the evaluator depend on pervasive handle resolution in hot paths.
- Keep handle resolution at explicit async/service boundaries.

### Policy

Shared objects are not a replacement for local values.
They are a backing model for a narrow class of non-lexical runtime objects.

### Exit Criteria

- evaluator fast paths remain region-centric,
- handle resolution remains boundary-local.

## Phase 6: Introduce Domain Abstraction Before Any Cross-Domain Expansion

If Omni grows beyond one scheduler thread, do not start by migrating fibers.

### Tasks

- Define a scheduler/domain abstraction:
  - each domain owns its libuv loop, scheduler state, local scopes, and fibers,
  - domains exchange only `Sendable` payloads and `SharedHandle`s.
- Keep fibers bound to one domain.
- Add domain-level publish/consume APIs layered on the same bridge model.
- Treat cross-domain communication as message passing, not stack/context sharing.

### Explicit Non-Goal

Do not migrate `StackCtx*` across domains in this phase.

### Exit Criteria

- a multi-domain future is possible without rewriting the local runtime model.

## Phase 7: Add Deferred Reclamation Only When Shared Lifetimes Demand It

This phase is intentionally late.

### Preconditions

Do not start this phase until:

- persistent shared objects exist,
- shared handles are used repeatedly,
- immediate destruction or simple explicit retire queues are no longer enough.

### Tasks

- Start with the simplest retire mechanism that fits actual usage.
- Prefer explicit retire queues and scheduler/domain safe points first.
- Add generation-stale tests before optimizing reclaim throughput.
- Only move to epoch-style reclamation if measurements justify it.

### Rule

Deferred reclamation must apply only to the shared-object registry, never to
ordinary `ScopeRegion` language values.

### Exit Criteria

- shared-object reclamation is robust without contaminating local runtime
  ownership semantics.

## Code Audit Checklist Per Phase

For every implementation phase, audit these questions:

- Does this code move a raw local runtime pointer across a thread/domain
  boundary?
- Does this shared object own or retain arbitrary `Value` graphs?
- Can the same behavior be achieved by `Sendable` copy/transfer instead?
- Is this shared identity persistent enough to justify a handle?
- Is there one finalizer or retire authority?
- Is stale access detectable?
- Does this change keep fibers local?

If any answer is unclear, the change should not land yet.

## Validation Plan

Each phase must add targeted tests, not only structural refactors.

### Required Test Categories

- scope return boundary safety
- closure capture / env copy boundary safety
- destruction path safety
- worker publish / scheduler consume boundary
- invalid handle or stale generation rejection
- cancel/drop paths for shared/sendable objects
- repeated publish/consume stress
- thread-affinity misuse remains fail-fast

### Minimum Commands

- `c3c build`
- `c3c build --sanitize=address`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

Latest validation snapshot for Phase 7 completion:

- `c3c build` (pass)
- `timeout -t 240 env LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `1492 passed, 0 failed`
- `c3c build --sanitize=address` (pass)
- `timeout -t 300 env ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `1491 passed, 0 failed`

If concurrency ownership code changes land, also run targeted repeated offload
and thread-task stress scenarios.

## Sequencing Notes

The safe sequence is:

1. stabilize baseline,
2. define the worlds,
3. enforce the bridge,
4. replace `SharedBlob` with `SharedHandle(kind=BLOB)`,
5. add narrow `SharedHandle`,
6. project shared objects back into local values,
7. add domains if needed,
8. add deferred reclamation only if shared lifetimes demand it.

Do not invert that order.

In particular:

- do not rewrite fibers before the bridge exists,
- do not add handle-managed language values,
- do not add epochs before persistent shared objects exist,
- do not add multi-domain runtime semantics before local/shared separation is
  explicit.

## Practical End State

If this plan succeeds, future Omni looks like this:

- fibers remain simple local owner-thread execution contexts,
- local evaluation remains region-centric,
- all cross-thread/cross-domain traffic goes through one bridge,
- direct `SharedBlob` transport disappears from the concurrency boundary,
- `SharedHandle(kind=BLOB)` becomes the byte-sharing path,
- `SharedHandle` supports persistent shared async/service objects,
- ordinary language values never inherit a second hidden lifetime system.

That is the simplest path to future concurrency without fragility.
