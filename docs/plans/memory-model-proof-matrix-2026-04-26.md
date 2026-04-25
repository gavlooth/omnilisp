# Memory Model Proof Matrix

- status: active
- date: 2026-04-26
- owner: runtime/memory agents
- TODO backing: `docs/todo_parts/todo_part_18.md`
- current baseline: memory runtime green, validation green, FFI foreign runtime yellow

## Purpose

The ScopeRegion TEMP/ESCAPE memory model is implemented and the live migration
queue is closed, but the next quality bar is explicit proof coverage across the
whole runtime. This plan defines what every memory-sensitive subsystem must
prove, measure, and harden before it is treated as fully covered by the new
model.

The governing question is:

Can every runtime object, wrapper, native payload, async handle, and boundary
return path obey ScopeRegion ownership without creating a second Omni value
lifetime system?

## Global Invariants

- `ScopeRegion` remains the owner of ordinary Omni language values.
- TEMP allocations may be reset or destroyed without invalidating committed
  ESCAPE roots.
- ESCAPE roots must not retain reachable Omni-owned edges into non-surviving
  TEMP storage.
- `scope_adopt` remains retired from normal return flow.
- `scope_retain` and `scope_release` may retain whole lifetime envelopes, not
  individual language values.
- Ordinary language values must not gain per-type RC or hidden GC ownership.
- External resource exceptions must be rare, explicit, and local: they may own
  foreign payloads, but not arbitrary Omni `Value` graphs.
- Every allocation or destructor-registration failure on an owning path must
  fail closed and clean partial native/heap state.
- Unknown boundary, bridge, copy, or traversal modes must fail closed.
- Fallbacks are allowed only when they preserve the boundary contract and remain
  observable through route/fault counters or tests.

## Shared Proof Template

Every lane below must produce the same four artifacts.

### Inventory

- List each owning constructor, destructor, finalizer, retained scope, global
  table, callback registration, native handle, boundary route, and rollback
  helper in the lane.
- Classify each owned thing as one of:
  - ScopeRegion-owned Omni value.
  - ScopeRegion-owned wrapper over foreign payload.
  - Foreign payload with explicit finalizer.
  - Process-lifetime singleton.
  - Borrowed non-owning pointer.
- Record exceptions in `memory/CHANGELOG.md` with a rollback note.

### Proof

- State the local ownership invariant.
- Show how the invariant is preserved across:
  - construction,
  - return boundary,
  - env/closure capture when relevant,
  - mutation or invalidation when relevant,
  - rollback or partial failure,
  - destruction.
- Add tests for the exact boundary where the invariant can break.

### Measurement

- Add or reuse counters for constructor counts, destructor counts, finalizer
  counts, retained/released scope counts, boundary route counts, fail-closed
  counts, copy/materialization/transplant bytes, and native acquire/release
  deltas as appropriate.
- Benchmarks must report correctness counters first. Timing gates are deferred
  until repeated bounded-container runs prove stability.

### Hardening

- Check every allocation and destructor/finalizer registration on owning paths.
- Roll back partial state explicitly.
- Prefer shared helpers/macros for repeated ownership checks.
- Add forced fault-injection tests for wrapper allocation, payload allocation,
  destructor registration, bridge allocation, copy failure, and finalizer
  failure where supported.
- Run targeted bounded validation and the smallest meaningful broad gate.

## Lanes

### `MEM-PROOF-001` Inventory And Manifest Coverage

Goal: produce the complete memory ownership inventory and wire missing manifest
coverage checks.

Prove:
- Every memory-sensitive file belongs to at least one proof lane.
- Every `ValueTag` and every native/FFI wrapper family has declared ownership,
  edge traversal, copy/materialization, destructor, rollback, and graph-audit
  policy.

Measure:
- Count unclassified constructors, destructors, finalizers, callbacks, and
  global tables.
- Add a guard that fails if a new `ValueTag` or bridge family lacks policy
  classification.

Harden:
- Extend `scripts/boundary_value_policy_manifest.tsv` or add adjacent manifests
  for native/async/FFI surfaces.
- Add a script gate that reports unclassified owning call sites.

Closure:
- Inventory committed.
- Guard passes in bounded validation.
- No unclassified owning constructor/destructor path remains.

### `MEM-PROOF-002` ScopeRegion Core

Goal: prove ScopeRegion is the only ordinary language-value ownership authority.

Prove:
- TEMP and ESCAPE teardown order is deterministic.
- ESCAPE destructor splicing preserves order and generation correctness.
- `scope_retain` / `scope_release` symmetry holds for child scopes and detached
  env scopes.
- Destructor-registration OOM fails closed in TEMP and ESCAPE lanes.
- Cross-thread owner guards reject invalid scope operations.

Measure:
- TEMP/ESCAPE allocation bytes, chunk bytes, slow sequences, unused slack,
  destructor registration counts, destructor cancel counts, retain/release
  deltas, and owner-thread rejection counts.

Harden:
- Keep lane reset, destroy, splice, and retain/release checks in shared
  ScopeRegion helpers.
- Add forced OOM tests for chunk allocation and TEMP/ESCAPE destructor records.
- Add misuse tests for owner-thread and invalid parent/child splices.

Closure:
- ScopeRegion unit tests, bounded `memory-lifetime-smoke`, and Valgrind smoke
  pass.
- No `scope_adopt` normal-return path exists.

### `MEM-PROOF-003` Value Constructors

Goal: prove all heap-backed values are ScopeRegion-owned and fail closed.

Prove:
- Scalar no-destructor values are explicitly classified.
- Heap-backed constructors register the correct destructor or explicitly use
  ESCAPE build-scope rollback.
- Constructor failure cleans wrapper and payload allocations.

Measure:
- Constructor counts by tag, heap payload bytes, destructor registration
  failures, constructor fail-closed counts.

Harden:
- Replace unchecked constructor destructor registrations with checked helpers.
- Add forced OOM tests for wrapper, payload, and dtor-record allocation per
  heap-backed family.

Closure:
- Constructor fault-injection slice passes.
- Manifest says every `ValueTag` has constructor/destructor policy.

### `MEM-PROOF-004` Env And Closure

Goal: prove environment and closure lifetimes remain region-centric.

Prove:
- Env-copy success requires registered teardown for copied frames.
- Closure `env_scope` retain/release symmetry holds through copy, stable
  materialization, iterator wrapping, JIT delayed closure construction, and
  rollback.
- No closure reachable from a committed ESCAPE root retains TEMP state unless
  the result graph explicitly carries a valid continuation/context owner.

Measure:
- Env-copy frame counts, binding counts, env-scope retain/release deltas,
  closure materialization counts, closure rollback counts.

Harden:
- Centralize closure/env-scope retain and release paths.
- Add forced faults for env-copy dtor registration, env-scope allocation,
  closure wrapper allocation, signature copy, and closure-specific destructor
  registration.

Closure:
- Env-copy, closure lifecycle, JIT closure, and memory-lifetime smoke slices
  pass.
- Boundary graph audit has no unexpected closure TEMP-edge diagnostics.

### `MEM-PROOF-005` Boundary Commit Routes

Goal: prove every return boundary route is explicit, observable, and
fail-closed.

Prove:
- Planner route selection covers reuse, region transplant, stable
  materialization, compatibility destination, direct promotion, and fail-closed
  cases.
- Rejected transplant proof can fall back only to a route with an independent
  valid proof.
- Destination builders clean partial graphs on nested child failure.
- Error propagation does not suppress broken boundary contracts.

Measure:
- Planned and selected route counters, transplant proof rejection reasons,
  materialization node/byte counts, copy fallback counts, fail-closed counters.

Harden:
- Check all staged build-scope destructor registrations.
- Add tests for nested fault rollback, direct-promotion disallowance, forced
  no-splice materialization, and destination error build failure.

Closure:
- `memory-lifetime-smoke`, boundary policy guard, and benchmark envelope pass.
- Every route has at least one positive and one fail-closed regression.

### `MEM-PROOF-006` Stable Escape, Prepared Graphs, And Transplant

Goal: prove stable indexed metadata and region transplant are constrained fast
paths, not hidden ownership authorities.

Prove:
- Stable handles do not make raw source pointers authoritative after mutation
  or scope teardown.
- Prepared graph indices remain valid or fail closed.
- Mutation epochs invalidate stale prepared graphs.
- Region transplant requires parent/child legality, refcount-one child,
  owner-thread match, lane proof, and graph-audit proof.

Measure:
- Stable publication/materialization counts, mutation invalidations, stale
  handle rejects, transplant attempts/successes/rejection reasons, copy-debt
  bytes by tag.

Harden:
- Keep passport validation and mutation invalidation in shared helpers.
- Add stale index, mutation drift, cyclic graph, shared-child, and refcount
  rejection tests.

Closure:
- Stable escape rollout tests and memory-lifetime benchmark pass.
- No stable handle can outlive its proof without validation.

### `MEM-PROOF-007` Collections And Mutation

Goal: prove arrays, dictionaries, sets, method tables, iterators, partials, and
lists preserve ownership and rollback invariants.

Prove:
- Container copy/materialization tracks all child edges.
- Rollback cleans nested copied children once, including shared children and
  cycles.
- Mutation invalidates stable assumptions before a stale prepared graph can be
  reused.
- Known-capacity constructors avoid growth churn without bypassing checked
  insertion paths.

Measure:
- Construct counts, growth counts, capacity slack, rollback counts, stable
  materialization bytes by container tag, mutation invalidation counts.

Harden:
- Add shared rollback helpers for repeated graph/container shapes.
- Add forced OOM tests for item buffer, hashmap entries, set entries, method
  table entries, and destructor registration.

Closure:
- Container slices and benchmark envelope report zero known-entry growth
  regressions.
- Mutation-drift tests fail closed.

### `MEM-PROOF-008` Native Tensor, ML, And Device Paths

Goal: prove native/device payload ownership does not bypass ScopeRegion wrapper
authority.

Prove:
- Each CPU/native/CUDA/Vulkan tensor payload has exactly one cleanup authority.
- Wrapper destructor registration failure releases or avoids acquiring native
  payloads.
- Boundary copy/promotion/materialization never duplicates native/device
  ownership accidentally.
- Device-context/shared-resource refcounts are local foreign-resource policy,
  not Omni value ownership.

Measure:
- Tensor wrapper counts, payload bytes, device allocation/free counts,
  acquire/release deltas, destructor registration failures, backend capability
  fail-closed counts.

Harden:
- Replace remaining unchecked native-result destructor registration sites with
  checked helpers.
- Add Valgrind/ASAN or backend-specific leak checks for targeted CPU/native
  slices; add CUDA/Vulkan mock or capability-gated cleanup assertions where
  device runtime is unavailable.

Closure:
- Native tensor/ML audit residuals remain closed.
- Targeted tensor/ML memory slices and relevant backend capability tests pass.

### `MEM-PROOF-009` Async, Scheduler, Threads, And Callbacks

Goal: prove asynchronous execution cannot keep dead Omni values alive through
raw pointers or callbacks.

Prove:
- Jobs, futures, offload handles, process wrappers, sockets, TLS handles, and
  callbacks have explicit ownership and teardown.
- Captured Omni values either live in a retained ScopeRegion envelope or are
  copied/bridged explicitly.
- Cancellation and teardown release captured resources exactly once.
- Cross-thread scope access is rejected unless the API explicitly transfers or
  retains a valid envelope.

Measure:
- Job/callback registration and unregistration counts, captured scope
  retain/release deltas, cancellation cleanup counts, thread rejection counts,
  process/socket/TLS finalizer counts.

Harden:
- Add callback-after-source-release tests.
- Add cancellation-before-start, cancellation-during-run, and teardown-after-run
  tests.
- Make unknown async capture modes fail closed.

Closure:
- Scheduler, async, TLS/process, and memory smoke slices pass.
- No async path stores raw Omni `Value*` without an explicit scope envelope or
  bridge declaration.

### `MEM-PROOF-010` FFI ScopeRegion Migration Closure

Goal: close the yellow FFI foreign runtime area by proving FFI wrappers are
ScopeRegion-owned and foreign payloads are explicitly finalized.

Prove:
- Every FFI wrapper is an ordinary Omni value owned by a ScopeRegion.
- Every foreign payload has exactly one finalizer authority.
- Boundary movement uses an explicit bridge mode: opaque, keepalive, copy-hook,
  trace-hook, or fail-closed.
- Foreign RC/finalizers never own ordinary Omni `Value` graphs.
- Callback captures have explicit keepalive/bridge declarations.

Measure:
- FFI wrapper constructions by bridge mode, boundary crossing counts, bridge
  copy/keepalive/fail-closed counts, dlopen/native handle acquire/release
  deltas, callback registration/unregistration counts, finalizer counts,
  Valgrind leak/UAF results.

Harden:
- Extend FFI bridge manifests to cover every wrapper family.
- Check every FFI constructor destructor registration.
- Add tests for return boundary, env/closure capture, destruction, callback
  after source-scope release, and callback teardown before native release.

Closure:
- FFI policy manifest covers all wrapper families.
- Bounded FFI slices and traced-child Valgrind pass with no definite,
  indirect, or possible leaks and no invalid accesses.
- `docs/areas/ffi-foreign-runtime.md` may move from yellow to green only after
  this lane closes and `scripts/check_status_consistency.sh` is updated.

## Validation Ladder

Use the smallest meaningful validation first, then broaden.

1. C3 LSP diagnostics for touched C3 files.
2. Targeted unit/slice tests for the changed lane.
3. Bounded container build:
   `OMNI_VALIDATION_TIMEOUT_SEC=300 scripts/run_validation_container.sh c3c --threads 1 build --obj-out obj_container`
4. Bounded memory smoke:
   `OMNI_VALIDATION_TIMEOUT_SEC=600 scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
5. Bounded benchmark or envelope when counters changed:
   `OMNI_BOUNDARY_BENCH=1 OMNI_LISP_TEST_SLICE=memory-lifetime-bench`
   followed by `scripts/check_memory_telemetry_benchmark_envelope.sh`.
6. Valgrind or ASAN when native/FFI/destructor behavior remains suspect.
7. `scripts/check_status_consistency.sh`.
8. `git diff --check`.

## Green Criteria For The Whole Matrix

The model is proof-complete when:

- All ten TODO-backed `MEM-PROOF-*` lanes are closed.
- No owning constructor/destructor/finalizer/callback path is unclassified.
- All ordinary Omni language values remain ScopeRegion-owned.
- All ownership exceptions are limited to explicit foreign resources and are
  recorded in `memory/CHANGELOG.md`.
- Memory-lifetime smoke, relevant targeted slices, benchmark envelope, and
  required Valgrind/ASAN gates pass in bounded validation.
- `TODO.md` returns to actionable count `0`.
- `docs/areas/memory-runtime.md`, `docs/areas/validation-status.md`, and, after
  FFI closure, `docs/areas/ffi-foreign-runtime.md` accurately reflect the new
  status.
