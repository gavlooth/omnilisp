# Memory and Runtime

Status: `green` (boundary hardening, nested fail-closed wrapper promotion, bounded runtime/JIT gates, and release-signal cleanup are all currently validated)
As of: 2026-04-10

## Canonical Sources

- `memory/CHANGELOG.md` (**primary current-state source of truth**)
- `memory/DESTINATION_ARENA_PLAN.md` (target architecture + closure markers)
- `docs/BOUNDARY_ARCHITECTURE_AUDIT_2026-03-10.md` (Session 44 architecture audit + invariants contract)
- `docs/BOUNDARY_RUNTIME_AUDIT_2026-03-05.md` and `docs/BOUNDARY_SURFACE_AUDIT.md`
- `docs/areas/memory-runtime-cycle.md` (end-to-end architecture and cycle diagrams)
- `AGENTS.md` runtime invariants and ownership guardrails

Conflict rule: if current status in `DESTINATION_ARENA_PLAN.md` disagrees with
validated runtime behavior, follow `memory/CHANGELOG.md` and this area doc.

## Current State

- Dual-lane `ScopeRegion` (`TEMP` and `ESCAPE`) is implemented.
- Boundary facade is active (`boundary_*` helpers).
- `scope_splice_escapes` and `scope_reset_temp_lane` are live in runtime paths.
- `scope_adopt` is not present in current `src/` runtime callsites.
- Scoped finalize unification is live via `boundary_finalize_scoped_result(...)`, and eval/JIT finalize flows share that helper.
- Boundary state restore uses `BoundarySession` (`boundary_session_begin/end`) in audited helpers and regression probes.
- Nested `CONS` / `PARTIAL_PRIM` / `ITERATOR` boundary-copy and ESCAPE
  promotion paths now fail closed when they encounter opaque primitive payload
  state transitively; they no longer silently embed null/error payloads into
  rebuilt structured wrappers.
- Shared-wrapper abort paths for `ARRAY`, `HASHMAP` / `SET`, and
  `METHOD_TABLE` now unwind already-copied child ownership side effects before
  freeing the partially built heap wrapper, so failed late-element copy does
  not leak retained closure env scopes or other copied child resources.
- That cleanup path now also descends through copied nested `CONS` shells, so
  partial cleanup remains correct even when the already-copied retained child
  was itself wrapped in another copied structured value before the later
  failure.
- Shared rollback/partial-cleanup helpers now tombstone the copied target-scope
  dtor entry before manually unwinding a materialized child, so env-copy,
  ordinary boundary copy, root-store clone, and ESCAPE-promotion late-failure
  cleanup no longer consume copied closure/env retains and then replay the
  same destructor again during later scope teardown.
- The same late-abort cleanup contract now also holds for root-store
  `ARRAY` / `HASHMAP` / `SET` clone routing, not just ordinary boundary copy
  and ESCAPE promotion.
- Destination-escape commit builders now fail closed on boundary-generated
  nested child copy faults: `CONS`, `PARTIAL_PRIM`, and `ITERATOR` destination
  builds bubble those faults back out as top-level error commits instead of
  rebuilding wrappers that contain embedded boundary `ERROR` children.
- Env-copy iterator rollback now unwinds already-copied iterator payloads even
  when the payload was materialized as a copied `PARTIAL_PRIM`, so wrapper
  construction failure does not leave nested closure retains live in the target
  scope.
- Env-copy mid-frame rollback now descends into copied `CONS`, `ARRAY`,
  `HASHMAP` / `SET`, and `METHOD_TABLE` bindings as well, so nested copied
  closure/env retains are unwound immediately when one later binding fails
  instead of surviving until the abandoned target scope is released.
- Env-copy rollback now also tombstones the copied target-scope dtor entry
  before manually unwinding a materialized value, so failed env-copy no longer
  consumes a copied closure/env retain during rollback and then replays the
  same destructor again when the abandoned target scope is later released.
- Fast reuse for target-chain shared wrappers now walks nested `ARRAY`,
  `HASHMAP` / `SET`, and `METHOD_TABLE` payload edges before returning wrapper
  identity, so target-chain reuse no longer aliases a wrapper whose nested
  child still points into the releasing scope.
- `PARTIAL_PRIM` / iterator reuse now applies the same nested payload check to
  `first_arg` / `second_arg`, so target-chain partial wrappers no longer alias
  a shared-wrapper argument whose nested child still belongs to the
  releasing/source scope.
- JIT TCO env-frame transfer now applies that same nested alias-safety rule
  before keeping a target-chain binding by identity, so target-chain
  partial/iterator bindings no longer bypass copying when their shared-wrapper
  args still point into the releasing scope.
- Destination-builder memo entries are now explicitly treated as temporary
  build-scope state: nested child routing may memoize within one builder
  invocation, but those memo nodes are discarded when the builder returns or
  aborts and are not part of same-epoch reuse semantics.
- Splice legality checks are reason-coded (`BoundaryScopeTransferReason`) and enforced through `boundary_check_scope_transfer(...)`.
- `ScopeRegion` escape splice uses O(1) tail-link concatenation (`escape_chunks_tail`, `escape_dtors_tail`) with consistency assertions.
- Boundary guard scripts exist and are wired:
  - `scripts/check_boundary_facade_usage.sh`
  - `scripts/check_boundary_change_policy.sh`
  - `scripts/run_boundary_hardening.sh`
- Focused runtime follow-up guard scripts also exist:
  - `scripts/check_scheduler_state_guards.sh`
  - `scripts/check_jit_env_scope_guards.sh`
- Verification status for the already-closed hardening profile remains current:
  - `c3c build` passed.
  - bounded `memory-lifetime-smoke` passed at `pass=92 fail=0`.
  - `rm -rf build/obj/linux-x64 build/main && mkdir -p build/obj/linux-x64/tmp_c_compile && c3c build --sanitize=address` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1678/0`, `compiler: 85/0`).
  - `scripts/run_boundary_hardening.sh` passed end-to-end (Stage 0 through Stage 8, including Stage 4 ASAN with leak detection enabled).
- Latest boundary smoke regression evidence:
  - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` passed (`unified: 92/0`).
- Boundary return-path telemetry now reports zero copy fallback pressure in both profiles (`copy_fallback_total=0` in normal and ASAN boundary hardening runs).
- Env-copy iterator payloads now route closure thunks through the same safe
  undelimited global-env clone helper as plain closure bindings, so iterator
  wrappers no longer fail closed more aggressively than the underlying closure
  contract.
- Detached recursive closure publication now fails closed if typed
  method-signature cloning into the detached env scope fails; it no longer
  silently publishes a downgraded closure with `type_sig = null`.
- Session 44 docs closure artifact is published:
  - `docs/BOUNDARY_ARCHITECTURE_AUDIT_2026-03-10.md` defines boundary invariants contract and residual risk list.
- Memory/ownership test policy is split into explicit lanes:
  - `memory-lifetime` maps to `memory-lifetime-smoke`.
  - `memory-lifetime-smoke` owns boundary/scoping/coroutine ownership correctness and is the minimum container lane for boundary/lifetime changes.
  - `memory-lifetime-policy` owns boundary-policy parser/config contracts; it is not a generic syntax/compiler lane.
  - `memory-lifetime-bench` owns boundary allocation/perf coverage when the touched change is boundary/lifetime allocation-sensitive.
  - `memory-lifetime-soak` owns explicit heavy saturation/stress ownership probes.
  - `memory-stress` owns the global stress profile.
  - `allocator-validation` owns AST allocator correctness (non-benchmark) and is separate from boundary/lifetime ownership coverage.
  - `allocator-bench` owns AST throughput/benchmark-only coverage and is only implied by allocator benchmark-sensitive work.
- Contributor/container guidance is explicit by ownership family:
  - boundary/lifetime lanes (`memory-lifetime*`, `memory-stress`) require `scripts/run_validation_container.sh`.
  - allocator lanes (`allocator-validation`, `allocator-bench`) are separate from boundary/lifetime lanes and should be chosen only when allocator behavior or benchmarks changed.
  - syntax/compiler-only changes should stay on their own non-memory lanes and do not implicitly require memory-ownership coverage.
  - do not route syntax/compiler-only work through a memory lane by default; pick the lane that matches the touched semantics.
- Parser/AST benchmark instrumentation is available under `OMNI_AST_ARENA_BENCH=1`, with dedicated parser and compiler smoke summaries (`ast_parser_smoke`, `ast_compiler_smoke`) for AST allocator shape validation.
- Boundary graph traversal/copy-fallback routing is no longer a production return-path mechanism for eval/JIT finalize flows:
  - boundary commit paths now return explicit hard outcomes for disallowed fallback classes,
  - root/persistent-store mutation paths are routed through explicit destination-aware helpers (not deep-copy fallback branches),
  - graph traversal remains a debug/audit surface only.
- Committed-root graph-reachability validation is now debug-only and threshold-gated:
  - controlled by `OMNI_BOUNDARY_GRAPH_AUDIT`,
  - sampled by `OMNI_BOUNDARY_GRAPH_AUDIT_RATE` (default 1),
  - bounded by `OMNI_BOUNDARY_GRAPH_AUDIT_MAX_ROOTS` (default unlimited),
  - and reported through boundary telemetry counters (`graph_audit_*`) when verbose diagnostics are enabled.

## Known Drift

- `memory/DESTINATION_ARENA_PLAN.md` closure language must remain synchronized with changelog-backed validation.
- Historical ASAN repro artifacts are retained for forensics; current reruns are fully green for the active hardening profile.
- `scripts/check_jit_env_scope_guards.sh` is now the narrow JIT env/scope regression gate; it is green again after the iterator closure-boundary fix and the later continuation/effect replay repair.
- The bounded `basic` slice is green again after aligning explicit JIT parity checks with the top-level finalize/promote contract used by `run(...)`.
- The former `advanced` continuation/effect failure cluster is fully closed:
  - exact bounded repros are green,
  - the full bounded `advanced` slice is green again,
  - and the container stack-budget mismatch that used to make the runner look unstable is fixed in `scripts/container_exec.sh`.
- Telemetry visibility for boundary policy gates is now tracked in verbose dumps (`graph_audit_invoked`, `graph_audit_skipped_rate`, `graph_audit_skipped_max_roots`) so hot-path scan suppression is observable.

Repro artifacts:

- `build/asan_sequential_repro.log`
- `build/asan_fiber_temp_repro.log`
- `build/boundary_hardening_asan.log`
- `build/boundary_hardening_summary.json`

## Non-Negotiable Architecture Constraints

- RC scope/region ownership remains primary authority.
- No per-type RC lifetime model for language values.
- No root pinning as a general correctness workaround.
- Boundary paths remain source of truth for return/env/mutation/promotion semantics.

## Next Steps

1. Use `scripts/run_validation_status_summary.sh build/validation_status_summary.json` as the broad bounded-gate snapshot before drilling into narrower runtime guards.
2. Keep `memory/CHANGELOG.md`, `TODO.md`, and `memory/DESTINATION_ARENA_PLAN.md` closure wording synchronized per landing.
3. Keep `scripts/run_boundary_hardening.sh` and policy checks as required gate runs for boundary-sensitive changes.
4. Keep nested wrapper fail-closed coverage (`CONS` / `PARTIAL_PRIM` / `ITERATOR` with opaque primitive payloads) in the bounded smoke lane when touching boundary-copy or ESCAPE promotion code.
5. Use `scripts/check_scheduler_state_guards.sh` and `scripts/check_jit_env_scope_guards.sh` as the narrow release-signal reruns before escalating to broader runtime slices.
6. Treat any new bounded `advanced`, `basic`, or `memory-lifetime-smoke` regression as a fresh blocker instead of reopening stale historical notes here.
7. Keep runtime modularization queue updates in sync with `docs/plans/runtime-modularization-split-2026-03-11.md` and `memory/CHANGELOG.md` when deduce/runtime test splits land.
8. Keep contributor guidance aligned with lane ownership: boundary/lifetime lanes stay container-bound, allocator lanes stay separate, and syntax/compiler-only work should not inherit memory lanes by convenience.

## Concurrency Boundary Plan Alignment

`docs/plans/concurrency-hybrid-memory-checklist.md` is the governing checklist
for future concurrency ownership evolution.

- Worker/scheduler offload boundaries route byte payloads through
  `SharedHandle(kind=BLOB)` and no longer expose raw `SharedBlob*` at the
  production concurrency boundary.
- The explicit local/sendable/shared bridge API is introduced for offload byte
  payloads, and a scheduler-owned `SharedHandle` registry with generation
  validation is now present for persistent shared-object identity.
- Shared-handle projection into local `Value*` is now one-way and explicit at
  scheduler boundary completion consumption.
- Remaining concurrency ownership work is focused on later-phase
  shared-object/domain lifecycle consolidation.
- Offload path behavior changed in this migration; this section tracks phase
  sequencing while further shared-object consolidation proceeds.
