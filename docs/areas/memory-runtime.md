# Memory and Runtime

Status: `green` (boundary hardening runtime closure and L5 sync gates are closed)  
As of: 2026-03-09

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
- Splice legality checks are reason-coded (`BoundaryScopeTransferReason`) and enforced through `boundary_check_scope_transfer(...)`.
- `ScopeRegion` escape splice uses O(1) tail-link concatenation (`escape_chunks_tail`, `escape_dtors_tail`) with consistency assertions.
- Boundary guard scripts exist and are wired:
  - `scripts/check_boundary_facade_usage.sh`
  - `scripts/check_boundary_change_policy.sh`
  - `scripts/run_boundary_hardening.sh`
- Verification status for this hardening wave is current:
  - `c3c build` passed.
  - `c3c build --sanitize=address` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1678/0`, `compiler: 85/0`).
  - `scripts/run_boundary_hardening.sh` passed end-to-end (Stage 0 through Stage 8, including Stage 4 ASAN with leak detection enabled).
- Boundary return-path telemetry now reports zero copy fallback pressure in both profiles (`copy_fallback_total=0` in normal and ASAN boundary hardening runs).
- Session 44 docs closure artifact is published:
  - `docs/BOUNDARY_ARCHITECTURE_AUDIT_2026-03-10.md` defines boundary invariants contract and residual risk list.
- Memory/ownership test policy is split into explicit lanes:
  - `memory-lifetime` remains a compatibility alias for `memory-lifetime-smoke`.
  - `memory-lifetime-smoke` owns boundary/scoping/coroutine ownership correctness.
  - `memory-lifetime-policy` owns boundary-policy parser/config contracts.
  - `memory-lifetime-bench` owns boundary allocation/perf coverage.
  - `memory-lifetime-soak` owns explicit heavy saturation/stress ownership probes.
  - `memory-stress` owns the global stress profile.
  - `allocator-validation` owns AST allocator correctness (non-benchmark).
  - `allocator-bench` owns AST throughput/benchmark-only coverage.
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
- Remaining updates are maintenance/synchronization hygiene, not runtime hardening correctness gaps.
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

1. Keep `memory/CHANGELOG.md`, `TODO.md`, and `memory/DESTINATION_ARENA_PLAN.md` closure wording synchronized per landing.
2. Keep `scripts/run_boundary_hardening.sh` and policy checks as required gate runs for boundary-sensitive changes.
3. Continue pruning stale fallback-era wording in adjacent area docs as follow-on maintenance proceeds.

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
