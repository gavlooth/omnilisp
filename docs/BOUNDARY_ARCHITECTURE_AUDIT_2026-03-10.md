# Boundary Architecture Audit (2026-03-10)

Status: Session 44 docs closure  
Scope: boundary ownership/lifetime architecture, invariants contract, and residual risks

## 1) Boundary Architecture Summary

- Ownership authority remains region-centric (`ScopeRegion`), with dual-lane memory (`TEMP`/`ESCAPE`).
- Return-boundary commit/finalize flow is consolidated on boundary facade helpers:
  - `boundary_finalize_scoped_result(...)`
  - `boundary_commit_escape(...)`
- Eval and JIT return paths are aligned on shared boundary policy and typed fault/outcome surfaces.
- Direct legacy transition primitives are guarded by policy:
  - `scripts/check_boundary_facade_usage.sh`
  - policy map `scripts/boundary_facade_policy.txt`
  - surface report `docs/BOUNDARY_SURFACE_AUDIT.md`

## 2) Invariants Contract (Normative)

These invariants are the required runtime contract for boundary-sensitive work.

1. Ownership model
- `ScopeRegion` is lifetime authority for Omni graph values.
- No per-type RC ownership model is introduced for language values.
- No root pinning is used as general correctness policy.

2. Boundary transition legality
- Cross-scope transitions must route through boundary facade entrypoints.
- Scope splice legality is reason-coded and checked before mutation.
- Commit/finalize outcomes are explicit and typed (no silent fallback branch behavior).

3. State restoration
- Boundary state transitions (`current_scope`, `releasing_scope`) must restore exactly.
- Session-like boundary helpers use paired begin/end discipline (`defer`-safe restore pattern).

4. Graph safety (debug contract)
- For committed ESCAPE roots, reachable Omni-owned edges must not retain TEMP-owned edges.
- Graph-audit hooks remain debug/telemetry policy surfaces, not normal return-path execution.

5. Guard and drift controls
- Legacy direct-call guard remains required for boundary-sensitive changes.
- Boundary surface audit must report zero violations.
- Changelog and area docs remain synchronized with landed behavior.

## 3) Closure Evidence Snapshot

- Boundary facade guard: zero violations (`scripts/check_boundary_facade_usage.sh`).
- Boundary surface audit: zero violations (`docs/BOUNDARY_SURFACE_AUDIT.md`).
- Session 44 Commit A legacy entrypoint sweep is closed:
  - retired fully replaced env-copy wrappers from runtime surface.
  - guard/audit symbol inventories now track live legacy symbols only.
- Local safety validation remains workstation-safe:
  - `c3c build`
  - targeted low-memory suites (`scope`, `stack`)
- Heavy lisp memory-lifetime path is now split for operator safety:
  - `memory-lifetime` / `memory-lifetime-smoke`: default local-safe path
  - `memory-lifetime-soak`: explicit heavy path (stress + soak)

## 4) Residual Risk List

1. Heavy soak profiles remain resource-intensive
- `memory-lifetime-soak` and `memory-stress` can still exceed workstation capacity.
- Mitigation: keep soak opt-in (`OMNI_GLOBAL_GATES_INCLUDE_LIFETIME_SOAK=1`) and run on CI/large-host runners.

2. CI/runtime environment dependency
- Boundary hardening and ASAN soak confidence still depend on runner/toolchain parity.
- Mitigation: preserve strict guard scripts and summary evidence checks.

3. Documentation drift risk
- Historical docs and active status docs can diverge without changelog-first updates.
- Mitigation: maintain precedence order:
  - `memory/CHANGELOG.md`
  - area docs
  - deep plan docs

## 5) Operator Notes

- Local developers should default to `memory-lifetime` (smoke) during iteration.
- Use `memory-lifetime-soak` only when explicitly validating stress/soak behavior.
- Boundary-sensitive changes should keep guard/audit scripts as required pre-merge checks.
