# Memory and Runtime

Status: `red` (known ASAN blocker on current baseline)  
As of: 2026-03-07

## Canonical Sources

- `memory/CHANGELOG.md` (**primary current-state source of truth**)
- `memory/DESTINATION_ARENA_PLAN.md` (target architecture + closure markers)
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
- Boundary guard scripts exist and are wired:
  - `scripts/check_boundary_facade_usage.sh`
  - `scripts/check_boundary_change_policy.sh`
  - `scripts/run_boundary_hardening.sh`

## Known Drift

- `memory/DESTINATION_ARENA_PLAN.md` Revision XV states closure/no blockers, including:
  - ASAN + `OMNI_FIBER_TEMP=1` repeated soak clean.
- Current reproducible state is a deterministic ASAN crash in advanced unified tests:
  - `AddressSanitizer: stack-buffer-overflow`
  - stack includes:
    - `src/lisp/value_symbol_table.c3:114`
    - `src/lisp/value_constructors.c3:466`
    - `src/lisp/jit_jit_handle_signal.c3:77`
    - `src/lisp/jit_jit_handle_signal.c3:151`

Repro artifacts:

- `build/asan_sequential_repro.log`
- `build/asan_fiber_temp_repro.log`
- `build/boundary_hardening_asan.log`

## Non-Negotiable Architecture Constraints

- RC scope/region ownership remains primary authority.
- No per-type RC lifetime model for language values.
- No root pinning as a general correctness workaround.
- Boundary paths remain source of truth for return/env/mutation/promotion semantics.

## Next Steps

1. P0: triage and fix the ASAN overflow on the JIT signal error path.
2. Re-run sequential gates after fix:
   - `c3c build`
   - `c3c build --sanitize=address`
   - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
   - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
   - `scripts/run_boundary_hardening.sh`
3. Record resolution and verification in `memory/CHANGELOG.md`.
4. Update `memory/DESTINATION_ARENA_PLAN.md` closure claims to match changelog-backed validation.

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
