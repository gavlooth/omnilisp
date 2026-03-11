# Post-Complete Backlog

Purpose: track non-blocking work after language feature completion.

Status: active backlog queue (no release-blocking items).

## Queue Rules

1. Do not add language-completion blockers here; blockers belong in a dedicated hotfix/release gate plan.
2. Keep items concrete, testable, and scoped to one area.
3. Prefer runtime safety/perf/tooling follow-ups over speculative redesign.

## Candidate Backlog Items

- [ ] Performance profiling pass for boundary-heavy workloads under Docker-capped gates.
- [ ] Additional micro-bench coverage for scheduler + async I/O interaction hotspots.
- [ ] CLI/tooling UX cleanup pass (`--help`, diagnostics consistency, exit-code map).
- [ ] Documentation simplification pass for legacy terminology drift in root `README.md`.
- [ ] Expand examples catalog with one minimal effect-handler + dispatch tutorial project.
- [ ] Run a pass over examples and docs for legacy syntax debt and remove deprecated syntax aliases once migration notes are complete.
- [ ] Add a minimal analytics extension task list for `deduce` covering statistics and data-cleanup verbs as a separate follow-up milestone.
- [ ] Add tests for deterministic behavior in `deduce` cleanup and statistics operations under empty-relation and malformed-schema inputs.
- [ ] Add benchmarks for deduce scan/query/copy hotspots at corpus sizes that represent regression envelopes.
- [ ] Continue largest-first modularization for oversized runtime/test modules outside the compiler/parser tracker.
- [ ] Split memory-lifetime correctness tests away from benchmark-only modules so regression coverage and throughput harnesses do not share the same files.
- [ ] Add a migration note for effect-handler composition helper names to avoid abbreviation drift in public-facing docs.

Completed items were archived to:
- `docs/plans/TODO_ARCHIVE_2026-03-11.md`
