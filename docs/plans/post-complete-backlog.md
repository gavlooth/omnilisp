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

