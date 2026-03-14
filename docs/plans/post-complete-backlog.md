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

### Numeric Dispatch Coercion Tightening

Runtime dispatch now uses exact-type-or-subtype applicability only.
Cross-numeric matching is explicit via constructors (for example `(Double 7)`).

- [x] Lock the language rule: dispatch does not silently coerce numeric arguments; method applicability is exact type match or subtype only.
- [x] Remove dispatch-only `Integer` -> `Double` widening/scoring from the runtime compatibility path in `src/lisp/eval_dispatch_match.c3` and any shared typed-call boundary helpers that currently reuse that rule.
- [x] Preserve explicit numeric conversion through constructors/aliases only:
  - [x] `(Double 3)` remains the canonical Integer -> Double conversion path.
  - [x] `(Integer 3.9)` and `(Int 3.9)` remain the canonical Double -> Integer conversion paths.
- [x] Update normative docs to remove dispatch-widening language and replace it with explicit-conversion guidance:
  - [x] `docs/LANGUAGE_SPEC.md`
  - [x] `docs/type-system-syntax.md`
  - [x] `docs/reference/04-type-system.md`
  - [x] `docs/areas/types-dispatch.md`
- [x] Add regression coverage for the semantic change:
  - [x] exact-match dispatch still wins (`^Integer` on integer, `^Double` on double),
  - [x] subtype dispatch still works unchanged,
  - [x] integer argument no longer matches a `^Double` method implicitly,
  - [x] explicit constructor conversion restores the intended route (`(only-double (Double 7))`),
  - [x] ambiguity/explain output stays deterministic after widening removal.
- [x] When implementation starts, record the behavior change in the current-state changelog/docs first, then run targeted dispatch tests plus `c3c build`.

Completed items were archived to:
- `docs/plans/TODO_ARCHIVE_2026-03-11.md`
