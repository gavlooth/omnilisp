# Plans Index

This folder is the canonical location for implementation plans.

## Usage Rules

1. Use `memory/CHANGELOG.md` as current-state truth.
2. Keep one active plan per area whenever possible.
3. Move completed/superseded plans to historical context in git, not to active docs.

## Current Plan Set

- `server-example-plan.md` (retained name; points to financial-service webserver plan)
- `financial-service-webserver-plan.md` (canonical product example direction)
- `effects-typesystem-parity-plan.md`
- `compiler-parser-refactor-plan.md` (historical compiler/parser refactor tracker; queue closed 2026-03-19)
- `pika-regex-parity-plan.md`
- `asan-jit-escape-scope-overflow.md`
- `session-34-44-boundary-hardening.md`
- `fiber-temp-detailed-implementation-plan.md`
- `fiber-temp-session-plan.md`
- `fiber-temp-teardown-revision-summary.md`
- `fiber-continuation-unification.md`
- `dual-lane-scope-region-rollout.md`
- `aot-unification.md`
- `library-gaps-todo.md`
- `remaining-optimizations.md`
- `concurrency-hybrid-memory-checklist.md`
- `editor-tooling-roadmap.md` (Tree-sitter, LSP, Neovim, formatter, and machine-readable CLI tooling roadmap)
- `post-complete-backlog.md` (non-blocking queue after language completion)
- `codebase-improvement-backlog-2026-03-19.md` (ranked blocker-first backlog after the split-heavy pass)
- `runtime-backend-simplification-backlog-2026-03-19.md` (completed post-split backlog for runtime/backend structure, build iteration, and validation observability)
- `interp-state-runtime-cleanup-2026-03-19.md` (completed interpreter-state cleanup lane after runtime/backend backlog closeout)
- `boundary-graph-audit-cleanup-2026-03-19.md` (completed boundary graph audit cleanup lane after interpreter-state cleanup)
- `runtime-effects-cleanup-2026-03-19.md` (completed runtime effects cleanup lane after boundary graph audit cleanup)
- `largest-runtime-files-pass-2026-03-19.md` (active largest-first runtime split pass after the focused cleanup queues)
- `deduce-robust-datalog-roadmap.md` (Deduce roadmap: planner + semi-naive recursion + stratified negation)
- `deduce-sophistication-plan-2026-03-20.md` (concrete execution queue for the next Deduce capability/robustness pass)
- `deduce-full-roadmap-2026-03-20.md` (full non-conservative product roadmap for Deduce across v1, v1.5, and v2)
- `deduce-actionable-backlog-2026-03-20.md` (ordered actionable Deduce queue derived from the full roadmap)
- `deduce-v15-aggregate-implementation-plan-2026-03-20.md` (aggregate lane plan; recursive aggregate seminaive rollout is now shipped for the current stratified surface, with follow-up cleanup/perf notes)
- `deduce-incremental-delta-propagation-design-2026-03-13.md` (Deduce O7.8 dependency-aware incremental invalidation design)

## Active Plan Ownership Notes

- Compiler/parser modularization history is centralized in `compiler-parser-refactor-plan.md`; it is no longer an active queue.
- Runtime/backend follow-on work through the post-split cleanup is closed in `runtime-backend-simplification-backlog-2026-03-19.md`; open a fresh backlog for any new runtime queue.
- The interpreter-state follow-on queue is complete in `interp-state-runtime-cleanup-2026-03-19.md`.
- The boundary graph audit follow-on queue is complete in `boundary-graph-audit-cleanup-2026-03-19.md`.
- The runtime effects follow-on queue is complete in `runtime-effects-cleanup-2026-03-19.md`.
- The active follow-on runtime queue is now `largest-runtime-files-pass-2026-03-19.md`.
- Historical completed references that mention compiler/parser refactor slices (for example `library-gaps-todo.md`, `aot-unification.md`) are not active tracker files for this area.
