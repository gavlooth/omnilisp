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
- `compiler-parser-refactor-plan.md` (single active compiler/parser refactor tracker)
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
- `post-complete-backlog.md` (non-blocking queue after language completion)

## Active Plan Ownership Notes

- Compiler/parser modularization tracking is centralized in `compiler-parser-refactor-plan.md`.
- Historical completed references that mention compiler/parser refactor slices (for example `library-gaps-todo.md`, `aot-unification.md`) are not active tracker files for this area.
