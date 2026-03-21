# Area Documentation Hub

This folder provides one operational status document per major area.
Each area file answers:

1. What is the canonical source of truth?
2. What is the current implementation status?
3. What are the concrete next steps?

Deep design history and detailed rationale remain in the existing docs.
Area docs are the execution-facing "you are here" layer.

## Authority Order

When documents disagree, use this precedence:

1. `memory/CHANGELOG.md` (latest validated implementation truth)
2. Area docs in this folder (current operator view + next steps)
3. Deep design docs/plans (architecture intent and rationale)

## Areas

- `memory-runtime.md`
- `memory-runtime-cycle.md` (end-to-end architecture and lifecycle diagrams)
- `effects-error-model.md`
- `types-dispatch.md`
- `compiler-parser-refactor.md`

## Status Legend

- `green`: implemented and validated at current baseline.
- `yellow`: partially implemented or needs parity cleanup.
- `red`: known regression/blocker.

## Maintenance Rule

When behavior, validation state, or roadmap priority changes:

1. Update `memory/CHANGELOG.md` first with concrete validation outcomes.
2. Update the relevant area doc with current status and next steps.
3. Update deep source docs (specs/plans) to remove drift.
4. Keep next steps explicit and testable.

## Operator Entry Point

For a single machine-readable snapshot of the current bounded gate status, use:

- `scripts/run_validation_status_summary.sh build/validation_status_summary.json`

That artifact is the preferred broad status entrypoint before drilling into
area-specific docs or narrower guard scripts.
