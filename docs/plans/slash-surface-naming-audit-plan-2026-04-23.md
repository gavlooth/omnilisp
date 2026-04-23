# Slash Surface Naming Audit Plan - 2026-04-23

Status: Closed. `SURFACE-NAMING-001` through `SURFACE-NAMING-006` are closed.
Follow-up implementation work is tracked by the dedicated plans linked below.
This plan records the naming audit direction after the slash-symbol ergonomics
rule was documented.

## Current Decisions

- Pika is language-core, not a detachable library module. Omni should be built
  around Pika as the parser/grammar substrate, so `pika/...` remains acceptable
  as a language-core primitive family for now. The follow-up is documentation
  and surface integration, not module extraction.
- Deduce should move toward a real module/facade boundary. The current
  `deduce/...` aliases are readable, but the surface is broad enough to be a
  database subsystem rather than a compact primitive family. The accepted
  decision is `docs/plans/deduce-module-boundary-decision-2026-04-23.md`.
- ML should keep compact tensor/learning kernels as `ml/...` primitives while
  moving visualization and optimizer/checkpoint helpers toward real facades.
  The accepted decision is
  `docs/plans/ml-module-surface-split-decision-2026-04-23.md`.
- `matrix/...`, `tensor/capture`, `tensor/run`, `kernel/capture`,
  `kernel/run`, `stats/...`, and operation-level `io/...` effect tags fit the
  current rule and do not need immediate renaming.
- Math and stats should become real core scientific modules/facades. The
  accepted target surface is `math.*` and `stats.*`; current `math/...` and
  `stats/...` slash callables are transitional migration inputs, not the final
  canonical shape. The accepted plan is
  `docs/plans/math-stats-scientific-module-plan-2026-04-23.md`.

## Naming Criteria

- Use slash names for compact, always-present core families where the prefix
  helps humans scan dense operation lists.
- Keep cross-cutting generic operations unprefixed: examples include `map`,
  `ref`, `length`, `sort`, `shape`, `rank`, `dtype`, `device`, `realize`, and
  `contract`.
- Avoid deep pseudo-paths and mechanically long slash names.
- Prefer real modules/imports when a surface is optional, independently
  versioned, large, or likely to carry many subfamilies.
- Do not add compatibility aliases unless the owner explicitly requests a
  migration window.

## Work Items

- `SURFACE-NAMING-001` (closed 2026-04-23): Document Pika as language-core.
  Language and reference docs now describe `pika/...` as Omni's parser/grammar
  substrate rather than as an optional PEG helper library.
- `SURFACE-NAMING-002` (closed 2026-04-23): Produce the Deduce
  module-boundary decision. The accepted decision moves broad `deduce/...`
  command aliases toward an imported `deduce` facade and keeps the unified
  dispatcher only as the planned `deduce.dispatch` escape hatch:
  `docs/plans/deduce-module-boundary-decision-2026-04-23.md`.
- `SURFACE-NAMING-003` (closed 2026-04-23): Decide the ML module split. The
  accepted decision keeps compact tensor/learning kernels as `ml/...`
  primitives and moves visualization plus optimizer/checkpoint helpers toward
  `ml.visualization.*` and `ml.optimizers.*` facades:
  `docs/plans/ml-module-surface-split-decision-2026-04-23.md`.
- `SURFACE-NAMING-004` (closed 2026-04-23): Flatten
  `ml/linear-batched-reduce`. The runtime registration, AOT lookup, tests,
  docs, and diagnostics now use the public `ml/linear-batched-reduce`
  spelling with no compatibility alias for the old three-segment symbol.
- `SURFACE-NAMING-005` (closed 2026-04-23): Resolve special-function math
  naming. The accepted direction is a real core scientific module surface:
  `math.lgamma`, `math.erf`, `math.erfc`, and `stats.normal-*`. Do not add
  global special-function names, and do not treat `math/...` or `stats/...`
  slash names as permanent final surface:
  `docs/plans/math-stats-scientific-module-plan-2026-04-23.md`.
- `SURFACE-NAMING-006` (closed 2026-04-23): Tighten ML/NN activation docs.
  The language and primitive reference docs now state that `ml/<activation>`
  names are eager tensor activations while `nn/<activation>` names construct
  layer specs, and the shipped activation primitives are listed consistently.

## Validation Path

- For documentation-only decision updates: `git diff --check` and
  `scripts/check_status_consistency.sh`.
- For primitive renames or removals: `c3c build --obj-out obj`,
  compiler-slice tests, focused affected runtime slice, primitive-doc parity,
  status consistency, and file-size gate.
- For Deduce module-boundary changes: bounded/container validation for affected
  Deduce slices before reporting closure.

## Negative-Memory Constraints

- Do not treat slash as module lookup. Slash remains ordinary symbol syntax.
- Do not add deep slash pseudo-paths to avoid designing a real module boundary.
- Do not preserve non-canonical aliases by default; pre-alpha surface cleanup
  should converge to one canonical name unless the owner requests a migration
  window.
- Do not convert Pika to an optional module; the current owner decision is that
  Pika is part of the language substrate.
