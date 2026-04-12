# Plans Index

This folder now holds supporting implementation notes and design documents.
The live backlog has been consolidated into `TODO.md` at the repo root.

## Usage Rules

1. Use `memory/CHANGELOG.md` as current-state truth.
2. Keep `TODO.md` as the only live execution queue.
3. Treat files in this folder as design context or one-off implementation notes, not as parallel backlog sources.

## Current TODO-Linked Plans

There are currently no open live-queue entries in `TODO.md`.

Recently closed TODO-linked plans:

- `ffi-first-class-grouped-module-plan-2026-04-11.md`: grouped C ABI FFI,
  grouped bindgen output, generated bind manifests, strict bind TOML hardening,
  and `exclude-functions` denylist filtering.
- `foreign-runtime-core-plan-2026-04-11.md`: common `ForeignHandle` runtime
  descriptor and adapter boundary for future C, Python, Julia, CUDA/cuBLAS,
  optional C++ tooling, and polyglot lanes; the common-core lane is closed
  through `FOREIGN-CORE-002R`.
- `tensor-scientific-computing-plan-2026-04-11.md`: canonical `Tensor`,
  tensor-dispatched `map`, `contract`, `materialize`; public surface cleanup,
  singleton-axis broadcasting, and backend-boundary design are closed.

Historical plans in this directory remain useful context, but they are not
live backlog entries unless `TODO.md` explicitly references them.

Recently closed:

- `validation-all-slice-2026-04-11.md`: non-Tensor all-slice failure follow-up;
  final bounded all-slice-without-TLS result used
  `OMNI_SKIP_TLS_INTEGRATION=1` and is unified `pass=2798 fail=0`, compiler
  `pass=208 fail=0`; this is not a replacement for the TLS integration gate.
- `validation-all-slice-nested-let-residual-2026-04-11.md`: closed after
  balancing the malformed memory-stress nested-let fixture.
