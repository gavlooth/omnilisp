# Stack Backend Contract Decision (2026-04-08)

## Scope
- TODO lane: `STACK-BACKEND-001`
- Surface under review: stack-engine portability boundary between runtime flow and architecture-specific ABI/switch/bootstrap details.

## Decision
- Introduce an explicit stack backend contract and route runtime call sites through it.
- Keep x86_64 as the only implemented backend for now; unsupported targets remain fail-closed.

## Contract (v1)
- Backend identity/capability:
  - `stack_backend_kind()`
  - `stack_backend_name()`
  - `stack_backend_supports_switch()`
- Context bootstrap:
  - `stack_backend_init_context(...)`
- FP/control save-restore around switches:
  - `stack_backend_save_fp_state(...)`
  - `stack_backend_restore_fp_state(...)`
- Context switch entrypoints:
  - `stack_backend_protected_switch(...)`
  - `stack_backend_switch(...)`

## First extraction pass landed
- Added backend seam in `src/stack_engine_backend_contract.c3`.
- Moved stack context bootstrap register/FPU initialization behind backend helper (`stack_ctx_init` now delegates to `stack_backend_init_context`).
- Routed runtime context switch flow (`stack_engine_context_runtime.c3`) through backend helper functions instead of direct raw calls.

## Current backend status
- Implemented backend: `x86_64-sysv`.
- Unsupported backends: fail closed with explicit backend message from `stack_ctx_init`.

## Follow-up
- Keep stack guard orchestration, ASAN hooks, lifecycle, and pool ownership in common runtime lanes.
- For new architectures (e.g., aarch64), implement backend functions behind this contract rather than branching runtime call sites.
