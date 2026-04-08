# Declarative FFI Execution Contract Decision (2026-04-08)

Status: accepted current-state decision

## Context

Omni currently ships three interop lanes:

1. Declarative FFI (`define [ffi lib]` / `define [ffi λ]`)
2. Bindgen-generated raw-plus-facade modules under `lib/ffi/`
3. Explicit C ABI shims plus Omni facades for complex subsystems

Bindgen output itself is declarative FFI code. This means bindgen and
declarative FFI execution-mode semantics must stay aligned.

Compiler tests currently assert that declarative FFI forms are rejected in AOT
mode:

- `Compiler: declarative ffi lib is rejected in AOT mode`
- `Compiler: declarative ffi function is rejected in AOT mode`

Source anchor:
- `src/lisp/tests_compiler_core_groups.c3`

## Options Considered

1. Keep declarative FFI interpreter/JIT-only for now, document it explicitly,
   and defer AOT support to a dedicated design/implementation lane.
2. Claim near-term AOT support for declarative FFI without a concrete lowering,
   runtime dispatch, and link/runtime-loading contract.
3. Hide declarative FFI from user-facing docs until AOT parity exists.

## Decision

Adopt option 1.

Current shipped contract:

- Declarative FFI is interpreter/JIT-only.
- AOT currently rejects declarative FFI forms.
- Bindgen remains useful for interpreter/JIT workflows and for producing
  reviewable wrappers, but generated declarative bindings are not an AOT-safe
  contract today.
- Complex libraries should continue to use explicit C ABI shims plus Omni
  facades.

## Rejected Direction

Do not imply that declarative FFI is already AOT-capable without a specific
runtime/linking design and validation path.

## Follow-up Lane

If AOT declarative FFI is requested later, open a dedicated lane with:

1. explicit AOT lowering semantics for `ffi` forms,
2. runtime loading/symbol-resolution model for compiled artifacts,
3. linker/deployment contract,
4. parity tests that move compiler expectations from rejection to supported
   behavior.
