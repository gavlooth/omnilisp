# Declarative FFI Execution Contract Decision (2026-04-08)

Status: accepted current-state decision

## Context

Omni currently ships three interop lanes:

1. Declarative FFI (`define [ffi lib]` / `define [ffi λ]`)
2. Bindgen-generated raw-plus-facade modules under `lib/ffi/`
3. Explicit C ABI shims plus Omni facades for complex subsystems

Bindgen output itself is declarative FFI code. This means bindgen and
declarative FFI execution-mode semantics must stay aligned.

Current compiler tests assert that declarative FFI forms lower through the AOT
runtime bridge for supported scalar and foreign-handle ABI tags:

- `Compiler: declarative ffi lib lowers through AOT runtime bridge`
- `Compiler: declarative ffi function lowers through AOT runtime bridge`
- `Compiler: declarative ffi lowers foreign-handle/bool/double ABI tags`

Source anchor:
- `src/lisp/tests_compiler_core_groups.c3`

## Options Considered

1. Carry `ForeignHandle` metadata policy through the AOT bridge explicitly with
   generated descriptors for parameter and return policy.
2. Claim full AOT policy support for declarative FFI without carrying
   `ForeignHandle` metadata through generated runtime declarations.
3. Hide declarative FFI from user-facing docs until AOT parity exists.

## Decision

Adopt option 1.

Current shipped contract:

- Declarative FFI is interpreter/JIT policy-complete for the current scalar and
  `ForeignHandle` metadata dictionary surface.
- AOT lowering supports the scalar/foreign-handle ABI tag path and carries
  `ForeignHandle` metadata through generated policy descriptors.
- Bindgen remains useful for interpreter/JIT workflows and for producing
  reviewable wrappers; generated declarative FFI now preserves the current
  `ForeignHandle` handle-family/nullability/ownership/finalizer policy through
  the AOT bridge.
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
