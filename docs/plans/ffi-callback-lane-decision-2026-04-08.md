# FFI Callback Lane Decision (2026-04-08)

Status: accepted current-state decision

## Context

Current interop facts in Omni:

1. Declarative `ffi` works in interpreter/JIT mode.
2. Bindgen emits raw-plus-facade modules and now tags callback-like params with
   explicit metadata (`role=callback`, teardown hints).
3. Complex subsystems that already work well (`uv`, `TLS`, `FTXUI`, `deduce`)
   use explicit C ABI shims plus opaque handles, not direct closure-to-function-
   pointer coercion.

The open callback lane asks whether first shipped support should be:

- generic callback handles in the FFI substrate, or
- shim-only callback support for v1.

## Options Considered

1. Ship generic callback registration/trampoline support directly in declarative
   FFI.
2. Keep callback support shim-only for v1 and require explicit subsystem C ABI
   wrappers + opaque handles.
3. Keep callback invocation unsupported everywhere and leave only bindgen
   scaffolding placeholders.

## Decision

Adopt option 2 for first shipped callback support.

Current contract:

- Direct closure-to-native-function-pointer coercion remains unsupported.
- First shipped callback execution support is shim-only.
- Bindgen may emit callback metadata and wrapper scaffolding, but generated
  callback helpers stay placeholders until explicit runtime callback handles are
  implemented.
- Generic callback substrate work stays a separate follow-up lane, not an
  implicit side effect of bindgen scaffolding.

## Naming Freeze For Follow-up Generic Lane

If/when generic callback handles are implemented later, use these canonical
surface names (no shorthand aliases by default):

1. `ffi/callback-register`
2. `ffi/callback-unregister`
3. `ffi/callback-handle`

These names are reserved for the future explicit runtime-handle lane; do not
pre-claim them in docs as already shipped behavior.

## Rejected Directions

- Do not cast Omni closures to native function pointers directly.
- Do not present bindgen callback wrappers as if teardown/lifetime semantics are
  already solved.
- Do not broaden declarative FFI callback claims before runtime-handle
  ownership/teardown semantics are explicit and tested.

## Follow-up Lane

When generic callback handles are staffed, require all of:

1. runtime handle model (retain payload + native entrypoint + user-data shape),
2. deterministic unregister/teardown semantics,
3. focused tests for register/invoke/unregister and owner-scope release,
4. boundary tests for error paths and leaked-handle prevention.
