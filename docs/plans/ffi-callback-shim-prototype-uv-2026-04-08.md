# Shim-Only Callback Prototype Plan (`uv`) (2026-04-08)

Status: historical prototype plan; not a live TODO lane

## Scope

Prototype the first callback lane as shim-only on one concrete subsystem:

- subsystem: `uv` timer callback shape (single callback registration + teardown),
- no generic declarative-FFI callback coercion,
- explicit handle lifecycle and deterministic unregister path.

## Prototype Contract

1. Register path returns an opaque handle.
2. Native callback entrypoint resolves handle and dispatches retained Omni
   callable.
3. Unregister path invalidates handle and releases retained payload exactly once.
4. Post-unregister invocation fails closed (no stale dispatch).

## Implementation Targets

1. C shim helpers (`csrc/uv_helpers*.c`) for:
   - register callback handle,
   - invoke trampoline by handle,
   - unregister callback handle.
2. Omni runtime bridge layer in existing `uv` primitives:
   - explicit register/unregister APIs around the shim handle.
3. No changes to generic `[ffi λ]` callback coercion in this prototype.

## Focused Tests

1. Register + invoke:
   - callback executes and receives expected payload/context.
2. Unregister idempotence:
   - repeated unregister does not double free.
3. Stale handle:
   - callback after unregister fails closed.
4. Owner-scope release:
   - retained callback payload is released once at teardown.
5. Isolation:
   - two handles do not cross-dispatch.

## Exit Criteria

Prototype is complete when:

1. explicit register/invoke/unregister flow is implemented for the `uv` lane,
2. focused tests above pass,
3. no generic callback coercion was introduced,
4. failure paths are deterministic and fail closed.
