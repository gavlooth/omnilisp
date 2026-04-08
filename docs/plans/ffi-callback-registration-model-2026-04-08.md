# FFI Callback Registration Model (2026-04-08)

Status: draft follow-up model for the deferred generic callback lane

## Scope

This note defines the runtime-handle model that the deferred generic callback
lane should implement later.

It does not change the current shipped contract:

- first shipped callback support remains shim-only,
- direct closure-to-native-function-pointer coercion stays unsupported,
- generated bindgen callback scaffolding stays placeholder-only until runtime
  handles exist.

## Canonical Surface Names

Reserve these names for the generic runtime-handle lane:

1. `ffi/callback-register`
2. `ffi/callback-unregister`
3. `ffi/callback-handle`

Do not pre-claim these names as shipped behavior in user-facing docs.

## Handle Model

A callback handle is an opaque runtime object that owns:

- a stable native trampoline entrypoint,
- the retained Omni callable payload,
- an explicit user-data payload,
- the handle state and generation/validity marker,
- the teardown policy that was selected at registration time.

The handle must not expose the Omni callable as a raw C function pointer.
Foreign code can hold only the opaque handle or a native trampoline reference
that routes through the handle registry.

## Registration Flow

`ffi/callback-register` should:

1. validate that the Omni value is a callable with a supported arity,
2. validate that the requested C ABI signature is supported by the trampoline,
3. allocate a handle slot and record the state as live,
4. retain the callable payload in the owner scope or equivalent lifetime root,
5. store user-data payload and teardown metadata,
6. return the opaque handle.

Registration must fail closed:

- allocation failure returns no handle and releases any partial retention,
- unsupported callable/signature shape returns no handle,
- invalid metadata or teardown policy returns no handle,
- duplicate or inconsistent handle state must not leak a partially live slot.

## Invocation Flow

The trampoline receives the native arguments plus a handle or registry key.
It must:

1. resolve the handle,
2. verify the handle is live and matches the expected generation,
3. dispatch into the retained Omni callable,
4. convert the result back to the native ABI shape,
5. fail closed if the handle is stale, torn down, or mismatched.

The callback path must not assume the handle still exists after owner-scope
teardown. A stale callback should be dropped or reported as a runtime failure,
not treated as a valid live dispatch.

## Unregister Flow

`ffi/callback-unregister` should be explicit and idempotent.

Required behavior:

- mark the handle dead before releasing retained payloads,
- release the retained Omni callable and user-data payload exactly once,
- detach the trampoline registry entry,
- tolerate repeated unregister requests without double free,
- make a later invocation fail closed.

If unregister is triggered by owner-scope teardown, it must follow the same
single-release path as a manual unregister.

## Ownership Rules

- Omni callable ownership remains region-based.
- The callback handle is the ownership boundary, not the callable itself.
- Foreign code never receives direct ownership of Omni values.
- Handle lifetime must be explicit and deterministic.
- Handle teardown must not depend on garbage collection or finalizers that can
  run after the owner scope has already been released.

## Fail-Closed Rules

The generic callback lane must fail closed in all of these cases:

- unsupported callable arity or ABI shape,
- allocation failure during registration,
- stale or mismatched handle during invocation,
- repeated unregister,
- owner-scope teardown before foreign code stops calling back,
- registry corruption or missing trampoline entry.

Fail closed means the runtime must avoid unsafe dispatch and must preserve
ownership invariants even if the callback cannot complete normally.

## Bindgen Interaction

Bindgen metadata already tags callback-shaped parameters with:

- `role=callback`
- teardown hints such as `callback-unregister`

That metadata should eventually drive the generic lane, but the shipped code
must remain conservative until the runtime-handle implementation lands.

## Focused Tests

When the generic lane is implemented, add tests for:

1. register/invoke/unregister on a supported callback shape,
2. owner-scope release invalidates the handle and prevents later reuse,
3. repeated unregister does not double free,
4. stale invocation after teardown fails closed,
5. registration failure does not leak the retained callable or user-data,
6. multiple live handles stay isolated from each other,
7. callback return values propagate through the trampoline correctly,
8. user-data payload is preserved end to end.

Prefer tests that exercise one callback shape at a time so the failure mode is
obvious when the runtime-handle contract regresses.

## Next Implementation Step

Prototype the shim-only callback lane on one concrete subsystem using explicit
register/unregister handle flow and focused lifetime tests. Keep the generic
runtime-handle model in reserve until that subsystem proves the ownership and
teardown contract end to end.
