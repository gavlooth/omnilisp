# Declarative FFI AOT Runtime/Linking Model (2026-04-08)

Status: implemented baseline model for `INTEROP-AOT-FFI-001` (startup preload, explicit required/optional symbol policy handling, and compile-mode sidecar manifest emission are landed; broader compiler baseline still has unrelated failures)

## Scope

This note defines a concrete AOT model for declarative `ffi` support so future
implementation work has one explicit target.

It does not change current shipped behavior:

- interpreter/JIT declarative FFI remains supported,
- compiler/AOT still rejects declarative `ffi` forms,
- rejection stays enforced until this model is implemented and validated.

Implemented against this note so far:

- startup-wide FFI preload emission for supported declarative forms,
- explicit required-load policy emitted alongside AOT callsites,
- optional-symbol fail-closed runtime handling,
- compile mode emits an adjacent `*.ffi-manifest.json` contract sidecar for declarative FFI programs,
- direct AOT compile/runtime coverage for supported scalar and pointer ABI tags.

## AOT Lowering Semantics

For declarative forms:

- `[ffi lib]` lowers to an AOT-generated static descriptor:
  - logical library id,
  - library soname/base name,
  - lazy/open policy,
  - symbol-resolution mode (`required` by default).
- `[ffi λ]` lowers to an AOT-generated callsite descriptor:
  - target library descriptor id,
  - symbol name,
  - canonical ABI signature (`Integer`, `Double`, `String`, `Pointer`, `Boolean`, `Void`),
  - per-arg marshaling policy,
  - return marshaling policy.

The compiled artifact must not embed ad-hoc pointer coercions for unsupported
annotations. Unsupported signatures remain compile-time errors.

## Runtime Loading/Symbol Resolution

At process startup (or first call when lazy), runtime does:

1. open each required library descriptor via deterministic loader path,
2. resolve each required symbol for declared callsites,
3. store function pointers in immutable runtime slots,
4. fail closed before first user call if required symbols are unresolved.

For optional symbols (future extension), callsites must branch through an
explicit “unavailable symbol” error path instead of null-call attempts.

## Linker/Deployment Contract

Compiled artifacts must declare FFI runtime expectations explicitly:

1. Which dynamic libraries must be present at runtime.
2. Which symbols are required.
3. Whether load is eager or lazy.
4. Whether unresolved symbols are fatal at startup or fatal at first call.

The build output includes a machine-readable manifest adjacent to the generated
artifact (`<output>.ffi-manifest.json`) so deployment tooling can preflight host
environments.

## Fail-Closed Rules

AOT declarative FFI must fail closed in all of:

- unresolved required library,
- unresolved required symbol,
- unsupported annotation/ABI shape at compile time,
- marshaling mismatch at runtime boundary,
- nullability/ownership policy violation for declared contract.

Fail closed means no silent fallback to permissive pointer coercion and no
attempted indirect call through unresolved symbol slots.

## Test Plan (Required Before Enabling)

1. Positive parity:
   - basic `libc` / `libm` declarations compile and execute in AOT.
2. Startup failure:
   - missing required library fails with deterministic error.
3. Symbol failure:
   - missing required symbol fails deterministically.
4. Annotation guard:
   - unsupported annotation rejected at compile time.
5. Marshaling guard:
   - runtime mismatch path fails closed.
6. Contract visibility:
   - generated AOT manifest matches declared `ffi` forms.

Only after these tests pass should compiler rejection tests be replaced.

## Implementation Staging

1. Land descriptor structs + manifest emission behind compiler flag.
2. Land runtime loader/symbol resolver + fail-closed wiring.
3. Land AOT callsite lowering for supported signatures.
4. Land parity/failure tests.
5. Flip compiler behavior from reject to enable for supported subset.
