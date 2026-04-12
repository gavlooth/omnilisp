# Common Foreign Runtime Core Plan (2026-04-11)

Status: `closed`; `LANG-FOREIGN-RUNTIME-CORE-107` closed as a common-core
design lane. `FOREIGN-CORE-001` and `FOREIGN-CORE-002A` through
`FOREIGN-CORE-002R` landed.
Owner: language owner + Codex workflow
Mode: cross-runtime abstraction plan
TODO lane: `LANG-FOREIGN-RUNTIME-CORE-107` (closed; residuals split into
runtime-lane follow-ons)

## Purpose

Define a minimal foreign runtime core that keeps the C ABI / `ForeignHandle`
contract stable while leaving Python, Julia, CUDA/cuBLAS, optional C++
tooling, polyglot/plugin machinery, and tensor buffer hooks as separate
optional lanes instead of one overloaded runtime model.

The first goal is a small, explicit, safe contract:

- Foreign resources are represented by `ForeignHandle` values, not raw pointers.
- Runtime and capability metadata remains dictionary-driven and declarative.
- Invocation, reflection, marshalling, and release are standardized, while
  runtime-specific behavior stays in adapters.

## Current context and constraints

- Core language invariants remain unchanged: no raw pointer types in user code,
  no stop-the-world GC, region-based ownership for Omni values.
- Metadata uses symbols and dictionaries only. Keys use quoted symbols
  (for example `'parameters`, `'runtime`, `'ownership`).
- Arrays are canonical in metadata payload examples such as
  `'parameters [x y]` and `'args [..]`; list forms may be accepted by a
  parser later, but arrays are the documented primary shape.
- Grouped/`[ffi module]` syntax now lowers to the shared C ABI declarative FFI
  path and should remain a sugar layer over the common callable descriptor
  contract.
- Bindgen can now emit grouped raw modules with
  `[dependencies.ffi.NAME] raw-syntax = "grouped"`, while the default legacy
  output remains available.
- `foreign-describe`, `foreign-release`, and C ABI bound-function calls now
  route through a small internal `ForeignRuntimeAdapter` dispatch point. The
  current adapter preserves the shipped C ABI/generic-handle behavior and gives
  Python, Julia, CUDA/cuBLAS, optional C++ tooling, polyglot/plugin machinery,
  and tensor backend buffer hooks separate places to plug in later without
  exposing raw pointers to Omni code.

## `LANG-FOREIGN-RUNTIME-CORE-107` lane closure

This lane is now narrowed and closed as the **shared, common runtime-core
contract** layer.

What is already landed:

- `ForeignHandle` descriptor-first abstraction and canonical metadata keys for
  `'runtime`, `'parameters`, `'ownership`, `'returns`, and `'capabilities`.
- C ABI grouped-module lowering and canonical surface shape (`[ffi module]` over
  `[ffi lib]` + `[ffi λ]`).
- Explicit adapter boundary for `foreign-describe`, `foreign-release`, and C ABI
  bound calls.
- Constructor/capability/failure-hardening for release authority and cleanup
  ordering (`FOREIGN-CORE-002A` through `FOREIGN-CORE-002R`, including 002D-002F
  and 002G-002R follow-ons).
- Reflection metadata policy as dictionaries/arrays, with no user-facing raw
  pointer exposure.

Residuals split out from this lane:

- C ABI-adjacent follow-on lane only: `FOREIGN-CORE-004` runtime tracks.
  - Python/JL runtime adapter wiring (plugin/bridge behavior, `import_module`/
    `resolve_member` runtime implementation).
  - CUDA/cuBLAS adapter + tensor-buffer marshalling path.
  - Optional C++ tooling/CppInterOp integration remains shim-generation-only and
    is not part of common-core runtime paths.
  - Polyglot/plugin runtime support remains separate from `ForeignRuntimeAdapter`
    core contract.
  - Tensor backend hooks for non-C call paths remain behind optional lanes.
- Generalized runtime validation follow-ups that are still concrete and exact:
  - scheduler-bound TLS lifecycle regression for `tls-close` races against
    concurrent `tls-read`/`tls-write`.

## Design sketch (minimal core)

### Core handles and descriptors

- `ForeignHandle` is the universal user-facing abstraction for anything foreign.
- Every handle carries declarative metadata, at minimum:
  - `'name`
  - `'runtime` (for example `'c-abi`, `'python`, `'julia`, `'cuda`)
  - `'ownership` (`borrowed`, `owned`, `manual`)
  - `'finalizer` (required when ownership is `owned`)
  - `'nullability` (`non-null` or `nullable`)
- Handle families remain distinct via metadata values, not via raw pointer family
  or ad-hoc special types.

### Reflection metadata

- Reflection is data: dictionaries and arrays returned by runtime tooling.
- Canonical payload keys include (minimum):
  - `'kind` (`'function`, `'module`, `'object`, `'type`)
  - `'name`
  - `'runtime`
  - `'parameters` (array of parameter descriptors)
  - `'returns`
  - `'capabilities`
- Reflection should be produced from tooling (bindgen/headers/API mode) and used
  for generation, verification, or optional runtime display.

### Marshalling and call contract

- Common conversions are defined for:
  - scalar primitives,
  - `Array`, `List`, `Dictionary`,
  - `Tensor`,
  - `ForeignHandle`.
- Invocation takes one canonical foreign callable/handle path and delegates
  to adapter logic by runtime/capability metadata.
- Return/exception behavior is standardized into Omni effect-like payloads
  (for example `'kind 'ffi-error`) at the integration boundary.
- `release` is explicit and owned-handle driven; no silent drop semantics.

## Phased rollout

### `FOREIGN-CORE-001` Core contract and naming freeze

Status: landed first implementation slice.

Lock the public dictionary surface and metadata vocabulary for handles,
reflected objects, runtime descriptors, and call/release fields.

- Confirm canonical keys (`'runtime`, `'parameters`, `'returns`, `'capabilities`,
  `'ownership`, `'finalizer`, `'nullability`).
- Document that arrays are the canonical collection shape for metadata sequence
  fields.
- `FfiHandle` now carries runtime kind, handle kind, and capability descriptors.
- C ABI library handles and pointer-return handles are tagged through the common
  descriptor path.
- `foreign-describe` returns a dictionary using quoted-symbol metadata keys;
  `'capabilities` is an array of capability symbols.
- `FOREIGN-CORE-002A` also landed the explicit release helper
  `(foreign-release handle)`, sharing the same payload finalizer/free path as
  scope teardown and clearing the live payload pointer so shared wrappers cannot
  double-close.
- `FOREIGN-CORE-002B` also landed C ABI callable reflection through
  `(foreign-describe ffi-bound-function)`. The result uses the same
  quoted-symbol dictionary surface, with `'parameters` as an array of descriptor
  dictionaries and `'returns` as a descriptor dictionary.
- `FFI-GROUP-001` also landed grouped C ABI parser/lowering. `[ffi module]`
  expands to the existing `[ffi lib]` plus `[ffi λ]` path, so runtime callable
  reflection, `ForeignHandle` policy descriptors, AOT preload, and contract
  manifests continue to use the shared descriptor contract.

### `FOREIGN-CORE-002` Runtime adapter interfaces

Status: started with `FOREIGN-CORE-002D`, extended by `FOREIGN-CORE-002E`
internal adapter dispatch, continued by `FOREIGN-CORE-002F` import/member
adapter hooks, tightened by `FOREIGN-CORE-002I` borrowed-handle release
regression alignment, hardened through `FOREIGN-CORE-002O` constructor
failure caller alignment, tightened through `FOREIGN-CORE-002Q` capability
reflection authority alignment, and closed with `FOREIGN-CORE-002R`
process-spawn cleanup failure-injection coverage.

Define adapter responsibilities and split-by-runtime behavior.

- Common interface methods:
  - descriptor/handle creation,
  - load/import,
  - resolve member,
  - call,
  - release, and
  - describe.
- Keep each adapter constrained to policy boundaries defined by metadata.
- C ABI adapter remains libffi-centric and portable.
- Python/Julia adapters are explicit plugin/bridge lanes.
- `FOREIGN-CORE-002D` added the first internal adapter shape around:
  - runtime kind,
  - adapter operation capability bits,
  - handle description,
  - callable description,
  - handle release,
  - bound-function call dispatch.
- The current implementation routes `foreign-describe` and `foreign-release`
  through that adapter boundary. C ABI callable reflection and C ABI
  bound-function calls are registered on the C ABI adapter; generic handle
  describe/release remains available for all current runtime kinds until
  runtime-specific adapters replace it.
- `FOREIGN-CORE-002E` split C ABI lazy loading and symbol resolution into
  explicit adapter slots:
  - `load_bound` owns the current `dlopen` path,
  - `resolve_bound` owns the current `dlsym` path,
  - `call_bound` now reaches libffi only after adapter load/resolve succeeds,
  - `tensor_buffer` is reserved behind `FOREIGN_CAP_TENSOR_BUFFER` for a later
    BLAS/cuBLAS backend and is not registered for C ABI yet.
- `FOREIGN-CORE-002F` split runtime/module import and member lookup into
  capability-gated adapter slots:
  - `FOREIGN_CAP_IMPORT` and `FOREIGN_CAP_MEMBER` guard the new adapter
    behavior,
  - `import_module` owns runtime/module loading for non-C adapters,
  - `resolve_member` owns member lookup for those imported runtime objects,
  - internal dispatch helpers `foreign_runtime_import_module` and
    `foreign_runtime_resolve_member` route the current adapter calls,
  - no new public user primitive was added, and Python/Julia/CUDA behavior is
    still not wired yet.
- `FOREIGN-CORE-002G` cleaned up the C ABI `^String` return path:
  - non-null C `char*` returns are copied into Omni `String` values,
  - null returns continue to map to `nil`,
  - raw foreign addresses are no longer exposed as integer values for string
    returns.
- `FOREIGN-CORE-002H` cleaned up opaque handle name reflection:
  - opaque foreign-resource handles preserve metadata families such as `File`
    as symbols under `'name`,
  - C ABI library handles keep their soname/path `'name` as a string because
    that field is the `dlopen` target.
- `FOREIGN-CORE-002I` aligned the borrowed-handle release regression with the
  real C ABI path:
  - the test binds the real `fopen` symbol with borrowed `ForeignHandle`
    metadata,
  - it closes the payload through `fclose`,
  - and it verifies `foreign-release` rejects the non-releasable borrowed
    wrapper.
- `FOREIGN-CORE-002J` normalized mixed release authority:
  - internal handles constructed with both a native finalizer and
    `free_lib_handle` now make the finalizer authoritative,
  - the plain free fallback is disabled at construction,
  - and explicit release cannot execute both paths for one payload.
- `FOREIGN-CORE-002K` preserved explicit manual return ownership:
  - return-handle construction now uses explicit return metadata ownership
    (including `manual`) instead of only finalizer presence.
  - `(foreign-describe)` reports manual return handles as `'ownership manual`.
  - manual-return handles remain non-releasable (no release capability),
    preserving user-intended lifecycle boundaries.
  - AOT declaration policy rejects manual ownership combined with a finalizer,
    matching the interpreter metadata parser.
- `FOREIGN-CORE-002L` close AOT return finalizer ownership parity:
  - return policies containing a `ForeignHandle` finalizer now require
    `'ownership owned` in AOT policy lowering.
  - This prevents borrowed/default finalizer-bearing return metadata from being
    accepted by lowering and then ignored by finalizer resolution.
- `FOREIGN-CORE-002M` harden finalizer-owned foreign payload release:
  - finalizer-backed internal handle wrappers now free their own heap payloads
    after resource-specific teardown because the native finalizer path is the
    authoritative release path when present,
  - covered wrappers include UV timer callback, process, signal, TCP, UDP,
    FS stream, TLS, deduce relation, deduce database, and deduce transaction
    handles,
  - deduce relation scan forced column-key-value OOM handling now raises before
    allocating the relation cache, closing the sanitizer-only leak on that test
    path.
- `FOREIGN-CORE-002N` harden construction-failure ownership:
  - `make_ffi_handle_ex_with_descriptor` now releases finalizer/free-backed
    non-library payloads on both box-allocation and value-allocation failures,
  - C ABI library handles remain non-releasable and stay under the existing
    `dlopen` caller cleanup path on constructor failure,
  - owned C FFI pointer returns rely on constructor cleanup when return-handle
    wrapping fails instead of running a second caller-side finalizer,
  - TLS client/server wrapper OOM paths use the graceful TLS handle finalizer
    for connected initialized handles before freeing nested TLS storage,
  - allocation-failure tests pin exactly-once cleanup for finalizer-backed FFI
    payloads across box-allocation and value-allocation failures.
- `FOREIGN-CORE-002O` align constructor-failure callers:
  - redundant caller-side cleanup after `make_ffi_handle_ex` null returns was
    removed now that the constructor owns finalizer/free-backed non-library
    payload cleanup on failure,
  - async, scheduler, atomic, TLS, deduce, and boundary helper paths now rely
    on that single release authority,
  - malformed C ABI library descriptors with accidental release capability are
    covered directly and still reject `foreign-release`; opaque malformed
    handles with the same release bit are not reflected as owned,
  - released handles no longer reflect as owned after payload cleanup clears
    the live pointer and release capability,
  - C ABI/AOT library registration failures close the raw library handle, clear
    the root-scoped value pointer, and release the constructed wrapper box so
    later teardown cannot see a freed box.
- `FOREIGN-CORE-002P` align capability reflection authority:
  - `foreign-describe` now omits `'release` from reflected capabilities unless
    a handle has actual release authority,
  - malformed opaque handles with a stray release bit remain rejected by
    `foreign-release` and no longer leak that bit into the describe surface.
- `FOREIGN-CORE-002Q` tighten C ABI library capability reflection:
  - C ABI library handles now reflect `[load resolve reflect]`, not `'call`,
    because the library handle itself is not directly callable,
  - C ABI bound functions keep reflecting `'call` through their
    `ForeignCallable` descriptors.
- `FOREIGN-CORE-002R` add process-spawn cleanup failure-injection coverage:
  - async process-spawn tests force `stdin`, `stdout`, and `stderr` wrapper
    allocation failures after `uv_process` creation,
  - each failure path verifies opened pipes and process state are cleaned
    before the failed spawn returns.
- `FOREIGN-CORE-002` common-core work is closed; non-C behavior is now handled
  through runtime-track follow-ons under `FOREIGN-CORE-004`.
- Validation follow-ups from the general audit:
  - add a scheduler-bound TLS lifecycle regression for `tls-close` attempted
    while `tls-read` or `tls-write` is in flight,

### `FOREIGN-CORE-003` Reflection-first tooling flow

Route bindgen/headers/API workflows through the reflection data shape.

- Reflection metadata is emitted as dictionaries/arrays and consumed by:
  - grouped-module generation,
  - facade generation,
  - lintable policy checks (ownership, nullability, parameter arity).
- No core-language parser changes in this slice beyond aligning current
  declarations with the shared descriptor contract.

### `FOREIGN-CORE-004` Optional acceleration/runtime lanes

- Add runtime adapters for non-C lanes with explicit feature gates.

- CUDA/cuBLAS: adapter and call path through `Tensor` marshalling first.
- Optional C++ backend remains tooling/API mode for shim generation and layout
  validation, not part of the startup/runtime-required path.
- Polyglot runtime lane remains separate (plugin-style), with a clear host/guest
  direction boundary and explicit stubs.

## Non-goals

- Not replacing existing C FFI syntax with a language-level C++/Python/Julia
  execution model.
- Not exposing raw integer or raw foreign-resource values to user code.
- Not promising automatic zero-copy across every backend in the first slice.
- Not adding implicit closure conversion to foreign function pointers.
- Not introducing per-language type-specific ownership systems for Omni values.

## Current validation (canonical)

- `c3c build --warn-deprecation=no`
- Host targeted `advanced-ffi-system` group: `pass=75 fail=0`
- Host targeted `compiler` slice with libclang: `pass=276 fail=0`
- `c3c build --sanitize=address --warn-deprecation=no`
- ASAN targeted `advanced-ffi-system` group: `pass=74 fail=0`
- ASAN targeted `deduce` slice: `pass=330 fail=0`
- Bounded container `memory-lifetime-smoke` slice: `pass=225 fail=0`
- Final `c3c build --warn-deprecation=no`
- Host targeted `deduce` slice: `pass=330 fail=0`
- `scripts/check_status_consistency.sh`
- `git diff --check`

## Historical acceptance signals (design-level)

- `ForeignHandle`-centric call/release path is used consistently by the grouped
  FFI and existing declarative FFI.
- Metadata schemas are shared for core and adapter-facing reflection.
- Adapters are separable and independently testable by contract.
- Existing C FFI path remains valid and unchanged in shape for ordinary users.
- Historical validation for `FOREIGN-CORE-001`:
  - `c3c build --warn-deprecation=no`
  - Docker-bounded `advanced` slice filtered to `advanced-ffi-system`:
    `pass=51 fail=0`
  - Docker-bounded `compiler` slice: `pass=205 fail=0`
  - `c3c build --sanitize=address --warn-deprecation=no`
  - Docker-bounded `memory-lifetime-smoke`: `pass=203 fail=0`
  - `git diff --check`
- Historical validation for `FOREIGN-CORE-002A`:
  - `c3c build --warn-deprecation=no`
  - Docker-bounded `advanced` slice filtered to `advanced-ffi-system`:
    `pass=53 fail=0`
  - `c3c build --sanitize=address --warn-deprecation=no`
  - Docker-bounded `memory-lifetime-smoke`: `pass=203 fail=0`
  - `git diff --check`
- Historical validation for `FOREIGN-CORE-002D`:
  - `c3c build --warn-deprecation=no`
  - Host targeted `advanced-ffi-system` group: `pass=60 fail=0`
  - Docker-bounded targeted `advanced-ffi-system` group: `pass=60 fail=0`
  - `c3c build --sanitize=address --warn-deprecation=no`
  - final `c3c build --warn-deprecation=no`
- Historical validation for `FOREIGN-CORE-002E`:
  - `c3c build --warn-deprecation=no`
  - Host targeted `advanced-ffi-system` group: `pass=61 fail=0`
  - Docker-bounded targeted `advanced-ffi-system` group: `pass=61 fail=0`
  - `c3c build --sanitize=address --warn-deprecation=no`
  - final `c3c build --warn-deprecation=no`
  - `git diff --check`
- Historical validation for `FOREIGN-CORE-002F`:
  - `c3c build --warn-deprecation=no`
  - Host targeted `advanced-ffi-system` group: `pass=61 fail=0`
  - Docker-bounded targeted `advanced-ffi-system` group: `pass=61 fail=0`
  - `c3c build --sanitize=address --warn-deprecation=no`
  - final `c3c build --warn-deprecation=no`
- Historical validation for `FOREIGN-CORE-002G`:
  - `c3c build --warn-deprecation=no`
  - Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
  - `c3c build --sanitize=address --warn-deprecation=no`
  - ASAN targeted `advanced-ffi-system` group: `pass=63 fail=0`
  - final `c3c build --warn-deprecation=no`
- Historical validation for `FOREIGN-CORE-002H`:
  - `c3c build --warn-deprecation=no`
  - Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- Historical validation for `FOREIGN-CORE-002I`:
  - `c3c build --warn-deprecation=no`
  - Host targeted `advanced-ffi-system` group: `pass=66 fail=0`
- Historical validation for `FOREIGN-CORE-002J`:
  - `c3c build --warn-deprecation=no`
  - Host targeted `advanced-ffi-system` group: `pass=67 fail=0`
- Historical validation for `FOREIGN-CORE-002K`:
  - `c3c build --warn-deprecation=no`
  - Host targeted `advanced-ffi-system` group: `pass=69 fail=0`
  - `c3c build --sanitize=address --warn-deprecation=no`
  - ASAN targeted `advanced-ffi-system` group: `pass=69 fail=0`
- Historical validation for `FOREIGN-CORE-002L`:
  - `c3c build --warn-deprecation=no`
  - Host targeted `compiler` slice with libclang: `pass=258 fail=0`
  - `git diff --check`

## Related work

- `docs/plans/ffi-first-class-grouped-module-plan-2026-04-11.md` is the
  grouped syntax direction and should continue to lower into the core described
  here.
- `TODO.md` lanes: `LANG-FFI-FIRST-CLASS-GROUPED-MODULE-106` and
  `LANG-FOREIGN-RUNTIME-CORE-107` are coordinated but not redundant; this one
  is the adapter/core-contract layer for multiple runtimes.
