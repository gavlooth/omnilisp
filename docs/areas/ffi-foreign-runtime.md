# FFI And Foreign Runtime Area

## Canonical Source Of Truth

- Current implementation truth: `memory/CHANGELOG.md`
- Normative language surface: `docs/LANGUAGE_SPEC.md`
- User reference: `docs/reference/09-concurrency-ffi.md`
- Tooling reference: `docs/PROJECT_TOOLING.md`
- Relevant closed plans:
  - `docs/plans/ffi-first-class-grouped-module-plan-2026-04-11.md`
  - `docs/plans/foreign-runtime-core-plan-2026-04-11.md`

## Current Status

Status: `yellow`
As of: 2026-04-26

Yellow is intentional for this area: the C ABI / `ForeignHandle` runtime path
is shipped and validated, while optional non-C runtime adapter families and
backend buffer hooks remain future tracks. This is not a live TODO queue by
itself; open concrete work in `TODO.md` before treating any residual track below
as active implementation work.

## Lane Closure Snapshot

`LANG-FOREIGN-RUNTIME-CORE-107` is closed as a common-core design lane.

Already-landed core behavior:

- `ForeignHandle` as the single user-facing foreign abstraction.
- Stable C ABI grouped syntax and descriptor flow (`[ffi module]` over `[ffi lib]` +
  `[ffi λ]`).
- Internal adapter boundary for `foreign-describe`, `foreign-release`, and C ABI
  bound-call dispatch.
- Shared metadata and capability model (`'runtime`, `'parameters`, `'returns`,
  `'capabilities`, `'ownership`, `'finalizer`).

Runtime-specific residuals explicitly split out of this closed common-core
lane:

- Non-C runtime adapters (Python, Julia, CUDA/cuBLAS, polyglot/plugin).
- Optional C++ tooling via C++ shim/API mode only.
- Tensor backend buffer hooks (`FOREIGN_CAP_TENSOR_BUFFER` path).
- Exact concrete follow-up closed by regression coverage:
  - scheduler-bound TLS lifecycle regression (`tls-close` while `tls-read` or
    `tls-write` is active)

The shipped C ABI path is still the stable runtime path. It now has a clearer
first-class surface:

- `[ffi module]` is grouped parser sugar over `[ffi lib]` plus `[ffi lambda]`.
- `^ForeignHandle` and metadata dictionaries describe opaque foreign resources;
  user code does not use raw pointer values.
- `foreign-describe` reflects C ABI handles and bound C ABI callables through
  dictionaries and arrays.
- `foreign-release` explicitly closes releasable foreign handles through the
  same finalizer/free path used by scope teardown.
- `^String` FFI returns copy non-null C plain `char*` returns into Omni
  `String` values, while null returns map to `nil`; user code does not receive
  raw address integers for string returns.
- Manual-return `ForeignHandle` metadata now remains visible in runtime reflection:
  returned manual handles are tracked as `'ownership manual` and remain non-releasable.
- AOT policy lowering now requires return `ForeignHandle` finalizers to be owned;
  borrowed/default finalizer-bearing return metadata is rejected to match
  runtime finalizer-resolution behavior.
- Bindgen keeps byte pointers such as `signed char*` and `unsigned char*` as
  `ForeignHandle` values instead of string-shaped parameters or returns.
- For plain character pointers, bindgen treats `const char*` and
  `char const*` as string-input shaped, while `char* const` stays a mutable
  string-buffer wrapper contract.
- Only single-level plain `char*` pointers are string-shaped; pointer-to-pointer
  spellings such as `char**` and `const char**` stay `ForeignHandle` values.
- Name-based mutable string-buffer direction inference treats `inout` as more
  specific than `out`, so names such as `inout_buffer` generate
  `buffer-direction=inout`.
- Opaque foreign-resource handle descriptors preserve metadata families such
  as `File` as symbols under `'name`; C ABI library handles keep their
  soname/path `'name` as a string because that field is the `dlopen` target.
- Borrowed foreign-resource handles remain non-releasable through
  `foreign-release`; explicit release stays owned-handle/finalizer driven.
- Malformed opaque handles that carry `FOREIGN_CAP_RELEASE` without release
  authority are rejected by `foreign-release` and are not reflected as owned.
- Internally mixed foreign handles with both a native finalizer and
  `free_lib_handle` normalize to finalizer-owned release authority at
  construction, so release cannot execute both paths for one payload.
- Finalizer-backed internal handle wrappers now free their own heap payloads
  after resource-specific teardown. This keeps UV timer callback, process,
  signal, socket, FS stream, TLS, and deduce foreign wrappers aligned with the
  finalizer-authoritative release contract.
- Failed `ForeignHandle` construction releases finalizer/free-backed
  non-library payloads through the constructor cleanup path; C ABI library
  handles remain non-releasable and stay under the existing `dlopen` caller
  cleanup path. Owned C FFI pointer returns rely on constructor cleanup if
  return-handle wrapping fails.
- Constructor failure call sites now rely on that single release authority
  instead of running redundant caller-side finalizer/free cleanup after
  `make_ffi_handle_ex` returns null.
- C ABI/AOT library registration failures release the constructed wrapper box
  after closing the raw library handle and clearing the root-scoped value
  pointer, avoiding a wrapper leak without making library handles generally
  releasable or leaving teardown with a freed box pointer.
- Malformed opaque descriptors carrying `FOREIGN_CAP_RELEASE` without
  finalizer/free release authority are still non-releasable and are not
  reflected as owned by `foreign-describe`.
- `foreign-describe` now omits `'release` from reflected capabilities unless a
  handle has actual release authority, so malformed opaque handles with a stray
  release bit do not leak that bit into the describe surface.
- C ABI library handles reflect load/resolve/reflect capability only; direct
  call capability stays on `ForeignCallable` descriptors for bound functions.
- Async process-spawn cleanup coverage now forces `stdin`, `stdout`, and
  `stderr` wrapper allocation failures after `uv_process` creation and proves
  opened pipes and process state are cleaned before each failed spawn returns.
- Released handles are no longer reflected as owned after payload cleanup clears
  the live pointer and release capability.
- Bindgen can emit grouped raw modules with
  `[dependencies.ffi.NAME] raw-syntax = "grouped"`.
- Bind TOML fail-closed parsing now marks the active dependency invalid when a
  malformed section-header line starts with `[`, so a broken section line cannot
  leave the previous dependency looking valid while following keys are ignored.
- Grouped raw bindgen output carries `;; Raw syntax: grouped` as the narrow
  `FFI-BIND-003` reviewability slice.
- Bindgen accepts `[dependencies.ffi.NAME] strip-prefixes = ["prefix_"]` for
  facade-only prefix stripping. Raw binding names are intentionally preserved
  so the current C ABI symbol lookup remains correct. Malformed, empty, or
  overlong `strip-prefixes` entries fail the dependency before header parsing.
- FFI dependency `library`, `raw-syntax`, `headers`, and `functions` values are
  strict on the bind path as well; missing required, malformed, empty,
  overlong, unquoted, or incorrectly shaped values fail before header parsing
  instead of being silently truncated, accepted loosely, or treated as no-op
  dependencies.
- Adjacent quoted scalar tails such as `library = "sqlite3" "m"` are treated as
  malformed and fail closed.
- Unsupported keys in `[dependencies.ffi.NAME]` fail closed before header
  parsing so typos cannot silently widen a bind or skip a generated-name policy.
- Unsafe `library` stems containing slash, backslash, quote, whitespace, or
  control characters fail before header parsing or generated output.
- Explicit `functions = [...]` filters require every listed C function to be
  found in parsed headers; missing entries fail before generated output.
- Explicit `exclude-functions = [...]` filters require every denied C function
  to be found in parsed headers, omit those functions from generated raw and
  facade output, skip denied functions before libclang type mapping, and record
  the denylist in the generated manifest.
- Multi-header dependency parse failures release functions parsed from earlier
  headers before failing, so partial parse state cannot survive the error path.
- Section headers accept inline `#` comments; malformed section-header lines
  starting with `[` follow the fail-closed invalidation behavior above before
  resetting parser context.
- Strict bind TOML string arrays accept a trailing comma before `]`, and strict
  quoted strings decode the TOML basic-string escapes `\b`, `\f`, `\uXXXX`, and
  `\UXXXXXXXX` while keeping malformed Unicode escapes fail-closed.
- Compile-side FFI contract JSON and bindgen generated manifest TOML now escape
  remaining C0 control characters instead of emitting invalid JSON/TOML text for
  unusual dependency, path, or prefix strings.
- Compile-side `.ffi-manifest.json` sidecars now use the same fail-closed
  posture: JSON strings escape all C0 control bytes, and manifest writes go
  through a sibling temp file plus final rename so a pre-rename failure
  preserves the previous final manifest.
- If a new raw module is written but facade generation fails, bindgen removes
  the new raw file before returning failure; existing raw files are left in
  place on rerun failures.
- If raw cleanup is attempted after a facade-generation failure, `bindgen_delete_output_file_if_exists`
  now returns whether deletion succeeded and the pair generator returns failure when cleanup
  cannot be performed.
- If raw/facade generation succeeds but manifest writing fails, first-time raw,
  facade, and manifest artifacts are cleaned before dependency failure; existing
  raw, facade, or manifest artifacts are left in place on rerun failures.
- Raw, facade, and manifest text writers use sibling temp paths and rename into
  place only after a full write and close succeeds; failed final renames clean
  their temp output instead of replacing the target with a partial file.
- Anonymous C parameter fallback names format the full numeric index (`arg123`,
  etc.) so high-arity generated bindings do not emit invalid fallback symbols.
- Unsafe return-type metadata now fails bindgen generation closed before any
  raw/facade output is written.
- Repeated FFI dependency keys also fail closed instead of using last-write-wins
  semantics for the bind target, function filter, syntax mode, or
  generated-name rewrite policy.
- Repeated `[dependencies.ffi.NAME]` sections fail closed so two dependencies
  cannot target the same generated raw, facade, or manifest output stem.
- Dependency section count overflow fails closed before header parsing or
  output generation, and overflow section keys cannot mutate the last accepted
  dependency.
- Strict FFI dependency value parsing strips inline `#` comments only outside
  quoted values, preserving quoted strings that contain `#`.
- Bindgen writes `lib/ffi/<name>_manifest.toml` beside the generated raw and
  facade files. The manifest records the effective shipped output config:
  dependency, library, raw syntax, raw/facade output paths, and
  `strip-prefixes` plus `exclude-functions`.

The internal foreign runtime adapter boundary has landed enough structure for
future optional lanes behind the stable C ABI / `ForeignHandle` contract:

- runtime kind,
- operation capability bits,
- handle/callable description,
- handle release,
- C ABI load/resolve/call slots,
- import/member hook slots for future runtime adapters.

Python, Julia, CUDA/cuBLAS, optional C++ tooling, polyglot/plugin machinery,
and tensor backend buffer hooks remain separate follow-on lanes and are not
user-facing runtime paths yet.

## Residual Tracks

These are optional follow-on families, not active TODO-backed work:

1. Keep C ABI grouped FFI stable while expanding runtime-lane follow-ons in
   separate tracks:
   - Python/JL adapter lane.
   - CUDA/cuBLAS lane.
   - Optional C++/CppInterOp toolchain lane (shim/API-only).
   - Polyglot/plugin support lane.
2. Keep tensor backend buffer hooks as their own optional lane.
3. Keep the TLS offload/in-flight lifecycle regression as a closed validation
   checkpoint; no live implementation follow-up remains for that lane.

## Validation

Recent targeted validation recorded in `memory/CHANGELOG.md`:

- `c3c build --warn-deprecation=no`
- targeted `advanced-ffi-system` group: `pass=75 fail=0`
- targeted `compiler` slice with libclang: `pass=276 fail=0`
- `c3c build --sanitize=address --warn-deprecation=no`
- ASAN targeted `advanced-ffi-system` group: `pass=74 fail=0`
- ASAN targeted `deduce` slice: `pass=330 fail=0`
- bounded container `memory-lifetime-smoke` slice: `pass=225 fail=0`
- final `c3c build --warn-deprecation=no`
- targeted `compiler` slice for compile-side FFI sidecar manifest hardening:
  `pass=276 fail=0`
- targeted `deduce` slice: `pass=330 fail=0`
- targeted `data-format` slice for the TOML option arity fix: `pass=64 fail=0`
- `scripts/check_status_consistency.sh`
- `git diff --check`
