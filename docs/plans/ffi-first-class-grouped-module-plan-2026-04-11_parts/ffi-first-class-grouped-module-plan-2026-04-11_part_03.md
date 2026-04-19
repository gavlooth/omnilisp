# ffi-first-class-grouped-module-plan-2026-04-11 Part 03

Source: `docs/plans/ffi-first-class-grouped-module-plan-2026-04-11.md`

### FFI-TOML-004: `omni.toml` Dependency Contract

Implement the `omni.toml` fields needed by grouped/raw/facade generation.

Status: landed through `FFI-TOML-004AB`.

Landed work (through `FFI-TOML-004AB`):

- `FFI-TOML-004A`: `strip-prefixes = ["prefix_"]` keeps facade names readable while
  preserving raw binding names.
- `FFI-TOML-004B`: `--bind` writes generated manifest output alongside raw/facade
  artifacts and the manifest records `strip-prefixes`.
- `FFI-TOML-004C`: `library`-derived dependency output stems are path-safe.
- `FFI-TOML-004D` through `FFI-TOML-004G`: strict parsing and collision handling for
  overlong names, prefix trimming, preflight checks, and duplicate prefix outputs.
- `FFI-TOML-004H` through `FFI-TOML-004R`: strict core parse fields, duplicate
  dependency handling, fail-closed status updates, and inline comment/context hardening.
- `FFI-TOML-004W`: malformed dependency section headers invalidate active dependency
  state before parser-context reset.
- `FFI-TOML-004Z`: raw control-byte rejection for quoted TOML metadata.
- `FFI-TOML-004AA`: escaped NUL rejection for quoted TOML metadata.
- `FFI-TOML-004AB`: `exclude-functions = [...]` denylist filtering for bind
  generation and generated manifests.

Implemented behavior details:

- `raw-syntax = "legacy" | "grouped"` selects the raw module syntax.
- `strip-prefixes = ["prefix_"]` removes matching C prefixes from generated
  facade names and raw import aliases while preserving raw binding names.
- Prefix-stripped names are validated before output files are written, and
  number-leading emitted names fail closed.
- Duplicate generated Omni-facing names after prefix stripping fail closed
  before output files are written.
- Malformed, empty, or overlong `strip-prefixes` entries fail the dependency
  before header parsing instead of being silently truncated.
- Missing required, malformed, empty, overlong, unquoted, or incorrectly shaped
  `library`, `raw-syntax`, `headers`, and `functions` values fail the
  dependency before header parsing instead of being silently truncated, accepted
  loosely, or treated as no-op dependencies.
- Adjacent quoted scalar tails, such as `library = "sqlite3" "m"`, are treated
  as malformed and fail closed.
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
  starting with `[` now mark the active dependency invalid before resetting
  parser context.
- If a new raw module is written but facade generation fails, bindgen removes
  the new raw file before returning failure; existing raw files are left in
  place on rerun failures.
- If raw/facade generation succeeds but manifest writing fails, first-time raw,
  facade, and manifest artifacts are cleaned before dependency failure; existing
  raw, facade, or manifest artifacts are left in place on rerun failures.
- Raw, facade, and manifest text writers use sibling temp paths and rename into
  place only after a full write and close succeeds; failed final renames clean
  their temp output instead of replacing the target with a partial file.
- Anonymous C parameter fallback names format the full numeric index (`arg123`,
  etc.) so high-arity generated bindings do not emit invalid fallback symbols.
- Repeated FFI dependency keys fail closed instead of using last-write-wins
  semantics.
- Repeated `[dependencies.ffi.NAME]` sections fail closed instead of letting
  multiple dependencies target the same generated output stem.
- Dependency section count overflow fails closed before header parsing or
  output generation, and overflow section keys cannot mutate the last accepted
  dependency.
- Inline `#` comments are stripped only outside quoted bind dependency values.
- Generated `lib/ffi/<name>_manifest.toml` records the effective shipped output
  config: dependency name, library, raw syntax, raw/facade output paths, and
  `strip-prefixes` plus `exclude-functions`.
- Compile-side FFI contract JSON and bindgen generated manifest TOML escape
  C0 control characters, so dependency, path, and prefix strings cannot emit
  invalid generated metadata text.
- Compile-side `.ffi-manifest.json` sidecars also escape all C0 control bytes
  and write via sibling temp file plus final rename, preserving an existing
  final manifest if a failure occurs before rename.
- `--bind` output path construction rejects empty FFI dependency names and
  names containing anything outside ASCII letters, digits, `_`, and `-` before
  writing raw, facade, or manifest files. This keeps dependency names safe as
  both file stems and generated Omni module names.
- Overlong `[dependencies.ffi.NAME]` section names now fail closed instead of
  being silently truncated before output path generation.

Acceptance criteria:

- unknown or unsupported field values fail closed,
- defaults preserve current behavior,
- generated manifest records the effective config: done for the currently
  shipped output fields,
- tests cover both accepted and rejected config shapes.

Future TOML work (post-`FFI-TOML-004AB`):

- The following optional fields remain deferred and are expected to stay rejected as
  unsupported until their owning slice lands:
  - `clang-args`
  - `mode`
  - `facade`
  - `name-style`
  - `ownership-policy`
  - `output-raw`
  - `output-facade`
  - `generator`

### FFI-OPAQUE-005: Opaque Handle Families

Add first-class opaque family declarations only after grouped functions are
stable.

The surface must stay consistent with `ForeignHandle` metadata and must not
reintroduce raw integer pointer values.

### FFI-STRUCT-006: Struct Layout Lane

Design and implement C struct layout support as a separate lane. Do not merge
this with grouped function syntax.

### FFI-CALLBACK-007: Callback Handle Lane

Keep generic callback support separate and explicit. Do not treat bindgen
callback placeholders as executable callback support until register/invoke/
unregister ownership and teardown tests exist.

Status: callback wrapper scaffolds now validate callback metadata and then fail
closed until a facade author wires a concrete subsystem callback-handle shim.
Generic bindgen output must not route arbitrary C callback parameters through
the `uv` timer callback prototype. Generated callback unregister helpers keep a
nil handle as an idempotent no-op, while non-nil generic handles still fail
closed until the concrete subsystem shim is supplied.

Generated mutable string-buffer helpers now fail closed for `manual-review`
teardown until a facade author edits in an explicit allocation and writeback
policy. `none` teardown buffer helpers remain pass-through after role,
ownership, direction, and size validation.

### FFI-EFFECT-008: FFI Error Effects

Route FFI failures through a canonical effect payload after grouped syntax and
bindgen ergonomics are stable.

## Non-Goals

- No `->` return syntax.
- No bracketed entries in the grouped module body for the first slice.
- No raw integer pointer ABI surface.
- No C++ runtime dependency in Omni startup.
- No MetaFFI/CppInterOp mandatory dependency for normal FFI.
- No generic closure-to-native-function-pointer coercion.
- No struct/callback safety claims before ownership and teardown tests exist.

## Historical Validation

For implementation slices:

- run `c3c build --warn-deprecation=no`,
- run focused parser/compiler tests for grouped FFI forms,
- run the bounded `advanced-ffi-system` subgroup for runtime behavior,
- run compiler/AOT policy descriptor tests for lowered grouped bindings,
- run `git diff --check`.
