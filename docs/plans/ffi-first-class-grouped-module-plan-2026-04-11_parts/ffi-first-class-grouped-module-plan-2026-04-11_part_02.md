# ffi-first-class-grouped-module-plan-2026-04-11 Part 02

Source: `docs/plans/ffi-first-class-grouped-module-plan-2026-04-11.md`

## Implementation Slices

### FFI-GROUP-001: Parser And Lowering

Implement `define [ffi module]` as sugar over the existing declarative FFI
forms.

Status: landed.

The parser now accepts the flat grouped body shape and lowers it to an internal
block containing the existing `[ffi lib]` and `[ffi λ]` forms. Missing return
annotations fail closed, `ForeignHandle` metadata dictionaries reuse the same
single-function validation path, and AOT preload/manifest scans recurse through
the lowered block.

Validation:

- `c3c build --warn-deprecation=no`
- Docker-bounded `advanced` slice filtered to `advanced-ffi-system`:
  `pass=60 fail=0`
- Docker-bounded `compiler` slice: `pass=206 fail=0`
- `c3c build --sanitize=address --warn-deprecation=no`
- Docker-bounded `memory-lifetime-smoke`: `pass=203 fail=0`

Acceptance criteria:

- grouped form creates the library binding and each function binding,
- malformed body sequences fail at parse or definition time,
- return and parameter annotations reuse current validation,
- `ForeignHandle` metadata dictionaries behave exactly like single
  `[ffi lambda]` definitions,
- interpreter/JIT behavior matches equivalent lowered definitions,
- AOT policy descriptors match equivalent lowered definitions.

### FFI-GROUP-002: Bindgen Output Option

Status: landed.

Bindgen now emits grouped module syntax behind the per-dependency
`omni.toml` option `raw-syntax = "grouped"`.

The default stays `raw-syntax = "legacy"` to preserve the existing generated
raw module shape:

```toml
[dependencies.ffi.math]
library = "m"
headers = ["/usr/include/math.h"]
functions = ["sin", "cos", "sqrt"]
raw-syntax = "grouped"
```

Unsupported `raw-syntax` values fail closed before header parsing.

Validation:

- Docker-bounded `compiler` slice: `pass=207 fail=0` (container libclang
  unavailable, grouped raw-syntax option check still ran)
- Host targeted `compiler` slice with libclang: `pass=212 fail=0`
- `c3c build --sanitize=address --warn-deprecation=no`
- final `c3c build --warn-deprecation=no`
- `git diff --check`

Acceptance criteria:

- generated raw modules can use grouped syntax: done,
- existing raw-plus-facade behavior remains available: done,
- generator output remains deterministic: done,
- unsupported signatures still fail closed or emit explicit review notes: done.

### FFI-BIND-003: Bindgen Ergonomics

Improve generated names and reviewability.

Status: first three narrow slices landed.

Scope:

- real parameter names from libclang,
- kebab-case facade names,
- prefix stripping: landed for facade/exported Omni names,
- generated bind manifest: landed as an effective-config
  `<name>_manifest.toml`,
- generated comments only where manual review is required.
- grouped raw output should include a minimal syntax note so review diffs stay
  readable without changing binding behavior: done.
- `strip-prefixes` keeps raw binding names unchanged and strips only the
  facade/exported Omni names plus `raw-*` aliases. Raw names still carry the
  C-derived symbol text used by the current FFI parser's `dlsym` path, so this
  slice does not need or add a separate `c-name` syntax.
- Generated Omni-facing names are preflighted before output files are written;
  prefix stripping fails closed when it would produce a number-leading emitted
  token such as `3d-distance`, or when stripping would make two C functions emit
  the same Omni-facing name.
- `strip-prefixes` TOML parsing is strict for this generated-name rewrite path:
  malformed, empty, or overlong prefix entries fail the dependency before header
  parsing instead of being silently truncated.
- `library`, `raw-syntax`, `headers`, and `functions` TOML parsing is also
  strict on the bind path; missing required, malformed, empty, overlong,
  unquoted, or incorrectly shaped values fail before header parsing instead of
  being silently truncated, accepted loosely into different bind targets, or
  treated as no-op dependencies.
- Adjacent quoted scalar tails, such as `library = "sqlite3" "m"`, are treated
  as malformed and fail closed.
- Unsupported keys in `[dependencies.ffi.NAME]` fail closed before header
  parsing so typos cannot silently widen a bind or skip a generated-name policy.
- Unsafe `library` stems containing slash, backslash, quote, whitespace, or
  control characters fail before header parsing or generated output.
- Explicit `functions = [...]` filters require every listed C function to be
  found in parsed headers; missing entries fail before generated output.
- Repeated FFI dependency keys fail closed instead of using last-write-wins
  semantics for the bind target, function filter, raw syntax mode, or
  generated-name rewrite policy.
- Repeated `[dependencies.ffi.NAME]` sections fail closed so two dependencies
  cannot target the same generated raw, facade, or manifest output stem.
- Dependency section count overflow fails closed before header parsing or
  output generation, and overflow section keys cannot mutate the last accepted
  dependency.
- Inline `#` comments are stripped only outside quoted bind dependency values,
  preserving quoted strings that contain `#`.
- Malformed dependency section headers mark the currently active dependency
  invalid before resetting parser context, so a broken section line cannot leave
  the previous dependency looking valid while following keys are ignored outside
  any dependency.
- Strict bind TOML string arrays accept a trailing comma before `]`, and strict
  quoted strings decode the TOML basic-string escapes `\b`, `\f`, `\uXXXX`, and
  `\UXXXXXXXX` while malformed Unicode escapes still fail closed.

Historical slice validation for the landed syntax-note slice:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=212 fail=0`
- `git diff --check`

Historical slice validation for the landed `strip-prefixes` slice:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=214 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=61 fail=0`

Historical slice validation for the generated bind manifest slice:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=215 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=61 fail=0`

Historical slice validation for path-safe bind output dependency names:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=218 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- `git diff --check`

Historical slice validation for overlong bind dependency section names:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=219 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- `git diff --check`

Historical slice validation for prefix-stripped emitted name preflight:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=220 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- `git diff --check`

Historical slice validation for prefix-stripped emitted name collision preflight:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=221 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- `git diff --check`

Historical slice validation for strict `strip-prefixes` TOML parsing:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=224 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- `git diff --check`

Historical slice validation for strict bind dependency core TOML fields:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=231 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for duplicate bind dependency TOML key rejection:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=236 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for duplicate FFI dependency section rejection:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=237 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for inline comment handling in strict bind TOML values:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=238 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for dependency count overflow fail-closed:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=239 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- `git diff --check`

Historical slice validation for malformed adjacent quoted scalar rejection:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=240 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for unknown FFI dependency key rejection:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=241 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for unsafe bind library stem rejection:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=242 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for explicit function filter match enforcement:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=243 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for partial header parse cleanup:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=244 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for section-header comment/context hardening:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=246 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for malformed dependency section fail-closed cleanup:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=260 fail=0`
- `scripts/check_status_consistency.sh`
- `c3c build --sanitize=address --warn-deprecation=no`
- ASAN targeted `advanced-ffi-system` group: `pass=74 fail=0`

Historical slice validation for raw/facade pair failure cleanup:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=247 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for stale raw cleanup fail-closed:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=256 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=67 fail=0`
- `git diff --check`

Historical slice validation for metadata control-character hardening:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=257 fail=0`
- `git diff --check`

Historical slice validation for manifest failure cleanup:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=248 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for atomic bindgen text writes:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=249 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for anonymous parameter fallback naming:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=250 fail=0`

Historical slice validation for the callback scaffold fail-closed cleanup:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=215 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

Historical slice validation for the callback unregister nil-cleanup idempotence cleanup:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=215 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- `git diff --check`

Historical slice validation for the string-buffer manual-review fail-closed cleanup:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=215 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- `git diff --check`

Historical slice validation for the string-buffer none-policy guard cleanup:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=216 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- `git diff --check`

Historical slice validation for the string-buffer `inout` direction precedence cleanup:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=217 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- `git diff --check`

Historical slice validation for the byte-pointer bindgen classification cleanup:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=217 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- `git diff --check`

Historical slice validation for the const-pointee char-pointer classification cleanup:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=217 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- `git diff --check`

Historical slice validation for the char-pointer depth classification cleanup:

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=217 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
- `git diff --check`
