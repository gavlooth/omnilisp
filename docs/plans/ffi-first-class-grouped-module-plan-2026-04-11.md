# First-Class FFI Grouped Module Plan (2026-04-11)

Status: `closed` for TODO lane `LANG-FFI-FIRST-CLASS-GROUPED-MODULE-106`;
grouped syntax, grouped bindgen, and bind TOML hardening through
`FFI-TOML-004AB` have landed
Owner: language owner + Codex workflow
Mode: syntax decision and implementation plan
TODO lane: `LANG-FFI-FIRST-CLASS-GROUPED-MODULE-106`

## Current validation (canonical)

- `c3c build --warn-deprecation=no`
- Host targeted `compiler` slice with libclang: `pass=276 fail=0`
- Host targeted `advanced-ffi-system` group: `pass=75 fail=0`
- Host targeted `advanced-collections-module` group: `pass=212 fail=0`
- Bounded container `memory-lifetime-smoke` slice: `pass=225 fail=0`
- `c3c build --sanitize=address --warn-deprecation=no`
- ASAN targeted `advanced-ffi-system` group: `pass=74 fail=0`
- `git diff --check`

## Purpose

Make Omni's FFI feel first-class and easy while preserving the current
declarative FFI substrate:

- `[ffi lib]` declares a library handle.
- `[ffi lambda]` / `[ffi lambda ...]` binds a foreign function as a normal
  Omni callable.
- `^ForeignHandle` and FFI-local metadata dictionaries carry opaque-handle
  family, nullability, ownership, and finalizer policy.
- `libffi` remains the portable ABI execution layer.

This plan records the grouped module syntax direction, the borrowed ideas that
fit Omni, the `omni.toml` extension sketch, and the staged implementation
slices.

## Syntax Decision

Adopt a grouped FFI module form that keeps Omni's current return-annotation
shape and does not introduce `->` syntax.

Chosen surface:

```lisp
(define [ffi module] libc "libc.so.6"
  (strlen (^String s)) ^Integer
  (abs (^Integer n)) ^Integer
  (fopen (^String path) (^String mode))
    ^{'name File 'ownership owned 'finalizer fclose}
  (fclose (^{'name File 'ownership borrowed 'nullability non-null} file))
    ^Integer)
```

The body intentionally uses no bracketed entry forms for now. Each entry is a
signature expression immediately followed by a return annotation. The parser
should treat the grouped form as a sequence of `(signature, return_annotation)`
pairs and lower each pair to the existing single-function declarative FFI
binding.

Equivalent lowered shape:

```lisp
(define [ffi lib] libc "libc.so.6")
(define [ffi lambda libc] (strlen (^String s)) ^Integer)
(define [ffi lambda libc] (abs (^Integer n)) ^Integer)
(define [ffi lambda libc] (fopen (^String path) (^String mode))
  ^{'name File 'ownership owned 'finalizer fclose})
(define [ffi lambda libc]
  (fclose (^{'name File 'ownership borrowed 'nullability non-null} file))
  ^Integer)
```

Rules for the first implementation:

- Keep `[ffi module]` as syntax sugar over shipped `[ffi lib]` and
  `[ffi lambda]`.
- Do not introduce `->`.
- Do not introduce bracketed body entries such as `[fn ...]` in this slice.
- Reject odd or malformed body sequences fail-closed.
- Reuse the current annotation validator for return annotations and parameter
  annotations.
- Reuse existing `ForeignHandle` metadata dictionary parsing and policy
  enforcement.
- Stay within the current metadata policy: supported ownership values are
  `borrowed`, `owned`, and `manual`; supported nullability values are
  `nullable` and `non-null`; owned parameters remain rejected until a real
  transfer-of-finalizer-authority model exists.
- Keep C symbol resolution identical to `[ffi lambda]` initially.

Future body forms such as constants, opaque handles, or structs must preserve
the no-bracket body constraint unless a later owner decision explicitly changes
it.

## Borrowed Ideas To Keep

### Declare Once, Call Normally

Borrow from Janet and Common Lisp CFFI: once a binding is declared, the call
site is ordinary Omni code.

```lisp
(define [ffi module] libc "libc.so.6"
  (strlen (^String s)) ^Integer)

(strlen "hello")
```

Call sites should not repeat library handles, C symbol strings, or ABI type
lists.

### Header Import As A Workflow

Borrow from Zig and Swift: make header-driven import the normal workflow, but
keep it in `omni --bind` rather than adding a mandatory runtime Clang
dependency.

The bindgen path should:

- parse C headers with libclang,
- generate raw grouped FFI modules when practical,
- generate idiomatic Omni facade modules,
- write a manifest with the effective shipped output config,
- emit review notes for ownership, buffers, callbacks, structs, macros, and
  unsupported signatures.

### C Declaration Paste For Prototyping

Borrow from LuaJIT FFI: support a future REPL/prototype convenience that can
parse pasted C declarations and lower them to the same grouped FFI machinery.

Candidate spelling for a later lane:

```lisp
(ffi/cdef libc "libc.so.6"
  "size_t strlen(const char* s);
   int abs(int n);")
```

This must remain a convenience layer over the canonical declarative FFI
surface, not a second FFI engine.

### Raw Bindings Plus Safe Facades

Borrow from Rust bindgen: keep generated raw bindings mechanical and generate
or scaffold safer Omni facades separately.

Raw output should stay close to the C ABI. Facades should own:

- Omni-facing names,
- dispatch-friendly wrappers,
- `ForeignHandle` family names,
- ownership/finalizer policy,
- buffer validation,
- effect-friendly error shaping,
- review markers where the generator cannot prove safety.

### ABI Mode And API Mode

Borrow from Python cffi: separate fast ABI binding from stronger API/shim
binding.

- ABI mode: bind directly to installed shared libraries from signatures.
- API mode: compile a shim against headers to verify layout, constants,
  macros, inline functions, and platform-specific details.

API mode is the right place to use CppInterOp or other C/C++ tooling later.
It should still emit C ABI shims plus Omni facades rather than making C++ a
normal Omni runtime dependency.

### C++ Introspection Backend

Borrow from CppInterOp: treat the C++ compiler as an optional introspection
service for hard C++ libraries, not as Omni's normal FFI runtime.

Useful ideas:

- incremental adoption: let one binding family opt into a C++ backend without
  changing normal C FFI behavior,
- compiler-backed reflection over C++ declarations instead of ad-hoc parsing,
- template/class awareness for generating C ABI shims,
- optional out-of-process or tool-mode execution for heavy C++ analysis,
- a narrow API-mode backend that produces reviewable generated artifacts.

Candidate later `omni.toml` direction:

```toml
[dependencies.ffi.ftxui]
library = "ftxui-component"
headers = ["vendor/ftxui/include/ftxui/component/component.hpp"]
mode = "api"
generator = "cppinterop"
facade = true
ownership-policy = "review"
```

The output should still be C ABI shim code plus Omni grouped FFI/facade files.
Do not require CppInterOp for interpreter startup, ordinary `[ffi module]`
evaluation, or plain C `--bind`.

### Polyglot Runtime Plugin Ideas

Borrow from MetaFFI at the architecture level, not as a core replacement for
Omni FFI.

Useful ideas:

- explicit runtime/plugin boundaries for non-C ecosystems,
- a common data transfer shape for polyglot values,
- generated host stubs as an ergonomic layer over a lower-level dynamic API,
- entity paths or manifest entries for locating foreign module members,
- plugin installation and runtime discovery separated from language syntax,
- a test matrix that validates host/guest directions independently.

Candidate later `omni.toml` direction:

```toml
[dependencies.polyglot.analytics]
runtime = "python3"
module = "analytics.py"
entities = ["normalize", "score_batch"]
stub-output = "lib/polyglot/analytics.omni"
ownership-policy = "review"
```

Keep this as a separate polyglot lane. It should not weaken the grouped C FFI
syntax, `ForeignHandle` policy, or region-centric ownership model. If Omni ever
integrates with MetaFFI directly, start with one direction only:

- Omni host calls a MetaFFI-supported guest runtime, or
- a MetaFFI host calls an Omni guest module.

Do not attempt both host and guest plugin support in one slice.

### Structs And Opaque Handles

Borrow from Racket FFI: when structs land, generate layout-aware constructors
and accessors instead of treating them as raw pointers.

Do not include structs in the first grouped-module slice. Before structs ship,
the plan needs:

- size/alignment validation,
- by-value vs by-pointer policy,
- nested struct policy,
- array field policy,
- boundary copy/promotion tests,
- destructor/finalizer authority when structs own external resources.

Opaque handles should land first through `ForeignHandle` families.

### Callbacks As Explicit Handles

Borrow from Racket/CFFI callback capability, but preserve the existing Omni
callback decision: do not cast Omni closures directly to native function
pointers.

The first callback execution support should remain shim-only. A later generic
callback lane should use explicit callback handles and the reserved names from
the existing callback decision note:

- `ffi/callback-register`
- `ffi/callback-unregister`
- `ffi/callback-handle`

### Effects For FFI Errors

Make error reporting an Omni-specific advantage rather than copying another
FFI wholesale.

Future effect payload lanes may include:

- `ffi/load-error`
- `ffi/symbol-error`
- `ffi/type-error`
- `ffi/null-error`
- `ffi/ownership-error`
- `ffi/callback-error`

Start with one canonical FFI error payload if needed, then split only when
real handlers need finer control.

## `omni.toml` Extensions

Extend FFI dependency declarations so generated bindings are reproducible and
reviewable.

Initial sketch:

```toml
[dependencies.ffi.sqlite]
library = "sqlite3"
headers = ["/usr/include/sqlite3.h"]
functions = ["sqlite3_open", "sqlite3_close", "sqlite3_exec"]
clang-args = ["-DSQLITE_ENABLE_JSON1"]
mode = "abi"
facade = true
name-style = "kebab"
ownership-policy = "review"
```

Candidate fields:

- `library`: shared library name or path.
- `headers`: header inputs for libclang or API/shim mode.
- `functions`: allowlisted C functions.
- `exclude-functions`: denylisted C functions.
- `clang-args`: extra parser/compiler arguments.
- `mode`: `abi` or `api`.
- `facade`: whether to generate the idiomatic Omni facade.
- `name-style`: initial values `preserve` or `kebab`.
- `strip-prefixes`: C prefixes to remove in facade names.
- `ownership-policy`: `review`, `borrowed-default`, or `fail-unknown`.
- `output-raw`: raw binding module path override.
- `output-facade`: facade module path override.
- `generator`: `clang` initially; future values may include optional C++ shim
  backends.

All new fields must fail closed on unsupported values. Defaults must preserve
the current successful `--bind` workflow.

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
