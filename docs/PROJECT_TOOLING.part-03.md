## omni.toml Format

The project configuration uses a minimal TOML subset.

### Project Metadata

```toml
[project]
name = "myproject"
version = "0.1.0"
```

### Build Configuration

The `[build]` section controls compilation settings. These map to C3 compiler flags in the generated `build/project.json`.

```toml
[build]
output-dir = "build"        # Where build artifacts go
safety = "safe"             # "safe", "fast", or "none"
opt = "O0"                  # "O0", "O1", "O2", "O3", "Os", "Oz"
debug-info = "full"         # "full", "line-tables", "none"
# sanitize = "none"         # "none", "address", "memory", "thread"
# single-module = false
```

| Field | Default | Values | Description |
|-------|---------|--------|-------------|
| `output-dir` | `"build"` | Any path | Build output directory |
| `safety` | `"safe"` | `"safe"`, `"fast"`, `"none"` | Runtime bounds/null checks, contracts |
| `opt` | `"O0"` | `"O0"`–`"O3"`, `"Os"`, `"Oz"` | Optimization level |
| `debug-info` | `"full"` | `"full"`, `"line-tables"`, `"none"` | Debug symbol level |
| `sanitize` | `"none"` | `"none"`, `"address"`, `"memory"`, `"thread"` | Sanitizer |
| `single-module` | `false` | `true`, `false` | Compile all modules together (more inlining) |

**Safety levels explained:**

- **`"safe"`** (default) — Bounds checking, null pointer checks, contract enforcement. Use during development.
- **`"fast"`** — Disables safety checks for maximum performance. Use for release/production.
- **`"none"`** — No safety, no contracts. Only for benchmarking or when you know exactly what you're doing.

### FFI Dependencies

Each C library dependency gets a `[dependencies.ffi.NAME]` section:

```toml
[dependencies.ffi.math]
library = "m"                           # Shared lib name → dlopen("libm.so")
headers = ["/usr/include/math.h"]       # C headers to parse
functions = ["sin", "cos", "sqrt"]      # Optional: only bind these functions
# raw-syntax = "grouped"                # Optional: legacy (default) or grouped
# strip-prefixes = ["sqlite3_"]         # Optional: facade-name prefix stripping
```

| Field | Required | Description |
|-------|----------|-------------|
| `library` | Yes | Shared library name stem (without `lib` prefix and `.so` suffix). Must be a non-empty quoted string shorter than 128 bytes and must not contain slash, backslash, quote, whitespace, or control characters. |
| `headers` | Yes | Non-empty array of non-empty quoted C header file paths to parse. Entries must be shorter than 256 bytes. |
| `functions` | No | Non-empty array of non-empty quoted function names to bind. If omitted, all exported functions are bound. Entries must be shorter than 64 bytes. |
| `raw-syntax` | No | Quoted `"legacy"` (default) emits `[ffi lib]` plus `[ffi lambda]`; quoted `"grouped"` emits grouped `[ffi module]` raw bindings. Unquoted or unsupported values fail closed. |
| `strip-prefixes` | No | Array of C name prefixes to remove from generated facade/exported Omni names and `raw-*` aliases. Raw binding names preserve the C-derived symbol names used by `dlsym`. Entries are parsed strictly; malformed, empty, or overlong prefixes fail the dependency. |

`NAME` is also used as the generated output filename stem under `lib/ffi/`.
It must be a non-empty output-safe name containing only ASCII letters, digits,
`_`, and `-`, and it must fit the 63-byte bind output stem limit. Unsafe or
overlong names, and repeated `[dependencies.ffi.NAME]` sections, are rejected
before raw, facade, or manifest files are written, keeping `NAME` safe as both a
file stem and generated Omni module name.
The current parser accepts at most `TOML_MAX_DEPS` FFI dependency sections;
larger manifests fail closed before header parsing or output generation, and
overflow section keys cannot mutate the last accepted dependency.
Missing required, malformed, empty, overlong, or incorrectly shaped `library`,
`raw-syntax`, `headers`, `functions`, and `strip-prefixes` values fail the
dependency before header parsing instead of being silently truncated, accepted
loosely, or treated as a no-op dependency.
Malformed adjacent quoted scalar tails, such as `library = "sqlite3" "m"`, are
rejected instead of being accepted as one library or raw-syntax string.
Unsupported keys in `[dependencies.ffi.NAME]` also fail closed before header
parsing, so typos such as `function = [...]` do not silently widen the bind to
all exported functions.
Unsafe `library` stems containing slash, backslash, quote, whitespace, or
control characters fail before header parsing or generated output.
When `functions = [...]` is present, every requested C function must be found
in the parsed headers; missing filter entries fail the dependency before output
generation instead of producing partial bindings or a successful no-op.
If a later header fails after earlier headers were parsed, `--bind` releases
the earlier parsed function metadata before failing and does not write partial
outputs.
Inline `#` comments after section headers are accepted. Malformed
section-header lines starting with `[` reset parser context so following keys
cannot mutate the previously active dependency section.
If bindgen writes a new raw module but facade generation then fails, the new
raw file is removed before returning failure. Existing raw files are left in
place on rerun failures.
If raw/facade generation succeeds but manifest writing then fails, first-time
raw, facade, and manifest artifacts are cleaned before dependency failure.
Existing raw, facade, or manifest artifacts are left in place on rerun failures.
Raw, facade, and manifest text writers use sibling temp paths and rename into
place only after a full write and close succeeds. Failed final renames clean
their temp output instead of replacing the target with a partial file.
Compile-side `.ffi-manifest.json` sidecars also publish through temp-file plus
final rename, and their JSON string fields escape all C0 control bytes.
Anonymous C parameter fallback names format the full numeric index (`arg123`,
etc.) so high-arity generated bindings do not emit invalid fallback symbols.
Repeating any of these keys in one dependency section also fails closed instead
of letting a later value overwrite the bind target, filter, syntax mode, or
generated-name rewrite policy.
Inline `#` comments are stripped only outside quoted values, so examples such
as `library = "m" # comment` stay valid while quoted values containing `#` are
preserved.

Planned FFI tooling extensions are tracked in
`docs/plans/ffi-first-class-grouped-module-plan-2026-04-11.md`, not all
implemented in the current parser/bindgen path. The parser now accepts
fail-closed fields such as `exclude-functions`, `clang-args`, `mode`
(`abi` or `api`), `facade`, `name-style`, `ownership-policy`, `output-raw`,
`output-facade`, and `generator`. The shipped output-format pieces are
`raw-syntax`, facade-only `strip-prefixes`, `exclude-functions`,
`mode`, `generator`, and the generated effective-config manifest.

The policy split is:

- ABI mode binds directly to installed shared libraries from signatures.
- API mode may compile or generate a shim against headers to validate layout,
  constants, macros, inline functions, and platform details.
- Optional C++/CppInterOp-style introspection belongs in API/tooling mode and
  is recorded as API-mode metadata in the bindgen manifest and raw/facade
  output markers; it must still emit reviewable C ABI shims plus Omni facades,
  not add a required C++ runtime dependency for ordinary Omni startup.
- Polyglot runtime/plugin support is not planned for this FFI tooling surface
  and should not reuse or weaken the C FFI `ForeignHandle` policy.

### Multiple Dependencies

```toml
[dependencies.ffi.math]
library = "m"
headers = ["/usr/include/math.h"]
functions = ["sin", "cos", "sqrt", "pow", "floor", "ceil"]

[dependencies.ffi.curl]
library = "curl"
headers = ["/usr/include/curl/curl.h"]
# No functions list = bind ALL exported function declarations
```

---

## Generated Bindings

Running `--bind` now produces three files per dependency in `lib/ffi/`:

- `lib/ffi/<name>_raw.omni`: regenerated low-level declarative FFI bindings
- `lib/ffi/<name>.omni`: facade stub created only when missing; intended place for Omni-facing cleanup
- `lib/ffi/<name>_manifest.toml`: regenerated effective-config manifest for
  the shipped bindgen output fields

### Example: `lib/ffi/math_raw.omni`

```lisp
;; Auto-generated raw FFI bindings for libm
;; Regenerate with: omni --bind

(module ffi-math-raw (export cos sin sqrt)

  (define [ffi lib] _lib "libm.so")

  (define [ffi lambda _lib] (cos (^Float64 x)) ^Float64)

  (define [ffi lambda _lib] (sin (^Float64 x)) ^Float64)

  (define [ffi lambda _lib] (sqrt (^Float64 x)) ^Float64)

)
```

With `raw-syntax = "grouped"`, the raw module uses the grouped declarative FFI
form while preserving the same facade behavior:

```lisp
;; Auto-generated raw FFI bindings for libm
;; Regenerate with: omni --bind
;; Raw syntax: grouped

(module ffi-math-raw (export cos sin sqrt)

  (define [ffi module] _lib "libm.so"
    (cos (^Float64 x)) ^Float64
    (sin (^Float64 x)) ^Float64
    (sqrt (^Float64 x)) ^Float64
  )

)
```

### Example: `lib/ffi/math.omni`

```lisp
;; Auto-generated FFI facade stub for libm
;; Raw bindings live in the sibling *_raw.omni file.
;; This facade is created only when missing so local edits survive reruns.

(module ffi-math (export cos sin sqrt)

  (import "math_raw.omni")
  (import ffi-math-raw ((cos 'as raw-cos) (sin 'as raw-sin) (sqrt 'as raw-sqrt)))

  ;; Edit this facade to add Omni-facing naming, ownership, or buffer cleanup.
  (define cos raw-cos)
  (define sin raw-sin)
  (define sqrt raw-sqrt)
)
```

### Example: `lib/ffi/math_manifest.toml`

```toml
# Auto-generated FFI bindgen manifest.
# Regenerate with: omni --bind
format-version = "1"
dependency = "math"
library = "m"
raw-syntax = "grouped"
raw-output = "lib/ffi/math_raw.omni"
facade-output = "lib/ffi/math.omni"
strip-prefixes = []
```

For non-trivial string/pointer/callback contracts, the facade emits a typed
wrapper scaffold instead of only a raw alias:

```lisp
;; TODO(bindgen): review ret-string — string return ownership/nullability is not automatic.
(define (ret-string)
  (let (result (raw-ret-string)
        result_text result)
    ;; REVIEW: decide whether to copy, borrow, or null-check the returned string.
    result_text)
)
```

### What Gets Generated

- A regenerated raw module with the low-level declarative bindings; by default
  it uses legacy `[ffi lib]` plus `[ffi lambda]`, and `raw-syntax = "grouped"`
  opts into grouped `[ffi module]`
- A facade stub that imports the raw module and:
  - re-exports trivial bindings as simple aliases
  - emits typed wrapper-function scaffolds for non-trivial string/pointer/callback contracts
  - gives return-bearing wrappers a local `result` binding plus category-specific edit-point locals such as `result_text` or `result_handle`
  - gives parameter-shaped wrappers category-specific local aliases such as `buffer_input`, `callback_handle`, or `<name>_handle` before the raw call
- Pure `const char *` input functions stay simple aliases even though the raw file still records their metadata; wrapper scaffolds are reserved for contracts that actually need body-level review work
- The facade file is only created when missing, so rerunning `--bind` does not overwrite local wrapper edits
- A regenerated `<name>_manifest.toml` file records the effective shipped
  bindgen output config: dependency, library, raw syntax, raw/facade output
  paths, and `strip-prefixes`
- FFI dependency names used for generated output paths must be non-empty and
  contain only ASCII letters, digits, `_`, and `-`, and must fit the 63-byte
  bind output stem limit; unsafe or overlong names fail closed before files are
  written
- FFI dependency section count overflow fails closed before files are written,
  and overflow section keys cannot mutate the last accepted dependency
- C `snake_case` names are converted to Omni `kebab-case` (e.g., `string_length` becomes `string-length`)
- C parameter names are preserved when libclang exposes them; unnamed prototypes fall back to `argN`
- Non-trivial string/pointer/callback contracts now emit `bindgen-descriptor`
  comments using the shared `ForeignCallable` descriptor vocabulary
  (`type`, `runtime`, `kind`, `name`, `c-name`, `parameters`, `returns`,
  `abi-type`) plus `bindgen-policy` comments for generator review policy; these
  comments are documentation for generated code, not a runtime reflection source
- Callback wrapper helpers validate metadata but fail closed until the facade is
  edited to use a concrete subsystem callback-handle shim; generic bindgen
  output does not route arbitrary callback parameters through the `uv` timer
  callback shim
- Mutable string-buffer wrapper helpers validate buffer, direction, and size
  metadata, but `manual-review` buffers fail closed until the facade is edited
  with an explicit allocation and writeback policy; `none` teardown buffers
  remain caller-owned pass-through values after role, ownership, direction, and
  size validation
- Name-based mutable string-buffer direction inference treats `inout` as more
  specific than `out`, so names such as `inout_buffer` are generated with
  `buffer-direction=inout`
- Variadic C functions are skipped with a comment because declarative `ffi` variadics are not a shipped runtime contract yet

### Using Generated Bindings

```lisp
;; Import the facade module
(import "lib/ffi/math.omni")

;; Use via qualified access
(ffi-math.sin 1.0)    ;; => 0.8414709848...
(ffi-math.sqrt 2.0)   ;; => 1.4142135623...

;; Or import specific functions
(import ffi-math (sin cos sqrt))
(sin 3.14159)          ;; => ~0.0
```

---

## C-to-Omni Type Mapping

The binding generator uses libclang to resolve types (including typedefs) and maps them:

| C Type | Omni FFI Symbol | Omni Type Annotation | Notes |
|--------|----------------|---------------------|-------|
| `int`, `long`, `unsigned int`, `unsigned long` | `'int` | `^Integer` | All integer-width types |
| `size_t`, `ssize_t` | `'int` | `^Integer` | Resolved via typedef |
| `enum` types | `'int` | `^Integer` | Enums are integers |
| `float`, `float64` | `'float64` | `^Float64` | All floating-point types |
| plain `const char *`, `char *` returns | `'string` | `^String` | Non-null returns are copied into Omni strings; return ownership/nullability is still surfaced via metadata/comments for review |
| plain mutable `char *` parameters | `'ptr` | `^ForeignHandle` | Fail-closed default for in/out buffers; emitted metadata still marks these as `string-buffer` for facade scaffolding |
| `signed char *`, `unsigned char *`, byte pointers | `'ptr` | `^ForeignHandle` | Treated as opaque foreign-handle data, not Omni strings or string buffers |
| `void *`, other pointers | `'ptr` | `^ForeignHandle` | Opaque boxed foreign handle; when the handle family and teardown policy are known, bindgen may emit FFI-local metadata dictionaries such as `^{'name File 'ownership owned 'finalizer fclose}` |
| `void` (return only) | `'void` | `^Void` | Returns the runtime `Void` singleton value |

For generated bindings, `const char*` and `char const*` are string-input
shaped. `char* const` is still mutable pointee storage with a const pointer
slot, so bindgen keeps it on the mutable string-buffer wrapper path.
Only single-level plain `char*` pointers are string-shaped; pointer-to-pointer
spellings such as `char**` and `const char**` remain opaque `ForeignHandle`
values.

Use `^ForeignHandle` as the simple default for opaque foreign resources. FFI-local
metadata dictionaries may refine the policy in `ffi λ` position, for example
`^{'name File 'ownership owned 'finalizer fclose}` implies `ForeignHandle`, and
the explicit `^{'type ForeignHandle ...}` form is also accepted. Dictionary
entries are key/value pairs with quoted symbol keys; Omni does not use colon
keywords. Supported metadata keys are `'type`, `'name`, `'ownership`,
`'finalizer`, and `'nullability`; ownership values are `borrowed`, `owned`, and
`manual`; nullability values are `nullable` and `non-null`. Owned return
handles require a finalizer, and owned parameter policies are rejected until
the runtime has a truthful argument-ownership transfer model.

Grouped FFI module syntax is shipped and can be used by `--bind` with
`raw-syntax = "grouped"`:

```lisp
(define [ffi module] libc "libc.so.6"
  (strlen (^String s)) ^Integer
  (fopen (^String path) (^String mode))
    ^{'name File 'ownership owned 'finalizer fclose})
```

The implementation plan is
`docs/plans/ffi-first-class-grouped-module-plan-2026-04-11.md`. Generated raw
modules continue to use `[ffi lib]` plus `[ffi λ]` unless the FFI dependency
opts into grouped output.

`strip-prefixes = ["prefix_"]` can be used with either raw syntax mode to strip
matching C prefixes from generated facade names and `raw-*` aliases. The raw
binding module keeps the original C-derived names so the current parser-driven
`dlsym` path remains truthful. Bindgen validates the generated Omni-facing names
before writing output files; prefix stripping fails closed if it would produce a
name that the lexer reads as a number-leading token, such as `3d-distance`, or
if two generated Omni-facing names would collide after stripping. The
`library`, `raw-syntax`, `headers`, `functions`, and `strip-prefixes` TOML
fields are parsed strictly on this bind path: missing required, malformed,
empty, overlong, unquoted, or incorrectly shaped values fail the dependency
before header parsing instead of being truncated or accepted loosely.
Adjacent quoted scalar tails, such as `library = "sqlite3" "m"`, are treated as
malformed and fail closed.
Unsupported keys fail closed before header parsing; unimplemented future
extension fields must be added deliberately instead of being accepted as no-ops.
Unsafe `library` stems containing slash, backslash, quote, whitespace, or
control characters fail before generated raw/facade/manifest output.
Explicit `functions = [...]` filters require every listed C function to be
found in parsed headers; missing entries fail closed before output generation.
If a later header fails after earlier headers were parsed, `--bind` releases
the earlier parsed function metadata before failing and does not write partial
outputs.
Duplicate keys fail closed instead of using last-write-wins semantics for the
bind target, function filter, syntax mode, or generated-name rewrite policy.
Repeated dependency sections fail closed for the same reason: multiple entries
must not target the same generated raw, facade, or manifest output stem.
Dependency section count overflow fails closed before header parsing or output
generation, and overflow section keys cannot mutate the last accepted
dependency.
Inline `#` comments are ignored outside quoted values and preserved inside
quoted strings.

### Limitations

- **Struct parameters/returns**: C functions that pass or return structs by value are rejected explicitly; `--bind` fails instead of emitting a pointer-coerced binding (the declarative `ffi` surface only ships scalar/pointer calls today)
- **Variadic functions**: Skipped automatically (e.g., `printf`)
- **Function pointers as parameters**: Mapped as `'ptr`/`^ForeignHandle` (callback registration requires manual wrappers; metadata dictionaries can still name the family and finalizer policy when available)
- **Unsupported parameter metadata allocation**: `--bind` now fails closed if it cannot allocate parameter descriptors instead of silently emitting malformed zero-argument wrappers
- **Macros**: `#define` constants and macro-functions are not parsed (libclang only sees declarations)

---

## Dependencies

### libclang (optional — only needed for `--bind`)

libclang is loaded at runtime via `dlopen` only when `--bind` is invoked. It is **not** required for building, running, or compiling Omni programs.

**Install:**

| Distribution | Command |
|-------------|---------|
| Arch Linux | `pacman -S clang` |
| Debian/Ubuntu | `apt install libclang-dev` |
| Fedora | `dnf install clang-devel` |

The tool searches for libclang in this order:
1. `libclang.so`
2. `libclang.so.1`
3. `/usr/lib/libclang.so`
4. `/usr/lib/llvm/lib/libclang.so`

If libclang is not found, `--bind` prints install instructions and exits with an error.

---

## Complete Workflow Example

```bash
# 1. Create a new project
omni --init calculator

# 2. Edit omni.toml to add math library
cat > calculator/omni.toml << 'EOF'
[project]
name = "calculator"
version = "0.1.0"

[build]
output-dir = "build"
safety = "safe"
opt = "O0"
debug-info = "full"

[dependencies.ffi.math]
library = "m"
headers = ["/usr/include/math.h"]
functions = ["sin", "cos", "tan", "sqrt", "pow", "log", "exp", "floor", "ceil"]
EOF

# 3. Generate FFI bindings
omni --bind calculator/

# 4. Inspect or edit the generated facade if needed
sed -n '1,80p' calculator/lib/ffi/math.omni

# 5. Write your program
cat > calculator/src/main.omni << 'EOF'
(import "lib/ffi/math.omni")

(println "sin(pi/2) =" (ffi-math.sin 1.5707963))
(println "sqrt(2)   =" (ffi-math.sqrt 2.0))
(println "2^10      =" (ffi-math.pow 2.0 10.0))
EOF

# 6. Run it
cd calculator
omni src/main.omni

# AOT note: generated declarative ffi modules carry ABI tags and
# ForeignHandle policy descriptors for parameters and returns.
```

---

## Implementation Details

### Architecture

```
omni.toml ──► TOML Parser ──► TomlConfig
                                    │
C Headers ──► libclang (dlopen) ──► ParsedFunc[]
                                    │
                                    ▼
                            Binding Generator ──► lib/ffi/*_raw.omni + facade stubs
```

### Source Files

| File | Description |
|------|-------------|
| `src/lisp/toml.c3` | Minimal TOML parser (~260 lines) |
| `src/lisp/libclang_bind.c3` | libclang dlopen wrapper + C header visitor (~300 lines) |
| `src/lisp/bindgen.c3` | Omni FFI module code generator (~150 lines) |
| `src/entry.c3` | `run_init()` and `run_bind()` CLI handlers |

### Design Decisions

- **`omni.toml` + `build/project.json`**: Omni's config lives at project root; C3's `project.json` is generated inside `build/` to keep the root clean and make the ownership clear. C3 is invoked with `--path build/` to find it.
- **libclang via C3-level dlopen** (not Omni declarative FFI): libclang returns structs by value (`CXString`, `CXCursor`, `CXType`) which the declarative `ffi` surface does not model. C3 function pointer aliases with proper struct types handle the host ABI correctly.
- **Optional runtime dependency**: libclang is only loaded when `--bind` is invoked. Projects that don't use `--bind` have zero additional dependencies.
- **Canonical type resolution**: The type mapper calls `clang_getCanonicalType` to resolve typedefs before mapping, so `size_t` correctly maps to `'int` regardless of the typedef chain.
