# ffi-first-class-grouped-module-plan-2026-04-11 Part 01

Source: `docs/plans/ffi-first-class-grouped-module-plan-2026-04-11.md`

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
