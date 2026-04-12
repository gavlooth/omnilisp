# Concurrency, FFI & Schema Validation

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 29. Concurrency

### Spawn & Await

```lisp
;; Current runtime behavior: pooled background job on the worker backend
(define task-handle (task-spawn 'gzip "payload"))

;; Wait for result
(define result (task-join task-handle))

;; With timeout
(task-join-timeout task-handle 5000)   ;; 5 second timeout

;; Cancel
(task-cancel task-handle)
```

`task-cancel` and `thread-cancel` are command-style operations and return
`Void` on successful completion.
`fiber-cancel` also returns `Void` for valid fiber ids, including no-op
completion when the target fiber is already done or running.

Current state:

- `task-spawn` / `task-join` are implemented as pooled background-task
  handles over the scheduler offload backend, not dedicated OS-thread creation.
- This is the canonical pooled-task surface in the current runtime.
- `task-join` / `task-join-timeout` are valid in both root and running-fiber
  contexts; fiber waits use non-blocking scheduler slices.

Target split:

- `fiber` / `spawn` / `await`: full structured async concurrency
- `offload`: immediate CPU-bound offload
- `task-*`: pooled background jobs on `uv_queue_work`
- `thread-*`: dedicated OS threads

### Run Fibers

```lisp
(define a (spawn (lambda () (+ 1 2))))
(define b (spawn (lambda () (* 3 4))))
(run-fibers)          ;; => #<void>
(list (await a) (await b))
;; => (3 12)
```

### Atomics

```lisp
(define counter (atomic 0))
(atomic-add! counter 1)         ;; => 0 (returns old value)
(atomic-read counter)           ;; => 1
(atomic-cas! counter 1 2)       ;; => true (compare-and-swap)
```

### Offload

```lisp
;; Run CPU-bound work on the worker queue
(offload 'gzip "payload")
(offload 'deflate "payload")

;; Test/scheduler utility
(offload 'sleep-ms 5)
```

### Concurrency Boundary Model

Concurrency crossings are intentionally narrow:

- Scheduler and local evaluation state stay thread-affine (`Value*`, `ScopeRegion*`,
  `StackCtx*`, closures, continuations).
- worker-thread payloads should be sendable copies/scalars or explicit foreign
  handles that do not own Omni value graphs.
- completion returns should pass through scheduler boundary helpers before becoming
  local `Value*` again.

`SharedBlob` is a historical byte transport object from earlier notes.
Production concurrency boundaries use `SharedHandle(kind=BLOB)` for
byte-sharing payload transport.

`offload` and the current `task-spawn` surface are CPU-bound/public pooled
job surfaces. Internal runtime I/O jobs (`accept-fd`, `tcp-connect`,
`http-get`) are not part of the public API.

Current backend policy (`uv_queue_work` + admission cap):

- `offload` and `task-*` enqueue pooled jobs through `uv_queue_work`.
- admission is bounded by an in-flight cap (`OFFLOAD_ADMISSION_MAX_IN_FLIGHT`,
  currently `256`); saturation raises deterministic scheduler/task admission
  errors instead of unbounded queue growth.
- cancellation is two-phase:
  - pending job: `uv_cancel` attempt
  - running job: cooperative cancel flag, with cancelled completion surfaced at join
- fairness is best-effort and queue-driven, not strict priority scheduling:
  completion order can differ from submission order under mixed workloads.

All concurrency primitives go through effects and can be intercepted.

---

## 30. FFI

### Declare a Library

```lisp
(define [ffi lib] libc "libc.so.6")
```

### Bind Functions

```lisp
(define [ffi λ libc] (strlen (^String s)) ^Integer)
(define [ffi λ libc] (abs (^Integer n)) ^Integer)
(define [ffi λ libc] (getpid) ^Integer)

(strlen "hello")    ;; => 5
(abs -42)           ;; => 42
(getpid)            ;; => process ID
```

### Multi-Library

```lisp
(define [ffi lib] libm "libm.so.6")
(define [ffi λ libm] (sqrt (^Double x)) ^Double)
(define [ffi λ libm] (pow (^Double x) (^Double y)) ^Double)

(sqrt 4.0)          ;; => 2.0
(pow 2.0 10.0)      ;; => 1024.0
```

### Type Mapping

| Omni Annotation | C Type | FFI Type |
|-----------------|--------|----------|
| `^Integer` | `int`, `long`, `size_t` | sint64 |
| `^Double` | `double`, `float` | double |
| `^String` | plain `char*` | copied C-string path |
| `^ForeignHandle` | `void*`, other opaque pointers | boxed foreign handle |
| `^Boolean` | `int` (0/1) | sint64 |
| `^Void` | `void` | maps the C return to the runtime `Void` singleton value |
| (none) | `void` return | use `^Void` when binding a function that returns C `void` |

Use `^ForeignHandle` as the simple default for opaque foreign resources.
Generated bindings treat only plain C `char*` as string-shaped. Byte pointers
such as `signed char*` and `unsigned char*` stay `ForeignHandle` values rather
than string buffers or string returns.
For plain character pointers, `const char*` and `char const*` are string-input
shaped; `char* const` is still mutable pointee storage and stays on the
mutable string-buffer wrapper path.
Only single-level plain `char*` pointers are string-shaped; pointer-to-pointer
spellings such as `char**` and `const char**` remain opaque `ForeignHandle`
values.
In `ffi λ`, FFI-local metadata dictionaries may refine the handle policy:
`^{'name File 'ownership owned 'finalizer fclose}` implies `ForeignHandle`, and
  the explicit `^{'type ForeignHandle ...}` form is also accepted. Dictionary
  entries are key/value pairs with quoted symbol keys; Omni does not use colon
  keywords.
Supported metadata keys are `'type`, `'name`, `'ownership`, `'finalizer`, and
`'nullability`. Supported ownership values are `borrowed`, `owned`, and
`manual`; supported nullability values are `nullable` and `non-null`. Owned
return handles require a finalizer. Owned parameter policies are rejected in
the current runtime because call arguments cannot transfer Omni-side finalizer
authority safely.
If an internal foreign handle is constructed with both a native finalizer and a
plain `free_lib_handle` fallback, the finalizer is authoritative and the plain
free path is disabled at construction. This keeps explicit release
single-authority for one foreign payload.
`ffi λ` currently accepts only the canonical annotations `^Integer`, `^Double`,
`^String`, `^ForeignHandle`, `^Boolean`, and `^Void` at the base annotation
level; unsupported annotations now raise a definition-time error instead of
defaulting to foreign-handle metadata.
Argument conversion is fail-closed:
- `^Integer`: Omni `Integer` only
- `^Double`: Omni `Double` or `Integer`
- `^Boolean`: Omni `true` / `false` only
- `^String`: Omni `String`, or `nil` for a null `char*`
- `^ForeignHandle`: live `FFI_HANDLE`, or `nil` for null
  - Metadata dictionaries in FFI position use the same quoted-symbol key/value
    syntax as the rest of Omni.
Return conversion follows the same no-raw-pointer rule:
- `^String`: non-null C `char*` returns are copied into Omni `String` values;
  null returns become `nil`
- `^ForeignHandle`: non-null pointer returns become `ForeignHandle` values;
  null returns become `nil`
Declarative `variadic` bindings are rejected at definition time until the
runtime carries truthful fixed/variadic metadata.
`^Integer` and `^Boolean` are not accepted shorthand aliases in new surface text; use
canonical integer/boolean annotations (`^Integer`, `^Boolean`) instead.
Execution mode contract:
- Interpreter/JIT `ffi λ` enforces `ForeignHandle` metadata dictionaries.
  - AOT lowering carries `ForeignHandle` policy descriptors into generated
  runtime declarations: parameters preserve handle family/nullability, and
  returns preserve handle name/ownership/finalizer.
  Manual return ownership is preserved for foreign returns and reflected as
  `'ownership manual` in `(foreign-describe)` output. Manual returns do not
  carry release capability.
  AOT return policy now rejects finalizer-bearing descriptors unless ownership is
  `'owned` so return finalizers and owned-ownership cannot drift across lowers.
- Foreign handles also carry a small common runtime descriptor: runtime kind,
  handle kind, and capability bits. `(foreign-describe handle)` returns this
  as a dictionary with quoted-symbol keys such as `'type`, `'runtime`, `'kind`,
  `'ownership`, `'name`, `'live`, and `'capabilities`; capability sequences are
  arrays of symbols, matching metadata sequence fields like `'parameters [..]`.
  Opaque foreign-resource handle names such as `File` are returned as symbols;
  C ABI library handle names remain strings because they are the `dlopen`
  target. C ABI library handles reflect load/resolve/reflect capabilities;
  direct call capability is reflected on `ForeignCallable` descriptors for
  bound functions.
- FFI-bound C ABI functions also support `(foreign-describe fn)`. The result
  uses the same dictionary/array reflection style: `'type 'ForeignCallable`,
  `'runtime 'c-abi`, `'kind 'function`, `'parameters [..]`, `'returns`, and
  call/reflect capabilities. Parameter and return descriptors include a
  user-facing `'type` plus ABI-level `'abi-type`; handle descriptors also carry
  `'name`, `'ownership`, `'nullability`, and `'finalizer` when present.
- `(foreign-release handle)` explicitly closes releasable `ForeignHandle`
  payloads through the same finalizer/free path used by scope teardown. It is
  idempotent for already-closed handles, returns `Void`, and makes released
  handles stop reflecting as `'ownership owned` once the live pointer and
  release capability are cleared. It rejects non-releasable library, borrowed,
  or manual resource handles.

### Features

- Uses libffi for portable ABI support
- Lazy dlsym: symbol resolution deferred to first call and cached
- Mixed int/double parameters supported
- Parser recognizes both `lambda` and `λ` in `[ffi λ]`
- Parser recognizes `[ffi module]` as grouped sugar over `[ffi lib]` plus
  `[ffi λ]` declarations.
- Common `ForeignHandle` descriptors are in place for the current C ABI path
  and later runtime adapters.
- `foreign-describe` covers both C ABI handles and FFI-bound C ABI callables.
- Explicit `foreign-release` is available for releasable foreign resources.

### Grouped Modules

Grouped syntax is documented in
`docs/plans/ffi-first-class-grouped-module-plan-2026-04-11.md` and shipped for
the C ABI path:

```lisp
(define [ffi module] libc "libc.so.6"
  (strlen (^String s)) ^Integer
  (abs (^Integer n)) ^Integer
  (fopen (^String path) (^String mode))
    ^{'name File 'ownership owned 'finalizer fclose}
  (fclose (^{'name File 'ownership borrowed 'nullability non-null} file))
    ^Integer)
```

`[ffi module]` lowers to the current `[ffi lib]` plus `[ffi λ]` declarations,
reusing the same annotation validation, `ForeignHandle` metadata policy, AOT
policy descriptors, and libffi execution layer. The body is a flat sequence of
function signature and return annotation pairs; missing return annotations fail
closed in this first grouped slice.

When `--bind` is used for C dependencies, generated FFI artifacts include the
raw binding module, the editable facade stub, and a sibling
`<name>_manifest.toml` file. The manifest records the effective shipped output
config: dependency, library, raw syntax, generated raw/facade paths, and
`strip-prefixes`. The dependency `NAME` must be non-empty, use only ASCII
letters, digits, `_`, and `-`, and fit the 63-byte bind output stem limit.
Malformed, empty, or overlong `strip-prefixes` entries fail the dependency
before header parsing instead of being silently truncated.
Missing required, malformed, empty, overlong, or incorrectly shaped `library`,
`raw-syntax`, `headers`, and `functions` values also fail the bind dependency
before header parsing, avoiding unquoted raw syntax selectors, truncated shared
library names, header paths, function filters, or no-op dependencies.
Adjacent quoted scalar tails, such as `library = "sqlite3" "m"`, are treated as
malformed and fail closed.
Unsupported keys in `[dependencies.ffi.NAME]` fail closed before header parsing
so typos cannot silently widen a bind or skip a generated-name policy.
Unsafe `library` stems containing slash, backslash, quote, whitespace, or
control characters fail before header parsing or generated output.
Explicit `functions = [...]` filters require every listed C function to be
found in parsed headers; missing entries fail before generated output.
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
Compile-side `.ffi-manifest.json` sidecars follow the same safety rule: JSON
strings escape all C0 control bytes, and sidecar writes publish with temp-file
plus final rename so a pre-rename failure preserves the previous final file.
Anonymous C parameter fallback names format the full numeric index (`arg123`,
etc.) so high-arity generated bindings do not emit invalid fallback symbols.
Repeated bind dependency keys fail for the same reason: Omni does not use
last-write-wins semantics for the library target, function filter, raw syntax
mode, or generated-name rewrite policy.
Repeated `[dependencies.ffi.NAME]` sections also fail closed so two dependencies
cannot target the same generated raw, facade, or manifest output stem.
Dependency section count overflow also fails closed before header parsing or
output generation, and overflow section keys cannot mutate the last accepted
dependency.
Inline `#` comments are stripped only outside quoted bind dependency values;
quoted strings containing `#` are preserved.

Design constraints from the grouped-FFI plan:

- no `->` return syntax and no bracketed body entries in the first slice,
- no raw integer pointer ABI surface,
- normal C FFI stays on libffi and must not require a C++ runtime dependency,
- optional C++/CppInterOp support, if added later, is a bindgen/API-mode
  introspection backend that emits C ABI shims plus Omni facades,
- polyglot runtime/plugin support belongs in a separate lane and must not
  weaken `ForeignHandle` policy,
- common runtime descriptors and `foreign-describe` are shared metadata, not a
  promise that every runtime adapter has shipped,
- generic callback support must use explicit callback handles, not direct
  Omni-closure-to-native-function-pointer coercion,
- generated bindgen callback scaffolds fail closed until edited for a concrete
  subsystem callback-handle shim; arbitrary callback parameters are not routed
  through the `uv` timer callback shim,
- generated bindgen mutable string-buffer scaffolds fail closed for
  `manual-review` teardown until the facade is edited with an explicit
  allocation/writeback policy; `none` teardown buffers remain caller-owned
  pass-through values after role, ownership, direction, and size validation.
- name-based mutable string-buffer direction inference treats `inout` as more
  specific than `out`, so names such as `inout_buffer` are generated with
  `buffer-direction=inout`.

---

## 31. Schema Validation

Data-driven validation where schemas are plain Omni data.

```lisp
(define [schema] person
  (map
    (name string)
    (age (and int (> 0)))
    (email (maybe (re "^[^@]+@[^@]+$")))))

;; Validate
(validate 'person {'name "Alice" 'age 30})
;; => true

;; Explain failures
(schema-explain 'person {'name "Alice" 'age -1})
;; => ((age "must satisfy (> 0)"))
```

### Schema Types

| Schema | Validates | Example |
|--------|-----------|---------|
| `string` | String value | `'string` |
| `int` | Integer | `'int` |
| `double` | Float | `'double` |
| `bool` | Boolean | `'bool` |
| `any` | Always passes | `'any` |
| `(and s1 s2)` | Both pass | `'(and int (> 0))` |
| `(or s1 s2)` | Either passes | `'(or string int)` |
| `(not s)` | Negation | `'(not nil)` |
| `(maybe s)` | Nil or passes | `'(maybe string)` |
| `(= v)` | Exact value | `'(= 42)` |
| `(> n)` / `(< n)` | Comparison | `'(> 0)` |
| `(re pat)` | Regex match | `'(re "[a-z]+")` |
| `(map ...)` | Dictionary schema | `'(map (name string))` |
| `(array-of s)` | Array schema | `'(array-of int)` |
| `(tuple s1 s2)` | Fixed array | `'(tuple string int)` |
| `(enum v1 v2)` | One of values | `'(enum "a" "b")` |
