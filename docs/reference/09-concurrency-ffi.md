# Concurrency, FFI & Schema Validation

**[Back to Index](../OMNI_REFERENCE.md)**

---

## 29. Concurrency

### Spawn & Await

```lisp
;; Spawn CPU-bound work on the worker queue
(define task-id (thread-spawn 'gzip "payload"))

;; Wait for result
(define result (thread-join task-id))

;; With timeout
(thread-join-timeout task-id 5000)   ;; 5 second timeout

;; Cancel
(thread-cancel task-id)
```

### Run Fibers

```lisp
(define results (run-fibers
  (lambda () (+ 1 2))
  (lambda () (* 3 4))))
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

`SharedBlob` is a legacy byte transport object retained only for migration
history. Production concurrency boundaries use `SharedHandle(kind=BLOB)` for
byte-sharing payload transport.

`offload` and `thread-spawn` are CPU-bound surfaces. Internal runtime I/O jobs
(`accept-fd`, `tcp-connect`, `http-get`) are not part of the public API.

Current backend policy: CPU offload jobs run on Omni's existing worker queue.
The runtime has not switched this surface to `uv_queue_work` yet.

All concurrency primitives go through effects and can be intercepted.

---

## 30. FFI

### Declare a Library

```lisp
(define [ffi lib] libc "libc.so.6")
```

### Bind Functions

```lisp
(define [ffi λ libc] (strlen (^String s)) ^Int)
(define [ffi λ libc] (abs (^Int n)) ^Int)
(define [ffi λ libc] (getpid) ^Int)

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
| `^Int` | `int`, `long`, `size_t` | sint64 |
| `^Double` | `double`, `float` | double |
| `^String` | `char*` | pointer |
| `^Ptr` | `void*` | pointer |
| `^Bool` | `int` (0/1) | sint64 |
| `^Void` | `void` | void |
| (none) | `void` return | void |

### Features

- Uses libffi for portable ABI support
- Lazy dlsym: symbol resolution deferred to first call and cached
- Mixed int/double parameters supported
- Parser recognizes both `lambda` and `λ` in `[ffi λ]`

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
(explain 'person {'name "Alice" 'age -1})
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
| `(map ...)` | Dict schema | `'(map (name string))` |
| `(vector-of s)` | Array schema | `'(vector-of int)` |
| `(tuple s1 s2)` | Fixed array | `'(tuple string int)` |
| `(enum v1 v2)` | One of values | `'(enum "a" "b")` |
