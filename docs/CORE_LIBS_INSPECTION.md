# Core Libraries Inspection — Making Omni Usable

## Current State (Session 67)

Omni has a solid runtime (JIT, fibers, effects, scope-region memory, FFI) plus a full
I/O and data processing stack. The effect fast-path dispatch table allows adding new
I/O operations without touching the JIT.

### Integrated (Session 67, all statically linked)

| Library       | Role                         | License  | Files                          |
|--------------|------------------------------|----------|--------------------------------|
| GNU Lightning | JIT code generation          | LGPL-3.0 | (dynamic)                      |
| libffi        | FFI calling convention       | MIT      | (dynamic)                      |
| replxx        | REPL (highlighting, history) | BSD-2    | (dynamic)                      |
| libclang      | Bindgen (`--bind`)           | Apache-2.0 | (dynamic)                    |
| Pika engine   | Regex + PEG grammar (built-in C3) | —   | `src/pika/` (11 files)         |
| utf8proc      | Unicode string ops           | MIT      | `src/lisp/unicode.c3`          |
| yyjson        | JSON parse/emit              | MIT      | `src/lisp/json.c3` + `csrc/json_helpers.c` |
| libdeflate    | gzip/deflate compression     | MIT      | `src/lisp/compress.c3`         |
| libuv         | TCP/DNS/timers (linked, POSIX fast-path) | MIT | `src/lisp/async.c3`       |
| BearSSL       | TLS client connections       | MIT      | `src/lisp/tls.c3` + `csrc/tls_helpers.c` (33 lines) |

Static archives built from source via `deps/build_static.sh` → `deps/lib/*.a`.

### Effect Fast-Path Dispatch Table

All I/O goes through effects (`signal`/`handle`/`resolve`). When no handler is installed,
a dispatch table (`FastPathEntry[32]` on Interp) routes to blocking `__raw-*` primitives.
New effects: just call `register_fast_path(interp, "io/foo", "__raw-foo")` — zero JIT changes.

---

## Tier 1: DONE — Foundation ✓

### 1. libuv — Async I/O ✓
TCP connect/read/write/close, DNS resolve, async-sleep. All through effects.
POSIX blocking fast-path for v1. libuv linked for future async scheduler.

### 2. utf8proc — Unicode ✓
Unicode-aware string-upcase/downcase, string-normalize (NFC/NFD/NFKC/NFKD),
string-graphemes, string-codepoints, char-category.

### 3. Pika regex/grammar — INTEGRATED ✓
re-match, re-find-all, re-split, re-replace, re-fullmatch, re-match-pos, re-find-all-pos.
pika/grammar, pika/parse, pika/fold. `lisp_semantics.c3` disabled (AST mismatch).

### 4. yyjson — JSON ✓
json-parse (JSON → dict/array/string/int/double/nil), json-emit, json-emit-pretty.

### 5. BearSSL — TLS ✓
tls-connect (wraps TCP handle), tls-read, tls-write, tls-close.
Direct extern fn to BearSSL, 33-line C callback helper. v1 skips cert verification.

### 6. libdeflate — Compression ✓
gzip, gunzip, deflate, inflate.

---

## Tier 2: Data Integrity — Contracts + Datalog

### 7. Data-Driven Contracts (Malli-inspired)

**What:** Schema validation system where schemas are plain Omni data (lists + symbols).
Validates dicts, arrays, and values against structural schemas.

**Design lineage:** Malli (Clojure) → adapted for Omni's quoted symbols + multiple dispatch.

**What's kept from Malli:**
- Schemas are data, not macros — composable, introspectable, serializable
- Combinators: `and`, `or`, `not`, `maybe` (optional)
- Collection schemas: `map` (required/optional keys), `vector`, `tuple`
- Coercion and explain (error reporting)

**What's changed for Omni:**
- Clojure keywords (`:string`) → quoted symbols (`'string`) — Omni has no keywords
- Protocol dispatch → multiple dispatch on schema tag symbol
- Runtime schema registry → schemas are just values, dispatch handles extension
- Zero-allocation validation path — dispatch directly, no intermediate schema objects

**Syntax:**

```lisp
;; Schema as quoted data
(define [schema] person
  (map
    (name string)
    (age (and int (> 0)))
    (email (maybe (re "^[^@]+@[^@]+$")))))

;; Validate
(validate 'person (dict 'name "Alice" 'age 30))
;; => true

;; Explain failures
(explain 'person (dict 'name "Alice" 'age -1))
;; => ((age "must satisfy (> 0)"))

;; Compose schemas
(define [schema] team
  (map
    (name string)
    (members (vector-of person))))
```

**Schema types (dispatch targets):**

| Tag | Validates | Example |
|-----|-----------|---------|
| `string` | String value | `'string` |
| `int` | Integer value | `'int` |
| `double` | Float value | `'double` |
| `bool` | Boolean | `'bool` |
| `any` | Always passes | `'any` |
| `(and s1 s2)` | Both pass | `'(and int (> 0))` |
| `(or s1 s2)` | Either passes | `'(or string int)` |
| `(not s)` | Fails | `'(not nil)` |
| `(maybe s)` | Nil or passes | `'(maybe string)` |
| `(= v)` | Exact value | `'(= 42)` |
| `(> n)` `(< n)` | Comparison | `'(> 0)` |
| `(re pat)` | Regex match (Pika) | `'(re "[a-z]+")` |
| `(map ...)` | Dict with keys | `'(map (name string))` |
| `(vector-of s)` | Array of schema | `'(vector-of int)` |
| `(tuple s1 s2)` | Fixed-size array | `'(tuple string int)` |
| `(enum v1 v2)` | One of values | `'(enum "a" "b" "c")` |

**Implementation approach:**
- Parser: `define [schema]` stores schema value in global env
- Validation: recursive walk of schema data, multiple dispatch on `car` of each node
- `validate` returns bool, `explain` returns list of `(path message)` pairs
- Zero allocation on success (dispatch + comparisons only)
- Pika regex integrated via `re` schema tag

**Estimated complexity:** ~300 lines C3 + ~50 lines stdlib

---

### 8. Relational Datalog on LMDB

**What:** Embedded deductive database. Named relations (tables) with Datalog queries
and recursive rules. Backed by LMDB for persistence.

**Design lineage:**

```
Malli ──→ data-driven schemas (kept)
Datalevin/Datomic ──→ LMDB backend (kept), EAV triples (DROPPED)
Soufflé ──→ relational model, bottom-up evaluation (adopted)

Result: Soufflé's query model + Datalevin's storage + Omni's define [attr] syntax
```

**Why not Datomic/DataScript style (EAV triples):**

| Datomic Pain Point | Problem | Relational Fix |
|--------------------|---------|---------------|
| EAV triples | Reading 1 entity = N index lookups | Relations return complete tuples |
| Entity reassembly | Need Pull API to reconstruct objects | Tuples are already complete |
| Implicit schema | Entity "types" = bags of attributes | `define [relation]` with named columns |
| Immutable history forever | Disk grows monotonically | Mutable default, opt-in history |
| Entity ID confusion | Tempids, lookup refs, idents | Declared primary keys |
| Weak aggregation | Datalog bad at GROUP BY / ORDER BY | Aggregate functions in query syntax |

**Why not other approaches:**

| Alternative | Rejected Because |
|-------------|-----------------|
| DataScript (in-memory) | Persistent data structures (HAMT) need GC. Worst fit for region memory. |
| Curry (functional-logic) | Non-deterministic narrowing generates allocation pressure from search trees. |
| PostgreSQL | External process, network overhead. Violates self-contained. |
| SQLite | Good for SQL, but Datalog rules/recursion are awkward via CTEs. |

**Why LMDB:**
- **mmap-based** — OS manages page caching, no interaction with Omni's region allocator
- **Embedded** — single C library, ~50KB compiled
- **Single-writer** — matches Omni's single-threaded model
- **Copy-on-write B+ tree** — ACID transactions, crash-safe
- **Proven** — Datalevin (Clojure) and Mentat (Rust/Mozilla) both do Datalog-on-LMDB

**Syntax:**

```lisp
;; Open database
(define db (db-open "app.db"))

;; Define relations (named columns, explicit schema)
(define [relation] person (name age email))
(define [relation] follows (follower followee))
(define [relation] road (city1 city2 km))

;; Assert facts — tuples, not triples
(assert! 'person "Alice" 30 "alice@b.com")
(assert! 'person "Bob" 25 "bob@b.com")
(assert! 'follows "Alice" "Bob")

;; Datalog query — ?vars are logic variables
(query
  '((person ?name ?age _)
    (> ?age 28)))
;; => (("Alice" 30 "alice@b.com"))

;; Rules — derived relations
(define [rule] (adult ?name ?age)
  (person ?name ?age _)
  (>= ?age 18))

;; Recursive rules — Datalog's strength (always terminates)
(define [rule] (reachable ?a ?b)
  (follows ?a ?b))

(define [rule] (reachable ?a ?b)
  (follows ?a ?mid)
  (reachable ?mid ?b))

;; Query derived relations
(query '((reachable "Alice" ?who)))
;; => (("Bob"))
```

**Graph encoding — Datalog's sweet spot:**

```lisp
;; Graphs are just relations
(define [relation] edge (from to))
(define [relation] road (city1 city2 km))

;; Reachability — 2 rules
(define [rule] (reachable ?a ?b) (edge ?a ?b))
(define [rule] (reachable ?a ?b) (edge ?a ?mid) (reachable ?mid ?b))

;; Cycle detection
(define [rule] (has-cycle ?node) (reachable ?node ?node))

;; Shortest path (weighted + aggregation)
(define [rule] (path ?a ?b ?cost) (road ?a ?b ?cost))
(define [rule] (path ?a ?b ?cost)
  (road ?a ?mid ?c1)
  (path ?mid ?b ?c2)
  (= ?cost (+ ?c1 ?c2)))

(query '((path "Athens" ?dest ?km))
       '(aggregate (min ?km) (group ?dest)))

;; Social graph
(define [rule] (fof ?a ?c)
  (follows ?a ?b)
  (follows ?b ?c)
  (not (= ?a ?c)))

(define [rule] (mutual ?a ?b)
  (follows ?a ?b)
  (follows ?b ?a))
```

**LMDB storage model:**

Each relation gets B+ tree indices in LMDB:
- Primary index: all columns concatenated as sorted key bytes
- Secondary indices: by each column for efficient lookups

```
relation "person":
  index 0 (full):  "Alice\x0030\x00alice@b.com" → ∅
  index 1 (name):  "Alice" → [ptr to full tuple]
  index 2 (age):   30 → [ptr to full tuple]
```

Tuples stored as concatenated bytes with type tags. LMDB's sorted B+ tree gives
range queries for free (e.g., `(> ?age 28)` becomes a range scan on index 2).

**Query evaluation:**

Bottom-up semi-naive evaluation (like Soufflé):
1. Evaluate base relations (LMDB lookups)
2. Apply rules to derive new facts
3. Repeat until no new facts (fixpoint)
4. Filter by query pattern

Semi-naive optimization: only join with *newly derived* facts each round,
avoiding redundant re-derivation.

**Contracts + Datalog integration:**

Schemas validate data before insertion:
```lisp
(define [schema] person-valid
  (tuple string (and int (> 0)) (re "^[^@]+@.+")))

;; assert! checks schema before writing to LMDB
(assert! 'person "Alice" -1 "bad")
;; => error: age must satisfy (> 0)
```

**Implementation approach:**

| Component | Lines (est.) | Notes |
|-----------|-------------|-------|
| LMDB extern fn | ~30 | `mdb_env_*`, `mdb_txn_*`, `mdb_cursor_*` |
| Relation storage | ~200 | Tuple encoding/decoding, index management |
| Unification engine | ~150 | Pattern matching with logic variables |
| Semi-naive evaluator | ~200 | Bottom-up fixpoint with stratified negation |
| Query compiler | ~150 | Omni query syntax → evaluation plan |
| Parser (`define [relation]`, `define [rule]`) | ~80 | New bracket attributes |
| Primitives (assert!, retract!, query) | ~100 | Omni-facing API |
| **Total** | **~910** | |

**Dependencies:** LMDB (BSD, ~50KB compiled). Build from source like the other 5 libraries.

---

## Three-Tier Pattern Matching (PCRE2 eliminated)

Backreferences (`\1`, `\2`) are **context-sensitive** — they make regex matching NP-complete.
Pika is a **context-free** bottom-up DP engine with linear/polynomial guarantees.
These are fundamentally incompatible. Adding backreferences would break Pika's architecture.

**Dialectic reasoning (4 rounds):**

1. "Add semantic predicates" → fails: Pika parses R→L, capture group doesn't exist yet when
   backreference is encountered (it's to the left in the string, parsed later)
2. "Parse `\1` as `.*`, filter post-hoc" → fails: combinatorial explosion of spurious parse
   trees, destroys O(n³) guarantee, exponential memory
3. "Redirect to grammar DSL" → viable but ergonomically painful for simple cases
4. "Accept the theoretical boundary" → correct: backreferences are NP-complete, don't pretend
   a polynomial engine can handle them

**Resolution: three tiers, no PCRE2.**

```
Tier 1: re-match (fast, linear)     — 95% of use cases
         [0-9]+, \w+, (foo|bar), (?=...), [a-z]{3,5}
         Zero external deps, Pika-native

Tier 2: pika/grammar (powerful)     — 4.9% of use cases
         Recursive grammars, left recursion, structural parsing
         Handles what backreferences typically solve:

         ;; Match balanced quotes (would need \1 in PCRE)
         ;; Instead: match with Omni code
         (define (match-quoted s)
           (let (q (char-at s 0))
             (and (or (= q "'") (= q "\""))
                  (= q (char-at s (- (string-length s) 1))))))

Tier 3: Omni code (unlimited)       — 0.1% of use cases
         When you need context-sensitive matching, write a function.
         Multiple dispatch + Pika regex + string ops = anything.
```

**Why this beats PCRE2:**

| | PCRE2 via FFI | Three-tier (no PCRE2) |
|---|---|---|
| Dependencies | +1 C library (~500KB) | Zero new deps |
| Two regex engines | Confusing: which to use? | One engine, clear escalation |
| Catastrophic backtracking | PCRE2 can ReDoS | Pika always terminates |
| Learning curve | Two regex flavors | One system to learn |
| Maintenance | Two engines to maintain | One engine + grammar DSL |

---

### 9. Reader Dispatch and Symbol-as-Function

**Two syntax enhancements** that unlock ergonomics across contracts, Datalog, and pipelines.

#### `'symbol` as lookup function

When a quoted symbol is applied as a function, it performs a key lookup:

```lisp
('name (dict 'name "Alice" 'age 30))   ;; => "Alice"

;; Powerful with HOFs and pipelines
(map 'name people)                       ;; => ("Alice" "Bob")
(filter 'active? users)
(sort-by 'age people)

;; Works with Datalog results
(map 'age (query '((person ?name ?age _))))

;; Pipe-friendly
(|> people (filter 'active?) (sort-by 'age) (map 'name))
```

Implementation: ~10 lines in `jit_apply_value` — when applying a SYMBOL, treat as `(ref arg symbol)`.

**Why not `.name` accessor lambdas:** `.name` conflicts visually with `person.name` field access.
`person.name.city` vs `.name.city` — same characters, different meanings. Confusing.
`'name` has zero ambiguity: it's always a quoted symbol, never a field access.

#### `#` reader dispatch extensions

| Syntax | Meaning | Example |
|--------|---------|---------|
| `;` | Line comment | `; this is a comment` |
| `#\| ... \|#` | Block comment (nestable) | `#\| multi-line \|#` |
| `#_` | Skip next form | `#_ (ignored) (this runs)` |
| `#N_` | Skip next N forms | `#3_ (a) (b) (c) (this runs)` |
| `#{}` | Set literal | `#{"red" "green" "blue"}` |
| `#r"pat"` | Compiled Pika regex | `#r"[0-9]+"` |

**Set literals** complete the collection trinity:
```lisp
'(1 2 3)          ;; list
[1 2 3]           ;; array
{'a 1 'b 2}       ;; dict
#{"a" "b" "c"}    ;; set (desugars to (set "a" "b" "c"))
```

**Regex literals** make Pika regex first-class:
```lisp
;; #r"" compiles regex at read time, no string escaping needed
(re-match #r"[0-9]+" "abc123")      ;; vs (re-match "[0-9]+" "abc123")
(re-split #r"\s+" "hello world")    ;; vs (re-split "\\s+" "hello world")

;; Particularly valuable for complex patterns
(re-match #r"(\w+)\s*=\s*(\w+)" "key = value")
```

**Multi-form comment** `#N_` — the number comes BEFORE `_` to avoid ambiguity with `#_ 3`:
```lisp
;; Disable a function and its test
#2_ (define (broken x) ...)
     (assert (= (broken 1) 2))

;; Disable one branch of a cond
(cond
  (< x 0) "negative"
  #1_ (= x 0) "zero"       ;; disabled: skip 1 form (the "zero" string)
  true "non-negative")
```

**Block comments** nest — safe to comment out code containing block comments:
```lisp
#|
  This whole section is disabled.
  #| Even nested comments |# are fine.
  (define (unused) ...)
|#
```

**Implementation:** ~80 lines in parser (reader dispatch table for `#`).

---

## Tier 3: Nice to Have

### 10. stb_image — Image loading

Single-header image decoder. Relevant given omni-torch exists. Public domain, ~100KB.

### 11. HTTP/1.1 client

Build on libuv TCP + BearSSL TLS. Parse with Pika grammar.
Alternative: libcurl via FFI if building from scratch is too slow.

### 12. Concurrency Model — Fibers, Scheduler, Threads

Omni's concurrency builds entirely on existing primitives (coroutines + effects).
No new concepts — just composition.

**Three tiers, increasing isolation:**

```
Coroutines    — cooperative, same thread, share everything (zero copy)
Fibers        — cooperative, same thread, scheduler manages (zero copy)
Threads       — parallel, isolated Interp, share NOTHING (copy flat data only)
```

#### Tier 1: Coroutines (LANDED)

Manual cooperative multitasking. `coroutine`/`resume`/`yield`. StackCtx-based.
User explicitly controls scheduling. Good for generators, lazy sequences, streams.

#### Tier 2: Fibers = Coroutines + Scheduler Effect Handler

A fiber is a coroutine whose `resume` is called by the scheduler, not the user.
The scheduler is an effect handler wrapping user code:

```lisp
;; spawn creates a coroutine, registers it with the scheduler
(spawn (lambda () (tcp-read (tcp-connect "a.com" 80))))

;; Under the hood:
;; 1. spawn = create coroutine + add to scheduler's ready queue
;; 2. tcp-connect signals io/tcp-connect effect
;; 3. Scheduler handler catches it, starts libuv async op, suspends fiber
;; 4. libuv callback fires → scheduler marks fiber as ready
;; 5. Scheduler resumes fiber with the result via resolve
```

Scheduler loop (~150 lines C3):
```
while fibers exist:
    while ready_queue not empty:
        resume next fiber
        if it signaled I/O effect → start libuv op, keep suspended
    uv_run(loop, UV_RUN_ONCE)   ;; poll libuv for completions
    ;; callbacks push fibers back to ready_queue
```

No new types. A fiber IS a coroutine. The scheduler IS an effect handler.

#### Tier 3: Threads = Fibers on OS Threads (share-nothing)

Each thread gets its own `Interp` + scope-region (complete isolation).
Communication via effects — the handler manages a lock-free inter-thread queue.
`uv_thread_create` + `uv_async_t` (cross-thread wakeup) from libuv.

**Critical design: send DATA, not closures.**

Copying closures across threads is heavy (env chain + all captured bindings = O(n)).
Instead, send flat data (int, string, symbol, list of scalars). Each thread loads
the same source code, so all functions are already defined in every Interp:

```lisp
;; LIGHT — send a symbol + an int (O(1) copy)
(signal io/thread-send (cons target '(compute-fib 40)))
;; Receiving thread already has compute-fib, just calls it

;; NOT THIS — copying a closure means copying the entire env chain
;; (signal io/thread-send (cons target (lambda () (compute big-captured-data))))
```

**No built-in channels.** Inter-thread communication goes through effects.
The "channel" is a queue inside the effect handler — not a language primitive.
Users who want channel abstractions write a handler:

```lisp
;; Channel = effect handler managing a queue
(define (make-channel)
  (let (queue (array))
    (dict 'send (lambda (msg) (signal io/chan-send (cons queue msg)))
          'recv (lambda () (signal io/chan-recv queue)))))
```

**Why no channels as primitives:**
- Effects already provide the suspend/resume mechanism
- A channel is just a queue + two effects (send/recv)
- Baking it in adds a concept where composition suffices
- Different channel types (buffered, unbuffered, broadcast) = different handlers

**What this model provides (all via composition):**

| Pattern | How |
|---------|-----|
| Message passing | Effect handler + queue |
| Future/Promise | Spawn fiber, await via effect |
| Fork/Join | Spawn N fibers, collect via effects |
| Map-reduce | Scatter data to threads, gather via effects |
| Pipeline | Chain of effect handlers |
| Supervision | Parent handler monitors child fibers |

**What's NOT needed (share-nothing eliminates):**
- Locks/mutexes — no shared mutable state
- STM — no shared state to transact on
- Atomics — no shared memory to CAS on (except the inter-thread queue, handled in C)

**Implementation:**

| Component | Lines (est.) | Phase |
|-----------|-------------|-------|
| Fiber scheduler (effect handler + libuv poll loop) | ~150 C3 | E |
| `spawn` / `await` primitives | ~50 C3 | E |
| Thread spawn (`uv_thread_create` + per-thread Interp init) | ~100 C3 | G |
| Inter-thread message queue (lock-free, `uv_async_t` wakeup) | ~80 C3 | G |
| Cross-thread value copy (flat data only, no closures) | ~50 C3 | G |

---

## Recommended Adoption Order

```
Phase A (DONE):    libuv + utf8proc + Pika + yyjson + libdeflate + BearSSL
                   Effect fast-path dispatch table
                   ↓
Phase B (next):    Reader dispatch (#r"", #{}, #N_, #| |#) + symbol-as-function
                   ~90 lines parser, pure syntax — no new deps
                   ↓
Phase C:           Contracts (define [schema]) — ~300 lines, pure Omni + dispatch
                   ↓
Phase D:           LMDB integration + Datalog engine (define [relation], define [rule])
                   ↓
Phase E:           Fiber scheduler (coroutines + effect handler + libuv event loop)
                   ↓
Phase F:           HTTP/1.1 client (libuv + BearSSL + Pika grammar)
                   ↓
Phase G:           Threads (uv_thread_create + per-thread Interp + effect-mediated messaging)
                   ↓
Phase H (extras):  stb_image
```

Phase B is pure parser work. Phase C is pure Omni + dispatch. Phase D adds LMDB (~50KB).
Phase E is ~200 lines C3 (scheduler + spawn/await). Phase F composes existing libraries.
Phase G is ~230 lines C3 (threads + messaging). No new external dependencies after Phase D.
PCRE2 eliminated — three-tier pattern matching covers all use cases.

---

## Dependency Summary

```
INTEGRATED:
  libuv (MIT)          ── TCP/DNS/timers, future async scheduler
  utf8proc (MIT)       ── Unicode normalization, case, graphemes
  Pika engine (built-in) ── regex, PEG grammar
  yyjson (MIT)         ── JSON parse/emit
  BearSSL (MIT)        ── TLS client (no-malloc design)
  libdeflate (MIT)     ── gzip/deflate compression

PLANNED:
  LMDB (OpenLDAP PL)  ── embedded B+ tree DB for Datalog storage

EXISTING:
  GNU Lightning (LGPL) ── JIT code generation
  libffi (MIT)         ── FFI calling convention
  replxx (BSD-2)       ── REPL
  libclang (Apache-2)  ── bindgen
```

All MIT/BSD/PD/OpenLDAP — no license conflicts.

---

## Integration Architecture

```
User-facing Omni code
        |
    signal effect (io/read-file, io/tcp-connect, io/tls-read, ...)
        |
    ┌── Handler installed? ──→ handler clause runs, may resolve
    │
    └── No handler ──→ fast-path dispatch table
                       │
                       lookup FastPathEntry[tag] → raw primitive
                       │
                       arity 0: call(nil)
                       arity 1: call(arg)
                       arity 2+: curry via cons pair
                       │
                       C3 primitive → extern fn to C library → result as Omni Value
```

For contracts: `validate`/`explain` are pure Omni dispatch, no effects involved.
For Datalog: `query`/`assert!` go through LMDB extern fns, results as Omni lists.
