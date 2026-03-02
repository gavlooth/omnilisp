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

## Tier 3: Nice to Have

### 9. stb_image — Image loading

Single-header image decoder. Relevant given omni-torch exists. Public domain, ~100KB.

### 10. HTTP/1.1 client

Build on libuv TCP + BearSSL TLS. Parse with Pika grammar.
Alternative: libcurl via FFI if building from scratch is too slow.

### 11. Async scheduler (effect-based)

Full libuv event loop integration with fiber scheduler. The scheduler is an effect
handler wrapping user code — fibers signal I/O effects, scheduler dispatches to libuv,
libuv callbacks resume fibers via resolve. ~150 lines C3.

### 12. PCRE2 — Regex fallback

For patterns the Pika engine can't handle (backreferences, recursive patterns).
FFI binding to PCRE2. ~95% of patterns covered by built-in Pika engine.

---

## Recommended Adoption Order

```
Phase A (DONE):    libuv + utf8proc + Pika + yyjson + libdeflate + BearSSL
                   Effect fast-path dispatch table
                   ↓
Phase B (next):    Contracts (define [schema]) — ~300 lines, pure Omni + dispatch
                   ↓
Phase C:           LMDB integration + Datalog engine (define [relation], define [rule])
                   ↓
Phase D:           Async scheduler (effect handler over libuv event loop)
                   ↓
Phase E:           HTTP/1.1 client (libuv + BearSSL + Pika grammar)
                   ↓
Phase F (extras):  stb_image, PCRE2
```

Phase B is pure Omni code (no new C dependencies). Phase C adds LMDB (~50KB).
Phase D is ~150 lines C3. Phase E composes existing libraries.

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
  PCRE2 (BSD)          ── regex fallback for backreferences

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
