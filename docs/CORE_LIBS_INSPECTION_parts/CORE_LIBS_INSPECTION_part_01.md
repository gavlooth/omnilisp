# CORE_LIBS_INSPECTION Part 01

Source: `docs/CORE_LIBS_INSPECTION.md`

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
| libdeflate    | gzip/deflate/zlib compression | MIT      | `src/lisp/compress.c3`         |
| libuv         | TCP/DNS/timers (linked, POSIX fast-path) | MIT | `src/lisp/async.c3`       |
| BearSSL       | TLS client + server wrap     | MIT      | `src/lisp/tls.c3` + `csrc/tls_helpers.c` |

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
Unicode-aware string-upcase/downcase/titlecase, string-normalize (NFC/NFD/NFKC/NFKD),
string-casefold, string-graphemes, string-codepoints, char-category, char-width,
char-property.
Case conversion no longer uses a fixed-size 1024-byte buffer; long and
multi-byte inputs are covered by regression tests.

### 3. Pika regex/grammar — INTEGRATED ✓
re-match, re-find-all, re-split, re-replace, re-fullmatch, re-match-pos, re-find-all-pos.
pika/grammar, pika/parse, pika/fold. `lisp_semantics.c3` disabled (AST mismatch).

### 4. yyjson — JSON ✓
json-parse (JSON → dict/array/string/int/double/nil), json-emit, json-emit-pretty,
and `json-get` (RFC-6901 pointer traversal with `~0`/`~1` unescape).
`json-parse` also supports explicit permissive mode flags for comments, trailing
commas, and `NaN`/`Inf` via an optional option list.

### 5. BearSSL — TLS ✓
tls-connect, tls-server-wrap (server-side wrap), tls-read, tls-write, tls-close.
Direct extern fn to BearSSL + C helper for socket callbacks and CA bundle parsing.
Current runtime verifies server certificates against trust anchors loaded from
`OMNI_TLS_CA_FILE` / `SSL_CERT_FILE` / system CA bundle paths, supports
optional client certificate auth (PEM cert/key), and exposes optional
in-process hostname session resumption policy on `tls-connect`.

### 6. libdeflate — Compression ✓
gzip, gunzip, deflate, inflate, zlib-compress, zlib-decompress.
`gzip`/`deflate`/`zlib-compress` support optional compression level `0..12`.
`adler32` and `crc32` expose checksum primitives.

---

## Tier 2: Data Integrity — Contracts + Datalog

### 7. Data-Driven Contracts (Malli-inspired)

**What:** Schema validation system where schemas are plain Omni data (lists + symbols).
Validates dicts, arrays, and values against structural schemas.

**Design lineage:** Malli (Clojure) → adapted for Omni's quoted symbols + multiple dispatch.

**What's kept from Malli:**
- Schemas are data, not macros — composable, introspectable, serializable
- Combinators: `and`, `or`, `not`, `maybe` (optional)
- Collection schemas: `map` (required/optional keys), `array`, `tuple`
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
(validate 'person (Dictionary 'name "Alice" 'age 30))
;; => true

;; Explain failures
(schema-explain 'person (Dictionary 'name "Alice" 'age -1))
;; => ((age "must satisfy (> 0)"))

;; Compose schemas
(define [schema] team
  (map
    (name string)
    (members (array-of person))))
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
| `(map ...)` | Dictionary with keys | `'(map (name string))` |
| `(array-of s)` | Array of schema | `'(array-of int)` |
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

**Deduce** — embedded relational Datalog for Omni. Named relations (not EAV triples),
bottom-up semi-naive evaluation, recursive rules, LMDB-backed persistence.
Query results are **lists of dicts** (keyed by variable name, `?` stripped).

**Design lineage:**

```
Malli ──→ data-driven schemas (kept)
Datalevin/Datomic ──→ LMDB backend (kept), EAV triples (DROPPED)
Soufflé ──→ relational model, bottom-up evaluation (adopted)

Result: Soufflé's query model + Datalevin's storage + Omni's define [attr] syntax
Name: Deduce (the engine deduces new facts from rules)
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

#### Deduce Grammar

**Unified API — single `deduce` primitive with quoted symbol dispatch:**

All Deduce operations go through `(deduce 'command args...)`. The first argument is always a quoted symbol selecting the operation.

**Database — LMDB-backed, always in-memory (mmap), optionally persistent:**
```lisp
;; Persistent — file on disk, survives restart
(define db (deduce 'open "app.db"))

;; Ephemeral — anonymous LMDB, same API, no durable file
(define tmp (deduce 'open 'memory))
```

LMDB is mmap-based: reads hit OS page cache (memory speed), writes flush to disk lazily.
No separate "in-memory mode" — LMDB already IS in-memory with persistence as side effect.

**Relations — column syntax:**

Columns reuse `define [type]` field syntax. Optional role prefix (`key`, `index`) before the type:

```lisp
;; Minimal — bare column names, no types
(define [relation db] edge (from to))

;; Typed columns — same as define [type] fields
(define [relation db] road (^String city1) (^String city2) (^Integer km))

;; With roles — role prefix before ^Type name
(define [relation db history] person
  (key ^String name)             ;; role + type + name
  (unique ^String email)         ;; enforced unique constraint
  (unique ^String dept)          ;; second unique marker joins the same composite constraint
  (ref users id ^Integer user-id) ;; referenced tuple must exist in users.id
  (index ^Integer age)           ;; indexed for fast range scans
  (^String city))                ;; no role, just typed

;; Parser: first token is ^ → plain column. First token is key/unique/ref/index → constrained.
```

| Column form | Meaning |
|-------------|---------|
| `(name)` | Untyped column |
| `(^String name)` | Typed column (same as type fields) |
| `(key ^String name)` | Primary key. Conflicting writes are rejected; identical full-tuple reasserts stay idempotent. |
| `(unique ^String email)` | Enforced unique constraint; multiple `unique` columns on one relation form one composite unique constraint. |
| `(ref users id ^Integer user-id)` | Enforced reference integrity: `user-id` must match an existing tuple in `users.id`. |
| `(index ^Integer age)` | Secondary B+ tree index. Fast range scans. |
| `(key index ^String name)` | Both key and indexed. |

**Relation-level attributes (in the `define` bracket):**

| Attribute | Syntax | Purpose |
|-----------|--------|---------|
| `db` | `[relation db]` | Which LMDB database to use |
| `history` | `[relation db history]` | Temporal: keep old versions. Enables `:at`/`:time` queries. |
| `schema` | `[relation db schema name]` | Contract validation on every `assert!`. |

**Facts — assert tuples into relations:**
```lisp
;; person is a first-class Relation value (from define [relation])
(deduce 'fact! person "Alice" 30 "alice@b.com")
(deduce 'fact! person "Bob" 25 "bob@b.com")
(deduce 'fact! edge "A" "B")
(deduce 'fact! edge "B" "C")
```

**Rules — derived relations (name, output vars, body clauses):**
```lisp
(define [rule] adult (?name ?age)
  (person ?name ?age _)
  (>= ?age 18))

;; Recursive — multiple defines = implicit OR (standard Datalog)
(define [rule] reachable (?a ?b)
  (edge ?a ?b))

(define [rule] reachable (?a ?b)
  (edge ?a ?mid)
  (reachable ?mid ?b))
```

**Query — special form, no quoting. Returns list of dicts (? stripped from keys):**
```lisp
;; All columns
(query (person ?name ?age _) (> ?age 28))
;; => ((Dictionary 'name "Alice" 'age 30 'email "alice@b.com"))

;; Projection — [...] selects which variables to return
(query [?name ?age]
  (person ?name ?age _)
  (> ?age 28))
;; => ((Dictionary 'name "Alice" 'age 30))

;; Synergy with 'symbol-as-function:
(|> (query [?name ?age] (person ?name ?age _) (> ?age 28))
    (map 'name))
;; => ("Alice")
```

**Negation — (not ...) as a clause (stratified):**
```lisp
(query [?name]
  (person ?name _ _)
  (not (edge ?name "BadNode")))
```

**Aggregation — aggregates in select, bare vars become group-by:**
```lisp
(query [(count ?name)]
  (adult ?name _))
;; => ((Dictionary 'count 5))

(query [?dept (avg ?salary) (max ?salary)]
  (employee ?name ?dept ?salary))
;; => ((Dictionary 'dept "engineering" 'avg 95000 'max 140000)
;;     (Dictionary 'dept "sales" 'avg 72000 'max 90000))
```

**Ordering — (order-by ?var 'asc/'desc) as final clause:**
```lisp
(query [?name ?age]
  (person ?name ?age _)
  (order-by ?age 'desc))
;; => ((Dictionary 'name "Alice" 'age 30) (Dictionary 'name "Bob" 'age 25))
```

**Temporal queries — when history is enabled on a relation:**
```lisp
;; Query state at a specific time
(query [?name ?age]
  (person ?name ?age _ :at "2025-06-01"))

;; Query all versions of a record
(query [?name ?age ?t]
  (person ?name ?age _ :time ?t)
  (= ?name "Alice"))
;; => ((Dictionary 'name "Alice" 'age 29 't "2025-01-01")
;;     (Dictionary 'name "Alice" 'age 30 't "2026-01-15"))
```

**Retract — with wildcard support:**
```lisp
(deduce 'retract! person "Alice" 30 "alice@b.com")  ;; exact match
(deduce 'retract! person "Alice" _ _)               ;; wildcard: all Alice records
```

#### Graph Encoding — Datalog's Sweet Spot

```lisp
(define [relation] edge (from to))
(define [relation] road (city1 city2 km))

;; Reachability — 2 rules
(define [rule] reachable (?a ?b) (edge ?a ?b))
(define [rule] reachable (?a ?b) (edge ?a ?mid) (reachable ?mid ?b))

;; Cycle detection
(define [rule] has-cycle (?node) (reachable ?node ?node))

;; Shortest path (weighted + aggregation)
(define [rule] path (?a ?b ?cost) (road ?a ?b ?cost))
(define [rule] path (?a ?b ?cost)
  (road ?a ?mid ?c1)
  (path ?mid ?b ?c2)
  (= ?cost (+ ?c1 ?c2)))

(query [?dest (min ?km)]
  (path "Athens" ?dest ?km))

;; Social graph
(define [rule] fof (?a ?c)
  (edge ?a ?b)
  (edge ?b ?c)
  (not (= ?a ?c)))

(define [rule] mutual (?a ?b)
  (edge ?a ?b)
  (edge ?b ?a))

(query [?who] (reachable "A" ?who))
;; => ((Dictionary 'who "B") (Dictionary 'who "C"))
```

#### Contracts + Deduce Integration

Schemas validate data before insertion:
```lisp
(define [schema] person-valid
  (map
    (name string)
    (age (and int (> 0)))
    (email (re "^[^@]+@.+"))))

;; assert! can check schema before writing to LMDB
;; (configurable per-relation)
```

Query results are dicts — validatable with the same contract system:
```lisp
(map (lambda (row) (validate 'person-valid row))
     (query [?name ?age ?email] (person ?name ?age ?email)))
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

**Deduce implementation:**

| Component | Lines (est.) | Notes |
|-----------|-------------|-------|
| LMDB extern fn (C3 native, no wrapper) | ~30 | `mdb_env_*`, `mdb_txn_*`, `mdb_cursor_*` |
| Relation storage + attributes | ~250 | Tuple encoding, LMDB indices, key/index/schema/history handling |
| Unification engine | ~150 | `?var` pattern matching with logic variables |
| Semi-naive evaluator | ~200 | Bottom-up fixpoint with stratified negation |
| Query compiler | ~150 | `query` special form → evaluation plan |
| Dictionary result builder | ~50 | Query results as dicts (strip `?` from var names) |
| Aggregation engine | ~100 | `count`, `sum`, `avg`, `min`, `max` + group-by |
| Temporal (history/`:at`/`:time`) | ~80 | Version tracking with timestamps, temporal query clauses |
| Parser (`[relation]`, `[rule]`, `query`, `?var`) | ~120 | Bracket attrs + relation attribute clauses + `?` reader char |
| Unified `deduce` dispatcher (`'open`, `'fact!`, `'retract!`, `'scan`, `'query`, `'count`, `'match`) | ~100 | Single primitive with quoted symbol dispatch |
| **Total** | **~1230** | |

**Storage model:** LMDB is always the backend (mmap = in-memory speed, disk persistence
as side effect). `(deduce 'open 'memory)` for ephemeral use (anonymous mmap, no file).
No separate in-memory engine — LMDB already IS in-memory.

**Parser changes needed:**
- `?` as reader prefix for logic variables → `T_LOGIC_VAR` token type
- `define [relation]` → `E_DEFRELATION` expr tag
- `define [rule]` → `E_DEFRULE` expr tag (name, output vars list, body clauses)
- `query` → special form, treats body as pattern data (like `match`), `[...]` = projection

**Dependencies:** LMDB (BSD, ~50KB compiled). Build from source like the other 5 libraries.

---
