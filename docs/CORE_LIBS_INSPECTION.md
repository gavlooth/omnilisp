# Core Libraries Inspection — Making Omni Usable

## Current State

Omni has a solid runtime (JIT, fibers, effects, scope-region memory, FFI) but almost no I/O
or data processing capabilities beyond `print`/`println` and file `load`. To write real programs,
users need: file I/O, networking, text processing, data interchange, and string correctness.

### Already integrated

| Library      | Role                      | License      |
|-------------|---------------------------|--------------|
| GNU Lightning | JIT code generation       | LGPL-3.0     |
| libffi       | FFI calling convention    | MIT          |
| replxx       | REPL (highlighting, history) | BSD-2     |
| libclang     | Bindgen (`--bind`)        | Apache-2.0   |

### Unwired stubs in codebase

- `tests/syntax_suite.omni:108-111` — `(define [grammar simple] [greeting "hello"])` parsed but no runtime
- `tests/unwired_features.omni:26-27` — `pika-match` primitive referenced but not registered
- These suggest an early plan for a PEG grammar DSL built into the language

---

## Tier 1: Makes Omni a Real Language

### 1. libuv — Async I/O, networking, filesystem, processes

**What it gives you:** Cross-platform event loop, TCP/UDP sockets, filesystem ops, DNS,
child process spawning, timers, signals, pipes, TTY handling.

**Why:** Without this, Omni can't read files (beyond `load`), open sockets, spawn processes,
or do anything beyond computation + print. This is the single biggest gap.

**Integration with Omni's architecture:**

```
Omni user code (synchronous-looking)
        |
    signal :read / :write / :connect / :spawn
        |
    effect handler (stdlib or runtime)
        |
    libuv request (non-blocking)
        |
    fiber suspended ──→ scheduler runs other fibers
        |                       |
    libuv callback fires ←──────┘
        |
    fiber resumed with result
```

The key insight: libuv's callback model + Omni's fiber/effect model = Go-style concurrency.
Users write blocking-looking code, the runtime multiplexes fibers over a single libuv event loop.

**Required glue (~200-300 lines C):**
- Scheduler: round-robin ready queue + `uv_run(UV_RUN_NOWAIT)` polling
- Bridge: effect handler that maps `:read`/`:write`/`:connect` to `uv_fs_*`/`uv_tcp_*`
- Fiber wake: libuv callbacks push fibers back onto ready queue

**Scope interaction:** libuv manages its own memory (malloc/free internally). Omni Values
wrapping libuv handles (uv_tcp_t, uv_fs_t) should be allocated in root_scope with destructors
that call `uv_close()`. Fits the existing FFI_HANDLE pattern.

**Risk:** libuv's event loop wants to be the "main loop." For the REPL, this means integrating
replxx's fd with libuv's polling (doable — replxx exposes its fd). For scripts, the main thread
runs `uv_run(UV_RUN_DEFAULT)` after kicking off the first fiber.

| Attribute    | Value                    |
|-------------|--------------------------|
| License     | MIT                      |
| Maturity    | 15+ years, Node.js core  |
| Portability | Linux, macOS, Windows, BSDs, others |
| API size    | ~150 functions (only need ~30 for core I/O) |
| Conflicts   | Event loop ownership — need scheduler integration |

**Alternatives considered:**
- **libev** — Lighter, but Linux/Unix only. No Windows. Less maintained.
- **io_uring** — Highest performance, but Linux 5.1+ only. Would need a fallback.
- **Raw POSIX** — Portable enough for Linux/macOS but you'd reimplement half of libuv.

**Verdict:** Use libuv. It's the standard answer for a reason.

---

### 2. utf8proc — Unicode string correctness

**What it gives you:** UTF-8 validation, normalization (NFC/NFD/NFKC/NFKD), case folding,
grapheme cluster iteration, character category lookup, bidirectional text properties.

**Why:** Omni's strings are byte arrays. `(length "café")` returns 5 (bytes), not 4 (characters).
`(string-upcase "straße")` can't work correctly without Unicode tables. Any string comparison
or sorting is wrong for non-ASCII text.

**Integration:**
- `string-length` dispatches: byte-length (current) vs char-length (utf8proc) vs grapheme-length
- `string-upcase`/`string-downcase` use `utf8proc_map` with UPPERCASE/LOWERCASE flags
- `string-normalize` exposes NFC/NFD normalization
- Comparison: `string<?` uses `utf8proc_iterate` for codepoint comparison

**Scope interaction:** utf8proc returns malloc'd buffers from `utf8proc_map`. Wrap in Omni
STRING values with destructors that free the buffer. Straightforward.

| Attribute    | Value                    |
|-------------|--------------------------|
| License     | MIT                      |
| Maturity    | Julia's core Unicode lib |
| Portability | Pure C, no deps          |
| Size        | ~200KB compiled (Unicode tables) |
| Conflicts   | None                     |

**Alternatives:**
- **ICU** — Full Unicode but ~30MB. Overkill.
- **Manual UTF-8 decoding** — Gets iteration right but not normalization, case folding, or categories.

**Verdict:** Use utf8proc. Tiny, no deps, covers 99% of Unicode needs.

---

### 3. Regex / PEG Parser — ALREADY IMPLEMENTED in `pika.bak/`

**A complete PEG-based regex/parser engine already exists in the codebase.** It was backed up
when the project renamed from Pika to Omni but is fully written C3, designed for this runtime.

#### What exists (`pika.bak/`, ~170KB, 13 files):

| File | What |
|------|------|
| `structs.c3` | Core types: Clause (tagged union), Match, Grammar, ParserState, PikaQueue |
| `clauses.c3` | Clause properties: is_terminal, child_clauses, epsilon analysis, do_match_clause |
| `grammar.c3` | `make_grammar` — topo sort, epsilon propagation, seed computation |
| `parse.c3` | **Pika parser core** — right-to-left DP scan, splay tree memo, `parse_lex` |
| `memo.c3` | Splay tree for match memoization |
| `queue.c3` | Min-heap priority queue |
| `traverse.c3` | Parse tree traversal |
| `regex.c3` | **PEG-based regex** — compiles PCRE-like patterns to PEG grammars internally |
| `frontend.c3` | RuleBuilder for grammar construction API |
| `lisp_pika.c3` | **Lisp bindings** — re-match, re-find-all, re-split, re-replace, re-fullmatch |
| `lisp_grammar.c3` | Grammar DSL — define PEG grammars from Omni code |
| `lisp_semantics.c3` | Semantic actions on parse trees |

**Regex features already supported:**
- Literals, `.`, `*`, `+`, `?`, `{n,m}` quantifiers
- Character classes: `[abc]`, `[a-z]`, `[^...]` (negated)
- Shorthand: `\d`, `\D`, `\w`, `\W`, `\s`, `\S`
- Possessive quantifiers: `*+`, `++`, `?+` (PEG-native, no backtracking)
- Groups: `(...)`, `(?:...)` non-capturing
- Lookahead: `(?=...)` positive, `(?!...)` negative
- Anchors: `^`, `$`
- Alternation: `a|b`

**Lisp API already written:**
```lisp
(re-match "[0-9]+" "abc123def")     ;; => "123"
(re-find-all "[a-z]+" "foo 42 bar") ;; => ("foo" "bar")
(re-split "\\s+" "hello  world")    ;; => ("hello" "world")
(re-replace "\\d+" "v1.2" "X")     ;; => "vX.X"
(re-fullmatch "[a-z]+" "hello")    ;; => "hello"
```

**PEG grammar API already written:**
```lisp
(pika/grammar rules...)   ;; define grammar from Omni
(pika/parse grammar input) ;; parse and return tree
(pika/fold grammar input fn) ;; parse and fold over tree
```

#### Work needed to reintegrate:

1. **Move from `pika.bak/` to `src/pika/`** — rename module if needed
2. **Update to current Omni APIs** — `alloc_value` → scope-aware allocation, register
   primitives via current `register_primitives` pattern
3. **Adapt memory** — regex/grammar allocations should use scope regions (parse state is
   temporary, grammar objects are long-lived). Memoization tables are a natural fit for
   scope-per-parse.
4. **Wire the `[grammar]` syntax** — parser already recognizes it, just needs runtime dispatch
5. **Add capture groups** — the regex engine compiles to PEG which doesn't naturally capture;
   `lisp_pika.c3` has capture extraction but it may need polish
6. **Test against current test suite** — ensure no regressions

#### PCRE2 via FFI — keep as fallback option

For patterns the PEG engine can't handle (backreferences, recursive patterns), PCRE2
can be added later via FFI. But the built-in engine covers ~95% of real-world regex use.
The advantage of the built-in engine: no external dependency, PEG-native possessive
quantifiers, grammar composability, scope-friendly memory.

---

## Tier 2: Enables Real-World Applications

### 4. yyjson — JSON

**What:** Fastest C JSON parser. Zero-copy, read-only mode fits scope allocation perfectly.

**Integration:**
```lisp
(json-parse "{\"name\": \"Alice\", \"age\": 30}")
;; => (dict "name" "Alice" "age" 30)    — native Omni dict

(json-emit (dict "x" 1 "y" 2))
;; => "{\"x\":1,\"y\":2}"
```

Parse into a child scope, build Omni dict/array/string values, promote to parent, release
the yyjson document. Perfect fit for scope-region model.

| License | MIT | Maturity | High — used in many projects |
|---------|-----|----------|------------------------------|
| Size    | ~50KB compiled | Conflicts | None        |

---

### 5. BearSSL — TLS/Crypto (preferred over mbedtls)

**Why BearSSL over mbedtls:** BearSSL is designed for constant-time operations and minimal
dynamic allocation. Its "no-malloc" design philosophy aligns with Omni's region-based memory.
You provide buffers, BearSSL fills them.

**Integration:** Wrap TLS contexts as FFI_HANDLE values. Use with libuv's TCP streams.

```lisp
(let (conn (tls-connect "example.com" 443))
  (tls-write conn "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n")
  (println (tls-read conn)))
```

| License | MIT | Size | ~100KB compiled |
|---------|-----|------|-----------------|
| Allocation model | No-malloc (caller provides buffers) | Conflicts | None |

---

### 6. sqlite3 — Embedded Database

**Why:** The universal embedded database. Public domain, zero-config, single-file.

**Integration:** Database handle as FFI_HANDLE with destructor calling `sqlite3_close`.
Prepared statements as handles. Results as Omni lists.

```lisp
(let (db (db-open "app.db"))
  (db-exec db "CREATE TABLE IF NOT EXISTS users (name TEXT, age INT)")
  (db-exec db "INSERT INTO users VALUES (?, ?)" "Alice" 30)
  (let (rows (db-query db "SELECT * FROM users"))
    (for-each println rows)))
```

| License | Public domain | Maturity | 25+ years |
|---------|---------------|----------|-----------|
| Size    | ~1MB compiled | Conflicts | None    |

---

## Tier 3: Nice to Have

### 7. libdeflate — Compression

Faster than zlib (3-4x decompression). For gzip/deflate support in HTTP responses
and file compression. MIT license, ~50KB.

### 8. stb_image — Image loading

Single-header image decoder. Relevant given omni-torch exists. Public domain, ~100KB.

### 9. libcurl — HTTP client (ALTERNATIVE to raw libuv+BearSSL)

If building HTTP/1.1 on libuv+BearSSL is too much work initially, libcurl provides a
complete HTTP client. Downside: callback-heavy API that's awkward with fibers.
MIT-like license. Consider this a fallback if the libuv+BearSSL HTTP path is too slow
to implement.

---

## Recommended Adoption Order

```
Phase A (foundation):  libuv + scheduler integration
                       ↓
Phase B (text):        utf8proc + reintegrate pika.bak/ (regex + PEG grammar)
                       ↓
Phase C (data):        yyjson (JSON)
                       ↓
Phase D (network):     BearSSL + HTTP on libuv
                       ↓
Phase E (storage):     sqlite3
                       ↓
Phase F (extras):      libdeflate, stb_image, PCRE2 (fallback for backrefs)
```

Phase A is the critical path — everything else can be built on top of it. Phase B gives
Omni string correctness and its distinctive grammar/regex feature (already 90% written).
Phase C makes Omni practical for data processing scripts. Phase D enables network
applications. Phase E adds persistence.

---

## Dependency Summary

```
libuv (MIT)          ── async I/O, networking, FS, processes
utf8proc (MIT)       ── Unicode normalization, case, graphemes
PCRE2 (BSD)          ── POSIX-style regex (via FFI)
PEG engine (built-in) ── grammar DSL, structural parsing
yyjson (MIT)         ── JSON parse/emit
BearSSL (MIT)        ── TLS, crypto (no-malloc design)
sqlite3 (PD)         ── embedded database
libdeflate (MIT)     ── compression
stb_image (PD)       ── image loading
```

All MIT/BSD/PD — no license conflicts with Omni's existing LGPL-3.0 (GNU Lightning) dependency.

---

## Integration Architecture

All libraries integrate through the same pattern:

```
User-facing Omni primitive (e.g., file-read, json-parse, regex-match)
        |
    Dispatched primitive or effect signal
        |
    C3 primitive function (prim_file_read, prim_json_parse, etc.)
        |
    FFI call to C library OR built-in C3 implementation
        |
    Result wrapped as Omni Value (in appropriate scope)
```

For I/O operations (libuv): signal an effect → suspend fiber → libuv callback → resume fiber.
For pure computation (utf8proc, yyjson, PCRE2, PEG): call directly, return result.
For handles (sqlite3, BearSSL, PCRE2 compiled patterns): wrap as FFI_HANDLE with scope destructor.
