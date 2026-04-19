# CORE_LIBS_INSPECTION Part 03

Source: `docs/CORE_LIBS_INSPECTION.md`

## Recommended Adoption Order

```
Phase A (DONE):    libuv + utf8proc + Pika + yyjson + libdeflate + BearSSL
                   Effect fast-path dispatch table
                   ↓
Phase B (next):    Reader dispatch (#r"", #N_, #| |#) + symbol-as-function
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
