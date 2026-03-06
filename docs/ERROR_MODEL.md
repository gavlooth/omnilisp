# Omni Error Model Migration Matrix

Status: In progress
Last updated: 2026-03-06
Owner: Codex session workflow

Normative contract source:
- `docs/ARCHITECTURE.md` ADR-2026-03-06-A (`Effects-First Failure Contract`)
- `docs/EFFECTS_SEMANTICS.md` (effect dispatch/handler semantics)

## 1. Contract Summary

Failure classes:
1. `absence`
2. `recoverable-op-failure`
3. `programmer-error`
4. `internal-runtime-error`

Surface mapping:
- `absence` -> `nil` (or `false` for predicate APIs)
- `recoverable-op-failure` -> `signal raise` with canonical payload
- `programmer-error` -> `signal raise` with canonical payload
- `internal-runtime-error` -> hard runtime error

Canonical `raise` payload:

```lisp
{ 'code ... 'message ... 'domain ... 'data ... }
```

Omni has no dedicated keyword type; payload keys are quoted symbols.

## 2. Current Failure-Style Inventory (P2.1)

This inventory is grouped by API family and current observed style.

| Family | Representative APIs | Current style (2026-03-06) | Evidence |
|--------|----------------------|------------------------------|----------|
| Search/lookup absence | `find`, `assoc-ref` | `nil` on not-found | `stdlib/stdlib.lisp` |
| Regex primitives | `re-match`, `re-fullmatch`, `re-find-all`, `re-replace` | `nil` for no-match paths; canonical `raise` for bad args | `src/pika/lisp_pika.c3` |
| Pika grammar/primitives | `pika/grammar`, `pika/parse`, `pika/fold`, `pika/parse-lisp` | canonical `raise` for invalid args/grammar failures; `nil` for parse absence | `src/pika/lisp_pika.c3` |
| I/O/network async | `tcp-*`, `dns-resolve`, `async-sleep` | canonical payloaded `raise` for core async primitives (`io/*`) | `src/lisp/async.c3` |
| Effect runtime internals | `signal`, `resolve`, strict/unhandled paths | `make_error`/`raise_error` string path | `src/lisp/jit_jit_handle_signal.c3`, `src/lisp/jit_jit_runtime_effects.c3` |
| Deduce relation/query | `__define-relation`, `deduce-query` family | canonical payloaded `raise` (`deduce/*`) | `src/lisp/deduce_schema_query.c3`, `src/lisp/deduce_relation_ops.c3`, `src/lisp/unify.c3` |
| String/number conversion | `read-string`, `string->symbol`, `number->string` family | `raise_error` string path | `src/lisp/prim_string_convert.c3` |
| Compression | `gzip`, `gunzip`, `deflate`, `inflate` | `raise_error` string path | `src/lisp/compress.c3` |
| Stdlib error conventions | `try`, `assert!` | `signal raise` exists, but payload unconstrained (`^Any msg`) | `stdlib/stdlib.lisp` |

## 3. APIs Intentionally Remaining `nil` (P2.3)

These remain `nil`-returning by contract because they model `absence`.

| API | Absence meaning | Keep `nil`? |
|-----|------------------|------------|
| `find` | no matching element | yes |
| `assoc-ref` | key not present | yes |
| `re-match` | no match in input | yes (but invalid-arg/path failures must migrate) |
| `re-fullmatch` | full match not found | yes (same migration caveat) |
| `re-match-pos` | no match span | yes |
| `re-find-all` | zero matches | return empty list (not failure) |
| `pika/parse` | parse does not match grammar root | currently `nil`; acceptable as absence only when input is valid and grammar exists |

## 4. APIs Targeted for `signal raise` Migration (P2.4)

These are not absence and should migrate to canonical payloaded `raise`.

| API family | Failure class | Target `'domain` |
|------------|---------------|------------------|
| `pika/grammar` / `pika/parse` / `pika/fold` invalid args/grammar-not-found | programmer-error or recoverable-op-failure | `parser` / `regex` |
| I/O/network runtime primitives (`tcp-*`, `dns-resolve`, file I/O wrappers) | recoverable-op-failure + programmer-error | `io` |
| Deduce DB and relation operations | recoverable-op-failure + programmer-error | `deduce` |
| Effect runtime validation (`resolve` misuse, strict unhandled) | programmer-error | `runtime` |
| Conversion/compression argument/type checks | programmer-error | `type` / `runtime` |

## 5. Migration Matrix (P2.2)

Status legend: `done`, `partial`, `missing`.

| Family | Owner | Status | Current public behavior | Target behavior | Notes |
|--------|-------|--------|--------------------------|-----------------|-------|
| Stdlib `try`/`assert!` base | `stdlib` | `partial` | catches/raises with unconstrained payload | enforce canonical payload keys | compatibility wrappers landed: `raise->message`, `try-message` |
| Regex match/search primitives | `pika` | `partial` | canonical payloaded `raise` for bad args; `nil` for no-match paths | `nil` for no match only; `raise` for bad args/pattern failures | argument-validation migration completed; malformed-pattern signaling still partial |
| Pika grammar APIs | `pika` | `done` | canonical payloaded `raise` for invalid args/grammar-not-found/invalid grammar forms; `nil` for parse absence | canonical `raise` payload | `lisp_pika.c3` print-and-nil control flow removed for programmer/recoverable failures |
| File I/O effect wrappers | `runtime-io` | `partial` | effects-first surface exists | canonical payload shape not normalized | fast path already wired |
| Async/network primitives | `runtime-io` | `partial` | canonical payloaded `raise` for `tcp-*`/`dns`/`async-sleep` core primitives; residual wrappers still mixed | canonical payloaded `raise` | `async.c3` now emits `io/*`; remaining network wrappers (for example TLS path) still pending normalization |
| Scheduler runtime primitives | `runtime-io` | `done` | canonical payloaded `raise` in scheduler runtime paths | canonical payloaded `raise` | `offload`, `thread-*`, `spawn`/`await`, wakeup/tcp-read bridge migrated to `scheduler/*` |
| Effect dispatcher internals | `runtime-core` | `partial` | mixed message-string + payloaded `raise` paths | payloaded `raise` for programmer/recoverable paths | runtime effect/handle paths now emit canonical payloads; remaining non-migrated callsites pending |
| Deduce APIs | `deduce` | `done` | canonical payloaded `raise` with stable `deduce/*` codes for open/dispatch/relation/query/fact/retract/count/scan/match | canonical payloaded `raise` | deduce-family string raises removed from `deduce.c3`, `deduce_relation_ops.c3`, `deduce_schema_query.c3`, and `unify.c3` |
| Conversion/compression primitives | `runtime-core` | `missing` | string `raise_error` paths | canonical payloaded `raise` | many shallow argument checks |
| Absence APIs (`find`, `assoc-ref`) | `stdlib` | `done` | `nil` for no result | keep as-is | contract-aligned already |

## 6. Domain and Code Normalization Baseline (P2.7)

Planned stable domains:
- `io`
- `parser`
- `regex`
- `scheduler`
- `deduce`
- `type`
- `runtime`

Initial code taxonomy (examples):
- `io/not-found`, `io/permission-denied`, `io/invalid-handle`
- `parser/invalid-grammar`, `parser/no-root-match`
- `regex/invalid-pattern`, `regex/invalid-arg-type`
- `scheduler/invalid-handle`, `scheduler/timeout`
- `deduce/relation-not-found`, `deduce/txn-failed`
- `type/arg-mismatch`, `type/arity`
- `runtime/unhandled-effect`, `runtime/invalid-continuation`

## 7. Compatibility and Documentation Notes

Compatibility wrappers (`P2.5`) are partially implemented (`raise->message`,
`try-message`); additional API-family wrappers are still pending.

Doc updates completed in this slice (`P2.8`):
- `docs/ARCHITECTURE.md` contract ADR
- `docs/EFFECTS_SEMANTICS.md` normative semantics and test anchors
- `docs/LANGUAGE_SPEC.md` links and glossary
- this matrix (`docs/ERROR_MODEL.md`)

Pending implementation work:
- broader compatibility wrappers where migration is user-visible (`P2.5`) are now partially implemented (`raise->message`, `try-message`); remaining API-family-specific wrappers pending
- additional domain/code normalization coverage beyond runtime+scheduler+core-async+deduce+parser paths (`P2.7`)
- broader migrated API-family regressions (`P2.10`)
