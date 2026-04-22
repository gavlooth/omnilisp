# Omni Error Model Migration Matrix

Status: Complete — all raw `raise_error` call sites migrated to `raise_error_with_payload_names` (~1013/1013)
Last updated: 2026-04-22
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

| Family | Representative APIs | Current style (2026-03-09) | Evidence |
|--------|----------------------|------------------------------|----------|
| Search/lookup absence | `find` | `nil` on not-found | `stdlib/stdlib.lisp` |
| Regex primitives | `re-match`, `re-fullmatch`, `re-find-all`, `re-replace` | `nil` for no-match paths; canonical `raise` for bad args | `src/pika/lisp_pika.c3` |
| Pika grammar/primitives | `pika/grammar`, `pika/parse`, `pika/fold`, `pika/parse-lisp` | canonical `raise` for invalid args/grammar failures; `nil` for parse absence | `src/pika/lisp_pika.c3` |
| I/O/network async | `tcp-*`, `dns-resolve`, `async-sleep` | canonical payloaded `raise` for core async primitives (`io/*`) | `src/lisp/async.c3` |
| Effect runtime internals | `signal`, `resolve`, strict/unhandled paths | canonical payloaded `raise` (`runtime/*`) | `src/lisp/jit_jit_handle_signal.c3`, `src/lisp/jit_jit_runtime_effects.c3` |
| Deduce relation/query | `__define-relation`, `deduce-query` family | canonical payloaded `raise` (`deduce/*`) | `src/lisp/deduce_schema_query.c3`, `src/lisp/deduce_relation_ops.c3`, `src/lisp/unify.c3` |
| String/number conversion | `read-string`, `Symbol`, `String` constructor family | canonical payloaded `raise` (`type/arg-mismatch`, `runtime/read-string-failed`) | `src/lisp/prim_string_convert.c3` |
| Compression | `gzip`, `gunzip`, `deflate`, `inflate` | canonical payloaded `raise` (`type/*`, `runtime/*`) | `src/lisp/compress.c3` |
| Stdlib error conventions | `try`, `assert!` | canonical payload normalization in stdlib surface (`{ 'code 'message 'domain 'data }`) | `stdlib/stdlib.lisp` |
| FFI surface | `ffi-callback`, `foreign-describe`, `foreign-release`, `foreign-import`, `foreign-resolve`, `define [ffi lib]`, `define [ffi λ]` | canonical payloaded `raise` (`ffi/*`) | `src/lisp/prim_ffi_callback.c3`, `src/lisp/foreign_runtime_core.c3`, `src/lisp/eval_ffi_eval.c3` |
| HTTP client | `http-get`, `http-request` | canonical payloaded `raise` (`io/*`) | `src/lisp/http.c3` |
| JSON parser | `json-parse`, `json-get` | canonical payloaded `raise` (`parser/*`) | `src/lisp/json.c3` |
| Collection mutations | `ref`, `push!`, `keys`, `values`, `has?`, `remove!`, `set!`, `set-add`, `set-remove`, `set-contains?` | canonical payloaded `raise` (`type/*`) | `src/lisp/prim_collection_generic_set.c3` |

## 3. APIs Intentionally Remaining `nil` (P2.3)

These remain `nil`-returning by contract because they model `absence`.

| API | Absence meaning | Keep `nil`? |
|-----|------------------|------------|
| `find` | no matching element | yes |
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
| Stdlib `try`/`assert!` base | `stdlib` | `done` | `try` normalizes caught raise payloads; `assert!` emits canonical payloads | enforce canonical payload keys | normalized in `stdlib/stdlib.lisp`; regression coverage includes payload shape, domain/code/message, and string-raise normalization |
| Regex match/search primitives | `pika` | `done` | canonical payloaded `raise` for bad args and malformed patterns; `nil` for valid no-match paths | `nil` for no match only; `raise` for bad args/pattern failures | malformed-pattern signaling is now deterministic across `re-*` primitives (`CP-03`) |
| Pika grammar APIs | `pika` | `done` | canonical payloaded `raise` for invalid args/grammar-not-found/invalid grammar forms; `nil` for parse absence | canonical `raise` payload | `lisp_pika.c3` print-and-nil control flow removed for programmer/recoverable failures |
| File I/O effect wrappers | `runtime-io` | `done` | canonical payloaded `raise` with stable wrapper codes (`io/read-file-*`, `io/write-file-*`, `io/file-exists?-*`, `io/read-lines-*`) | canonical payload shape normalized | missing-path/error-path coverage now asserts canonical codes per wrapper (`CP-04`) |
| Async/network primitives | `runtime-io` | `done` | canonical payloaded `raise` for `tcp-*`, `dns`, `async-sleep`, and `tls-*` wrappers | canonical payloaded `raise` | TLS wrapper migration completed with stable `io/tls-*` payload codes (`CP-05`) |
| Scheduler runtime primitives | `runtime-io` | `done` | canonical payloaded `raise` in scheduler runtime paths | canonical payloaded `raise` | `offload`, `thread-*`, `spawn`/`await`, wakeup/tcp-read bridge migrated to `scheduler/*` |
| Effect dispatcher internals | `runtime-core` | `done` | canonical payloaded `raise` across effect dispatch/handle/checkpoint/capture/fast-path internals | payloaded `raise` for programmer/recoverable paths | `jit_jit_handle_signal.c3`, `jit_jit_runtime_effects.c3`, and related continuation/effect compile helpers now avoid mixed string-only error constructors (`CP-06`) |
| Deduce APIs | `deduce` | `done` | canonical payloaded `raise` with stable `deduce/*` codes for open/dispatch/relation/query/fact/retract/count/scan/scan-range/match/txn/clear/drop | canonical payloaded `raise` | deduce-family string raises removed from `deduce.c3`, `deduce_relation_ops.c3`, `deduce_schema_query.c3`, and `unify.c3` |
| Conversion/compression primitives | `runtime-core` | `done` | canonical payloaded `raise` for conversion (`String`, `Float64`, `Integer`, `Symbol`, `read-string`) and compression/checksum (`gzip`, `gunzip`, `deflate`, `inflate`, `zlib-*`, `adler32`, `crc32`) families | canonical payloaded `raise` | argument paths now emit `type/arity` / `type/arg-mismatch`; compression operational failures emit stable `runtime/*` codes with regression coverage for domain/code extraction |
| Absence APIs (`find`) | `stdlib` | `done` | `nil` for no result | keep as-is | contract-aligned already |
| FFI callback primitives | `ffi` | `done` | canonical payloaded `raise` with stable `ffi/*` codes | canonical payloaded `raise` | `prim_ffi_callback.c3` migrated to `ffi_raise` helper; regression coverage for foreign-release finalizer path |
| FFI runtime core | `ffi` | `done` | canonical payloaded `raise` with stable `ffi/*` codes | canonical payloaded `raise` | `foreign_runtime_core.c3` migrated to `ffi_raise` helper |
| FFI library/function evaluation | `ffi` | `done` | canonical payloaded `raise` with stable `ffi/*` codes | canonical payloaded `raise` | `eval_ffi_eval.c3` migrated to `ffi_raise` helper |
| HTTP client | `io` | `done` | canonical payloaded `raise` with stable `io/*` codes | canonical payloaded `raise` | `http.c3` migrated to `http_raise` helper |
| JSON parser | `parser` | `done` | canonical payloaded `raise` with stable `parser/*` codes | canonical payloaded `raise` | `json.c3` migrated to `json_raise` helper |
| Collection generic mutations | `type` | `done` | canonical payloaded `raise` with stable `type/*` codes | canonical payloaded `raise` | `prim_collection_generic_set.c3` migrated to `collection_raise` helper |
| FFI bound call | `ffi` | `done` | canonical payloaded `raise` with stable `ffi/*` codes | canonical payloaded `raise` | `eval_ffi_bound_call.c3` migrated to `ffi_raise` helper |
| JSON emit | `parser` | `done` | canonical payloaded `raise` with stable `parser/*` codes | canonical payloaded `raise` | `json_emit.c3` migrated to `json_raise` helper |
| UI FTXUI helpers | `ui` | `done` | canonical payloaded `raise` with stable `ui/*` codes | canonical payloaded `raise` | `prim_ui_ftxui_helpers.c3` migrated to `ftxui_raise` helper |
| UI FTXUI primitives | `ui` | `done` | canonical payloaded `raise` with stable `ui/*` codes | canonical payloaded `raise` | `prim_ui_ftxui.c3` migrated to `ftxui_raise` helper |
| Boundary promotion/escape/copy | `boundary` | `done` | canonical payloaded `raise` with stable `boundary/*` codes | canonical payloaded `raise` | `eval_promotion_*` and `eval_boundary_*` families migrated to `boundary_raise` helper |
| JIT apply/compile/eval | `jit` | `done` | canonical payloaded `raise` with stable `runtime/*` codes | canonical payloaded `raise` | `jit_apply_*.c3`, `jit_compile_*.c3`, `jit_eval_*.c3`, `jit_closure_*.c3`, `jit_define_method_table.c3`, `jit_dispatch_helpers.c3`, `runtime_backend_hooks.c3` migrated to `jit_raise` helper |
| String/unicode operations | `string` | `done` | canonical payloaded `raise` with stable `type/*` / `runtime/*` codes | canonical payloaded `raise` | `unicode.c3`, `unicode_case_mapping.c3`, `prim_string_*.c3` migrated to `string_raise` helper |
| Data format parsing | `data-format` | `done` | canonical payloaded `raise` with stable `parser/*` / `runtime/*` codes | canonical payloaded `raise` | `primitives_data_formats*.c3`, `primitives_toml_bridge.c3`, `json_pointer_options.c3` migrated to `data_format_raise` helper |
| Runtime primitives | `runtime` | `done` | canonical payloaded `raise` with stable `runtime/*` codes | canonical payloaded `raise` | `threads.c3`, `primitives_coroutine*.c3`, `value_constructors_core.c3`, `prim_system.c3`, `schema*.c3`, `eval_dispatch_*.c3`, `eval_pattern_matching.c3`, `eval_type_*.c3`, `eval_signal.c3`, `tests_memory_lifetime_*.c3`, `tests_harness_helpers.c3`, `tests_runtime_feature_jit_groups_failures.c3` migrated to `runtime_raise` helper |
| Tensor operations | `tensor` | `done` | canonical payloaded `raise` with stable `tensor/*` codes | canonical payloaded `raise` | `prim_tensor_*.c3`, `value_tensor_constructors.c3`, `value_tensor_expr_edges.c3`, `value_predicates_accessors_basic.c3` migrated to `tensor_raise` / `tensor_runtime_raise` helpers |
| Big numeric types | `big-numeric` | `done` | canonical payloaded `raise` with stable `type/*` / `runtime/*` codes | canonical payloaded `raise` | `value_big_float.c3`, `value_big_complex.c3`, `value_big_integer.c3` migrated to `big_float_raise`, `big_complex_raise`, `big_integer_raise` helpers |
| Math primitives | `math` | `done` | canonical payloaded `raise` with stable `type/*` / `runtime/*` codes | canonical payloaded `raise` | `prim_math.c3`, `prim_math_arithmetic.c3`, `primitives_core.c3` migrated to `math_raise` helper |
| I/O file primitives | `io` | `done` | canonical payloaded `raise` with stable `io/*` codes | canonical payloaded `raise` | `prim_io_file.c3`, `prim_io.c3` migrated to `io_raise` helper |

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

Compatibility wrappers (`P2.5`) are retired for stdlib `try`/`assert!`; the
stdlib surface now guarantees canonical payload dictionaries at handler
boundaries.

Doc updates completed in this slice (`P2.8`):
- `docs/ARCHITECTURE.md` contract ADR
- `docs/EFFECTS_SEMANTICS.md` normative semantics and test anchors
- `docs/LANGUAGE_SPEC.md` links and glossary
- this matrix (`docs/ERROR_MODEL.md`)

Pending implementation work:
- no open migration rows in the current matrix; keep adding regression anchors when new API families are introduced
