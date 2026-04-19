    - `(deduce 'clear! relation)`
    - `(deduce 'drop! relation)`
  - Extended unknown-command guidance to include `'clear!` and `'drop!`.
  - Added `Relation.dropped` state marker to prevent use-after-drop behavior.
- `src/lisp/deduce_relation_ops.c3`
  - Added `prim_deduce_clear(...)`:
    - bulk row clear via `mdb_drop(txn, dbi, 0)` with commit/error handling.
  - Added `prim_deduce_drop(...)`:
    - relation drop via `mdb_drop(txn, dbi, 1)`, and marks relation dropped on commit success.
  - Added dropped-relation guard checks in `fact!`, `retract!`, `count`, `scan`, and `scan-range`.
- `src/lisp/deduce_schema_query.c3`
  - Relation allocation now initializes `rel.dropped = false`.
  - Hardened `__define-relation` database-handle validation to require `deduce-db` handles.
  - Added dropped/db-open checks in `deduce-query`.
- `src/lisp/unify.c3`
  - Added stricter relation-handle validation and dropped/db-open checks in `deduce-match`.
- `src/lisp/tests_tests.c3`
  - Added regressions:
    - `clear!` empties rows but relation remains writable/queryable.
    - `drop!` invalidates relation operations (`count` errors after drop).
- `docs/reference/08-libraries.md`
  - Added `clear!` / `drop!` usage examples.
- `docs/plans/library-gaps-todo.md`
  - Marked Track F bulk relation drop/clear item complete.

### Validation
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-07: Scheduler Boundary Shared-Bytes Bridge Wiring

### Summary
Completed a targeted runtime hardening step for scheduler/offload boundaries by
routing shared-byte transport through explicit bridge helpers instead of
scattered direct handle operations.

### What changed
- `src/lisp/scheduler_state_offload.c3`
  - Added explicit shared-bytes boundary helpers:
    - `scheduler_publish_shared_bytes_owned`
    - `scheduler_publish_shared_bytes_copy`
    - `scheduler_publish_shared` (bridge-shape alias)
    - `scheduler_project_shared_bytes_view`
    - `scheduler_project_shared` (bridge-shape alias)
    - `scheduler_take_shared_bytes_owned`
    - `scheduler_release_shared`
    - `scheduler_publish_sendable_int`
    - `scheduler_consume_sendable_int`
  - Added `scheduler_set_offload_bytes_owned` to centralize bytes-result
    publication into `OffloadCompletion`.
- `src/lisp/scheduler_offload_worker.c3`
  - Routed worker byte input reads through `scheduler_project_shared`.
  - Routed worker bytes result publication through
    `scheduler_set_offload_bytes_owned`.
  - Routed integer sendable publication through `scheduler_publish_sendable_int`.
  - Preserved specific offload error reasons by avoiding generic request-failed
    overwrite when an earlier error is already set.
- `src/lisp/scheduler_wakeup_io.c3`
  - Routed scheduler-side bytes ownership transfer through
    `scheduler_take_shared_bytes_owned` and `scheduler_release_shared`.
  - Routed integer sendable consumption through `scheduler_consume_sendable_int`.
- `src/lisp/scheduler_primitives.c3`
  - Replaced direct shared release calls with `scheduler_release_shared` on
    boundary enqueue/rejection paths.
- `src/lisp/tests_tests.c3`
  - Updated boundary completion setup to publish bytes via
    `scheduler_publish_shared`.

### Validation
- `c3c build`
- `c3c build --sanitize=address`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-07: Deduce Explicit Transaction API (`begin`/`commit`/`abort`)

### Summary
Added explicit user-visible deduce transaction handles and wired transactional write usage
through unified `(deduce '...)` commands.

### What changed
- `src/lisp/deduce.c3`
  - Added `deduce-txn` handle type with deterministic finalizer-backed abort.
  - Added new dispatcher commands:
    - `(deduce 'begin db ['read|'write])`
    - `(deduce 'commit txn)`
    - `(deduce 'abort txn)`
  - Added strict deduce-local FFI handle name checking helper to avoid unsafe
    cross-handle casts.
- `src/lisp/deduce_relation_ops.c3`
  - Extended `(deduce 'fact! ...)` and `(deduce 'retract! ...)` to accept optional
    leading transaction handles:
    - `(deduce 'fact! relation ...)` (existing path, unchanged)
    - `(deduce 'fact! txn relation ...)` (new explicit transaction path)
    - `(deduce 'retract! relation ...)` (existing path, unchanged)
    - `(deduce 'retract! txn relation ...)` (new explicit transaction path)
  - Added transaction/relation database mismatch validation.
  - Added stricter relation handle-type validation on `count`/`scan`/`scan-range`.
- `src/lisp/tests_tests.c3`
  - Added regression coverage:
    - transaction commit visibility,
    - transaction abort rollback behavior,
    - transaction vs relation cross-database mismatch failure.
- `docs/reference/08-libraries.md`
  - Added deduce transaction usage examples.
- `docs/plans/library-gaps-todo.md`
  - Marked Track F explicit transaction API item complete.

### Validation
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-07: LMDB Range-Scan API for Deduce

### Summary
Added explicit bounded range scanning for deduce relations through LMDB cursor semantics.

### What changed
- `src/lisp/deduce_relation_ops.c3`
  - Added shared bounded scan helper that performs cursor range-seek and stop-at-upper-bound checks.
  - Added tuple bound parsing and encoding for `deduce 'scan-range relation lower upper`.
  - Added strict validation for bound-list arity and tuple/list shape.
  - Kept `deduce 'scan` behavior unchanged and now routes through the shared scan helper.
- `src/lisp/deduce.c3`
  - Added unified dispatch support for `'scan-range`.
  - Updated command help text and unknown-command error surface to include `'scan-range`.
- `src/lisp/tests_tests.c3`
  - Added coverage for bounded scan count behavior, empty-range behavior, and basic invalid bound validation.
- `docs/reference/08-libraries.md`
  - Documented the new `deduce 'scan-range` usage example.
- `docs/plans/library-gaps-todo.md`
  - Marked Track F LMDB range-scan item as complete.

### Validation
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-07: JSON Emit Writer Controls and Parse Matrix

### Summary
Expanded yyjson emit surface with writer/format controls and completed the parse-option
behavior matrix for permissive versus strict JSON parsing.

### What changed
- `csrc/json_helpers.c`
  - Added `omni_yyjson_mut_write_with_flags` for all writer paths.
  - Added `omni_yyjson_fp_to_fixed_flag` helper for fixed-point precision selection.
- `src/lisp/json.c3`
  - Added `json-emit`/`json-emit-pretty` optional option list parsing (arity `1-2`).
  - Implemented optional controls for:
    - `precision`/`fp-precision` (1..15 fixed-point),
    - `pretty`, `pretty-two-spaces`,
    - `escape-unicode`, `escape-slashes`, `allow-invalid-unicode`,
    - `allow-inf-and-nan`, `inf-and-nan-as-null`,
    - `newline-at-end`,
    - `fp-to-float`.
  - Added validation for unknown option keys and invalid value types.
- `src/lisp/eval_init_primitives.c3`
  - Updated primitive arities: `json-emit` and `json-emit-pretty` now accept optional options.
- `src/lisp/tests_tests.c3`
  - Added parse-option matrix tests for strict defaults and permissive combinations.
  - Added writer control tests for `json-emit` precision and flag behavior, including negative cases.
- `docs/reference/07-io-networking.md`
  - Documented emit options and permissive parse-matrix usage.
- `docs/reference/11-appendix-primitives.md`
  - Updated JSON emit arities to `1-2` and notes about writer options.
- `docs/plans/library-gaps-todo.md`
  - Marked remaining Track E advanced parity checklist items complete.

### Validation
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-07: TOML Parse Option Surface

### Summary
Added a documented parser-control surface for `toml-parse` so callers can control
UTF-8 validation behavior and get clearer validation errors for unsupported option
usage.

### What changed
- `csrc/toml_helpers.c`
  - Added `omni_toml_parse_with_options` wrapper that applies TOML parser options per-call
    (currently `check_utf8`) and restores the previous global parser state after parse.
- `src/lisp/primitives_data_formats.c3`
  - Added `TomlParseOptions` with `check-utf8` default `false`.
  - Added option-list parsing and validation for `toml-parse` (`1-2` arity).
  - Added error messages for unknown option keys and invalid boolean option values.
- `src/lisp/tests_tests.c3`
  - Added coverage for:
    - supported options list path,
    - unknown option key rejection,
    - invalid option value rejection.
- `docs/reference/11-appendix-primitives.md`
  - Updated `toml-parse` arity to `1-2` and documented the options list.
- `docs/plans/library-gaps-todo.md`
  - Marked TOML advanced control-surface item complete and updated the TOML status row.

### Validation
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- Targeted runtime smoke check: `LD_LIBRARY_PATH=/usr/local/lib ./build/main /tmp/omni_check_toml_opts.omni`

## 2026-03-07: CSV Emit Quote-Style/Line-Ending Adjustments

### Summary
Aligned CSV emission behavior and test expectations with the RFC-style output contract:
line endings are validated and emitted as actual control bytes, and quote-style handling
does not force quoting of fields that do not require it.

### What changed
- `src/lisp/primitives_data_formats.c3`
  - Updated `csv_emit_escaped_cell` so `quote-style 'always` now follows the same
    quoting trigger condition as minimal output (`delimiter`, `quote-char`, `\n`, `\r`),
    while still escaping embedded quote characters when quoting occurs.
- `src/lisp/tests_tests.c3`
  - Corrected CSV custom line-ending expected string to use actual `\r\n` bytes.

### Validation
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main /tmp/check_exprs.omni` (targeted CSV regressions: line ending, quote style, custom quote char)

## 2026-03-07: Track G CSV Emit/Parse Option Parity

### Summary
Expanded CSV primitives to support option-driven formatting/parsing controls, including
RFC-style row end handling toggles and direct emit controls for delimiter, line ending,
quote behavior, nil rendering, and strict parsing mode.

### What changed
- `src/lisp/primitives_data_formats.c3`
  - `csv-parse` now accepts:
    - delimiter override as string (existing format) or option list
    - `(strict true|false)` option (`true` default)
  - `csv-parse` lenient mode (`strict false`) tolerates otherwise strict RFC edge cases:
    - standalone `\r` line terminators
    - non-standard quote placement in unquoted fields
    - trailing characters after a closing quote
  - added helper support for CSV emit options:
    - `line-ending` (`"\n"`, `"\r"`, `"\r\n"`),
    - `quote-char` (single-character string),
    - `quote-style` (`'minimal`, `'always`, `'none`),
    - `nil-as` (string or symbol for nil values),
    - `strict` (default `true`).
  - `csv-emit` now uses options for delimiter/line endings and emits according to
    quote style and custom quote character.
  - strict mode now rejects equal delimiter/quote-char to prevent ambiguous output.
- `src/lisp/tests_tests.c3`
  - added CSV test coverage for strict-mode parsing, strict/lenient parity cases,
    and CSV emit option matrix (`line-ending`, `quote-style`, `nil-as`, `quote-char`).
- `docs/reference/11-appendix-primitives.md`
  - updated `csv-parse`/`csv-emit` documentation to include supported options.
- `docs/plans/library-gaps-todo.md`
  - marked CSV writer/strict-mode track item complete.

### Validation
- `c3c build`
- `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ./build/main`

## 2026-03-07: Track E JSON Pointer Query

### Summary
Added JSON Pointer-style traversal for parsed JSON values, including escape
decoding and direct path indexing into objects/arrays.

### What changed
- `src/lisp/json.c3`
  - added `json-get` primitive: `(json-get value pointer)` with 2-argument arity.
  - added RFC 6901 pointer token parsing with `~0` (`~`) and `~1` (`/`) unescaping.
  - added traversal behavior:
    - path `""` returns the input value (root).
    - `"/foo/bar"` and array index steps traverse dict/array values.
    - missing path elements return `nil`.
    - invalid pointer escapes or non-root malformed path return errors.
- `src/lisp/eval_init_primitives.c3`
  - registered `json-get` in regular primitive table.
- `src/lisp/compiler_primitive_variable_hash_table.c3`
  - added `json-get` (and `json-parse/json-emit/json-emit-pretty`) lookup entries for compiler path generation.
- `src/lisp/compiler_free_vars_utils.c3`, `src/lisp/compiler_let_set_flat.c3`
  - marked `json-get` as builtin primitive for inline compiler handling.
- `src/lisp/tests_tests.c3`
  - added unit coverage for object/array traversal, escaped keys, root path,
    missing-path `nil`, invalid pointer syntax.
- `src/lisp/tests_compiler_tests.c3`
  - added compile-path coverage for `json-get`.
- `docs/reference/07-io-networking.md`
  - added `json-get` usage example.
- `docs/reference/11-appendix-primitives.md`
  - added `json-get` to primitive table.
- `docs/CORE_LIBS_INSPECTION.md`
  - documented JSON pointer parity completion.
- `docs/plans/library-gaps-todo.md`
  - marked Track E JSON pointer item complete.

### Validation
- Build + full tests
  - `c3c build`
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-07: Track E JSON Parse-Option Parity

### Summary
Added permissive `json-parse` controls so callers can opt into yyjson non-standard
syntax modes without relaxing strict mode by default.

### What changed
- `src/lisp/json.c3`
  - added optional second argument to `(json-parse json-text [options])` with strict
    arity validation and option-list parsing.
  - added supported option symbols: `allow-comments`, `allow-trailing-commas`,
    `allow-nan-inf` (with aliases `comments`, `trailing-commas`,
    `nan-inf`, and `naninf`).
  - mapped parsed options into wrapper flags and passed through to `omni_yyjson_read`.
  - changed primitive arity registration expectation from fixed `1` to variadic (`-1`) with
    explicit argument validation in the primitive.
- `csrc/json_helpers.c`
  - extended `omni_yyjson_read` to accept explicit read flags and forward them to
    `yyjson_read`.
- `src/lisp/eval_init_primitives.c3`
  - updated `json-parse` registration from arity `1` to variadic (`-1`) to permit options.
- `src/lisp/tests_tests.c3`
  - added strict-mode regression coverage for comments/trailing comma/NaN defaults.
  - added success coverage for each permissive option mode.
- `docs/reference/07-io-networking.md`
  - documented permissive `json-parse` examples.
- `docs/reference/11-appendix-primitives.md`
  - updated `json-parse` arity metadata to `1-2`.
- `docs/CORE_LIBS_INSPECTION.md`
  - documented `json-parse` permissive mode support.
- `docs/plans/library-gaps-todo.md`
  - marked Track E parse-option subitems complete.

### Validation
- Planned: run after current patch batch:
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-07: TimePoint constructor and predicate primitives

### Summary
Added explicit `time-point` and `time-point?` primitives to create and inspect
`TIME_POINT` values directly in Lisp without going through TOML parsing.

### What changed
- `src/lisp/primitives_data_formats.c3`
  - added `prim_time_point` with kind-driven constructors:
    - `(time-point 'date year month day)`
    - `(time-point 'time hour minute second microsecond)`
    - `(time-point 'datetime year month day hour minute second microsecond)`
    - `(time-point 'datetime-tz year month day hour minute second microsecond tz-offset)`
  - added `symbol_matches` helper for kind dispatch.
- `src/lisp/primitives_meta_types.c3`
  - added `prim_time_point_p` (`time-point?`).
- `src/lisp/eval_init_primitives.c3`
  - registered `time-point` and `time-point?`.
- `src/lisp/compiler_primitive_variable_hash_table.c3`
  - added AOT hash entries for `time-point` and `time-point?`.
- `src/lisp/compiler_free_vars_utils.c3`
  - added to compiler primitive allowlists.
- `src/lisp/compiler_let_set_flat.c3`
  - added to built-in primitive detection list.
- `src/lisp/tests_tests.c3`
  - added end-to-end constructor/predicate coverage and error-path coverage.
- `src/lisp/tests_compiler_tests.c3`
  - added compile-path coverage for `(time-point 'date ...)`.
- `docs/reference/11-appendix-primitives.md`
  - added TimePoint primitive rows.
- `src/lisp/tests_tests.c3`
  - added regression coverage for `time-point` arity mismatch and `datetimetz` alias.
- `src/lisp/tests_compiler_tests.c3`
  - added compile-path coverage for `time-point` with `datetimetz` alias.

### Corrections
- Fixed `time-point` constructor for `'datetime-tz` to require the full 8 value arguments
  after kind and map `microsecond` and `tz-offset` correctly.

### Validation
- Added tests in runtime and compiler suites.

## 2026-03-07: TOML Datetime Formatting Adjustment

### Summary
Normalized `toml-parse` output for datetimes with explicit `+00:00` or `Z` offsets
so they round-trip as local-date-time strings when offset is zero:
`1979-05-27T07:32:00` instead of `1979-05-27T07:32:00+00:00`.

### What changed
- `src/lisp/primitives_data_formats.c3`
  - updated `toml_timestamp_to_string` to omit offset rendering when `tz == 0`
    in `TOML_DATETIMETZ` conversion path.

### Validation
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- Result: `Unified Tests: 1433 passed, 0 failed`

## 2026-03-07: Finwatch idiom expansion — 5 new modules, ~33 idioms showcased

### Summary
Extended finwatch example with 5 new modules demonstrating ~33 additional Omni Lisp idioms.
Fixed `procedure?` to include continuations. Discovered and documented several runtime bugs.

### New files
- `examples/finwatch/types.omni` — [abstract], [alias], type-of, is?, instance?, import 'as, conversions
- `examples/finwatch/analytics.omni` — sets, zip, flatten, find, remove, any?, every?, string-join, string-contains?
- `examples/finwatch/events.omni` — coroutine/resume/yield, reset/shift, iterators, delay/force, unless, assert!
- `examples/finwatch/rules.omni` — define [macro], gensym, eval/read-string, apply, macroexpand, quasiquote
- `examples/finwatch/alerts.omni` — define [effect], with-handlers, custom effect dispatch

### Modified files
- `src/lisp/primitives_meta_types.c3` — `procedure?` now returns true for CONTINUATION values
- `examples/finwatch/smoke_test.omni` — comprehensive tests for all new modules
- `examples/finwatch/TODO.md` — full idiom coverage checklist (75+ items checked)

### Runtime bugs discovered
- stream-yield macro: shift is parser-level, macros can't expand to it
- Nil/false corruption across module scope boundaries in recursive functions
- let-inside-define-inside-unless scoping failure
- Continuations are single-shot (multi-shot generator pattern doesn't work)

### Tests
- Before: 1306 unified, 0 failures
- After: 1407 unified + 76 compiler, 0 failures
- Smoke test: all 6 sections pass (original, types, analytics, events, rules, alerts)

## 2026-03-07: Session 263 - Track D5 `char-property`

### Summary
Completed Track D item 5 by adding `char-property` with symbol-keyed Unicode
metadata queries, plus regression coverage and doc parity.

### What changed
- runtime primitive
  - `src/lisp/unicode.c3`
    - added `(char-property codepoint property-symbol)` with supported keys:
      - `category`
      - `width`
      - `upper?`
      - `lower?`
      - `letter?`
      - `digit?`
      - `whitespace?`
      - `word?`
    - returns appropriate string/int/boolean values and explicit unknown-key
      error messaging.
- primitive registration
  - `src/lisp/eval_init_primitives.c3`
    - registered `char-property` with arity `2`.
- regression tests
  - `src/lisp/tests_tests.c3`
    - added coverage for:
      - category lookup
      - width lookup
      - boolean property truthy/nil behavior
      - unknown property error path
- docs + plan parity
  - `docs/reference/11-appendix-primitives.md`
    - added `char-property`.
  - `docs/reference/08-libraries.md`
    - added usage examples.
  - `docs/CORE_LIBS_INSPECTION.md`
    - updated utf8proc capability summary.
  - `docs/plans/library-gaps-todo.md`
    - marked `Track D / char-property` complete.

### Validation
- Build + full tests:
  - `c3c build`
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1407/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 262 - Track D4 `char-width`

### Summary
Completed Track D item 4 by adding `char-width` (utf8proc display-width hint)
with regression coverage and reference parity updates.

### What changed
- runtime primitive
  - `src/lisp/unicode.c3`
    - added extern binding `utf8proc_charwidth`.
    - added primitive `(char-width codepoint)` returning utf8proc width hint
      (`-1`, `0`, `1`, or `2`).
- primitive registration
  - `src/lisp/eval_init_primitives.c3`
    - registered `char-width` in the Unicode primitives section.
- regression tests
  - `src/lisp/tests_tests.c3`
    - added:
      - `char-width 65 -> 1`
      - non-int argument validation test.
- docs + plan parity
  - `docs/reference/11-appendix-primitives.md`
    - added `char-width`.
  - `docs/reference/08-libraries.md`
    - added `char-width` example.
  - `docs/CORE_LIBS_INSPECTION.md`
    - updated utf8proc capability list.
  - `docs/plans/library-gaps-todo.md`
    - marked `Track D / char-width` complete.

### Validation
- Build + full tests:
  - `c3c build`
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1401/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 261 - Track D3 `string-titlecase`

### Summary
Completed Track D item 3 by adding `string-titlecase` as a Unicode-aware
dispatched primitive with compiler/runtime parity and regression tests.

### What changed
- runtime primitive
  - `src/lisp/unicode.c3`
    - added `prim_string_titlecase_unicode`:
      - titlecases word starts (`utf8proc_totitle`) and lowercases interior
        letters (`utf8proc_tolower`) using Unicode category boundaries.
    - added small internal helpers for dynamic UTF-8 output buffer growth and
      word-codepoint classification.
- dispatch + compiler parity
  - `src/lisp/eval_init_primitives.c3`
    - increased dispatched primitive count `32 -> 33`.
    - registered `string-titlecase`.
  - `src/lisp/compiler_let_set_flat.c3`
  - `src/lisp/compiler_free_vars_utils.c3`
  - `src/lisp/compiler_primitive_variable_hash_table.c3`
    - added `string-titlecase` to builtin primitive detection/hash maps.
- regression tests
  - `src/lisp/tests_tests.c3`
    - added titlecase tests:
      - `"hello world" -> "Hello World"`
      - `"hELLO wORLD" -> "Hello World"`
- docs + plan parity
  - `docs/reference/11-appendix-primitives.md`
    - updated dispatched primitive count and added `string-titlecase`.
  - `docs/reference/08-libraries.md`
    - added titlecase example in Unicode section.
  - `docs/CORE_LIBS_INSPECTION.md`
    - updated utf8proc capability summary.
  - `docs/plans/library-gaps-todo.md`
    - marked `Track D / string-titlecase` complete.

### Validation
- Build + full tests:
  - `c3c build`
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1399/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 260 - Track D2 `string-casefold`

### Summary
Completed Track D item 2 by adding a Unicode-aware `string-casefold` primitive
using utf8proc casefold mapping and wiring compiler/runtime/doc parity.

### What changed
- runtime primitive
  - `src/lisp/unicode.c3`
    - added `prim_string_casefold_unicode`:
      - `(string-casefold s)` using `utf8proc_map` with
        `UTF8PROC_STABLE | UTF8PROC_CASEFOLD`.
- primitive dispatch registration
  - `src/lisp/eval_init_primitives.c3`
    - increased dispatched primitive count `31 -> 32`.
    - registered `string-casefold` as a dispatched string primitive.
- compiler primitive awareness
  - `src/lisp/compiler_let_set_flat.c3`
  - `src/lisp/compiler_free_vars_utils.c3`
  - `src/lisp/compiler_primitive_variable_hash_table.c3`
    - added `string-casefold` to builtin primitive detection/hash maps.
- regression tests
  - `src/lisp/tests_tests.c3`
    - added:
      - ASCII casefold test.
      - Unicode sharp-s casefold test (`"Straße" -> "strasse"`).
- docs + plan parity
  - `docs/reference/11-appendix-primitives.md`
    - updated dispatched primitive count and added `string-casefold`.
  - `docs/reference/08-libraries.md`
    - added Unicode casefold example.
  - `docs/CORE_LIBS_INSPECTION.md`
    - updated utf8proc capability summary.
  - `docs/plans/library-gaps-todo.md`
    - marked `Track D / string-casefold` complete.

### Validation
- Build + full tests:
  - `c3c build`
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1397/0`
    - `compiler`: `76/0`

## 2026-03-07: Session 259 - Track D1 Unicode Buffer Truncation Fix

### Summary
Completed Track D item 1 by removing the fixed 1024-byte cap in
`string-upcase`/`string-downcase` and switching to dynamically grown buffers.
Added long-string and multi-byte regression coverage (Track D test item).

### What changed
- unicode runtime implementation
  - `src/lisp/unicode.c3`
    - replaced fixed stack buffers (`char[1024]`) in:
      - `prim_string_upcase_unicode`
      - `prim_string_downcase_unicode`
    - added dynamic heap buffer growth with `mem::realloc` so conversion does
      not truncate long inputs.
    - preserved invalid UTF-8 fallback semantics (copy raw byte and continue).
- regression tests
  - `src/lisp/tests_tests.c3`
    - added long ASCII coverage:
      - upcase/downcase of 1500-character strings with full length checks.
    - added long multi-byte coverage:
      - upcase/downcase of repeated `é`/`É` inputs with codepoint-count checks.
- docs + plan parity
  - `docs/plans/library-gaps-todo.md`
    - marked complete:
      - `Track D / fixed 1024-byte truncation`
      - `Track D / long-string and multi-byte Unicode regression tests`
  - `docs/CORE_LIBS_INSPECTION.md`
    - updated utf8proc section to note dynamic-buffer behavior and regression
      coverage.

### Validation
- Build + full tests:
  - `c3c build`
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Results:
    - `stack_engine`: `21/0`
    - `scope_region`: `51/0`
    - `unified`: `1395/0`
    - `compiler`: `76/0`
