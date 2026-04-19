### Validation
- Normal build: `/opt/c3/c3c build` succeeded.
- Normal suite: `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` succeeded.
  - `stack_engine`: 21 pass / 0 fail
  - `scope_region`: 51 pass / 0 fail
  - `unified`: 1301 pass / 0 fail
  - `compiler`: 73 pass / 0 fail
- ASAN build: `/opt/c3/c3c build --sanitize=address` succeeded.
- ASAN suite: `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` succeeded.
  - `stack_engine`: 20 pass / 0 fail (overflow recovery test skipped under ASAN)
  - `scope_region`: 51 pass / 0 fail
  - `unified`: 1300 pass / 0 fail
  - `compiler`: 73 pass / 0 fail

## 2026-03-06: Session 234 - Core Async I/O Error Payload Normalization (`io/*`)

### Summary
Continued Phase 2 migration with a non-scheduler I/O slice:
- migrated core async/network primitives in `async.c3` from string-only raises
  to canonical payloaded `raise` under `io/*` codes,
- added deterministic regressions for async I/O payload domain/code behavior,
- updated migration docs/plan status for this coverage.

### What changed
- `src/lisp/async.c3`
  - added `io_raise(...)` helper (domain fixed to `io`).
  - replaced all local `raise_error(...)` callsites with canonical
    `raise_error_with_payload_names(...)` via `io_raise`.
  - migrated failure paths for:
    - `tcp-connect`
    - `tcp-listen`
    - `tcp-accept`
    - `tcp-write`
    - `tcp-read`
    - `tcp-read-timeout`
    - `tcp-close`
    - `dns-resolve`
    - `async-sleep`
  - representative codes:
    - `io/tcp-connect-host-not-string`
    - `io/tcp-listen-bind-listen-failed`
    - `io/tcp-read-recv-failed`
    - `io/dns-resolve-resolution-failed`
    - `io/async-sleep-ms-not-integer`
- `src/lisp/tests_advanced_tests.c3`
  - added payload regressions for core async I/O:
    - `tcp-connect` payload domain (`io`) and code
    - `tcp-listen` deterministic bind/listen failure code
    - `dns-resolve` deterministic resolution-failure code
- `docs/ERROR_MODEL.md`
  - async/network row updated to reflect `async.c3` migration to `io/*`,
    with residual wrapper normalization still pending.
- `.claude/plans/effects-typesystem-parity-plan.md`
  - added progress note for completed core non-scheduler async I/O migration.

### Validation
- Normal build: `/opt/c3/c3c clean && /opt/c3/c3c build` succeeded.
- Normal suite: `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` succeeded.
  - `stack_engine`: 21 pass / 0 fail
  - `scope_region`: 51 pass / 0 fail
  - `unified`: 1297 pass / 0 fail
  - `compiler`: 73 pass / 0 fail
- ASAN build: `/opt/c3/c3c build --sanitize=address` succeeded.
- ASAN suite: `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` succeeded.
  - `stack_engine`: 21 pass / 0 fail
  - `scope_region`: 51 pass / 0 fail
  - `unified`: 1297 pass / 0 fail
  - `compiler`: 73 pass / 0 fail

## 2026-03-06: Session 233 - Scheduler Error Payload Normalization + ASAN Validation

### Summary
Continued Phase 2 execution from `.claude/plans/effects-typesystem-parity-plan.md`
with scheduler-domain error normalization:
- migrated scheduler runtime raise paths from string-only errors to canonical
  payloaded `raise` with `scheduler/*` codes,
- added scheduler payload regressions,
- updated migration docs/plan state,
- completed both normal and ASAN validation gates.

### What changed
- `src/lisp/scheduler_state_offload.c3`
  - added `scheduler_raise(...)` helper (domain fixed to `scheduler`).
  - migrated offload job parser failures to stable codes:
    - `scheduler/offload-invalid-job`
    - `scheduler/offload-op-not-symbol`
    - `scheduler/offload-invalid-sleep-ms`
    - `scheduler/offload-invalid-compress-payload`
    - `scheduler/offload-out-of-memory`
    - `scheduler/offload-invalid-accept-fd`
    - `scheduler/offload-unsupported-op`
- `src/lisp/scheduler_primitives.c3`
  - migrated public scheduler primitive failures to canonical payloaded raise:
    - `offload`, `thread-spawn`, `thread-join`, `thread-join-timeout`,
      `thread-cancel`, `fiber-cancel`, `spawn`, `await`.
  - introduced explicit code plumbing in helper validators/parsers so each
    path emits stable `scheduler/*` codes while preserving older messages.
- `src/lisp/scheduler_tcp_async_bridge.c3`
  - migrated async `tcp-read` bridge failures (OOM/libuv/probe misuse paths)
    to `scheduler/tcp-read-*` codes.
- `src/lisp/scheduler_wakeup_io.c3`
  - migrated wakeup consume/value paths for async `tcp-read` and offload
    completion handling to `scheduler/*` codes.
  - offload worker-provided error strings are now wrapped in canonical payloads
    using `scheduler/offload-worker-error`.
- `src/lisp/tests_advanced_tests.c3`
  - added payload regressions:
    - scheduler offload payload domain (`scheduler`)
    - scheduler offload unsupported-op code
    - scheduler thread-join-timeout code
- `docs/ERROR_MODEL.md`
  - migration matrix updated:
    - async/network row moved to `partial` with scheduler `tcp-read` note
    - scheduler runtime primitives row marked `done`
  - migration-wrapper note corrected (`raise->message`, `try-message`
    already partial-implemented).
- `.claude/plans/effects-typesystem-parity-plan.md`
  - added Phase 2 progress notes for completed runtime/scheduler slices.
  - marked validation gates:
    - `V3` done (`c3c build --sanitize=address`)
    - `V4` done (ASAN test suite run)
    - `V5` done (docs updated for behavior changes)
    - `A2.3` done (full suite green in normal + ASAN)

### Validation
- Normal build: `/opt/c3/c3c build` succeeded.
- Normal suite: `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` succeeded.
  - `stack_engine`: 21 pass / 0 fail
  - `scope_region`: 51 pass / 0 fail
  - `unified`: 1293 pass / 0 fail
  - `compiler`: 73 pass / 0 fail
- ASAN build: `/opt/c3/c3c build --sanitize=address` succeeded.
- ASAN suite: `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` succeeded.
  - `Unified Tests`: 1293 passed / 0 failed
  - `Compiler Tests`: 73 passed / 0 failed

## 2026-03-06: Session 232 - Effects Error Payload Helpers + Runtime Effect Migration Slice

### Summary
Executed the next pending Phase 2 migration slice from the effects/typesystem
parity plan:
- added canonical runtime helpers for structured `raise` payloads,
- introduced pending-payload transport across handler dispatch boundaries,
- migrated runtime effect/resolve/unhandled error paths to canonical payloaded
  `raise`,
- added regression tests for payload shape/codes on migrated effect paths,
- updated migration docs and plan status.

### What changed
- `src/lisp/value_constructors.c3`
  - added canonical payload helpers:
    - `make_raise_payload(...)`
    - `make_raise_payload_names(...)`
    - `raise_error_with_payload(...)`
    - `raise_error_with_payload_names(...)`
  - added shared pending-raise implementation (`raise_error_pending_impl`) used
    by `raise_error(...)` and payloaded raise paths.
  - payload schema now constructed as:
    `{ 'code <symbol> 'message <string> 'domain <symbol> 'data <dict|nil> }`.
- `src/lisp/value_interp_state.c3`
  - added `Interp.raise_payload` for pending structured raise transport.
  - runtime init now zeros `raise_msg` and `raise_payload`.
- `src/lisp/jit_jit_handle_signal.c3`
  - upgraded effect-runtime error paths (`unhandled`, strict-unhandled,
    argument type mismatch, suspend guard failure) to payloaded raise helpers.
  - `dispatch_pending_raise(...)` now forwards structured payload when present,
    with string fallback for older callers.
  - `preserve_raise_across_restore(...)` now preserves payload state as well as
    message bytes.
- `src/lisp/jit_jit_runtime_effects.c3`
  - migrated runtime raise sites (`reset/shift/resolve/handle` error paths) to
    canonical payloaded raise helpers with stable `runtime/*` codes.
- `src/lisp/eval_run_pipeline.c3`
  - stale raise scrub now clears `raise_payload`.
- `src/lisp/tests_tests.c3`
  - boundary-reset/stale-raise tests now also assert payload state is cleared.
- `src/lisp/tests_advanced_tests.c3`
  - added payload regressions for:
    - `resolve` misuse payload tag/domain/code
    - unhandled effect payload code
- `stdlib/stdlib.lisp`, `src/lisp/compiler_stdlib_prelude.c3`
  - added migration helpers:
    - `raise->message` (extract canonical payload message when present)
    - `try-message` (always forwards message string to user handlers)
- `docs/LANGUAGE_SPEC.md`
  - documented `raise->message` and `try-message` under Effect Utilities.
- `docs/ERROR_MODEL.md`
  - updated effect-dispatcher migration row from `missing` to `partial`
  reflecting this runtime slice.
- `.claude/plans/effects-typesystem-parity-plan.md`
  - marked `P2.6` done, `P2.10` partial, `A2.1` partial.
  - marked validation `V1`/`V2` done for this slice.

### Validation
- Build: `/opt/c3/c3c build` succeeded.
- Runtime tests: `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` succeeded.
- Summary:
  - `stack_engine`: 21 pass / 0 fail
  - `scope_region`: 51 pass / 0 fail
  - `unified`: 1290 pass / 0 fail
  - `compiler`: 73 pass / 0 fail

## 2026-03-06: Session 231 - Module Marker Syntax Canonicalization (`'as` / `'all`)

### Summary
Canonicalized module marker syntax away from colon-prefixed symbols and onto
quoted symbols:
- import rename marker is now `'as`
- import/export-from all marker is now `'all`
- parser diagnostics/docs/tests now explicitly reflect quoted marker syntax
- `:as` / `:all` module markers are rejected with targeted parser errors

### What changed
- `src/lisp/parser_import_export.c3`
  - parser now recognizes quoted markers for module forms:
    - `(import mod (sym 'as alias))`
    - `(import mod 'all)`
    - `(export-from mod 'all)`
  - added explicit rejection path for `:as` / `:all` markers in module
    syntax with clear diagnostics.
  - tightened trailing-specifier validation for `(import ...)` so invalid
    tokens after module name fail early with a targeted message.
- `src/lisp/value_interp_state.c3`
  - module marker symbols now intern `"as"` / `"all"` (quoted at syntax level)
    instead of `":as"` / `":all"`.
- `src/lisp/compiler_expr_serialize_exprs.c3`
  - serializer now emits `'as` / `'all` in import/export-from forms.
- `src/lisp/tests_advanced_tests.c3`
  - module-system tests updated to quoted marker syntax.
  - parser edge tests updated to quoted-marker diagnostics.
  - added explicit regressions that reject `:as` / `:all` module forms.
- `src/lisp/value_ast_effects.c3`, `src/lisp/value_expr_ast_core.c3`
  - inline syntax comments updated to quoted marker forms.
- `src/lisp/scheduler_primitives.c3`
  - non-behavioral C3 v0.7.10 migration fix for unsigned modulo divisor
    typing in round-robin offset calculation.
- Docs updated for drift alignment:
  - `docs/LANGUAGE_SPEC.md`
  - `docs/SYNTAX_SPEC.md`
  - `docs/FEATURES.md`
  - `docs/reference/05-macros-modules.md`
  - `CLAUDE.md` design-intent note now states no dedicated keyword type and
    quoted-symbol module markers.
- Plan update:
  - `.claude/plans/effects-typesystem-parity-plan.md` now includes a
    top-priority hotfix section for quoted module markers.

### Validation
- Build: `/opt/c3/c3c build` (v0.7.10) succeeded.
- Runtime tests: `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` succeeded.
- Summary:
  - `stack_engine`: 21 pass / 0 fail
  - `scope_region`: 51 pass / 0 fail
  - `unified`: 1284 pass / 0 fail
  - `compiler`: 73 pass / 0 fail

## 2026-03-06: Session 230 - Effects Semantics + Error Model Contract Docs (Phase 1 + Phase 2 Docs Slice)

### Summary
Completed the contract/spec documentation slice for the selected
effects/typesystem parity plan:
- finalized normative effects semantics document,
- added error-model migration matrix and API-family inventory,
- wired cross-links in language/effects docs,
- advanced plan checklist state for Phase 1 and Phase 2 doc tasks.

### What changed
- `docs/EFFECTS_SEMANTICS.md` (new)
  - normative MUST/SHOULD semantics for handler lookup, resolve/abort,
    unhandled effects, reset/shift interaction, and async boundary rules.
  - explicit Do/Don’t guidance for I/O and scheduler paths.
  - 10 executable examples mirrored to existing tests.
  - rule-to-test anchor table.
- `docs/ERROR_MODEL.md` (new)
  - effects-first failure contract summary and canonical payload schema.
  - runtime/stdlib API-family inventory by current failure style.
  - explicit nil-retention list (absence semantics only).
  - migration matrix with owner + status per API family.
  - domain/code normalization baseline.
- `docs/LANGUAGE_SPEC.md`
  - added cross-link from Effect Handlers section to
    `docs/EFFECTS_SEMANTICS.md`.
- `docs/EFFECTS_GUIDE.md`
  - added normative-reference callout to `docs/EFFECTS_SEMANTICS.md`.
- `.claude/plans/effects-typesystem-parity-plan.md`
  - marked Phase 1 checklist + acceptance complete.
  - marked Phase 2 documentation tasks complete/partial as appropriate.

### Validation
- Docs-only slice; no runtime code changes.
- Build/test commands were not run in this session.

## 2026-03-06: Session 229 - Pika Regex Parity Sessions 1-5 (Parser, Scanner Context, Cache)

### Summary
Completed Sessions 1-5 of the Pika regex parity plan:
- parser parity fixes
- regex compiler safety hardening
- large-pattern cap removal/clarification
- scanner context refactor (no fixed slot tables)
- bounded cache eviction with counters

### What changed
- `src/pika/clauses.c3`
  - added `SCAN_CTX` terminal handling path.
- `src/pika/structs.c3`
  - introduced `ClauseTag.SCAN_CTX` and `ScanCtxFn` support in `Clause`.
- `src/pika/frontend.c3`
  - added `mk_scan_ctx(...)` constructor.
- `src/pika/regex.c3`
  - tokenizer/compiler hardening:
    - strict malformed token handling and clearer trailing-token diagnostics
    - bounded quantifier validation and safer expansion logic
    - unbounded `{n,}` lowered via `many` instead of finite silent cap
    - trailing escape in char class now errors deterministically
  - removed fixed scan-slot table and switched char classes to `mk_scan_ctx(...)`.
  - added bounded cache policy (LRU-style eviction at `REGEX_CACHE_MAX_ENTRIES`)
    with counters (`hit/miss/eviction/fallback`).
  - added `regex_cache_reset()` to free all cached compiled regex entries and
    copied pattern buffers deterministically.
- `src/pika/lisp_pika.c3`
  - removed fixed 8-slot scan-function table in grammar compiler;
    scanner clauses now use `mk_scan_ctx(...)`.
- `src/pika/grammar.c3`, `src/pika/structs.c3`, `src/pika/clauses.c3`
  - parser parity cleanup from session 1:
    - `seeded_by` empty-seq guard
    - `memo_root` docs aligned with `-1` sentinel
    - unreachable-rule pruning contract enforced.
- `src/lisp/tests_tests.c3`
  - added regressions for:
    - strict invalid regex forms
    - long concatenation/alternation patterns
    - large bounded quantifier shapes
    - `>32` char classes in one regex
    - `pika/grammar` scan clause coverage, including beyond scanner cap
    - cache usability under growth/eviction stress
  - `run_test_global_boundary_reset()` now calls `pika::regex_cache_reset()`
    to keep test groups isolated from prior regex cache state.
- `.claude/plans/pika-regex-parity-plan.md`
  - updated checklist status through session 5 completion.
- `docs/LANGUAGE_SPEC.md`
  - added explicit Regex + Pika grammar section and semantics notes.

### Validation
- normal:
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: `Unified Tests: 1274 passed, 0 failed`; `Compiler Tests: 73 passed, 0 failed`.
- ASAN:
  - `c3c build --sanitize=address`
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - Result: `Unified Tests: 1275 passed, 0 failed`; `Compiler Tests: 73 passed, 0 failed`.

## 2026-03-06: Session 228 - Pika Regex Strict Grammar Validation Hardening

### Summary
Hardened `src/pika/regex.c3` to reject malformed patterns deterministically
instead of accepting partial parses or relying on token-position heuristics.
Added focused regressions for invalid regex grammar shapes.

### What changed
- `src/pika/regex.c3`
  - char-class range validation:
    - rejects descending ranges (e.g. `[z-a]`) with tokenizer error.
    - rejects empty char classes (e.g. `[]`).
  - bounded quantifier validation:
    - requires numeric lower bound after `{`.
    - requires numeric upper bound in `{n,m}` form.
    - rejects `{n,m}` when `m < n`.
    - malformed quantifiers now tokenize as `RE_ERROR` (not literal fallback).
  - escape validation:
    - trailing escape at end-of-pattern now sets tokenizer error.
  - compiler atom pushback fix:
    - replaced `tok.pos -= 1` with full-position restore (`saved_pos`) to avoid
      desync on multi-char tokens.
  - group validation:
    - missing `)` now marks `"Unclosed group"` tokenizer error in compile path.
  - full-consumption validation:
    - `regex_compile` now enforces end-of-pattern after optional `$`.
    - unexpected trailing tokens (for example unmatched `)`) invalidate the
      compiled regex with explicit error.

- `src/lisp/tests_tests.c3`
  - added strict-grammar regressions in `run_pika_tests`:
    - unmatched close paren: `a)`
    - unmatched open paren: `(ab`
    - leading quantifier: `*a`
    - bounded quantifier missing min: `a{,2}`
    - bounded quantifier descending: `a{3,2}`
    - empty char class: `[]`
    - invalid class range: `[z-a]`

### Validation
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- Result: all tests green in normal and ASAN runs.

## 2026-03-05: Session 227 - Deduce CRUD False Duplicate Fix

### Summary
Fixed a functional regression in `examples/deduce_crud_server.omni` where
`repo/create` could incorrectly return `"item already exists"` on an empty
relation, and tightened Deduce query predicate truthiness to match language
semantics.

### Root cause
- `repo/get-by-id` used `(car (deduce 'query ...))` directly, which produced an
  `ERROR` value when the query returned an empty list.
- In `(if existing ...)`, that `ERROR` value is truthy, so create-path logic
  incorrectly took the duplicate branch.
- `prim_deduce_query` accepted any non-`nil` predicate result as true, but Omni
  truthiness treats both `nil` and `false` as falsy.

### What changed
- `examples/deduce_crud_server.omni`
  - `repo/get-by-id` is now nil-safe:
    - returns `nil` for empty query result
    - returns `(car rows)` only when rows are present
- `src/lisp/deduce_schema_query.c3`
  - query filter now uses `!is_falsy(keep, interp)` instead of `keep.tag != NIL`
    so explicit `false` predicates are handled correctly.
- `src/lisp/tests_tests.c3`
  - added regression: `deduce 'query` with `(lambda (row) false)` returns zero.
  - added regression: load CRUD example and verify create/duplicate/get-by-id
    flow:
    - first create is `"ok"`
    - second create is duplicate error
    - missing id lookup returns `nil`
  - added HTTP pipeline regressions for CRUD example:
    - LF-only request decode fallback (`\n\n`)
    - POST/GET pipeline execute roundtrip via request maps
    - PUT/DELETE pipeline roundtrip with repo-state assertions
    - HTTP response formatting check from `http/response` + pipeline payload
    - schema gate check for invalid `item-write` payload
    - duplicate-id POST race via concurrent `spawn` + `pipeline/execute`
    - `/items/not-int` route degradation to route-not-found
    - `/unknown` route degradation to route-not-found
    - malformed request-line decode degradation to route-not-found

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- Result: `Unified 1240 passed, 0 failed`; `Compiler 73 passed, 0 failed`.

## 2026-03-05: Session 226 - `load` Crash Fixes (Parser + Relation Define Attr)

### Summary
Fixed a real script-mode crash (`EXIT:139`) when loading
`examples/deduce_crud_server.omni`, and added regression coverage for
`define [relation ...]` parser/lowering.

### Root cause
- `Parser.build_define_relation_call` in `src/lisp/parser_define_attrs.c3`
  emitted `E_CALL` using stale layout assumptions and wrote into
  `call.call.*` without first allocating `ExprCall`.
- Secondary parser hardening: multiple literal-string builder paths allocated
  exact-size buffers and then wrote a trailing NUL at `chars[len]`.

### What changed
- `src/lisp/parser_define_attrs.c3`
  - fixed relation-attr lowering to current call representation:
    - allocates `ExprCall`
    - sets `call.call.func` to `__define-relation`
    - passes args as `db`, relation symbol, column symbols
- `src/lisp/deduce_schema_query.c3`
  - `prim_define_relation` now accepts both shapes:
    - `(cons db '(name col...))`
    - direct `(__define-relation db 'name 'col...)`
  - added direct-arg column extraction with explicit symbol validation.
- String-literal builder safety hardening:
  - `src/lisp/prim_string_format.c3`: `strval_new` now reserves one extra byte
    for trailing NUL.
  - call sites switched to `strval_new(len + 1)` in:
    - `src/lisp/parser_expr_atoms.c3`
    - `src/lisp/parser_quasiquote_datum.c3`
    - `src/lisp/parser_pattern_match.c3`
    - `src/lisp/primitives_meta_types.c3`
- Example fix:
  - `examples/deduce_crud_server.omni`
    - normalized `or` to binary form in `util/third`
    - kept LF fallback in HTTP body split.
- Tests:
  - added `deduce [relation] define attr` regression in
    `src/lisp/tests_tests.c3`.

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- Result: `Unified 1228 passed, 0 failed`; `Compiler 73 passed, 0 failed`.

## 2026-03-05: Session 225 - TCP Server Primitives + Fiber CRUD Server Example

### Summary
Added server-side TCP primitives and effect fast paths, then built a full Omni
CRUD web server example using Deduce + `define [schema]` with fiber/effect
pipeline composition (no channels).

### What changed
- Runtime networking:
  - added `tcp-listen` and `tcp-accept` primitives in `src/lisp/async.c3`.
  - introduced explicit TCP handle modes (`TCP_STREAM`, `TCP_LISTENER`) so
    stream-only ops (`tcp-read`/`tcp-write`) reject listener handles.
  - `tcp-accept` is fiber-safe: in fiber context it offloads `accept()` and
    resumes on completion.
- Offload worker:
  - added offload operation `accept-fd` (`OFFLOAD_ACCEPT_FD`) to run blocking
    `accept()` on worker thread and return accepted fd.
  - files:
    - `src/lisp/scheduler_state_offload.c3`
    - `src/lisp/scheduler_offload_worker.c3`
- Primitive/effect registration:
  - added `__raw-tcp-listen`, `__raw-tcp-accept`.
  - added fast paths `io/tcp-listen`, `io/tcp-accept`.
  - file: `src/lisp/eval_init_primitives.c3`
- Stdlib effect wrappers:
  - added `io/tcp-listen`, `io/tcp-accept` declarations and wrappers
    `tcp-listen`, `tcp-accept`.
  - file: `stdlib/stdlib.lisp`
- Tests:
  - added async tests for listener lifecycle, invalid accept argument, and
    fiber/offload accept path end-to-end with loopback connect/write/read.
  - file: `src/lisp/tests_tests.c3`
- Example app:
  - added `examples/deduce_crud_server.omni`:
    - Deduce relation-backed CRUD
    - request/response schema validation via `define [schema]` + `validate`
    - namespaced pipeline stages and fiber request handlers
    - channel-free concurrency via `spawn` + `tcp-accept` + effects
- Docs:
  - added TCP server section to networking reference.
  - documented raw listener/accept primitives in primitive appendix.
  - files:
    - `docs/reference/07-io-networking.md`
    - `docs/reference/11-appendix-primitives.md`

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-05: Session 224 - Remove Constructor/Type Aliases (`Array`/`Dict`/`List`/`Set`, `typeof`)

### Summary
Reverted constructor/type alias exposure and standardized docs/tests back to the
canonical primitive names only.

### What changed
- Runtime primitives:
  - removed `List` alias for `list`
  - removed `Array` alias for `array`
  - removed `Dict` alias for `dict`
  - removed `Set` alias for `set`
  - removed `typeof` alias for `type-of`
  - file: `src/lisp/eval_init_primitives.c3`
- Tests:
  - removed alias-specific coverage for constructor/type aliases.
  - file: `src/lisp/tests_advanced_tests.c3`
- Docs:
  - reverted examples/tables to canonical names (`list`, `array`, `dict`,
    `set`, `type-of`) and removed alias mentions.
  - files:
    - `docs/LANGUAGE_SPEC.md`
    - `docs/reference/06-effects.md`
    - `docs/reference/11-appendix-primitives.md`
    - `docs/reference/12-appendix-stdlib.md`

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`

## 2026-03-05: Session 223 - Constructor/Type Alias Polish (`Array`/`Dict`/`List`/`Set`, `typeof`)

### Summary
Completed naming-style follow-up so constructor/type ergonomics match the
capitalized type-token model while preserving current behavior.

### What changed
- Primitive alias registration:
  - `List` -> `list`
  - `Array` -> `array`
  - `Dict` -> `dict`
  - `Set` -> `set`
  - `typeof` -> `type-of`
  - file: `src/lisp/eval_init_primitives.c3`
- Constructor dispatch polish:
  - `(list it)` now consumes iterators directly.
  - `(array it)` now consumes iterators directly.
  - files:
    - `src/lisp/primitives_core.c3`
    - `src/lisp/prim_collection_sort_array.c3`
- Tests:
  - added alias coverage for `Array`/`List`/`Dict`/`Set`.
  - added `typeof` alias coverage.
  - file: `src/lisp/tests_advanced_tests.c3`
- Docs:
  - language and references updated to document alias style and exact-type
    equality via symbols (`(= (type-of x) 'Type)`).
  - files:
    - `docs/LANGUAGE_SPEC.md`
    - `docs/reference/06-effects.md`
    - `docs/reference/11-appendix-primitives.md`
    - `docs/reference/12-appendix-stdlib.md`

### Validation
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `Unified: 1224 passed, 0 failed`
  - `Compiler: 73 passed, 0 failed`

## 2026-03-05: Session 222 - Constructor Dispatch for Iterators + Unified Infinite Sources

### Summary
Extended constructor and sequence dispatch so iterators can be forced through
core collection constructors directly, and aligned infinite source naming with
the unified collection API.

### What changed
- Runtime primitives:
  - `list` now accepts a single `Iterator` and consumes it to a list.
  - `array` now accepts a single `Iterator` and consumes it to an array.
  - Files:
    - `src/lisp/primitives_core.c3`
    - `src/lisp/prim_collection_sort_array.c3`
- Stdlib API unification follow-up:
  - added/expanded collection-dispatched variants for `List`/`Array`:
    - `reverse`, `take`, `drop`, `zip`, `foldl`, `foldr`
  - renamed iterator sources:
    - `irepeat` -> `repeat`
    - `icycle` -> `cycle`
  - file:
    - `stdlib/stdlib.lisp`
- Tests/docs:
  - migrated iterator forcing examples/tests from `collect`/`to-array` toward
    `(list it)` / `(array it)`,
  - added array/list dispatch coverage and explicit `foldr` coverage.
  - files:
    - `src/lisp/tests_advanced_tests.c3`
    - `docs/reference/06-effects.md`
    - `docs/reference/11-appendix-primitives.md`
    - `docs/reference/12-appendix-stdlib.md`
    - `docs/LANGUAGE_SPEC.md`

### Why this matters
- Removes unnecessary API surface for "forcing" iterators.
- Keeps one consistent mental model:
  collection constructors and collection functions dispatch by input type.
- Preserves laziness for iterator flows until explicitly materialized.

### Validation
