# Active TODO

Last condensed: 2026-04-11

This file is now the sole live backlog.
List only still-open items here.

Current actionable count: 1

Completed backlog snapshots:

- `docs/TODO-backup-2026-03-26.md`
- `docs/TODO-backup-2026-03-31.md`
- `docs/TODO-backup-2026-04-01.md`
- `docs/TODO-backup-2026-04-08.md`

Use this file only for still-open work.

## Live Queue

- [ ] `LANG-TENSOR-SCIENTIFIC-SURFACE-091` implement the canonical Tensor
  scientific-computing surface
  - design note: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`
    locks the proposed surface around `Tensor`, tensor-dispatched `map`,
    `contract`, and `materialize`.
  - next step: start with the plan's `TENSOR-010` runtime representation slice,
    then proceed through constructor/indexing, tensor-expression/materialize,
    `map`, and `contract` only after each slice has targeted tests.

## Recently Closed

- [x] `AUDIT-IMMER-FFI-COMPAT-101` retire the optional Immer bridge instead
  of extending FFI around it
  - closure evidence:
    - the owner clarified that C++/Immer support was not intended as core
      language infrastructure.
    - deleted the unsupported `lib/immer.omni` wrapper and the `lib/immer/`
      C++ bridge tree, including the tracked nested `lib/immer/immer` gitlink.
    - deleted the obsolete `docs/plans/immer-ffi-compat-plan-2026-04-11.md`
      compatibility plan.
    - no `^Value`, automatic value-handle, or pointer-only rewrite was added
      for this legacy optional library.
  - validation:
    - active source/reference search confirms no supported surface references
      remain outside historical TODO/changelog/plans.
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- [x] `AUDIT-LET-BRACKET-SHORTHAND-102` remove legacy outer `let [...]`
  shorthand from live library code
  - closure evidence:
    - `lib/core.omni` macro expansions now emit flat-pair `let` and named
      `let` binding lists.
    - `lib/test-utils.omni` now uses flat-pair `let` binding syntax.
    - the remaining `let [` text matches are only the syntax decision note and
      the negative parser regression that verifies the shorthand is rejected.
  - validation:
    - `rg "\(let\s*\[" -n lib stdlib tests examples docs src` returns only
      the syntax decision note and the negative parser regression.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --check lib/test-utils.omni`
    - bounded `basic` slice: `pass=142 fail=0`
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- [x] `AUDIT-IMMER-PERSISTENT-DISPATCH-088` harden persistent collection
  wrappers before adding generic dispatch
  - closure evidence:
    - `persistent-dictionary` now rejects odd key/value argument lists instead
      of silently dropping the final key.
    - persistent collection values are now tagged Omni wrapper dictionaries
      around the raw bridge handle, and public operations unwrap by expected
      family before calling the C++ bridge.
    - `persistent-array?`, `persistent-dictionary?`, and `persistent-set?`
      predicates now exist, while generic `count`/`conj`/`into` dispatch
      remains intentionally frozen.
    - residual runtime bridge compatibility is split into
      `AUDIT-IMMER-FFI-COMPAT-101`.
    - superseded by `AUDIT-IMMER-FFI-COMPAT-101`, which retires the optional
      Immer bridge entirely instead of adding FFI machinery around it.
  - historical validation before the bridge was retired:
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --check lib/immer.omni`
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - runtime preload remains blocked by `AUDIT-IMMER-FFI-COMPAT-101`
      because `ffi-declare` is no longer bound.
    - bridge build remains blocked locally: `make -C lib/immer test_bridge`
      fails on missing `immer/flex_vector.hpp`.

- [x] `AUDIT-LIST-HELPER-ALIAS-086` keep lowercase `list` as an approved public
  helper
  - closure evidence:
    - `List` remains the canonical constructor/conversion surface.
    - lowercase `list` is explicitly approved as an idiomatic Lisp
      list-builder/conversion helper, not a new canonical constructor family.
    - runtime primitive registration and compiler primitive hash coverage
      already route `List` and `list` through the same implementation.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- [x] `AUDIT-PROCESS-WRAPPER-PAYLOAD-FALLBACK-100` preserve process
  runtime payload errors through stdlib wrappers
  - closure evidence:
    - `process-spawn` and `process-kill` now have untyped fallbacks after their
      typed wrapper methods, matching the `tcp-*`, `offload`, and
      `thread-spawn` pattern.
    - invalid command and signal arguments now reach the runtime
      `io/process-*` payload contracts instead of being intercepted by generic
      typed dispatch.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `advanced-effect-union-limit` subgroup: `pass=67 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-FILESYSTEM-SURFACE-087` canonicalize filesystem wrappers on
  `fs-*`
  - closure evidence:
    - stdlib no longer exports `filesystem-*` compatibility aliases.
    - runtime, compiler primitive hash entries, docs, and tests now agree on
      the canonical `fs-*` wrapper family.
    - regression coverage verifies representative long-form filesystem aliases
      are unbound.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `advanced-effect-union-limit` subgroup: `pass=65 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-COMPILER-PRIMITIVE-HASH-COVERAGE-099` complete compiler
  primitive hash coverage for public runtime primitives
  - closure evidence:
    - public runtime primitive value references now have compiler primitive hash
      entries, reducing the public-ish missing-hash audit set to zero.
    - the primitive hash table was resized from 256 to 512 to preserve a safe
      load factor after expanding coverage to 195 entries.
    - regression coverage now verifies `sin`, `string-byte-length`, and
      `sort-by` are looked up as primitives, not captured as C3 locals.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - audit script: `hash entries=195`, `focus missing public-ish hash entries=0`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-COMPILER-PRIMITIVE-CLASSIFICATION-098` consolidate compiler
  primitive classification around the primitive hash table
  - closure evidence:
    - free-variable and delegation primitive checks now use one shared
      hash-backed classifier instead of duplicated hardcoded name arrays.
    - legacy non-hash exceptions for `ForeignHandle` and `__ui-ftxui-run`
      remain excluded from closure capture without adding value-position
      lowering.
    - regression coverage now verifies hash-only primitives such as `Dict` and
      `json-parse` are looked up as primitives, not captured as C3 locals.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-FFI-FOREIGN-HANDLE-SURFACE-103` replace the public FFI pointer
  annotation surface with `^ForeignHandle`
  - closure evidence:
    - `docs/LANGUAGE_SPEC.md` now documents `^ForeignHandle` as the pointer ABI
      annotation and only accepts live `FFI_HANDLE` values or `nil`.
    - `docs/reference/09-concurrency-ffi.md` now states that pointer-shaped C
      values use `^ForeignHandle` rather than raw integer addresses.
    - `docs/PROJECT_TOOLING.md` now maps non-string pointer-shaped bindings to
      `^ForeignHandle` in bindgen output.
    - runtime FFI call packing now rejects raw integer addresses for
      pointer-ABI arguments and expects `nil` or live `FFI_HANDLE` values.
    - non-null pointer-ABI returns now produce non-owning `FFI_HANDLE` values
      instead of raw address integers.
    - the audit/remediation and AOT runtime/linking notes now use
      `ForeignHandle` instead of the stale `Pointer` exception wording where
      current-state docs describe the live surface.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=43 fail=0`
    - bounded `compiler` slice: `pass=197 fail=0`
    - `git diff --check`

- [x] `AUDIT-FFI-FOREIGN-HANDLE-AOT-POLICY-105` carry `ForeignHandle`
  metadata policy through the AOT runtime bridge
  - closure evidence:
    - AOT generated declarations now carry `ForeignHandle` policy descriptors
      into `aot::ffi_declare_fn`.
    - Parameter policy descriptors preserve handle family and nullability.
    - Return policy descriptors preserve handle name, ownership, and finalizer
      symbol, and the AOT bridge resolves owned-return finalizers before
      installing the bound primitive.
    - AOT lowering rejects owned `ForeignHandle` parameter policy fail-closed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=49 fail=0`
    - bounded `compiler` slice: `pass=209 fail=0`
    - `git diff --check`

- [x] `AUDIT-FFI-FOREIGN-HANDLE-METADATA-104` implement the interpreter/JIT
  ForeignHandle metadata dictionary policy
  - closure evidence:
    - `^ForeignHandle` remains the simple default foreign-handle annotation.
    - FFI-local metadata dictionaries now accept the refinements:
      `^{'name File 'ownership owned 'finalizer fclose}` implies
      `ForeignHandle`, and the explicit `^{'type ForeignHandle ...}` form is
      also accepted.
    - runtime FFI now enforces handle family and non-null policy at call
      packing, resolves owned return finalizers with `dlsym`, and wraps pointer
      returns in named `FFI_HANDLE` boxes.
    - FFI AST serialization now preserves metadata dictionary annotations for
      compiler roundtrip coverage.
    - AOT policy propagation was split into
      `AUDIT-FFI-FOREIGN-HANDLE-AOT-POLICY-105` and is now closed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=49 fail=0`
    - bounded `compiler` slice: `pass=204 fail=0`

- [x] `AUDIT-E2E-PRIMITIVE-CAPTURE-SANITIZATION-096` fix generated
  e2e C3 primitive capture name sanitization
  - closure evidence:
    - shared compiler symbol-value emission now routes primitive references and
      closure capture initializers through the same primitive-cache path.
    - runtime primitives exposed by the e2e corpus now have compiler primitive,
      free-variable, and hash-table coverage: `error`, `error?`,
      `error-message`, `is?`, `instance?`, and `type-args`.
    - AOT `Coroutine` now accepts `aot::make_closure` wrapper thunks without
      inspecting JIT-only closure payload fields.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct `Coroutine` type probe returns `Coroutine`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-NUMBER-PARSE-SURFACE-085` canonicalize permissive numeric parsing
  as `parse-number`
  - decision note: `docs/plans/number-parse-surface-decision-2026-04-11.md`
    selects `parse-number` and keeps `Number` non-callable.
  - closure evidence:
    - public runtime/compiler primitive surfaces now register `parse-number`
      instead of `string->number`.
    - live tests, examples, and docs now use `parse-number`.
    - `Number` remains an abstract/meta type descriptor for annotation and
      dispatch, not a value-position constructor.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `parse-number` int/double/nil results, removed
      `string->number` binding, live `parse-number` binding, and non-callable
      `Number`
    - bounded `advanced-macro-hygiene-string-number` subgroup: `pass=9 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `compiler` slice: `pass=196 fail=0`
    - `./build/main --check examples/deduce_crud_server.omni`
    - `./build/main --check examples/finwatch/smoke_test.omni`
    - broader Docker `scripts/run_e2e.sh` reached generated-source parity but
      failed the generated C3 build on primitive capture names `error` /
      `error?`; subsequently closed as
      `AUDIT-E2E-PRIMITIVE-CAPTURE-SANITIZATION-096`.

- [x] `AUDIT-LIST-STRING-CONSTRUCTOR-SURFACE-084` canonicalize list/string
  conversion through constructors
  - decision note: `docs/plans/list-string-constructor-decision-2026-04-11.md`
    selects `List(String)` and `String(List)` as the canonical public surface.
  - closure evidence:
    - `List(String)` now reuses the UTF-8 codepoint splitter and returns a
      proper list of one-codepoint strings.
    - `String(List)` now concatenates proper lists of string fragments and
      treats `nil` as the empty list / empty string.
    - public `string->list` and `list->string` primitive/compiler aliases were
      removed; internal C helpers remain for runtime implementation and memory
      regression tests.
    - docs and Lisp-level tests now use constructor forms.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `List(String)`, `String(List)`, `String(nil)`,
      non-string list element rejection, and removed public arrow bindings
    - bounded `advanced-unicode-iterator` subgroup: `pass=138 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `limit-busting` slice: `pass=17 fail=0`
    - bounded `compiler` slice: `pass=196 fail=0`

- [x] `AUDIT-STRING-GENERIC-BYTE-CODEPOINT-094` decide byte versus codepoint
  semantics for generic string `length` and `ref`
  - closure evidence:
    - selected codepoint/character semantics for generic string sequence
      operations to match `string-length`, `char-at`, and `List(String)`.
    - kept byte count explicit through `string-byte-length`.
    - changed generic `ref` and postfix `.[index]` on strings to return
      single-character strings instead of byte integers.
    - added non-ASCII regressions for generic `length`, `ref`, and postfix
      indexing.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `length`, `string-length`, `string-byte-length`, `ref`,
      `char-at`, and postfix `.[index]` on non-ASCII strings
    - bounded `advanced-unicode-iterator` subgroup: `pass=136 fail=0`
    - bounded `advanced-collections-module` subgroup: `pass=142 fail=0`

- [x] `AUDIT-CONS-REF-SPEC-PARITY-095` reconcile cons/list `ref` behavior with
  the language spec
  - closure evidence:
    - kept the existing tested runtime behavior as the contract rather than
      narrowing `ref` on cons cells back to pair-only `0`/`1` access.
    - updated `docs/LANGUAGE_SPEC.md`, `docs/reference/03-collections.md`, and
      `docs/reference/11-appendix-primitives.md` to document cons/list chain
      indexing, negative indexes, and dotted terminal tail length/indexing.
  - validation:
    - bounded `advanced-collections-module` subgroup: `pass=139 fail=0`

- [x] `AUDIT-LIST-PREDICATE-CONTRACT-093` reconcile `list?` proper-list
  semantics with the stdlib implementation
  - closure evidence:
    - registered the existing strict `prim_is_list` primitive as public `list?`.
    - removed the stdlib override that treated every pair as a list.
    - added regressions proving `(list? (cons 1 2))` is false.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `basic` slice: `pass=142 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `advanced-type-dispatch-mutation-chain` subgroup:
      `pass=237 fail=0`

- [x] `AUDIT-LIST-WALKER-IMPROPER-LIST-092` normalize improper-list handling
  across public list walkers
  - closure evidence:
    - added a private proper-list guard and applied it across the public stdlib
      list walkers covered by the audit: `map`, `filter`, `foldl`, `foldr`,
      `append`, `take`, `drop`, `zip`, `for-each`, `any?`, `every?`,
      `flatten`, `partition`, `remove` via `filter`, `find`, and `nth`.
    - mirrored the guard in the compiler stdlib prelude for the overlapping
      walker definitions.
    - guarded nested list traversal in `flatten` so improper inner lists no
      longer fall through to low-level `car`/`cdr` failures.
    - left pair/iterator protocols such as `stream-take` and iterator `drop`
      outside this proper-list-only lane.
  - validation:
    - bounded `advanced-stdlib-numeric` subgroup after partial-success slice:
      `pass=275 fail=0`
    - bounded `compiler` slice after partial-success slice: `pass=196 fail=0`
    - bounded `advanced-stdlib-numeric` subgroup after remaining walker slice:
      `pass=285 fail=0`
    - bounded `compiler` slice after remaining walker slice: `pass=196 fail=0`

- [x] `AUDIT-EVAL-VALUE-TO-EXPR-FAIL-CLOSED-096` harden the runtime `eval`
  data-to-expression conversion path
  - closure evidence:
    - value-to-expression conversion now enforces structural arity for malformed
      special-form data instead of defaulting missing operands to `nil` or
      ignoring extras.
    - `define`, two-operand `set!`, `capture`, and `signal` now reject
      non-symbol names/tags instead of coercing them to symbol id `0`.
    - multi-argument `set!` data forms now lower through normal call dispatch,
      preserving parser-equivalent generic collection setter behavior.
    - lambda and let data forms now preserve parser-equivalent implicit block
      bodies, and `macroexpand` now surfaces structural conversion errors for
      malformed cons forms.
    - added eval and macroexpand regressions in the advanced stdlib numeric
      introspection group.
  - validation:
    - bounded `advanced-stdlib-numeric` subgroup after arity/name slice:
      `pass=265 fail=0`
    - bounded `advanced-stdlib-numeric` subgroup after block/macroexpand slice:
      `pass=268 fail=0`

- [x] `AUDIT-NAMED-LET-INIT-ERROR-PROPAGATION-097` preserve initializer
  error values through named `let`/tail multi-argument calls
  - closure evidence:
    - `make_cons` now treats successfully ESCAPE-promoted `ERROR` values as
      first-class cons elements instead of always interpreting an `ERROR` tag
      result as promotion failure.
    - `append` now checks the intermediate `(reverse a)` value before entering
      its named-let loop, preserving the original proper-list error for
      improper left input.
    - added core regressions for tail multi-argument calls that ignore or
      return an error-valued argument, plus the append improper-left regression.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `c3c build --sanitize=address --warn-deprecation=no`: green
    - direct tail multi-arg/append probes: green
    - bounded `basic` slice: `pass=142 fail=0`
    - bounded `advanced-stdlib-numeric` subgroup: `pass=256 fail=0`
    - bounded `tco-recycling` slice: `pass=11 fail=0`
    - bounded `memory-lifetime-smoke` slice: `pass=201 fail=0`

- [x] `AUDIT-PIKA-REGEX-STRESS-CACHE-090` fix repeatable bounded `pika` regex
  stress/cache failures
  - closure evidence:
    - direct Pika large-pattern controls passed, while named-let
      multi-argument recursion with string parameters reproduced the same
      `arg list too short` failure without invoking regex internals.
    - `make_cons` now permits identity promotion only for values already owned
      by the current target scope chain or ESCAPE lane, and still rejects
      identity returns for current TEMP values that failed to promote.
    - added a memory-lifetime regression covering named-let string argument-list
      promotion under the TCO/ESCAPE-lane path.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `pika` slice: `pass=83 fail=0`
    - bounded `memory-lifetime-smoke` slice: `pass=201 fail=0`

- [x] `AUDIT-HTTP-CRUD-DICT-PROMOTION-089` fix repeatable bounded `http` slice
  failures after constructor cleanup
  - closure evidence:
    - `pipeline/decode-request` now uses line-based request splitting so LF-only
      and CRLF requests do not treat header lines as JSON bodies.
    - duplicate-post payload construction now uses `ref` for optional response
      error fields, so success responses do not inject error values into a
      dictionary literal.
    - `prim_dict` now propagates error-valued literal key/value arguments before
      insertion instead of reporting a misleading backing-storage OOM.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `http` slice: `pass=29 fail=0`

- [x] `AUDIT-CONSTRUCTOR-DISPATCH-SURFACE-083` remove high-confidence
  noncanonical constructor/conversion aliases and vector terminology
  - closure evidence:
    - `number->string`, `symbol->string`, `string->symbol`,
      `exact->inexact`, and `inexact->exact` are no longer public primitive
      registrations; `String`, `Symbol`, `Double`, and `Integer` are the
      canonical constructor/coercion surfaces.
    - `set-size` and `set->list` are no longer public primitive
      registrations; `length` handles set cardinality and `List(Set ...)`
      materializes deterministic canonical set element order.
    - fast-dev-only `Int`, `Bool`, and duplicate `filesystem-*` primitive
      aliases were removed.
    - schema validation now uses `array-of` instead of `vector-of`.
    - `lib/immer.omni` now exposes `persistent-array`,
      `persistent-dictionary`, and `persistent-set` names instead of
      `vector`, `hash-map`, and `hash-set`.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `string-type` slice: `pass=40 fail=0`
    - bounded `advanced` slice: `pass=1189 fail=0`
    - bounded `compiler` slice: `pass=194 fail=0`
    - quick eval confirmed removed set aliases, `length`/`List(Set ...)`, and
      `array-of` schema behavior.

- [x] `AUDIT-RUNTIME-INTERN-RAISEPAYLOAD-GUARDS-081` guard runtime intern and
  unhandled-effect raise payload paths
  - closure evidence:
    - `src/lisp/eval_init_primitive_registration.c3` now rejects failed `nil`
      symbol interning before defining the constant.
    - `src/lisp/jit_jit_closure_runtime.c3` now treats failed promise env-tag
      interning as a non-match.
    - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now uses
      non-raising dictionary allocation for unhandled-effect payloads and
      checks payload-key interning before publication.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal `jit-policy`: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`
    - bounded normal `advanced`: `pass=1185 fail=0`
    - bounded ASAN `advanced`: `pass=1172 fail=0`

- [x] `AUDIT-RUNTIME-RESULT-KEY-INTERN-GUARDS-080` guard runtime result,
  lookup, and optional diagnostic payload symbols before dictionary use
  - closure evidence:
    - `src/lisp/eval_dispatch_error_payloads.c3` now rejects
      `INVALID_SYMBOL_ID`, null, `ERROR`, and invalid-symbol payload values in
      the shared optional dispatch payload setter.
    - `src/lisp/async_process_spawn.c3` now interns process-spawn result keys
      before constructing key symbols and closes spawned resources on key
      interning failure.
    - `src/lisp/http_url_response.c3` now rejects failed response-field key
      interning before publishing HTTP response payload dictionaries.
    - `src/lisp/prim_ui_ftxui_helpers.c3` now rejects failed lookup-key
      interning before probing UI dictionaries.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal+ASAN `async` slice with FTXUI smoke enabled:
      `pass=65 fail=0`
    - bounded normal `jit-policy`: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`

- [x] `AUDIT-DEDUCE-GOAL-DIRECTED-INTERN-GUARDS-079` guard
  goal-directed deduce explain/why-result symbols before payload publication
  or dictionary lookup
  - closure evidence:
    - `src/lisp/deduce_rule_ops_explain_goal_directed_components.c3` now
      routes goal-directed blocker and shape symbols through a helper that
      rejects `INVALID_SYMBOL_ID` with the existing explain OOM error instead
      of constructing invalid `SYMBOL` payload values.
    - `src/lisp/deduce_rule_ops_explain_snapshot.c3` now uses that helper for
      goal-directed shape and execution-path payload values.
    - `src/lisp/deduce_why_result_path_payload.c3` now rejects invalid lookup
      key interning before constructing the temporary dictionary key symbol.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal+ASAN `deduce` slice: `pass=330 fail=0`

- [x] `AUDIT-DEDUCE-GOAL-DIRECTED-PAYLOAD-NO-PRESEED-078` make
  goal-directed deduce diagnostic payload allocation non-raising before
  fallback deduce OOM publication
  - closure evidence:
    - `src/lisp/deduce_rule_eval_analyze_setup.c3` now uses
      `make_hashmap_no_raise(...)` for goal-directed selector analysis error
      payload dictionaries instead of the raising dictionary constructor path.
    - `src/lisp/deduce_rule_eval_fixpoint_goal_directed_selector_prepare.c3`
      now uses the same non-raising dictionary path for selector and relation
      surface diagnostic payload dictionaries.
    - Both files route payload field insertion through local no-raise setters
      before publishing the existing deduce out-of-memory fallback error.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=200 fail=0`

- [x] `AUDIT-DEDUCE-INTEGRITY-PAYLOAD-NO-PRESEED-077` make optional deduce
  diagnostic payload allocation non-raising under active handlers
  - closure evidence:
    - `src/lisp/deduce_relation_ops_validation_payload.c3` now uses
      `make_hashmap_no_raise(...)` for integrity/check-context diagnostic
      payload dictionaries instead of the raising dictionary constructor path.
    - `src/lisp/deduce_rule_eval_exec_component_state.c3` now uses the same
      non-raising dictionary helper for iteration-limit diagnostic payloads
      that are returned as payload-or-null before the later iteration-limit
      raise.
    - Integrity payload dictionary field insertion now routes through
      no-raise local setters that return `null` from the optional payload
      builder on allocation, interning, or promotion failure.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now installs a
      raise handler, forces deduce integrity and iteration-limit payload
      allocation failure, and verifies `raise_pending`, `raise_payload`, and
      `raise_msg_len` remain clear.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=200 fail=0`

- [x] `AUDIT-CTOR-PAYLOAD-NO-PRESEED-076` make optional constructor mismatch
  diagnostic payload allocation non-raising under active handlers
  - closure evidence:
    - `src/lisp/primitives_meta_types_ctor_helpers.c3` now uses
      `make_hashmap_no_raise(...)` for `ctor_mismatch_data(...)` instead of the
      raising dictionary constructor path.
    - Constructor mismatch payload keys are checked for `INVALID_SYMBOL_ID`
      before constructing payload key symbols.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now installs a
      raise handler, forces constructor mismatch payload allocation failure, and
      verifies `raise_pending`, `raise_payload`, and `raise_msg_len` remain
      clear.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=199 fail=0`

- [x] `AUDIT-DISPATCH-PAYLOAD-NO-PRESEED-075` make optional dispatch
  diagnostic payload allocation non-raising under active handlers
  - closure evidence:
    - `src/lisp/prim_collection_hashmap.c3` now exposes
      `make_hashmap_no_raise(...)` for callers that need optional dictionary
      payload storage without publishing a runtime raise on allocation failure.
    - `src/lisp/value_constructors.c3` and
      `src/lisp/eval_dispatch_error_payloads.c3` use the non-raising helper
      for handled raise payload and dispatch diagnostic payload construction.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3` now installs a
      raise handler, forces dispatch payload dictionary allocation failure, and
      verifies `raise_pending`, `raise_payload`, and `raise_msg_len` remain
      clear.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal `jit-policy` with FTXUI smoke enabled: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`

- [x] `AUDIT-JIT-MUTABLE-LOCAL-NULL-BOX-073` make JIT mutable-local helper
  paths fail closed when a root-box env allocation is missing
  - closure evidence:
    - `src/lisp/jit_jit_apply_multi_prims_tail.c3` now makes
      `jit_env_lookup_local(...)` return an explicit
      `jit: missing mutable local binding` error when the helper receives a
      null env or cannot find the requested binding.
    - `jit_env_reparent(...)` now returns the effective env and treats a null
      source env as a no-op reparent to the requested parent, so compiled env
      capture does not reload a known-null helper result.
    - `src/lisp/jit_jit_compile_expr_basic.c3` and
      `src/lisp/jit_jit_emit_helpers.c3` now use the checked helper contracts.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3` covers both helper
      contracts directly in the `jit-policy` slice.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal `jit-policy` with FTXUI smoke enabled: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`

- [x] `AUDIT-RAISE-PAYLOAD-NESTED-PENDING-074` make handled raise payload
  construction avoid publishing stale nested pending-raise state on allocation
  failure
  - closure evidence:
    - `src/lisp/value_constructors.c3` now builds raise payload dictionaries
      through `make_hashmap_no_raise(...)` instead of the raising
      `make_hashmap(...)` constructor path.
    - `raise_error_pending_impl(...)` receives the intended
      payload-construction failure without a pre-existing `raise_pending` side
      effect from nested dictionary construction.
    - The existing `pending-raise-payload-alloc-failure` `jit-policy`
      regression now passes in the full bounded slice.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal `jit-policy` with FTXUI smoke enabled: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`

- [x] `AUDIT-TCO-ENV-COPY-FAIL-CLOSED-071` make TCO env-chain copy reject
  boundary-copy failures before binding copied env frames
  - closure evidence:
    - `src/lisp/jit_jit_eval_scope_copy.c3` now uses checked boundary-copy
      results for TCO env-frame binding copy and aborts the copied frame when
      copy returns a fault, null value, or `ERROR`.
    - Parent rewrites for root-persistent env boxes now also fail closed when a
      required parent-chain copy fails.
    - `src/lisp/tests_memory_lifetime_tco_budget_groups.c3` now covers a
      forced opaque-primitive boundary-copy failure and verifies the source env
      binding remains intact while the copied env is rejected.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=198 fail=0`

- [x] `AUDIT-TCO-RECYCLE-HOOK-FAIL-CLOSED-072` make TCO recycle preparation
  preserve the previous env and return explicit error on env-copy failure
  - closure evidence:
    - `src/lisp/runtime_backend_hooks.c3` now keeps `*env_io` unchanged when
      TCO env-chain copy fails, releases the fresh recycle scope, restores the
      prior call scope, and returns an explicit `ERROR`.
    - Active defer retargeting is restored back to the original call scope when
      env-copy fails after fresh-scope retargeting.
    - `src/lisp/tests_memory_lifetime_tco_budget_groups.c3` now verifies the
      recycle hook returns `jit: failed to copy TCO recycle env` while
      preserving env and recycle-scope state.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=198 fail=0`

- [x] `AUDIT-JIT-MODULE-VALUE-GROWTH-069` make first-class module values
  survive module table growth
  - closure evidence:
    - `src/lisp/value_predicates_accessors_basic.c3` now snapshots the module
      descriptor into root-scope storage for first-class `MODULE` values
      instead of storing an address into the reallocating interpreter module
      table.
    - `src/lisp/eval_path.c3` now fails closed for invalid module descriptors
      before reading exports or env bindings.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now forces
      module table growth after creating a first-class module value and then
      verifies exported path access remains valid under normal and ASAN smoke.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=196 fail=0`

- [x] `AUDIT-JIT-MUTATION-PROMOTION-NULL-GUARDS-070` harden mutation/define
  paths against null promotion results
  - closure evidence:
    - `src/lisp/jit_jit_closure_let_set_helpers.c3` now treats null cons-field
      promotion and null instance-field boundary-copy results as errors before
      mutating storage.
    - `src/lisp/jit_jit_define_method_table.c3` now rejects null typed method
      implementations and null global define promotion results before
      appending method table entries or replacing fallbacks.
    - `src/lisp/aot_type_definitions.c3` now rejects null AOT typed-method
      promotion before calling into method-table publication.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now covers the
      JIT instance-field boundary-copy fault path and verifies the old field
      remains intact.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=196 fail=0`

- [x] `AUDIT-BOUNDARY-INSTANCE-MODULE-ALIAS-GRAPH-068` align boundary
  alias-reuse traversal with graph-bearing `INSTANCE` / `MODULE` wrappers
  - closure evidence:
    - `src/lisp/eval_boundary_provenance.c3` now treats `INSTANCE` and
      `MODULE` as graph-bearing alias payloads, matching the committed-root
      graph audit edge table.
    - The rare `INSTANCE` / `MODULE` path uses a heap-backed reachability scan
      instead of adding more large local arrays to the alias walker frame, so
      the FTXUI/effect stack budget remains protected.
    - The scan checks value and environment reachability into the releasing
      scope, including by-value instance fields whose stored `scope_gen` or
      nested graph still points back to the releasing scope.
    - Root-persistent env boxes are still traversed for parent/binding edges;
      they are only excluded from direct temp-frame ownership checks.
    - `src/lisp/tests_memory_lifetime_boundary_state_groups.c3` now includes
      a regression that forces an instance field graph to retain a releasing
      payload and verifies alias reuse rejects it.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=194 fail=0`

- [x] `AUDIT-BOUNDARY-DESTINATION-BUILD-SPLICE-FACADE-067` keep destination
  build-scope splice callsites policy-clean without changing commit semantics
  - closure evidence:
    - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` no longer
      calls `main::scope_splice_escapes(...)` directly.
    - `src/lisp/eval_boundary_commit_escape_builders.c3` now owns the narrow
      allowlisted `boundary_destination_build_scope_splice(...)` shim, keeping
      the low-level splice within the existing boundary implementation file
      that the facade policy permits.
    - Destination `cons`, `partial`, `iterator`, and `error` builders retain
      the previous build-scope commit behavior, avoiding a source-wrapper
      fallback that breaks nested effect payload return boundaries.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/check_boundary_facade_usage.sh`: green
    - host `scripts/check_boundary_change_policy.sh`: green
    - host `scripts/check_status_consistency.sh`: green
    - host `git diff --check`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=193 fail=0`

- [x] `AUDIT-BOUNDARY-ALIAS-WORKLIST-PERF-065` replace boundary alias linear
  visited tracking with a bounded hash/set helper
  - closure evidence:
    - `src/lisp/eval_boundary_provenance.c3` now keeps the authoritative linear
      `seen` list for no-false-negative correctness, but fronts it with a small
      bounded `ushort` index-table accelerator for common repeated composite
      alias checks.
    - Scalar/non-graph roots now return before entering the large traversal
      frame, and the large-array walker sits behind a small stack-headroom
      wrapper that fails closed to copy-required if the current stack context is
      too shallow.
    - The accelerator is deliberately small to stay under the FTXUI/effect
      resolve stack budget; it saturates into the existing linear scan rather
      than dropping entries or failing open.
    - The smoke lane now includes a shared composite cycle payload regression
      alongside the wide scalar payload and nested effect payload regressions.
    - Larger local pointer/index-table attempts regressed FTXUI smoke with a
      `smoke.omni` boundary resolve stack overflow and were not kept.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=193 fail=0`

- [x] `AUDIT-FTXUI-C-ABI-EXCEPTION-SAFETY-063` wrap exported FTXUI C ABI
  entrypoints in fail-closed exception guards
  - closure evidence:
    - `csrc/ftxui_shim.cpp` now routes status-returning backend work through
      a shared `status_try(...)` guard that maps `std::bad_alloc` to
      `OMNI_FTXUI_STATUS_OUT_OF_MEMORY` and other C++ exceptions to
      `OMNI_FTXUI_STATUS_INTERNAL_ERROR`.
    - Argument and ABI validation remains outside the guard where it was
      already fail-closed, preserving existing validation behavior.
    - Deferred graph/render/event/quit-key callback adapters catch callback
      exceptions locally and return safe fallback values rather than allowing
      callback exceptions to escape through FTXUI render/event frames.
    - A coverage check over `omni_ftxui_status` exports found no unguarded
      status-returning entrypoint except the trivial last-error accessor.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green

- [x] `AUDIT-FTXUI-SCREEN-LIFETIME-064` make quit-key wrapper lifetime explicit
  - closure evidence:
    - `csrc/ftxui_shim.cpp` now stores the underlying `ScreenInteractive` with
      shared ownership.
    - `omni_ftxui_component_wrap_quit_keys(...)` now captures the shared screen
      object instead of the raw screen handle and retains it in the wrapped
      component keep-alive list.
    - `csrc/ftxui_shim.h` and `docs/plans/ftxui-c-abi-shim.md` document that
      the quit wrapper retains the screen's underlying loop object until the
      wrapped component is destroyed.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green

- [x] `AUDIT-OFFLOAD-WIDTH-GUARDS-066` harden offload descriptor/session/temp-id
  narrowing edges
  - closure evidence:
    - `src/lisp/scheduler_offload_network.c3` now rejects listener file
      descriptors that cannot fit in the `int` `tcp_accept_fd(...)` API before
      narrowing.
    - `src/lisp/eval_repl_server_state.c3` now formats REPL session IDs from a
      guarded `long` value instead of truncating `next_session_id` through
      `int`.
    - `src/lisp/scheduler_offload_ops.c3` now formats the full guarded
      `unique_id` lane for atomic temp-path suffixes instead of truncating it to
      `uint`.
    - `src/lisp/scheduler_state_support_types.c3` now has a compile-time guard
      for the current `OffloadWork` pointer-through-`long` payload contract.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green
    - bounded normal+ASAN `async`: green (`pass=65 fail=0`)

- [x] `AUDIT-AOT-MODULE-DEDUCE-COLLECTION-SENTINELS-058` harden AOT/env
  sentinel handling, module load rollback, deduce restore failure propagation,
  and nullable collection backing guards
  - closure evidence:
    - `src/lisp/aot_runtime_bridge_closure.c3`,
      `src/lisp/aot_runtime_bridge_helpers.c3`,
      `src/lisp/aot_runtime_bridge_ffi.c3`, and
      `src/lisp/aot_runtime_bridge_ffi_lib.c3` now route AOT bridge symbol
      binding and lookup through checked intern helpers instead of allowing
      `INVALID_SYMBOL_ID` to reach environment lookup/define/set paths.
    - `src/lisp/value_environment_storage.c3`,
      `src/lisp/value_environment.c3`, and
      `src/lisp/value_environment_barrier.c3` now reject invalid symbol IDs
      before hash probing or binding mutation.
    - `src/lisp/jit_jit_module_setup_helpers.c3`,
      `src/lisp/jit_jit_module_import_setup.c3`,
      `src/lisp/macros_expansion.c3`, and
      `src/lisp/value_interp_lifecycle.c3` now roll back newly published
      modules on body/path/top-level load failure, rebuild module hash state,
      skip tombstones during hash rebuild, and avoid stale module pointers when
      nested loads grow the module table.
    - `src/lisp/deduce_db_storage_open.c3`,
      `src/lisp/deduce_db_rule_catalog_persistence.c3`,
      `src/lisp/deduce_db_rule_signature_restore.c3`, and
      `src/lisp/deduce_db_handles_storage.c3` now distinguish missing DBIs
      from other LMDB open failures, reject invalid interned restored symbols,
      roll back partially restored rule signatures/schemas, and fail closed on
      relation metadata policy intern failure.
    - `src/lisp/eval_promotion_root_clones.c3` now checks method-table clone
      entry allocation size before `sizeof * capacity`.
    - `src/lisp/value_constructors.c3`,
      `src/lisp/prim_collection_sort_array.c3`,
      `src/lisp/prim_collection_hashmap.c3`, and
      `src/lisp/prim_collection_generic_set.c3` now reject invalid raise
      payload symbols and nullable array/dict/set backing storage before
      dereference.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded advanced slice: `pass=1183 fail=0`
      - bounded compiler slice: `pass=191 fail=0`
      - bounded deduce slice: `pass=330 fail=0`
      - bounded string-type slice: `pass=40 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-NUMERIC-MACRO-ASYNC-TAIL-GUARDS-057` harden numeric overflow,
  macro/parser symbol allocation, data-format option tails, and async cleanup
  paths
  - closure evidence:
    - `src/lisp/prim_math.c3`, `src/lisp/prim_math_core.c3`, and
      `src/lisp/prim_math_arithmetic.c3` now fail closed on `getrandom`
      failure/partial reads and on signed `long.min` / integer overflow edges
      in arithmetic, `abs`, `gcd`, and `lcm`.
    - `src/lisp/prim_string_format_directives.c3` now rejects overflowing
      format width/precision specifiers and `%b` no longer negates
      `long.min`.
    - `src/lisp/prim_string_ops.c3`,
      `src/lisp/prim_string_transform.c3`,
      `src/lisp/primitives_data_formats_csv.c3`, and
      `src/lisp/primitives_data_formats_toml_options.c3` now preserve
      multibyte `list->string` elements and reject improper option/list tails.
    - `src/lisp/prim_string_convert.c3`,
      `src/lisp/macros_template_expansion.c3`,
      `src/lisp/macros_expr_conversion_form_builders.c3`,
      `src/lisp/parser_parser.c3`, and
      `src/lisp/parser_application_placeholders.c3` now reject intern/allocation
      failure instead of propagating invalid symbol IDs or writing through
      unchecked macro AST allocations.
    - `src/lisp/async_tcp_transport_helpers.c3`,
      `src/lisp/async_tcp_transport_core.c3`,
      `src/lisp/async_process_signal_dns.c3`,
      `src/lisp/async_tcp_transport_connect.c3`,
      `src/lisp/async_udp_pipe.c3`,
      `src/lisp/async_tcp_transport_listen.c3`,
      `src/lisp/scheduler_state_types.c3`, and
      `src/lisp/scheduler_wakeup_queue.c3` now reject negative TCP read sizes,
      close stranded pending async state on resumed-before-completion errors,
      and track writable wakeup coalesces separately.
    - regression coverage added in:
      - `src/lisp/tests_advanced_core_unicode_groups.c3`
      - `src/lisp/tests_advanced_stdlib_numeric_groups.c3`
      - `src/lisp/tests_runtime_data_unicode_groups.c3`
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded arithmetic-comparison slice: `pass=45 fail=0`
      - bounded string-type slice: `pass=40 fail=0`
      - bounded data-format slice: `pass=62 fail=0`
      - bounded async slice: `pass=61 fail=0`
      - bounded scheduler slice: `pass=111 fail=0`
      - bounded advanced numeric string/predicate/format group:
        `pass=59 fail=0`
      - bounded advanced unicode iterator group: `pass=130 fail=0`
      - bounded advanced macro hygiene group: `pass=82 fail=0`

- [x] `AUDIT-RUNTIME-REGISTRY-IO-TLS-GUARDS-056` harden registry,
  bootstrap, unicode, collection, I/O, and TLS guard paths
  - closure evidence:
    - `src/lisp/value_symbol_table.c3` and
      `src/lisp/value_type_registry.c3` now reject exhausted ID spaces before
      narrowing counts to `SymbolId` / `TypeId`; symbol probing also skips
      out-of-range stale indices, and type rollback now rebuilds the hash table
      so open-address probe chains remain sound.
    - `src/lisp/value_interp_init_helpers.c3` and
      `src/lisp/value_interp_state.c3` now fail fast if interpreter bootstrap
      symbols cannot be interned instead of publishing invalid sentinel IDs.
    - `src/lisp/unicode_case_mapping.c3` now rejects strings too large for
      `utf8proc`'s `long` length parameter before case conversion.
    - `src/lisp/scheduler_offload_network.c3` now calls `br_sslio_close(...)`
      only after `br_sslio_init(...)` has completed.
    - `src/lisp/prim_collection_hashmap.c3` now computes Dictionary initial
      capacity with checked arithmetic and allocates the hashmap payload before
      publishing the root wrapper.
    - `src/lisp/prim_io_file_helpers.c3` now rejects file sizes that cannot fit
      in `usz` before buffer allocation, and
      `src/lisp/prim_io_console_helpers.c3` now reports render/write failures
      as typed I/O errors.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded compiler slice: `pass=191 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`
      - bounded async slice: `pass=61 fail=0`
      - bounded scheduler slice: `pass=111 fail=0`
      - bounded advanced unicode iterator slice: `pass=129 fail=0`

- [x] `AUDIT-AOT-DISPATCH-SIGNATURE-ALLOC-GUARDS-055` harden AOT/JIT
  signature allocation and type publication failure paths
  - closure evidence:
    - `src/lisp/aot_type_spec_helpers.c3` now rejects overflowing type
      annotation, method-signature parameter, and method-constraint allocation
      sizes before allocation and delays count publication until staging
      succeeds.
    - `src/lisp/jit_jit_closure_support.c3` now guards lambda method-signature
      constraint counting and parameter/constraint staging on both scope and
      heap copies.
    - `src/lisp/eval_type_declarations.c3` now checks derived type-info
      field/type-param/constraint/variant allocation sizes and rolls back a
      newly registered type if later constructor/global binding/type-value
      publication fails.
    - `src/lisp/value_type_registry.c3` now exposes a narrow
      just-added-type rollback helper for new-entry failure cleanup.
    - `src/lisp/eval_dispatch_match.c3`,
      `src/lisp/eval_dispatch_match_breakdown.c3`,
      `src/lisp/schema_explain_helpers.c3`, and
      `src/lisp/aot_runtime_bridge_helpers.c3` now reject overflowing temporary
      dispatch/schema/AOT staging buffers.
    - `src/lisp/eval_dispatch_types.c3` and
      `src/lisp/eval_init_primitives.c3` now clean up empty heap method-table
      bootstrap payloads when root-wrapper or global binding publication fails.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded compiler slice: `pass=191 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-JIT-DEDUCE-ALLOC-BOUNDS-054` harden JIT effect and deduce
  aggregate/materialization allocation arithmetic
  - closure evidence:
    - `src/lisp/jit_jit_handle_signal_handle.c3`,
      `src/lisp/jit_jit_runtime_effects_signal.c3`, and
      `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now reject
      overflowing effect clause and signal argument buffer sizes before
      allocation.
    - `src/lisp/deduce_rule_eval_exec_aggregate_groups.c3`,
      `src/lisp/deduce_rule_eval_exec_aggregate_helpers.c3`,
      `src/lisp/deduce_rule_ir_helpers.c3`, and
      `src/lisp/deduce_rule_ir.c3` now check aggregate group, encoded tuple,
      rule-term, and rule-atom allocation arithmetic before writing staged
      state.
    - `src/lisp/deduce_relation_row_materialization.c3` now clamps dictionary
      capacity arithmetic and rejects overflowing column-key value buffers.
    - `src/lisp/deduce_db_goal_directed_read_tracking.c3` now bounds
      read-position buffer sizing and writeback iteration from `ulong` counts.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded deduce slice: `pass=330 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-ENV-PROCESS-SPAWN-SIZE-GUARDS-053` harden environment,
  closure-wrapper, and process-spawn allocation sizing
  - closure evidence:
    - `src/lisp/value_environment_storage.c3` now rejects overflowing hash-table
      capacity and byte-size calculations before allocation.
    - `src/lisp/value_environment.c3` now guards load-factor multiplication
      before hash-table rebuild comparisons.
    - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now rejects overflowing
      closure parameter-copy allocations.
    - `src/lisp/async_process_spawn.c3` now rejects overflowing argv/env pointer
      table counts and byte sizes before process-spawn staging.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - bounded async slice: `pass=61 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-RUNTIME-PROMOTION-ALLOC-STAGING-052` harden promotion,
  root-clone, env-copy, and pattern sequence allocation sizing and staging
  - closure evidence:
    - `src/lisp/eval_promotion_copy_route_helpers.c3`,
      `src/lisp/eval_promotion_escape_structured.c3`, and
      `src/lisp/eval_promotion_root_clone_basic.c3`
      now reject overflowing array, hashmap, method-table, signature, and
      closure-parameter allocation sizes before copying boundary-owned data.
    - `src/lisp/eval_promotion_escape_structured.c3`
      now resets staged method signatures on dependent allocation failure and
      delays closure result wrapper publication until fallible clone/env work
      succeeds.
    - `src/lisp/eval_env_copy_frame_helpers.c3`
      now allocates non-inline binding storage before publishing the copied
      frame and frees it if frame allocation fails.
    - `src/lisp/eval_pattern_matching.c3`
      now rejects overflowing sequence element collection buffers.
    - validation:
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-DEDUCE-SCC-ALLOC-BOUNDS-051` harden deduce SCC, reachability,
  proof-key, aggregate batch, component-delta, and schema-capacity arithmetic
  - closure evidence:
    - `src/lisp/deduce_rule_eval_scc_plan.c3`
      now checks SCC square matrix and stratum relaxation bound arithmetic.
    - `src/lisp/deduce_rule_eval_validation.c3`
      now checks reachability matrix square sizing before allocation.
    - `src/lisp/deduce_rule_eval_exec_seminaive.c3`,
      `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates_impl.c3`,
      and `src/lisp/deduce_rule_eval_exec_component_delta_restore.c3`
      now guard proof-key, aggregate batch, and decoded delta-entry allocation
      sizes.
    - `src/lisp/deduce_schema_query_input_shape.c3`,
      `src/lisp/deduce_schema_query_input_roles.c3`, and
      `src/lisp/deduce_schema_query_input_constraints.c3`
      now reject overflowing `count + 1` capacity requests.
    - validation:
      - bounded deduce slice: `pass=330 fail=0`

- [x] `AUDIT-AOT-FFI-ALLOC-STAGING-050` harden AOT type definition cleanup
  ownership and runtime FFI call argument staging
  - closure evidence:
    - `src/lisp/aot_type_definitions.c3`
      now initializes cleanup-owned type fields and union variants before any
      later fallible allocation can trigger deferred cleanup, and checks
      AOT field/variant/type-parameter allocation sizes before allocation.
    - `src/lisp/eval_ffi_bound_call.c3`
      now rejects unsupported/narrowing argument counts and overflowing
      libffi staging buffer sizes before preparing runtime call storage.
    - validation:
      - bounded compiler slice: `pass=191 fail=0`
      - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- [x] `AUDIT-PARSER-AST-ARRAY-ALLOC-HELPER-049` centralize parser AST array
  allocation overflow checks
  - closure evidence:
    - `src/lisp/parser_parser.c3`
      now exposes `Parser.alloc_ast_array_bytes(...)`, which checks
      `elem_size * count` before delegating to AST arena allocation.
    - Parser dynamic AST array allocations for calls, relation definitions,
      literals, patterns, type annotations, module bodies, lambda params,
      named-let rewrites, path expressions, macro clauses, blocks, and pipe
      rewrites now route through the checked helper.
    - `src/lisp/parser_set_pipe_helpers.c3`
      now rejects overflowing `arg_count + 1` before growing pipe call
      arguments.
    - validation:
      - bounded compiler slice: `pass=191 fail=0`
      - bounded deduce slice: `pass=330 fail=0`

- [x] `AUDIT-PARSER-AST-ARG-SIZE-GUARDS-048` harden parser call/import/FFI
  argument sizing before AST arena allocation
  - closure evidence:
    - `src/lisp/parser_define_relation_attr_helpers.c3`
      now checks relation role/count arithmetic and `Expr*` allocation sizing
      before building `__define-relation` call arguments.
    - `src/lisp/parser_application_helpers.c3`
      now checks generic call argument `Expr*` allocation sizing before
      copying argument pointers.
    - `src/lisp/parser_ffi_helpers.c3`
      now rejects overflowing FFI parameter counts before `+ 1` capacity
      checks.
    - `src/lisp/parser_import_helpers_specs.c3`
      now rejects overflowing selective-import counts before `+ 1` capacity
      checks.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded compiler slice: `pass=191 fail=0`
      - bounded deduce slice: `pass=330 fail=0`

- [x] `AUDIT-SHARED-ASYNC-STRING-SIZE-GUARDS-047` harden shared blob,
  process/TLS string duplication, filesystem result array growth, and
  compression output allocation sizes
  - closure evidence:
    - `src/lisp/scheduler_shared_handles_blob.c3`
      now rejects max-sized blob copies before `len + 1` allocation.
    - `src/lisp/async_process_signal_handles.c3`
      now rejects max-sized C-string copies, cons sequence count overflow, and
      argv/env `char*` table byte-size overflow before allocation.
    - `src/lisp/tls_offload_connect.c3`
      now rejects max-sized TLS duplicate strings before `len + 1` allocation.
    - `src/lisp/prim_io_fs_handles.c3`
      now rejects invalid/overflowing filesystem result array growth before
      replacing array backing storage.
    - `src/lisp/scheduler_offload_network.c3`
      now rejects max-sized compression bounds before allocating the output
      scratch buffer.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded async slice: `pass=61 fail=0`
      - bounded scheduler slice: `pass=111 fail=0`

- [x] `AUDIT-ASYNC-FILE-IO-SIZE-GUARDS-046` harden async file I/O payload,
  temp-path, and read-buffer allocation sizes against `len + 1` overflow
  - closure evidence:
    - `src/lisp/prim_io_file.c3`
      now checks path/content payload length arithmetic before building the
      internal async write-file offload payload.
    - `src/lisp/scheduler_offload_ops.c3`
      now checks atomic temp-path length arithmetic before allocating the
      temporary path buffer.
    - `src/lisp/prim_io_file_helpers.c3`
      now rejects a max-sized read buffer before adding the trailing byte for
      the file-read scratch buffer.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded async slice: `pass=61 fail=0`

- [x] `AUDIT-SCHEDULER-OFFLOAD-NIL-COMPLETION-PROJECTION-045` make
  scheduler offload `OFFLOAD_RES_NIL` projection terminal so async file
  missing-path failures are remapped to their I/O payload codes instead of
  leaking `scheduler/offload-invalid-completion-kind`
  - closure evidence:
    - `src/lisp/scheduler_wakeup_io.c3`
      now returns directly from each offload completion-kind projection,
      including the nil result path used by failed async read-file offloads.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded async slice: `pass=61 fail=0`

- [x] `AUDIT-IO-STRING-BUFFER-GROWTH-HARDENING-044` harden console, input,
  CSV, REPL, and Unicode string-buffer growth paths against
  overflow-before-allocation and invalid capacity state
  - closure evidence:
    - `src/lisp/prim_io_console_helpers.c3`
      now checks console capture append/copy growth arithmetic before
      appending or duplicating buffered text.
    - `src/lisp/prim_io_helpers.c3`
      now rejects overflowing input-state append lengths before mutating the
      live buffer.
    - `src/lisp/primitives_data_formats_csv_parse.c3`
      now guards CSV field and row-array growth before allocating replacement
      storage.
    - `src/lisp/eval_repl_server_state.c3`
      now rejects overflowing session-string and session-capacity growth
      before publishing the replacement session state.
    - `src/lisp/unicode_case_utf8proc.c3` and
      `src/lisp/unicode_case_mapping.c3`
      now route case-mapping append growth through a checked helper so the
      UTF-8 output buffer cannot overflow or grow from invalid capacity state.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded unicode slice: `pass=25 fail=0`
      - bounded data-format slice: `pass=59 fail=0`
      - bounded async slice after scheduler nil-completion projection fix:
        `pass=61 fail=0`
      - bounded advanced unicode iterator group: `pass=129 fail=0`

- [x] `AUDIT-DEDUCE-DIRECT-ALLOC-SCHEMA-MUTATION-043` harden direct
  deduce schema/rule mutation allocation sites and transaction insert
  accounting
  - closure evidence:
    - `src/lisp/deduce_db_handles_mutation.c3`
      now guards rule-signature count increments, range checks, and direct
      `sizeof * count` array allocations before copying rule metadata.
    - `src/lisp/deduce_db_relation_schema_init.c3`,
      `src/lisp/deduce_db_handles.c3`, and
      `src/lisp/deduce_db_handles_register.c3`
      now reject overflowing schema/index sizing before allocation or count
      publication.
    - `src/lisp/deduce_db_handles_mutation_txn.c3`
      now increments transaction `inserted_count` only after tuple delta
      append succeeds.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded deduce slice: `pass=330 fail=0`

- [x] `AUDIT-CORE-REGISTRY-TABLE-GROWTH-HARDENING-042` harden remaining
  core registry/table growth paths against overflow-before-allocation,
  failed-allocation state corruption, and invalid hashmap capacity state
  - closure evidence:
    - `src/lisp/value_symbol_table.c3` and
      `src/lisp/value_type_registry.c3`
      now reject overflowed init/grow byte-size arithmetic and keep previous
      table state when replacement allocation cannot be materialized.
    - `src/lisp/value_interp_init_helpers.c3`
      now checks macro/module init table/hash size multiplication before
      allocation.
    - `src/lisp/eval_pattern_match_support.c3`
      now guards `gensym` / match-binding grow loops against doubling and
      allocation-byte overflow.
    - `src/lisp/prim_collection_hashmap.c3` and
      `src/lisp/prim_collection_sort_array.c3`
      now enforce valid hashmap grow preconditions and reject overflowing grow
      arithmetic before mutating live collection state.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded allocator-validation slice: `pass=1 fail=0`
      - bounded advanced collections/module group: `pass=134 fail=0`
      - bounded advanced type-dispatch/mutation-chain group: `pass=236 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- [x] `AUDIT-OVERFLOW-HARDENING-BATCH-041` close the next allocator/cursor
  overflow audit batch across deduce persistence, AST/parser allocation,
  JIT/effect staging, interpreter/env table growth, and method-table growth
  - closure evidence:
    - persisted deduce rule signature/catalog record encode/restore paths now
      use checked size and cursor helpers before allocation, copy, and slice
      movement.
    - AST arena alignment/chunk accounting and parser import/module/export
      growth now reject allocation-size overflow before state mutation.
    - JIT arg-buffer, effect-handler, and handle-state copy allocation sites
      now reject oversized element counts before byte-size multiplication.
    - interpreter macro/module/handler table growth, env binding growth, and
      method-table growth now check doubling and allocation sizes before
      mutating tables.
    - validation:
      - `c3c build --warn-deprecation=no`
      - bounded deduce slice: `pass=330 fail=0`
      - bounded allocator-validation slice: `pass=1 fail=0`
      - bounded advanced collections/module group: `pass=134 fail=0`
      - bounded advanced effect-continuation group: `pass=56 fail=0`
      - bounded advanced runtime-control group: `pass=22 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- [x] `AUDIT-RUNTIME-MODULE-EXPORT-GROWTH-FAILCLOSED-040` make module
  export-table growth fail closed instead of writing through failed
  replacement allocation
  - closure evidence:
    - `src/lisp/jit_jit_module_setup_helpers.c3`
      now routes export allocation/growth through a checked helper and
      preserves existing exports on growth failure.
    - `src/lisp/jit_jit_compile_effects_modules.c3` and
      `src/lisp/jit_jit_module_import_setup.c3`
      now propagate export growth failure from re-export and implicit module
      export paths.
    - `src/lisp/tests_advanced_stdlib_module_groups.c3`
      now pins forced module export growth allocation failure.
    - validation:
      - `c3c build`
      - bounded advanced collections/module group: `pass=134 fail=0`

- [x] `AUDIT-CAPACITY-REALLOC-BYTE-OVERFLOW-039` guard deduce/JIT
  capacity growth before `sizeof * new_cap` arithmetic can wrap and
  under-allocate
  - closure evidence:
    - deduce relation, aggregate, delta, query-demand, transaction,
      dirty-predicate, rule-signature, relation-schema, and persisted-rule
      catalog growth helpers now fail closed before allocation byte counts
      overflow.
    - `src/lisp/jit_jit_module_setup_helpers.c3`
      now rejects oversized source-dir vector growth and path length
      increments before allocation-size arithmetic can wrap.
    - `src/lisp/tests_deduce_groups_parallel.c3`
      now pins the oversized-capacity fail-closed regression.
    - validation:
      - `c3c build`
      - bounded deduce parallel group: `pass=6 fail=0`
      - bounded scheduler slice: `pass=111 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- [x] `AUDIT-SCHEDULER-OFFLOAD-MISSING-COMPLETION-038` make non-task
  offload worker double-allocation failure wake and fail closed instead of
  stranding a blocked waiter
  - closure evidence:
    - `src/lisp/scheduler_offload_worker.c3`
      now publishes or directly handles readiness even when both worker
      completion and fallback alloc-failure completion are null.
    - `src/lisp/scheduler_wakeup_io.c3`
      now consumes active completed-null offload slots as
      `"offload: missing completion"` and clears the pending slot.
    - `src/lisp/tests_scheduler_boundary_offload_payload_groups.c3`
      now pins the completed-null wakeup/consume path.
    - validation:
      - `c3c build`
      - bounded scheduler slice: `pass=111 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- [x] `AUDIT-FORMAT-DISPLAY-TEMP-BUILDER-FAILCLOSED-037` fix `%s`
  formatting display path so plain `(format "%s" value)` does not fail with
  `"format: failed to grow temporary builder"` for ordinary values
  - closure evidence:
    - `src/lisp/prim_string_format_helpers.c3`
      now computes `StringVal` target capacity through checked overflow
      addition instead of comparing normal small appends against `usz.max`.
    - `src/lisp/tests_advanced_core_unicode_groups.c3`
      now pins both `(format "%s" nil)` and `(format "%s" (Void))`.
    - validation:
      - `c3c build`
      - bounded `advanced` slice with
        `OMNI_ADVANCED_GROUP_FILTER=advanced-unicode-iterator`: `pass=129 fail=0`
      - bounded `memory-lifetime-smoke`: `pass=189 fail=0`
      - bounded ASAN `memory-lifetime-smoke`: `pass=189 fail=0`
      - direct JSON REPL probes for `(format "%s" nil)`,
        `(format "%s" (Void))`, and a long `%s` string that requires growth

- [x] `AUDIT-RUNTIME-EFFECT-PUBLICATION-FAILCLOSED-036` make effect
  publication/dispatch fail closed when payload or continuation materialization
  fails, instead of degrading payload shape or null-dereferencing
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now makes handled raises reject payload-construction failure before
      handler bind instead of degrading to a message-only handled raise.
    - `src/lisp/value_interp_continuation_helpers.c3`
      now exposes a narrow continuation-allocation failure seam instead of
      dereferencing failed root-scope allocation.
    - `src/lisp/jit_jit_handle_signal.c3`,
      `src/lisp/jit_jit_runtime_effects_handle.c3`,
      `src/lisp/jit_jit_reset_shift.c3`, and
      `src/lisp/jit_jit_runtime_effects_reset_shift.c3`
      now fail closed on continuation allocation failure in handled effect and
      capture dispatch.
    - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
      now returns `"runtime effect payload: out of memory"` when
      unhandled-effect diagnostic payload construction cannot complete,
      instead of silently dropping the payload.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` and
      `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now pin both handled-raise payload construction failure and handled
      effect continuation allocation failure directly in bounded slices.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=pending-raise-payload-alloc-failure,handle-continuation-alloc-failure ./build/main --test-suite lisp'`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-RUNTIME-LIST-MATERIALIZATION-FAILCLOSED-035` make helper-owned
  list materializers fail closed instead of continuing after cons-constructor
  faults
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now exposes a shared `make_cons_or_error(...)` helper with a narrow
      nth-failure seam for deterministic runtime list-builder tests.
    - `src/lisp/prim_string_transform.c3`
      now makes `string-split` reject internal result-list construction
      failure instead of continuing with partial/null list state.
    - `src/lisp/prim_io_file.c3`
      now makes `read-lines` reject internal result-list construction failure
      instead of continuing with partial/null list state.
    - `src/lisp/prim_collection_hashmap_key_helpers.c3`
      now makes `keys` / `values` canonical list assembly fail closed in both
      sorted and fallback paths instead of reusing raw `make_cons(...)`.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now directly pins string-split and hashmap key/value list allocation
      failure in the bounded runtime alloc lane.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-JIT-HELPER-ARG-CONSTRUCTION-FAILCLOSED-034` make JIT helper
  arg-list and variadic rest-list construction fail closed instead of passing
  raw cons-constructor faults into normal dispatch/binding flows
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now exposes a checked `make_list1_or_error(...)` helper with a narrow
      nth-failure seam for deterministic JIT variadic-rest tests.
    - `src/lisp/jit_jit_apply_helpers.c3` and
      `src/lisp/jit_jit_apply_runtime.c3`
      now reject variadic zero-fixed-arg rest-list construction failure before
      binding the rest parameter environment.
    - `src/lisp/jit_jit_dispatch_helpers.c3`
      now routes instance `ref` dispatch arg-list materialization through the
      shared checked two-item helper instead of nesting raw `make_cons(...)`.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now directly pins the variadic rest-list allocation failure in the
      `jit-policy` slice.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=variadic-rest-list-alloc-failure ./build/main --test-suite lisp'`

- [x] `AUDIT-JIT-QUASIQUOTE-CONS-FAILCLOSED-033` make JIT quasiquote pair
  construction fail closed instead of wrapping cons-constructor faults as
  successful quasiquote values
  - closure evidence:
    - `src/lisp/jit_jit_quasiquote_macros.c3`
      now routes all internal quasiquote pair construction through one checked
      helper with a narrow nth-failure seam, and returns
      `"quasiquote: failed to allocate pair"` on allocation failure instead of
      passing raw cons-constructor faults through `eval_ok(...)`.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now directly pins both nested quasiquote and list quasiquote pair
      materialization failure in the `jit-policy` slice.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=quasiquote-cons-alloc-failure ./build/main --test-suite lisp'`

- [x] `AUDIT-JIT-MULTI-ARG-LIST-FAILCLOSED-032` make JIT multi-arg call
  construction and iterative apply fail closed on malformed arg-list state
  instead of degrading to partial success
  - closure evidence:
    - `src/lisp/jit_jit_apply_runtime.c3`
      now rejects `make_cons(...)` failure while constructing continuation-safe
      multi-arg call lists instead of passing malformed arg lists downstream.
    - `src/lisp/jit_jit_apply_multi_prims.c3`
      now makes `jit_apply_multi_args_iterative(...)` return
      `"arg list too short"` when the arg list breaks before all required args
      are consumed, instead of breaking and returning the partial result.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now directly pins the malformed multi-arg list case in `jit-policy`
      using a two-arg curried closure and a one-element arg list.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=multi-arg-malformed-list-fails-closed ./build/main --test-suite lisp'`

- [x] `AUDIT-TWO-ARG-LIST-MATERIALIZATION-FAILCLOSED-031` make shared
  two-value list/arg materialization fail closed instead of publishing cons
  constructor failures as ordinary runtime data
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now exposes one checked `make_list2_or_error(...)` helper with a narrow
      nth-failure seam for deterministic runtime/JIT constructor tests.
    - `src/lisp/prim_system.c3`
      now makes `(shell cmd true)` fail closed with
      `"shell: failed to construct result list"` if the final two-item result
      list cannot be built.
    - `src/lisp/jit_jit_runtime_effects_handle.c3`
      now routes both pending-raise and normal effect-handler arg-pair
      construction through the same checked helper, so constructor failure
      propagates before handler call-through.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins the shell result-list construction seam.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now directly pins the pending-raise and signal-handler arg-pair seam in
      the `jit-policy` slice.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=handler-arg-list-alloc-failure ./build/main --test-suite lisp'`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEDULER-BATCH-RESULT-LIST-FAILCLOSED-030` make scheduler
  batch primitives fail closed when result-list cons construction fails instead
  of publishing partial success or leaking spawned task state
  - closure evidence:
    - `src/lisp/scheduler_primitives_threads.c3`
      now routes batch thread result-list assembly through one checked
      scheduler-local prepend helper with a narrow nth-failure seam.
    - `src/lisp/scheduler_primitives_offload_execute.c3`
      now makes offload batch result-list construction return a typed
      `"offload: out of memory"` error instead of publishing a partial result
      list.
    - `src/lisp/scheduler_primitives_task_spawn.c3`
      now drops already-spawned live thread-task entries if result-list
      publication fails after task creation.
    - `src/lisp/tests_scheduler_groups_more.c3`
      now pins forced result-list cons allocation failure for offload-batch,
      task-spawn-batch, and thread-spawn-batch, and proves active thread-task
      count is unchanged after the failure path.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`

- [x] `AUDIT-ITERATOR-CONS-CONSTRUCTOR-FAILCLOSED-029` make iterator
  coroutine cons construction fail closed instead of publishing constructor
  failures as data or remapping them to misleading apply errors
  - closure evidence:
    - `src/lisp/primitives_iter_coroutine.c3`
      now routes `zip` item-pair and `foldl` arg-list construction through a
      checked iterator-local cons helper with a narrow nth-failure seam.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now proves forced constructor failure makes `zip` return
      `"__iterator-zip: failed to allocate item pair"` and `foldl` return
      `"__iterator-foldl: failed to allocate call args"` instead of embedding
      `ERROR` values into iterator data or degrading to `"arg list too short"`.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-PENDING-RAISE-PAYLOAD-FAILCLOSED-028` make pending raise
  payload/message materialization fail closed instead of binding constructor
  failures as ordinary handler data
  - closure evidence:
    - `src/lisp/value_constructors.c3`
      now rejects `boundary_promote_to_root(...)` null/error results before
      publishing pending raise payload state.
    - `src/lisp/jit_jit_runtime_effects_handle.c3`
      now rejects raise fallback `make_string(...)` and arg-pair
      `make_cons(...)` failure before handler call-through.
    - `src/lisp/jit_jit_handle_signal_handle.c3`
      now rejects raise fallback string materialization failure before clause
      env extension.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now proves the surface `handle` form returns a top-level eval error
      instead of invoking the raise clause when pending raise message
      materialization fails.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now pins the same fail-closed contract in the `jit-policy` slice.
    - validation:
      - `c3c build`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=pending-raise-string-alloc-failure ./build/main --test-suite lisp`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEDULER-OS-THREAD-NULL-COMPLETION-FAILCLOSED-027` make
  `scheduler_complete_os_thread(...)` drop+wake instead of stranding a running
  OS-thread entry when both completion materialization and alloc-failure
  completion materialization fail
  - closure evidence:
    - `src/lisp/scheduler_thread_task_transition_scaffold.c3`
      now exposes a narrow transition-completion allocation fail seam for
      deterministic scheduler boundary tests.
    - `src/lisp/scheduler_thread_task_transitions.c3`
      now drops the OS-thread entry on null completion plus alloc-failure
      completion OOM instead of returning with the entry still running.
    - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`
      now pins the double-failure seam and proves the blocked waiter wakes,
      join token clears, and the OS-thread entry is removed.
    - `src/lisp/tests_scheduler_groups.c3`
      now wires the new boundary regression into the bounded scheduler slice.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-ITERATOR-MALFORMED-TAIL-FAILCLOSED-026` make iterator
  terminal and coroutine helpers reject malformed pair tails instead of
  truncating pipelines as successful completion
  - closure evidence:
    - `src/lisp/primitives_iter_state.c3`
      now exposes one shared `iterator_tail_or_error(...)` helper for
      fail-closed iterator tail validation.
    - `src/lisp/primitives_iter_terminal.c3`
      now makes `collect` / `to-array` reject malformed iterator pairs and
      malformed iterator tails instead of silently truncating the result.
    - `src/lisp/primitives_iter_coroutine.c3`
      now makes `map`, `filter`, `take`, `zip`, and `foldl` reject malformed
      iterator tails instead of truncating or deferring broken state as normal
      completion.
    - `src/lisp/tests_advanced_core_unicode_groups.c3`
      now pins malformed-tail rejection through the surface `List`, `Array`,
      `map`, `filter`, `take`, `zip`, and `foldl` iterator pipelines.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc "rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib ./build/main --eval \"(handle (List (Iterator (lambda () (cons 1 2)))) (raise msg (ref msg 'message)))\""`
      - `scripts/run_validation_container.sh bash -lc "rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib ./build/main --eval \"(handle (foldl (lambda (a x) (+ a x)) 0 (Iterator (lambda () (cons 1 9)))) (raise msg (ref msg 'message)))\""`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEDULER-WAKEUP-PUBLISH-FALLBACK-025` make scheduler timer,
  sleep, poll-error, and non-task offload wakeups fail closed when reliable
  wakeup enqueue fails instead of dropping the blocked-fiber completion
  - closure evidence:
    - `src/lisp/scheduler_wakeup_callbacks.c3`
      now makes timer, sleep, and poll-error callbacks fall back to the same
      direct wakeup handlers on reliable queue publish failure.
    - `src/lisp/scheduler_offload_worker.c3`
      now makes non-task worker completion fall back to
      `scheduler_handle_wakeup_offload_ready(...)` on publish failure instead
      of freeing the live completion payload.
    - `src/lisp/tests_scheduler_groups_more.c3`
      now pins the real enqueue-failure seam for timer, sleep, poll-error,
      and offload-after fallback.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEDULER-SHARED-PROJECTION-FAILCLOSED-024` make scheduler
  shared-handle and offload-path projection fail closed instead of publishing
  empty-string or false-success results
  - closure evidence:
    - `src/lisp/scheduler_state_shared_handles.c3`
      now makes `scheduler_project_shared_to_local_value(...)` return
      scheduler `ERROR`s for missing handle refs and shared-payload
      materialization failure instead of an empty string.
    - `src/lisp/scheduler_offload_ops.c3`
      now makes `scheduler_offload_read_file(...)` and
      `scheduler_offload_file_exists(...)` report `OFFLOAD_RES_ERROR` for
      missing/invalid projected path payloads instead of synthesizing
      `nil`/`0` success results.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins both the direct shared-handle projection failure and the
      offload-path projection failure family.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-CONS-ESCAPE-PROMOTION-FAILCLOSED-023` make escape-lane cons
  publication fail closed when string/error promotion cannot actually move the
  field into the ESCAPE lane
  - closure evidence:
    - `src/lisp/value_constructors_core.c3`
      now stages `car` / `cdr` escape promotion before pair allocation,
      rejects null or `ERROR` promotion results, rejects the string/error
      case where promotion falls back to the original non-escape value, and
      unwinds staged promoted fields if final pair allocation fails.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins the forced string/error escape-promotion seam and proves
      `make_cons(...)` returns a typed error instead of publishing an
      ESCAPE-lane cons that still points at a TEMP string.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-DATA-FORMAT-ARRAY-PROMOTION-FAILCLOSED-022` make JSON/TOML
  array assembly reject promoted boundary `ERROR` values instead of storing
  them as ordinary array elements
  - closure evidence:
    - `src/lisp/json.c3`
      now rejects `boundary_promote_to_root(...)` results that come back as
      `ERROR` values during JSON array assembly instead of publishing them
      into successful arrays.
    - `src/lisp/primitives_toml_bridge.c3`
      now applies the same fail-closed rule for TOML array element promotion.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins a TOML array-element promotion fault through the existing
      `TIME_POINT` wrapper-copy allocation seam under a non-root scope and
      proves the array conversion returns the boundary error directly.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-COROUTINE-THUNK-PROMOTION-FAILCLOSED-021` make coroutine thunk
  publication fail closed when root promotion returns an `ERROR` or invalid
  callable state instead of allocating coroutine stack context around a bad
  thunk
  - closure evidence:
    - `src/lisp/primitives_coroutine.c3`
      now makes both `prim_coroutine_prepare_thunk(...)` and
      `prim_coroutine_create_ctx(...)` reject:
      - null promotion results,
      - promoted `ERROR` values,
      - and non-closure / null-closure thunk state
      before any stack context or coroutine wrapper allocation.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins the forced closure-wrapper promotion-allocation seam and proves
      coroutine construction aborts before `stack_ctx_pool` allocation
      counters change.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-APPLY-DISPATCH-FAILCLOSED-020` make malformed primitive and
  partial-application helper paths fail closed instead of dereferencing null
  call targets or silently consuming invalid state
  - closure evidence:
    - `src/lisp/eval_apply.c3`
      now rejects invalid primitive application state when the primitive
      wrapper is null, wrongly tagged, missing `prim_val`, or missing
      `prim_val.func`, and rejects partial-application state when
      `first_arg == null`.
    - `src/lisp/jit_jit_apply_helpers.c3`
      now makes `jit_apply_value_primitive(...)` reject malformed primitive
      wrappers before any function-pointer call-through.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins invalid partial state and invalid primitive-wrapper execution in
      the direct runtime helper lane.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      now pins malformed primitive wrapper rejection in the JIT helper lane.
    - validation:
      - `c3c build`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=invalid-primitive-state-fails-closed ./build/main --test-suite lisp`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-APPLY-PROMOTION-FAILCLOSED-019` make partial-application state
  and root-promotion array/hashmap helper surfaces fail closed instead of
  executing impossible state or storing promoted `ERROR` values as ordinary
  data
  - closure evidence:
    - `src/lisp/eval_apply.c3`
      now rejects invalid `PARTIAL_PRIM` state before call-through:
      null/non-partial input, null function pointer, `remaining <= 0`,
      `remaining > 2`, and the inconsistent `remaining == 2` with a prefilled
      `second_arg`.
    - `src/lisp/prim_collection_hashmap.c3`
      now makes `hashmap_set_symbol_checked(...)` and
      `hashmap_set_checked(...)` reject `boundary_promote_to_root(...)`
      results that come back as `ERROR` values instead of inserting them.
    - `src/lisp/prim_io_fs_handles.c3` and
      `src/lisp/primitives_data_formats_csv_parse.c3`
      now make `fs_array_push(...)` and `csv_array_push(...)` reject promoted
      `ERROR` values instead of appending them into successful arrays.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins:
      - invalid partial-application state,
      - checked hashmap insertion under opaque primitive promotion failure,
      - and `fs` / CSV array helper pushes under the same promoted-error seam.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-COLLECTION-ARRAY-FAILCLOSED-018` make collection/apply array
  write helpers fail closed on boundary promotion faults, grow failures, and
  comparator runtime errors
  - closure evidence:
    - `src/lisp/prim_collection_sort_array.c3`
      now makes `sort` / `sort-by` list rebuilding propagate `make_cons(...)`
      failure directly, makes `sort-by` propagate comparator application
      errors instead of silently returning a partial sort, makes `array`,
      `list->array`, `set!` on arrays, and `push!` reject
      `boundary_promote_to_root(...)` failures instead of storing `ERROR`
      values as data, and makes `push!` fail closed on grow allocation
      failure instead of null-dereferencing the new item buffer.
    - `src/lisp/primitives_iter_terminal.c3`
      now makes `collect` propagate list-construction failure directly and
      makes `to-array` reject `boundary_promote_to_root(...)` failures
      instead of returning arrays populated with `ERROR` elements.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins array constructor/mutator and `to-array` boundary-promotion
      failure, `push!` grow failure, and `sort-by` comparator failure.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-STRING-PAYLOAD-MATERIALIZERS-017` make string-backed runtime
  payload/list helpers fail closed on string wrapper allocation failure
  - closure evidence:
    - `src/lisp/prim_system.c3`
      now makes `(shell cmd true)` propagate stdout string construction
      failure directly instead of returning a success-shaped `(ERROR
      exit-code)` list.
    - `src/lisp/prim_io_fs_handles.c3`
      now makes `fs-readdir` propagate entry-name string construction failure
      directly instead of storing `ERROR` values as directory entries in a
      successful array.
    - `src/lisp/http.c3`
      now makes `http-get` / `http-request` propagate host/request string
      materialization failure directly before transport setup/write.
    - `src/lisp/schema_validation.c3`
      now makes `schema-explain` propagate failure of its singleton message
      string instead of returning a one-element list containing `ERROR` as
      ordinary data.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins `shell`, `fs-readdir`, and `schema-explain` under forced
      `make_string(...)` allocation failure.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-STRING-LIST-MATERIALIZERS-016` make pure string/list helper
  surfaces fail closed on per-element string wrapper allocation failure
  - closure evidence:
    - `src/lisp/prim_string_transform.c3`
      now makes `string-upcase` / `string-downcase` return constructor
      `ERROR`s directly and makes `string-split` propagate per-part
      string-allocation failure.
    - `src/lisp/prim_string_ops.c3`
      now makes `string->list` propagate per-character string wrapper
      allocation failure instead of embedding an `ERROR` into a successful
      list.
    - `src/lisp/unicode.c3`
      now makes `string-graphemes` propagate grapheme-cluster string
      construction failure directly instead of storing `ERROR` values in the
      cluster array/list.
    - `src/lisp/prim_io_file.c3`
      now makes `read-lines` propagate per-line string-construction failure
      directly.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins `string-upcase`, `string-downcase`, `string->list`,
      `string-split`, and `string-graphemes` under forced
      `make_string(...)` allocation failure.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-SCHEMA-EXPLAIN-LIST-BUILDERS-015` make schema-explain list
  assembly fail closed instead of hard-aborting on internal cons allocation
  failure
  - closure evidence:
    - `src/lisp/schema_explain_payload_helpers.c3` now routes list
      accumulation/reversal through `explain_prepend_or_oom(...)` instead of
      raw `make_cons(...)`.
    - `src/lisp/schema_explain_helpers.c3`,
      `src/lisp/schema_explain_effect_helpers.c3`, and
      `src/lisp/schema_explain_effect_runtime.c3`
      now propagate that failure through dispatch candidates, handler tag
      lists, and effect candidates.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      now pins helper-level and top-level schema-explain list-builder OOM
      seams through a dedicated local `nth` fail seam.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-COLLECTION-MUTATOR-CHECKED-RETURNS-014` make dictionary/set
  mutation fail closed on backing-storage grow failure instead of silently
  dropping writes behind void mutator wrappers
  - closure evidence:
    - `src/lisp/prim_collection_hashmap.c3` now makes
      `hashmap_set_symbol(...)`, `hashmap_grow(...)`, and `hashmap_set(...)`
      return checked `bool` results instead of discarding insertion/grow
      failure.
    - `src/lisp/prim_collection_hashmap.c3` now makes `set!` on dictionary
      targets return `runtime/out-of-memory` when backing-storage growth
      fails, instead of returning `Void` after a dropped write.
    - `src/lisp/prim_collection_generic_set.c3` now makes `set-add` follow
      the same checked mutator contract for `SET`.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves
      both mutators fail with typed errors and leave the failed key absent
      from the target collection after grow failure.
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- [x] `AUDIT-DEDUCE-EXPLAIN-INSERT-FAILCLOSED-013` make deduce explain,
  analyze, schema, stats, and why-result payload builders propagate checked
  `explain_dict_set*` failures instead of returning partially populated maps
  - closure evidence:
    - `src/lisp/deduce_why_result_payload.c3`,
      `src/lisp/deduce_why_result_path_payload.c3`,
      `src/lisp/deduce_why_result_lookup.c3`, and
      `src/lisp/deduce_why_result_lookup_derived.c3`
      now return the first insertion/grow failure from path/context/payload
      attachment instead of silently dropping the failed field and returning a
      successful why-result payload.
    - `src/lisp/deduce_rule_eval_analyze_payload_fields.c3`,
      `src/lisp/deduce_rule_eval_analyze_payload_tail.c3`, and
      `src/lisp/deduce_rule_eval_analyze_payload_result.c3`
      now treat payload-field insertion failure as a first-class
      `deduce/analyze` error instead of ignoring the failed write and
      continuing with a partial result map.
    - the remaining deduce explain/schema/stats helper family now follows the
      same checked insertion contract:
      - `src/lisp/deduce_parallel_runtime_truth.c3`
      - `src/lisp/deduce_rule_ops_explain_goal_directed.c3`
      - `src/lisp/deduce_rule_ops_explain_goal_directed_components.c3`
      - `src/lisp/deduce_rule_ops_explain_plan_payload.c3`
      - `src/lisp/deduce_rule_ops_explain_plan_steps.c3`
      - `src/lisp/deduce_rule_ops_explain_projection.c3`
      - `src/lisp/deduce_rule_ops_explain_snapshot.c3`
      - `src/lisp/deduce_rule_ops_explain_step_counters.c3`
      - `src/lisp/deduce_schema_query_metadata_schema_helpers.c3`
      - `src/lisp/deduce_schema_query_metadata_schema_payloads.c3`
      - `src/lisp/deduce_schema_query_metadata_stats_parallel_fields.c3`
      - `src/lisp/deduce_schema_query_metadata_stats_payload.c3`
      - `src/lisp/deduce_schema_query_metadata_stats_tail.c3`
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'` -> `pass=328 fail=0`

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-GUARDED-HASHMAP-CALLERS-012B` normalize the remaining already-guarded `make_hashmap(...)` caller family onto checked constructors or a shared fail-closed helper contract
  - closure evidence:
    - `src/lisp/deduce_relation_row_materialization.c3` now uses checked
      hashmap construction plus checked insertion for row-dict materialization.
    - `src/lisp/deduce_relation_ops_validation_payload.c3` now routes
      integrity payload maps through a checked helper and drops the payload
      cleanly when insertion fails instead of returning partially populated
      dicts.
    - deduce runtime helper state maps in:
      - `src/lisp/deduce_rule_eval_exec_component_state_helpers.c3`
      - `src/lisp/deduce_rule_eval_exec_component_state.c3`
      - `src/lisp/deduce_rule_eval_exec_aggregate_state.c3`
      - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
      - `src/lisp/deduce_rule_eval_scc.c3`
      - `src/lisp/deduce_relation_scan_helpers_join.c3`
      - `src/lisp/deduce_rule_eval_analyze_setup.c3`
      - `src/lisp/deduce_rule_eval_fixpoint_goal_directed_selector_prepare.c3`
      now all use checked constructor/insertion paths.
    - the remaining deduce explain/schema/analyze and why-result payload dict
      families no longer use raw `make_hashmap(...)` either:
      - `rg -n "make_hashmap\\(" src/lisp/deduce_* src/lisp/unify_* -S`
        returns no matches.
    - regressions now pin:
      - helper-state constructor OOM in
        `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
      - integrity payload-map OOM degradation in
        `src/lisp/tests_deduce_groups_integrity.c3`
      - why-result payload/path OOM in
        `src/lisp/tests_deduce_query_groups.c3`
    - validation:
      - `c3c build`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'` -> `pass=328 fail=0`

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-RAW-HASHMAP-CRASHERS-012A` close the direct raw-hashmap caller class that still dereferenced payloads without constructor checks
  - closure evidence:
    - `src/lisp/unify_match_helpers.c3` now routes `build_result_dict(...)`
      through checked hashmap construction and checked insertion, and returns
      `deduce/match-out-of-memory` on constructor or insertion failure.
    - `src/lisp/unify_scan_helpers.c3` now propagates that builder `ERROR`
      directly instead of embedding it into a successful result list.
    - `src/lisp/tests_deduce_query_groups.c3` now proves `deduce 'match`
      propagates result-dict constructor OOM directly.
    - the residual raw-hashmap backlog is now just the already-guarded
      normalization family:
      - `AUDIT-COLLECTION-CONSTRUCTOR-GUARDED-HASHMAP-CALLERS-012B`

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-RAW-ARRAY-AOT-011A` close raw array constructor and AOT dict payload fail-closed gaps
  - closure evidence:
    - `src/lisp/value_predicates_accessors_basic.c3` now routes
      `make_array(...)` through the checked array constructor path instead of
      raw unchecked allocation.
    - `src/lisp/prim_collection_sort_array.c3` now propagates array
      constructor `ERROR`s from `array(...)` and `list->array(...)` instead of
      dereferencing a partially initialized wrapper.
    - `src/lisp/aot_runtime_bridge.c3` now routes `dict_from_args(...)`
      through checked hashmap construction and checked insertion, so bridge
      payload creation fails closed under allocation pressure.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now pins raw
      array-constructor OOM behavior directly.
    - `src/lisp/tests_compiler_core_groups_fail_closed.c3` now pins active
      bridge-interpreter `dict_from_args(...)` hashmap-constructor OOM
      behavior directly.

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-RUNTIME-PAYLOADS-011` migrate remaining runtime/status payload builders off unchecked collection constructors
  - closure evidence:
    - `src/lisp/async_process_signal_dns_process.c3`,
      `src/lisp/async_process_spawn.c3`, and
      `src/lisp/prim_io_fs_handles.c3`
      now route runtime status payload builders through checked `HASHMAP` /
      `ARRAY` constructors and checked hashmap insertion instead of mutating
      unchecked constructor results.
    - `src/lisp/http_url_response.c3` now constructs parsed HTTP response
      payload maps through the same checked contract.
    - `process-spawn` now also closes its live process/fs handles if final
      success-payload map construction fails, so constructor OOM cannot strand
      a half-built success-shaped result with open resources.
    - `src/lisp/eval_dispatch_error_payloads.c3` now treats lambda and
      ambiguous-dispatch payload dictionaries as optional under OOM: the
      primary typed error still returns even if payload-map construction or
      insertion fails.
    - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now applies
      the same optional-payload contract to unhandled-effect error payloads.
    - `src/lisp/primitives_meta_types_ctor_helpers.c3` now makes
      `ctor_mismatch_data(...)` fail closed by returning `null` instead of
      dereferencing unchecked hashmap payloads.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now pins fs,
      process-spawn, process-wait, HTTP response payload, dispatch-payload,
      runtime-effect payload, and ctor-mismatch constructor OOM paths directly.
    - no same-lane residual callsites remain from the staged runtime/status
      payload-builder family.

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-SCHEMA-EXPLAIN-010` migrate schema explain payload builders onto checked collection-constructor OOM contracts
  - closure evidence:
    - `src/lisp/schema_explain_payload_helpers.c3` now centralizes checked map
      construction and checked `explain_dict_set*` insertion through one
      explicit `"schema explain: out of memory"` contract.
    - `src/lisp/schema_explain_helpers.c3`,
      `src/lisp/schema_explain_effect.c3`,
      `src/lisp/schema_explain_effect_result_payload.c3`,
      `src/lisp/schema_explain_effect_runtime.c3`, and
      `src/lisp/schema_explain_effect_helpers.c3`
      now route entrypoint/result/candidate/source payload maps through that
      checked path instead of dereferencing unchecked `make_hashmap(...)`
      results.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves:
      - dispatch explain result construction fails closed on map OOM,
      - effect explain result construction fails closed on map OOM,
      - helper payload/source maps fail closed on map OOM.
    - residual unchecked collection-constructor work is now just the separate
      runtime/status payload-builder lane:
      - `AUDIT-COLLECTION-CONSTRUCTOR-RUNTIME-PAYLOADS-011`
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=134 fail=0`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=134 fail=0`

- [x] `AUDIT-COLLECTION-CONSTRUCTOR-CALLSITE-MIGRATION-009` close the data-format bridge slice of internal collection-constructor OOM hardening and split the residual backlog by real callsite family
  - closure evidence:
    - `src/lisp/json.c3` and `src/lisp/primitives_toml_bridge.c3` now use
      checked `ARRAY` / `HASHMAP` constructors plus checked hashmap insertion
      in their recursive decode paths.
    - nested conversion `ERROR`s in those files now propagate directly instead
      of being embedded into partial arrays/dicts.
    - `src/lisp/primitives_data_formats_csv_parse.c3` now uses checked row and
      result-array constructors and propagates constructor/cell materialization
      errors directly.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves JSON,
      TOML, and CSV constructor OOM paths fail closed.
    - the broad umbrella item is now split into:
      - `AUDIT-COLLECTION-CONSTRUCTOR-SCHEMA-EXPLAIN-010`
      - `AUDIT-COLLECTION-CONSTRUCTOR-RUNTIME-PAYLOADS-011`
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=133 fail=0`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=133 fail=0`

- [x] `AUDIT-ITERATOR-TAIL-ERROR-PROPAGATION-008` stop iterator tail-construction faults from degrading into silent truncation
  - closure evidence:
    - `src/lisp/primitives_iter_state.c3` now routes `(item . next)` iterator
      pair construction through `iterator_make_pair_or_propagate(...)`, which
      returns tail `ERROR` values directly instead of wrapping them in `CONS`.
    - source iterator thunks in `src/lisp/primitives_iter_sources.c3` and
      coroutine/transform thunks in `src/lisp/primitives_iter_coroutine.c3`
      now share that helper, so tail constructor failure no longer looks like
      normal iterator completion/truncation.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves both
      a source thunk (`repeat`) and a coroutine thunk (`take`) propagate the
      tail allocation error directly.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=128 fail=0`

- [x] `AUDIT-RUNTIME-CONSTRUCTOR-OOM-SUBSTRATE-008` harden shared runtime error/collection constructor paths that live iterator/error helpers depend on
  - closure evidence:
    - `src/lisp/value_core_types.c3`,
      `src/lisp/value_interp_alloc_helpers.c3`,
      `src/lisp/value_constructors_lifecycle.c3`, and
      `src/lisp/primitives_meta_types.c3`
      now track whether `STRING` / `ERROR` chars are heap-owned, so fallback
      literal-backed error values no longer flow into invalid frees during
      normal teardown or `unsafe-free`.
    - `src/lisp/value_constructors.c3` now makes `make_error(...)` fail closed
      when its message buffer allocation fails.
    - `src/lisp/value_predicates_accessors_basic.c3` and
      `src/lisp/prim_collection_hashmap.c3`
      now expose checked `ARRAY` / `HASHMAP` / `SET` constructor and grow
      helpers, and the live runtime-dependent surfaces now use them:
      - raise payload construction
      - `Dictionary`
      - `Set`
      - `to-array`
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now pins the
      exact constructor seams for:
      - printable `make_error(...)` fallback
      - iterator ctor raise payload-map failure
      - `to-array` result-array failure
      - checked collection constructor/grow failure
    - residual broader internal constructor migration is now split into
      `AUDIT-COLLECTION-CONSTRUCTOR-CALLSITE-MIGRATION-009` instead of being
      left implicit.

- [x] `AUDIT-STRING-BUILDER-OOM-007` harden shared `StringVal` builder creation and growth to fail closed
  - closure evidence:
    - `src/lisp/prim_string_format_helpers.c3` now gives `StringVal` an
      explicit fail-closed contract:
      - `strval_new(...)` returns `null` instead of dereferencing failed
        allocations,
      - `strval_ensure(...)` returns `bool`, guards size overflow, and marks
        builder failure on grow failure,
      - `strval_push(...)` / `strval_append(...)` / padding helpers now stop
        writing after a failed growth attempt,
      - deterministic seams were added for initial builder allocation and
        builder growth failure.
    - parser string literal construction paths in
      `src/lisp/parser_datum_helpers.c3`,
      `src/lisp/parser_expr_atoms.c3`,
      `src/lisp/parser_patterns_values.c3`, and
      `src/lisp/parser_quasiquote_datum_helpers.c3`
      now share the checked builder path and set parser errors instead of
      dereferencing a failed builder allocation.
    - runtime string helpers in
      `src/lisp/prim_string_ops.c3`,
      `src/lisp/prim_string_format.c3`, and
      `src/lisp/prim_string_format_directives.c3`
      now fail closed on builder creation/growth failure instead of writing
      through invalid builder buffers.
    - `src/lisp/primitives_meta_types.c3` no longer uses unchecked `StringVal`
      allocation in the `unsafe-free` error path.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=127 fail=0`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp'` -> `pass=191 fail=0`

- [x] `AUDIT-JIT-POLICY-FULL-SLICE-006` isolate and close the remaining non-continuation `jit-policy` slice crash
  - closure evidence:
    - the crash was isolated to the `stale-raise-scrub` JIT policy case, but
      the actual fault site was the TCO recycle TEMP-graph scan in
      `src/lisp/jit_jit_eval_scope_chain_helpers.c3`, not stale raise state.
    - `jit_graph_binding_reaches_temp_scope(...)` no longer allocates four
      `4096`-entry pointer arrays on the runtime stack; it now uses one
      heap-backed `JitTempGraphScan`, closing the entry-time stack-overflow
      crash on smaller runtime stacks.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=stale-raise-scrub ./build/main --test-suite lisp'` -> `1 passed, 0 failed`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp'` -> `pass=41 fail=0`

## Recently Closed

- [x] `AUDIT-BOUNDARY-WRAPPER-SLOT-LEAK-005` eliminate pre-dtor wrapper-slot leaks on partial-abort copy/root-store paths
  - closure evidence:
    - `src/lisp/eval_promotion_copy_route_helpers.c3`,
      `src/lisp/eval_promotion_root_clone_basic.c3`, and
      `src/lisp/eval_promotion_root_clones.c3` now allocate/register the
      destination wrapper only after all fallible child-copy and payload clone
      work succeeds.
    - wrapper-allocation failure after payload success now routes through the
      existing partial-cleanup helpers, so already-copied child retains and
      heap payloads are unwound before returning.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves repeated
      failed shared-wrapper copy attempts do not grow the surviving target
      scope allocation count.
    - `src/lisp/tests_memory_lifetime_root_boundary_groups.c3` now proves the
      same invariant for repeated failed root-store method-table clone
      attempts against `root_scope`.
    - validation:
      - `c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=106 fail=0`
      - `scripts/check_status_consistency.sh` -> pass

- [x] `AUDIT-BOUNDARY-DESTINATION-CTX-005` route direct destination escape promotion through the caller promotion context
  - closure evidence:
    - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` now routes
      direct destination escape promotion through a shared ctx-aware helper
      instead of silently falling back to `interp.active_promotion_ctx`.
    - `src/lisp/eval_boundary_commit_escape_helpers.c3` and
      `src/lisp/eval_boundary_commit_destination.c3` now use that helper for
      releasing-scope retry, mixed-destination retry, and direct destination
      promotion, so destination commit stays inside the caller-owned
      memo/budget/abort epoch.
    - destination-builder teardown now restores both `memo_head` and the
      small scope-chain cache snapshot, so temporary build-scope cache entries
      cannot survive after the builder returns or aborts.
    - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now
      proves:
      - repeated builder calls do not retain builder-local scope-chain cache
        state, and
      - a non-active caller `PromotionContext` still receives the abort state
        from direct destination promotion while the unrelated active context
        remains untouched.
    - validation:
      - `c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> pass
      - `scripts/check_status_consistency.sh` -> pass

- [x] `AUDIT-BOUNDARY-DESTINATION-MEMO-004` define correct promotion-context memo semantics for destination builders
  - closure evidence:
    - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` now makes the
      shipped contract explicit: memo entries remembered while routing nested
      children inside temporary destination build scopes are builder-local and
      are discarded when the builder returns or aborts.
    - `src/lisp/eval_boundary_commit_escape_cons.c3` and
      `src/lisp/eval_boundary_commit_escape_wrappers.c3` now route that
      save/restore policy through shared helpers instead of leaving it as an
      implicit per-builder pattern.
    - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now
      proves repeated destination-builder calls in one promotion epoch do not
      retain child memo entries after return and therefore materialize fresh
      destination graphs instead of reusing transient builder-local memo state.
    - validation:
      - `c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=90 fail=0`

- [x] `AUDIT-BOUNDARY-PROVENANCE-WRAPPER-004` re-audit target-chain wrapper reuse for nested child-owned payloads
  - closure evidence:
    - `src/lisp/eval_boundary_provenance.c3` now walks nested `ARRAY`,
      `HASHMAP` / `SET`, and `METHOD_TABLE` payload edges before admitting
      target-chain fast reuse, so the reuse classifier now agrees with the
      existing graph-audit ownership model instead of checking only the wrapper
      shell.
    - target-chain shared wrappers now fall back into the existing copy /
      ESCAPE builders whenever any nested child still lives in the releasing
      scope or outside the surviving target chain.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now pins the exact
      target-chain-wrapper regression for `ARRAY`, `HASHMAP`, `SET`, and
      `METHOD_TABLE`, and
      `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` keeps it in the
      bounded smoke lane.
    - validation:
      - `c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=89 fail=0`

- [x] `AUDIT-COMPILER-DIAGNOSTIC-PARITY-003` unify remaining JIT/AOT diagnostic drift and cover prelude-remapped parser coordinates
  - closure evidence:
    - `src/lisp/jit_jit_apply_multi_prims.c3` now emits the same canonical under-arity text as the tail/AOT helpers for both fixed multi-arg closures and variadic multi-arg closure application.
    - `src/lisp/tests_compiler_core_groups_fail_closed.c3` now directly asserts that compile-time parser failures report user-source coordinates after the stdlib prelude offset is stripped.
    - validation:
      - `rm -rf build/obj/linux-x64 build/main && c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` -> `pass=194 fail=0`
      - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-BOUNDARY-METHOD-TABLE-FAILURE-TEST-003` add deterministic coverage for method-table partial-cleanup abort lanes
  - closure evidence:
    - `src/lisp/eval_promotion_root_clones.c3` now exposes narrow abort-cleanup telemetry for partial method-table reclamation, and `src/lisp/jit_jit_closure_support.c3` now exposes a targeted heap-signature copy failure seam.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now drives both copy-to-parent and escape-promotion abort paths through that seam and proves partially copied heap signatures are reclaimed instead of leaked.
    - `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` now includes that regression in the bounded smoke lane.
    - validation:
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=83 fail=0`

- [x] `AUDIT-ASYNC-PROCESS-CONCURRENCY-003` define and harden shared process-handle concurrency semantics
  - closure evidence:
    - `src/lisp/async_process_signal_runtime.c3` now gives each process handle a shared in-flight guard, and `src/lisp/async_process_lifecycle.c3` preserves the closed state while wait/kill activity is serialized through the same contract.
    - `src/lisp/async_process_signal_dns_process.c3` now fails closed with `io/process-handle-busy` when `process-wait` / `process-kill` reuse the same live handle concurrently.
    - focused regression coverage now lives in `src/lisp/tests_advanced_io_effect_ffi_scheduler_boundary.c3` and `src/lisp/tests_advanced_io_effect_ffi_groups.c3`.
    - validation:
      - `c3c build`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp` -> pass

- [x] `HTTP-CRUD-CONCURRENT-WRITES-001` decide and harden spawned CRUD write semantics for one in-memory Deduce DB
  - closure evidence:
    - `examples/deduce_crud_server.omni` now guards CRUD mutation entrypoints with a shared atomic write gate, preserving keyed-write duplicate-id normalization while preventing spawned overlap from surfacing raw runtime `ERROR` payloads.
    - the shipped application contract is now explicit: overlapping spawned writes over one in-memory Deduce CRUD store may resolve as either one success plus `"crud write already in progress"` or one success plus `"item already exists"`, but never as an unnormalized runtime error and never with more than one persisted row for the shared id.
    - the spawned concurrency probe now runs in the focused `http-crud` slice instead of the broad `http` slice, so deterministic HTTP regressions stay stable while the concurrency lane remains exercised.
    - validation:
      - `rm -rf build/obj/linux-x64 build/main && c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp` -> `pass=29 fail=0`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http-crud ./build/main --test-suite lisp` -> `pass=1 fail=0`

- [x] `AUDIT-VALIDATION-INTEGRITY-002` repair status/validation tooling contracts
  - closure evidence:
    - `scripts/check_status_consistency.sh` now accepts the current zero-item TODO wording instead of hard-requiring one historical sentinel string.
    - `scripts/run_validation_container.sh` now serializes bounded validation runs with a repo-local lock so overlapping host/container/ASAN jobs do not corrupt the shared build tree.
    - `scripts/run_validation_status_summary.sh` now treats missing required `OMNI_TEST_SUMMARY` telemetry as a validation failure instead of trusting exit status alone.
    - `scripts/c3c_limits.sh` now preserves quoted extra Docker args instead of re-splitting them unsafely.
    - `scripts/run_e2e.sh` and `scripts/check_e2e_baseline_policy.sh` now guard Stage 3 compile-source parity explicitly against entry-build drift.
    - validation:
      - `scripts/check_status_consistency.sh` -> pass
      - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity` -> pass
      - `bash -n scripts/check_status_consistency.sh scripts/run_validation_container.sh scripts/run_validation_status_summary.sh scripts/c3c_limits.sh scripts/run_e2e.sh scripts/check_e2e_baseline_policy.sh` -> pass

- [x] `AUDIT-BOUNDARY-SHARED-WRAPPER-003` re-audit shared-wrapper boundary aliasing after fail-closed propagation landed
  - closure evidence:
    - `src/lisp/eval_promotion_copy.c3`, `src/lisp/eval_promotion_copy_route_helpers.c3`, `src/lisp/eval_promotion_escape_leaf.c3`, and `src/lisp/eval_promotion_escape_structured.c3` no longer return disjoint `ARRAY` / `HASHMAP` / `SET` / `METHOD_TABLE` wrappers by pointer identity once fast reuse is declined; they now defensively clone those wrappers and recurse through nested payload edges.
    - `METHOD_TABLE` shared-wrapper clones now keep signature arrays heap-backed so the existing value destructor contract remains sound during scope teardown.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now covers both defensive clone behavior for shared wrappers and nested fail-closed boundary-copy behavior.
    - `src/lisp/tests_memory_lifetime_finalize_groups.c3` also stops reading detached-scope env memory after release, removing the finalize-lane UAF that surfaced while validating the integrated smoke suite.
    - validation:
      - `rm -rf build/obj/linux-x64 build/main && c3c build` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=81 fail=0`
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=0:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=81 fail=0`

- [x] `AUDIT-COMPILER-PARITY-002` harden JIT multi-arg allocation failures and restore AOT/JIT parity
  - closure evidence:
    - `src/lisp/jit_jit_apply_multi_prims.c3` and `src/lisp/jit_jit_apply_multi_prims_tail.c3` now null-guard multi-arg argument-buffer allocation for primitive and method-table dispatch, with a focused test seam that fails closed instead of dereferencing `null`.
    - `src/lisp/parser_top_level_parse.c3` and `src/lisp/compiler_program_pipeline_helpers.c3` now fail closed on malformed trailing forms; they no longer return a silently truncated prefix program after parser error.
    - `src/lisp/aot.c3` now exposes shared AOT arg-list counting and arity-error helpers, and `src/lisp/compiler_code_emission_lambda_defs.c3` uses them so generated multi-arg lambdas reject under-application, preserve JIT-style over-application chaining through `aot::apply_multi(...)`, and reject malformed arg lists explicitly.
    - `src/lisp/compiler_native_call_compilation_flat_style.c3` now guards generated closure-capture allocation in flat expression lowering without emitting invalid raw `return` statements into non-`Value*` contexts.
    - focused regressions landed in:
      - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
      - `src/lisp/tests_compiler_core_groups_fail_closed.c3`
      - `src/lisp/tests_compiler_core_groups.c3`
    - validation:
      - `c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp` -> `pass=35 fail=0`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` -> `pass=193 fail=0`
      - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`

- [x] `HTTP-CRUD-DUPLICATE-POST-001` restore a deterministic green broad HTTP slice after the parser regressions landed
  - closure evidence:
    - `examples/deduce_crud_server.omni` no longer performs a check-then-insert race in `repo/create`; it now treats keyed `deduce 'fact!` as the atomic source of truth and maps `deduce/integrity-key-conflict` to the existing `"item already exists"` API result.
    - `src/lisp/tests_runtime_feature_http_groups.c3` now keeps deterministic broad-slice coverage for duplicate-id rejection using two POSTs with the same id but different payloads, rather than the order-sensitive spawned race probe.
    - the HTTP helper-method fiber smoke remains isolated from the long-lived group interpreter so the broad `http` slice no longer inherits unrelated state from earlier cases.
    - validation:
      - `c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp` -> `pass=29 fail=0`

- [x] `AUDIT-BOUNDARY-FAILCLOSED-002` make nested boundary copy/promotion fail closed for opaque primitive payloads
  - closure evidence:
    - `src/lisp/eval_promotion_copy.c3`, `src/lisp/eval_promotion_copy_route_helpers.c3`, and `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now propagate `BoundaryCopyFault` through nested `CONS` / `PARTIAL_PRIM` / `ITERATOR` copy paths, so nested opaque primitive rejection aborts transitively instead of embedding null/error payloads into rebuilt wrappers.
    - `src/lisp/eval_promotion_escape_leaf.c3` and `src/lisp/eval_promotion_escape_structured.c3` now fail closed transitively through the same wrapper shapes during ESCAPE promotion.
    - primitive-copy rejection now validates opaque payload legality before allocating the destination wrapper where practical, removing the target-scope garbage-allocation path from rejected primitive copies.
    - focused regressions landed in `src/lisp/tests_memory_lifetime_boundary_groups.c3` and `src/lisp/tests_memory_lifetime_promotion_context_groups.c3`, and the smoke suite wires them through `src/lisp/tests_memory_lifetime_groups.c3`.
    - validation:
      - `c3c build` -> pass
      - `rm -rf build/obj/linux-x64 build/main && mkdir -p build/obj/linux-x64/tmp_c_compile && c3c build --sanitize=address` -> pass
      - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=80 fail=0`
      - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-ASYNC-FFI-SAFETY-002` harden REPL worker lifecycle and handle-kind validation
  - closure evidence:
    - `src/lisp/eval_repl_server_worker.c3` now refuses to start the REPL worker thread unless both mutex and condition-variable initialization succeeded, so partial-init paths fail closed before the worker can touch uninitialized sync state.
    - `src/lisp/async_socket_handle_runtime.c3`, `src/lisp/tls_handle_lifecycle.c3`, and `src/lisp/prim_io_fs_stream.c3` now validate exact FFI handle names before casting TCP/UDP/TLS/FS payloads, closing the type-confusion path where unrelated `FFI_HANDLE` boxes could be reinterpreted as transport state.
    - `src/lisp/async_process_spawn.c3` now treats constructor-returned error values from `make_process_handle(...)` and `make_fs_handle(...)` as hard failures instead of packaging them into a success-shaped spawn result.
    - `src/lisp/http_url_response.c3` now rejects malformed `:port` suffixes with trailing garbage or missing digits, and trims HTTP response header slices so the exposed header string no longer retains delimiter residue.
    - focused regressions landed in:
      - `src/lisp/tests_runtime_async_repl_server_groups.c3`
      - `src/lisp/tests_advanced_io_effect_ffi_groups.c3`
      - `src/lisp/tests_runtime_feature_http_groups.c3`
    - validation:
      - `c3c build` -> pass
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=async ./build/main --test-suite lisp` -> `pass=61 fail=0`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system ./build/main --test-suite lisp` -> `pass=42 fail=0`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_VERBOSE=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp` now passes the new parser regression coverage but still reports one pre-existing unrelated failure:
        - `crud pipeline duplicate-post race (error: unexpected token in expression)`
        - tracked separately as `HTTP-CRUD-DUPLICATE-POST-001`

- [x] `REF-SPLIT-INFRA-002` continue runtime large-file decomposition (largest-first, ownership-preserving)
  - closure evidence:
    - completed largest-first structural splits in the remaining targeted runtime files:
      - `src/lisp/eval_run_pipeline.c3` (`274 -> 164`) with `src/lisp/eval_run_pipeline_helpers.c3` (`117`)
      - `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates.c3` (`275 -> 64`) with `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates_impl.c3` (`236`)
    - preserved runtime contracts by keeping coordinator entrypoints and moving internal helpers only.
    - validation:
      - `c3c build` -> pass
      - `OMNI_LISP_TEST_SLICE=schema OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=42 fail=0`

- [x] `REF-SPLIT-TESTS-002` split oversized test files by existing feature/group seams
  - closure evidence:
    - completed largest-first splits for all oversized files listed in this lane:
      - `src/lisp/tests_deduce_rule_groups_more_tail.c3` (`1902 -> 310`) + extracted seam files
      - `src/lisp/tests_deduce_query_bench_groups.c3` (`1684 -> 117`) + extracted seam files
      - `src/lisp/tests_deduce_rule_groups_explain.c3` (`1471 -> 54`) + extracted seam files
      - `src/lisp/tests_deduce_durability_groups.c3` (`1403 -> 493`) + extracted seam files
    - preserved test harness behavior by keeping coordinator runners and existing test names/filters.
    - validation:
      - `c3c build` -> pass
      - `OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=324 fail=0`

- [x] `ERR-MODEL-STRICT-001` reduce `!!` usage in non-test runtime/compiler paths per `docs/C3_STYLE.md`
  - closure evidence:
    - removed the remaining non-test `!!` sites from runtime/compiler paths, including:
      - `src/lisp/parser_callable_helpers_params.c3`
      - `src/lisp/jit_jit_compile_let_set_helpers.c3`
      - `src/lisp/aot_runtime_bridge_helpers.c3`
      - `src/lisp/eval_dispatch_types.c3`
      - `src/lisp/prim_ui_ftxui_helpers.c3`
      - `src/lisp/primitives_meta_types_ctor.c3`
      - `src/lisp/compiler_temp_type_forms_defs_misc.c3`
      - `src/lisp/async_tcp_transport_helpers.c3`
      - `src/lisp/eval_type_evaluators.c3`
    - repo re-audit now shows zero non-test hits:
      - `rg -n "\)!!|!!;|!!," src/lisp --glob '!**/tests*'` -> no matches
    - validation:
      - `c3c build` -> pass
      - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=189 fail=0`
      - `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=60 fail=0`


- [x] `AUDIT-PARSER-SAFETY-001` fix parser progress/error defects and parser-surface drift
  - closure evidence:
    - lexer/parser fail-closed fixes landed in:
      - `src/lisp/parser_lexer_token_scanners_dot.c3`
      - `src/lisp/parser_lexer_whitespace.c3`
      - `src/lisp/parser_lexer.c3`
      - `src/lisp/parser_lexer_core_api.c3`
      - `src/lisp/parser_top_level_parse.c3`
      - `src/lisp/eval_run_pipeline.c3`
    - Pika/core surface parity fixes landed in:
      - `src/pika/lisp_grammar_build.c3`
      - `src/pika/lisp_grammar_scanners.c3`
    - targeted regressions added in:
      - `src/lisp/tests_compiler_core_groups_fail_closed.c3`
      - `src/lisp/tests_runtime_feature_schema_reader_groups.c3`
      - `src/lisp/tests_runtime_feature_pika_groups.c3`
    - validation:
      - `c3c build` -> pass
      - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=183 fail=0`
      - `OMNI_LISP_TEST_SLICE=reader-dispatch OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=12 fail=0`
      - `OMNI_LISP_TEST_SLICE=schema OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=42 fail=0`
      - `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=78 fail=0`

- [x] `VCS-JJ-CHECKOUT-ACL-001` restore `jj` checkout-state readability and bookmark workflow
  - closure evidence:
    - recovered broken checkout metadata non-destructively by rebuilding `.jj/working_copy` from readable repository state and preserving backups:
      - `.jj/working_copy.recover.bak.20260409-022334`
      - `.jj/repo/op_heads/heads.recover.bak.20260409-022441`
      - `.jj/repo/store/extra/heads.recover.bak.20260409-022625`
      - `.jj/repo/index.recover.bak.20260409-022536`
    - restored command health:
      - `jj status` -> exit `0`
      - `jj bookmark list` -> exit `0`
    - corrected bookmark workflow for current `jj 0.39.0` surface:
      - create/update by name: `jj bookmark set <name> -r <revset>`
      - move existing bookmarks: `jj bookmark move <name> --to <revset>`
      - advance closest bookmarks: `jj bookmark advance --to <revset>`
    - note:
      - `jj bookmark update` is not a valid subcommand in this CLI version.

- [x] `AUDIT-DEDUCE-SLICE-RED-001` investigate and reduce current deduce-lane failures
  - closure evidence:
    - deduce lane is now green end-to-end:
      - `OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=378 fail=0`
    - repaired high-signal root-cause clusters in materialization metadata, selector disjunctive-demand fallback, and why-result payload shape alignment:
      - `src/lisp/deduce_schema_query_metadata_schema_helpers.c3`
      - `src/lisp/deduce_schema_query_execution_goal_directed_selector_disjunction_projected.c3`
      - `src/lisp/deduce_why_result_payload.c3`
      - `src/lisp/deduce_why_result_path_payload.c3`
    - targeted admin-surface lane is also green after fixes:
      - `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=query OMNI_DEDUCE_QUERY_FILTER=admin-surface OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=184 fail=0`

- [x] `AUDIT-ADVANCED-SLICE-RED-001` restore advanced-lane green without relaxing strict contracts
  - closure evidence:
    - fixed parser macro-pattern zero-length allocation bug that broke `syntax-match` empty-sequence clauses (`[]`) and silently prevented stdlib/test macro definitions:
      - `src/lisp/parser_patterns_values.c3`
      - avoided zero-byte AST arena allocations for empty pattern buffers in sequence/dict pattern parsing.
    - stdlib macro/predicate lane restored:
      - `stdlib/stdlib.lisp`
      - `branch` macro now loads during bootstrap again.
      - `boolean?` contract aligned to strict boolean values (`true`/`false`) while preserving existing truthiness semantics (`nil`/`false` falsy).
    - strict higher-order arity lane restored:
      - `src/lisp/primitives_meta_predicates.c3`
      - `src/lisp/eval_init_primitive_tables.c3`
      - `stdlib/stdlib.lisp`
      - introduced primitive `error?` and used it in stdlib `map` to propagate callback runtime errors instead of returning lists of embedded `ERROR` values.
    - validation:
      - `OMNI_LISP_TEST_SLICE=advanced OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=1156 fail=0`
      - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=182 fail=0`

- [x] `META-STATS-001` remove duplicate SCC/parallel topology planner work from `deduce/stats`
  - closure evidence:
    - removed dead duplicate topology-planning wrapper that rebuilt SCC + batch metadata independently of the stats path:
      - deleted `deduce_parallel_batch_topology_counts(...)` from `src/lisp/deduce_schema_query_metadata_parallel_topology.c3`
    - the live stats path remains on a single metadata-build lane:
      - `src/lisp/deduce_schema_query_metadata_stats_parallel_fields.c3`
      - `src/lisp/deduce_schema_query_metadata_parallel_topology.c3`
    - no residual callsites remain for the deleted duplicate planner wrapper:
      - `rg -n "deduce_parallel_batch_topology_counts\\(" src/lisp` -> no matches
    - integration safety:
      - `c3c build` passes and links `build/main`
  - validation note:
    - the broader `OMNI_LISP_TEST_SLICE=deduce` lane is currently green in this workspace:
      - `OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=324 fail=0`

- [x] `REF-SPLIT-INFRA-REPL-001` split REPL server runtime surfaces by ownership lane
  - closure evidence:
    - split `src/lisp/eval_repl_server.c3` top-down into focused runtime surfaces without behavioral contract changes:
      - request auth/dispatch lane extracted to `src/lisp/eval_repl_server_request.c3`
      - unix/tcp listener lane extracted to `src/lisp/eval_repl_server_listeners.c3`
      - stream/session orchestration retained in `src/lisp/eval_repl_server.c3`
    - primary runtime file was reduced from `332` lines to `67` lines:
      - `wc -l src/lisp/eval_repl_server.c3 src/lisp/eval_repl_server_request.c3 src/lisp/eval_repl_server_listeners.c3`
    - integration + async REPL validation remain green after the split:
      - `c3c build`
      - `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=59 fail=0`

- [x] `AUDIT-BINDGEN-DEFERRED-001` resolve generated bindgen TODO ownership/teardown policy gaps
  - closure evidence:
    - bindgen wrappers now enforce concrete ownership/role/teardown guards and fail closed where policy is unresolved:
      - `src/lisp/bindgen.c3`
      - added explicit raise paths:
        - `bindgen/opaque-arg-role-mismatch`
        - `bindgen/opaque-arg-ownership-mismatch`
        - `bindgen/opaque-arg-unsupported-teardown`
        - `bindgen/opaque-arg-nil`
        - `bindgen/string-return-unknown-ownership`
        - `bindgen/opaque-return-manual-review`
    - staged comment marker switched from `TODO(bindgen)` to `REVIEW(bindgen)`:
      - `src/lisp/bindgen.c3`
      - `src/lisp/tests_compiler_codegen_groups_tail.c3`
    - compiler bindgen coverage remains green:
      - `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=189 fail=0`

- [x] `AUDIT-DEDUCE-WHY-DERIVED-001` implement missing derived-subject why-result support
  - closure evidence:
    - removed explicit unsupported derived-subject error path from why-result lookup:
      - `src/lisp/deduce_why_result_lookup_derived.c3`
      - `rg -n "why-result-derived-subject-not-yet-supported|not-yet-supported" src/lisp/deduce_why_result_* src/lisp/deduce_*` returns no matches
    - derived why-result now returns structured provenance payloads instead of a legacy unsupported error for supported read shapes:
      - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(block ... (deduce/query reach (lambda (row) (= (ref row 'src) 8))) (deduce/why-result reach 8 10))"`
      - observed payload includes:
        - `kind why-result`
        - `path-kind derived`
        - `status partial`
        - `paths` with structured support frames

- [x] `AUDIT-BUILD-IMPORT-001` restore green build by fixing invalid allocator import paths
  - closure evidence:
    - updated allocator import/module references in:
      - `src/lisp/scheduler_thread_tasks.c3`
      - `src/lisp/eval_repl_server_worker.c3`
    - `c3c build` now completes and links `build/main`.

- [x] `AUDIT-REPL-SECURITY-001` lock down unauthenticated remote REPL execution surfaces
  - closure evidence:
    - TCP REPL now enforces loopback bind (`localhost`/`127.0.0.1`/`::1`) and requires `OMNI_REPL_TCP_AUTH_TOKEN` at startup:
      - `src/lisp/eval_repl_server.c3`
    - per-request authorization gate added for non-`describe` operations with `auth` token matching:
      - `src/lisp/eval_repl_server.c3`
      - `src/lisp/eval_repl_server_protocol.c3`
      - `src/lisp/eval_repl_server_protocol_parse.c3`
      - `src/lisp/eval_repl_server_state.c3`
      - `src/lisp/eval_repl_server_output.c3`
    - regression coverage added:
      - `src/lisp/tests_runtime_async_repl_server_groups.c3`
    - `OMNI_LISP_TEST_SLICE=async OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=59 fail=0`.

- [x] `AUDIT-DEDUCE-PARALLEL-RUNTIME-001` replace metadata-only parallel mode with truthful runtime state
  - closure evidence:
    - added shared runtime-truth field helper with explicit serial runtime counters:
      - `src/lisp/deduce_parallel_runtime_truth.c3`
    - updated analyze/explain/stats payloads to emit truthful runtime mode/counters:
      - `src/lisp/deduce_rule_eval_analyze_payload_fields.c3`
      - `src/lisp/deduce_rule_ops_explain_snapshot.c3`
      - `src/lisp/deduce_schema_query_metadata_stats_parallel_fields.c3`
    - updated assertions:
      - `src/lisp/tests_deduce_query_admin_surface_tail.c3`
      - `src/lisp/tests_deduce_rule_groups_explain.c3`
    - targeted eval checks for analyze/stats runtime mode now return `true`.

- [x] `AUDIT-DEDUCE-NAIVE-FALLBACK-001` reduce recursive component fallback to naive execution
  - closure evidence:
    - seminaive recursive aggregate path is now selected directly when seminaive recursive mode is enabled:
      - `src/lisp/deduce_rule_eval_fixpoint_component_eval_non_txn.c3`
      - `src/lisp/deduce_rule_eval_fixpoint_component_eval.c3`
    - analyze/explain runtime-truth payloads remain aligned with serial execution counters.

- [x] `AUDIT-NET-IPv6-001` remove IPv4-only DNS resolution limitation
  - closure evidence:
    - DNS address rendering now supports both `AF_INET` and `AF_INET6` and reports unsupported families explicitly:
      - `src/lisp/async_tcp_transport_helpers.c3`
      - `src/lisp/async_runtime_base.c3`
      - `src/lisp/async_process_signal_dns.c3`
    - deterministic coverage added for IPv4 + IPv6 addrinfo rendering:
      - `src/lisp/tests_runtime_async_io_tls_groups.c3`
    - `OMNI_LISP_TEST_SLICE=async ... --test-suite lisp` remains green (`pass=59 fail=0`).

- [x] `STACK-AARCH64-CONT-001` arm64 language-level continuation multi-shot parity
  - closure evidence:
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack` -> `Stack engine: 23 passed, 0 failed`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))"` -> `11`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (with-continuation k (k 41))))"` -> `42`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(checkpoint (+ 1 (capture k (k 10))))"` -> `11`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(block (define replay-set-counter 0) (define replay-set-r (checkpoint (+ (capture k (+ (k 1) (k 1))) (block (set! replay-set-counter (+ replay-set-counter 1)) replay-set-counter)))) (+ (* 10 replay-set-r) replay-set-counter))"` -> `52`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-continuation OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=56 fail=0`
    - `OMNI_JIT_POLICY_FILTER=multishot-capture-scope-guard-clone OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` -> `pass=1 fail=0`
  - implementation note:
    - arm64 GNU lightning register IDs were corrected in `src/lisp/jit_lightning_constants.c3`.
    - effect fast-path primitive dispatch now preserves primitive error payloads and supports dotted cons payloads for fixed-arity wrappers in:
      - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
      - `src/lisp/jit_jit_runtime_effects_signal.c3`
- [x] `AUDIT-AOT-PRINT-DEDUCE-SENTINELS-059` close AOT/type-spec, print, and deduce persistence soundness defects
  - closure evidence:
    - AOT generated type/type-spec surfaces now reject `INVALID_SYMBOL_ID`
      before constructing type definitions, aliases, effects, method
      signatures, match helpers, dictionary symbol lookup, and effect explain
      payloads:
      - `src/lisp/aot.c3`
      - `src/lisp/aot_runtime_bridge.c3`
      - `src/lisp/aot_runtime_bridge_helpers.c3`
      - `src/lisp/aot_runtime_match_helpers.c3`
      - `src/lisp/aot_type_definitions.c3`
      - `src/lisp/aot_type_spec_helpers.c3`
    - AOT compiled list helpers now reject negative `long` indexes before
      converting to `usz`.
    - direct and buffered value printers now tolerate nullable dictionary/set
      backing storage, and `print_value_to_buf` now rejects null/zero-capacity
      buffers before writing.
    - constructor type constraint diagnostics now use guarded type-registry
      lookups, and instance type inference rejects invalid type IDs.
    - deduce tuple persistence now stores full 32-bit `SymbolId` values and
      rejects invalid/out-of-range decoded symbols.
    - deduce materialized metadata delete now distinguishes missing metadata DBI
      from real DBI-open errors.
    - deduce DBI name/path copy helpers now use checked addition before
      allocation.
    - deduce relation/rule install failure paths now roll back newly appended
      in-memory schemas/rule signatures when later fallible persistence or
      handle-publication steps fail.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `compiler`: green (`pass=191 fail=0`)
    - bounded `deduce`: green (`pass=330 fail=0`)
    - bounded `advanced`: green (`pass=1183 fail=0`)
    - bounded `memory-lifetime-smoke`: green (`pass=189 fail=0`)

- [x] `AUDIT-PARSER-JIT-ASYNC-BOUNDARY-060` close parser/compiler, JIT boundary, macro splice, and async/TLS soundness defects
  - closure evidence:
    - parser language-surface interning now fails closed through a parser-local
      checked helper before publishing symbols into import paths, path
      segments, type annotations, collection constructors, explain selectors,
      relation definition rewrites, quasiquote/template underscores, and
      special-form comparisons.
    - compiler lambda effect-wrapper scanning now uses checked AST arena
      allocation and invalid-symbol guards before publishing synthetic wrapper
      bodies or handler bodies.
    - primitive variable hash bootstrap now rejects `INVALID_SYMBOL_ID` keys
      and reports initialization failure instead of treating the sentinel as an
      empty slot.
    - compiler integer emission now avoids `long.min` negation and avoids
      `usz -> long` narrowing for unsigned decimal output.
    - macro splice append now rejects improper splice tails and recursion-limit
      exhaustion instead of silently truncating to the rest/nil.
    - runtime boundary string/error copying now guards `len + 1` allocation
      arithmetic, and boundary policy integer parsing now rejects overflow.
    - JIT resolve/continuation yield-failure paths restore saved interpreter
      state before returning; pending raise handler staging now clears
      `raise_pending` only after payload/env/list construction succeeds.
    - runtime handle entry points now reject null tags/closures arrays when
      `count > 0`.
    - TLS offload yield-error paths now close pending offload state before
      returning, TCP/UDP ports and signal numbers are range-checked before
      narrowing to `int`, and file-read close failure now fails the read.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded `compiler`: green (`pass=191 fail=0`)
    - bounded `async`: green (`pass=61 fail=0`)
    - bounded `memory-lifetime-smoke`: green (`pass=189 fail=0`)
    - bounded `advanced` macro hygiene group: green (`pass=82 fail=0`)

- [x] `AUDIT-SCHEMA-LIFETIME-WIDTH-061` close schema/deduce payload and lifetime-width soundness defects
  - closure evidence:
    - schema explain payload setters now reject failed key/value symbol
      interning before publishing dictionary keys or symbol payload values.
    - JSON pointer symbol-key fallback now treats failed token interning as a
      miss instead of constructing an invalid symbol key.
    - filesystem/process payload helpers now check result-key interning and
      file-size/handle-key bounds before constructing keys or integer payloads.
    - user-facing `exit`, `TimePoint`, Unicode codepoint predicates, `fs-open`,
      `tcp-listen`, and zlib original-size paths now validate integer ranges
      before narrowing to C `int`/`usz`/external API widths.
    - deduce materialize now rejects failed `"manual"` policy interning before
      persisting relation/schema state.
    - deduce integrity payload construction now propagates actual allocation/
      intern/set errors instead of collapsing them to `null`, and list payload
      builders now stop on cons allocation errors.
    - materialized-stale and integrity-violation payload symbols now reject
      failed interning before `make_symbol`.
    - primitive name matching now rejects null primitive backing pointers and
      overlong expected names before reading the fixed primitive name buffer.
    - checked array construction now allocates backing payloads before
      publishing the root wrapper, mirroring the hashmap constructor pattern.
    - closure escape promotion now releases any retained/detached env scope if
      final wrapper allocation fails.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - bounded normal+ASAN `data-format`: green (`pass=64 fail=0`)
    - bounded normal+ASAN `unicode`: green (`pass=27 fail=0`)
    - bounded normal+ASAN `compression`: green (`pass=27 fail=0`)
    - bounded normal+ASAN `async`: green (`pass=65 fail=0`)
    - bounded normal+ASAN `compiler`: green (`pass=194 fail=0`)
    - bounded normal+ASAN `memory-lifetime-smoke`: green (`pass=190 fail=0`)
    - bounded normal `advanced`: green (`pass=1185 fail=0`)
    - bounded ASAN `advanced`: green (`pass=1172 fail=0`)
    - bounded normal+ASAN `deduce`: green (`pass=330 fail=0`)

- [x] `AUDIT-FTXUI-SMOKE-SEGFAULT-062` investigate FTXUI smoke crash outside the Lisp slice set
  - observed during bounded validation after all targeted Lisp slices had
    passed:
    - `scripts/run_ftxui_smoke.sh` `smoke.omni` exited with SIGSEGV.
  - closed:
    - root cause was a runtime lifetime-boundary provenance/reuse walk over a
      nested effect payload graph, not the FTXUI lowering path.
    - `src/lisp/eval_boundary_provenance.c3` now uses a bounded iterative
      alias-safety worklist with visited tracking for nested arrays, dicts,
      sets, method tables, partials, iterators, and cons payloads, avoiding the
      recursive stack overflow that surfaced through FTXUI `ui.graph` payloads.
    - `src/lisp/eval_boundary_provenance.c3` now skips scalar leaves before
      consuming alias worklist/visited capacity, so wide scalar-only payloads do
      not fail closed as if they were unsafe graph overflows.
    - `src/lisp/prim_ui_ftxui.c3` now checks child component count arithmetic
      before allocating the FTXUI child pointer array.
    - `src/lisp/prim_ui_ftxui_helpers.c3` now guards helper-array capacity
      growth and graph-series allocation size math before `realloc`/`malloc`.
    - `src/lisp/prim_ui_ftxui_lowering.c3` now rejects menu item counts that
      cannot be represented by the FTXUI `int` selected-index state.
    - `csrc/ftxui_shim.cpp` now declares `keep_alive` before `component`, so
      component teardown runs before retained borrowed backing data is released.
    - `csrc/ftxui_shim.cpp` now rejects nonzero child counts with null child
      arrays, checks table `rows * cols` overflow before comparing against the
      child count, and rejects table row/column selectors that cannot fit in
      FTXUI `int` APIs.
    - `src/lisp/tests_memory_lifetime_boundary_state_groups.c3` adds a minimal
      nested effect-payload regression matching the failing view shape plus a
      wide scalar payload regression, and
      `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` keeps both in the
      bounded smoke lane.
  - validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      green (`pass=192 fail=0`)
