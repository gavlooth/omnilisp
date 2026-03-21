# Active TODO

Last condensed: 2026-03-19

This file now tracks only active, actionable work.
Full completed history is archived at:
- `docs/plans/TODO_ARCHIVE_2026-03-11.md`

Current actionable count: 0
Live blocker queue:
- none currently; reopen with a fresh ranked plan if a bounded gate or status
  signal turns red again

## Invasive Runtime Hardening (2026-03-16)

- [x] Rework JIT TCO env-copy/rewrite ownership gating to stop depending on stale scope-generation stamps.
  target:
  - `src/lisp/jit_jit_eval_scopes.c3`
  - `src/lisp/eval_promotion_copy.c3`
  acceptance:
  - moved/chunk-transferred values in releasing scope are treated as copy-required even when generation stamps are stale.
  - root-persistent parent rewrite cannot skip when parent physically resides in releasing scope.
  - add focused regressions for stale-generation moved-binding and stale-generation persistent-parent paths.
  - validate with `c3c build --sanitize=address` and `OMNI_LISP_TEST_SLICE=jit-policy`.
  completed:
  - JIT recycle/rewrite gates now use physical releasing-scope residency checks (`boundary_ptr_in_scope`) rather than stale `scope_gen` equality.
  - boundary copy fast-reuse now fail-closes when closure/iterator payload provenance still points into releasing scope.
  - regressions added:
    - `run_jit_policy_tco_stale_generation_moved_binding_copy_test(...)`
    - `run_jit_policy_tco_persistent_parent_stale_generation_rewrite_test(...)`
  - validation:
    - `c3c build --sanitize=address`
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp` (`22 passed, 0 failed`)

- [x] Make env-copy frame materialization failure transactional so partially copied bindings/wrappers cannot leak into target scope.
  target:
  - `src/lisp/eval_env_copy.c3`
  acceptance:
  - partial binding-copy failure releases any already materialized target-scope wrapper allocations/retains.
  - no partially copied env frame remains reachable after failure return.
  - add regressions for mid-frame copy failure with previously cloned closure/iterator/instance payloads.
  - validate with `c3c build --sanitize=address` and `OMNI_LISP_TEST_SLICE=memory-lifetime-smoke` (container path).
  completed:
  - env-copy frame materialization now performs transactional rollback of already materialized binding values when:
    - binding-value copy fails mid-frame, and
    - parent-frame copy fails after binding materialization.
  - rollback helpers added in `src/lisp/eval_env_copy.c3` to unwind target-scope side effects for copied closure/iterator/instance wrappers via existing scope destructor symmetry.
  - added regression `run_memory_lifetime_env_copy_transactional_binding_rollback_test(...)` in `src/lisp/tests_memory_lifetime_env_copy_groups.c3` and wired it into env-copy escape-mode suite dispatch.
  - validation:
    - `c3c build --sanitize=address`
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp` (blocked by pre-existing `boundary_commit_escape` failures + ASAN UAF in `tests_memory_lifetime_boundary_commit_escape_groups.c3:147`; reproduces even when env-copy rollback callsites are temporarily disabled)
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_VERBOSE=1 ./build/main --test-suite lisp` (`56 passed, 4 failed`; decoded pass log includes `[PASS] lifetime: env-copy transactional mid-frame rollback`; 4 failures are the same pre-existing `boundary_commit_escape` cases)

- [x] Unify closure/iterator alias safety policy across env-copy and boundary promotion paths.
  target:
  - `src/lisp/eval_env_copy.c3`
  - `src/lisp/eval_promotion_copy.c3`
  - `src/lisp/eval_promotion_escape.c3`
  acceptance:
  - closures/iterators lacking detached ownership envelope are fail-closed (or deep-copied) regardless of originating transient scope, not only `releasing_scope`.
  - iterator payloads cannot preserve unsafe aliasing through non-closure thunk wrappers.
  - add regressions for cross-scope undelimited closure and iterator-thunk alias rejection.
  - validate with `c3c build --sanitize=address`, `OMNI_LISP_TEST_SLICE=jit-policy`, and memory-lifetime targeted env-copy tests.
  completed:
  - added shared alias-safety provenance helpers in `src/lisp/eval_boundary_provenance.c3`:
    - `boundary_closure_alias_unsafe_for_reuse(...)`
    - `boundary_iterator_payload_alias_unsafe_for_reuse(...)`
  - `src/lisp/eval_env_copy.c3` now fail-closes undelimited closure aliases based on target-chain residency (not only `releasing_scope`) and rejects iterator wrappers with disjoint non-closure thunk payload aliases.
  - `src/lisp/eval_promotion_copy.c3` fast-reuse gate now uses the same shared closure/iterator alias-unsafe policy before deciding wrapper reuse.
  - `src/lisp/eval_promotion_escape.c3` fast-path reuse now also routes alias-unsafe closure/iterator wrappers through defensive disjoint promotion instead of reusing wrapper identity.
  - regression coverage:
    - added `run_memory_lifetime_env_copy_rejects_cross_scope_undelimited_alias_test(...)` in `src/lisp/tests_memory_lifetime_env_copy_groups.c3`.
    - added `run_jit_policy_cross_scope_alias_policy_test(...)` in `src/lisp/tests_runtime_feature_jit_groups.c3`.
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`23 passed, 0 failed`)
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp` (blocked by pre-existing `boundary_commit_escape` failures + ASAN UAF in `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3:147`)
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_VERBOSE=1 ./build/main --test-suite lisp` (`57 passed, 4 failed`; decoded pass lines include:
      - `[PASS] lifetime: env-copy rejects undelimited closure alias`
      - `[PASS] lifetime: env-copy rejects cross-scope undelimited aliases`
      - `[PASS] lifetime: env-copy transactional mid-frame rollback`)

## Runtime Hardening Follow-Ups (2026-03-16)

- Scheduler follow-up note (2026-03-19):
  - the round-limit assertion closure landed in `src/lisp/tests_scheduler_groups.c3`.
  - current Docker-bounded scheduler validation is green:
    - `OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp` (`89 passed, 0 failed`).
  - the historical validation notes below predate that closure.

- [x] Preserve explicit cancellation error payload when converting late worker completions.
  target:
  - `src/lisp/scheduler_thread_task_transitions.c3`
  acceptance:
  - cancellation-converted completions always carry deterministic cancellation text (`task` and `thread` paths).
  - regressions assert join-visible payload is cancellation-specific, not generic worker-failure.
  completed:
  - cancellation conversion helper now rewrites late completion payload text to explicit cancellation strings:
    - task path: `"thread task cancelled"`
    - os-thread path: `"thread cancelled"`
  - late completion conversion keeps shared payload cleanup while ensuring converted error payloads do not fall back to generic worker-failure text.
  - added regression `run_scheduler_cancel_conversion_payload_boundary_tests(...)` in `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`, wired into `run_scheduler_tests(...)`, covering:
    - thread-task cancel conversion with join-visible error message assertion,
    - os-thread cancel conversion with join-visible error message assertion,
    - explicit negative assertion against `"worker failed"` fallback text.
  - validation:
    - `c3c build`
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp` (`86 passed, 2 failed`; failures are pre-existing `await surfaces round-limit failure` and `run-fibers surfaces round-limit failure`)

- [x] Stop masking non-timeout condvar wait failures as normal timeouts in join waiters.
  target:
  - `src/lisp/scheduler_thread_task_waiters.c3`
  - `src/lisp/scheduler_primitives_task_wait_join.c3`
  - `src/lisp/scheduler_primitives_thread_helpers.c3`
  acceptance:
  - real `wait_timeout` error paths map to deterministic wait-failed scheduler errors, not timeout.
  - explicit timeout behavior is unchanged for real timeout events.
  - add regression coverage for wait-failed mapping in non-fiber join paths.
  completed:
  - added deterministic join-wait timeout-failure injection helpers in `src/lisp/scheduler_thread_task_waiters.c3`:
    - `scheduler_test_inject_wait_timeout_failure(...)`
    - `scheduler_test_clear_wait_timeout_failure_injection(...)`
    - internal one-shot consumption in wait-timeout callsites.
  - wired injected non-timeout wait-timeout failure through join waiter state functions so callers receive `SCHEDULER_JOIN_WAIT_FAILED` rather than timeout status.
  - added regression `run_scheduler_join_wait_failure_mapping_boundary_tests(...)` in `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`, covering non-fiber:
    - `task-join` wait-failed mapping (`task-join: wait failed`, not timeout),
    - `thread-join` wait-failed mapping (`thread-join: wait failed`, not timeout).
  - wired regression into scheduler suite dispatch in `src/lisp/tests_scheduler_groups.c3`.
  - validation:
    - `c3c build`
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp` (`87 passed, 2 failed`; failures remain the pre-existing round-limit assertions)

- [x] Fail closed on closure signature-copy failure during promotion/copy instead of silently clearing `type_sig`.
  target:
  - `src/lisp/eval_promotion_copy.c3`
  - `src/lisp/eval_promotion_escape.c3`
  acceptance:
  - signature-copy failure aborts boundary move with typed failure result.
  - no successful boundary copy/promotion path silently drops callable signature metadata.
  - add regression coverage for signature-copy allocation failure behavior.

- [x] Prevent aborted escape-builder walks from leaving partially built destination-lane objects.
  target:
  - `src/lisp/eval_boundary_commit_escape_builders.c3`
  - `src/lisp/eval_promotion_escape.c3`
  acceptance:
  - `ctx.aborted` is checked before destination allocations in commit/build paths, or allocated objects are rolled back deterministically.
  - no unreachable partial destination objects remain after aborted walk.
  - add targeted regressions for abort-mid-walk object allocation paths.

- [x] Harden JIT scope-guard defer pop fallback so mismatched active context cannot strand stale defer state.
  target:
  - `src/lisp/jit_common.c3`
  - `src/lisp/jit_jit_eval_scopes.c3`
  acceptance:
  - fallback context is attempted when current stack context pop is inapplicable/fails.
  - call-scope wrappers treat unresolved defer-pop as fail-closed error path.
  - add regression for mismatched stack-context cleanup ordering.

## Lookup Accessor Rollout (Phased)

- [x] Phase 1: support leading `.[expr]` as a first-class accessor form that desugars to `(lambda (x) (ref x expr))`.
- [x] Phase 2: add `.name` shorthand as sugar for `.['name]`.
- [x] Phase 3: add quoted leading-dot shorthand `."name"` as sugar for `.["name"]`.
- [x] Phase 4: add numeric shorthand `.1` / `.-1` as sugar for `.[1]` / `.[-1]`.

## Semantic Clarification Follow-Up (2026-03-14)

- [x] Decide and lock the predicate contract for `Void` in control forms (`if`, `when`, `match`) as normative behavior.
- [x] Add an explicit `LANGUAGE_SPEC` table for predicate truthiness inputs (`nil`, `false`, `Void`, numbers, strings, collections).
- [x] Add advanced regression coverage for the chosen `Void`-in-predicate behavior in interpreter mode.
- [x] Add compiler/e2e parity regression coverage for the same `Void`-in-predicate behavior.
  completed:
  - `LANGUAGE_SPEC` now includes a normative predicate contract and explicit truthiness table for `nil`/`false`/`Void`/numbers/strings/collections.
  - control-form docs now explicitly lock that `if`, `when`, and `match` guards share the same truthiness semantics and treat `Void` as truthy.
  - interpreter regression coverage added in `run_advanced_void_nil_contract_tests(...)` for `when` and `match` predicate behavior on command-produced `Void`.
  - compiler/e2e parity coverage added in `E2E_TESTS_EXTENDED` for `if`/`when`/`match` predicate behavior on command-produced `Void`.
- [x] Promote `match` to explicit core control-form status in language docs (not just pattern syntax), with clause-selection semantics locked as normative.
- [x] Define `match` behavior for `Void`/`nil`/`false` literal patterns, wildcard fallthrough, and guard evaluation order.
- [x] Add interpreter+compiler parity regressions for `match` semantics on `Void`/`nil`/`false` and guarded clauses.
  completed:
  - `LANGUAGE_SPEC` now treats `match` as an explicit core control form with normative clause-selection order (`scrutinee once`, `left-to-right`, `first-match`, wildcard fallthrough).
  - parser pattern semantics now reserve `nil`/`false`/`Void` as literal patterns (instead of variable binds) in `match` clauses.
  - guard-order contract is now explicit in docs and regressions:
    - sub-pattern match happens before guard evaluation,
    - failed sub-patterns skip guard evaluation,
    - later clause guards are not evaluated after an earlier clause matches.
  - interpreter regressions added in `run_advanced_core_semantics_match_tests(...)` for `nil`/`false`/`Void` literals, wildcard fallthrough, and guard evaluation ordering.
  - compiler/e2e parity coverage added in `E2E_TESTS_CORE` + `E2E_TESTS_EXTENDED` for the same `match` semantics.
- [x] Document `resume` as “invoke captured continuation with a value” in language + reference docs with one canonical example.
- [x] Add a runtime regression asserting `resume` behavior for repeated invocation of a multi-shot captured continuation.
- [x] Add a runtime regression asserting deterministic error payload for invalid `resume` target values.
  completed:
  - `LANGUAGE_SPEC` and `docs/reference/06-effects.md` now explicitly define continuation `resume` as invoking captured continuation values with `(k value)` and include a canonical example.
  - docs now explicitly distinguish continuation resume (`(k value)`) from coroutine primitive resume (`(resume coroutine)`).
  - runtime regressions added in `run_advanced_effect_continuation_tests(...)`:
    - repeated invocation of a captured continuation via a bound resume alias (`resume-k`) in a multi-shot path,
    - deterministic payload code checks for invalid resume targets bound to `__k` (`42`, `nil`, `(Void)`), each asserting `runtime/invalid-continuation`.
- [x] Perform a primitive-surface audit to classify every primitive as command-style (`Void`) or query-style (`nil`-on-absence).
- [x] Add a docs appendix listing command/query classification for core mutation, I/O, and scheduler primitives.
- [x] Add coverage that command-style collection mutators never return `nil` on success (only `Void`).
- [x] Add coverage that query-style lookup predicates never return `Void` (absence remains `nil`).
- [x] Lock constructor alias policy text: canonical names appear in docs/errors; aliases are accepted input but non-canonical output.
- [x] Add regression coverage that constructor aliases (`Int`, `Bool`, `Dict`) normalize to canonical type identity in introspection.
- [x] Decide canonical type descriptor rendering shape (`#<type Dictionary>` vs `#<Dictionary>`) and record as a normative print contract.
- [x] Align printer implementation to the chosen type-descriptor shape across REPL output and `%s` fallback.
- [x] Add regression coverage for canonical type-descriptor printing of constructors and abstract/meta type symbols.
- [x] Lock deterministic multiple-dispatch ambiguity tie-break/error reporting fields and ordering in the spec.
- [x] Add advanced regression coverage for tie cases (same specificity distance) and assert stable ambiguity payload.
- [x] Add parity coverage that ambiguity reporting is identical across interpreter, JIT, and compiled execution.
- [x] Extend checkpoint/capture replay docs and tests to explicitly cover side-effect replay parity expectations for compiled paths.

  completed:
  - dispatch ambiguity now fails with deterministic payload code `type/dispatch-ambiguous` (domain `type`) and stable data keys:
    - `reason`, `method`, `arg-count`, `arg-types`, `best-score`, `tie-count`, `candidate-indices`.
    - `candidate-indices` are emitted in method-table index order (ascending), with no implicit tie-break winner.
  - `LANGUAGE_SPEC` now locks ambiguity/error-reporting invariants:
    - no implicit tie-break on equal best-score matches,
    - canonical ambiguity payload code and data fields,
    - explicit explain-dispatch ambiguity decision invariants (`status`, `reason`, `winner-index=nil`, `best-score`, `tie-count`).
  - advanced dispatch regressions now include equal-specificity tie payload checks for both:
    - stable ambiguity payload field values, and
    - stable candidate-index ordering when non-winning lower-specificity methods are present.
  - compiled parity coverage added in e2e generation cases:
    - ambiguity payload parity case captures deterministic payload fields through handled `raise`,
    - checkpoint/capture replay parity cases cover side-effect replay for:
      - `set!` mutation in resumed segments, and
      - handled effect paths in resumed segments.
  - replay docs now explicitly state execution-mode parity expectations across interpreter, JIT, and compiled execution (`LANGUAGE_SPEC` + `docs/reference/06-effects.md`).

  completed:
  - Primitive return-style audit is now recorded as a normative command/query contract in `LANGUAGE_SPEC` (`7.24`), with source-of-truth tied to `src/lisp/eval_init_primitives.c3`.
  - Primitive appendix now includes an explicit command/query audit block (`docs/reference/11-appendix-primitives.md`) with focused classification tables for core mutation, I/O, and scheduler primitives.
  - Exhaustive audit snapshot is documented (`263` registered names; `42` command-style, `221` query-style), with an explicit command-style name set and complement rule for query-style.
  - extended `run_advanced_void_nil_contract_tests(...)` coverage in `src/lisp/tests_advanced_core_unicode_groups.c3`:
    - command-style collection mutators now explicitly assert `Void` success for `array-set!`, `dict-set!`, `set-add`, and `set-remove` (in addition to existing `set!`/`push!`/`remove!` checks),
    - query-style lookup predicates now explicitly assert non-`Void` hit paths for `ref`, `has?`, and `set-contains?`, while miss-path assertions remain pinned to `Nil`.
  - constructor alias policy is now explicit in `LANGUAGE_SPEC` and `docs/reference/04-type-system.md`: aliases are accepted on input, but canonical names are required for docs/examples, introspection output, and constructor error text.
  - advanced constructor regression coverage now pins alias normalization in `run_advanced_callable_type_symbol_constructor_tests(...)`:
    - canonical introspection identity for `Int`/`Bool`/`Dict` via `type-of`,
    - canonical constructor names in alias-invoked error messages (`Integer`, `Boolean`, `Dictionary`).
  - canonical descriptor print shape is now locked as `#<type Name>` in `LANGUAGE_SPEC` and `docs/reference/04-type-system.md`; `#<Name>` is non-canonical.
  - printer parity is aligned across REPL output (`print_value`) and `%s` fallback (`print_value_to_buf`): constructor-backed method tables now render as type descriptors even when typed methods are present (for example `Iterator`), while ordinary callable dispatch tables keep `#<dispatch ...>` and primitive fallback-only tables keep `#<primitive ...>`.
  - advanced regressions now pin descriptor rendering for constructor symbols/aliases (`Integer`, `Int`, `Bool`, `Dict`, `Iterator`) and abstract/meta type descriptors (`Any`, `Number`, `Collection`) while preserving non-callable behavior for meta symbols in value-position calls.

## Semantic Clarifications (2026-03-14)

- [x] Lock continuation shotness as an explicit language contract.
  completed:
  - normative + language/reference syntax docs now explicitly state that `capture` continuations are multi-shot by contract.
- [x] Specify `capture` replay semantics for side effects (`set!`, I/O, deduce writes) under repeated continuation invocation.
  completed:
  - docs define replay behavior for resumed continuation segments and stack-snapshot restart semantics.
  - advanced runtime regressions lock replay for:
    - `set!` mutation (`multi-shot replay set! in resumed segment`) across interpreter+JIT.
    - Deduce write paths during replay (`multi-shot replay deduce fact writes`) in interpreter runtime semantics.
    - handled I/O replay behavior under checkpoint/capture + effect handlers (`multi-shot replay handled io effect`) in interpreter semantics.
- [x] Define `set!` target matrix (variable, dot-path field, `Array` index, `Dictionary` key, cons `car/cdr`) and invalid-target error behavior.
  completed:
  - language/syntax/reference docs now include a normative `set!` target/dispatch matrix with explicit invalid-target error strings,
  - advanced regression coverage now asserts invalid-target behavior for unbound roots, bad intermediate segments, missing fields, cons-segment restrictions, and generic non-collection targets.
- [x] Lock `set!` success return contract to `Void` across interpreter, JIT, and AOT paths.
  completed:
  - runtime regression coverage continues to assert `Void` success on variable/dot-path/generic collection `set!` surfaces,
  - compiler regression coverage now includes both generic `set!` collection shapes (`Array` + `Dictionary`) as accepted compile forms,
  - e2e parity coverage now asserts `type-of` on generic collection `set!` results is `Void` across interpreter and compiled execution.
- [x] Codify `Void` vs `Nil` semantics as a normative rule (`Void` for command/effect completion, `Nil` for absence/query miss).
  completed:
  - language/reference docs now include a dedicated normative `Void` vs `Nil` contract section with explicit examples,
  - advanced regression coverage now includes a focused contract block asserting `Void` on command completion (`set!`, `push!`, `remove!`) and `Nil` on absence/query misses (`ref`, `has?`, `set-contains?`).
- [x] Lock constructor/coercion behavior for callable type symbols (`Integer`, `Double`, `String`, `Boolean`, `List`, `Array`, `Dictionary`, `Set`, `Iterator`) including deterministic failure semantics.
  completed:
  - constructor failure signaling is now explicitly deterministic for callable type symbols: `type/arity` for constructor-shape mismatch and `type/arg-mismatch` for non-convertible values,
  - runtime paths now emit canonical payloaded constructor failures for iterator helpers (`Iterator`, `make-iterator`) and `Dictionary` odd key/value arity,
  - advanced regression coverage now asserts constructor failure payload codes for `Dictionary`/`Dict` odd arity and `Iterator`/`make-iterator` invalid argument types, in addition to existing scalar constructor checks.
- [x] Formalize numeric conversion and overflow behavior (truncate/widen/overflow policy) and enforce interpreter/JIT/AOT parity tests.
  completed:
  - numeric narrowing policy is now explicit and enforced: double→Integer conversions truncate toward zero, require finite input, and require Integer-range results,
  - `Integer`/`inexact->exact` now reject non-finite or out-of-range values with deterministic `type/arg-mismatch` payloads; `string->number` now returns `nil` for integer overflow/underflow parse cases,
  - math narrowing forms (`floor`, `ceiling`, `round`, `truncate`) now reject out-of-range/non-finite results deterministically instead of relying on unchecked casts,
  - advanced regression coverage now asserts overflow/underflow conversion behavior, and compiler slice remains clean after parity-case additions in e2e generation fixtures.
- [x] Define iteration-order guarantees for `Dictionary` and `Set` and enforce with deterministic regression tests.
  completed:
  - `keys` and `values` now use the same deterministic canonical key order (type/value comparator) instead of raw hash-slot traversal order,
  - `set->list` now returns deterministic canonical element order,
  - advanced + e2e regressions now assert canonical dictionary/set iteration order, including mutation paths.
- [x] Define effect-resume discipline (`resolve` single-use, double-resolve error, no-resolve abort semantics) with explicit runtime/docs tests.
  completed:
  - interpreter `resolve` now reuses the same single-shot continuation-resume path as AOT (`jit_resolve_value`), eliminating JIT/AOT discipline drift,
  - `resolve` is now explicitly handler-bound in runtime (`runtime/invalid-continuation` when `__k` is not handler-bound),
  - runtime guards keep deterministic double-use rejection (`runtime/continuation-resumed`) at continuation boundary level,
  - advanced regressions now assert abort semantics (`no resolve` skips post-signal body execution) and terminal `resolve` clause behavior,
  - language + reference docs now codify the resolve discipline and the `with-continuation` multi-shot distinction.
- [x] Standardize printing/introspection contract for values and type descriptors (`type-of` symbols vs `#<type ...>` rendering).
  completed:
  - constructor-backed callable symbols now render as type descriptors (`#<type Integer>`, `#<type Dictionary>`) while ordinary callable primitives keep primitive rendering (`#<primitive +>`),
  - method-table fallback rendering now distinguishes constructor aliases (`Int`/`Bool`/`Dict`) from non-constructor primitive callables,
  - `%s` formatting fallback now routes through canonical value rendering (`print_value_to_buf`) with buffer-printer coverage aligned for `METHOD_TABLE`/`TYPE_INFO` and related introspection tags.

## Language Surface Consistency (2026-03-12)

- [x] Decide the public fate of compatibility/runtime factory aliases:
  `make-iterator`, lowercase `iterator`, lowercase `list`/`array`/`dict`/`time-point`, and similar surfaces still exist for compatibility; decide which remain public, which become deprecated, and which should disappear from docs/examples.
  decided:
  - canonical constructor surfaces for new docs/examples: `List`, `Array`, `Dictionary`, `Iterator`, `TimePoint`
  - retained public helper: `list` (idiomatic Lisp builder/conversion helper)
  - compatibility-only aliases (non-preferred): `array`, `dict`, `dictionary`, `iterator`, `time-point`, `make-iterator`
  notes: language/reference docs and primitive appendix now reflect this split explicitly; runtime alias behavior is preserved for compatibility.
- [x] Decide whether abstract/meta builtin type symbols remain type-only:
  `Any`, `Value`, `Number`, and `Collection` currently participate in type annotations and dispatch but do not have callable value-position constructor/coercion behavior.
  decided: keep them type-position-only; they remain valid annotation/dispatch surfaces, but they are not callable runtime constructors. `Value` stays reserved for value-literal dispatch inside `^(Value literal)` rather than `(Value ...)`. Added advanced regression coverage for both annotation acceptance and value-position rejection, and updated language/type-system docs.
- [x] Remove implicit dispatch-time coercion/widening and require exact-or-subtype matching:
  numeric widening like `Integer` satisfying `^Double` should be removed so dispatch no longer performs hidden conversions; cross-type conversion should happen only through explicit constructor/coercion calls such as `(Double 3)` or `(Integer 3.9)`.
- [x] Audit constructor/coercion semantics across all builtin callable type symbols:
  ensure every cross-type conversion is explicit and localized to constructors (`Integer`, `Double`, `String`, `Symbol`, `Boolean`, `Nil`, `Closure`, `List`, `Array`, `Dictionary`, `Set`, `Iterator`, `Coroutine`, `TimePoint`) rather than leaking through dispatch or other implicit compatibility rules.
  completed:
  - constructor-type annotation matching resolves aliases canonically (`Int`/`Bool`/`Dict` behave as shorthands for `Integer`/`Boolean`/`Dictionary` in constructor annotation checks),
  - advanced constructor/type-parametric conformance assertions remain aligned to canonical `type-of`/`type-args` names (`Integer`, `Boolean`),
  - callable constructor audit matrix now covers `List`/`Array`/`Dictionary`/`Set`/`Iterator`/`Coroutine`/`TimePoint` in advanced tests,
  - regression coverage now asserts no implicit dispatch conversion for non-matching collection types (`^Array`, `^Iterator`) unless explicit constructor conversion is used.
- [x] Decide whether the current `false`/`nil` collapse is a stable language rule:
  the new `Boolean`/`Bool` and `Nil` constructor contracts currently reflect the existing runtime truthiness model; if `Boolean` and `Nil` should become distinct runtime values later, constructor semantics and docs will need another pass.
  decided: keep the current collapse stable for now. `false` remains the canonical falsy alias of `nil` at value level, `Boolean`/`Bool` constructors normalize falsy inputs to `nil`, and `Nil` constructor keeps accepting both `nil` and `false`. `Void` remains distinct and truthy.
- [x] Migrate command-style no-result language surfaces from `Nil` to `Void`:
  now that `Void` is a real builtin singleton type/value, command-style success paths should stop overloading `nil`; start with obvious no-payload operations such as printing, sleeping, close/release paths, and similar effectful runtime commands.
  completed:
  - command-style runtime surfaces now use `Void` for successful completion paths (`print`/`println`/`newline`/`display`, `sleep`/`async-sleep`, socket/process/signal/tls/fs close-style commands, scheduler cancel forms, `write-file`, `unsafe-free!`, `run-fibers`, module/import/export command forms, deduce command mutations/txn-control),
  - `fiber-cancel` now aligns with other cancel surfaces and returns `Void` for valid ids even when cancellation is a no-op (already done/running),
  - query/absence surfaces remain on `nil` (`file-exists?` miss, lookup/ref misses, predicate misses), preserving the `Nil` vs `Void` semantic line.
- [x] Update command-style collection mutation tests to assert `Void` result tags for `array-set!`, `push!`, `set-add`, `set-remove`, `dict-set!`, and `remove!`.
- [x] Preserve `Nil` specifically for absence/falsey/query semantics while `Void` covers successful no-payload completion:
  document and enforce the semantic line so lookups/not-found/query-style APIs stay on `nil`, while successful command-style completion uses `Void`.
  completed:
  - `LANGUAGE_SPEC` now explicitly codifies absence-vs-command semantics and false/nil truthiness collapse as stable behavior,
  - type-system and primitive appendix docs now call out `Nil` vs `Void` and `false` alias behavior directly,
  - advanced regression coverage enforces `Void` on command-style mutations and `nil` on query misses (`has?`, `set-contains?`, missing `ref` paths, etc.).
- [x] Normalize mutating update surfaces around `Void` results:
  mutating operations should return `Void` on success rather than `nil` or ad hoc payloads unless they have a strong data-return reason.
  completed:
  - canonical mutators now return `Void` on success (`set!`, `array-set!`, `push!`, `dict-set!`, `remove!`, `set-add`, `set-remove`, deduce command mutators, close/release/cancel command mutators),
  - JIT + AOT surfaces are aligned for `set!` variable/dot-path/collection update behavior,
  - intentional exceptions with explicit data-return semantics are retained (`atomic-add!` returns prior value, `atomic-cas!` returns boolean-like result).
- [x] Replace type-specific update forms with generic multiple-dispatch `set!` where the operation is conceptually “update collection/path slot”:
  array/dictionary and similar mutation paths should converge on a generic dispatched `set!` surface rather than permanently fragmenting into type-specific mutation names; the canonical success result should still be `Void`.
  completed:
  - parser/runtime support canonical generic collection updates via `(set! collection key value)` for `Array` and `Dictionary` while preserving two-arg variable/dot-path mutation behavior,
  - compiler/JIT/AOT surfaces and regressions cover generic collection `set!` flows (`Array`/`Dictionary`) and `Void` result semantics,
  - `array-set!` and `dict-set!` remain compatibility aliases rather than primary language-facing mutation surfaces.
- [x] Finish the descriptive-name audit for remaining shorthand-heavy surfaces:
  `Ptr`, low-level namespace abbreviations like `fs-*`/`tcp-*`/`udp-*`/`tls-*`/`dns-*`, and any other public spellings that still prioritize brevity over clarity should be reviewed explicitly instead of drifting ad hoc.
  decided:
  - descriptive names are the canonical language-facing direction in docs/examples (`filesystem-*`, `transmission-control-*`, `user-datagram-*`, `domain-name-resolve`, `transport-layer-security-*`, `^Pointer`),
  - shorthand spellings remain accepted for compatibility and ergonomics (`fs-*`, `tcp-*`, `udp-*`, `dns-resolve`, `tls-*`, `^Ptr`),
  - compatibility shorthands stay documented, but are explicitly marked non-preferred for new public examples/spec text,
  - effect-tag internals remain on stable `io/*` operation symbols (`io/tcp-*`, `io/udp-*`, etc.) to preserve handler/runtime parity and existing diagnostic domains.
- [x] Run a Docker-capped full constructor-surface conformance pass:
  validate the constructor/type-symbol behavior across the advanced/e2e gates inside the validation container, not just the host-safe build/basic-smoke path.
  completed:
  - Docker-capped advanced constructor/dispatch surface pass is clean (`1019 passed, 0 failed`) via:
    - `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
  - Docker-capped e2e compile+run parity is clean (`ALL 397 e2e compiler tests passed!`) via:
    - `scripts/run_validation_container.sh env OMNI_HARD_MEM_CAP_METHOD=none scripts/run_e2e.sh` (with `OMNI_VALIDATION_EXTRA_ARGS` header/lib mounts for `yyjson`, `bearssl`, `uv`, `ffi`, and `libreplxx`)
  - Container e2e diff is clean after run (`build/e2e_diff.txt` removed by `scripts/run_e2e.sh` on success).
- [x] Lock in the new direct AOT lowering for type-definition forms:
  keep `deftype`, `defabstract`, `defunion`, `defalias`, and `defeffect` on structured helper paths and prevent regressions back to `aot::eval_serialized_expr(...)`.
- [x] Add direct compiled-define regression coverage for each definition form individually:
  `deftype`, `defabstract`, `defunion`, `defalias`, `defeffect`, plus constructor/variant global sync expectations where applicable.
  completed: compiler regression group now asserts direct helper lowering for all five definition forms and absence of delegated eval fallback.
- [x] Extend direct typed-define parity coverage beyond single-method smoke cases:
  cover repeated method-table extension, fallback installation, captured globals, and direct AOT callable registration without delegated eval.
  completed: compiler regression group now asserts multi-method direct typed helper call counts, typed+untyped fallback mixes, captured-global behavior, and no delegated eval fallback.
- [x] Remove or explicitly quarantine `aot::eval_serialized_expr(...)` from normal compiler paths:
  it should not remain an accidental escape hatch for core language lowering; if it stays, it should be treated as legacy/debug-only infrastructure.
  completed: compiler type-form lowering no longer has a serialized-eval default path, compiler regressions assert no emitted `aot::eval_serialized_expr(...)` in normal lowering, and the AOT bridge is now marked legacy/debug-only via deprecation.
- [x] Keep compiled `explain 'dispatch` on the direct helper path:
  preserve current direct lowering for compiled dispatch explain output and add regression coverage for multi-arg + zero-arg call targets.
  completed: compiler regression group now asserts direct `aot::explain_dispatch(...)` lowering for baseline, multi-arg call targets, and zero-arg call targets with no serialized-eval fallback.
- [x] Expand compiled `explain 'effect` direct lowering coverage:
  cover both canonical special-form shapes and call-surface spellings (`perform`/`signal`, `resolve`) so parser shape changes do not silently fall back to generic primitive invocation.
  completed: compiler regressions now assert direct effect-helper lowering for `signal`, `perform` call-surface, and `resolve` explain targets, with no serialized-eval fallback emission.
- [x] Decide the long-term representation of AOT closures:
  today AOT closures still use primitive wrappers rather than first-class runtime `CLOSURE` values, which leaves `closure?`/`type-of` parity incomplete.
  decided: keep primitive wrapper representation for compiled AOT closures for now (`aot-closure`/`aot-closure-var`) but classify them semantically as `Closure` in `closure?`, `type-of`, and runtime type inference; parity e2e cases were added for closure factory + variadic lambda introspection.
- [x] Decide whether compiled module/import/export semantics are intentionally static:
  AOT still treats module surfaces differently from interpreter/JIT runtime handling; either align semantics or document compiled-module behavior as intentionally distinct.
  decided: keep intentionally static compiled semantics for now; compiler backend inlines module bodies and lowers `import`/`export-from` to command-style `Void` no-ops. `LANGUAGE_SPEC`/module reference docs now state this explicitly, and compiler regression coverage asserts the static lowering shape.
- [x] Run a Docker-capped parity regression pass after the direct-lowering change:
  include compiler slice, focused explain/type-dispatch e2e checks, and at least one compiled-binary parity sample that exercises typed define + type forms together.
  completed:
  - Docker-capped compiler slice passes with direct-lowering regressions (`104 passed, 0 failed`) via:
    - `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
  - Focused explain/type-dispatch parity is covered by Docker e2e cases (`dispatch exact subtype`, `dispatch parent subtype`, `dispatch numeric explicit conversion`, `dispatch ambiguity reason`).
  - Compiled-binary parity sample requirement is satisfied by Docker e2e setup fixtures that combine type forms + typed defines (`define [abstract]/[struct]/[union]/[alias]` plus typed `define` dispatch methods), validated by the same `scripts/run_e2e.sh` container pass.

## Active Failure Recovery (2026-03-11)

- [x] Fix `advanced`: `non-tail recursion exceeds former 1024 eval cap` (interp + jit parity).
- [x] Fix `advanced`: `parser matrix accepts Value bool constructor`.
- [x] Fix `advanced`: `match Some` regression (`got 0`).
- [x] Fix `advanced`: `match nested Some` regression (`got 0`).
- [x] Fix `async`: `dns-resolve localhost (fiber)` (`got='?'`).
- [x] Fix `async`: `dns-resolve returns string (fiber)`.
- [x] Re-run Docker-capped `advanced` and `async` slices and record clean pass.

## Optimization Follow-Up Queue (2026-03-10)

### O1. Boundary Profiling and Evidence Capture

- [x] Run a Docker-capped profiling pass for boundary-heavy workloads and record the baseline counters/trace summary. (`docs/plans/boundary-profiling-baseline-2026-03-11.md`)
- [x] Capture scope-chain scan pressure and hint-hit/miss ratios from the current boundary telemetry surface. (`docs/plans/boundary-profiling-baseline-2026-03-11.md`)
- [x] Identify which return-path outcomes dominate in practice (`reused`, destination-built, direct-promoted, spliced, disallowed). (`docs/plans/boundary-profiling-baseline-2026-03-11.md`)
- [x] Record the accepted regression envelope for boundary-heavy workloads in docs or scripts. (`docs/plans/boundary-profiling-baseline-2026-03-11.md`)

### O2. Dispatch Hot-Path Allocation Reduction

- [x] Add focused micro-bench coverage for method dispatch and typed lambda call boundaries before and after the allocation changes. (`docs/plans/dispatch-hot-path-benchmark-baseline-2026-03-11.md`)

### O3. Structural Equality Workspace Reuse

- [x] Audit `src/lisp/eval_pattern_support.c3` deep equality workspace allocation (`stack`/`seen`) under nested list/array comparisons. (`docs/plans/equality-workspace-audit-2026-03-11.md`)
- [x] Introduce a bounded scratch or inline-first workspace strategy that preserves cycle safety and current semantics. (`src/lisp/eval_pattern_support.c3`)
- [x] Add a regression/benchmark slice for large nested equality comparisons so allocator churn is measurable. (`docs/plans/equality-nested-benchmark-baseline-2026-03-11.md`)

### O4. Scheduler and Offload Throughput

- [x] Add micro-bench coverage for scheduler + async I/O/offload interaction hotspots (`queue`, completion publish, TLS/http offload). (`docs/plans/scheduler-offload-hot-path-benchmark-baseline-2026-03-11.md`)
- [x] Audit per-request heap allocation in `src/lisp/scheduler_offload_worker.c3` and classify reusable worker-local buffers versus required owned outputs. (`docs/plans/scheduler-offload-allocation-audit-2026-03-11.md`)
- [x] Prototype a narrow pool/reuse strategy for offload request/completion scaffolding without changing boundary ownership semantics. (`docs/plans/scheduler-offload-queued-work-pool-prototype-2026-03-11.md`)
- [x] Validate that any pooling change preserves generation/task handoff correctness and offload failure cleanup. (`docs/plans/scheduler-offload-queued-work-pool-prototype-2026-03-11.md`)

### O5. Deduce Scan/Query Throughput

- [x] Add benchmarks for `deduce` scan/query/count paths at corpus sizes large enough to expose regression envelopes. (`docs/plans/deduce-scan-query-count-benchmark-baseline-2026-03-11.md`)
- [x] Measure row materialization cost in `relation_scan_range(...)`, including per-row hashmap creation and per-column symbol-key allocation. (`docs/plans/deduce-scan-range-materialization-cost-baseline-2026-03-11.md`)
- [x] Reduce avoidable scan-path allocation where semantics allow (for example cached relation key values or other stable row-shape helpers). (`docs/plans/deduce-scan-range-key-cache-optimization-2026-03-11.md`)
- [x] Evaluate whether `deduce-query` should stay as full-scan + callback filtering or gain a narrower optimization path for common predicates. (`docs/plans/deduce-query-optimization-evaluation-2026-03-11.md`)

### O6. Largest-First Runtime Modularization

- [x] Continue largest-first modularization for oversized runtime modules that still combine hot-path logic with diagnostics or explainability helpers. (`docs/plans/runtime-modularization-split-2026-03-11.md`)
- [x] Prioritize split candidates by size and hot-path relevance (`schema.c3`, `eval_dispatch_types.c3`, `scheduler_offload_worker.c3`). (`docs/plans/runtime-modularization-split-2026-03-11.md`)
- [x] Keep behavior unchanged while splitting, and pair each split slice with targeted validation for the touched subsystem. (`docs/plans/runtime-modularization-split-2026-03-11.md`)

### O7. Deduce Robust Datalog Engine

#### O7.1 Semantics Lock (Spec + Scope)

- [x] Add an execution roadmap for robust Deduce Datalog evolution with measurable phase gates. (`docs/plans/deduce-robust-datalog-roadmap.md`)
- [x] Add a dedicated Deduce Datalog semantics draft (rules, recursion, stratified negation, explain contract, compatibility rules). (`docs/deduce-datalog-spec.md`)
- [x] Finalize v1/v1.5 split in the spec (core deliverables vs post-v1 deferred features) and tag each item with acceptance tests. (`docs/deduce-datalog-spec.md`)
- [x] Lock canonical public rule/query surface names (`deduce/rule!`, `deduce/query`, `deduce/match`, explain/analyze commands) with explicit compatibility notes. (`docs/deduce-datalog-spec.md`)

#### O7.2 Logical IR + Validation Pipeline

- [x] Introduce internal rule/query IR structs (`Predicate`, `Atom`, `Rule`, `Program`, `Binding`) decoupled from raw list AST handling. (`src/lisp/deduce_rule_ops.c3`)
- [x] Add deterministic variable normalization so alpha-equivalent rules share the same canonical IR. (`src/lisp/deduce_rule_ops.c3`)
- [x] Add arity validation for all head/body predicate references. (`src/lisp/deduce_rule_ops.c3`, `src/lisp/deduce_db_handles.c3`)
- [x] Add unsafe-variable validation (head variables must be grounded in positive body atoms). (`src/lisp/deduce_rule_ops.c3`)
- [x] Add negation-safety validation (variables in negated atoms must be positively bound beforehand). (`src/lisp/deduce_rule_ops.c3`)
- [x] Add non-stratifiable dependency rejection with explicit `deduce/non-stratifiable-program` error payloads. (`src/lisp/deduce_rule_ops.c3`)

#### O7.3 Predicate Access + Index Layer

- [x] Extend relation metadata to describe available secondary/composite indexes. (`src/lisp/deduce_db_handles.c3`)
- [x] Implement bound-argument-mask analysis for atoms to drive index-choice eligibility. (`src/lisp/deduce_rule_ops.c3`)
- [x] Add tuple-iterator accessors that avoid eager per-row dict materialization in hot paths.
  completed:
  - introduced `DeduceTupleIterator` open/next/close helpers in `src/lisp/deduce_relation_scan_helpers.c3`,
  - migrated `relation_scan_range(...)`, `deduce/query`, and `deduce/match` scan loops to tuple iteration.
- [x] Preserve current API payload shapes by delaying dict materialization to output/projection boundaries.
  completed:
  - dict rows are now materialized only at output/projection boundaries via `deduce_relation_materialize_row_dict(...)`,
  - API return shapes remain unchanged (scan/query/match still return dict rows).
- [x] Add index-creation/open validation tests across memory-backed and file-backed Deduce DB modes.
  completed:
  - added schema index-shape assertions for memory-backed `define relation` and file-backed `open-named` paths in `src/lisp/tests_deduce_rule_groups.c3`,
  - validated via `OMNI_LISP_TEST_SLICE=deduce` (`56 passed, 0 failed`).

#### O7.4 Planner v1

- [x] Implement logical-to-physical planning for conjunctive rule bodies and explicit query bodies.
  completed:
  - added deterministic conjunctive-body planner kernel (`deduce_plan_conjunctive_body`) in `src/lisp/deduce_rule_ops.c3`,
  - `deduce/rule!` registration now stores body predicates/masks/index selections in planner order instead of raw parse order,
  - planner scoring now prioritizes bound-mask/index selectivity with stable tie-breaking.
- [x] Add baseline relation statistics capture (cardinality + distinct estimates) to support join ordering.
  completed:
  - extended relation schema metadata with `cardinality_estimate` and `distinct_estimate`,
  - added schema-estimate maintenance hooks on non-transactional `fact!`/`retract!`/`clear!`/`drop!` paths,
  - planner scoring now incorporates baseline schema cardinality/distinct estimates.
- [x] Add filter pushdown and projection pushdown decisions in physical planning.
  completed:
  - planner now computes and stores per-step `body_filter_pushdown` and `body_projection_masks` metadata at `deduce/rule!` registration time,
  - filter pushdown decision currently marks bound-argument predicates (and local equality-constrained atoms) as pushdown candidates,
  - projection pushdown decision now tracks downstream-needed variable columns per planned step (head + future-body usage),
  - `deduce/explain` now surfaces these decisions per step as `filter-pushdown` and `projection-mask`,
  - validated via `OMNI_LISP_TEST_SLICE=deduce` (`59 passed, 0 failed`).
- [x] Add planner fallback behavior when no index is available (explicit full-scan operator node rather than implicit behavior).
  completed: planner step IR now records explicit operator kind (`FULL_SCAN`/`INDEX_SCAN` and negated variants), with deterministic full-scan fallback when no index is selected.
- [x] Add deterministic `deduce/explain` payload skeleton for chosen operators and join order.
  completed:
  - added `deduce/explain` namespaced + dispatch command surfaces (`deduce/explain`, `deduce 'explain ...`) backed by planner metadata from registered rule signatures,
  - payload now includes deterministic `join-order` and per-step operator/index fields (`steps` with `predicate`, `operator`, `bound-mask`, `selected-index`, `negated`),
  - added deterministic explain regression in `src/lisp/tests_deduce_rule_groups.c3`,
  - validated via `OMNI_LISP_TEST_SLICE=deduce` (`59 passed, 0 failed`).

#### O7.5 Join Execution

- [x] Implement index nested-loop join operator for selective bound predicates.
  completed:
  - added `deduce_index_nested_loop_join_count(...)` in `src/lisp/deduce_relation_scan_helpers.c3` with explicit join-column validation and per-probe execution stats,
  - operator now reuses the outer iterator read transaction for same-environment joins (with cross-env readonly fallback txn), avoiding LMDB same-thread reader slot failures,
  - exact inner probes now use encoded tuple keys via `mdb_get(...)` for index-backed existence checks,
  - added regressions in `src/lisp/tests_deduce_rule_groups.c3` for exact bound join and literal+bound join probe paths,
  - validated with `c3c build` and `OMNI_LISP_TEST_SLICE=deduce LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`61 passed, 0 failed`).
- [x] Implement hash-join operator for large/intermediate joins.
  completed:
  - added `deduce_hash_join_count(...)` in `src/lisp/deduce_relation_scan_helpers.c3`,
  - implementation builds an in-memory join-key frequency map from the inner side (with optional static literal filtering) and probes it from the outer side,
  - keyed join counts support duplicate inner matches (multi-match emission) while preserving `DeduceJoinExecStats` probe/emission counters,
  - added hash-join regressions in `src/lisp/tests_deduce_rule_groups.c3` for multi-match and literal+bound selective probes,
  - validated with `c3c build` and `OMNI_LISP_TEST_SLICE=deduce LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`63 passed, 0 failed`).
- [x] Add adaptive runtime fallback thresholds for large estimate miss conditions.
  completed:
  - added `deduce_adaptive_join_count(...)` in `src/lisp/deduce_relation_scan_helpers.c3`,
  - adaptive policy now chooses index-probe first and falls back to hash join when probe volume and miss-ratio thresholds indicate estimate/selectivity miss,
  - fallback thresholds are configurable per call (`fallback_probe_threshold`, `fallback_miss_ratio_percent`) with sensible defaults when omitted/invalid,
  - added regressions in `src/lisp/tests_deduce_rule_groups.c3` for both fallback-to-hash and keep-index scenarios,
  - validated with `c3c build` and `OMNI_LISP_TEST_SLICE=deduce LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`65 passed, 0 failed`).
- [x] Add operator-level counters (rows read, rows emitted, index hits/misses) for planner observability.
  completed:
  - `DeduceJoinExecStats` tracks `outer_rows`, `inner_rows`, `inner_probes`, `index_hits`, `index_misses`, and `rows_emitted`,
  - index/hash/adaptive helper paths populate these counters and regression tests assert core hit/miss/fallback behavior,
  - `deduce/explain` per-step payload now includes deterministic `counters` dictionary:
    - `rows-read`, `rows-emitted`, `index-hits`, `index-misses`, `counter-kind`,
  - current `counter-kind` is `estimated`; counters are derived from rule-step planner metadata + relation cardinality/distinct estimates.

#### O7.6 Recursive Evaluation

- [x] Implement naive bottom-up evaluator as correctness reference mode.
  completed:
  - extended persisted `DeduceRuleSignature` metadata to include canonical head/body term mappings (variable ids + promoted literal payloads) in planner order,
  - added `deduce/analyze` + `(deduce 'analyze ...)` command surfaces backed by a deterministic naive fixpoint evaluator,
  - naive evaluator now executes installed rules over LMDB tuples with positive/negated body matching and derived-head insertion until convergence (or guarded iteration limit),
  - added recursive closure regression in `src/lisp/tests_deduce_rule_groups.c3` (transitive `ancestor` derivation via `parent` rules),
  - validated with `c3c build` and `OMNI_LISP_TEST_SLICE=deduce LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`66 passed, 0 failed`).
- [x] Implement SCC decomposition to isolate recursive strata.
  completed:
  - added deterministic SCC decomposition (Tarjan) over rule dependency graph and persisted per-rule component assignment in analyze-time planning,
  - evaluator now executes SCCs in dependency order (dependency-first component schedule), applying local fixpoint loops only to recursive SCCs,
  - `deduce/analyze` payload now reports SCC diagnostics (`strata-count`, `recursive-strata`) alongside existing run stats,
  - extended recursive analyze regression in `src/lisp/tests_deduce_rule_groups.c3` to assert SCC diagnostics for transitive-closure fixture.
- [x] Implement semi-naive delta evaluator for recursive SCCs.
  completed:
  - recursive SCC evaluation now runs a delta-driven semi-naive loop (`deduce_seminaive_evaluate_recursive_component`) instead of naive local fixpoint repetition,
  - evaluator seeds per-predicate delta sets from current SCC relation state, then iterates rule application using anchor-step gating over recursive positive body atoms,
  - newly derived tuples are tracked into next-iteration delta sets and used as the convergence signal for recursive SCC termination,
  - `deduce/analyze` output now includes `execution-engine` with value `semi-naive-scc` while retaining `mode = naive-bottom-up` for compatibility,
  - updated recursive analyze regression in `src/lisp/tests_deduce_rule_groups.c3` to assert `execution-engine`,
  - validated with `c3c build` and `OMNI_LISP_TEST_SLICE=deduce LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`66 passed, 0 failed`).
- [x] Add convergence diagnostics and hard-stop safeguards for pathological rule graphs.
  completed:
  - analyze output now includes convergence-oriented runtime diagnostics:
    `iteration-limit` and `max-component-iterations`,
  - recursive SCC iteration-limit failures now emit structured payload data
    (`component-id`, `iteration-limit`) on `deduce/analyze-iteration-limit`,
  - recursive analyze regression now asserts the new convergence diagnostics
    fields in `src/lisp/tests_deduce_rule_groups.c3`,
  - validated with `c3c build` and `OMNI_LISP_TEST_SLICE=deduce LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`66 passed, 0 failed`).
- [x] Add regression parity tests: naive output == semi-naive output for recursive fixtures.
  completed:
  - `deduce/analyze` now accepts optional recursive-engine selector (`'naive` / `'semi-naive`) while preserving one-arg default behavior,
  - analyze payload now reports selected engine through `execution-engine` (`naive-scc` or `semi-naive-scc`),
  - added recursive closure parity regression in `src/lisp/tests_deduce_rule_groups.c3` asserting naive vs semi-naive equality on closure count and derived-facts,
  - validated with `c3c build` and `OMNI_LISP_TEST_SLICE=deduce LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`67 passed, 0 failed`).

#### O7.7 Stratified Negation Runtime

- [x] Implement stratum scheduler that enforces dependency order across positive/negative edges.
  completed:
  - SCC planning now computes explicit per-component stratum assignments (`component_strata`) with negative-edge strictness (`from > to` on negated edges),
  - evaluate order is now stratum-first and dependency-stable within each stratum (topological rank tie-break),
  - analyze payload now includes `stratum-count` alongside existing SCC diagnostics.
- [x] Add runtime rejection path for non-stratifiable programs at rule-install time.
  completed:
  - `deduce_rule_validate_stratification(...)` rejects negative recursion cycles with `deduce/non-stratifiable-program` during `deduce/rule!` install,
  - rejection path remains covered by the existing `src/lisp/tests_deduce_rule_groups.c3` regression (`deduce rule! rejects negative recursion cycle`).
- [x] Add targeted tests for mixed positive/negative recursion boundaries.
  completed:
  - added `deduce analyze enforces negation stratum order` regression (negative dependency scheduling correctness),
  - added `deduce analyze handles mixed recursive and negated stratum boundaries` regression (recursive positive closure + negated downstream stratum),
  - validated with `c3c build` and `OMNI_LISP_TEST_SLICE=deduce LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`69 passed, 0 failed`).

#### O7.8 Incremental Maintenance (Post-v1)

- [x] Design dependency-aware delta propagation for `fact!`/`retract!` updates against derived relations.
  completed:
  - added concrete incremental propagation design doc with execution-ready
    runtime contract in
    `docs/plans/deduce-incremental-delta-propagation-design-2026-03-13.md`,
  - added dependency-aware dirty propagation helpers in Deduce runtime
    (`deduce_db_note_mutation`, transitive head invalidation over rule
    dependencies),
  - `deduce/analyze` now reports incremental diagnostics
    (`incremental-*-...`) and resets dirty tracking after successful full
    fixpoint run.
- [x] Implement transaction-safe derived invalidation/update boundaries.
  completed:
  - write-transaction (`deduce 'block ... 'write`) mutations now accumulate in
    per-relation transaction logs (insert/delete + destructive markers),
  - on successful `deduce 'commit`, logs apply schema estimate deltas and
    dependency-aware invalidation updates atomically at commit boundary,
  - on `deduce 'abort`, pending mutation logs are discarded with no incremental
    state drift,
  - if mutation logging degrades (allocation pressure), commit falls back to
    conservative `full-recompute` invalidation mode.
- [x] Add mutation-heavy benchmark suite to compare full recompute vs incremental maintenance.
  completed:
  - added `run_deduce_incremental_mutation_benchmarks(...)` in
    `src/lisp/tests_deduce_query_groups.c3` behind the existing
    `OMNI_DEDUCE_BENCH=1` gate,
  - benchmark now emits `OMNI_BENCH_SUMMARY suite=deduce_incremental_mutation`
    with tracked-mode (`fact!` only) versus full-recompute-mode
    (`retract!`+`fact!`) lanes and per-lane analyze/mode counters,
  - recorded baseline capture in
    `docs/plans/deduce-incremental-mutation-benchmark-baseline-2026-03-13.md`.

#### O7.9 Benchmarks + Validation Gates

- [x] Add selective join benchmark suite with explain-counter assertions.
  completed:
  - added `run_deduce_selective_join_benchmarks(...)` in
    `src/lisp/tests_deduce_query_groups.c3` behind `OMNI_DEDUCE_BENCH=1`,
  - benchmark emits `OMNI_BENCH_SUMMARY suite=deduce_selective_join` with
    explain-lane assertion counters (`explain_ok`, `explain_mode_miss`) and
    captured counter estimates (`step0_rows_read`, `step1_rows_read`),
  - analyze lane validates derived output cardinality each iteration for the
    selective join fixture (`expected_rows`),
  - baseline capture recorded in
    `docs/plans/deduce-selective-join-benchmark-baseline-2026-03-13.md`.
- [x] Add recursive transitive-closure benchmark suite for semi-naive speedup tracking.
  completed:
  - added `run_deduce_recursive_closure_benchmarks(...)` in
    `src/lisp/tests_deduce_query_groups.c3` behind `OMNI_DEDUCE_BENCH=1`,
  - benchmark compares explicit analyze engines (`'naive` vs `'semi-naive`)
    on a chain-closure fixture and emits engine-hit + cardinality assertions
    per lane,
  - summary now includes `speedup_x1000` for regression tracking without
    assuming a fixed winner across data shapes/runtime revisions,
  - baseline capture recorded in
    `docs/plans/deduce-recursive-closure-benchmark-baseline-2026-03-13.md`.
- [x] Add skewed-cardinality benchmark suite to stress planner estimate robustness.
  completed:
  - added `run_deduce_skewed_cardinality_benchmarks(...)` in
    `src/lisp/tests_deduce_query_groups.c3` behind `OMNI_DEDUCE_BENCH=1`,
  - benchmark seeds a heavily skewed join fixture and emits explain-counter
    stability metrics (`explain_ok`, `explain_mode_miss`) plus selectivity
    tracking (`explain_selectivity_hits`, `step0_rows_read`, `step1_rows_read`),
  - analyze lane validates derived cardinality under skew each iteration,
  - baseline capture recorded in
    `docs/plans/deduce-skewed-cardinality-benchmark-baseline-2026-03-13.md`.
- [x] Add container-gated Deduce perf envelope checks to detect regression drift across releases.
  completed:
  - added `scripts/run_deduce_perf_envelope.sh`:
    - Docker-gated outside containers (re-enters via
      `scripts/run_validation_container.sh`),
    - runs Deduce benchmark lane with `OMNI_LISP_TEST_SLICE=deduce` and
      `OMNI_DEDUCE_BENCH=1`,
    - enforces summary assertions across all Deduce benchmark suites
      (`deduce_scan_query_count`, `deduce_incremental_mutation`,
      `deduce_selective_join`, `deduce_recursive_closure`,
      `deduce_skewed_cardinality`),
    - mounts required host headers/libs into validation container for this repo
      toolchain path (`yyjson`, `bearssl`, `libuv`, `libffi`, `libreplxx`).
  - validated in Docker-bound run:
    - `scripts/run_deduce_perf_envelope.sh`
    - result: `Deduce perf envelope checks passed.`
- [x] Run ASAN build and targeted Deduce slices for each memory-sensitive evaluator/planner change.
  completed:
  - `c3c build --sanitize=address`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  - result: `=== Unified Tests: 70 passed, 0 failed ===` with no LeakSanitizer report and zero ASAN summary errors.
  - closure notes:
    - parser/macro AST sub-allocations were moved from raw heap (`mem::malloc`) to AST-arena ownership (`ast_arena_alloc`) across the high-frequency parse/macro conversion paths,
    - this removed the prior parser/macro startup leak footprint from the Deduce ASAN lane.
