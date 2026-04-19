# defect-audit-remediation-wave-2-2026-04-09 Part 04

Source: `docs/plans/defect-audit-remediation-wave-2-2026-04-09.md`

    - destination `cons`, `partial`, `iterator`, and `error` builders keep the
      previous build-scope commit behavior, avoiding source-wrapper fallback
      through nested effect payload return boundaries.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - host `scripts/check_boundary_facade_usage.sh`: green
      - host `scripts/check_boundary_change_policy.sh`: green
      - host `scripts/check_status_consistency.sh`: green
      - host `git diff --check`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=193 fail=0`
  - Closed follow-up `AUDIT-BOUNDARY-INSTANCE-MODULE-ALIAS-GRAPH-068`:
    - `src/lisp/eval_boundary_provenance.c3` now treats `INSTANCE` and
      `MODULE` as graph-bearing alias payloads, matching committed-root graph
      audit coverage.
    - the rare path uses a heap-backed value/env reachability scan instead of
      adding large local arrays to the hot alias walker stack frame.
    - the scan rejects reuse when by-value instance fields or module/env
      bindings still reach the releasing scope.
    - root-persistent env boxes are still traversed for parent/binding edges;
      they are only excluded from direct temp-frame ownership checks.
    - `src/lisp/tests_memory_lifetime_boundary_state_groups.c3` now covers an
      instance field graph retaining a releasing-scope array payload.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - host `scripts/check_boundary_facade_usage.sh`: green
      - host `scripts/check_status_consistency.sh`: green
      - host `git diff --check`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=194 fail=0`
  - Closed follow-up `AUDIT-JIT-MODULE-VALUE-GROWTH-069`:
    - `src/lisp/value_predicates_accessors_basic.c3` now snapshots module
      descriptors into root-scope storage for first-class `MODULE` values, so
      module values no longer point into the reallocating interpreter module
      table.
    - `src/lisp/eval_path.c3` now fails closed on invalid module descriptors
      before reading exports/envs.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now forces
      module table growth after creating a first-class module value and
      verifies exported path access still works under normal and ASAN smoke.
  - Closed follow-up `AUDIT-JIT-MUTATION-PROMOTION-NULL-GUARDS-070`:
    - `src/lisp/jit_jit_closure_let_set_helpers.c3` now rejects null
      cons-field promotion and null instance-field boundary-copy results before
      mutating storage.
    - `src/lisp/jit_jit_define_method_table.c3` now rejects null typed method
      implementations and null global define promotion results before mutating
      method-table entries or fallbacks.
    - `src/lisp/aot_type_definitions.c3` mirrors the typed-method promotion
      null guard for AOT.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - host `scripts/check_boundary_facade_usage.sh`: green
      - host `scripts/check_boundary_change_policy.sh`: green
      - host `scripts/check_status_consistency.sh`: green
      - host `git diff --check`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=196 fail=0`
  - Closed follow-up `AUDIT-TCO-ENV-COPY-FAIL-CLOSED-071`:
    - `src/lisp/jit_jit_eval_scope_copy.c3` now uses checked boundary-copy
      results while copying TCO env-frame bindings and rejects copy faults,
      null results, and copied `ERROR` values before binding them into a copied
      env frame.
    - Root-persistent env-box parent rewrites now fail closed if the required
      parent-chain copy fails.
  - Closed follow-up `AUDIT-TCO-RECYCLE-HOOK-FAIL-CLOSED-072`:
    - `src/lisp/runtime_backend_hooks.c3` now preserves `*env_io`, releases the
      fresh recycle scope, restores the original call scope, and returns an
      explicit error when TCO env-chain copy fails.
    - Active defer retargeting is restored back to the original call scope on
      that failure path.
    - `src/lisp/tests_memory_lifetime_tco_budget_groups.c3` now covers both the
      direct env-copy failure and recycle-hook state-restore failure paths.
    - `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` wires both
      regressions into the bounded smoke lane.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - host `scripts/check_boundary_facade_usage.sh`: green
      - host `scripts/check_boundary_change_policy.sh`: green
      - host `scripts/check_status_consistency.sh`: green
      - host `git diff --check`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=198 fail=0`
  - Closed follow-up `AUDIT-JIT-MUTABLE-LOCAL-NULL-BOX-073`:
    - `src/lisp/jit_jit_apply_multi_prims_tail.c3` now makes
      `jit_env_lookup_local(...)` reject null envs and missing bindings with an
      explicit `jit: missing mutable local binding` error.
    - `jit_env_reparent(...)` now returns the effective env and lets compiled
      capture setup continue with the helper result when the original env box is
      null.
    - `src/lisp/jit_jit_compile_expr_basic.c3` and
      `src/lisp/jit_jit_emit_helpers.c3` now consume those checked helper
      contracts.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3` covers both helper
      contracts in `jit-policy`.
  - Closed follow-up `AUDIT-RAISE-PAYLOAD-NESTED-PENDING-074`:
    - `src/lisp/value_constructors.c3` now builds handled raise payload
      dictionaries through the non-raising hashmap allocation helper instead
      of the raising dictionary constructor path.
    - Payload dictionary allocation failure no longer publishes a stale nested
      `raise_pending` value that the surrounding handler can accidentally
      consume as a successful handled raise.
  - Closed follow-up `AUDIT-DISPATCH-PAYLOAD-NO-PRESEED-075`:
    - `src/lisp/prim_collection_hashmap.c3` now provides
      `make_hashmap_no_raise(...)` for optional dictionary payload construction
      that must not publish allocation failures through the raise channel.
    - `src/lisp/eval_dispatch_error_payloads.c3` now uses that helper so
      dispatch diagnostic payload allocation failure cannot pre-seed
      `raise_pending` before the intended dispatch/type error is raised.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3` covers the active
      handler case directly.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - host `scripts/check_boundary_facade_usage.sh`: green
      - host `scripts/check_boundary_change_policy.sh`: green
      - host `scripts/check_status_consistency.sh`: green
      - host `git diff --check`: green
      - bounded normal `jit-policy` with FTXUI smoke enabled:
        `pass=51 fail=0`
      - bounded ASAN `jit-policy`: `pass=50 fail=0`
  - Closed follow-up `AUDIT-CTOR-PAYLOAD-NO-PRESEED-076`:
    - `src/lisp/primitives_meta_types_ctor_helpers.c3` now uses
      `make_hashmap_no_raise(...)` for optional constructor mismatch payloads
      and checks payload key interning before symbol construction.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` covers the
      active handler case directly and verifies constructor payload allocation
      failure leaves pending raise state clear.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - host `scripts/check_boundary_facade_usage.sh`: green
      - host `scripts/check_boundary_change_policy.sh`: green
      - host `scripts/check_status_consistency.sh`: green
      - host `git diff --check`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=199 fail=0`
  - Closed follow-up `AUDIT-DEDUCE-INTEGRITY-PAYLOAD-NO-PRESEED-077`:
    - `src/lisp/deduce_relation_ops_validation_payload.c3` now uses
      `make_hashmap_no_raise(...)` for optional integrity/check-context
      diagnostic payloads and routes payload field insertion through local
      no-raise setters.
    - `src/lisp/deduce_rule_eval_exec_component_state.c3` now uses the same
      non-raising helper for optional iteration-limit diagnostic payloads
      returned before the later iteration-limit raise.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` covers the
      active handler case directly and verifies deduce integrity and
      iteration-limit payload allocation failures leave pending raise state
      clear.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=200 fail=0`
  - Closed follow-up `AUDIT-DEDUCE-GOAL-DIRECTED-PAYLOAD-NO-PRESEED-078`:
    - `src/lisp/deduce_rule_eval_analyze_setup.c3` now uses
      `make_hashmap_no_raise(...)` and local checked insertion for
      goal-directed selector analysis diagnostic payload dictionaries before
      publishing the existing `deduce/analyze-out-of-memory` fallback.
    - `src/lisp/deduce_rule_eval_fixpoint_goal_directed_selector_prepare.c3`
      now uses the same non-raising payload construction path for
      goal-directed selector and relation surface diagnostics before
      publishing the existing `deduce/out-of-memory` fallback.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=200 fail=0`
  - Closed follow-up `AUDIT-DEDUCE-GOAL-DIRECTED-INTERN-GUARDS-079`:
    - `src/lisp/deduce_rule_ops_explain_goal_directed_components.c3` now
      routes goal-directed blocker and shape symbols through a checked helper
      that rejects `INVALID_SYMBOL_ID` before constructing payload symbols.
    - `src/lisp/deduce_rule_ops_explain_snapshot.c3` now uses that helper for
      goal-directed shape and execution-path payload values.
    - `src/lisp/deduce_why_result_path_payload.c3` now rejects invalid lookup
      key interning before constructing a temporary dictionary key symbol.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - bounded normal+ASAN `deduce` slice: `pass=330 fail=0`
  - Closed follow-up `AUDIT-RUNTIME-RESULT-KEY-INTERN-GUARDS-080`:
    - `src/lisp/eval_dispatch_error_payloads.c3` now rejects invalid payload
      keys and invalid symbol payload values in the optional dispatch payload
      setter.
    - `src/lisp/async_process_spawn.c3` now checks process-spawn result key
      interning before constructing dictionary key symbols and closes spawned
      resources on failure.
    - `src/lisp/http_url_response.c3` now checks HTTP response-field key
      interning before publishing response payload dictionaries.
    - `src/lisp/prim_ui_ftxui_helpers.c3` now checks UI dictionary lookup key
      interning before probing.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - bounded normal+ASAN `async` slice with FTXUI smoke enabled:
        `pass=65 fail=0`
      - bounded normal `jit-policy`: `pass=51 fail=0`
      - bounded ASAN `jit-policy`: `pass=50 fail=0`

  - Closed follow-up `AUDIT-RUNTIME-INTERN-RAISEPAYLOAD-GUARDS-081`:
    - `src/lisp/eval_init_primitive_registration.c3` now rejects failed `nil`
      symbol interning before defining the language constant.
    - `src/lisp/jit_jit_closure_runtime.c3` now treats failed promise env-tag
      interning as a non-match instead of probing environments with invalid
      symbols.
    - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now builds
      unhandled-effect raise payload dictionaries through the non-raising
      hashmap helper and checks payload-key interning before publication.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - bounded normal `jit-policy`: `pass=51 fail=0`
      - bounded ASAN `jit-policy`: `pass=50 fail=0`
      - bounded normal `advanced`: `pass=1185 fail=0`
      - bounded ASAN `advanced`: `pass=1172 fail=0`

  - Closed follow-up `AUDIT-CONSTRUCTOR-DISPATCH-SURFACE-083`:
    - public conversion aliases `number->string`, `symbol->string`,
      `string->symbol`, `exact->inexact`, and `inexact->exact` were removed
      in favor of canonical callable constructors `String`, `Symbol`,
      `Float64`, and `Integer`.
    - `set-size` and `set->list` were removed from public primitive
      registration and compiler primitive maps; `length` now covers set
      cardinality and `List(Set ...)` materializes deterministic canonical set
      element order.
    - fast-dev-only constructor aliases `Int`/`Bool` and duplicate
      `filesystem-*` primitive aliases were removed.
    - schema validation now uses `array-of` instead of `vector-of`.
    - `lib/immer.omni` now exposes `persistent-array`,
      `persistent-dictionary`, and `persistent-set` names instead of
      `vector`, `hash-map`, and `hash-set`; the broken generic
      `count`/`conj`/`into` facade was removed because current opaque FFI
      handles have no defined persistent collection predicates.
    - residual open items were split into explicit TODO lanes for:
      - list/string constructor semantics,
      - string-to-number parse/coercion naming,
      - lowercase `list` helper policy,
      - `fs-*` versus `filesystem-*` naming,
      - typed persistent Immer wrappers or explicit non-generic API
        (later superseded by retiring the optional Immer bridge entirely),
      - repeated bounded `http` and `pika` slice failures.
    - validation:
      - host `c3c build --warn-deprecation=no`: green
      - bounded `string-type` slice: `pass=40 fail=0`
      - bounded `advanced` slice: `pass=1189 fail=0`
      - bounded `compiler` slice: `pass=194 fail=0`
      - quick eval confirmed `set-size`/`set->list` are unbound,
        `length`/`List(Set ...)` behavior is correct, and `array-of` schema
        validation works.

- Closed slice `AUDIT-OFFLOAD-WIDTH-GUARDS-066`:
  - `src/lisp/scheduler_offload_network.c3` now rejects listener file
    descriptors outside `int` range before calling `tcp_accept_fd(...)`.
  - `src/lisp/eval_repl_server_state.c3` now formats REPL session IDs from a
    guarded `long` value instead of narrowing `next_session_id` through `int`.
  - `src/lisp/scheduler_offload_ops.c3` now formats atomic temp-path
    `unique_id` suffixes from a guarded `long` value instead of truncating to
    `uint`.
  - `src/lisp/scheduler_state_support_types.c3` now asserts the current
    `OffloadWork` pointer-through-`long` payload width contract at compile
    time.
  - Validation:
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green
    - bounded normal+ASAN `async`: `pass=65 fail=0`
