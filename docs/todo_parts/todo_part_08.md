# Active TODO Index Part 08

Source: `TODO.md`

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
