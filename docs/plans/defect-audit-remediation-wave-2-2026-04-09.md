# Defect Audit Remediation Wave 2 (2026-04-09)

Status: completed execution note

Post-wave follow-up (2026-04-09, late pass):

- Additional follow-up fixes landed after the main wave closure:
  - env-copy now routes disjoint `ARRAY` / `HASHMAP` / `SET` / `METHOD_TABLE`
    bindings through boundary policy instead of returning source wrappers by
    identity;
  - method-table copy/escape abort paths now destroy already-copied heap
    signatures before freeing partial tables;
  - AOT multi-arg trampolines now reject negative arg counts fail-closed;
  - flat-style closure lowering now frees capture payload on closure-constructor
    `ERROR` returns;
  - HTTP client paths now close connections on all post-connect early returns,
    propagate transport read errors, and reject out-of-range URL ports.
- Process-handle concurrency lane is now closed:
  - `src/lisp/async_process_signal_runtime.c3` and
    `src/lisp/async_process_lifecycle.c3` now share a fail-closed in-flight
    guard for process handles, so concurrent `process-wait` / `process-kill`
    reuse returns a deterministic `io/process-handle-busy` error instead of
    racing on the same live handle.
  - `src/lisp/tests_advanced_io_effect_ffi_scheduler_boundary.c3` and
    `src/lisp/tests_advanced_io_effect_ffi_groups.c3` now cover the concurrent
    reuse contract directly.
  - `TODO.md` no longer tracks `AUDIT-ASYNC-PROCESS-CONCURRENCY-003`.
- Remaining follow-up lanes are now also closed:
  - `src/lisp/jit_jit_apply_multi_prims.c3` now uses the canonical
    under-arity format for non-tail fixed/variadic multi-arg closure apply,
    and `src/lisp/tests_compiler_core_groups_fail_closed.c3` now asserts
    prelude-stripped parser coordinates through the compiler pipeline.
  - `src/lisp/eval_promotion_root_clones.c3` and
    `src/lisp/jit_jit_closure_support.c3` now expose a narrow
    method-table-abort cleanup seam, and
    `src/lisp/tests_memory_lifetime_boundary_groups.c3` proves both
    copy-to-parent and escape-promotion partial-cleanup paths reclaim copied
    heap signatures on abort.
  - `TODO.md` is back to zero live items for this audit wave.

Post-wave follow-up (2026-04-10):

- Additional boundary/env-copy hardening landed:
  - `src/lisp/eval_promotion_context.c3` now fails closed if memo-entry
    allocation itself fails, instead of dereferencing a null `PromotionMemoEntry`.
  - `src/lisp/eval_env_copy_values.c3` and
    `src/lisp/eval_env_copy_frame_helpers.c3` now unwind iterator payload
    materialization when iterator wrapper construction fails mid-copy, including
    iterator payloads backed by copied `PARTIAL_PRIM` values.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now covers the
    iterator-wrapper failure rollback lane directly.
- Additional provenance hardening also landed:
  - `src/lisp/eval_boundary_provenance.c3` now walks nested `ARRAY`,
    `HASHMAP` / `SET`, and `METHOD_TABLE` payload edges before admitting
    target-chain fast reuse, so provenance/reuse classification now matches
    the graph-audit ownership model for shared wrappers.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now includes the exact
    missing target-chain-wrapper regression and the bounded smoke lane covers it.
- The destination-builder memo follow-up is also now closed:
  - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` now makes the
    current contract explicit: memo entries created while routing nested
    children inside temporary destination build scopes are builder-local and do
    not survive after the builder returns or aborts.
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now pins
    that contract directly.
- No live backlog items remain from this audit wave.

## Objective

Address the concrete defects found in the 2026-04-09 multi-lane audit without
mixing unrelated workstreams:

- boundary/runtime fail-closed regressions,
- async/REPL/FFI correctness defects,
- compiler/JIT/AOT fail-closed and parity defects,
- tooling/validation contract defects.

`memory/CHANGELOG.md` remains the implementation-truth source once behavior
changes land. This file is execution context only.

## Open Workstreams

### WS1: Boundary Fail-Closed Propagation

Defects:

- nested `copy_to_parent(...)` can silently materialize `null` payloads when a
  nested primitive copy rejects opaque `user_data`,
- nested `promote_to_escape(...)` can embed error/null-like payloads into
  `PARTIAL_PRIM` / `ITERATOR` wrappers instead of failing the boundary,
- primitive-copy rejection allocates target-scope garbage before it knows the
  copy is legal,
- shared-wrapper copy still returns original wrappers for some tags even after
  fast reuse has already been declined.

Primary files:

- `src/lisp/eval_promotion_copy_route_helpers.c3`
- `src/lisp/eval_promotion_copy_wrapper_helpers.c3`
- `src/lisp/eval_promotion_copy.c3`
- `src/lisp/eval_promotion_escape_leaf.c3`
- relevant memory-lifetime regression groups

Required closure:

- recursive boundary copy/promotion must abort transitively,
- no silently corrupted nested structures,
- focused regressions for nested opaque primitive payloads.

### WS2: Async / REPL / FFI Handle Safety

Defects:

- REPL worker startup/teardown can run with uninitialized mutex/condition
  state after partial init failure,
- TCP/UDP/TLS primitives accept arbitrary `FFI_HANDLE` boxes and reinterpret
  unrelated handle payloads,
- `process-spawn` can package constructor error values into a success-shaped
  result,
- HTTP URL/response parsing has malformed-port acceptance and header-slice
  boundary defects.

Primary files:

- `src/lisp/eval_repl_server_state.c3`
- `src/lisp/eval_repl_server_worker.c3`
- `src/lisp/async_socket_handle_runtime.c3`
- `src/lisp/async_tcp_transport_core.c3`
- `src/lisp/tls_handle_lifecycle.c3`
- `src/lisp/tls_primitives.c3`
- `src/lisp/async_process_spawn.c3`
- `src/lisp/http_url_response.c3`

Required closure:

- handle getters must validate handle kind before cast/use,
- REPL worker lifecycle must fail closed on partial init,
- process spawn must propagate constructor failure instead of returning poisoned
  success payloads,
- malformed HTTP URL/response boundaries must reject or parse cleanly.

Progress update (2026-04-09):

- shipped in the working tree:
  - REPL worker startup now refuses partial sync-init state,
  - TCP/UDP/TLS/FS handle getters validate exact handle names before cast,
  - `process-spawn` propagates handle-constructor error values instead of
    returning poisoned success payloads,
  - HTTP URL parsing rejects malformed `:port` suffixes and response header
    extraction trims delimiter residue.
- focused regressions landed for REPL worker start guards, handle-kind
  mismatch rejection, and HTTP parser edges.
- validation status:
  - async slice: green,
  - focused advanced FFI system group: green,
  - broad HTTP slice: green after the duplicate-id HTTP coverage was made deterministic.
- follow-up resolved:
  - the old broad-slice residual `HTTP-CRUD-DUPLICATE-POST-001` is closed.
  - `HTTP-CRUD-CONCURRENT-WRITES-001` is also closed:
    - the example now guards CRUD mutation entrypoints with a shared atomic
      write gate,
    - the concurrency probe moved into the focused `http-crud` slice,
    - the shipped application contract is "one success plus one normalized
      conflict" (`crud write already in progress` or `item already exists`),
      without raw runtime `ERROR` leakage.

### WS3: Compiler / JIT / AOT Fail-Closed and Parity

Defects:

- JIT multi-arg primitive/method-table paths dereference `args` without
  allocation checks,
- AOT multi-arg closure application diverges from JIT strict-arity semantics,
- generated AOT closure capture code dereferences `malloc` results without a
  null guard,
- `parse_program(...)` remains fail-open and current tests codify truncation.

Primary files:

- `src/lisp/jit_jit_apply_multi_prims.c3`
- `src/lisp/jit_jit_apply_multi_prims_tail.c3`
- `src/lisp/aot_runtime_bridge_trampoline.c3`
- `src/lisp/compiler_code_emission_lambda_defs.c3`
- `src/lisp/compiler_native_call_compilation_flat_style.c3`
- `src/lisp/parser_top_level_parse.c3`
- compiler core regressions

Required closure:

- low-memory JIT apply paths return explicit failure instead of crashing,
- AOT/JIT multi-arg closure behavior is semantically aligned,
- generated AOT capture allocation fails closed,
- `parse_program(...)` behavior is made explicit and regression-tested.

Progress update (2026-04-09):

- shipped in the working tree:
  - `src/lisp/jit_jit_apply_multi_prims.c3` and
    `src/lisp/jit_jit_apply_multi_prims_tail.c3` now null-guard multi-arg
    argument-buffer allocation for primitive and method-table dispatch, with a
    dedicated test seam for fail-closed OOM coverage.
  - `src/lisp/parser_top_level_parse.c3` and
    `src/lisp/compiler_program_pipeline_helpers.c3` now fail closed on parser
    error instead of returning a silently truncated prefix program.
  - `src/lisp/aot.c3` now provides shared arg-list counting and arity helpers,
    and `src/lisp/compiler_code_emission_lambda_defs.c3` uses them so
    generated multi-arg lambdas reject under-application, preserve JIT-style
    over-application chaining through `aot::apply_multi(...)`, and reject
    malformed arg lists explicitly.
  - `src/lisp/compiler_native_call_compilation_flat_style.c3` now guards
    closure-capture allocation in flat expression lowering without emitting
    invalid raw `return` statements into non-`Value*` generated contexts.
- focused regressions landed in:
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
  - `src/lisp/tests_compiler_core_groups_fail_closed.c3`
  - `src/lisp/tests_compiler_core_groups.c3`
- validation status:
  - `c3c build`: green
  - `OMNI_LISP_TEST_SLICE=jit-policy`: green
  - `OMNI_LISP_TEST_SLICE=compiler`: green
  - `scripts/run_e2e.sh`: green (`ALL 404 e2e compiler tests passed!`)

### WS4: Tooling / Validation Integrity

Defects:

- `check_status_consistency` disagrees with current zero-item `TODO.md`
  wording,
- validation scripts race on shared `build/obj` artifacts across concurrent
  host/container/ASAN runs,
- e2e compile-source parity is not policy-checked,
- validation status summary trusts exit codes more than emitted summary data,
- extra Docker args are expanded through unsafe shell word splitting.

Primary files:

- `scripts/check_status_consistency.sh`
- `scripts/run_validation_container.sh`
- `scripts/run_validation_status_summary.sh`
- `scripts/c3c_limits.sh`
- `scripts/run_e2e.sh`
- `scripts/check_e2e_baseline_policy.sh`

Required closure:

- zero-item backlog state passes the gate,
- concurrent validation runs cannot corrupt each other’s artifacts,
- e2e source-list drift is guarded,
- summary artifacts assert required suite telemetry,
- Docker argument handling preserves quoting.

Implementation note:

- start with the live `TODO.md` count/status wording fix and the repo-local
  validation lock, then tighten summary telemetry checks without changing the
  broader validation flow.

Progress update (2026-04-09):

- shipped in the working tree:
  - `scripts/check_status_consistency.sh` now accepts the current zero-item
    backlog wording.
  - `scripts/run_validation_container.sh` now serializes bounded validation
    runs with a repo-local lock.
  - `scripts/run_validation_status_summary.sh` now treats missing required
    `OMNI_TEST_SUMMARY` telemetry as a validation failure.
  - `scripts/c3c_limits.sh` now preserves quoted extra Docker args.
  - `scripts/run_e2e.sh` and `scripts/check_e2e_baseline_policy.sh` now guard
    Stage 3 compile-source parity explicitly.
- validation status:
  - `scripts/check_status_consistency.sh`: green
  - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: green
  - shell syntax checks across all touched validation scripts: green

## Execution Order

1. Land WS4 and the fail-closed portion of WS1 first.
2. Run bounded memory-lifetime validation and status/tooling checks.
3. Land WS2 safety fixes next.
4. Land WS3 parity and allocation hardening after the boundary/runtime surface
   is stable.

## Validation Baseline

- `scripts/check_status_consistency.sh`
- `c3c build`
- `c3c build --sanitize=address` for memory-sensitive lanes
- `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
- targeted async/compiler slices for WS2/WS3
- `scripts/run_e2e.sh` or `OMNI_E2E_COMPILE_ONLY=1 scripts/run_e2e.sh` where
  appropriate

## Completion

- WS1 shipped and validated through bounded `memory-lifetime-smoke`,
  including the shared-wrapper defensive clone policy and the finalize-lane
  detached-env UAF test fix that surfaced during integration validation.
- WS2 shipped and validated, including the CRUD concurrency follow-up closure.
- WS3 shipped and validated.
- WS4 shipped and validated.
- `TODO.md` actionable count returns to `0`.
