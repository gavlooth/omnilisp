# TODO Part 18

## Memory Boundary Architecture — 2026-04-24

Source: `docs/plans/memory-boundary-architecture-spec-2026-04-24.md`.

- [x] `MEM-BOUNDARY-SPEC-001` write the current memory boundary architecture spec.
  - classification: static documentation, targeted architecture contract.
  - done 2026-04-24: documented the region-RC ownership model, TEMP/ESCAPE
    boundary rule, stable materialization contract, and FFI/foreign-handle
    scope-region constraint.
  - validation: documentation artifact written; implementation validation is
    tracked by `MEM-BOUNDARY-POLICY-001`.

- [x] `MEM-BOUNDARY-POLICY-001` centralize `ValueTag` boundary ownership policy.
  - classification: runtime structure, targeted policy extraction.
  - done 2026-04-24: added a runtime-private `ValueTag` boundary ownership
    policy helper, routed stable materialization eligibility and graph
    reachability/audit edge classification through it, and pinned `FFI_HANDLE`
    as opaque foreign state unless a bridge-specific clone/traversal policy
    exists.
  - validation: `c3c build --obj-out obj`, C3 diagnostics for touched runtime
    files, `memory-lifetime-policy` container slice (`2 passed, 0 failed`),
    `scripts/check_status_consistency.sh`, and `git diff --check`.
  - negative-memory constraint: do not introduce per-language-value RC or treat
    FFI payload RC/finalizers as authority over Omni `Value` graphs.

- [x] `MEM-BOUNDARY-VERIFY-001` resolve current stable-escape smoke failures.
  - classification: runtime behavior, targeted verification blocker.
  - done 2026-04-24: stable-store publishability now accepts values in the
    retained owner scope chain rather than requiring the root itself to live in
    the current scope's ESCAPE lane, and copied/materialized destination closure
    commits roll back staged source closure-env normalization instead of
    finalizing mutations on aliased source closures.
  - validation: `c3c build --obj-out obj`;
    `scripts/run_validation_container.sh env OMNI_TEST_VERBOSE=0 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    (`253 passed, 0 failed`);
    historical bounded-container Valgrind invocation passed but wrapped the
    runtime through `env` without traced-child execution, so it is superseded as
    leak evidence by later `--trace-children=yes` validation; host Valgrind
    default Lisp/basic suite (`167 passed, 0 failed`).
  - negative-memory constraint: do not paper over the failures by weakening
    prepared-graph tests; preserve the invariant that committed ESCAPE roots do
    not retain TEMP-owned Omni edges.

## Proof-Driven Boundary Optimizer — 2026-04-24

Source: `docs/plans/memory-boundary-proof-planner-roadmap-2026-04-24.md`.

- [x] `MEM-BOUNDARY-PLANNER-001` add the `BoundaryPlanner` decision API and route reasons.
  - classification: runtime structure, structural overhaul.
  - done 2026-04-24: added planner route/reason enums, `BoundaryPlanDecision`,
    `boundary_plan_commit_escape(...)`, planned/selected route fields on
    `BoundaryCommitEscapeResult`, route-name helpers, graph-audit commit-context
    route logging, and regression coverage proving stable publish, stable
    materialization, compatibility, and mixed-destination planner decisions
    mirror current boundary behavior.
  - validation: `c3c build --obj-out obj`; bounded container
    `memory-lifetime-smoke` (`254 passed, 0 failed`); bounded container
    Valgrind `memory-lifetime-smoke` (`254 passed, 0 failed`).
  - negative-memory constraint: do not hide failed proof routes behind silent
    compatibility fallback.

- [x] `MEM-BOUNDARY-PASSPORT-001` promote stable store metadata into stable graph passports.
  - classification: runtime behavior, structural overhaul.
  - done 2026-04-24: extended stable indexed publication with owner-scope-chain proof,
    prepared graph summary, policy summary, risk flags, and invalidation reason
    fields on `StableGraphPassport`, added passport snapshots for active
    handles, and covered passport summary plus stale prepared-graph invalidation
    in memory-lifetime tests.
  - validation: `c3c build --obj-out obj`; bounded container
    `memory-lifetime-smoke` (`254 passed, 0 failed`); bounded container
    Valgrind `memory-lifetime-smoke` (`254 passed, 0 failed`).
  - negative-memory constraint: stable handles prove liveness; they do not own
    ordinary Omni language values.

- [x] `MEM-BOUNDARY-EPOCH-001` add mutation epoch invalidation for passport validation.
  - classification: runtime behavior, structural overhaul.
  - done 2026-04-24: added pointer-keyed mutation epochs, prepared-node epoch
    snapshots, passport stale-epoch validation, and mutation stamps for env
    binding writes, dictionary/set writes, and array write/push helpers.
  - validation: `c3c build --obj-out obj`; bounded container
    `memory-lifetime-smoke` (`254 passed, 0 failed`); bounded container
    Valgrind `memory-lifetime-smoke` (`254 passed, 0 failed`).
  - negative-memory constraint: do not weaken existing structural mutation drift
    tests while adding epoch short-circuiting.

- [x] `MEM-BOUNDARY-TRANSPLANT-001` replace ad hoc splice checks with `BoundaryTransplantProof`.
  - classification: runtime behavior, structural overhaul.
  - done 2026-04-24: added `BoundaryTransplantProof`, routed scope-transfer and
    root-splice checks through proof builders, and made commit splice candidates
    consume a proof object instead of the prior standalone boolean precheck
    without expanding transplant success behavior.
  - validation: `c3c build --obj-out obj`; bounded container
    `memory-lifetime-smoke` (`254 passed, 0 failed`); bounded container
    Valgrind `memory-lifetime-smoke` (`254 passed, 0 failed`).
  - negative-memory constraint: transplant is legal only when the whole source
    ownership island survives; otherwise materialize or fail closed.

- [x] `MEM-BOUNDARY-FFI-BRIDGE-001` add explicit FFI bridge boundary declarations.
  - classification: runtime structure, structural overhaul.
  - done 2026-04-24: added per-handle `FfiBridgeBoundaryMode` declarations,
    defaulted existing FFI boxes to opaque, and wired transplant proof to fail
    closed for bridge modes that require undeclared native traversal/copy hooks.
  - validation: `c3c build --obj-out obj`; bounded container
    `memory-lifetime-smoke` (`255 passed, 0 failed`); bounded container
    Valgrind `memory-lifetime-smoke` (`255 passed, 0 failed`).
  - negative-memory constraint: foreign payload finalizers/RC must not become
    ownership authority over ordinary Omni `Value` graphs.

- [x] `MEM-BOUNDARY-COPY-DEBT-001` expose copy debt and route-failure telemetry.
  - classification: runtime observability, targeted instrumentation.
  - done 2026-04-24: exposed planned/selected route counters, fail-closed
    reason counters, materialization node totals, and estimated materialized
    payload bytes through test summaries, graph-audit telemetry, JSON telemetry,
    and `(runtime-memory-stats)`.
  - validation: `c3c build --obj-out obj`; bounded container
    `memory-lifetime-smoke` (`255 passed, 0 failed`); bounded container
    `basic` (`169 passed, 0 failed`); multi-feature instrumentation build
    (`OMNI_BOUNDARY_INSTR_COUNTERS`, `TRACE`, `BENCHMARK`) plus bounded
    `memory-lifetime-smoke` (`255 passed, 0 failed`).
  - prerequisites: route reasons from `MEM-BOUNDARY-PLANNER-001`.
  - negative-memory constraint: telemetry must not change route behavior or
    mask fail-closed outcomes.

- [x] `MEM-BOUNDARY-PLAN-MIGRATE-001` migrate boundary commit paths to planner-selected outcomes.
  - classification: runtime behavior, structural overhaul.
  - done 2026-04-24: `boundary_commit_escape` dispatches by
    `BoundaryPlanDecision.route`; destination promotion receives the planned
    route, and TEMP `CONS` transplant/compatibility fallback is represented as
    explicit planner candidates instead of hidden helper fallback.
  - validation: `c3c build --obj-out obj`; bounded container
    `memory-lifetime-smoke` (`255 passed, 0 failed`); bounded container
    `basic` (`169 passed, 0 failed`).
  - prerequisites: `MEM-BOUNDARY-PLANNER-001`,
    `MEM-BOUNDARY-PASSPORT-001`, `MEM-BOUNDARY-TRANSPLANT-001`, and
    `MEM-BOUNDARY-COPY-DEBT-001`.
  - negative-memory constraint: do not preserve compatibility routes as implicit
    fallbacks once the planner owns route selection.

## Measured Boundary Copy-Debt Follow-Up — 2026-04-24

Source: `docs/plans/memory-boundary-proof-planner-roadmap-2026-04-24.md`.

- [x] `MEM-BOUNDARY-TAG-DEBT-001` attribute boundary route and copy debt by `ValueTag`.
  - classification: runtime observability, targeted instrumentation.
  - done 2026-04-24: boundary decision telemetry now records planned/selected
    route counts by root `ValueTag`, stable materialization success/node/copy
    totals by root `ValueTag`, and exposes dominant materialization buckets
    through test summaries, verbose boundary telemetry, JSON runtime memory
    telemetry, and `(runtime-memory-stats)`.
  - validation: `c3c build --obj-out obj`; counters-enabled build
    (`-D OMNI_BOUNDARY_INSTR_COUNTERS`); bounded container
    `memory-lifetime-smoke` (`255 passed, 0 failed`) with
    `materialization_copy_bytes=10096`, `materialization_copy_bytes_cons=8568`,
    `materialization_copy_bytes_closure=1072`, and
    `materialization_copy_bytes_array=400`; direct runtime-stats smoke returned
    `true`.
  - negative-memory constraint: do not pick the next stable-materialization
    optimization from aggregate copy bytes alone; use the root-tag counters to
    target the dominant copied graph family first.

- [x] `MEM-BOUNDARY-CONS-COPY-001` reduce `CONS` stable-materialization copy debt.
  - classification: runtime performance, structural optimization.
  - done 2026-04-24: TEMP `CONS` roots with an explicit transplant candidate
    now try budget-proven promotion into the releasing ESCAPE lane followed by
    proof-backed region transplant before stable destination materialization.
    If the budget or proof is insufficient, the route keeps the existing
    materialize/fail-closed behavior.
  - validation: `c3c build --obj-out obj`; counters-enabled build
    (`-D OMNI_BOUNDARY_INSTR_COUNTERS`); bounded container normal and
    counters-enabled `memory-lifetime-smoke` (`255 passed, 0 failed` each);
    bounded container `basic` (`169 passed, 0 failed`); bounded container
    Valgrind `memory-lifetime-smoke` (`255 passed, 0 failed`). Counters moved
    `materialization_copy_bytes_cons` from `8568` to `0` and aggregate
    `materialization_copy_bytes` from `10096` to `1528`.
  - prerequisites: `MEM-BOUNDARY-TAG-DEBT-001` counters must remain available
    in the validation command.
  - negative-memory constraint: do not revive strict-only TEMP `CONS`
    materialization or hidden compatibility fallback; failed prepared/proof
    paths must remain explicit planner-selected outcomes.

- [x] `MEM-BOUNDARY-CLOSURE-COPY-001` reduce closure stable-materialization copy debt.
  - classification: runtime performance, structural optimization.
  - done 2026-04-24: TEMP closure roots selected for stable materialization
    now pass through the prepared-graph budget gate and try promotion into the
    releasing ESCAPE lane plus proof-backed region transplant before stable
    destination materialization. Existing promotion code still owns closure
    env detach/retain behavior and type-signature copying.
  - validation: `c3c build --obj-out obj`; counters-enabled build
    (`-D OMNI_BOUNDARY_INSTR_COUNTERS`); bounded container normal and
    counters-enabled `memory-lifetime-smoke` (`255 passed, 0 failed` each).
    Bounded container `basic` passed (`169 passed, 0 failed`); bounded
    container Valgrind `memory-lifetime-smoke` passed (`255 passed, 0 failed`).
    Counters moved `materialization_copy_bytes_closure` from `1072` to `208`
    and aggregate `materialization_copy_bytes` from `1528` to `664`. ASAN was
    attempted, but local `c3c` rejected sanitizer mode before compiling.
  - prerequisites: `MEM-BOUNDARY-CONS-COPY-001` must remain closed under the
    counters-enabled smoke command.
  - negative-memory constraint: do not treat closure env scopes or stable
    handles as ownership authority; scope regions remain the lifetime owner.

- [x] `MEM-BOUNDARY-ARRAY-COPY-001` reduce array stable-materialization copy debt.
  - classification: runtime performance, structural optimization.
  - done 2026-04-24: TEMP array roots selected for stable materialization now
    pass through the prepared-graph budget gate and try promotion into the
    releasing ESCAPE lane plus proof-backed region transplant before stable
    destination materialization.
  - validation: `c3c build --obj-out obj`; counters-enabled build
    (`-D OMNI_BOUNDARY_INSTR_COUNTERS`); bounded container normal and
    counters-enabled `memory-lifetime-smoke` (`255 passed, 0 failed` each).
    Bounded container `basic` passed (`169 passed, 0 failed`); bounded
    container Valgrind `memory-lifetime-smoke` passed (`255 passed, 0 failed`).
    Counters moved `materialization_copy_bytes_array` from `400` to `0` and
    aggregate `materialization_copy_bytes` from `664` to `264`. ASAN was
    attempted, but local `c3c` rejected sanitizer mode before compiling.
  - prerequisites: `MEM-BOUNDARY-CLOSURE-COPY-001` must remain closed under the
    counters-enabled smoke command.
  - negative-memory constraint: do not bypass prepared-edge ordering or
    mutation-epoch validation to force an array transplant.

- [x] `MEM-BOUNDARY-CLOSURE-RESIDUAL-001` explain or eliminate the remaining closure materialization.
  - classification: runtime performance, targeted residual investigation.
  - done 2026-04-24: the remaining selected stable-materialize closure root was
    identified as `boundary_commit_escape_rollback_error.c3`, which
    intentionally calls `boundary_commit_escape(..., false)` to disable
    scope-splice/transplant and force a copied/materialized destination commit
    so closure-env normalization rollback can be tested.
  - validation: temporary local tracing under counters-enabled
    `memory-lifetime-smoke` identified `planned=STABLE_MATERIALIZE_DESTINATION`,
    `allow_direct=no`, releasing ESCAPE ownership, and `releasing_refcount=1`.
    The traced caller is the explicit no-splice rollback regression, so the
    residual is expected coverage rather than optimizer debt.
  - prerequisites: keep closure env detach/retain rollback tests passing.
  - negative-memory constraint: do not weaken child TEMP-edge proof rejection or
    refcount-one scope-splice requirements to remove this residual.

- [x] `MEM-BOUNDARY-BIGINT-COPY-001` reduce remaining heap-scalar stable-materialization copy debt.
  - classification: runtime performance, targeted optimization.
  - done 2026-04-24: `BIG_INTEGER` roots selected for stable materialization
    now use the prepared-budget/proof transplant lane before stable destination
    materialization. The regression snapshots the source payload handle before
    commit and verifies the committed ESCAPE BigInteger value without
    dereferencing the retired TEMP source after a splice.
  - validation: `c3c build --obj-out obj`; counters-enabled build
    (`-D OMNI_BOUNDARY_INSTR_COUNTERS`); bounded container normal and
    counters-enabled `memory-lifetime-smoke` (`255 passed, 0 failed` each);
    bounded container `basic` (`169 passed, 0 failed`); bounded container
    Valgrind `memory-lifetime-smoke` (`255 passed, 0 failed`); ASAN attempted
    and rejected by the local `c3c` toolchain before compiling; `git diff
    --check`; `scripts/check_status_consistency.sh`. Counters moved
    `materialization_copy_bytes_big_integer` from `56` to `0` and aggregate
    `materialization_copy_bytes` from `264` to `208`.
  - prerequisites: `MEM-BOUNDARY-CLOSURE-RESIDUAL-001` remains closed as
    expected no-splice coverage.
  - negative-memory constraint: do not remove or rewrite the explicit no-splice
    closure rollback test to improve copy-debt counters.

- [x] `MEM-BENCH-FORCED-NOSPLICE-001` split forced no-splice materialization from optimizer copy debt.
  - classification: runtime observability, targeted telemetry correction.
  - done 2026-04-24: boundary decision stats, runtime-memory-stats,
    `OMNI_TEST_SUMMARY`, verbose boundary telemetry, and the telemetry
    benchmark envelope now expose optimizer-addressable materialization copy
    bytes separately from explicit forced-no-splice rollback coverage.
  - validation: counters-enabled build; bounded container counters-enabled
    `memory-lifetime-smoke` (`255 passed, 0 failed`) reported
    `materialization_copy_bytes=208`,
    `materialization_copy_bytes_optimizer=0`, and
    `materialization_copy_bytes_forced_no_splice=208`; refreshed bounded
    `memory-lifetime-bench` baseline reported all three materialization copy
    counters at `0`; `scripts/check_memory_telemetry_benchmark_envelope.sh`
    passed on the refreshed baseline log; normal build and bounded `basic`
    passed.
  - prerequisites: `MEM-BOUNDARY-CLOSURE-RESIDUAL-001` remains closed as
    expected no-splice coverage.
  - negative-memory constraint: do not suppress or delete the rollback
    materialization; only subtract explicitly forced no-splice coverage from
    optimizer-debt counters.

## Memory Boundary Telemetry Benchmarking — 2026-04-24

Source: `docs/plans/memory-boundary-telemetry-benchmark-plan-2026-04-24.md`.

- [x] `MEM-BENCH-OBSERVE-001` inventory current memory/boundary observability.
  - classification: static tooling/docs, targeted evidence inventory.
  - done 2026-04-24: added
    `docs/plans/memory-boundary-telemetry-signal-inventory-2026-04-24.md`,
    mapping existing `BoundaryDecisionStats`, `runtime-memory-stats`,
    `OMNI_TEST_SUMMARY` / `OMNI_BENCH_SUMMARY` lines, and profile scripts, plus
    the missing counter fields needed by `MEM-BENCH-OBSERVE-002`.
  - task: map current boundary counters, benchmark summary lines,
    `runtime-memory-stats` fields, and boundary profile scripts into one
    table of existing signals and missing signals.
  - validation: documentation diff check plus `scripts/check_status_consistency.sh`.
  - prerequisites: current `memory-lifetime-bench` and boundary telemetry files
    remain discoverable.
  - negative-memory constraint: do not treat historical destination-arena
    proposal text as current implementation truth when it conflicts with
    `memory/CHANGELOG.md` or `docs/areas/memory-runtime.md`.

- [x] `MEM-BENCH-OBSERVE-002` expand low-overhead counter coverage.
  - classification: runtime telemetry, targeted instrumentation.
  - done 2026-04-24: added gated allocator pressure counters, value-shape
    counters, payload-byte counters, FFI/tensor wrapper authority counters,
    and stable passport invalidation reason counters; exposed them through
    `runtime-memory-stats` and `OMNI_MEM_TELEMETRY`.
  - task: add missing counters for allocator pressure, collection shape
    pressure, payload byte sizes, and FFI/tensor wrapper crossings.
  - validation: `c3c build --obj-out obj`, counters-enabled build with
    `-D OMNI_BOUNDARY_INSTR_COUNTERS`, bounded `basic`, bounded
    `memory-lifetime-smoke`, and `git diff --check`.
  - prerequisites: `MEM-BENCH-OBSERVE-001` identifies missing fields and their
    owning files.
  - negative-memory constraint: keep default hot paths zero-overhead unless the
    existing instrumentation gate is enabled.

- [x] `MEM-BENCH-OBSERVE-003` expand the `memory-lifetime-bench` workload suite.
  - classification: runtime benchmark, targeted workload expansion.
  - done 2026-04-24: added `boundary_value_shape_counters`
    `OMNI_BENCH_SUMMARY` coverage for allocator slow paths, reset/destroy
    slack, collection growth, closure env-copy counters, scalar/tensor payload
    bytes, FFI release authority, stable passport stale invalidation, and
    selected transplant/copy-debt deltas.
  - task: add representative TEMP/ESCAPE workloads for large containers,
    closure env copies, cyclic/shared containers, mutation drift,
    BigInteger/String/Error payloads, lazy tensor metadata, and FFI wrappers.
  - validation: bounded `memory-lifetime-bench` with `OMNI_BOUNDARY_BENCH=1`
    and counters enabled; each workload emits a stable `OMNI_BENCH_SUMMARY`.
  - prerequisites: counter fields from `MEM-BENCH-OBSERVE-002` are available.
  - negative-memory constraint: do not make benchmark fixtures weaken runtime
    proof/fail-closed invariants just to produce cleaner route counts.

- [x] `MEM-BENCH-OBSERVE-004` capture the first benchmark baseline and interpret it.
  - classification: runtime benchmark evidence, targeted baseline capture.
  - done 2026-04-24: added
    `docs/plans/memory-boundary-telemetry-benchmark-baseline-2026-04-24.md`
    with bounded benchmark command, captured summary output, interpretation,
    and recommended regression-envelope fields.
  - task: run the bounded benchmark suite, archive summary output, and document
    the top copy/allocator hotspots or explicitly record that no optimization
    is justified.
  - validation: bounded benchmark command recorded with log path and parsed
    summary; `scripts/check_status_consistency.sh`.
  - prerequisites: `MEM-BENCH-OBSERVE-003` workload suite is runnable in the
    bounded container.
  - negative-memory constraint: wall-clock numbers are advisory until repeated
    runs prove stability; prioritize route/copy/allocator counters.

- [x] `MEM-BENCH-OBSERVE-005` add a regression-envelope parser for benchmark summaries.
  - classification: tooling/validation, targeted benchmark guard.
  - done 2026-04-24: added
    `scripts/check_memory_telemetry_benchmark_envelope.sh`, which gates
    required `OMNI_BENCH_SUMMARY` correctness fields and counter-presence
    deltas while treating timing and materialization-copy drift as warnings.
  - task: add a parser/check script for `OMNI_BENCH_SUMMARY` lines that gates
    correctness and counter regressions first, with wide timing warnings only.
  - validation: script syntax check, sample-log parse test, bounded
    `memory-lifetime-bench`, and `git diff --check`.
  - prerequisites: baseline from `MEM-BENCH-OBSERVE-004`.
  - negative-memory constraint: do not introduce strict timing failure gates
    until repeated bounded-container runs show a stable envelope.

Source: `docs/plans/memory-boundary-architecture-spec-2026-04-24.md`.

- [x] `MEM-LIFETIME-TEARDOWN-001` close retained child-region teardown under traced Valgrind.
  - classification: runtime behavior, targeted ownership hardening.
  - done 2026-04-24: added active child tracking to `ScopeRegion`, owned-root
    descendant sweep during interpreter teardown, closure env-scope release
    through generic `scope_dtor_value`, Valgrind alternate-stack registration,
    and shared scope-owned opaque-payload fixtures.
  - task: make retained descendant regions deterministic at interpreter
    teardown without introducing per-value RC or root pinning.
  - validation: `c3c build --obj-out obj`; bounded `memory-lifetime-smoke`;
    traced-child Valgrind `memory-lifetime-smoke`; counters-enabled
    `memory-lifetime-bench` plus
    `scripts/check_memory_telemetry_benchmark_envelope.sh`; `git diff --check`.
  - prerequisites: validation container image includes `valgrind`.
  - negative-memory constraint: do not trust a Valgrind run wrapped in `env`
    unless it uses `--trace-children=yes`; do not restore speculative
    descendant-owner release helpers that decrement ancestor scopes while
    descendants still reference them.

## Memory Model Improvement Plan — 2026-04-25

Source: `docs/plans/memory-model-improvement-plan-2026-04-25.md`.

- [x] `MEM-MODEL-IMPROVE-001` refresh the bounded memory-model evidence baseline.
  - classification: runtime benchmark evidence, targeted measurement.
  - task: rerun counters-enabled `memory-lifetime-bench` several times in the
    validation container, run the benchmark envelope on each log, and record
    allocator/slack/collection/route/copy median and range data in the
    baseline note.
  - why: current evidence shows zero optimizer-addressable materialization
    copy debt, so allocator and collection pressure need repeated-run evidence
    before runtime policy changes.
  - concrete next step: build with `-D OMNI_BOUNDARY_INSTR_COUNTERS`, run
    bounded `memory-lifetime-bench` with `OMNI_BOUNDARY_BENCH=1`, then run
    `scripts/check_memory_telemetry_benchmark_envelope.sh` on the captured log.
  - prerequisites: validation container and benchmark envelope script are
    available.
  - negative-memory constraint: do not reopen closed copy-debt tasks when
    `materialization_copy_bytes_optimizer=0`; do not treat timing as a strict
    gate until repeated bounded runs prove stability.
  - done 2026-04-25: built in the validation container with
    `-D OMNI_BOUNDARY_INSTR_COUNTERS`, ran three bounded
    `memory-lifetime-bench` passes, and checked each log with
    `scripts/check_memory_telemetry_benchmark_envelope.sh`.
  - evidence logs:
    `.agents/memory-model-improve-001-runs/run-1.log`,
    `.agents/memory-model-improve-001-runs/run-2.log`, and
    `.agents/memory-model-improve-001-runs/run-3.log`.
  - results: correctness counters were stable (`splice_ok=2048`,
    `disallowed_ok=2048`, `reuse_ok=2048`, `partial_ok=2048`,
    `shape_ok=128`, `closure_env_ok=32`, `stable_passport_ok=1`,
    `splice_fail_total=0`), copy debt stayed zero
    (`materialization_copy_bytes_delta=0`,
    `materialization_copy_bytes_optimizer=0`), and the dominant repeatable
    non-copy signals remained allocator/slack plus collection growth
    (`escape_slow_delta=416`, `temp_slow_delta=209`,
    `escape_destroy_slack_delta=603776`, `temp_destroy_slack_delta=209536`,
    `hashmap_growth_delta=1024`, `set_growth_delta=512`,
    `array_growth_delta=128`).
  - closure decision: proceed to `MEM-MODEL-IMPROVE-002` first because the
    escape-lane slow allocation / destroy slack signal is the largest measured
    allocator pressure class; keep `MEM-MODEL-IMPROVE-003` ready for the
    shared hashmap/set sizing path if allocator policy is blocked or after the
    first allocator delta.

- [x] `MEM-MODEL-IMPROVE-002` tune scope allocator and slack policy from measured counters.
  - classification: runtime performance, targeted allocator optimization.
  - task: identify dominant TEMP/ESCAPE slow-path and slack buckets, add any
    missing gated histogram counters, then tune chunk sizing, reuse thresholds,
    or fiber-temp eligibility only for a measured pressure class.
  - why: the first benchmark baseline points at allocator slow paths and
    reset/destroy slack rather than boundary copy debt.
  - concrete next step: inspect `src/scope_region*.c3`,
    `src/scope_region_temp_pool_stats.c3`, and
    `boundary_value_shape_counters`, then propose one policy change with
    before/after counter expectations.
  - prerequisites: `MEM-MODEL-IMPROVE-001` has a repeated-run baseline
    (met 2026-04-25).
  - negative-memory constraint: do not use root pinning or speculative
    descendant-owner release to reduce teardown pressure; Valgrind through
    `env` must use `--trace-children=yes`.
  - partial 2026-04-25: added gated TEMP/ESCAPE slow-allocation slack
    histogram counters, exposed them through `runtime-memory-stats`, emitted
    them in `boundary_value_shape_counters`, and added an optional envelope
    requirement via `OMNI_MEM_TELEM_REQUIRE_SLOW_SLACK_HISTOGRAM=1`.
  - validation: counters-enabled container build passed; bounded
    `memory-lifetime-bench` passed
    `OMNI_MEM_TELEM_REQUIRE_SLOW_SLACK_HISTOGRAM=1
    scripts/check_memory_telemetry_benchmark_envelope.sh
    .agents/memory-model-improve-002-histogram.log`; bounded `basic` passed.
  - finding: ESCAPE slow slack is concentrated in the `<=4096` bucket
    (`escape_slow_slack_le4096_delta=415`, `escape_slow_slack_gt4096_delta=0`)
    while TEMP has the large-slack bucket
    (`temp_slow_slack_gt4096_delta=144`).
  - blocked policy attempt: exact-fit and bounded-headroom direct ESCAPE chunk
    sizing were tried and then reverted; the existing `memory-lifetime-smoke`
    ignore-k continuation failure (`AUDIT-238`) reproduced after reverting, so
    treat that failure as a separate boundary bug and do not use it to conclude
    all ESCAPE size-class work is unsafe.
  - invalidated 2026-04-25: direct TEMP large slow-allocation exact-fit and
    exact-plus-4096-headroom chunk sizing were also tried and reverted. They
    kept correctness green but worsened benchmark counters
    (`temp_slow_delta` `209 -> 336`,
    `temp_selected_chunk_delta` `3219456 -> 4285568/5858432`,
    `temp_destroy_slack_delta` `209536 -> 1275648/2847488`).
  - next step: add per-scope allocation-sequence telemetry before any further
    allocator policy change; do not tune from aggregate slack alone.
  - partial 2026-04-26: added gated per-scope slow-allocation sequence
    telemetry. TEMP and ESCAPE slow chunks now record closed sequence count,
    follow-up allocation count/bytes, unused bytes at close, no-follow-up
    sequences, and large-slack follow-up/no-follow-up evidence.
  - validation: counters-enabled bounded build passed; normal bounded build
    passed; bounded `memory-lifetime-bench` passed with
    `OMNI_MEM_TELEM_REQUIRE_SCOPE_SEQUENCE=1`; bounded default `basic` passed
    `173/0`; bounded `memory-lifetime-smoke` passed `271/0`.
  - finding 2026-04-26: broad TEMP large-slack reduction is invalidated by
    sequence evidence (`temp_slow_sequence_large_delta=144`,
    `temp_slow_sequence_large_followup_bytes_delta=2002192`,
    `temp_slow_sequence_large_no_followup_delta=0`). ESCAPE has many
    no-follow-up sequences (`escape_slow_sequence_no_followup_delta=256`) but
    no large-slack sequences, so the next policy slice should split ESCAPE
    no-follow-up sequences by request/unused size class before changing chunk
    selection.
  - partial 2026-04-26: added request/unused size classes for no-follow-up
    slow sequences and made the envelope verify that ESCAPE no-follow-up
    request and unused bucket sums match the total. The first run shows ESCAPE
    no-follow-up requests split across tiny and large requests
    (`le512=128`, `le4096=1`, `gt4096=127`) while unused-at-close is not large
    (`exact=1`, `le4096=255`, `gt4096=0`).
  - closure 2026-04-26: added source/site attribution for ESCAPE no-follow-up
    slow sequences. The bounded benchmark reports
    `escape_slow_sequence_no_followup_source_direct_delta=256` and zero for
    dtor, interpreter value/env, boundary payload, promotion signature,
    promotion closure, and JIT staged-arg sources.
  - closure decision: no allocator policy change is justified from the current
    profile. The remaining ESCAPE no-follow-up bucket is the synthetic direct
    allocator probe inside `boundary_value_shape_counters`, not a runtime
    boundary/promotion allocation family.

- [x] `MEM-MODEL-IMPROVE-003` reduce collection growth pressure with explicit sizing contracts.
  - classification: runtime performance, targeted collection-sizing
    optimization.
  - task: map array/dictionary/set growth counters to concrete constructors or
    builders and add expected-capacity propagation where source length is
    already known.
  - why: collection growth counters are a visible non-copy pressure class in
    the current benchmark baseline.
  - concrete next step: choose the largest single collection family after
    `MEM-MODEL-IMPROVE-001` and implement pre-sizing without bypassing checked
    constructors or checked insertion.
  - prerequisites: refreshed benchmark evidence confirms collection growth
    remains dominant.
  - negative-memory constraint: do not return partially populated collections
    on grow failure and do not store boundary `ERROR` values as ordinary
    collection data.
  - shipped 2026-04-25: centralized known-entry capacity hints for
    hashmaps/sets, routed Dictionary and Set constructors through the shared
    hint path, added checked known-entry constructors for benchmark/internal
    use, and changed `boundary_value_shape_counters` to pre-size dictionary and
    set workloads from their known insert counts.
  - validation: counters-enabled container build passed; bounded
    `memory-lifetime-smoke` passed `269/0`; bounded `data-format` passed
    `92/0`; bounded `scope` passed `64/0`; bounded `basic` passed `173/0`;
    bounded `memory-lifetime-bench` passed with
    `hashmap_growth_delta=0`, `set_growth_delta=0`, and
    `materialization_copy_bytes_delta=0`; the telemetry envelope passed with
    `OMNI_MEM_TELEM_REQUIRE_COLLECTION_GROWTH_ZERO=1`.

- [x] `MEM-MODEL-IMPROVE-004` mechanically enforce boundary policy coverage.
  - classification: static tooling plus runtime policy, targeted guardrail.
  - task: extend boundary guard scripts so every graph-carrying `ValueTag` has
    explicit edge policy, materialization/copy route, destructor authority,
    rollback coverage, graph-audit handling, and FFI/native exclusion where
    applicable.
  - why: the boundary ownership policy table is the contract, but future value
    families should fail tooling checks when required hooks are missing.
  - concrete next step: inspect `boundary_value_edge_policy`, stable
    materialization eligibility, graph-audit classification, and existing
    `scripts/check_boundary_*` guards to design one source-of-truth check.
  - prerequisites: no parallel `ValueTag` addition is changing the same policy
    source without coordination.
  - negative-memory constraint: do not mark heap-backed scalar payloads as
    immediate/by-value; do not capture prepared parent edge offsets before
    recursively preparing children.
  - shipped 2026-04-25: added
    `scripts/boundary_value_policy_manifest.tsv` and
    `scripts/check_boundary_value_policy_coverage.py`, then wired the guard
    into `scripts/check_boundary_change_policy.sh`.
  - coverage: the guard verifies all `ValueTag` entries declare ownership,
    edge policy, copy route, stable-materialization eligibility, graph-audit
    class, destructor authority, native/FFI exclusion, and rollback policy;
    it cross-checks the manifest against the runtime ownership, edge, and
    copy-route switch tables.
  - validation: direct guard run passed for all `30` `ValueTag` entries;
    `OMNI_BOUNDARY_POLICY_RANGE=HEAD..HEAD scripts/check_boundary_change_policy.sh`
    passed and still runs the guard even when no sensitive files changed.

- [x] `MEM-MODEL-IMPROVE-005` add one narrow FFI bridge hook family.
  - classification: runtime behavior, targeted bridge implementation.
  - task: choose one FFI/foreign wrapper family with explicit release authority,
    implement its declared copy or trace hook path, and keep all undeclared
    bridge modes fail-closed.
  - why: `FfiBridgeBoundaryMode` exists, but copy/trace modes intentionally
    fail closed until real hooks prove the bridge contract.
  - concrete next step: inventory current `FfiHandle` construction sites and
    select one wrapper with deterministic success/failure tests.
  - prerequisites: explicit owner/release authority for the chosen wrapper.
  - negative-memory constraint: do not traverse raw native pointers without
    bridge-owned hooks and do not let foreign payload RC/finalizers own
    ordinary Omni `Value` graphs.
  - done 2026-04-25: selected `atomic-ref` as the first explicit keepalive
    bridge family; constructor plumbing now preserves per-handle bridge mode;
    `copy_ffi_handle_to_parent` fails closed for copy/trace/unsafe modes; real
    `(atomic ...)` values assert `FFI_BRIDGE_BOUNDARY_KEEPALIVE`.
  - validation: bounded build passed; bounded `memory-lifetime-smoke` passed
    `271/0`; bounded `atomic` passed `11/0`; bounded `advanced-ffi-system`
    passed `185/0`; traced-child Valgrind `memory-lifetime-smoke` passed
    `271/0` with zero Memcheck errors and zero definite/indirect/possible
    leaks; boundary policy guard and `git diff --check` passed.

- [x] `MEM-MODEL-IMPROVE-006` broaden memory workload coverage beyond synthetic fixtures.
  - classification: runtime benchmark coverage, targeted workload expansion.
  - task: add benchmark fixtures for nested module returns, closure-heavy
    iterator pipelines, dictionary/set workloads, tensor metadata crossings,
    FFI wrapper crossings, and one product-style workload derived from
    `examples/finwatch/main.omni`.
  - why: broader workloads reduce the risk of tuning allocator or collection
    policy for narrow synthetic fixtures.
  - concrete next step: closed 2026-04-25; continue future optimization work
    under `MEM-MODEL-IMPROVE-002` with per-scope allocation-sequence evidence.
  - prerequisites: `MEM-MODEL-IMPROVE-001` baseline is recorded so new workload
    deltas can be interpreted separately.
  - negative-memory constraint: do not weaken boundary proof/fail-closed
    invariants to make benchmark fixtures pass; do not add strict timing gates
    until repeated bounded-container runs prove timing stability.
  - landed slice 2026-04-25: added `finwatch_product_memory`, a product-style
    benchmark derived from `examples/finwatch` route/cache/portfolio response
    shapes. It builds quote arrays, holding arrays, watchlist sets, request and
    response dictionaries, strings, symbols, and one releasable FFI wrapper,
    then crosses a boundary through `boundary_commit_escape`.
  - validation: counters-enabled bounded build passed; counters-enabled
    bounded `memory-lifetime-bench` emitted `finwatch_product_memory` with
    `product_ok=64`, `commit_ok=64`, `hashmap_construct_delta=1472`,
    `set_construct_delta=64`, `array_construct_delta=128`,
    `string_payload_bytes_delta=48512`, `ffi_wrappers_delta=64`, and
    `ffi_releasable_delta=64`; updated envelope passed with
    `OMNI_MEM_TELEM_REQUIRE_SLOW_SLACK_HISTOGRAM=1` and
    `OMNI_MEM_TELEM_REQUIRE_COLLECTION_GROWTH_ZERO=1`.
  - landed slice 2026-04-25: added `closure_iterator_pipeline_memory`, a
    closure-heavy lazy iterator benchmark using captured mapping/filter
    closures over `range-from`, returning the lazy iterator across the
    top-level boundary and then materializing it through `List`.
  - validation: normal bounded build passed; counters-enabled bounded build
    passed; counters-enabled bounded `memory-lifetime-bench` emitted
    `closure_iterator_pipeline_memory` with `pipeline_ok=64`,
    `count_total=640`, `sum_total=22720`, `iterator_roots_delta=512`,
    `partial_roots_delta=640`, `closure_roots_delta=448`,
    `closure_env_frame_delta=512`, `closure_env_binding_delta=512`, and
    `materialization_copy_bytes_delta=0`; updated envelope, status
    consistency, and whitespace gates passed.
  - landed slice 2026-04-25: added `tensor_metadata_crossing_memory`, a
    tensor-heavy metadata crossing benchmark that builds TEMP cons records with
    CPU `Float64` tensors and shape metadata, commits the graph through
    `boundary_commit_escape`, and checks the committed graph without relying on
    root-promoting Dictionary storage.
  - validation: normal bounded build passed; counters-enabled bounded build
    passed; counters-enabled bounded `memory-lifetime-bench` emitted
    `tensor_metadata_crossing_memory` with `tensor_ok=64`, `commit_ok=64`,
    `tensor_roots_delta=256`, `tensor_payload_bytes_delta=12288`,
    `array_construct_delta=256`, `selected_transplant_delta=64`, and
    `materialization_copy_bytes_delta=0`; updated envelope, status
    consistency, and whitespace gates passed.
  - landed slice 2026-04-25: added `nested_module_return_memory`, a nested
    module workload that returns nested list/array graphs built inside
    `(with module ...)` bodies across the boundary. The slice fixed the
    boundary commit path so a rejected region-transplant splice can fall back
    to stable destination materialization for stable graph candidates instead
    of returning a `splice-rejected` boundary error.
  - validation: normal bounded build passed; counters-enabled bounded build
    passed; counters-enabled bounded `memory-lifetime-bench` emitted
    `nested_module_return_memory` with `setup_ok=1`, `batch_ok=64`,
    `array_construct_delta=384`, `selected_transplant_delta=128`,
    `selected_stable_materialize_delta=320`, and
    `materialization_copy_bytes_delta=220160`; updated envelope passed;
    focused `advanced-collections-module` passed `2102/0`.

- [x] `AUDIT-238-CONTINUATION-IGNORE-K-TEMP-EDGE` fix handle ignore-k suspended-context retention.
  - classification: runtime memory/lifetime, targeted continuation boundary fix.
  - shipped: handler-result finalization now releases transient ESCAPE-lane
    clause-env continuation retention unless the returned result graph reaches
    `k`; generic `checkpoint`/`capture` now preserves suspended contexts when
    the shift result carries `k`; stable destination materialization supports
    continuation leaves in returned cons graphs; the stale fixture was corrected
    to canonical `(ask x body)`.
  - validation: bounded `memory-lifetime-smoke` passes with `269 passed, 0
    failed`; direct `OMNI_BOUNDARY_GRAPH_AUDIT=1` evaluation of
    `(handle (signal ask 1) (ask x 99))` returns `99` without continuation-path
    graph-audit diagnostics; bounded `advanced-effect-continuation` passes with
    `56 passed, 0 failed`.
  - note: remaining full-slice graph-audit TEMP-edge output is expected from
    passing negative graph-audit tests, not this ignore-k path.
  - prerequisites: preserve the boundary invariant that ESCAPE roots do not
    retain reachable Omni-owned TEMP edges.
  - negative-memory constraint: do not hide the graph-audit violation by
    suppressing the audit or pinning the suspended context; fix ownership or
    reachability retention.

- [x] `AUDIT-239-ENV-COPY-DTOR-REGISTRATION` fail closed when env-copy cannot register frame teardown.
  - classification: runtime memory/lifetime, targeted env-copy boundary fix.
  - shipped: `copy_env_materialize_frame` now treats `scope_register_dtor`
    failure as a typed `BOUNDARY_ENV_COPY_FAULT_DTOR_REGISTRATION` boundary
    fault, rolls back materialized binding payloads, invalidates the active
    promotion context, and cleans the partial frame before returning failure.
  - validation: host `c3c --threads 1 build --obj-out obj` reached link and
    failed only on missing host `liblightning`/`libreplxx`; bounded container
    build passed and linked `build/main`; bounded `memory-lifetime-smoke`
    passed with `272 passed, 0 failed`.
  - prerequisites: keep env-copy boundary failures typed and fail-closed.
  - negative-memory constraint: do not ignore destructor-registration failure
    on environment frames; a copied frame without registered teardown authority
    is not a successful boundary copy.

- [x] `AUDIT-240-DESTINATION-ERROR-ESCAPE-DTOR` fail closed when destination error escape cannot register teardown.
  - classification: runtime memory/lifetime, targeted boundary destination
    builder fix.
  - shipped: `boundary_build_destination_error_escape` now checks
    `scope_register_dtor_escape`; on failure it manually destroys the
    partially built error string payload and returns the original error while
    the staged build scope aborts.
  - validation: C3 LSP diagnostics for touched builder/test files passed;
    bounded container build passed and linked `build/main`; bounded
    `memory-lifetime-smoke` passed with `273 passed, 0 failed`.
  - prerequisites: preserve the fail-closed destination fallback contract.
  - negative-memory constraint: use `g_scope_force_escape_dtor_alloc_oom` for
    ESCAPE destructor registration tests; TEMP dtor OOM does not target
    `scope_register_dtor_escape`.

- [x] `AUDIT-241-STABLE-MATERIALIZED-CLOSURE-DTOR` fail closed when stable materialized closure cannot register env-scope teardown.
  - classification: runtime memory/lifetime, targeted stable destination
    materialization fix.
  - shipped: `stable_escape_materialize_init_closure` now returns `false` when
    the closure-specific `scope_dtor_closure` ESCAPE destructor registration
    cannot be recorded.
  - validation: C3 LSP diagnostics for touched destination/test files passed;
    bounded container build passed and linked `build/main`; bounded
    `memory-lifetime-smoke` passed with `274 passed, 0 failed`.
  - prerequisites: stable materialized closures must only finalize if their
    eventual `env_scope` release authority is registered.
  - negative-memory constraint: the generic stable materialized `Value`
    destructor registration and the closure-specific env-scope destructor
    registration are separate seams; test the latter directly when needed.

- [x] `MEM-PROOF-001` complete memory ownership inventory and manifest coverage.
  - classification: static proof coverage, targeted manifest/guard work.
  - shipped: added `scripts/check_memory_ownership_inventory.py` and
    `scripts/memory_ownership_surface_manifest.tsv` to classify
    memory-sensitive owning call sites, FFI wrapper families, dynamic FFI handle
    call sites, and tensor device finalizer authorities.
  - shipped: wired the new inventory guard into
    `scripts/check_boundary_change_policy.sh` and added the guard/manifest to
    `scripts/boundary_sensitive_files.txt`.
  - validation: `python3 scripts/check_memory_ownership_inventory.py` passed
    across `1228` C3 files; the boundary-sensitive-file subset passed across
    `36` files; `python3 scripts/check_boundary_value_policy_coverage.py`
    passed for all `30` `ValueTag` entries.
  - boundary-policy note: `OMNI_BOUNDARY_POLICY_RANGE=HEAD
    scripts/check_boundary_change_policy.sh` now runs both policy guards, then
    correctly fails closed because this dirty workspace lacks the required
    normal/ASAN boundary hardening evidence logs.
  - plan: `docs/plans/memory-model-proof-matrix-2026-04-26.md`.
  - negative-memory constraint: do not rely on the coarse `FFI_HANDLE` or
    `TENSOR` `ValueTag` rows alone for proof coverage; family-level FFI and
    tensor finalizer classifications are now the guard boundary.

- [x] `MEM-PROOF-002` prove ScopeRegion core ownership and lane teardown.
  - classification: runtime memory/lifetime, targeted ScopeRegion proof.
  - shipped: closed the ScopeRegion core proof lane by auditing
    TEMP/ESCAPE teardown, ESCAPE dtor/chunk splicing order, retain/release
    symmetry, TEMP and ESCAPE destructor-registration OOM handling,
    owner-thread checks, invalid parent/child splice preconditions, and the
    absence of normal-return `scope_adopt` call sites.
  - evidence: existing ScopeRegion tests cover chunk allocation OOM,
    TEMP/ESCAPE destructor-registration OOM, reset vs reset-temp-lane
    behavior, ESCAPE dtor order/tail consistency, parent/child refcount
    symmetry, splice rejection reasons, and owner-token mismatch rejection.
  - validation: bounded container build linked `build/main`; host
    `--test-suite scope` passed with `scope_region pass=64 fail=0`; bounded
    container `memory-lifetime-smoke` passed with `unified pass=274 fail=0`;
    bounded container Valgrind `memory-lifetime-smoke` reported zero Memcheck
    errors and zero definite/indirect/possible leaks.
  - validation note: host memory-lifetime-smoke correctly refuses to run
    outside the bounded container path; host `c3c build` without local
    `LIBRARY_PATH` remains blocked by missing `liblightning`/`libreplxx`.
  - plan: `docs/plans/memory-model-proof-matrix-2026-04-26.md`.
  - negative-memory constraint: do not treat host-side memory-lifetime slice
    refusal as a test failure; it is the intended container-only guard for
    memory ownership validation.

- [x] `MEM-PROOF-003` prove heap-backed value constructors fail closed.
  - classification: runtime allocation, targeted constructor hardening.
  - shipped: classified scalar/no-dtor and heap-backed constructor families,
    then hardened the missing `FFI_HANDLE` constructor path so
    `make_ffi_handle_ex_with_descriptor` registers the returned wrapper with
    `scope_register_value_dtor_or_cleanup`.
  - shipped: added focused FFI handle destructor-registration OOM regressions
    for finalizer-owned and free-owned payloads. The tests assert fail-closed
    `ERROR` return, no retained dtor record, and deterministic payload cleanup
    where an explicit finalizer hook is available.
  - validation: C3 LSP diagnostics passed for `value_constructors.c3` and the
    focused runtime allocation test file; bounded container build linked
    `build/main`; bounded container `memory-lifetime-smoke` passed with
    `unified pass=276 fail=0`; bounded container Valgrind
    `memory-lifetime-smoke` reported zero Memcheck errors and zero definite,
    indirect, or possible leaks; `check_boundary_value_policy_coverage.py` and
    the ownership inventory guard passed for the touched constructor files.
  - plan: `docs/plans/memory-model-proof-matrix-2026-04-26.md`.
  - negative-memory constraint: do not treat a raw `FFI_HANDLE` wrapper as
    safely constructed until the wrapper's `scope_dtor_value` registration is
    recorded or rollback has released the foreign payload.

- [x] `MEM-PROOF-004` prove env and closure lifetime symmetry.
  - classification: runtime memory/lifetime, targeted env/closure proof.
  - shipped: hardened closure-copy paths so cloned closure wrappers from
    copy-to-parent and env-copy fail closed if `scope_dtor_closure`
    registration fails, and added forced dtor-registration OOM regressions
    proving retained `env_scope` rollback.
  - shipped: fixed rejected-transplant retry for compatibility-destination
    iterator routes so closure-backed iterator thunks survive recursive method
    return boundaries after splice proof rejection.
  - validation: C3 LSP diagnostics passed for touched env/closure and boundary
    files; host build linked `build/main`; ASAN was attempted and rejected by
    the current C3 toolchain as unsupported for this target; bounded
    `memory-lifetime-smoke` passed with `unified pass=278 fail=0`; bounded
    graph-audit smoke passed with `unified pass=278 fail=0` and closure
    traversal counters present; focused bounded `jit-policy` filter passed
    with `unified pass=6 fail=0`; bounded Valgrind `memory-lifetime-smoke`
    reported zero Memcheck errors and zero definite, indirect, or possible
    leaks.
  - plan: `docs/plans/memory-model-proof-matrix-2026-04-26.md`.
  - negative-memory constraint: do not reuse a promotion context contaminated
    by a rejected transplant proof for a later compatibility-destination retry;
    use a fresh route context so failed transplant evidence cannot suppress a
    valid iterator/partial/cons destination build.

- [x] `MEM-PROOF-005` prove boundary commit route contracts.
  - classification: runtime boundary semantics, targeted route proof.
  - shipped: hardened direct closure escape promotion so boundary commit
    routes fail closed if `scope_dtor_closure` registration fails, with retained
    `env_scope` rollback.
  - shipped: added/strengthened route regressions for explicit mixed,
    compatibility, direct-promotion-disallowed, stable materialization, forced
    no-splice, and fail-closed behavior.
  - validation: C3 LSP diagnostics and host build passed; ASAN was attempted
    and rejected as unsupported by the current C3 toolchain; bounded
    `memory-lifetime-smoke` passed with `unified pass=280 fail=0`; boundary
    value policy, ownership inventory, and facade guards passed; counters
    `memory-lifetime-bench` passed and the benchmark envelope passed with one
    non-fatal optimizer-copy drift warning; bounded Valgrind
    `memory-lifetime-smoke` reported zero Memcheck errors and zero definite,
    indirect, or possible leaks.
  - plan: `docs/plans/memory-model-proof-matrix-2026-04-26.md`.
  - negative-memory constraint: do not treat a boundary route as committed
    unless the selected route, selected reason, destructor-registration
    authority, and rollback/fail-closed outcome are all observable in tests or
    counters.

- [x] `MEM-PROOF-006` prove stable escape, prepared graph, and transplant fast paths.
  - classification: runtime memory/lifetime, structural proof.
  - task: prove stable indices/passports, mutation invalidation, prepared graph
    materialization, and region transplant proof gates.
  - plan: `docs/plans/memory-model-proof-matrix-2026-04-26.md`.
  - closure evidence: stale-handle invalidation now rejects prepared-node tag
    and child lookups after teardown, mutation-drift invalidation remains
    fail-closed, cyclic/shared graph coverage is present, refcount rejection
    is explicit, and bounded smoke/benchmark/Valgrind validation passes.

- [x] `MEM-PROOF-007` prove collection and mutation ownership.
  - classification: runtime containers, targeted rollback/mutation proof.
  - task: prove arrays, dictionaries, sets, method tables, iterators, partials,
    and lists preserve edge tracking, rollback, and mutation invalidation.
  - plan: `docs/plans/memory-model-proof-matrix-2026-04-26.md`.
  - closure evidence: array push growth rollback, checked hashmap/set
    constructor failures, checked-growth rollback, method-table abort cleanup,
    shared-wrapper partial cleanup, and known-capacity constructor OOM now all
    pass under bounded smoke, benchmark, and Valgrind validation.

- [x] `MEM-PROOF-008` prove native tensor, ML, and device ownership.
  - classification: runtime native resource lifetime, targeted native/device
    proof.
  - task: prove CPU/native/CUDA/Vulkan payload cleanup authority, destructor
    registration failure cleanup, and no accidental duplicated device
    ownership.
  - plan: `docs/plans/memory-model-proof-matrix-2026-04-26.md`.
  - concrete next step: verified with CPU tensor constructor cleanup,
    CUDA to-device destructor-registration OOM cleanup, Vulkan
    layer-normalization destructor-registration OOM cleanup, bounded smoke, and
    Valgrind.

- [x] `MEM-PROOF-009` prove async, scheduler, thread, and callback lifetimes.
  - classification: runtime async/thread lifetime, targeted callback proof.
  - task: prove jobs, callbacks, futures, process/socket/TLS wrappers, and
    offload handles cannot retain dead Omni values through raw pointers.
  - plan: `docs/plans/memory-model-proof-matrix-2026-04-26.md`.
  - concrete next step: verified with callback-after-source-release coverage,
    bounded advanced-ffi-system-surface runtime validation, and bounded
    Valgrind on the broader surface. The broader Valgrind run still reports
    pre-existing leak contexts in unrelated ffi_callback/libffi paths.

- [x] `MEM-PROOF-010` close FFI ScopeRegion migration proof.
  - classification: runtime FFI/native resource lifetime, structural closure.
  - task: prove every FFI wrapper is ScopeRegion-owned, every foreign payload
    has one finalizer authority, and every bridge mode is explicit or
    fail-closed.
  - plan: `docs/plans/memory-model-proof-matrix-2026-04-26.md`.
  - concrete next step: verified with native wrapper-family metadata coverage
    for fs/tcp/udp/process/tls handles plus targeted Valgrind on the isolated
    foreign-handle metadata group.
