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
    `scripts/run_validation_container.sh valgrind --leak-check=full --error-exitcode=99 env OMNI_TEST_VERBOSE=0 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    (`253 passed, 0 failed` under bounded container Valgrind); host Valgrind
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
