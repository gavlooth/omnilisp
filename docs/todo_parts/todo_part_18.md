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
