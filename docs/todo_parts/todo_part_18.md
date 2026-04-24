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

- [ ] `MEM-BOUNDARY-EPOCH-001` add mutation epoch invalidation for passport validation.
  - classification: runtime behavior, structural overhaul.
  - task: add cheap epoch snapshots for graph-carrying mutable values and use
    them to fail stale prepared handles before deep graph validation.
  - next step: start with arrays, dictionaries, sets, and closure env binding
    mutation sites, then extend to mutable cons paths if needed.
  - prerequisites: stable graph passport fields from
    `MEM-BOUNDARY-PASSPORT-001`.
  - negative-memory constraint: do not weaken existing structural mutation drift
    tests while adding epoch short-circuiting.

- [ ] `MEM-BOUNDARY-TRANSPLANT-001` replace ad hoc splice checks with `BoundaryTransplantProof`.
  - classification: runtime behavior, structural overhaul.
  - task: make whole-region transplant/splice a planner-selected route with a
    proof object that records owner thread, refcount, lane, scope-chain, TEMP
    edge, closure-env, and FFI/native constraints.
  - next step: wrap current scope-transfer legality checks in a proof-returning
    helper before changing any transplant success behavior.
  - prerequisites: planner decision API from `MEM-BOUNDARY-PLANNER-001`.
  - negative-memory constraint: transplant is legal only when the whole source
    ownership island survives; otherwise materialize or fail closed.

- [ ] `MEM-BOUNDARY-FFI-BRIDGE-001` add explicit FFI bridge boundary declarations.
  - classification: runtime structure, structural overhaul.
  - task: classify foreign handles as opaque, keepalive, copy-hook, trace-hook,
    or unsafe so boundary planning never guesses through native pointers.
  - next step: add bridge policy declarations while keeping existing
    `FFI_HANDLE` behavior opaque by default.
  - prerequisites: current `ValueTag` boundary ownership policy.
  - negative-memory constraint: foreign payload finalizers/RC must not become
    ownership authority over ordinary Omni `Value` graphs.

- [ ] `MEM-BOUNDARY-COPY-DEBT-001` expose copy debt and route-failure telemetry.
  - classification: runtime observability, targeted instrumentation.
  - task: report why stable publish or transplant failed, which route was
    selected, graph node count, and estimated copied payload bytes.
  - next step: extend runtime memory stats with planner route counters and
    materialization copy-debt counters.
  - prerequisites: route reasons from `MEM-BOUNDARY-PLANNER-001`.
  - negative-memory constraint: telemetry must not change route behavior or
    mask fail-closed outcomes.

- [ ] `MEM-BOUNDARY-PLAN-MIGRATE-001` migrate boundary commit paths to planner-selected outcomes.
  - classification: runtime behavior, structural overhaul.
  - task: make eval/JIT/root-store boundary commits consume planner decisions as
    the source of truth for stable publish, transplant, materialization, and
    fail-closed outcomes.
  - next step: migrate one boundary commit path after planner/passport/proof
    telemetry is validated.
  - prerequisites: `MEM-BOUNDARY-PLANNER-001`,
    `MEM-BOUNDARY-PASSPORT-001`, `MEM-BOUNDARY-TRANSPLANT-001`, and
    `MEM-BOUNDARY-COPY-DEBT-001`.
  - negative-memory constraint: do not preserve compatibility routes as implicit
    fallbacks once the planner owns route selection.
