# Active TODO Index Part 16

Source: `TODO.md`

This part starts the live rollout queue for
`docs/plans/stable-escape-graph-plan-2026-04-23.md`.

## 2026-04-24 Stable Escape Graph Rollout Backlog

- [x] `STABLE-ESCAPE-SHAPE-001` measure the shapes that actually reach the
  escape boundary and publish a baseline summary.
  - classification: runtime behavior, targeted measurement prep.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - files: `src/lisp/eval.c3`, `src/lisp/eval_boundary_provenance.c3`,
    `src/lisp/prim_runtime_memory_stats.c3`,
    `src/lisp/tests_memory_lifetime_boundary_groups.c3`,
    `src/lisp/tests_tests.c3`.
  - done 2026-04-24: added `BoundaryEscapeShapeStats` telemetry, wired the
    boundary reuse scan to record candidate-root shapes, exposed the counters
    through runtime memory stats and boundary traversal summaries, and added a
    smoke regression that exercises cons, array, and scalar promotions.
  - validation: `c3c build --obj-out obj`,
    `OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
    (`239 passed, 0 failed`), `scripts/check_status_consistency.sh`,
    `git diff --check`.

- [x] `STABLE-ESCAPE-STORE-001` introduce a runtime-private stable-handle
  store skeleton with generation checks.
  - classification: runtime structure, targeted scaffolding.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - files: `src/lisp/stable_escape_store.c3`,
    `src/lisp/value_interp_state.c3`,
    `src/lisp/value_interp_runtime_helpers.c3`,
    `src/lisp/value_interp_lifecycle.c3`,
    `src/lisp/tests_memory_lifetime_boundary_groups.c3`,
    `src/lisp/tests_tests.c3`.
  - done 2026-04-24: added an interpreter-owned stable-handle registry with
    slot/generation records, publish/resolve/retire/reset operations, scope
    generation checks, interpreter bootstrap reset wiring, and a smoke
    regression covering publish, resolve, explicit retire, and stale-handle
    failure.
  - validation: `c3c build --obj-out obj`, focused boundary smoke tests,
    `git diff --check`.

- [x] `STABLE-ESCAPE-PREP-001` route one boundary path through a prepared-
  graph publication hook and pin it with regression coverage.
  - classification: runtime behavior, targeted boundary integration.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - files: `src/lisp/eval_boundary_commit_escape.c3`,
    `src/lisp/eval_boundary_commit_escape_helpers.c3`,
    `src/lisp/stable_escape_store.c3`,
    `src/lisp/tests_memory_lifetime_boundary_commit_escape_primary_groups.c3`,
    `src/lisp/tests_tests.c3`.
  - done 2026-04-24: routed the already-safe reuse path through the
    prepared-publication hook, added publication and prepared-publication
    counters to the interpreter-owned stable store, and extended the
    already-safe reuse regression so it checks both counters increment while
    the returned value and copy-site count remain unchanged.
  - validation: `c3c build --obj-out obj`, targeted boundary smoke tests,
    `git diff --check`.

- [x] `STABLE-ESCAPE-GRAPH-001` land the first real prepared-graph data
  structure slice inside the stable escape store.
  - classification: runtime structure, targeted prepared-graph implementation.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - files: `src/lisp/stable_escape_store.c3`,
    `src/lisp/tests_memory_lifetime_boundary_groups.c3`,
    `src/lisp/tests_memory_lifetime_boundary_commit_escape_primary_groups.c3`,
    `TODO.md`, `memory/CHANGELOG.md`,
    `memory/changelog_parts/changelog_part_37.md`,
    `.agents/SESSION_REPORT.md`.
  - done 2026-04-24: added a real prepared-node graph inside the
    interpreter-owned stable store for reusable `CONS` roots with scalar
    leaves and `ARRAY` roots with shared child reuse, preserved node identity
    by stable child indices, exposed narrow inspection helpers for tests, and
    kept the already-safe boundary reuse path on the prepared publication route
    instead of the old raw-registry round-trip only.
  - validation: `c3c build --obj-out obj`,
    `scripts/run_validation_container.sh env OMNI_TEST_VERBOSE=0 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`,
    `scripts/check_status_consistency.sh`, `git diff --check`.

- [x] `STABLE-ESCAPE-OBSERVE-001` make prepared-publication outcomes
  observable instead of allowing silent compatibility fallback.
  - classification: runtime behavior, targeted validation instrumentation.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - files: `src/lisp/stable_escape_store.c3`,
    `src/lisp/prim_runtime_memory_stats.c3`, `src/lisp/tests_tests.c3`,
    `src/lisp/tests_memory_lifetime_boundary_groups.c3`,
    `docs/plans/stable-escape-graph-plan-2026-04-23.md`,
    `docs/plans/README.md`, `TODO.md`, `memory/CHANGELOG.md`,
    `memory/changelog_parts/changelog_part_37.md`.
  - done 2026-04-24: added explicit counters for raw publication fallback,
    prepared-publication success, prepare-failure fallback, alias-unsafe skips,
    and resolve failures; exposed the fallback counters through
    `runtime-memory-stats`; and added a boundary regression proving unsupported
    prepared graphs increment fallback counters instead of being mistaken for
    prepared success.
  - validation: C3 diagnostics for touched runtime/test C3 files passed; host
    shell validation is currently blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

- [x] `STABLE-ESCAPE-DICT-001` extend prepared graph metadata to dictionary
  roots.
  - classification: runtime structure, targeted prepared-graph implementation.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - files: `src/lisp/stable_escape_store.c3`,
    `src/lisp/tests_memory_lifetime_boundary_groups.c3`,
    `docs/plans/stable-escape-graph-plan-2026-04-23.md`,
    `docs/plans/README.md`, `TODO.md`, `memory/CHANGELOG.md`,
    `memory/changelog_parts/changelog_part_37.md`.
  - done 2026-04-24: prepared `HASHMAP` roots now publish key/value child-index
    pairs in hash-table slot order and preserve shared value identity through
    the existing memo table.
  - validation: C3 diagnostics for touched runtime/test C3 files passed; host
    shell validation is currently blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

- [x] `STABLE-ESCAPE-SET-001` extend prepared graph metadata to set roots.
  - classification: runtime structure, targeted prepared-graph implementation.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - files: `src/lisp/stable_escape_store.c3`,
    `src/lisp/tests_memory_lifetime_boundary_groups.c3`,
    `docs/plans/stable-escape-graph-plan-2026-04-23.md`,
    `docs/plans/README.md`, `TODO.md`, `memory/CHANGELOG.md`,
    `memory/changelog_parts/changelog_part_37.md`.
  - done 2026-04-24: prepared `SET` roots now publish member child indices
    only, preserving set semantics and leaving backing true-values out of the
    graph; unsupported fallback observability now probes `CLOSURE`.
  - validation: C3 diagnostics for touched runtime/test C3 files passed; host
    shell validation is currently blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

- [x] `STABLE-ESCAPE-CYCLE-001` pin cycle representation in prepared graph
  metadata.
  - classification: runtime behavior, targeted prepared-graph regression.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - files: `src/lisp/tests_memory_lifetime_boundary_groups.c3`,
    `docs/plans/stable-escape-graph-plan-2026-04-23.md`,
    `docs/plans/README.md`, `TODO.md`, `memory/CHANGELOG.md`,
    `memory/changelog_parts/changelog_part_37.md`.
  - done 2026-04-24: a self-referential `CONS` graph now has regression
    coverage proving both child edges point back to the already-registered root
    prepared node instead of recursively expanding.
  - validation: C3 diagnostics for touched runtime/test C3 files passed; host
    shell validation is currently blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

- [x] `STABLE-ESCAPE-CLOSURE-001` extend prepared graph metadata to closure
  roots.
  - classification: runtime structure, targeted prepared-graph implementation.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - files: `src/lisp/stable_escape_store.c3`,
    `src/lisp/tests_memory_lifetime_boundary_groups.c3`,
    `docs/plans/stable-escape-graph-plan-2026-04-23.md`,
    `docs/plans/README.md`, `TODO.md`, `memory/CHANGELOG.md`,
    `memory/changelog_parts/changelog_part_37.md`.
  - done 2026-04-24: prepared `CLOSURE` roots now publish captured environment
    binding-value edges up to, but not including, `global_env`; unsupported
    fallback observability now probes `ITERATOR`.
  - validation: C3 diagnostics for touched runtime/test C3 files passed; host
    shell validation is currently blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

- [x] `STABLE-ESCAPE-EDGE-ORDER-001` fix nested prepared edge range ordering.
  - classification: runtime correctness, targeted audit fix.
  - source: audit after `STABLE-ESCAPE-CLOSURE-001`.
  - files: `src/lisp/stable_escape_store.c3`, `TODO.md`,
    `docs/todo_parts/todo_part_16.md`, `memory/CHANGELOG.md`,
    `memory/changelog_parts/changelog_part_37.md`.
  - done 2026-04-24: array, dictionary, set, and closure preparation now collect
    child node indices first and append parent edges only after recursive child
    preparation, so parent `child_offset` ranges are contiguous and do not point
    at nested child edges.
  - validation: C3 diagnostics for touched runtime/test C3 files passed; host
    shell validation is currently blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

- [x] `STABLE-ESCAPE-MUTATION-001` seal prepared graph structure after
  publication.
  - classification: runtime behavior, targeted mutation policy implementation.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - files: `src/lisp/stable_escape_store.c3`,
    `src/lisp/tests_memory_lifetime_boundary_groups.c3`,
    `docs/plans/stable-escape-graph-plan-2026-04-23.md`,
    `docs/plans/README.md`, `TODO.md`, `memory/CHANGELOG.md`,
    `memory/changelog_parts/changelog_part_37.md`.
  - done 2026-04-24: prepared handles now fail closed when current child
    pointers or captured closure binding values no longer match the prepared
    child-index snapshot; regressions cover cons child mutation and closure
    captured-binding mutation.
  - validation: C3 diagnostics for touched runtime/test C3 files passed; host
    shell validation is currently blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

- [x] `STABLE-ESCAPE-COMPAT-001` make boundary commit route selection explicit.
  - classification: runtime behavior, targeted compatibility visibility.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - files: `src/lisp/eval_boundary_api_types.c3`,
    `src/lisp/eval_boundary_commit_escape.c3`,
    `src/lisp/eval_boundary_commit_escape_helpers.c3`,
    `src/lisp/eval_boundary_graph_audit_logging.c3`,
    `src/lisp/tests_memory_lifetime_boundary_commit_escape_primary_groups.c3`,
    `docs/plans/stable-escape-graph-plan-2026-04-23.md`,
    `TODO.md`, `memory/CHANGELOG.md`,
    `memory/changelog_parts/changelog_part_37.md`.
  - done 2026-04-24: `BoundaryCommitEscapeResult` now records
    `stable_publication_attempted` and `compatibility_path_used`; boundary
    commit helpers set those flags for already-safe prepared publication,
    destination promotion, releasing-scope splice, and mixed-destination
    promotion; regressions prove TEMP cons commits use compatibility while
    already-safe reusable commits use the stable prepared-publication route.
  - validation: C3 diagnostics for touched runtime/test C3 files passed; host
    shell validation is currently blocked by `bwrap: loopback: Failed RTM_NEWADDR`.

- [x] `STABLE-ESCAPE-ARENA-001` materialize prepared `CONS`/immediate-scalar
  graphs outside the source scope for TEMP return values.
  - classification: runtime behavior, structural prepared-graph materialization.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - files: `src/lisp/stable_escape_store.c3`,
    `src/lisp/eval_boundary_api_types.c3`,
    `src/lisp/eval_boundary_commit_destination.c3`,
    `src/lisp/eval_boundary_commit_escape.c3`,
    `src/lisp/eval_boundary_commit_escape_helpers.c3`,
    `src/lisp/eval_boundary_graph_audit_logging.c3`,
    `src/lisp/tests_memory_lifetime_boundary_commit_escape_primary_groups.c3`,
    `docs/plans/stable-escape-graph-plan-2026-04-23.md`,
    `TODO.md`, `memory/CHANGELOG.md`,
    `memory/changelog_parts/changelog_part_37.md`.
  - done 2026-04-24: added a build-scope prepared materializer for `CONS`
    graphs whose leaves are immediate scalar values, routed TEMP cons boundary
    commits through it before the compatibility builder, and extended route
    flags/tests so the TEMP cons path reports stable materialization instead of
    compatibility routing.
  - validation: C3 diagnostics and parse-document passed for touched runtime/test
    C3 files; host shell validation is currently blocked by
    `bwrap: loopback: Failed RTM_NEWADDR`.

- [x] `STABLE-ESCAPE-ARENA-002` extend prepared materialization beyond
  `CONS`/immediate-scalar graphs for current prepared container families.
  - classification: runtime behavior, structural prepared-graph materialization.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - shipped: destination commits now try prepared materialization for `CONS`,
    `ARRAY`, dictionary, set, and untyped closure roots before compatibility
    promotion; materialized graphs preserve prepared child indices, sharing, set
    member semantics, string/error payload copies, and promotion-budget
    accounting.
  - files: `src/lisp/eval_boundary_commit_destination.c3`,
    `src/lisp/stable_escape_store.c3`,
    `src/lisp/tests_memory_lifetime_boundary_commit_escape_primary_groups.c3`.
  - validation: C3 diagnostics and parse-document passed for the touched runtime
    and regression C3 files; shell validation remains blocked by
    `bwrap: loopback: Failed RTM_NEWADDR`.

- [x] `STABLE-ESCAPE-ARENA-003` close remaining prepared-materialization payload
  exceptions.
  - classification: runtime behavior, targeted structural materialization.
  - source: `docs/plans/stable-escape-graph-plan-2026-04-23.md`.
  - shipped: prepared materialization now clones `BIG_INTEGER`, `BIG_FLOAT`, and
    `BIG_COMPLEX` payload handles, and typed closures copy method signatures into
    the destination build scope's ESCAPE lane with `method_signature_copy_to_escape`.
  - files: `src/lisp/eval_boundary_commit_destination.c3`,
    `src/lisp/tests_memory_lifetime_boundary_commit_escape_primary_groups.c3`.
  - validation: C3 diagnostics and parse-document passed for the touched runtime
    and regression C3 files; shell validation remains blocked by
    `bwrap: loopback: Failed RTM_NEWADDR`.
  - closure note: this closes the planned prepared-materialization payload
    exceptions; any new work should be a fresh semantic boundary, not another
    case-specific residual under this rollout.
