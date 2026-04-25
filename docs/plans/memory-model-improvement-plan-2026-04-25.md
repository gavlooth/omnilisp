# Memory Model Improvement Plan - 2026-04-25

Status: completed plan

## Objective

Improve Omni's memory model after the proof-planner and copy-debt closure work
without weakening the region ownership contract.

The goal is not to replace `ScopeRegion`, add ordinary per-value RC, or chase
unmeasured copy reductions. The next useful work is to reduce measured
allocator and collection pressure, make the boundary contract harder to bypass,
and extend explicit bridge policies only where the proof model can stay
fail-closed.

## Active Hypothesis

The TEMP/ESCAPE architecture is now mature enough that architecture churn is
lower value than measured refinement. Current evidence shows:

- optimizer-addressable stable-materialization copy debt is zero in the
  benchmark profile;
- the explicit no-splice closure rollback materialization remains expected
  coverage, not optimization debt;
- allocator and collection shape counters are the dominant visible pressure:
  TEMP/ESCAPE slow paths, reset/destroy slack, hashmap growth, set growth, and
  array growth;
- `FfiBridgeBoundaryMode` exists, but copy/trace hook modes intentionally fail
  closed until real bridge hooks exist.

Therefore the next memory-model improvement should proceed in this order:

1. refresh and stabilize the benchmark evidence,
2. reduce allocator/slack pressure where counters justify it,
3. reduce collection growth pressure through explicit sizing contracts,
4. turn boundary policy requirements into mechanical checks,
5. add narrow FFI bridge hooks only for wrappers with explicit traversal/copy
   authority,
6. broaden workload coverage before opening another architecture phase.

## Current Approach

Keep `ScopeRegion` as the owner of ordinary Omni values and keep boundary
movement routed through the existing planner ladder:

1. stable indexed publish/reuse,
2. proof-backed whole-region transplant/splice,
3. prepared destination materialization,
4. fail closed.

Each implementation slice must preserve these constraints:

- no ordinary language-value per-object RC,
- no root pinning as a correctness shortcut,
- no hidden fallback from failed proof to unsafe compatibility behavior,
- no FFI/native traversal without an explicit bridge declaration and hook,
- no removal of the forced no-splice rollback coverage bucket to improve
  counters.

## Work Slices

### `MEM-MODEL-IMPROVE-001` - Refresh The Evidence Baseline

Classification: runtime benchmark evidence, targeted measurement.

Status: closed 2026-04-25.

Task:

- rerun bounded counters-enabled `memory-lifetime-bench` several times in the
  validation container;
- run `scripts/check_memory_telemetry_benchmark_envelope.sh` on each log;
- record median/range for allocator pressure, slack, collection growth, route
  selection, materialization copy bytes, and timing warnings;
- update the benchmark baseline note with a 2026-04-25+ evidence block.

Why:

- the current baseline proves copy debt is not the active optimization target;
- allocator/slack counters need repeated runs before changing chunk or pool
  policy.

Concrete next step:

```bash
c3c build --obj-out obj -D OMNI_BOUNDARY_INSTR_COUNTERS
scripts/run_validation_container.sh env \
  LD_LIBRARY_PATH=/usr/local/lib \
  OMNI_TEST_QUIET=1 \
  OMNI_TEST_SUMMARY=1 \
  OMNI_BOUNDARY_BENCH=1 \
  OMNI_BOUNDARY_INSTR_COUNTERS=1 \
  OMNI_LISP_TEST_SLICE=memory-lifetime-bench \
  ./build/main --test-suite lisp
```

Prerequisites:

- benchmark-capable build succeeds;
- validation container is available;
- existing benchmark envelope script remains authoritative.

Validation:

- `scripts/check_memory_telemetry_benchmark_envelope.sh <log>`;
- `scripts/check_status_consistency.sh`;
- `git diff --check`.

Negative-memory constraints:

- do not treat wall-clock timing as a strict gate until repeated bounded runs
  show stable timing;
- do not reopen closed copy-debt tasks when
  `materialization_copy_bytes_optimizer=0`.

Closure evidence:

- Built in the validation container with
  `c3c --threads 1 build --obj-out obj_mem_model -D OMNI_BOUNDARY_INSTR_COUNTERS`.
- Ran three bounded counters-enabled `memory-lifetime-bench` passes and checked
  each log with `scripts/check_memory_telemetry_benchmark_envelope.sh`.
- Raw logs are stored under `.agents/memory-model-improve-001-runs/`.
- Correctness counters were stable across all three runs:
  `splice_ok=2048`, `disallowed_ok=2048`, `reuse_ok=2048`,
  `partial_ok=2048`, `shape_ok=128`, `closure_env_ok=32`,
  `stable_passport_ok=1`, and `splice_fail_total=0`.
- Copy debt remained zero:
  `materialization_copy_bytes_delta=0`,
  `materialization_copy_bytes_optimizer=0`, and
  `materialization_copy_bytes_forced_no_splice=0`.
- Non-copy counters were stable and point at allocator/slack plus collection
  growth:
  `escape_slow_delta=416`, `temp_slow_delta=209`,
  `escape_destroy_slack_delta=603776`, `temp_destroy_slack_delta=209536`,
  `hashmap_growth_delta=1024`, `set_growth_delta=512`, and
  `array_growth_delta=128`.

### `MEM-MODEL-IMPROVE-002` - Tune Scope Allocator And Slack Policy

Classification: runtime performance, targeted allocator optimization.

Status: completed 2026-04-26.

Task:

- use refreshed counters to identify the dominant TEMP/ESCAPE slow-path and
  slack buckets;
- inspect `ScopeRegion` chunk selection, reset, destroy, recycle, and
  fiber-temp pool behavior;
- add any missing low-overhead size-class/histogram counters before changing
  policy;
- tune chunk sizing, reuse thresholds, or fiber-temp eligibility only when the
  counter delta points to a specific pressure class;
- preserve OOM fail-closed behavior and scope teardown ordering.

Why:

- the first baseline shows allocator pressure and slack, not boundary copy, as
  the most visible optimization surface.

Concrete next step:

- inspect `src/scope_region*.c3`, `src/scope_region_temp_pool_stats.c3`, and
  the `boundary_value_shape_counters` workload;
- propose one narrow policy change with before/after counter expectations.

Prerequisites:

- `MEM-MODEL-IMPROVE-001` has a repeated-run baseline;
- any added counters are compile-time or existing-debug-path gated.

Validation:

- `c3c build --obj-out obj`;
- bounded counters-enabled `memory-lifetime-bench`;
- bounded `memory-lifetime-smoke`;
- bounded container Valgrind `memory-lifetime-smoke` if ownership/teardown
  behavior changes;
- `scripts/check_memory_telemetry_benchmark_envelope.sh`;
- `git diff --check`.

Negative-memory constraints:

- do not use root pinning or speculative descendant-owner release to reduce
  teardown pressure;
- do not trust Valgrind through `env` unless the command uses
  `--trace-children=yes`.

Progress:

- 2026-04-25: added gated TEMP/ESCAPE slow-allocation slack histograms before
  changing allocator policy. `boundary_value_shape_counters` now reports exact,
  `<=512`, `<=4096`, and `>4096` slack buckets for each lane.
- Validation passed for the histogram slice:
  `OMNI_MEM_TELEM_REQUIRE_SLOW_SLACK_HISTOGRAM=1
  scripts/check_memory_telemetry_benchmark_envelope.sh
  .agents/memory-model-improve-002-histogram.log` and bounded `basic`.
- The first histogram run shows ESCAPE slow slack is mostly `<=4096`
  (`escape_slow_slack_le4096_delta=415`,
  `escape_slow_slack_gt4096_delta=0`), while TEMP owns the large-slack bucket
  (`temp_slow_slack_gt4096_delta=144`).
- Direct ESCAPE chunk size-class policy attempts were reverted. The validating
  `memory-lifetime-smoke` failure reproduced after revert and is tracked as
  `AUDIT-238`, so it is a separate continuation boundary bug rather than proof
  that all ESCAPE size-class work is invalid.
- Direct TEMP large slow-allocation exact-fit and exact-plus-4096-headroom
  chunk sizing attempts were also reverted. Correctness stayed green, but the
  benchmark signal regressed (`temp_slow_delta` `209 -> 336`,
  `temp_selected_chunk_delta` `3219456 -> 4285568/5858432`,
  `temp_destroy_slack_delta` `209536 -> 1275648/2847488`). Do not tune
  allocator policy from aggregate slack alone; add per-scope
  allocation-sequence telemetry before another allocator policy change.
- 2026-04-26: added per-scope slow-allocation sequence telemetry. Each TEMP
  and ESCAPE slow chunk now records whether later allocations consume its
  headroom before the sequence closes at the next slow allocation, reset,
  destroy, or splice/adoption boundary.
- The first sequence run shows broad TEMP large-slack reduction is the wrong
  next policy target: `temp_slow_sequence_large_delta=144`,
  `temp_slow_sequence_large_followup_bytes_delta=2002192`, and
  `temp_slow_sequence_large_no_followup_delta=0`. ESCAPE has no large-slack
  sequences in this workload but many no-follow-up sequences
  (`escape_slow_sequence_no_followup_delta=256`), so the next policy work
  should split ESCAPE no-follow-up sequences by request/unused size class
  before changing chunk selection.
- 2026-04-26 follow-up: added request/unused size classes for no-follow-up
  slow sequences and gated the ESCAPE bucket sums in the benchmark envelope.
  The first run shows the ESCAPE no-follow-up class is mixed:
  `request<=512=128`, `request<=4096=1`, `request>4096=127`, while unused at
  close is almost entirely `<=4096` (`unused_exact=1`, `unused<=4096=255`,
  `unused>4096=0`). This invalidates size-only ESCAPE chunk reduction as a
  broad policy; the next useful evidence is source/site attribution for
  no-follow-up ESCAPE sequences.
- 2026-04-26 closure: added source/site attribution for ESCAPE no-follow-up
  sequences and gated the source bucket sum in the benchmark envelope. The
  bounded run reports `escape_slow_sequence_no_followup_source_direct_delta=256`
  and zero for dtor, interpreter value/env, boundary payload, promotion
  signature, promotion closure, and JIT staged-arg sources. The remaining
  ESCAPE no-follow-up bucket is therefore the synthetic direct allocator probe
  in `boundary_value_shape_counters`; no allocator policy change is justified
  by the current profile.

### `MEM-MODEL-IMPROVE-003` - Reduce Collection Growth Pressure

Classification: runtime performance, targeted collection-sizing optimization.

Status: completed 2026-04-25.

Task:

- use `array_growth_delta`, `hashmap_growth_delta`, and `set_growth_delta`
  counters to identify builder/constructor hot paths;
- add explicit expected-capacity propagation where the source length is already
  known;
- keep constructor and insertion failure fail-closed;
- avoid storing boundary `ERROR` values as collection data.

Why:

- current baseline collection-growth counters are high enough to justify a
  focused pass after allocator evidence is refreshed.

Concrete next step:

- completed: shared Dictionary/Set known-entry capacity sizing eliminated the
  benchmark hashmap/set growth counters without bypassing checked constructors
  or checked insertion.

Prerequisites:

- `MEM-MODEL-IMPROVE-001` confirms collection growth remains a dominant signal.

Validation:

- targeted collection/runtime tests for touched families;
- bounded counters-enabled `memory-lifetime-bench`;
- bounded `memory-lifetime-smoke` if boundary-owned collections are touched;
- `c3c build --obj-out obj`;
- `git diff --check`.

Negative-memory constraints:

- do not bypass checked constructors or checked insertion helpers to pre-size
  faster;
- do not hide grow failure by returning partially populated collections.

Progress:

- 2026-04-25: `hashmap_capacity_hint_for_entry_count` now returns normalized
  power-of-two capacities. Dictionary and Set constructors share the same
  known-entry hint path, and checked known-entry constructors are available for
  internal benchmark/builder paths.
- `boundary_value_shape_counters` now pre-sizes dictionary and set workloads
  from `BOUNDARY_BENCH_SHAPE_ITEMS`; this moved
  `hashmap_growth_delta` from `1024` to `0` and `set_growth_delta` from `512`
  to `0` while leaving `materialization_copy_bytes_delta=0`.
- `scripts/check_memory_telemetry_benchmark_envelope.sh` now treats
  hashmap/set growth counters as required fields rather than mandatory
  positive pressure. Use
  `OMNI_MEM_TELEM_REQUIRE_COLLECTION_GROWTH_ZERO=1` to enforce the optimized
  benchmark contract.

### `MEM-MODEL-IMPROVE-004` - Mechanically Enforce Boundary Policy Coverage

Classification: static tooling plus runtime policy, targeted guardrail.

Status: completed 2026-04-25.

Task:

- add or extend a guard script that verifies every graph-carrying `ValueTag`
  has an explicit boundary ownership policy;
- make the guard check for declared edge policy, materialization/copy route,
  destructor authority, rollback coverage, graph-audit handling, and FFI/native
  exclusion where applicable;
- wire the guard into the existing boundary policy check path.

Why:

- the policy table is the right contract, but future value families should not
  rely on reviewer memory to add all required hooks.

Concrete next step:

- completed: `scripts/check_boundary_value_policy_coverage.py` verifies the
  manifest against the `ValueTag` enum and runtime ownership/edge/copy-route
  switch tables, and `scripts/check_boundary_change_policy.sh` runs it before
  checking boundary-sensitive validation logs.

Prerequisites:

- no new `ValueTag` family is being added in parallel without coordinating the
  policy source.

Validation:

- `scripts/check_boundary_change_policy.sh`;
- new/updated guard script direct run;
- `c3c build --obj-out obj` if runtime policy code changes;
- `git diff --check`.

Negative-memory constraints:

- do not mark heap-backed scalar payloads as immediate/by-value; strings,
  errors, big numbers, and signatures need explicit clone/finalizer policy;
- do not assume prepared parent edge ranges remain valid if captured before
  recursive child preparation.

Progress:

- 2026-04-25: added `scripts/boundary_value_policy_manifest.tsv` as the
  source-of-truth coverage manifest for every `ValueTag`.
- The manifest declares ownership policy, edge policy, copy route,
  stable-materialization eligibility, graph-audit class, destructor authority,
  native/FFI exclusion, and rollback policy.
- `scripts/check_boundary_value_policy_coverage.py` verifies all 30 current
  `ValueTag` entries and cross-checks runtime switch mappings in
  `value_boundary_ownership_policy.c3` and `eval_promotion_copy.c3`.
- `scripts/boundary_sensitive_files.txt` now includes boundary value policy,
  graph audit, stable store/materialization, and the new manifest/guard files.

### `MEM-MODEL-IMPROVE-005` - Add One Narrow FFI Bridge Hook Family

Classification: runtime behavior, targeted bridge implementation.

Status: Completed 2026-04-25.

Task:

- choose one FFI/foreign wrapper family that actually needs bridge-aware copy or
  trace behavior;
- implement the corresponding `FfiBridgeBoundaryMode` hook path for that family
  only;
- keep opaque as the default for all other FFI handles;
- prove unsafe/undeclared modes still fail closed.

Why:

- bridge declarations exist, but copy/trace modes intentionally fail closed
  until bridge hooks are implemented. A narrow first hook will validate the
  contract without making native pointers a generic ownership loophole.

Concrete next step:

- inventory current `FfiHandle` construction sites and identify one wrapper
  with clear release authority and no hidden Omni graph ownership.

Prerequisites:

- explicit owner/release authority for the chosen wrapper;
- tests can force bridge success and failure paths deterministically.

Validation:

- targeted FFI bridge tests;
- bounded `memory-lifetime-smoke`;
- bounded container Valgrind `memory-lifetime-smoke`;
- targeted advanced FFI group if public FFI behavior changes;
- `scripts/check_boundary_change_policy.sh`;
- `git diff --check`.

Closure:

- `atomic-ref` is the first explicit keepalive FFI bridge family. It is an
  Omni-allocated native cell with one release authority (`free_lib_handle`) and
  no Omni-owned graph edges.
- FFI constructor plumbing now carries `FfiBridgeBoundaryMode` through
  `make_ffi_handle_ex` and `make_ffi_handle_ex_with_descriptor`.
- `copy_ffi_handle_to_parent` shallow-shares only opaque/keepalive handles and
  fails closed for declared copy-hook, trace-hook, and unsafe modes.
- This is a keepalive/shareability hook slice. True native graph traversal
  hooks remain intentionally fail-closed; `ffi-callback` is the likely future
  family if a real trace/copy hook is needed.
- Validation passed: bounded build; bounded `memory-lifetime-smoke` `271/0`;
  bounded `atomic` `11/0`; bounded `advanced-ffi-system` `185/0`; boundary
  policy guard; traced-child Valgrind `memory-lifetime-smoke` `271/0` with
  zero Memcheck errors and zero definite/indirect/possible leaks;
  `git diff --check`.

Negative-memory constraints:

- do not traverse raw native pointers without bridge-owned trace/copy hooks;
- do not let foreign payload RC/finalizers own ordinary Omni `Value` graphs.

### `MEM-MODEL-IMPROVE-006` - Broaden Memory Workload Coverage

Classification: runtime benchmark coverage, targeted workload expansion.

Status: Closed 2026-04-25. Product-style, closure-heavy iterator,
tensor-metadata crossing, and nested-module return workload slices landed and
are envelope-checked.

Task:

- add benchmark fixtures that look more like real programs: nested module
  returns, closure-heavy iterator pipelines, dictionary/set workloads, tensor
  metadata crossings, FFI wrapper crossings, and one product-style example
  derived from `examples/finwatch/main.omni`;
- keep each fixture tied to stable `OMNI_BENCH_SUMMARY` counters;
- update the regression envelope only after the new counters are stable.

Why:

- current benchmark lanes are useful but synthetic. Broader workloads reduce
  the risk of optimizing allocator or collection behavior for narrow fixtures.

Closure:

- `finwatch_product_memory` covers product-style dictionaries, sets, arrays,
  strings, symbols, and FFI wrapper crossings;
- `closure_iterator_pipeline_memory` covers closure-heavy lazy iterator
  pipelines;
- `tensor_metadata_crossing_memory` covers tensor payload and metadata graph
  crossings;
- `nested_module_return_memory` covers graph payloads built inside nested
  module bodies and returned across the boundary.

Prerequisites:

- `MEM-MODEL-IMPROVE-001` baseline is recorded so new workload deltas can be
  interpreted separately from existing lanes.

Validation:

- bounded counters-enabled `memory-lifetime-bench`;
- benchmark envelope script updated with correctness and counter-presence
  gates;
- normal bounded build and counters-enabled bounded build;
- nested module regression coverage in the advanced module group;
- the nested-module slice records stable materialization copy debt when a
  region-transplant splice proof rejects a stable graph candidate.
- `scripts/check_status_consistency.sh`;
- `git diff --check`.

Landed slices:

- Added `finwatch_product_memory`, a product-style benchmark derived from the
  canonical `examples/finwatch` service shape. The fixture builds route-like
  request/response dictionaries, quote arrays, holding arrays, watchlist sets,
  strings, symbols, and a releasable FFI wrapper, then sends the response graph
  through `boundary_commit_escape`.
- The benchmark emits its own stable `OMNI_BENCH_SUMMARY` line. The envelope
  currently checks line presence, fixture/commit success, and nonnegative
  counter fields only; it intentionally does not add timing thresholds.
- Validation passed with counters-enabled bounded `memory-lifetime-bench`:
  `product_ok=64`, `commit_ok=64`, `hashmap_construct_delta=1472`,
  `set_construct_delta=64`, `array_construct_delta=128`,
  `string_payload_bytes_delta=48512`, `ffi_wrappers_delta=64`, and
  `ffi_releasable_delta=64`.
- Added `closure_iterator_pipeline_memory`, a closure-heavy lazy iterator
  benchmark. It builds captured `map`/`filter` lambdas over `range-from`,
  returns the lazy iterator across the top-level boundary, then realizes it
  through `List` and checks the resulting count and sum.
- Validation passed with normal and counters-enabled bounded builds plus a
  counters-enabled bounded `memory-lifetime-bench`: `pipeline_ok=64`,
  `count_total=640`, `sum_total=22720`, `iterator_roots_delta=512`,
  `partial_roots_delta=640`, `closure_roots_delta=448`,
  `closure_env_frame_delta=512`, `closure_env_binding_delta=512`, and
  `materialization_copy_bytes_delta=0`.
- Added `tensor_metadata_crossing_memory`, a tensor-heavy metadata crossing
  benchmark. It builds TEMP cons records containing CPU `Float64` tensors and
  shape metadata, commits the graph through `boundary_commit_escape`, and
  validates the committed graph without relying on root-promoting Dictionary
  storage.
- Validation passed with normal and counters-enabled bounded builds plus a
  counters-enabled bounded `memory-lifetime-bench`: `tensor_ok=64`,
  `commit_ok=64`, `tensor_roots_delta=256`,
  `tensor_payload_bytes_delta=12288`, `array_construct_delta=256`,
  `selected_transplant_delta=64`, and `materialization_copy_bytes_delta=0`.

Remaining workload slices:

- nested module returns;

Negative-memory constraints:

- do not weaken boundary proof/fail-closed invariants to make benchmark
  fixtures pass;
- do not add strict timing gates until repeated bounded-container runs prove
  timing stability.

## Validation Strategy

Use smallest meaningful checks first, then broaden:

1. Static guard/script checks for policy-only changes.
2. `c3c build --obj-out obj`.
3. Targeted runtime group for the touched surface.
4. Bounded counters-enabled `memory-lifetime-bench` for performance-sensitive
   changes.
5. Bounded `memory-lifetime-smoke` for ownership/lifetime behavior changes.
6. Bounded container Valgrind `memory-lifetime-smoke` for teardown,
   transplant, bridge, or ownership-authority changes.

Do not report memory/lifetime success without container-bounded validation when
the changed behavior can materially affect memory ownership.

## Next Checkpoint

The memory-model improvement queue is closed. Reopen allocator policy only when
a non-synthetic workload or repeated bounded benchmark shows a runtime
boundary/promotion allocation source with nonzero no-follow-up pressure, copy
debt, or collection growth outside the observability fixtures.

## Agent Assignments

Single integration owner: main Codex agent.

Parallel-safe future tracks, if explicitly orchestrated later:

- benchmark evidence and envelope updates,
- scope allocator/fiber-temp policy,
- collection growth reduction,
- boundary policy guard scripting,
- FFI bridge hook prototype.

Parallel edits must keep write scopes disjoint and the integration owner must
run the final validation gate.
