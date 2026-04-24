# Memory Boundary Telemetry Signal Inventory - 2026-04-24

Status: completed inventory for `MEM-BENCH-OBSERVE-001`.

## Scope

This inventory maps the observability that already exists before expanding the
memory-boundary benchmark lane. It covers boundary counters,
`runtime-memory-stats`, `OMNI_TEST_SUMMARY` / `OMNI_BENCH_SUMMARY` lines, and
profile scripts. It does not claim new runtime instrumentation.

## Existing Runtime Counters

| Surface | Source | Availability | Existing fields |
| --- | --- | --- | --- |
| Boundary route stats | `src/lisp/eval_boundary_telemetry.c3` | compile-time gated by `OMNI_BOUNDARY_INSTR_COUNTERS`, `OMNI_BOUNDARY_INSTR_TRACE`, or `OMNI_BOUNDARY_INSTR_BENCHMARK` | splice attempted/succeeded/failure reasons, promotion attempted/abort reasons, copy fallback total, planned route by route/tag, selected route by route/tag, route failure by reason, stable materialization success/node count/copy bytes by root tag, scope-chain scan total/hint/fallback/suppressed, graph audit invoked/skipped |
| Runtime memory dictionary | `src/lisp/prim_runtime_memory_stats.c3` | user-visible `runtime-memory-stats` primitive | top-level `scope`, `scope-transfer`, `fiber-temp`, `copy-to-parent`, `escape-shape`, and `boundary-decisions` dictionaries |
| Runtime memory JSON telemetry | `src/lisp/prim_runtime_memory_stats.c3` | runtime-gated by `OMNI_MEM_TELEMETRY` | JSON snapshot for scope bytes/chunks, scope transfer, fiber temp, copy-to-parent, escape shape, and boundary decisions |
| Copy-to-parent stats | `src/lisp/prim_runtime_memory_stats.c3`, `src/lisp/tests_tests.c3` | `runtime-memory-stats`; summary when `OMNI_BOUNDARY_TRAVERSAL_SUMMARY` is set | total calls, fast reuse hits, defensive copies, copy bytes, tag groups for cons/closure/other, selected copy-site counts, cons spine samples/average/peak |
| Escape-shape stats | `src/lisp/prim_runtime_memory_stats.c3`, `src/lisp/tests_tests.c3` | `runtime-memory-stats`; summary when `OMNI_BOUNDARY_TRAVERSAL_SUMMARY` is set | candidate roots, graph roots, unsafe roots, cons/array/hashmap/set/closure/partial/iterator/tensor roots, cons spine samples/peak |
| Stable-store hook stats | `src/lisp/prim_runtime_memory_stats.c3` | `runtime-memory-stats` | publication hook count, prepared/raw publication hook counts, prepare-failure fallbacks, alias-unsafe skips, resolve failures |

## Existing Summary Lines

| Line family | Source | Gate | Existing fields |
| --- | --- | --- | --- |
| `OMNI_TEST_SUMMARY suite=boundary_decisions` | `src/lisp/tests_tests.c3` | `OMNI_TEST_SUMMARY=1` and counters-enabled build for nonzero instrumentation | splice attempts/success/fail reasons, promotion attempts/abort reasons, fallback total, planned/selected stable publish/materialize/transplant, selected fail-closed, transplant rejection, materialization node/copy bytes, optimizer-addressable materialization copy bytes, forced-no-splice materialization copy bytes, selected stable-materialize counts for cons/array/hashmap/set/closure/big-integer, materialization copy bytes for those tags |
| `OMNI_TEST_SUMMARY suite=boundary_traversal` | `src/lisp/tests_tests.c3` | `OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1` | copy totals, fast reuse, defensive copies, copy tag groups, copy-site groups, promoted-then-spliced, promoted-then-fallback-copied, cons spine samples/average/peak |
| `OMNI_TEST_SUMMARY suite=boundary_escape_shape` | `src/lisp/tests_tests.c3` | `OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1` | candidate/graph/unsafe roots, root counts by major tag family, cons spine samples/peak |
| `[boundary][telemetry][tests] ...` | `boundary_dump_decision_stats_verbose("tests")` via `src/lisp/tests_tests.c3` | verbose boundary diagnostics path | detailed scope-chain and graph-audit counters consumed by profile scripts |

## Existing Benchmark Lines

| Benchmark suite | Source | Gate | Existing fields |
| --- | --- | --- | --- |
| `boundary_decision_cost` | `src/lisp/tests_memory_lifetime_boundary_decision_bench_groups.c3` | `OMNI_BOUNDARY_BENCH=1`, `OMNI_LISP_TEST_SLICE=memory-lifetime-bench` | iterations, splice/disallowed/reuse elapsed milliseconds, correctness counters |
| `boundary_destination_routed_escape` | `src/lisp/tests_memory_lifetime_boundary_decision_bench_groups.c3` | same as above | iterations, partial destination-routed elapsed milliseconds, correctness counter |
| `scope_splice_tail` | `src/lisp/tests_memory_lifetime_boundary_decision_bench_groups.c3` | same as above | iterations, escapes per iteration, splice elapsed milliseconds, correctness counter |
| `equality_nested_workspace` | `src/lisp/tests_memory_lifetime_boundary_decision_bench_groups.c3` | same as above | iterations, fixture state, list depth, array length, true/false timing and correctness for list/array/mixed equality, stack/seen workspace growth counts and bytes |
| `ast_arena_alloc*`, `ast_parser_smoke*`, `ast_compiler_smoke*`, `ast_macro_smoke*` | `src/lisp/tests_memory_lifetime_boundary_ast_bench_groups.c3` | memory benchmark slice families | AST allocation/reset/parser/compiler/macro timing, correctness, arena blocks/capacity/resets, validation failures |

## Existing Scripts

| Script | Current role | Inputs | Outputs / gates |
| --- | --- | --- | --- |
| `scripts/check_boundary_profile_thresholds.sh` | strict envelope for current profiled `memory-lifetime-bench` workload | `build/boundary_profile_memory_lifetime_bench.log` by default | requires boundary benchmark lines, boundary decision summary, and telemetry line; gates correctness, fallback count, splice fail count, timing ceilings, scan suppression, and hint miss ratio |
| `scripts/parse_boundary_profile_summary.sh` | converts current profile log into JSON and derived summaries | profile log path and output JSON path | JSON containing benchmark timings/correctness, boundary decision counters, traversal counters, scope-chain ratios, dominant return path |
| `scripts/run_boundary_profile_regression.sh` | orchestrates boundary profile regression run | benchmark-capable build and validation environment | produces profile log for parser/check scripts |
| `scripts/check_boundary_decision_thresholds.sh` | checks boundary decision summary limits | test output with `boundary_decisions` summary | guards route/copy/splice expectations for current boundary policy |
| `scripts/check_boundary_change_policy.sh` and related boundary policy scripts | static guardrails for boundary-sensitive code | source tree | prevents accidental bypass of boundary facade/policy conventions |

## Missing Signals For `MEM-BENCH-OBSERVE-002`

These gaps are the highest-value counter additions because they turn future
optimization proposals into measured decisions.

| Gap | Why it matters | Suggested owner surface |
| --- | --- | --- |
| TEMP/ESCAPE slow-path allocation requested size | distinguishes real large allocations from chunk-policy artifacts | scope-region allocator counters |
| selected chunk capacity by lane | needed before changing chunk size classes or paging policy | scope-region allocator counters |
| unused bytes at scope reset/release | measures fragmentation/slack instead of only allocated bytes | scope release/reset accounting |
| peak live chunks by lane | identifies scope pressure independent of total bytes | scope telemetry stats |
| scope freelist fresh allocation vs recycle hits | needed before per-thread pool work | scope lifecycle stats |
| fiber-temp take/return/drop by chunk capacity | required before size-class or pooling changes | fiber-temp pool stats |
| array length/capacity at construction and growth | evidence for or against small-array payload optimization | array constructor/push helpers |
| hashmap requested capacity/live count/growth | evidence for dictionary representation work | hashmap constructor/set helpers |
| set member count/growth | detects set-specific pressure now that prepared set graphs exist | set helpers |
| closure env depth and binding count at boundary | distinguishes closure-copy hotspots from payload-copy hotspots | env copy and closure commit helpers |
| string/error/big-number payload byte sizes crossing boundaries | separates heap-scalar payload pressure from node-count pressure | value-specific boundary copy/materialization helpers |
| tensor and FFI wrapper crossings with release authority class | verifies opaque wrapper churn without implying graph traversal | tensor/FFI handle constructors and boundary policy helpers |
| stable passport invalidation reason counts | shows why prepared publication falls back | stable store/passport validation |

## Current Coverage Interpretation

The existing observability is strong for route choice, copy debt, selected
container root families, current benchmark timing, and the low-overhead
allocator/value-shape counter families landed by `MEM-BENCH-OBSERVE-002`.
Those counters cover:

- TEMP/ESCAPE slow-path request bytes and selected chunk bytes.
- TEMP/ESCAPE peak live chunks and slack bytes observed at reset/destroy.
- Scope fresh allocation vs freelist reuse hits.
- Fiber-temp take/return/drop chunk bytes, including context-local bytes.
- Array/hashmap/set construction and growth capacity totals/peaks.
- Closure env-copy frame depth and binding-count totals/peaks.
- String, error, BigInteger decimal-input, and tensor payload bytes.
- FFI wrapper ownership/release-authority classes.
- Stable passport invalidation reason counts.

The remaining gap is benchmark coverage: the runtime now has fields to report,
but `memory-lifetime-bench` still needs workloads that exercise those fields
with stable `OMNI_BENCH_SUMMARY` output.

Therefore `MEM-BENCH-OBSERVE-003` should expand workloads next instead of
adding more counters first.

## Negative-Memory Constraints

- Do not treat destination-arena proposal text as current implementation truth
  when it conflicts with `memory/CHANGELOG.md` or `docs/areas/memory-runtime.md`.
- Do not remove the expected no-splice closure rollback coverage bucket to make
  copy-debt counters look cleaner.
- Do not add strict wall-clock failure gates for new workloads until repeated
  bounded-container runs establish stable timing.
