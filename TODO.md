# TODO Index

`TODO.md` remains the canonical backlog entrypoint; read the indexed part files for the full live queue and recently closed history.

The historical content was split mechanically to keep the backlog index
readable. Content order is preserved in the part files. The hard 1000-line
split gate applies to code files only.

## Parts

- Part 01: [docs/todo_parts/todo_part_01.md](docs/todo_parts/todo_part_01.md) (902 lines)
- Part 02: [docs/todo_parts/todo_part_02.md](docs/todo_parts/todo_part_02.md) (675 lines)
- Part 03: [docs/todo_parts/todo_part_03.md](docs/todo_parts/todo_part_03.md) (322 lines)
- Part 04: [docs/todo_parts/todo_part_04.md](docs/todo_parts/todo_part_04.md) (432 lines)
- Part 05: [docs/todo_parts/todo_part_05.md](docs/todo_parts/todo_part_05.md) (352 lines)
- Part 06: [docs/todo_parts/todo_part_06.md](docs/todo_parts/todo_part_06.md) (317 lines)
- Part 07: [docs/todo_parts/todo_part_07.md](docs/todo_parts/todo_part_07.md) (318 lines)
- Part 08: [docs/todo_parts/todo_part_08.md](docs/todo_parts/todo_part_08.md) (319 lines)
- Part 09: [docs/todo_parts/todo_part_09.md](docs/todo_parts/todo_part_09.md) (315 lines)
- Part 10: [docs/todo_parts/todo_part_10.md](docs/todo_parts/todo_part_10.md) (328 lines)
- Part 11: [docs/todo_parts/todo_part_11.md](docs/todo_parts/todo_part_11.md) (318 lines)
- Part 12: [docs/todo_parts/todo_part_12.md](docs/todo_parts/todo_part_12.md) (337 lines)
- Part 13: [docs/todo_parts/todo_part_13.md](docs/todo_parts/todo_part_13.md) (325 lines)
- Part 14: [docs/todo_parts/todo_part_14.md](docs/todo_parts/todo_part_14.md) (1915 lines)
- Part 15: [docs/todo_parts/todo_part_15.md](docs/todo_parts/todo_part_15.md) (951 lines)
- Part 16: [docs/todo_parts/todo_part_16.md](docs/todo_parts/todo_part_16.md) (261 lines)
- Part 17: [docs/todo_parts/todo_part_17.md](docs/todo_parts/todo_part_17.md) (149 lines)
- Part 18: [docs/todo_parts/todo_part_18.md](docs/todo_parts/todo_part_18.md) (798 lines)

## Live Queue

- Current actionable count: 10.
- `MEM-PROOF-001` through `MEM-PROOF-010` in Part 18 are open under
  `docs/plans/memory-model-proof-matrix-2026-04-26.md`. This lane applies the
  same proof/measurement/hardening treatment to the entire memory model:
  inventory/manifest coverage, ScopeRegion core, value constructors,
  env/closure, boundary commit routes, stable escape/transplant,
  collections/mutation, native tensor/device paths, async/scheduler/callbacks,
  and FFI ScopeRegion migration closure.
- `MEM-MODEL-IMPROVE-002` in Part 18 is closed under
  `docs/plans/memory-model-improvement-plan-2026-04-25.md`.
  The slice added slow-path slack histograms, per-scope slow-allocation
  sequence telemetry, request/unused buckets, and source/site attribution.
  Direct ESCAPE and direct TEMP chunk-size policy attempts are invalidated by
  measured counters; broad TEMP large-slack reduction is also invalidated.
  The final attribution run shows all remaining ESCAPE no-follow-up sequences
  come from the synthetic direct allocator probe
  (`escape_slow_sequence_no_followup_source_direct_delta=256`), not runtime
  boundary/promotion sources, so no allocator policy change is justified by
  the current profile.
- `MEM-MODEL-IMPROVE-003` in Part 18 is closed. Shared Dictionary/Set
  known-entry capacity sizing now eliminates the benchmark hashmap/set growth
  counters (`hashmap_growth_delta=0`, `set_growth_delta=0`) while keeping
  checked constructors and insertion paths.
- `MEM-MODEL-IMPROVE-004` in Part 18 is closed. Boundary value policy coverage
  now has a manifest-backed guard wired into `check_boundary_change_policy.sh`
  so every `ValueTag` must declare ownership, edge, copy-route,
  materialization, graph-audit, destructor, native/FFI, and rollback policy.
- `MEM-MODEL-IMPROVE-005` in Part 18 is closed. `atomic-ref` is the first
  explicit FFI bridge keepalive family, FFI wrapper copy now fails closed for
  declared traversal/unsafe modes, and public FFI metadata validation is green.
- `MEM-MODEL-IMPROVE-006` in Part 18 is closed. Product-style Finwatch,
  closure-heavy iterator pipeline, tensor-metadata crossing, and nested-module
  return benchmark slices are landed and envelope-checked. The nested-module
  slice also fixed stable materialization fallback after transplant proof
  rejection for stable graph returns.
- `AUDIT-238-CONTINUATION-IGNORE-K-TEMP-EDGE` in Part 18 is closed; the
  ignore-k continuation retention failure is fixed and `memory-lifetime-smoke`
  is green again.
- `AUDIT-239-ENV-COPY-DTOR-REGISTRATION` in Part 18 is closed; env-copy frame
  destructor-registration OOM now fails closed with a typed boundary fault
  instead of returning an unmanaged copied frame.
- `AUDIT-240-DESTINATION-ERROR-ESCAPE-DTOR` in Part 18 is closed; destination
  error escape building now fails closed if ESCAPE destructor registration
  cannot be recorded after allocating the copied error string.
- `AUDIT-241-STABLE-MATERIALIZED-CLOSURE-DTOR` in Part 18 is closed; stable
  destination materialized closures now fail closed if their closure-specific
  env-scope destructor cannot be recorded.
- The `MEM-BENCH-OBSERVE-001` through `MEM-BENCH-OBSERVE-005` memory-boundary
  telemetry evidence lane in Part 18 is closed. The lane produced a signal
  inventory, runtime counter coverage, benchmark workload coverage, first
  counter baseline, and regression-envelope parser.
- The memory boundary architecture verification blockers from
  `docs/plans/memory-boundary-architecture-spec-2026-04-24.md` are closed in
  Part 18.
- The proof-driven memory-boundary optimizer roadmap in
  `docs/plans/memory-boundary-proof-planner-roadmap-2026-04-24.md` is closed in
  Part 18 through planner-owned commit migration, tag attribution, `CONS`
  copy-debt reduction, closure copy-debt reduction, array copy-debt reduction,
  closure residual classification, and BigInteger copy-debt reduction.
  Counters-enabled `memory-lifetime-smoke` now reports no
  optimizer-addressable stable-materialization copy bucket; the remaining
  `208` copied bytes are expected no-splice closure rollback coverage.
- No open stable-escape prepared-materialization rollout items remain in Part 16.
- Recent closed stable-escape rollout history in Part 16 includes prepared
  `CONS`/`ARRAY`/dictionary/set/closure graph metadata, cyclic container
  back-edges, prepared-publication fallback observability, mutation-drift
  invalidation, explicit commit-route flags, first TEMP cons stable
  materialization, prepared root/container materialization for arrays,
  dictionaries, sets, closures, and cloned heap-backed scalar/signature payloads.
- Vulkan/CUDA/ML audit residuals from
  `AUDIT_REPORT_VULKAN_CUDA_ML_2026-04-23.md` are closed in Part 17.
