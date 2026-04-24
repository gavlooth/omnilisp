# Active TODO Index

`TODO.md` remains the canonical backlog entrypoint; read the indexed part files for the full live queue and recently closed history.

The historical content was split mechanically to keep the active backlog
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
- Part 17: [docs/todo_parts/todo_part_17.md](docs/todo_parts/todo_part_17.md) (75 lines)
- Part 18: [docs/todo_parts/todo_part_18.md](docs/todo_parts/todo_part_18.md) (138 lines)

## Live Queue

- The memory boundary architecture verification blockers from
  `docs/plans/memory-boundary-architecture-spec-2026-04-24.md` are closed in
  Part 18.
- The proof-driven memory-boundary optimizer roadmap in
  `docs/plans/memory-boundary-proof-planner-roadmap-2026-04-24.md` is closed in
  Part 18; the next memory-boundary work should be selected from measured
  route/copy-debt telemetry.
- No open stable-escape prepared-materialization rollout items remain in Part 16.
- Recent closed stable-escape rollout history in Part 16 includes prepared
  `CONS`/`ARRAY`/dictionary/set/closure graph metadata, cyclic container
  back-edges, prepared-publication fallback observability, mutation-drift
  invalidation, explicit commit-route flags, first TEMP cons stable
  materialization, prepared root/container materialization for arrays,
  dictionaries, sets, closures, and cloned heap-backed scalar/signature payloads.
- Open Vulkan/CUDA/ML audit residuals from
  `AUDIT_REPORT_VULKAN_CUDA_ML_2026-04-23.md` are tracked in Part 17.
