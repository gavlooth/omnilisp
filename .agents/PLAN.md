# Agent Plan Index

`.agents/PLAN.md` remains the operational plan entrypoint; read the indexed part files for the full plan state.

The historical content was split mechanically to keep individual files below the former 700-line repository limit. Content order is preserved in the part files. Current code-file split threshold is 1000 LOC from 2026-04-21 onward.

## Parts

- Part 01: [.agents/plan_parts/plan_part_01.md](plan_parts/plan_part_01.md) (341 lines)
- Part 02: [.agents/plan_parts/plan_part_02.md](plan_parts/plan_part_02.md) (624 lines)
- Part 03: [.agents/plan_parts/plan_part_03.md](plan_parts/plan_part_03.md) (353 lines)
- Part 04: [.agents/plan_parts/plan_part_04.md](plan_parts/plan_part_04.md) (395 lines)
- Part 05: [.agents/plan_parts/plan_part_05.md](plan_parts/plan_part_05.md) (758 lines)

## Current Checkpoint

Date: 2026-04-27

- Active hypothesis:
  - There is no active execution plan. The canonical queue is `TODO.md`, and it
    reports `Current actionable count: 0`.
  - Repository status checks are green for memory runtime, types/dispatch, FFI
    foreign runtime, and validation.
- Current approach:
  - Treat `.agents/PLAN.md` and the part files as historical handoff context,
    not as a parallel backlog.
  - Reopen work only by adding a concrete TODO-backed item with owner, scope,
    validation path, and negative-memory constraints.
- Validation path:
  - `scripts/check_status_consistency.sh` is the planning/status consistency
    gate.
  - Use targeted tests plus bounded container validation for future runtime,
    memory, FFI, CUDA/Vulkan, or high-memory work.
- Next checkpoint:
  - None. A new checkpoint is required only when a new TODO-backed work item is
    opened.
- Negative-memory constraints:
  - Do not treat historical plan text, roadmap notes, or old `.agents` entries
    as active work unless `TODO.md` explicitly reopens them.
  - Do not reopen Python/Julia adapters or polyglot/plugin runtime support for
    the FFI foreign-runtime area without a new product decision.
  - Do not reopen allocator policy tuning from old aggregate slack counters;
    require a non-synthetic bounded benchmark signal first.
- Agent assignments:
  - None currently.
