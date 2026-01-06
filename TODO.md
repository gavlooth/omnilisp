## Phase 13: Region-Based Reference Counting (RC-G) Refactor

Replace hybrid memory management with a unified Region-RC architecture.

- [R] Label: T-rcg-arena
  Objective: Integrate `tsoding/arena` as the physical allocator backend.
  Where: `third_party/arena/arena.h`, `runtime/src/memory/arena_core.c`
  What to change:
    - [ ] Download `arena.h` from `tsoding/arena`.
    - [ ] Rename internal `Region` struct to `ArenaChunk` to avoid conflicts.
    - [ ] Create `arena_core.c` wrapper exposing `arena_alloc/free`.
  How to verify: Compile `arena_core.c` and run a simple allocation test.
  Acceptance:
    - `arena.h` present in `third_party`.
    - `arena_alloc` works for bump allocation.

- [TODO] Label: T-rcg-region
  Objective: Implement the Logical `Region` Control Block.
  Where: `runtime/src/memory/region_core.h`, `runtime/src/memory/region_core.c`
  What to change:
    - [ ] Define `Region` struct with `arena`, `external_rc`, `tether_count`, `scope_alive`.
    - [ ] Implement `region_create()` and `region_destroy_if_dead()`.
  How to verify: Unit test creating a region, retaining/releasing, and verifying destroy is called at 0.
  Acceptance:
    - `Region` struct defined.
    - Lifecycle functions managed via RC/Liveness flags.

- [TODO] Label: T-rcg-ref
  Objective: Implement `RegionRef` fat pointer and atomic ops.
  Where: `runtime/include/omni_types.h`, `runtime/src/memory/region_core.c`
  What to change:
    - [ ] Define `RegionRef { void* ptr; Region* region; }`.
    - [ ] Implement atomic `region_retain` and `region_release`.
  How to verify: Multi-threaded test incrementing/decrementing refcounts.
  Acceptance:
    - Thread-safe RC operations.
    - Integration with `region_destroy_if_dead`.

- [TODO] Label: T-rcg-transmigrate
  Objective: Implement Adaptive Transmigration (Deep Copy + Promotion).
  Where: `runtime/src/memory/transmigrate.c`
  What to change:
    - [ ] Implement `transmigrate(obj, dest_region)` using `uthash` for forwarding.
    - [ ] Implement deep copy for all `ObjTags`.
    - [ ] Add adaptive logic: >4KB copies trigger "Arena Promotion".
  How to verify: Test copying cyclic graphs between regions.
  Acceptance:
    - Cycles preserved in copy.
    - Large copies trigger promotion (pointer reuse).

- [TODO] Label: T-rcg-inference
  Objective: Implement Lifetime-Based Region Inference in the compiler.
  Where: `csrc/analysis/region_inference.c`
  What to change:
    - [ ] Build Interaction Graph of variables.
    - [ ] Identify Connected Components (Candidate Regions).
    - [ ] Insert `region_create` at Dominators and `region_destroy` at Post-Dominators.
  How to verify: Inspect compiler output for `region_create/destroy` calls matching variable lifetimes.
  Acceptance:
    - Variables grouped by interaction.
    - Scope boundaries correctly identified.

- [TODO] Label: T-rcg-escape
  Objective: Update Escape Analysis to trigger Transmigration.
  Where: `csrc/analysis/escape.c`
  What to change:
    - [ ] Identify `ESCAPE_TO_CALLER` variables.
    - [ ] Emit `transmigrate` call when returning local region data.
  How to verify: Compile function returning a local list; verify `transmigrate` is emitted.
  Acceptance:
    - Escaping locals are copied.
    - Non-escaping locals are freed with region.

- [TODO] Label: T-rcg-codegen-lifecycle
  Objective: Emit C code for Region lifecycle management.
  Where: `csrc/codegen/codegen.c`
  What to change:
    - [ ] Emit `Region* _local_region = region_create();` at function start.
    - [ ] Pass `_local_region` to all constructors (`mk_pair`).
    - [ ] Emit `region_exit` and `region_destroy` at function end.
  How to verify: Generated C code compiles and runs without leaks.
  Acceptance:
    - All allocations use local region.
    - Region cleanup emitted on all paths.

- [TODO] Label: T-rcg-codegen-tether
  Objective: Emit Tethering for RegionRef arguments.
  Where: `csrc/codegen/codegen.c`
  What to change:
    - [ ] For `RegionRef` args, emit `region_tether_start` at entry.
    - [ ] Emit `region_tether_end` at exit.
  How to verify: Inspect generated code for tether calls.
  Acceptance:
    - Arguments are tethered during function execution.

- [TODO] Label: T-rcg-cleanup
  Objective: Remove obsolete runtime components.
  Where: `runtime/src/memory/`
  What to change:
    - [ ] Delete `scc.c`, `tarjan.c`, `component.c`.
    - [ ] Remove `scc_id` from `Obj` struct.
  How to verify: `make clean && make test` (full regression suite).
  Acceptance:
    - Codebase compiles without old cycle detector.
    - All tests pass with new Region-RC runtime.