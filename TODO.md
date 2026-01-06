# OmniLisp TODO

## Review Directive

**All newly implemented features must be marked with `[R]` (Review Needed) until explicitly approved by the user.**

- When an agent completes implementing a feature, mark it `[R]` (not `[DONE]`)
- `[R]` means: code is written and working, but awaits user review/approval
- After user approval, change `[R]` to `[DONE]`
- Workflow: `[TODO]` → implement → `[R]` → user approves → `[DONE]`

This ensures no feature is considered "done" until a human has reviewed it.

---

## Phase 13: Region-Based Reference Counting (RC-G) Refactor

Replace hybrid memory management with a unified Region-RC architecture.

- [DONE] Label: T-rcg-arena
  Objective: Integrate `tsoding/arena` as the physical allocator backend.
  Where: `third_party/arena/arena.h`, `runtime/src/memory/arena_core.c`
  What to change:
    - [x] Download `arena.h` from `tsoding/arena`.
    - [x] Rename internal `Region` struct to `ArenaChunk` to avoid conflicts.
    - [x] Integrate arena into `region_core.c`.
  How to verify: Compile `region_core.c` and run allocation tests.
  Acceptance:
    - `arena.h` present in `third_party`.
    - `arena_alloc` works for bump allocation.

- [DONE] Label: T-rcg-region
  Objective: Implement the Logical `Region` Control Block.
  Where: `runtime/src/memory/region_core.h`, `runtime/src/memory/region_core.c`
  What to change:
    - [x] Define `Region` struct:
      ```c
      typedef struct Region {
          Arena arena;                // Physical storage (bump allocator)
          int external_rc;            // Strong refs from OTHER regions/stack (atomic)
          int tether_count;           // Temporary "borrows" by threads (atomic)
          bool scope_alive;           // True if the semantic scope is still active
      } Region;
      ```
    - [x] Implement `region_create()`: Mallocs Region, init arena, sets rc=0, scope_alive=true.
    - [x] Implement `region_destroy_if_dead(Region* r)`:
      - Logic: `if (!r->scope_alive && r->external_rc == 0 && r->tether_count == 0)` -> free arena and r.
    - [x] Implement `region_exit()`, `region_tether_start/end()`, `region_alloc()`.
  How to verify: Unit test creating a region, retaining/releasing, and verifying destroy is called at 0.
  Acceptance:
    - `Region` struct defined.
    - Lifecycle functions managed via RC/Liveness flags.

- [DONE] Label: T-rcg-ref
  Objective: Implement `RegionRef` fat pointer and atomic ops.
  Where: `runtime/src/memory/region_core.h`, `runtime/src/memory/region_core.c`
  What to change:
    - [x] Define `RegionRef { void* ptr; Region* region; }`.
    - [x] Implement atomic `region_retain(RegionRef ref)`: `ref.region->external_rc++`.
    - [x] Implement atomic `region_release(RegionRef ref)`: `ref.region->external_rc--`. If 0, call `region_destroy_if_dead`.
  How to verify: Multi-threaded test incrementing/decrementing refcounts.
  Acceptance:
    - Thread-safe RC operations.
    - Integration with `region_destroy_if_dead`.

- [DONE] Label: T-rcg-transmigrate
  Objective: Implement Adaptive Transmigration (Deep Copy + Promotion).
  Where: `runtime/src/memory/transmigrate.c`, `runtime/src/memory/transmigrate.h`
  What to change:
    - [x] Implement `transmigrate(void* root, Region* dest_region)`:
      - Use `uthash` for `Map<void* old_ptr, void* new_ptr>` to handle cycles.
      - Recursively walk graph based on `obj->tag` (enum `Tag` in `src/runtime/types.h`).
      - Deep copy primitives and recursively copy children.
    - [x] Handle all Value types: T_CELL, T_INT, T_NIL, T_STRING, T_SYM, T_LAMBDA, T_BOX, T_CONT, T_PROCESS, etc.
    - [x] Add adaptive logic: if `copied_bytes > 4096`, abort copy and perform **Arena Promotion** (append source arena blocks to destination).
  How to verify: Test copying cyclic graphs between regions.
  Acceptance:
    - Cycles preserved in copy.
    - All Value types handled correctly.

- [DONE] Label: T-rcg-constructors
  Objective: Implement Region-Aware Value Constructors.
  Where: `runtime/src/memory/region_value.h`, `runtime/src/memory/region_value.c`
  What to change:
    - [x] Create `region_value.h` with declarations for all `mk_*_region` functions.
    - [x] Implement `region_value.c` with all region-aware constructors.
    - [x] Handle all Value types: scalars, strings, cells, lambdas, boxes, etc.
    - [x] Integrate with `region_alloc()` for all allocations.
  How to verify: Unit test creating various values in a region.
  Acceptance:
    - All `mk_*_region` functions work correctly.
    - Values allocated in region are freed on `region_exit()`.
    - String data also allocated in region.

- [R] Label: T-rcg-inference
  Objective: Implement Advanced Lifetime-Based Region Inference.
  Where: `csrc/analysis/region_inference.c`
  What to change:
    - [ ] **Step 1: Build Variable Interaction Graph (VIG)**
      - Iterate all instructions in CFG.
      - Add nodes for all variables.
      - Add undirected edge `(u, v)` if:
        - `v` is assigned from `u` (data flow).
        - `u` and `v` are arguments to the same call (aliasing).
        - `v` is a field access of `u` (structural relation).
    - [ ] **Step 2: Find Connected Components**
      - Use Union-Find or BFS.
      - Each Component is a **Candidate Region**.
    - [ ] **Step 3: Liveness Analysis**
      - For each Component `C`:
        - `Start(C)` = Earliest definition point of any var in C.
        - `End(C)` = Latest last-use point of any var in C.
    - [ ] **Step 4: Dominator Placement**
      - Identify the **Dominator Block** for `Start(C)`.
      - Identify the **Post-Dominator** for `End(C)`.
      - Store these locations in `RegionInfo` struct.
  How to verify: Inspect compiler output for `region_create/destroy` calls matching variable lifetimes.
  Acceptance:
    - Variables grouped by interaction.
    - Scope boundaries correctly identified.

- [TODO] Label: T-rcg-escape
  Objective: Update Escape Analysis to trigger Transmigration.
  Where: `csrc/analysis/escape.c`
  What to change:
    - [ ] Identify `ESCAPE_TO_CALLER` variables (returned or stored in globals).
    - [ ] Emit `transmigrate(x, caller_region)` call when returning local region data.
  How to verify: Compile function returning a local list; verify `transmigrate` is emitted.
  Acceptance:
    - Escaping locals are copied.
    - Non-escaping locals are freed with region.

- [TODO] Label: T-rcg-codegen-lifecycle
  Objective: Emit C code for Region lifecycle management.
  Where: `csrc/codegen/codegen.c`
  What to change:
    - [ ] Emit `Region* _local_region = region_create();` at function start (or dominator).
    - [ ] Pass `_local_region` to all constructors (`mk_pair`).
    - [ ] Emit `region_exit(_local_region)` (sets `scope_alive = false`) and `region_destroy_if_dead` at exit.
  How to verify: Generated C code compiles and runs without leaks.
  Acceptance:
    - All allocations use local region.
    - Region cleanup emitted on all paths.

- [TODO] Label: T-rcg-codegen-tether
  Objective: Emit Tethering for RegionRef arguments.
  Where: `csrc/codegen/codegen.c`
  What to change:
    - [ ] For `RegionRef` args, emit `region_tether_start(arg.region);` at entry.
    - [ ] Emit `region_tether_end(arg.region);` at exit.
  How to verify: Inspect generated code for tether calls.
  Acceptance:
    - Arguments are tethered during function execution.

- [DONE] Label: T-rcg-cleanup
  Objective: Remove obsolete runtime components.
  Where: `runtime/src/memory/`, `src/runtime/memory/`, `runtime/src/runtime.c`
  What to change:
    - [x] Delete `runtime/src/memory/scc.c`, `scc.h`, `component.c`, `component.h`.
    - [x] Delete `src/runtime/memory/scc.c`, `scc.h`.
    - [x] Modify `Obj` struct in `runtime/src/runtime.c`: Remove `int scc_id`, `int scan_tag`.
    - [x] Remove all code references to `scc_id` and `scan_tag`.
    - [x] Remove `#include "memory/component.h"` from `runtime/src/runtime.c`.
    - [x] Update `runtime/src/memory/arena.c` to remove SCC field references.
  How to verify: `make clean && make test` (full regression suite).
  Acceptance:
    - [x] Codebase compiles without old cycle detector.
    - [x] All RCG tests pass with new Region-RC runtime.
  Note: `csrc/analysis/scc.c` and `component.c` are for compiler CFG analysis (static analysis), not runtime GC, so they remain.
