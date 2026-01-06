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

- [R] Label: T-rcg-arena
  Objective: Integrate `tsoding/arena` as the physical allocator backend.
  Where: `third_party/arena/arena.h`, `runtime/src/memory/arena_core.c`
  What to change:
    - [x] Download `arena.h` from `tsoding/arena`.
    - [x] Rename internal `Region` struct to `ArenaChunk` to avoid conflicts.
    - [x] Create `arena_core.c` wrapper exposing `arena_alloc/free`.
  How to verify: Compile `arena_core.c` and run a simple allocation test.
  Acceptance:
    - `arena.h` present in `third_party`.
    - `arena_alloc` works for bump allocation.

- [R] Label: T-rcg-region
  Objective: Implement the Logical `Region` Control Block.
  Where: `runtime/src/memory/region_core.h`, `runtime/src/memory/region_core.c`
  What to change:
    - [ ] Define `Region` struct:
      ```c
      typedef struct Region {
          struct Arena* arena;       // Physical storage
          _Atomic int external_rc;   // Strong refs from OTHER regions/stack
          _Atomic int tether_count;  // Temporary "borrows" by threads
          bool scope_alive;          // True if semantic scope active
      } Region;
      ```
    - [ ] Implement `region_create()`: Mallocs Region, init arena, sets rc=0, scope_alive=true.
    - [ ] Implement `region_destroy_if_dead(Region* r)`:
      - Logic: `if (!r->scope_alive && r->external_rc == 0 && r->tether_count == 0)` -> free arena and r.
  How to verify: Unit test creating a region, retaining/releasing, and verifying destroy is called at 0.
  Acceptance:
    - `Region` struct defined.
    - Lifecycle functions managed via RC/Liveness flags.

- [R] Label: T-rcg-ref
  Objective: Implement `RegionRef` fat pointer and atomic ops.
  Where: `runtime/include/omni_types.h`, `runtime/src/memory/region_core.c`
  What to change:
    - [ ] Define `RegionRef { void* ptr; Region* region; }`.
    - [ ] Implement atomic `region_retain(RegionRef ref)`: `ref.region->external_rc++`.
    - [ ] Implement atomic `region_release(RegionRef ref)`: `ref.region->external_rc--`. If 0, call `region_destroy_if_dead`.
  How to verify: Multi-threaded test incrementing/decrementing refcounts.
  Acceptance:
    - Thread-safe RC operations.
    - Integration with `region_destroy_if_dead`.

- [TODO] Label: T-rcg-transmigrate
  Objective: Implement Adaptive Transmigration (Deep Copy + Promotion).
  Where: `runtime/src/memory/transmigrate.c`
  What to change:
    - [ ] Implement `transmigrate(void* root, Region* dest_region)`:
      - Use `uthash` for `Map<void* old_ptr, void* new_ptr>` to handle cycles.
      - Recursively walk graph based on `obj->tag` (enum `ObjTag` in `runtime/src/runtime.h`).
      - Deep copy primitives and recursively copy children.
    - [ ] Add adaptive logic: if `copied_bytes > 4096`, abort copy and perform **Arena Promotion** (append source arena blocks to destination).
  How to verify: Test copying cyclic graphs between regions.
  Acceptance:
    - Cycles preserved in copy.
    - Large copies trigger promotion (pointer reuse).

- [TODO] Label: T-rcg-inference
  Objective: Implement Advanced Lifetime-Based Region Inference.
  Where: `csrc/analysis/region_inference.c`
  What to change:
    - [ ] Build **Interaction Graph**: edge `(u, v)` if `v = car(u)` or `f(u, v)`.
    - [ ] Find **Connected Components** (Candidate Regions).
    - [ ] **Placement**:
      - Find **Dominator Block** for definitions -> insert `region_create`.
      - Find **Post-Dominator** (or latest Liveness Kill) -> insert `region_destroy`.
    - [ ] Optimize loops: inner regions reset per iteration.
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

- [TODO] Label: T-rcg-cleanup
  Objective: Remove obsolete runtime components.
  Where: `runtime/src/memory/`
  What to change:
    - [ ] Delete `scc.c`, `tarjan.c`, `component.c`.
    - [ ] Modify `Obj` struct: Remove `int scc_id`, `int scan_tag`.
  How to verify: `make clean && make test` (full regression suite).
  Acceptance:
    - Codebase compiles without old cycle detector.
    - All tests pass with new Region-RC runtime.
