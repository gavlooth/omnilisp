# Refactoring Plan: Implementing Region-Based RC (RC-G)

**Target Audience:** C Developer (Competent) | **Prerequisites:** None (Codebase/MemMgmt knowledge not assumed)
**Goal:** Replace the current hybrid memory management (Refcounting + Tarjan Cycle Detection) with a unified Region-Based system.

---

## Phase 1: Runtime Foundation (The "Storage" Layer)

**Objective:** Implement the physical storage (`Arena`) and the logical controller (`Region`).

### 1.1 Create the Arena (Physical Allocator)
**File:** `third_party/arena/arena.h` (Vendored)
**Concept:** Use the single-header library `tsoding/arena`.

**Task:**
1.  **Download:** Fetch `arena.h` from `https://github.com/tsoding/arena`.
2.  **Rename:** The library uses the name `Region` for its blocks. Rename this to `ArenaChunk` inside the file to avoid conflict with our Logical `Region` struct.
3.  **Integrate:** Include this file in `runtime/src/memory/arena_core.c`.
4.  **API:** Use `arena_alloc` and `arena_free` directly.

*Note: This replaces the need to write a custom linked-list allocator.*

### 1.2 Create the Region Control Block (Logical Owner)
**File:** `runtime/src/memory/region_core.h` (New)
**Concept:** A "Region" owns an Arena. It also tracks how many people are looking at it (`rc`).

**Struct Definition:**
```c
typedef struct Region {
    struct Arena* arena;       // The physical storage
    _Atomic int external_rc;   // Strong refs from OTHER regions/stack
    _Atomic int tether_count;  // Temporary "borrows" by threads
    bool scope_alive;          // True if the semantic scope is still active
} Region;
```

**Task:**
1.  Implement `region_create()`: Mallocs a `Region`, initializes `arena_create()`, sets `rc=0`, `scope_alive=true`.
2.  Implement `region_destroy_if_dead(Region* r)`:
    *   **Logic:** `if (!r->scope_alive && r->external_rc == 0 && r->tether_count == 0)` -> Call `arena_destroy(r->arena)` and `free(r)`.

### 1.3 The "Fat Pointer" (RegionRef)
**File:** `runtime/include/omni_types.h`
**Concept:** A standard pointer `Obj*` isn't safe enough anymore. We need to know which Region it lives in to keep that Region alive.

**Task:**
1.  Define:
    ```c
    typedef struct RegionRef {
        void* ptr;      // The actual object
        Region* region; // The region keeping it alive
    } RegionRef;
    ```
2.  Implement `region_retain(RegionRef ref)`: atomic increment `ref.region->external_rc`.
3.  Implement `region_release(RegionRef ref)`: atomic decrement `ref.region->external_rc`. If 0, call `region_destroy_if_dead`.

---

## Phase 2: The Transmigrator (The "Mover")

**Objective:** Move objects from a dying region to a living one.
**File:** `runtime/src/memory/transmigrate.c`

### 2.1 The "Forwarding Map"
**Concept:** When we copy a graph (A->B, A->C, B->C), we visit C twice. We need to know we already copied C so we don't duplicate it.

**Task:**
1.  Use the existing `uthash` or `stb_ds` (in `third_party/`) to create a map: `Map<void* old_ptr, void* new_ptr>`.

### 2.2 The Deep Copy Algorithm
**Task:** Implement `void* transmigrate(void* root, Region* dest_region)`.

**Algorithm:**
```text
function copy(obj, map, dest_region):
  if obj is NULL: return NULL
  if obj in map: return map[obj] // Already copied

  // 1. Allocate space in DESTINATION region
  new_obj = region_alloc(dest_region, sizeof(obj))
  
  // 2. Register in map to handle cycles
  map[obj] = new_obj

  // 3. Copy primitive fields
  memcpy(new_obj, obj, sizeof(obj))

  // 4. Recursively copy children
  // (You need a switch statement on obj->type)
  if obj is PAIR:
     new_obj->car = copy(obj->car, map, dest_region)
     new_obj->cdr = copy(obj->cdr, map, dest_region)
  
  return new_obj
```

### 2.3 The "Adaptive" Logic (Optimization)
**Task:** Add a byte counter to the map context.
*   If `copied_bytes > 4096`:
    *   **Abort** the copy.
    *   **Promote:** Take the *entire* `ArenaBlock` list from the Source Region and append it to the Destination Region's list.
    *   Return the original pointer (it's now valid in the new region).

---

## Phase 3: Compiler Analysis (The "Brain")

**Objective:** Teach the compiler to insert `region_create` and `region_destroy`.
**File:** `csrc/analysis/region_inference.c` (New)

### 3.1 Advanced Region Inference (Lifetime-Based)
**Concept:** Instead of one region per function (which hurts concurrency/memory), we infer the *minimal* regions needed.

**Algorithm:**
1.  **Interaction Graph:** Build a graph where nodes are Variables. Add an edge `(u, v)` if:
    *   `v` is assigned from `u` (e.g., `v = car(u)`).
    *   `u` and `v` are arguments to the same function call `f(u, v)` (aliasing potential).
2.  **Connected Components:** Find connected components in this graph. Each component is a **Candidate Region**.
3.  **Placement:** For each Component:
    *   Find the **Dominator Block** (the earliest point in the CFG that dominates all variable definitions in the component).
    *   Insert `region_create()` there.
    *   Find the **Post-Dominator** (or latest Liveness Kill point) for all variables.
    *   Insert `region_destroy()` there.
4.  **Result:** Loop variables get their own inner regions (reset per iteration). Independent tasks get separate regions (parallel-safe).

### 3.2 Escape Analysis Integration
**Task:** Update `csrc/analysis/escape.c`.
1.  Identify variables that are returned or stored in globals.
2.  **Mark them:** `ESCAPE_TO_CALLER`.
3.  **Action:** When generating code for `return x`:
    *   If `x` is `ESCAPE_TO_CALLER` and `x` is in the Local Region -> Emit `transmigrate(x, caller_region)`.

---

## Phase 4: Code Generation (The "Output")

**Objective:** Emit the C code that uses Phase 1 & 2.
**File:** `csrc/codegen/codegen.c`

### 4.1 Emit Region Lifecycle
**Task:**
1.  At function start: `Region* _local_region = region_create();`
2.  For every allocation `mk_pair(...)`, change to `mk_pair(_local_region, ...)` (passing the allocator).
3.  At function end (all return paths):
    *   `region_exit(_local_region);` (Sets `scope_alive = false`).
    *   `region_destroy_if_dead(_local_region);`

### 4.2 Emit Tethers
**Task:**
1.  If a function takes a `RegionRef` argument:
    *   Emit `region_tether_start(arg.region);` at entry.
    *   Emit `region_tether_end(arg.region);` at exit.
    *   This ensures the region doesn't die while the function runs.

---

## Phase 5: Cleanup & Verification

**Objective:** Remove the old complex systems.

**Task:**
1.  **Delete:** `runtime/src/memory/scc.c`, `tarjan.c`, `component.c`.
2.  **Modify:** `Obj` struct. Remove `int scc_id`, `int scan_tag`.
3.  **Verify:** Run `make test`.
    *   Expect *compilation errors* everywhere.
    *   Fix constructor calls to take `Region*`.
    *   Fix free calls to be removed (handled by Region).

---

## Self-Critique & Review

### Is this enough for a generic competent C dev?
*   **Structs:** Yes, explicitly defined.
*   **Algorithms:** "Deep Copy" and "Bump Alloc" are standard, but the pseudocode helps.
*   **Context:** The distinction between "Physical Arena" and "Logical Region" is crucial and emphasized.

### Missing Details?
*   **Type Switch:** In Phase 2.2, I said "switch statement on obj->type". A novice might not know *where* the type tag is.
    *   *Correction:* It's `obj->tag`. The enum `ObjTag` is in `runtime/src/runtime.h`.
*   **Forwarding Map:** Implementing a hash map in C is annoying.
    *   *Mitigation:* Point explicitly to `third_party/uthash/uthash.h` which is already in the repo.

### Complexity Check
*   **Phase 3 (Compiler)** is the hardest. "Region Inference" can get very math-heavy (Unification-based analysis).
    *   *Simplification:* I suggested "One Region per Function Scope" as a starter. This is basically "Stack Allocation" but safe for cycles. This is a valid MVP.

### Final Verdict
The plan is actionable. The "One Region per Function" simplification in Phase 3 is the key to making this achievable for a newcomer without a PhD in Static Analysis.
