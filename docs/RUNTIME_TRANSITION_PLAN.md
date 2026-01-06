# Runtime Transition Plan: Old -> RC-G

## 1. Current Status
*   **New Runtime:** Ready (`region_core`, `transmigrate`, `arena`).
*   **Old Runtime:** Active (`runtime.c`, `dec_ref`, `free_tree`).
*   **Wiring:** **Disconnected.** The compiler generates calls to Old Runtime.

## 2. The "Big Swap" Strategy

We cannot replace it atomicly in one commit without breaking everything. We will use a **Shim Strategy**.

### Phase A: The Constructor Swap (Codegen Update)
**Goal:** Make all allocations go to Regions.

1.  **Update Codegen:** Modify `csrc/codegen/codegen.c`.
2.  **Inject Region:** At function start, emit `Region* _r = region_create();`.
3.  **Swap Constructors:**
    *   Old: `mk_int(42)`
    *   New: `mk_int_region(_r, 42)`
4.  **Result:** All new objects live in Regions.

### Phase B: The Destructor Swap (Runtime Shim)
**Goal:** Make old `free` calls trigger Region logic.

1.  **Modify `free_tree` / `dec_ref`:**
    *   Since objects in Regions don't have individual refcounts, `dec_ref` becomes a no-op for Region-resident objects (or checks the Region header).
    *   *Correction:* We decided **Everything is a Region**.
2.  **Shim:**
    *   Redefine `Obj` to have a `Region*` pointer? No, we use `RegionRef`.
    *   The "Old" `Obj*` pointer is now effectively a "Raw Pointer" inside a region.
    *   We need to change the *Compiler* to pass `RegionRef` structs instead of `Obj*` pointers where ownership is shared.

### Phase C: Escape & Transmigrate
**Goal:** Handle return values.

1.  **Update Codegen:** On `return x`:
    *   Emit `transmigrate(x, _r, caller_region)`.
    *   Emit `region_exit(_r)`.

## 3. Execution Steps

1.  **[TODO] T-rcg-codegen-lifecycle:** Implement `region_create` injection.
2.  **[TODO] T-rcg-codegen-constructors:** Switch `mk_` calls to `mk_region_` calls.
3.  **[TODO] T-rcg-escape:** Implement Transmigration emission.
4.  **[TODO] T-rcg-cleanup-old:** Delete `dec_ref` logic from compiler.

## 4. Verdict
The new runtime is **NOT** wired.
We must complete the **Compiler Codegen** tasks (`T-rcg-codegen-*`) to flip the switch.
