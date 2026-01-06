# Report: Region-RC (Group Borrowing) vs. Current Codebase

## 1. Overview

This report compares the refined **Region-Based Reference Counting (RC-G)** architecture—based on "Group Borrowing" and the "ASAP" philosophy—with the existing memory management mechanisms found in the OmniLisp codebase.

## 2. Comparison Matrix

| Mechanism | Current Implementation | Refined Architecture (RC-G) | Alignment with `GEMINI.md` |
| :--- | :--- | :--- | :--- |
| **Object RC** | Fine-grained `dec_ref(obj)`. Atomic overhead on every shared object. | Coarse-grained `external_rc` on the **Region Control Block**. | **Better**: Reduces atomic operations by orders of magnitude. |
| **Cycle Management** | Complex `release_scc` (Tarjan's) and `sym_release_handle`. | **Bulk Reclamation**. The entire Region is the unit of deallocation. | **Better**: Simple, O(1) bulk free vs. O(N) cycle traversal. |
| **Borrowing** | `BorrowRef` (IPGE/GenRef) for safety. | **Tethers**. Prevents region mutation while content is active. | **Equal**: Both provide UAF safety; Tethers allow better aliasing. |
| **Static Integration** | Mixed. Some RC is manual in runtime; some is injected. | **Strictly ASAP-First**. RC and Tethers are only injected for escaping data. | **Better**: Moves decision-making to compile-time (ASAP). |

## 3. Analysis of Existing Codebase Features

### 3.1 Per-Region External Refcount (Existing)
The codebase already has `external_refcount` in `test_embedded.c` and `region.c`.
*   **Current State:** It is used as a simple gate for bulk-freeing.
*   **Proposed Evolution:** Enhance this into a "Group Aware" RCB that tracks not just pointers, but **Tethers**. This allows the "Group Borrowing" semantics where you can safely mutate a list's "Shell" (metadata) while a thread is reading the "Content" (data) of a specific element.

### 3.2 SCC-Based RC (Existing)
Currently, `scc.c` manages frozen cycles.
*   **Current State:** Requires complex runtime detection (Tarjan's) to find "Islands".
*   **Proposed Evolution:** Shift the responsibility to the **Compiler**. If the compiler knows a set of objects form a cyclic group, it assigns them to the same **Region** at allocation time. The runtime then only needs to track one `external_rc` for the whole Region, eliminating the need for runtime SCC detection.

### 3.3 Component Tethering (Existing)
`component.c` uses `sym_tether_begin/end`.
*   **Current State:** Implements a similar concept to Nick Smith's system but specifically for "Symmetric" components.
*   **Proposed Evolution:** Unify this with standard heap data. All cyclic islands become "Regions" and use the same Tethering API, creating a single, consistent memory model.

## 4. Addressing "ASAP" and Restrictions

The refined architecture solves the "Restriction" problem by moving the complexity to the **Compiler (ASAP)**:

1.  **No Developer Annotations:** The compiler's **Shape Analysis** and **Escape Analysis** (already present in `analysis/`) automatically decide which objects belong to which Region.
2.  **Safe Mutable Aliasing:** By adopting "Group Borrowing" semantics, the runtime allows `render(level.geom)` and `update(level.entities)` to run concurrently on the same Region without the developer needing to manage locks or smart pointers manually.
3.  **Zero-Cost Local Path:** For data that never escapes (proven by `ESCAPE_NONE`), the compiler elides the RCB entirely and uses pure static `free_tree` calls, satisfying the "ASAP First" mandate.

## 5. Conclusion

The refined architecture is **not a replacement** for the existing work but a **unification** of it. It takes the disparate optimizations (#8 Region-RC, #9 Tethers, #15 Isolation) and combines them into a single, cohesive model inspired by "Group Borrowing". 

Implementing this would allow for the **removal of complex SCC runtime logic** while providing **stronger safety guarantees** and **better concurrency** than the current fine-grained RC system.
