# Region RC Comparison Report

**Date:** January 6, 2026
**Subject:** Comparison of Proposed "RC-G" Architecture vs. Existing Codebase Mechanisms

## 1. Executive Summary

The proposed **Region-Based Reference Counting (RC-G)** architecture represents a shift towards coarse-grained, topology-aware memory management. While the current codebase contains rudimentary elements of region counting (`external_refcount`), it primarily relies on a mix of fine-grained object reference counting, explicit SCC (cycle) management, and component handles. The proposed system offers a more unified model that could simplify cycle handling and concurrency at the cost of requiring stricter data layout discipline ("Shell" vs. "Content" splitting).

## 2. Existing Reference Counting Systems

The current codebase does not use a single unified RC strategy. Instead, it employs at least five distinct mechanisms:

### 2.1 Standard Object RC
*   **Mechanism:** Per-object `ref_count` field in `Obj` headers.
*   **API:** `inc_ref(x)`, `dec_ref(x)`.
*   **Granularity:** Fine (Individual objects).
*   **Concurrency:** Semantics vary (some atomic, some mutex-protected, some non-atomic).
*   **Issues:** Susceptible to cycles; high overhead (cache line bouncing) if frequently mutated.

### 2.2 Per-Region "External" RC
*   **Location:** `csrc/tests/test_embedded.c`, `runtime/src/memory/region.h`.
*   **Mechanism:** `external_refcount` integer on the `Region` struct.
*   **API:** `REGION_INC_EXTERNAL(r)`, `REGION_DEC_EXTERNAL(r)`.
*   **Purpose:** Tracks *only* pointers coming from outside the region.
*   **Status:** Rudimentary. It serves as a bulk-free trigger (`REGION_CAN_BULK_FREE`) but lacks the sophisticated "Shell/Content" splitting or "Borrow State" tracking of the proposed architecture.

### 2.3 SCC (Cycle) RC
*   **Location:** `runtime/src/memory/scc.c`.
*   **Mechanism:** A reference count for a *group* of objects forming a Strongly Connected Component.
*   **API:** `release_scc(scc)`, `release_with_scc(obj)`.
*   **Purpose:** To manage cyclic garbage without a full tracing collector.
*   **Complexity:** High. Requires freezing cycles, separate metadata, and has been a source of bugs (e.g., `BUG-0004` Refcount Underflow).

### 2.4 Component & Handle RC
*   **Location:** `runtime/src/memory/component.c`.
*   **Mechanism:** `sym_release_handle`.
*   **Purpose:** Manages lifecycle of "Symmetric" components, distinct from standard heap objects.

### 2.5 Borrow References
*   **Location:** `runtime/src/memory/borrowref.c`.
*   **Mechanism:** `BorrowRef` objects that act as temporary handles.
*   **API:** `borrow_release(ref)`.
*   **Purpose:** Runtime tracking of temporary access, preventing premature freeing.

## 3. Comparison with Proposed RC-G

| Feature | Existing Implementation | Proposed RC-G Architecture |
| :--- | :--- | :--- |
| **Unit of Management** | Mixed (Object, SCC, Region, Component) | **Region** (Group) |
| **Intra-Group Refs** | Often refcounted (Standard RC) | **Raw Pointers** (Zero overhead) |
| **Inter-Group Refs** | Standard RC or External Refs | **Strong RegionRef** (Fat Ptr) |
| **Cycles** | Explicit SCC detection & "Freezing" | **Weak Region Refs** + Topology Design |
| **Borrowing** | `BorrowRef` handles (Runtime locks) | **Region Locks** (Reader/Writer bits) |
| **Concurrency** | Ad-hoc (Mutexes, Atomics mixed) | **Region-Granularity Locking** |
| **Deallocation** | Per-object or SCC-batch | **Region-batch** (Destructor Tables) |

## 4. Key Differences & Gaps

### 4.1 Granularity Mismatch
The current system essentially treats Regions as "Arenas" (simple allocators) and overlays Object RC on top. The proposed system treats Regions as the *primary* distinct entities.
*   **Gap:** We lack the "Smart Pointer" (`RegionRef`) abstraction in the current C runtime. Pointers are currently just `void*` or `Obj*`.

### 4.2 Cycle Handling
The current codebase invests heavily in **SCC detection** (`release_with_scc`). The proposed architecture largely sidesteps this by advocating for a **DAG topology** using "Shell" regions and "Weak" back-pointers.
*   **Implication:** Adopting RC-G would allow removing the complex SCC logic in favor of a stricter (but simpler) graph layout.

### 4.3 "Shell" vs. "Content" Pattern
The existing code does not explicitly encourage splitting objects across regions to isolate mutability. Objects tend to live in one heap or are clustered arbitrarily.
*   **Gap:** To realize the concurrency benefits of RC-G, existing large objects (like `GameLevel` in the example) would need to be refactored into multiple linked regions.

## 5. Conclusion

The `external_refcount` in the current codebase is a **primitive subset** of the proposed RC-G architecture. It implements the "count references from outside" logic but lacks the safety guarantees (smart pointers), the borrow checker integration (borrow states), and the structural patterns (Shell/Content) that make the proposed architecture robust.

**Migration Viability:**
Implementing RC-G is feasible but would require:
1.  Formalizing the `RegionReference` struct (Fat Pointer).
2.  Deprecating individual `Obj` refcounts in favor of `Region` refcounts.
3.  Replacing `scc_release` logic with Weak Region Ref logic.
