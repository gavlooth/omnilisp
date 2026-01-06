# Region vs. Arena: Rigorous Definitions

## 1. The Core Distinction

To avoid conflation in the OmniLisp runtime, we define **Region** and **Arena** as orthogonal concepts:

| Concept | Definition | Responsibility | Analogy |
| :--- | :--- | :--- | :--- |
| **Region** | **Logical Lifecycle Group** | *When* memory is freed. *Who* owns it. | The "Account" or "Contract". |
| **Arena** | **Physical Allocation Backend** | *How* memory is reserved. *Where* bytes live. | The "Bank Vault" or "Storage Unit". |

**Key Rule:** A **Region** *uses* an **Arena** (or another backend) to store its objects. A Region is the *policy*; an Arena is the *mechanism*.

## 2. Rigorous Definition: Region

A **Region** is a runtime entity representing a **group of objects that die together**.

*   **Identity:** Has a unique ID (`RegionID`) and a Control Block (`RCB`).
*   **Semantics:**
    *   **Atomic Life:** All objects in the region are valid as long as the Region is valid.
    *   **Unit of RC:** Reference counts apply to the *Region*, not individual objects.
    *   **Unit of Borrow:** Threads tether (borrow) the *Region*, locking its layout.
*   **Structure:**
    ```c
    struct Region {
        RCB* control_block;      // RC, Tethers, Liveness
        IRegionAllocator* backend; // The physical storage (Arena, Pool, etc.)
    };
    ```

## 3. Rigorous Definition: Arena

An **Arena** is a specific **Memory Allocator Implementation**.

*   **Identity:** A pointer to a contiguous (or chained) block of raw memory.
*   **Semantics:**
    *   **Bump Allocation:** Extremely fast (`ptr = top; top += size`).
    *   **Bulk Release:** Cannot free individual items. Can only "reset" or "free all".
    *   **No Ownership Logic:** The Arena doesn't know about "borrows" or "reference counts". It just holds bytes.
*   **Usage:**
    *   A `Region` might choose to use an `Arena` backend for performance (typical for cyclic data).
    *   A `Region` might choose a `Pool` backend for fixed-size objects.
    *   A `Region` might choose a `Malloc` backend (just tracking pointers) for huge, sparse objects.

## 4. Strategies for Escaping Data

When a simplified Region (using an Arena backend) is about to die, but some of its data has "escaped" (is referenced by an outer scope), we need a strategy to handle it. We cannot keep the whole 10MB Arena alive for one 16-byte object.

### Strategy A: Transmigration (Deep Copy)
**"The Cleaner"**
*   **Mechanism:** Deep-copy the escaping object graph from the dying Region's Arena into the *surviving* Region's backend (e.g., the parent's Arena).
*   **Pros:**
    *   **Eliminates Fragmentation:** The dying 10MB Arena is fully reclaimed.
    *   **Locality:** The surviving object moves close to its new owner.
*   **Cons:**
    *   **Cost:** Copying data takes time.
    *   **Pointer Fixups:** Requires updating all internal pointers in the copied graph.

### Strategy B: Promotion (Arena Detachment)
**"The Handover"**
*   **Mechanism:** If the escaping data is *large* (e.g., >50% of the arena), we simply "promote" the entire Arena to the parent Region. The parent adopts the Arena block.
*   **Pros:** Zero-copy. Instant.
*   **Cons:** Wasted memory (fragmentation) if the escaping part is small.

### Strategy C: Remote Reference (Weak/Strong Handles)
**"The Anchor"**
*   **Mechanism:** Keep the original Region alive (via RC), but mark it as "Ghost". It exists solely to serve the escaping object.
*   **Pros:** Simple. No copying.
*   **Cons:** Maximum fragmentation. High risk of "memory leaks" (unreclaimed space).

## 5. Summary of Architecture Changes

To implement this separation:

1.  **Refactor `region.h`**: Remove `ArenaRegion`, `PoolRegion` struct definitions from the public header. Hide them behind the `IRegionAllocator` interface.
2.  **Rename**: Ensure `Region` always implies the *Lifecycle Handle* and `Arena` always implies the *Allocator*.
3.  **API**:
    *   `region_new(Strategy)` -> Returns a `Region*`.
    *   `region_alloc(Region*, size)` -> Delegates to `backend->alloc()`.
    *   `transmigrate(Obj*, DestRegion*)` -> Implements Strategy A.
