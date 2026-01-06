# Transmigration Strategy

## 1. Context: The "All-Region" Decision

We have mandated that **All Reference Counting is Region Reference Counting.**
This implies:
1.  Individual objects do *not* have refcounts.
2.  Cycles within regions are handled by bulk deallocation.
3.  **Problem:** If a small object escapes a dying region, we must move it to a surviving region to avoid keeping the dead region alive.

## 2. Transmigration Candidates

We are choosing the mechanism for `transmigrate(obj, dest_region)`.

### Option A: Eager Deep Copy (The "Safety First" Approach)
*   **Mechanism:** Recursively copy the object and everything it points to into the destination region.
*   **Pointer Fixup:** As we copy, we build a `Map<OldPtr, NewPtr>`. If we encounter a pointer to an already-copied object, we use the NewPtr.
*   **Pros:**
    *   **Simple:** Standard graph copying algorithm.
    *   **Safe:** The source region can be immediately and totally destroyed.
    *   **Compacting:** Result is contiguous in the new region.
*   **Cons:**
    *   **Latency:** O(Graph Size). If the graph is large, this pauses the thread.
    *   **Memory:** Double memory usage during the copy operation.

### Option B: Shallow Copy + "Ghosting" (The "Fast" Approach)
*   **Mechanism:** Copy *only* the root object.
*   **Pointer Handling:** Pointers to children still point to the *Old Region*.
*   **Ghosting:** The Old Region is kept alive (RC=1), but marked as "Ghost" (read-only / deprecated).
*   **Pros:**
    *   **Instant:** O(1) operation.
    *   **Lazy:** We don't pay the copy cost until later (or never).
*   **Cons:**
    *   **Fragmentation:** The 10MB Old Region stays allocated for a tiny object. This defeats the purpose of Transmigration!
    *   **Complexity:** We now have cross-region pointers that defy the stack topology.

### Option C: Adaptive Copy (The "Smart" Approach)
*   **Mechanism:**
    1.  Start an Eager Deep Copy.
    2.  **Budget Check:** If we copy more than `N` bytes (e.g., 4KB), **STOP**.
    3.  **Fallback:** If the graph is huge, abort the copy and fall back to **Promotion** (handing over the entire Old Region block to the New Region).
*   **Pros:**
    *   **Best of Both:** Fast for small things (common case), robust for huge things (avoids deep-copy pause).
    *   **No Fragmentation:** Large objects *should* keep their regions; small objects shouldn't.
*   **Cons:**
    *   **Implementation Complexity:** Requires the copy function to be interruptible/abortable.

## 3. Recommendation: Option C (Adaptive)

We should implement **Adaptive Transmigration**:

1.  **Default:** Eager Deep Copy. Most return values are small (strings, small structs, config maps).
2.  **Threshold:** Set a limit (e.g., `TRANSMIGRATE_LIMIT = 4096` bytes).
3.  **Fallback:** If the limit is hit, we abort the copy and perform **Region Fusion** (Promotion), where the Destination Region simply adopts the Source Region's memory blocks into its own chain.

### Why not HVM Lazy Copy?
As discussed in `HVM_CLONING_EVALUATION.md`, HVM's laziness requires the source memory to stay valid. That is functionally identical to Option B (Ghosting), which leads to fragmentation. We want to *clean up* memory, not just manage it.
