# Third-Party Region Library Evaluation

## 1. Candidate: `AlxHnr/CRegion`

**Source:** https://github.com/AlxHnr/CRegion
**License:** Public Domain

### 1.1 Features
*   **Logical Region:** `CR_RegionNew()` creates a lifecycle handle.
*   **Callbacks:** `CR_RegionAttach` allows registering cleanup functions (destructors) to be run when the region dies.
*   **Growable Buffers:** Built-in support for dynamic arrays.
*   **Pools:** Built-in fixed-size object pools.

### 1.2 Mismatch for OmniLisp
While the feature set aligns with our "Logical Region" needs, there are critical issues:
1.  **Error Handling:** It calls `exit()` on allocation failure. A language runtime must never crash; it must raise an exception (or panic gracefully).
2.  **Thread Safety:** The documentation explicitly states it is **not thread-safe**. Our RC-G architecture relies on `Region` being the unit of concurrency (Tethers), requiring atomic refcounts in the region header.
3.  **Structure:** It combines the "Arena" and "Region" concepts tightly. We specifically decided to separate them (`docs/REGION_VS_ARENA_DEFINITION.md`) to allow swapping backends.

## 2. Other Candidates
*   **`San7o/micro-arena`**: Evaluated previously. Too limited (fixed size).
*   **`ccgargantua/arena-allocator`**: Purely physical allocator (Arena), not logical (Region).

## 3. Recommendation

**Do NOT adopt a third-party Region library.**

The "Region" concept in our architecture is highly specific to our runtime needs:
*   It needs **Atomic Reference Counting** (`external_rc`).
*   It needs **Tethering** logic (`tether_count`).
*   It needs to integrate with our specific `tsoding/arena` backend.

**Action Plan:**
Stick to the Refactoring Plan:
1.  Use `tsoding/arena` for the **Physical** layer (Arena).
2.  Write our own custom `Region` struct for the **Logical** layer to handle the specific Atomic/Tether/Transmigrate logic.
