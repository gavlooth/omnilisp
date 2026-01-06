# Third-Party Arena Evaluation

## 1. Candidate: `tsoding/arena`

**Source:** https://github.com/tsoding/arena
**License:** MIT

### 1.1 Implementation Analysis
It is a classic "STB-style" single-header library.
*   **Structure:**
    ```c
    typedef struct Region {
        struct Region *next;
        size_t capacity;
        size_t size;
        uintptr_t data[];
    } Region;
    
    typedef struct Arena {
        Region *begin, *end;
    } Arena;
    ```
*   **API:**
    *   `arena_alloc(Arena *a, size_t size)`: The core bump allocator.
    *   `arena_free(Arena *a)`: Frees the entire chain.
    *   `arena_reset(Arena *a)`: Resets pointers without freeing (reuse).
*   **Compliance:** Pure C99 (uses `uintptr_t`, `stdlib.h`).

### 1.2 Fit for OmniLisp
*   **Pros:**
    *   **Dead Simple:** ~150 lines of code. Easy to audit and embed.
    *   **Chained Regions:** Exactly fits our need for "Unlimited Growth" scratchpads.
    *   **Reset Capable:** Perfect for "One Region per Frame/Loop" optimization.
*   **Cons:**
    *   **Naming Collision:** It calls the internal block `Region`. We use `Region` for the logical controller.
    *   **Mitigation:** We can `#define Region ArenaBlock` before including it, or simple find-and-replace since we are vending it in.

## 2. Recommendation

**Adopt `tsoding/arena.h`**.

**Action Plan:**
1.  Download `arena.h` to `third_party/arena/arena.h`.
2.  Rename the internal struct `Region` to `ArenaChunk` to avoid confusion with our Logical Regions.
3.  Expose it via `runtime/src/memory/arena_core.h`.

This saves us writing the physical allocator from scratch (Phase 1.1 of the Refactoring Plan).
