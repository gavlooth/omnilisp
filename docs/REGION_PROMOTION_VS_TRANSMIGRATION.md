# Analysis: Region Promotion vs. Transmigration

## 1. Context
When data escapes a dying Region (Scope), we must ensure it survives in the destination Region. Two primary strategies were identified:
1.  **Transmigration:** Deep copying the data.
2.  **Promotion:** Transferring ownership of the underlying memory blocks.

## 2. The Case for Promotion (and why it's "Too Involved")

Promotion sounds elegant: "Just hand over the keys to the memory block." However, in practice, it introduces significant complexity and coupling.

### 2.1 The "Backend Mismatch" Problem
Promotion relies on the Source and Destination Regions using **identical allocator backends**.
*   **Scenario:** Region A uses a `BumpArena` (fast, linear). Region B uses a `SlotPool` (fixed size, reuse).
*   **Conflict:** You cannot "promote" a linear chunk of memory into a slot-based pool. The allocator logic is incompatible.
*   **Result:** Promotion requires runtime type-checking of backends, reducing performance and flexibility.

### 2.2 The "Inherited Garbage" Problem (Fragmentation)
Regions are often used for temporary scratch space.
*   **Scenario:** A 10MB region contains 9.9MB of temporary computation data and 0.1MB of "result" data.
*   **Action:** If we **Promote** the region (hand over the 10MB block to the parent), the parent now owns 10MB of memory, 99% of which is dead garbage.
*   **Result:** This is effectively a memory leak until the parent region dies. To fix this, you'd need a "Compact" phase... which is just Transmigration in disguise.

### 2.3 The "Block Chaining" Complexity
Allocator blocks are often linked lists.
*   **Mechanism:** Splicing a linked list from Region A into the middle of Region B's list is O(1).
*   **Risk:** It messes with the "Block Size" heuristics. If Region B expects 1MB blocks and inherits a tiny 4KB block from Region A, it might fragment its own allocation patterns or increase metadata overhead (walking longer chains).

## 3. The Case for Transmigration (The Robust Default)

Transmigration (Deep Copy) is "Data Movement," not "Memory Management Gymnastics."

*   **Backend Agnostic:** You can transmigrate from a `BumpArena` to a `SlotPool`, or even to a `Malloc` region. It's just copying bytes.
*   **Compacting:** By copying *only* the reachable escaping graph, you naturally leave the 9.9MB of garbage behind in the dying region.
*   **Simple Pointers:** The new copy is fresh. You update the pointers, and you're done.

## 4. Conclusion: When to use which?

| Strategy | Complexity | Efficiency | Best Use Case |
| :--- | :--- | :--- | :--- |
| **Transmigration** | Low (Copy logic) | Variable (O(Graph Size)) | **Default.** Small to medium results escaping a large scope. |
| **Promotion** | **High** (Allocator coupling) | High (O(1)) | **Special Case.** When the *entire* region's content is the result (e.g., loading a level and returning the whole structure). |

**Recommendation:** Stick to **Transmigration** as the primary mechanism. Reserve Promotion only for specialized "Builder" patterns where the region is explicitly designed to be handed over (e.g., `Region::create_detached()`).
