# Evaluation: San7o/micro-arena vs. tsoding/arena

## 1. Candidate: `San7o/micro-arena`

**Source:** https://github.com/San7o/micro-arena
**License:** MIT

### 1.1 Architecture Analysis
*   **Model:** Fixed-size "First-Fit" allocator.
*   **Storage:** Uses a static stack buffer (`MICRO_ARENA_STACK_MEM_SIZE = 4096`).
*   **Allocation:** Scans a list of free chunks to find a hole (`O(N)`).
*   **Deallocation:** Supports individual `free()`.
*   **Growth:** **Fixed.** It does not chain new blocks. If you exceed 4KB, it fails (returns `NULL`).

### 1.2 Mismatch for RC-G
This library is designed for embedded systems needing `malloc`-like behavior on the stack. It is **not** an Arena in the "Bump Pointer" sense.
*   **Alloc Speed:** O(N) vs O(1) for `tsoding/arena`.
*   **Capacity:** Limited vs Unlimited for `tsoding/arena`.
*   **Metadata:** Heavy (tracks every allocation) vs Zero (tracks only blocks) for `tsoding/arena`.

## 2. Verdict

**REJECT `San7o/micro-arena`.**
**KEEP `tsoding/arena`.**

We need an expanding scratchpad (Bump Pointer), not a fragmentation-managed fixed buffer. `tsoding/arena` implements exactly the "Chained Region" model we designed.
