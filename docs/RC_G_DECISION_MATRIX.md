# Decision Matrix: Region-Based RC (RC-G) vs. Current Hybrid

## 1. Executive Summary

| Feature | Current Hybrid Strategy | Proposed Region-RC (RC-G) |
| :--- | :--- | :--- |
| **Philosophy** | "Right tool for the right shape" (Mix of 5+ strategies) | "Everything is a Region" (Unified model) |
| **Performance** | High optimized, but cache-poor for RC | **Superior Cache Locality** (fewer atomic ops) |
| **Safety** | Complex (SCC detection required) | **Inherently Safe** (Bulk free handles cycles) |
| **Complexity** | High Runtime Complexity (SCC, Components) | **High Compiler Complexity** (Region Inference) |
| **Concurrency** | Object-level locking (Fine-grained) | Region-level Tethering (Coarse-grained) |

## 2. Pros & Cons Analysis

### A. Performance

| Metric | Region-RC (RC-G) | Current Hybrid |
| :--- | :--- | :--- |
| **Throughput** | **PRO:** 90% fewer atomic operations. Only the Region header is touched for refcounting, keeping hot object data in L1 cache. | **CON:** Standard RC "thrashing". Every pointer pass potentially dirties a cache line to update a count. |
| **Latency** | **PRO:** Deterministic bulk free. No "scan" phase. | **CON:** `release_scc` can trigger an O(N) traversal of the cycle island. |
| **Allocation** | **PRO:** Bump-pointer allocation (Arena) is nearly free. | **CON:** `malloc`/`pool` allocation per object is slower and causes fragmentation. |

### B. Safety & Cycles

| Metric | Region-RC (RC-G) | Current Hybrid |
| :--- | :--- | :--- |
| **Cycles** | **PRO:** Solved by design. A region cycle (intra-region) is reclaimed instantly when the region dies. | **CON:** Requires explicit "SCC Detection" (Tarjan's) or manual Weak Refs to avoid leaks. `BUG-0004` proved this is fragile. |
| **UAF (Use-After-Free)** | **PRO:** Tethers prevent the Region from dying while a thread is reading it. | **PRO:** BorrowRef/IPGE already handles this well. |

### C. Complexity & Implementation

| Metric | Region-RC (RC-G) | Current Hybrid |
| :--- | :--- | :--- |
| **Compiler** | **CON:** Much harder. Requires "Region Inference" (grouping objects) and Transmigration logic insertion. | **PRO:** Easier. Shape analysis is local and simpler to implement. |
| **Runtime** | **PRO:** Dead simple. `Region` struct + `free()` calls. No complex graph algorithms (Tarjan's) needed. | **CON:** Complex. Maintains multiple subsystems (SCC, Component, WeakTable) that must interact. |
| **Debugging** | **CON:** "Region Fragmentation" leaks are harder to debug than a simple "this object wasn't freed". | **PRO:** Tools like Valgrind work naturally with per-object mallocs. |

### D. Flexibility (The "Restrictions")

| Metric | Region-RC (RC-G) | Current Hybrid |
| :--- | :--- | :--- |
| **Granularity** | **CON:** Coarse. Modifying one object "locks" the whole region (if single-threaded mutable alias). | **PRO:** Fine. You can mutate Object A without caring about Object B next to it. |
| **Partial Escape** | **CON:** Requires "Transmigration" (Copying). Expensive if you return 1 large object from a huge region. | **PRO:** Zero-cost. Just pass the pointer; the object's RC handles it. |

## 3. The "Dealbreakers"

### Why you might REJECT RC-G:
1.  **Transmigration Cost:** If your workload involves creating massive graphs and then returning *deep inside* them constantly, the copying overhead of Transmigration will kill performance.
2.  **Compiler Effort:** If the team lacks the resources to write a robust "Region Inference" pass in the compiler, the runtime benefits won't be realized.

### Why you might ACCEPT RC-G:
1.  **Simplicity of Runtime:** You want to delete `scc.c`, `component.c`, and `tarjan.c`. You want a runtime that is just "Arenas and Counts".
2.  **Concurrency Safety:** You want to safely share data between threads without thinking about mutexes on every object.
3.  **Cache Efficiency:** You are targeting modern CPUs where cache misses are the primary bottleneck.

## 4. Recommendation

**Adopt RC-G** if:
*   You prioritize **Runtime Simplicity** and **throughput** over Compiler simplicity.
*   You are willing to accept the **Transmigration** trade-off (copying escaping data).

**Keep Hybrid** if:
*   You need **fine-grained mutation** of complex graphs.
*   You cannot afford the compiler engineering cost right now.
