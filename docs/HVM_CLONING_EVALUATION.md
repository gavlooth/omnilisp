# Evaluation of HVM Cloning for OmniLisp Transmigration

## 1. The HVM Paradigm

HVM (Higher-order Virtual Machine) employs a unique **Lazy Cloning** strategy based on Interaction Nets (specifically Lamping's optimal reduction).

### Key Mechanisms:
1.  **Linear Ownership:** Like Rust, every value has a single owner.
2.  **`dup` Primitive:** Instead of a deep copy, cloning inserts a `dup` node (fan-out gate).
3.  **Lazy/Incremental Copying:** The graph is not copied immediately. The copy propagates layer-by-layer only as the data is accessed (reduced).
4.  **Superposition:** When a lambda is cloned, it isn't fully copied. Instead, a "superposition" is created, allowing shared computation of the lambda's internal logic until the paths diverge.

## 2. Applicability to OmniLisp Transmigration

We are evaluating this for **Transmigration**: moving an object graph from a dying Region A to a surviving Region B.

### 2.1 The "Lazy Copy" Benefit
*   **HVM:** Copying is O(1) initially (just insert `dup`). Cost is paid at access time.
*   **OmniLisp Goal:** We want `region_exit` to be fast.
*   **Verdict:** **Potentially Dangerous.** In a Region-based system, the Source Region A is *dying*. We cannot "lazily" copy bytes from A to B because A's memory is about to be nuked.
    *   *Constraint:* Transmigration MUST be eager (deep copy) or the Source Region MUST be kept alive (Promotion/Ghosting). HVM's laziness assumes a global heap or a specific interaction net structure that persists.

### 2.2 The "Linearity" Match
*   **HVM:** Enforces linearity (single owner).
*   **OmniLisp:** Our "Region Escape" scenario is inherently linear: The Source is dying, handing off the object to the Destination.
*   **Verdict:** **Good Match.** Transmigration is a "Move" semantics operation. HVM's philosophy aligns with the idea that the source "consumes" the value to produce the copy.

### 2.3 The "Superposition" Complexity
*   **HVM:** Handles complex lambda cloning via interaction nodes.
*   **OmniLisp:** We deal with C-structs and raw pointers.
*   **Verdict:** **Too Complex.** Implementing interaction-net-style superposition for C-structs would require a massive runtime overhaul (rewriting the evaluator to be graph-reduction based).

## 3. Recommendation: "Eager HVM"

We should adopt the **Linear consumption** aspect of HVM but reject the Laziness/Superposition for Transmigration.

### The Strategy: "Eager Clone via Consumption"
Instead of HVM's `dup` node, we implement a `transmigrate` function that acts like an eager `dup`:

1.  **Input:** A pointer to the Root object in Region A.
2.  **Process:** Recursively walk the graph.
3.  **Consumption:** As we copy an node from A to B, we (conceptually) "destroy" the node in A.
    *   *Optimization:* Since Region A is about to be bulk-freed, we don't need to actually write zeros/nulls to A. We just read.
4.  **Forwarding:** If we encounter a node in A that has already been visited (DAG/Cycle), we use the forwarding address map (standard copying GC technique).

## 4. Conclusion

HVM's **laziness** is incompatible with **Region Deallocation** (unless we delay the region free, which defeats the purpose). However, HVM's **linear ownership model** confirms our architectural choice: Transmigration is a linear move.

**Final Decision:** Use **Eager Deep Copy** (Transmigration) with a standard forwarding map. Do not attempt to implement HVM's lazy `dup` nodes, as they require the source memory to remain valid.
