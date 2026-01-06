# Advanced Algorithms Research: Region Inference & Management

## 1. Theoretical Foundation

Our architecture aligns with the lineage of **Region-Based Memory Management (RBMM)**.

| Algorithm | Concept | Relevance to OmniLisp |
| :--- | :--- | :--- |
| **Tofte & Talpin (MLKit)** | Static Region Inference. Stack-like behavior. | **Foundation.** We use this for the "Local Region" inference (Phase 3.1). |
| **Cyclone** | "Dynamic Regions" + Garbage Collected Heap. | **Validation.** Our "Region-RC" corresponds to Cyclone's dynamic regions, but automated. |
| **Rust (Polonius)** | Flow-Sensitive Borrow Checking via Datalog. | **Future Work.** While powerful, implementing a Datalog engine is overkill for now. Our "Lifetime-Based Inference" is a simplified version of this. |
| **Lobster** | Compile-time Reference Counting. | **Current.** Our existing `rcopt` pass is based on this. |

## 2. Key Takeaways for RC-G

### 2.1 "Dynamic Regions" are Essential
Pure Tofte/Talpin fails when data outlives the stack. Cyclone solved this with "Dynamic Regions" (manually managed).
*   **OmniLisp Innovation:** We automate Dynamic Regions using **Region-RC**. The Region is the dynamic entity.

### 2.2 Transmigration is "Linearity"
HVM and Linear Types (Rust) treat moving data as a consumption event.
*   **OmniLisp Strategy:** Our "Transmigration" is effectively a **Linear Move** of a subgraph from a dying region to a living one.

### 2.3 Region Inference Strategy
We should adopt a **"Points-To Graph + Dominator"** approach (similar to LLVM's MemorySSA or standard SSA-based liveness):
1.  **Build Graph:** Which variables point to which?
2.  **Find Components:** Groups of variables that interact.
3.  **Place Region:**
    *   **Create:** At the *Dominator* of the component's definitions.
    *   **Destroy:** At the *Post-Dominator* of the component's last uses.

## 3. Conclusion

We are on the right track. We are effectively building **"Automated Cyclone"**:
*   Static Regions (Tofte/Talpin) for locals.
*   Dynamic Regions (RC) for escapes.
*   Linear Moves (Transmigration) for optimization.
