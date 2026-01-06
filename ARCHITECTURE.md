# Architecture: OmniLisp Region-RC (RC-G)

## 1. Core Idea
OmniLisp uses a **Region-Based Reference Counting (RC-G)** architecture. Memory management is coarse-grained, topology-aware, and strictly **GC-less**.

### Principles:
- **Everything is a Region**: Objects belong to logical groups (Regions) that share a single lifecycle.
- **ASAP First**: The compiler statically determines where to create and destroy regions.
- **Coarse-Grained RC**: Reference counting applies to the **Region**, not individual objects.
- **Mutable Aliasing**: Safe mutable aliasing is enabled via **Tethers** (region-level locks).
- **No Stop-the-World**: Reclamation is O(1) per block, with zero global pauses.

## 2. Memory Strategy

### A. Local Regions (Pure ASAP)
For data that doesn't escape its scope, the compiler inserts `region_create()` and `region_exit()` calls. The region uses an **Arena Backend** (Bump pointer), making allocation O(1) and deallocation O(1).

### B. Shared Regions (Region-RC)
If data escapes to the heap or another thread, the Region becomes refcounted. 
- **`external_rc`**: Tracks strong references to the Region handle.
- **Unit of Deallocation**: When RC hits 0, the entire Arena is freed in one operation. This naturally handles internal cycles.

### C. Transmigration
When a subset of a region escapes (e.g., returning one object from a large scratchpad), the runtime performs **Adaptive Transmigration**:
- **Small Data**: Eager deep copy into the destination region.
- **Large Data**: **Arena Promotion** (merging the physical memory blocks into the destination region).

## 3. Execution Strategy

```
┌─────────────────────────────────────────────────────────────────┐
│                    GO (Compiler Only)                            │
│  Parser → Region Inference → Codegen → Emit C                   │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    C (Runtime Only)                              │
│  Region-RC Runtime + tsoding/arena Backend                      │
└─────────────────────────────────────────────────────────────────┘
```

## 4. Key Components
- **`Region` (RCB)**: Logical lifecycle owner (RC, Tethers, Liveness).
- **`Arena`**: Physical allocator (Bump pointer, linked blocks).
- **`RegionRef`**: Fat pointer `{ Obj*, Region* }` for sharing ownership.
- **`Tether`**: Borrow mechanism to pin region layout during access.