# Architecture Diagrams: Current vs. Proposed (RC-G)

## 1. Current Hybrid Memory Strategy (v0.6.0)

This is the existing "Decision Tree" model where the compiler picks a strategy based on Shape and Escape analysis.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMPILE-TIME ANALYSIS                         │
│                                                                  │
│  Shape Analysis ──► TREE / DAG / CYCLIC                         │
│  Escape Analysis ──► LOCAL / ESCAPING                           │
└─────────────────────────────────────────────────────────────────┘
                              │
          ┌───────────────────┼───────────────────┐
          ▼                   ▼                   ▼
       **TREE**             **DAG**            **CYCLIC**
   (Unique/Unshared)    (Shared/Acyclic)    (Back-Edges)
          │                   │                   │
          │                   │           ┌───────┴───────┐
          │                   │           ▼               ▼
          │                   │        **BROKEN**      **UNBROKEN**
          │                   │      (Weak Refs)     (Strong Cycle)
          │                   │           │               │
          │                   │           │        ┌──────┴──────┐
          │                   │           ▼             ▼
          │                   │     **LOCAL**    **ESCAPING**
          ▼                   ▼    (Scope-Bound) (Heap-Bound)
      Pure ASAP           Standard RC     RC     Component/Tarjan
     (free_tree)          (dec_ref)    (dec_ref) (release_scc)
```

## 2. Proposed Region-Based RC (RC-G) Architecture

This model unifies the strategies under the concept of **Regions**. Every object belongs to a Region (Logical Group). The "Escape" path now leads to **Region-RC** instead of individual object RC.

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMPILE-TIME ANALYSIS                         │
│                                                                  │
│  Region Inference ──► Group Objects by Lifetime                 │
│  Escape Analysis  ──► DOES GROUP ESCAPE SCOPE?                  │
└─────────────────────────────────────────────────────────────────┘
                              │
          ┌───────────────────┴───────────────────┐
          ▼                                       ▼
    **NO ESCAPE**                           **ESCAPES**
 (Local Scratchpad)                      (Shared / Returned)
          │                                       │
          │                               ┌───────┴───────┐
          │                               ▼               ▼
          │                        **WHOLE**           **PARTIAL**
          │                     (Region Promoted)    (Subset Escapes)
          ▼                               │               │
      **PURE ASAP**                       │               ▼
   (Static Region Free)                   ▼        **TRANSMIGRATION**
    - Arena Backend                **REGION RC**    (Eager Deep Copy)
    - Zero Runtime Cost           (Refcounted RCB)    - Copy to Dest
    - Bulk Reclamation            - Tethers for       - Orig Region
                                    Mutation            Reclaimed
```

## 3. The "Tarjan vs. Region" Deallocation Showdown

This diagram specifically illustrates *why* Tarjan is slower and why Regions are faster.

### A. Current: Tarjan (SCC) Deallocation
*Constraint:* Must discover the cycle shape at runtime.

```text
       Step 1: RC=0       Step 2: Traverse (DFS)      Step 3: Free
      (Trigger)          (Cache Miss City!)          (One by one)

      [Root] --.          [Root]?-> [A]?              Free([Root])
                \             |      |                Free([A])
                 v            v      v                Free([B])
                [A] <------> [B] -> [C]?              Free([C])
                 |            ^      |                Free([D])
                 v            |      v
                [D] ----------'     [D]?

    * Latency: O(N) - proportional to graph size.
    * Cache:   Thrashing (chasing pointers).
    * Logic:   Complex (Stack, LowLinks, Indices).
```

### B. Proposed: Region-Hybrid Deallocation
*Constraint:* Structure is pre-grouped.

```text
       Step 1: RC=0       Step 2: Drop Block          Step 3: Done.
      (Trigger)          (Pointer Arithmetic)

      [Region RCB] --.    [ Arena Block 1 ]  ---->  (Return to OS/Pool)
       (Count=0)      \
                       \
                        ->[ Arena Block 2 ]  ---->  (Return to OS/Pool)
                          (Contains Root, A,
                           B, C, D packed
                           contiguously)

    * Latency: O(1) - proportional to number of blocks (usually 1 or 2).
    * Cache:   Hot (Only touched the Header).
    * Logic:   Trivial (free(ptr)).
```