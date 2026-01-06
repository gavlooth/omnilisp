# Memory Model Technical Reference (RC-G)

## 1. Object Layout
OmniLisp objects are header-light. Reference counts and cycle metadata are moved to the **Region Control Block**.

```c
typedef struct Obj {
    int mark;               // Unused or Region status
    int tag;                // Type tag
    int is_pair;            // Flag
    union {
        long i;
        double f;
        struct { struct Obj *a, *b; };
        void* ptr;
    };
} Obj;
```

## 2. Pointer Semantics

### Region Reference (Strong)
A fat pointer that owns a region.
*   **Structure:** `{ void* ptr, Region* rcb }`
*   **Assignment:** Increments `rcb->external_rc`.
*   **Drop:** Decrements `rcb->external_rc`. If 0, attempts bulk free.

### Intra-Region Pointer (Raw)
A naked pointer `Obj*` used within a region.
*   **Efficiency:** Zero RC overhead.
*   **Safety:** Valid as long as the parent Region is alive (guaranteed by ASAP or Tethers).

## 3. Lifecycle Management

### Creation
1.  `region_create()` initializes a new Arena.
2.  `region_alloc()` bumps the pointer in the arena.

### Destruction (The Bulk Free)
When `external_rc == 0` AND `tether_count == 0` AND `scope_alive == false`:
1.  All blocks in the Arena are returned to the OS.
2.  All registered destructors run.
3.  Cycles within the region are destroyed implicitly.

## 4. Concurrency (Tethers)
Threads do not lock individual objects. They **Tether** the entire Region.
*   **`region_tether_start(Region*)`**: Pins the region layout.
*   **Multiple Readers**: Many threads can tether the same region for reading.
*   **Mutation**: Modification is safe as long as the partitioning (Group structure) is preserved.