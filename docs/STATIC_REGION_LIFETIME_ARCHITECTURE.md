# Static Region Lifetime Architecture (ASAP-Region)

## 1. The Hierarchy of Reclamation

Memory reclamation follows a strict two-tier hierarchy:

### Tier 1: Static Deallocation (ASAP)
*   **Target:** 90% of all regions (Locals, Scratches, Temporaries).
*   **Mechanism:** The compiler determines the **Last Use Point** of a region. It inserts `region_create()` and `region_destroy()` calls directly into the C output.
*   **RC Status:** These regions have an `external_rc` of 0. They are never "retained".
*   **Result:** O(1) bulk free at a pre-determined line of code. Zero atomic instructions.

### Tier 2: Reference Counting (Fallback)
*   **Target:** 10% of regions (Escaping data, Shared globals, Multi-threaded transfers).
*   **Trigger:** Triggered when a value from a static region is **returned** or **stored** in a location that outlives the current scope.
*   **Mechanism:** The compiler emits `region_retain()` to transform the static region into a dynamic one. The `external_rc` becomes > 0.
*   **Result:** The region stays alive until the last reference is dropped.

## 2. Compiler Implementation: The "Promote-on-Escape" Rule

The compiler's `region_inference` pass must follow these rules:

1.  **Default to Static:** Every inferred region group starts as `LIFETIME_STATIC`.
2.  **Escape Detection:** If any variable in the region is `ESCAPE_RETURN` or `ESCAPE_GLOBAL`:
    - Mark the region as `LIFETIME_DYNAMIC`.
    - At the escape point, emit `region_retain()`.
3.  **Instruction Insertion:**
    - **Entry:** Insert `region_create()`.
    - **Exit (Static):** If the region is `LIFETIME_STATIC`, insert `region_destroy()` at the last use.
    - **Exit (Dynamic):** If the region is `LIFETIME_DYNAMIC`, insert `region_release()` at the last use.

## 3. Region Subsumption (Flattening)

To maximize performance, the compiler performs a **Subsumption Pass** before final codegen.

*   **Goal:** Eliminate redundant regions by merging them into their parents.
*   **Heuristic:** If Region B's entire lifecycle (from creation to last use) is strictly nested within Region A's active window, and they share similar isolation requirements, the compiler "flattens" B into A.
*   **Benefit:** All objects originally intended for B are allocated in A's arena. This removes the overhead of a second Region Control Block and additional arena management.

## 4. Promotion to Region-RC (The Fallback)

As confirmed, **Promotion to RC** refers specifically to **Region-level Reference Counting**.

*   Individual objects *never* carry refcounts.
*   Only the **Region Control Block (RCB)** is refcounted.
*   When a region escapes, it is transformed from a static scope into a refcounted entity.

## 5. Concurrency Safety (Tethers)

Tethering remains the bridge between the two tiers. Even a `STATIC` region can be tethered by a child thread. The `region_destroy()` call at the end of a static scope will wait (or the region will be deferred) if `tether_count > 0`, ensuring that "Static" does not mean "Unsafe".

## 4. Visual Model

```text
(let [x (make-list)] ... use x ...)
 └─ ASAP Analysis: x is local.
 └─ Emit:
    R = region_create();
    x = mk_list_region(R);
    ... use x ...
    region_destroy(R); // STATIC: No RC checks.
```

```text
(defn get-data [] (let [x (make-list)] x))
 └─ ASAP Analysis: x escapes via return.
 └─ Emit:
    R = region_create();
    x = mk_list_region(R);
    region_retain(R); // ESCAPED: Transition to RC.
    return (RegionRef){x, R};
```
