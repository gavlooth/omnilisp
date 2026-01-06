# ASAP for Regions: Static Block Deallocation

## 1. The Concept

The core ASAP (As Static As Possible) philosophy currently tracks individual objects:
*   *Analysis:* "Object `x` is last used at line 50."
*   *Action:* Insert `free_tree(x)` at line 51.

We can apply the exact same logic to **Regions**:
*   *Analysis:* "Region `R` (containing objects x, y, z) is last used at line 50."
*   *Action:* Insert `region_destroy(R)` at line 51.

## 2. How it Works

### 2.1 Region Scope Inference
The compiler infers a "Region Scope" for every group of related objects.
```clojure
(defn process-data []
  (let [R (region-create)      ; Implicit or Explicit
        x (in R (make-obj))
        y (in R (make-obj))]
    (compute x y)
    ; Last use of R (via x and y) is here.
  )) ; <--- Compiler inserts region_destroy(R)
```

### 2.2 Escaping Regions
If the region escapes, ASAP delegates to runtime RC (just like it currently delegates to `dec_ref` for escaping objects).
```clojure
(defn make-data []
  (let [R (region-create)
        x (in R (make-obj))]
    (return x))) ; Escapes!
; Compiler inserts: region_inc_external(R)
; No static destroy.
```

## 3. The "Whole Block" Advantage

Applying ASAP to Regions is actually **easier** and **more effective** than applying it to objects:

1.  **Reduced Cardinality:** A function might have 100 objects but only 1 or 2 Regions. The static analysis has fewer variables to track.
2.  **Cycle Immunity:** ASAP often struggles with static cycles (requiring `deferred_release`). With Regions, if the cycle is internal to the Region, ASAP simply destroys the Region. The cycle doesn't matter.
3.  **Deterministic Cleanup:** We guarantee that at line X, the *entire* scratchpad memory is reclaimed.

## 4. Implementation Strategy

We will reuse the existing Liveness Analysis (`csrc/analysis/analysis.c`).

1.  **Treat Regions as Variables:** The Region Handle (`Region* r`) is a variable tracked by liveness.
2.  **Propagate Liveness:** If object `x` is in Region `R`, then a usage of `x` counts as a usage of `R`.
3.  **Kill Point:** When `R` is dead (no live objects inside it are reachable), insert `region_destroy(R)`.

## 5. Conclusion

Yes. We can and should apply ASAP to Regions. It transforms the runtime from "Dynamic RC" to "Static Scoped Memory" for the vast majority of local code, using RC only as a fallback for complex escapes.
