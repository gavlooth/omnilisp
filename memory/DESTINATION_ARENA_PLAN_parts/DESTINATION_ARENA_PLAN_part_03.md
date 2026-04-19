# DESTINATION_ARENA_PLAN Part 03

Source: `memory/DESTINATION_ARENA_PLAN.md`

## 16. Revision IX (Dialectical Edge: The GC Pause Fallacy & True Amortization)

### Antithesis IX: The "Amortized" GC Pause
Section 13's Observation 1 admits that checking if a scope is `splittable` requires an $O(N)$ graph walk upon scope exit. It attempts to dismiss this cost by claiming it is "amortized against the O(N) list construction cost."

1. **Latency vs. Throughput:** "Amortization" only applies to total CPU throughput. In terms of runtime latency, an $O(N)$ graph walk executed synchronously at the exact moment of a function return is indistinguishable from a **Stop-The-World Garbage Collection Pause**.
2. **The Freeze:** If a TCO loop spends 5 seconds constructing a 10-million element list in the escape lane, returning that list will suddenly freeze the Omni Lisp runtime while it walks 10 million pointers to verify `scope_gen` integer stamps.
3. **Violation of Non-Negotiables:** This violates the core Omni mandate. The entire purpose of manual Region/Bump memory management is to guarantee predictable execution bounds without unpredictable pauses.

*Citation:* Blackburn, S. M., et al. (2004). "Myths and Realities: The Performance Impact of Garbage Collection." *SIGMETRICS*. This paper establishes that synchronous $O(N)$ traversal pauses destroy tail-latency and real-time predictability, fundamentally negating the primary latency benefits of region-based/manual memory management.

*Confidence Score:* **99%**. A synchronous $O(N)$ walk on a function return boundary is precisely the type of unpredictable latency spike that region-based allocators were invented to eliminate.

### Synthesis IX: The Distributed Write-Barrier (Fusing Section 13 and 15)
We must keep the brilliant C3 structural mapping of Section 13 (the `alloc_lane` flag and the TCO bump-reset), but we must absolutely eradicate the $O(N)$ `splittable` exit check. We do this by applying the Write-Barrier concept.

1. **The Single-Region / Dual-Lane Structure (from Sec 13):**
   We keep `ScopeRegion` as a single object, but we give it a `current_lane` enum (`LANE_TEMP` or `LANE_ESCAPE`).

2. **The Constructor Barrier (No End-of-Scope Walking):**
   Instead of checking validity at the *end* of the function, we check it at the *moment of allocation*.
   When Lisp calls `make_cons(car, cdr)`:
   * The C3 runtime checks `interp.current_scope.current_lane`.
   * If it is `LANE_ESCAPE`, it looks at the `scope_gen` stamps of `car` and `cdr`.
   * If `car.scope_gen == TEMP_GENERATION`, the runtime immediately invokes `copy_to_lane(car, LANE_ESCAPE)`.

3. **True Amortization:**
   Because the promotion happens *during* the loop iteration that created the dangling pointer, the cost is genuinely distributed. There is no massive GC pause at the end of the function.

4. **The $O(1)$ Return:**
   Because `make_cons` guarantees that no `TEMP` pointers ever make it into the `ESCAPE` lane, the `splittable` invariant is mathematically guaranteed. When the function returns, the runtime executes the $O(1)$ splice in 3 CPU instructions. Zero graph walking. Zero pauses.


---

## 17. Revision X (Dialectical Edge: Write-Barrier Completeness & Promotion Semantics)

### Antithesis X: The Holes in the Barrier
The findings critique Section 16 as theoretically sound but practically incomplete in a dynamic language. The specific failures are:

1. **Incomplete Coverage (Mutation Bypass):** Enforcing the invariant only in constructors (`make_cons`) ignores runtime mutation. If an object is allocated in the `ESCAPE` lane, and later mutated via `set-car!` or `array-set!` to point to a `TEMP` object, the barrier is bypassed and a dangling pointer is created.
2. **The Latency Shell Game:** Shifting the $O(N)$ graph walk from the end of the scope to the `copy_to_lane` promotion inside the loop does not eliminate the latency spike; it merely moves it. If a massive tree is promoted during a loop iteration, that iteration will stall.
3. **Cycle Collapse:** Localized promotion without a global visited-set (memoization) will unroll shared DAGs into trees, or infinite loop on cyclic data structures.

*Confidence Score:* **100%**. These are hard engineering facts. A write barrier must cover all writes, not just allocations.

### Synthesis X: The Comprehensive Write Barrier & Epoch Memoization
To secure the `ESCAPE` invariant without breaking latency or cyclic invariants, we must formalize the Write Barrier semantics across the entire runtime.

**1. Full Mutator Coverage (The True Write Barrier)**
The invariant check (`value.scope_gen == TEMP_GENERATION`) must be injected into **every** operation that modifies a heap object's pointers, not just constructors.
* Affected Primitives: `set-car!`, `set-cdr!`, `vector-set!`, `dict-set!`, and any struct field mutations.
* If the target container is `ESCAPE` and the payload is `TEMP`, the payload must be promoted before the assignment.

**2. Epoch-Based Memoization (Solving Cycles)**
To prevent DAG explosion and infinite loops during `copy_to_lane`, the promotion engine must maintain a transient `VisitedSet`.
* However, allocating a hash map for every single tiny `copy_to_lane` call is too slow.
* *Solution:* Attach the `VisitedSet` to the `ScopeRegion` itself, initialized lazily.
* When a promotion occurs, nodes are marked with an `epoch` counter. This guarantees cycle safety and topology preservation across multiple localized promotions within the same scope lifecycle.

**3. Bounded Latency (The Tri-State Fallback)**
To solve the latency spike of promoting a massive object during a loop iteration, we acknowledge a hard limit.
* The promotion engine tracks bytes copied. If `bytes_copied > LATENCY_BUDGET` (e.g., 4KB) during a single `copy_to_lane`, it aborts the promotion and raises a `PROMOTION_ABORTED` flag on the scope.
* If `PROMOTION_ABORTED` is raised, the scope is marked as **Unsplittable**.
* At the end of the function, if the scope is Unsplittable, it falls back to the safe, slow $O(N)$ `copy_to_parent` of the entire return value.

*Result:* The fast path (99% of cases) enjoys true zero-copy, O(1) returns with no end-of-scope graph walking. The worst-case latency spike (promoting huge subgraphs mid-loop) is bounded by a hard ceiling, falling back to a known-safe post-function copy, guaranteeing real-time predictability without sacrificing safety.

*Confidence Score:* **85%**. This represents a realistic systems-engineering compromise, balancing the theoretical purity of write barriers with the physical realities of dynamic graph mutation.

---

## 18. Revision XI (Operational Specification & Invariants)

### Antithesis XI: The "Almost Ready" Gap
Section 17 bridges the gap between theory and systems engineering, but falls short of a rigorous implementation specification. The findings correctly identify that the write-barrier definition is too narrow, the lifecycle of the memoization epoch is hand-waved, and the partial-failure state of an aborted promotion lacks a consistency invariant.

*Confidence Score:* **0.90**. These gaps are standard operational hazards when translating abstract algorithms to C3 pointer semantics. If left underspecified, they lead to memory leaks or DAG corruption during complex edge cases.

### Synthesis XI: The Final Implementation Constraints
To transition the plan to "Implementation Ready," we formally specify the exact state-machine invariants required for the Dual-Lane architecture.

**1. The "All-Pointer-Write" Barrier Rule**
The write barrier cannot rely on an enumerated list of functions (like `set-car!`). It is an architectural rule governing pointer writes.
* **The Rule:** Before *any* pointer `P` (where `P` is a `Value*` or `Env*` living in the `TEMP` lane) is written into a memory address `A` (where `A` resides within an `ESCAPE` chunk), `P` must be promoted.
* **Lane Membership Check:** Determining whether `A` is in `ESCAPE` must use either:
  - an explicit lane tag captured at allocation time (preferred O(1) fast path), or
  - a chunk-list range check as fallback/debug validation.
  The barrier fast path must be O(1); full chunk scans are not allowed on hot writes.
* **Coverage Checklist:**
  - Heap Containers (`make_cons`, `set-car!`, `set-cdr!`, `vector-set!`, `dict-set!`)
  - Environment Bindings (`Env.extend`, `Env.define`, `env_set!`)
  - Continuation/Fiber Contexts (capturing a temp variable into a suspended state)
  - Object/Struct Field Writes (for user-defined types)

**2. Epoch Memoization Lifecycle & Memory Ownership**
The `VisitedSet` is a hash map attached to `ScopeRegion`.
* **Initialization:** Lazily allocated on the first `copy_to_lane` trigger. It is allocated in the *C3 heap* (`mem::malloc`), not the bump chunks, because it is runtime metadata, not user Lisp data.
* **Epoch Increment:** `scope.promotion_epoch` increments at the start of every top-level `copy_to_lane` call.
* **Overflow:** If `promotion_epoch` overflows `uint.max`, the `VisitedSet` is `clear()`'d and the epoch resets to 1.
* **Cleanup:** The `VisitedSet` is destroyed (`mem::free`) when the `ScopeRegion` is destroyed or recycled. It is *not* transferred during `scope_splice_escapes`.

**3. The Consistency Invariant for Aborted Promotions (Tri-State)**
If `copy_to_lane` exceeds the latency budget and aborts, we are left with a partially promoted graph.
* **The Invariant:** A partially promoted graph is **valid and safe**, but the scope is now flagged `Unsplittable`.
* **The Fallback:** When an `Unsplittable` scope finishes, the runtime executes the traditional $O(N)$ `copy_to_parent` on the *entire* return root.
* **No-Duplication Guarantee:** The fallback `copy_to_parent` **must** participate in the same Epoch Memoization system! When it walks the graph, any nodes that were previously promoted will have their `scope_gen == ESCAPE_GENERATION`. The fallback walker treats these as already-safe leaves and does *not* recopy them. This prevents duplicating the partially-promoted nodes.
* **Shared Promotion Context:** `copy_to_lane` and fallback `copy_to_parent` must share one promotion context per return boundary (same epoch namespace + memo table instance) so partial work is reused deterministically.

**4. RC > 1 Behavior under Unsplittable**
If a scope is flagged `Unsplittable` (meaning it contains tangled temp/escape pointers) AND its `RC > 1` (a closure captured it):
* **The Behavior:** The runtime does *nothing*.
* **Why:** The fundamental rule of Region-RC is that captured scopes survive intact. Because the scope is surviving, the `TEMP` lane is surviving. Therefore, any pointers from `ESCAPE` to `TEMP` are perfectly safe. The "Unsplittable" flag simply means we cannot optimize the return by tearing the scope in half. Both lanes live on as a floating heap island for the closure.

**5. FFI / Raw Pointer Policy (Explicit Exclusion Zone)**
Raw/native pointers (FFI-managed memory or opaque external handles) are outside lane provenance unless wrapped in runtime-managed `Value` carriers with explicit ownership metadata.
* Barrier logic must not assume transitive safety through opaque native pointers.
* Any native write path that can embed runtime pointers must route through explicit bridge functions that apply the same lane checks/promotion rules.
* If ownership/provenance cannot be proven, treat as boundary-unsafe and force conservative promotion at the bridge boundary.

*Overall Plan Confidence:* **0.92**. The theoretical models (Cyclone/RC) are now fully grounded in strict memory ownership semantics, deterministic fallback states, and explicit coverage rules appropriate for the Omni runtime.


---

## 19. Revision XII (Final Pre-Implementation Clarifications)

### Antithesis XII: The Devil in the Micro-Details
While Section 18 formalizes the macro-architecture, three micro-level ambiguities remain that could cause performance degradation or correctness bugs during C3 implementation:
1. The $O(1)$ fast-path for checking if an address `A` is in the `ESCAPE` lane is undefined.
2. The exact handoff of the memoization epoch between an aborted `copy_to_lane` and the fallback `copy_to_parent` is implicit.
3. FFI and native raw pointers represent a potential blind spot in the "All-Pointer-Write" rule.

### Synthesis XII: The Micro-Specification
These final clarifications lock down the physical C3 implementation constraints.

**1. The $O(1)$ Address Lane Check**
Determining if memory address `A` resides in the `ESCAPE` lane must never involve a linear scan of the `escape_chunks` linked list.
* **The Implementation:** `Value` and `Env` structs must embed their lane designation directly in their metadata stamp. We already have `scope_gen` (which differentiates `TEMP_GENERATION` from `ESCAPE_GENERATION`).
* **The Fast Path:** `is_escape_lane(A)` is simply `((Value*)A)->scope_gen == scope->escape_generation`. This is a single memory load and integer comparison (1-2 CPU instructions).

**2. Epoch Handoff on Abort**
If a promotion aborts (Tri-State Fallback), the subsequent `copy_to_parent` executed at scope exit **must not** increment the promotion epoch.
* **The Rule:** The fallback `copy_to_parent` must be explicitly passed the *same* `promotion_epoch` that was active when the scope was flagged `Unsplittable`. This guarantees that nodes partially promoted during the aborted run are recognized as safely visited during the fallback walk.

**3. The FFI / Raw Pointer Exemption**
The "All-Pointer-Write" rule applies strictly to Lisp-managed heap pointers (`Value*`, `Env*`).
* **Native Pointers:** Raw C pointers (`void*`), FFI handles, and unmanaged native structs are opaque to the Omni region system. They do not trigger the write barrier, nor are they followed during promotion traversal. Their lifecycle is explicitly managed by the FFI boundary rules, not the bump allocator's lane routing.

*Final Readiness Confidence:* **0.90**. The design is structurally sound, mathematically bounded, and fully mapped to concrete C3 mechanics. Proceed to Commit 1.

---

## 20. Revision XIII (Clean Consolidated Systems Spec)

### Scope
This section consolidates Sections 1-19 into one implementation-ready specification.
Where conflicts exist, this section is authoritative.

### Consolidated Specification

**1. Core Architecture (Dual-Bump, Single-Scope)**
* Each `ScopeRegion` owns two allocation lanes: `TEMP` and `ESCAPE`.
* No stop-the-world GC is introduced; lifecycle remains RC + scope/region based.
* `scope_adopt` is not used in normal return flow and is removed once parity gates pass.

**2. Allocation Policy (Intent-Based, Not Tail-Guessing)**
* Allocation lane is selected by ownership intent, not by AST tail-position heuristics.
* `TEMP` is the default for local/transient work.
* `ESCAPE` is used for values that are expected to cross the boundary (return builders, captured env payloads, root-bound inserts).
* This keeps temporary churn cheap while minimizing promotion volume.

**3. Write-Barrier Invariant (Hard Rule)**
* A `TEMP` pointer must never be embedded inside an `ESCAPE` container.
* Every pointer-write mutator path (`make_cons`, `set-car!`, `set-cdr!`, array/hashmap/env writes, and bridge mutators) enforces this rule.
* Lane check must be O(1) using stamped provenance (`scope_gen`/lane metadata), never via chunk-list scans on hot writes.
* If a violating write is attempted, run targeted promotion (`copy_to_lane`) before commit.

**4. Fallback Semantics (Tri-State, Epoch-Reusing)**
* `copy_to_lane` runs under a bounded budget (for example, 4KB traversal budget).
* On budget abort: mark scope `Unsplittable` and record a deferred-boundary state.
* `copy_to_lane` and fallback `copy_to_parent` share the same promotion context (epoch namespace + memo table) so partial promotion work is reused without duplication.
* Unsplittable exit behavior:
  * `RC == 1`: perform fallback boundary promotion on the returned graph at scope exit.
  * `RC > 1`: do not split; both lanes survive as a captured island (correct by RC lifetime).

**5. Scope Exit Fast Path (O(1) Splice)**
* If scope is splittable and `RC == 1`: splice `ESCAPE` chunks into parent in O(1), run/free `TEMP` lane resources.
* If scope is splittable and `RC > 1`: keep both lanes attached to the captured scope.
* No mandatory graph walk is allowed on the hot return path once the one-way invariant has been maintained during writes.

### Readiness
This is a coherent production baseline: deterministic lifetimes, no stop-the-world pauses, O(1) return transfer on the common path, and bounded fallback on dynamic boundary-heavy graphs.

**Confidence Score:** **0.92**.

---

## 21. Revision XIV (Implementation Closure and Runtime Status)

### Scope
This section records what is implemented in the runtime as of 2026-03-03 and
acts as the operational truth for engineering work.

### Implemented Status

**1. Dual-Lane Core Is Live**
* `ScopeRegion` uses split TEMP/ESCAPE allocation and splice-based return handoff.
* Normal return flow uses lane promotion + O(1) ESCAPE splice; the old
  `scope_adopt` path is retired from runtime flow.
* The runtime remains deterministic (RC/scope lifetime), with no stop-the-world GC.

**2. Boundary Promotion Context Is Shared**
* Promotion/copy operations run with a shared boundary context (epoch + memo)
  so partial work is reused instead of duplicated.
* Budgeted abort is enforced on boundary promotion paths; fallback copy runs in
  the same epoch context for deterministic reuse semantics.

**3. Root-Boundary Safety Is Explicit**
* Root promotion uses a dedicated boundary helper that preserves defensive-copy
  semantics for disjoint transient lifetimes.
* This prevents accidental unsafe reuse when a value is outside both releasing
  scope and surviving target chain.

**4. `copy_env_to_scope` Hot Churn Reduced**
* Env-copy now reuses one promotion context across recursive copy.
* Shared captured values are memoized at boundary copy sites.
* Existing target-chain values are reused directly instead of deep-copied.
* Closure/iterator deep-copy paths are gated by scope-chain membership.

**5. Telemetry Is Tightened**
* Dead copy-site buckets were removed and active copy-site IDs compacted.
* Copy-site counting uses `COPY_SITE_COUNT` bounds, and site storage matches
  the active count.

### Guardrails Implemented (Must Stay Green)
Memory lifetime regression suite includes explicit gates for:
* `lifetime: root-boundary promotion defends disjoint scope`
* `lifetime: cons-barrier fallback sites stay at zero`
* `lifetime: copy_env shared-value memo gate`
* `lifetime: jit/copy hot-site budget gate`
* `lifetime: tco frame-copy budget gate`
* `lifetime: promotion context memo gate`
* `lifetime: promotion abort fallback gate`

### Validation Baseline
Current verified baseline (2026-03-03):
* Unified tests: `1051 passed, 0 failed`
* Compiler tests: `73 passed, 0 failed`
* ASAN run: clean (`c3c build --sanitize=address` + runtime suite)

### Remaining Work Classification
No architecture-critical migration blockers remain for this plan revision.
Future work is optimization/tuning only (for example longer soak/benchmark runs),
not correctness migration.

**Operational Confidence:** **0.95**.

---

## 22. Revision XV (Fiber-Temp Completion Marker and Architecture Freeze)

### Scope
This section records the closure state as of 2026-03-05 for the fiber-temp and
boundary-hardening execution track.

### Closure Status

The project phase is complete for the intended architecture target:
* Region/boundary ownership remains authoritative (`TEMP`/`ESCAPE` +
  retain/release boundary policy).
* Fiber TEMP is implemented as an optional backing strategy for eligible TEMP
  allocations, guarded by `OMNI_FIBER_TEMP`.
* Stack teardown semantics are generic/defer-based and clone-safe.
* Scheduler/offload boundary regressions for wakeup ordering, payload ownership,
  cancellation, timeout, and interleaving are covered and green.

### Explicit Non-Goal (By Design)

Fiber TEMP is not a new ownership authority and is not intended to replace the
region model. Eligibility and bypass gates are expected behavior:
* `eligible_slow_allocs`
* `bypass_large_allocs`
* `bypass_escape_activity_allocs`

These counters indicate safe routing decisions, not incomplete migration.

### Validation Snapshot (2026-03-05)

* Normal full suite: `Unified 1212 passed, 0 failed`; `Compiler 73 passed, 0 failed`
* ASAN full suite: `Unified 1211 passed, 0 failed`; `Compiler 73 passed, 0 failed`
* ASAN + `OMNI_FIBER_TEMP=1` repeated soak: clean across repeated runs

### Operational Posture

No correctness blockers remain for this phase. Remaining work is optional:
test decomposition, CI ergonomics, and incremental perf tuning under existing
ownership guardrails.
