# OmniLisp Memory System — Review Task List (CTRR + Region‑RC)
Date: 2026-01-10

This file is a **review backlog** for the OmniLisp memory system. It is intentionally exhaustive so you can pick/choose later.

Scope:
- **CTRR (Compile‑Time Region Reclamation)**: compile-time scheduling of region lifetimes and escape repair points.
- **Region‑RC (Per‑Region Reference Counting)**: runtime liveness is determined by **region-level** reference counts + scope liveness (no heap-wide collector).
- Building components: regions, per-region RC, tethering/borrowing, transmigration, concurrency + tooling.

Status legend (for this file):
- `TODO`: Candidate work item.
- `DEFER`: Only do if/when needed (explicitly justify).
- `N/A`: Not applicable (include a one-line reason).

Non‑negotiable constraints (global):
- No stop‑the‑world GC; no heap-wide scanning collectors.
- No language-visible “sharing primitive” (no `(share v)` or similar).
- Optimizations must remain **inside** the single authoritative CTRR/Region‑RC machinery (no bypass special-case walkers).
- Benchmark claims must use a reproducible protocol (see Issue 8).

Primary references (read before implementing *any* memory-model task):
- `docs/CTRR.md` (normative CTRR contract: “everything can escape”, Region Closure Property)
- `runtime/docs/ARCHITECTURE.md` (runtime architecture + model naming)
- `runtime/docs/CTRR_TRANSMIGRATION.md` (clone/trace contract, external-root rule, correctness invariants)
- `runtime/docs/CTRR_REMAP_FORWARDING_TABLE.md` (forwarding/remap identity)
- `docs/ADVANCED_REGION_ALGORITHMS.md` (long-term region/transmigration plans)

------------------------------------------------------------------------------

## Issue 1 — Formalize “Per‑Region RC” Semantics (Make the Model Explicit)

Goal: eliminate ambiguity about what Region‑RC means and what correctness guarantees it provides.

- [TODO] Label: R1-model-spec-core
  Objective: Write an explicit spec for the **per-region reference counting** model, including invariants, allowed pointer directions, and what CTRR must insert at compile time.
  Reference: `docs/CTRR.md`, `runtime/docs/ARCHITECTURE.md`
  Where:
    - New doc: `runtime/docs/REGION_RC_MODEL.md`
    - Update/align: `runtime/docs/ARCHITECTURE.md` (link to new doc)
  Why:
    “Per-region RC” only works robustly if we define:
    - what counts as an “external reference” to a region,
    - when a region may be reclaimed,
    - and how we prevent/handle region dependency cycles.
  What to write (minimum content checklist):
    1. **Region liveness rule:**
       - Region is alive iff `scope_alive == true || external_rc > 0`.
       - Region may free memory iff `scope_alive == false && external_rc == 0`.
    2. **Region Closure Property (runtime form):**
       - No pointers into dead regions.
    3. **External-root rule:**
       - Pointers outside the src region are treated as roots during transmigration and are not rewritten.
    4. **Region dependency DAG rule (see Issue 2):**
       - Define the “older-only store” rule or an equivalent rule that prevents region cycles.
    5. **Concurrency model hooks:**
       - define what operations may cross threads and what instrumentation/tooling is expected.
  Verification plan:
    - Add a short “spec conformance checklist” section to `runtime/docs/REGION_RC_MODEL.md`.
    - Ensure all existing docs link consistently (no contradictory naming).

- [TODO] Label: R1-define-external-ref-boundaries
  Objective: Define precisely which operations increment/decrement `external_rc` (or equivalent region-level refs).
  Reference: `runtime/docs/REGION_RC_MODEL.md` (to be written by R1-model-spec-core)
  Where:
    - New doc section: `runtime/docs/REGION_RC_MODEL.md` (“External references”)
    - Code touchpoints to list (do not implement here): `runtime/src/` modules for channel send/recv, closure capture, return, globals, etc.
  Why:
    Region‑RC is unsound if raw pointers can escape without RC updates; the runtime cannot observe arbitrary pointer copying.
  Implementation details (spec-level pseudocode):
    - On escape boundary (compile-time inserted):
      - `region_inc_external(root_region_of(value))` if value points into a region that outlives current scope.
      - `region_dec_external(...)` when the escape handle is dropped or transferred.
  Verification plan:
    - Spec must enumerate each escape boundary category:
      - return to parent/caller
      - closure capture
      - global store / module store
      - channel send/recv (cross-thread handoff)

------------------------------------------------------------------------------

## Issue 2 — Prevent Region Cycles (Region Dependency Graph Rule + Enforcement)

This is the largest “robustness unlock”. Per-region RC cannot reclaim cycles of regions, so we must prevent region dependency cycles by construction.

- [TODO] Label: R2-older-only-store-rule-spec
  Objective: Specify the **region dependency rule** that prevents region cycles (recommended: “older-only store”).
  Reference: `docs/CTRR.md` (Region Closure Property), `runtime/docs/REGION_RC_MODEL.md`
  Where:
    - New doc: `runtime/docs/REGION_DEPENDENCY_RULES.md`
  Why:
    Without an explicit rule, mutable Lisp updates can create:
    - older region objects pointing to younger region objects,
    - forming region cycles,
    - which per-region RC cannot collect.
  Spec content (minimum):
    - Define a region partial order (e.g., creation time / nesting depth).
    - Rule: a store into an object in region `R_old` may only reference values in `R_old` or an “older/equal” region (never a younger region).
    - If code attempts to store a younger pointer into an older object:
      - CTRR must either (a) transmigrate first, or (b) prohibit/guard with runtime barrier in debug builds.
  Verification plan:
    - Doc includes a table of allowed/disallowed cases with examples.
    - Include at least 2 examples showing how this prevents region cycles.

- [TODO] Label: R2-runtime-write-barrier-plan
  Objective: Design a runtime **store barrier** strategy for cases the compiler cannot prove (dynamic stores), without changing language syntax.
  Reference: `runtime/docs/REGION_DEPENDENCY_RULES.md`
  Where (planned implementation points):
    - `runtime/src/runtime.c` (or wherever `set-car!`, `set-cdr!`, `dict-set!`, array set, etc. are implemented)
    - `runtime/src/memory/` (helpers to map pointer→region id/domain)
  Why:
    The language is mutable; if we don’t enforce the region rule at mutation points, cycles/leaks appear.
  Implementation plan (pseudocode):
    - `store_slot(obj_in_Rold, slot, value)`:
      1. Determine region of `obj` (R_obj).
      2. Determine region of `value` if pointer into a region (R_val).
      3. If `R_val` is younger than `R_obj`:
         - Option A (preferred, transparent): `value2 = transmigrate(value, src=R_val, dst=R_obj_or_parent)` then store `value2`.
         - Option B (debug gate): abort with message explaining illegal store and suggesting transmigrate insertion.
  Verification plan:
    - Define tests that attempt illegal stores and confirm behavior (abort in debug OR auto-transmigrate if that’s the chosen contract).
    - Run under ASAN/TSAN once tooling exists (Issue 7/9).

------------------------------------------------------------------------------

## Issue 3 — Region Ownership + Cross‑Thread Handoff (Concurrency Robustness)

Pick a threading contract that matches “robustness + tooling”.

- [TODO] Label: R3-threading-contract-spec
  Objective: Specify the threading/ownership model for regions and objects (recommended: single-owner regions; cross-thread requires handoff/transmigrate).
  Reference: `runtime/docs/REGION_RC_MODEL.md`, `runtime/docs/ARCHITECTURE.md`
  Where:
    - New doc: `runtime/docs/REGION_THREADING_MODEL.md`
    - Update: `runtime/docs/ARCHITECTURE.md` (link)
  Why:
    TSAN usefulness and overall robustness depend on having a clear contract:
    - either transmigrate is called under a runtime lock,
    - or regions/allocations are thread-safe (harder).
  Spec requirements:
    - Define “region owner” (thread id) conceptually (even if not implemented yet).
    - Define allowed cross-thread operations:
      - channel send/recv of values with region transfer or region inc/dec.
    - Define how Region‑RC interacts with handoff (who decrements external refs).
  Verification plan:
    - Add doc examples showing safe handoff vs unsafe concurrent mutation.

- [TODO] Label: R3-channel-handoff-mechanics
  Objective: Specify how channels transfer values without STW GC and without language-visible share constructs.
  Reference: `runtime/docs/REGION_THREADING_MODEL.md`
  Where:
    - New doc section: `runtime/docs/REGION_THREADING_MODEL.md` (“Channels”)
  Why:
    Channel handoff is a primary real-world escape boundary. If it’s ambiguous, concurrency bugs will dominate.
  Plan options (document both; pick one):
    - Option A: **Ownership transfer** of an entire region (fastest, most robust).
    - Option B: **Transmigrate on send** into receiver’s long-lived region (safe, more copying).
    - Option C: **Shared region** with locks (risky; only if needed).
  Verification plan:
    - Provide at least one test case per option in the spec (source + expected behavior).

------------------------------------------------------------------------------

## Issue 4 — Atomic Policy Layer (Robust Concurrency + Tooling)

Even if we remain C99, concurrency robustness improves dramatically if all atomics go through one auditable layer.

- [TODO] Label: R4-omni-atomic-wrapper-design
  Objective: Design an `omni_atomic.h` abstraction so the runtime can use C11 atomics when available, or `__atomic_*` intrinsics otherwise.
  Reference: `runtime/docs/REGION_THREADING_MODEL.md`
  Where:
    - New header: `runtime/include/omni_atomic.h`
    - New doc: `runtime/docs/ATOMIC_POLICY.md`
  Why:
    Tooling (TSAN) and correctness review require explicit ordering semantics; ad-hoc intrinsics are easy to misuse.
  API sketch (example):
    ```c
    typedef struct { /* impl */ } omni_atomic_u32;
    uint32_t omni_atomic_load_acquire(const omni_atomic_u32*);
    void omni_atomic_store_release(omni_atomic_u32*, uint32_t);
    uint32_t omni_atomic_fetch_add_relaxed(omni_atomic_u32*, uint32_t);
    uint32_t omni_atomic_fetch_sub_acqrel(omni_atomic_u32*, uint32_t);
    ```
  Memory-order policy doc (must include):
    - which counters can be relaxed (most refcount increments)
    - which transitions require acquire/release (dec-to-zero path)
  Verification plan:
    - Add a small unit test (if tests infra permits) for atomic wrapper correctness (single-thread sanity).
    - Add TSAN gate tests later (Issue 9).

------------------------------------------------------------------------------

## Issue 5 — Tethering / Borrowing Hardening (Epoch + Tripwires)

Tethering exists to keep borrowing safe without GC. Robustness depends on making violations loud.

- [TODO] Label: R5-region-epoch-borrow-tripwires
  Objective: Specify a region “epoch” mechanism and BorrowedRef validation in debug builds.
  Reference: `runtime/docs/REGION_RC_MODEL.md`, `runtime/docs/REGION_THREADING_MODEL.md`
  Where:
    - New doc section: `runtime/docs/REGION_TETHERING.md`
    - Planned code: `runtime/src/memory/region_core.{c,h}`, borrow/tether APIs
  Why:
    Without epoch-style validation, stale borrows become silent UAFs; tooling catches them only sometimes.
  Spec plan:
    - Each region has `uint64_t epoch`.
    - On region destroy: `epoch++` and (debug) poison memory.
    - BorrowedRef captures `(region_id, epoch_snapshot, tether_depth/handle)`.
    - On dereference (debug): assert region still alive and epoch matches.
  Verification plan:
    - Tests:
      - borrowing across tether end triggers abort in debug
      - borrow after region exit triggers abort in debug

------------------------------------------------------------------------------

## Issue 6 — Regions (Allocator Policy, Inline Buffer, Accounting)

This is where you can win both robustness and performance without touching user code.

- [TODO] Label: R6-region-accounting-quotas
  Objective: Add a plan for per-region and global accounting (bytes, allocations, peak), and optional debug quotas.
  Reference: `runtime/docs/REGION_RC_MODEL.md`
  Where:
    - New doc: `runtime/docs/REGION_ACCOUNTING.md`
    - Planned code: `runtime/src/memory/region_core.c`, `runtime/include/omni.h`
  Why:
    Regions can “silently become long-lived” (retention cliff). Accounting makes issues visible.
  Plan details:
    - Track:
      - bytes allocated in region
      - chunk count
      - inline_buf usage
      - peak bytes
    - Optional debug quota:
      - abort with message “region exceeded quota; likely escape/retention”.
  Verification plan:
    - Provide a sample program and expected diagnostic output.

- [TODO] Label: R6-inline-buf-policy-and-domain
  Objective: Clarify inline_buf policy and ensure “in-region domain” includes inline allocations consistently (for bitmap, forwarding, and external-root classification).
  Reference: Phase 31 inline_buf coverage notes in `TODO.md`, `runtime/docs/CTRR_TRANSMIGRATION.md`
  Where:
    - `runtime/docs/REGION_DEPENDENCY_RULES.md` (domain note)
    - `runtime/docs/REGION_RC_MODEL.md` (domain note)
  Verification plan:
    - Tests must include objects allocated in inline_buf and ensure transmigration and domain checks behave correctly.

------------------------------------------------------------------------------

## Issue 7 — Transmigration: Structural Work (Correctness + Performance)

These refer to phases already in `TODO.md` (40–47). This file records the “why” and the dependency structure so it’s visible in one place.

- [TODO] Label: R7-transmigrate-edge-choke-point-priority
  Objective: Treat “single edge rewrite choke point” (Phase 42) as the prerequisite for all future perf work to prevent regression churn.
  Reference: `TODO.md` Phase 42
  Verification plan:
    - When Phase 42 is implemented, require a check that no edge path allocates WorkItem nodes in hot path.

- [TODO] Label: R7-forwarding-as-visited-priority
  Objective: Prioritize “forwarding table as visited+remap” (Phase 40) to reduce per-edge bookkeeping.
  Reference: `TODO.md` Phase 40
  Verification plan:
    - Must run under forced forwarding mode and demonstrate (via counters) that bitmap/remap fallback is not exercised in the hot path.

- [TODO] Label: R7-pair-microkernel-priority
  Objective: Prioritize `TAG_PAIR` microkernel (Phase 41) after Phase 42, to remove both clone and trace indirection for cons-heavy graphs.
  Reference: `TODO.md` Phase 41
  Verification plan:
    - Must include header-invariant tripwires and list/array regression benchmarks.

------------------------------------------------------------------------------

## Issue 8 — Benchmark Protocol (Reproducibility and Reportability)

Tooling matters: without reproducible benchmarks, performance work becomes narrative.

- [TODO] Label: R8-bench-protocol-spec
  Objective: Codify the benchmark protocol (compiler + flags + rebuild steps + reporting) as a doc and ensure every perf phase references it.
  Reference: Phase 39 protocol in `runtime/tests/Makefile`
  Where:
    - New doc: `runtime/docs/BENCH_PROTOCOL.md`
    - Update: `runtime/tests/bench_transmigrate_vs_c.c` (ensure it prints correct compiler/flags; avoid misleading “GCC 4.2.1” output if using clang)
  Why:
    “bench-rel” must be apples-to-apples: benchmark binary and runtime library must be built with consistent flags, or results are not comparable.
  Verification plan:
    - `bench-rel` output must print:
      - actual compiler ID and version
      - actual CFLAGS used to build *both* runtime and bench (or explicitly state if runtime was built differently)
      - mode name

------------------------------------------------------------------------------

## Issue 9 — Tooling Gates: ASAN/TSAN/UBSAN Focused on Memory Model

This is where “tooling matters” becomes concrete.

- [TODO] Label: R9-sanitizer-contract-tests
  Objective: Add focused sanitizer stress tests specifically for region lifecycle + tethering + transmigration invariants.
  Reference: `runtime/tests/Makefile` sanitizer targets, `runtime/docs/REGION_THREADING_MODEL.md`
  Where:
    - `runtime/tests/` (new focused tests)
  Why:
    Sanitizers are most useful when they target well-defined invariants rather than random workloads.
  Plan:
    - ASAN: stress alloc/exit/transmigrate loops; ensure no UAF/leaks.
    - UBSAN: ensure no UB in bitmap/index math.
    - TSAN: only if threading contract is explicit; otherwise TSAN will produce false positives.
  Verification plan:
    - Provide exact commands to run and what “pass” means (no sanitizer reports within the scope of the focused tests).

------------------------------------------------------------------------------

## Issue 10 — Repo Hygiene (Build Artifacts Policy)

This is not “runtime code” but it materially affects robustness of the workflow.

- [TODO] Label: R10-build-artifact-policy
  Objective: Stop tracked build artifacts from polluting diffs and accidental commits, or document/enforce a strict policy if artifacts must remain tracked.
  Reference: `TODO.md` Phase 48
  Where:
    - Decide policy in `runtime/docs/DEV_HYGIENE.md` (new) and align with JJ workflow.
  Verification plan:
    - After running tests/bench, `jj status` should not show modified tracked binaries/objects under normal workflows.

