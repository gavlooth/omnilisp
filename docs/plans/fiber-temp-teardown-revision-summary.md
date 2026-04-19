# Fiber Temp + Teardown Proposals (Revision Summary)

Date: 2026-03-05
Status: Draft with active execution checkpoint

## 1) Goal and Scope

Primary objective:
- Eliminate suspended-context teardown leaks and clone-related lifetime hazards without violating Omni's ownership model.

Secondary objective:
- Keep long-term room for faster temporary allocation strategies (fiber-local TEMP backing) without introducing a parallel lifetime system.

Out of scope for this draft:
- Full memory architecture rewrite.
- Any change that introduces per-type ownership models for language values.

## 2) Non-Negotiable Guardrails

These are hard gates for every proposal:

1. Region-centric ownership remains authoritative.
2. Stack engine stays generic; no direct `ScopeRegion` logic in stack core.
3. Finalizer-bearing values remain on deterministic ESCAPE/lifetime paths.
4. No root-pinning expansion as a general correctness workaround.
5. No new per-type RC ownership for language graph values (`Closure`, `Instance`, etc.).
6. Clone behavior must be explicit and testable (including ownership duplication semantics).

## 3) Revised Proposal Set

### Proposal A (Immediate): Generic Defer Substrate

Summary:
- Introduce generic cleanup registration in stack context (`stack_ctx_defer` / `stack_ctx_undefer`) with opaque callback + argument semantics.
- Lisp runtime registers boundary cleanups via this substrate.
- Stack destroy path executes cleanup callbacks in deterministic order.

Why now:
- Fixes correctness gap with minimal conceptual drift.
- Solves layering issue (no `ScopeRegion` ownership logic in stack engine).

Important revision from earlier drafts:
- Storage strategy is explicitly an internal optimization detail.
- Intrusive C-stack nodes are optional; not the architecture contract.

Clone semantics:
- Clone flow must re-establish defer metadata and call clone hooks (`ops.clone(arg)`) so semantic ownership (for example `scope_retain`) is correctly duplicated.

Risk:
- Low to medium (implementation bugs in clone/destroy sequencing are still possible).

Validation:
- `c3c build`
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- Targeted suspend/destroy + clone/discard stress cases.

### Proposal B (Later): Discontinuation/Unwind Signal

Summary:
- On destroy of suspended context, optionally resume in discontinuation mode and let runtime unwind normal epilogues.

Why not first:
- Requires stronger control-signal separation and thread-affinity discipline.
- Higher semantic blast radius than Proposal A.

Risk:
- Medium to high until signal/error channels are hardened.

Decision:
- Keep as a future design track, not immediate implementation.

### Proposal C (R&D): Fiber-Temp Backing Arena

Summary:
- Explore fiber-local temporary memory backing for TEMP allocations.

Critical constraint:
- Fiber-temp is only a TEMP allocation backing strategy, never a third ownership authority.
- ESCAPE paths remain the source of truth for long-lived/finalizer-bearing data.

Why R&D only:
- Clone interactions and boundary crossings are complex.
- Easy to drift from region-centric ownership if adopted prematurely.

Risk:
- High if moved into production without strong proofs and stress coverage.

Decision:
- Keep in research lane until Proposal A is stable and boundary invariants are centrally enforced.

## 4) Revisions Addressed (What Changed vs Earlier Brainstorm)

1. Layer violation concern addressed:
- Stack layer now defined as opaque callback substrate only.

2. Hot-path overhead concern addressed:
- Zero heap allocation target on fast path remains, but concrete storage strategy is not hardcoded in the contract.

3. Missing generic `defer` abstraction addressed:
- Proposal A formalizes generic defer as the immediate substrate.

4. Clone fragility concern addressed:
- Clone behavior is explicitly part of API contract via clone hooks and required stress tests.

5. Ownership drift concern addressed:
- Fiber-temp reframed as backing-only optimization, not ownership model.

## 5) Drift Assessment

Current drift posture:

- Proposal A: Low drift when implemented with opaque stack API and boundary-owned policy.
- Proposal B: Medium drift risk unless runtime control-flow semantics are standardized.
- Proposal C: Medium/high drift risk if promoted too early.

Key anti-drift checks:
- No direct `ScopeRegion` operations inside stack engine.
- No per-type RC ownership additions for language values.
- No production Fiber-temp adoption before invariants + ASAN/stress matrix are green.

## 6) Proposed Execution Order

Phase 1 (now):
- Implement Proposal A.
- Add targeted regressions for suspend/destroy, clone/discard, mixed boundary transitions.

Phase 2 (stabilize):
- Measure hot-path cost and optimize internal defer storage if needed.
- Keep external contract unchanged.

Phase 3 (evaluate):
- Revisit Proposal B only after control-flow signal policy is explicit.
- Continue Proposal C as R&D with explicit proof obligations.

## 7) Proof Obligations Before Any Fiber-Temp Production Work

1. Boundary invariants are centralized and enforced in tests.
2. ASAN runs clean on full suite for teardown/clone-sensitive paths.
3. Clone/discard permutation tests pass consistently.
4. Finalizer determinism remains unchanged.
5. No ownership-model exceptions leak into general runtime values.

## 8) Open Questions for Next Iteration

1. What exact defer callback ordering contract do we require under nested boundaries?
2. Should clone metadata replay be strict LIFO-preserving or logically grouped by boundary scope?
3. Which stress scenario currently gives the highest signal for teardown regressions?
4. What measurable performance budget (max overhead) do we accept for defer registration on hot call paths?

## 9) Execution Checkpoint (2026-03-05, Items 1/2/3 + 4 follow-up)

Requested:
- Complete items `1/2/3`, then finish `4` in the follow-up session.

Result:
1. Design freeze sign-off: **pass**.
2. Ownership/guardrail checkpoint: **pass**.
3. Defer hot-path perf sign-off: **pass**.
4. Follow-up completed: **pass**.

Validation run:
- `c3c build`
- `OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- `c3c clean`
- `c3c build --sanitize=address`
- `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
- Metrics pass:
  - `OMNI_STACK_DEFER_METRICS=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `STACK_DEFER_METRICS push=8 undefer=1 destroy_cb=9 clone_cb=1 update_arg=2 cloned_entries=2 peak_depth=3 heap_alloc=0`
- Follow-up verification:
  - Legacy stack-layer scope touchpoints removed from `src/stack_engine.c3` (`pinned_scope`, `stack_ctx_pin_scope`, `stack_ctx_unpin_scope`).
  - Suspend-lifetime scope retention now owned by runtime boundary helper (`suspend_with_scope_guard(...)`) on suspend sites.
