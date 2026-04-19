# session-34-44-boundary-hardening Part 05

Source: `docs/plans/session-34-44-boundary-hardening.md`

## Revision Draft (v3): Lifetime Teardown and Continuation Safety

Status:
- This section is a living revision draft for design decisions.
- It incorporates critical analysis of the multi-shot continuation cloning problem.

### Why This Revision Exists

- We still have teardown risk when suspended contexts are destroyed before normal call-scope epilogues run.
- Prior quick fixes created layer violations by leaking Lisp memory semantics (`ScopeRegion`) into stack engine internals.
- A newly discovered **latent danger** exists in multi-shot cloning: `memcpy` of the C-stack copies Lisp scope pointers but does not bump their Lisp-level refcounts, leading to silent double-frees upon eventual normal returns. We must solve the leak *and* the clone refcount problem together.

### Hard Constraints

- Keep stack engine generic. No direct `ScopeRegion` ownership logic in low-level coroutine core.
- Preserve deterministic cleanup behavior for finalizer-bearing values.
- Keep continuation clone behavior safe: absolute pointers must remain valid, and semantic ownership (refcounts) must be correctly duplicated.
- Avoid moving-GC complexity (no pointer rewriting of Lisp values on the C-stack).

### Candidate Architectures (Updated)

1. **Exception-driven unwinding (OCaml/Koka style)**
   - *Mechanism:* On destroy, resume the fiber with a special `UNWIND` signal. The JIT loop catches it and naturally unwinds through existing `scope_release` epilogues.
   - *Pros:* Zero hot-path overhead. Naturally scales to user-level `try/finally`.
   - *Cons:* High risk of violating thread-affinity if destructors run on scheduler threads. Requires deep JIT modification to separate standard errors from control-flow signals.

2. **Generic stack defer substrate with `DeferOps` VTable (Current Recommendation)**
   - *Mechanism:* Opaque defer registration (`stack_ctx_defer`) with `{ DeferOps* ops; void* arg }` semantics. `DeferOps` provides both `destroy(arg)` and `clone(arg)` callbacks. Storage strategy (intrusive stack nodes vs. inline slots) is an internal optimization choice.
   - *Pros:*
     - Solves the layer violation: `stack_engine.c3` only knows about function pointers.
     - Zero heap allocation on fast path (with suitable internal storage strategy).
     - Solves the latent clone double-free: `stack_ctx_clone` replays defer metadata and calls `ops.clone(arg)` (which the Lisp layer implements as `scope_retain`).
   - *Cons:* Mild hot-path setup cost (writing ~3 pointers per call scope).

3. **Fiber-Temp Arenas (The "Holy Grail")**
   - *Mechanism:* A 3-domain model (ROOT, ESCAPE/SCOPE, FIBER_TEMP). Ephemeral allocations bump-allocate from a chunk-pool owned by the `StackCtx`.
   - *Pros:* O(1) cleanup. Scopes naturally vanish when fibers die.
   - *Cons:* Highly complex interaction with multi-shot clones. Cloned fibers would need to freeze shared chunks and allocate from new chunks to avoid corrupting each other's state while preserving absolute pointers.

### Antithesis: The Smell in Candidate 2 (If Implemented as Intrusive C-Stack Nodes)

While Candidate 2 is the most mechanically sound immediate fix, it carries distinct architectural "smells" that we must acknowledge:

1. **Pointer Fixup Fragility:** Because `DeferNode`s live on the C-stack, a `memcpy` during `stack_ctx_clone` copies the `next` pointers exactly as they are. Those `next` pointers now point to the *original* fiber's stack memory, not the cloned stack. The stack engine will have to carefully walk and relocate these `next` pointers (similar to how it fixes `RBP` chains). This is delicate, unsafe C magic.
2. **The Heap Fallback Trap:** If a user ever allocates a `DeferNode` on the heap instead of the C-stack (e.g., inside a persistent coroutine object), the `memcpy` clone won't duplicate the node itself, but it *will* duplicate the pointer to it. Both the original and the clone will share the same heap node, destroying the intrusive list structure if either one unwinds. We would have to strictly enforce that `stack_ctx_push_defer` only accepts pointers within the `StackRegion` bounds.
3. **Semantic Mismatch:** Is "defer" solving the root problem, or just patching the symptom? The root problem is that Lisp-level call scopes are tethered to C-level control flow. The more we bind Lisp memory lifecycle to C-stack intrusive nodes, the harder it becomes to serialize continuations, move them across machines, or ever transition to a stackless VM architecture in the future.

### Recommended Sequence

**Phase A (The Mechanical Fix):**
- Adopt Candidate 2 (generic `DeferOps` substrate).
- Implement `stack_ctx_defer` and `stack_ctx_undefer` public surface.
- Define clone behavior via `DeferOps.clone` contract, independent of concrete defer storage.
- Use it to manage `ScopeRegion` release and clone-time retains.

**Phase B (The Structural Fix):**
- Once correctness is proven, evaluate Candidate 3 (Fiber-Temp Arenas) as an RnD track, not a committed endpoint.
- Any 3-domain migration proposal must first prove alignment with region-centric ownership guardrails before entering production roadmap.

### Open Questions (Resolved for this Revision)

- **Should discontinuation semantics be represented as a first-class runtime signal now, or deferred?**
  *Deferred.* Mechanical safety at the engine level (`DeferOps`) is required first.
- **What is the acceptable hot-path overhead budget for generic defer push/pop?**
  *Zero heap allocations on fast path.* Concrete storage remains an implementation detail; intrusive nodes are optional if clone-safety is proven.
- **Do we want clone-time duplication of defer metadata or shared immutable snapshots?**
  *Duplication via explicit `DeferOps.clone` hooks.* Stack clone flow must re-establish defer metadata deterministically, then let Lisp-layer `clone` hooks bump semantic ownership.
- **At what milestone does a fiber-temp domain become justified?**
  *Post-profiling, after boundary invariants hold.* Fiber arenas are the ultimate structural fix but require the boundary API to be perfectly solid first.

### Drift Audit (v3 vs. Omni Architecture)

Reference:
- `AGENTS.md` ownership/drift guardrails.
- `memory/DESTINATION_ARENA_PLAN.md` synthesis constraints.

Verdict:
- **Recommended Phase A path (generic defer substrate) is LOW drift** when kept strictly opaque.
- **Current v3 wording has LOW/MEDIUM drift risk** in one active place:
  - It frames Fiber-Temp as a likely structural endpoint; this needs explicit "RnD only" scoping to avoid ownership-model drift.

Specific drift findings:

1. Layering drift risk (medium)
- `DeferOps` itself is aligned.
- Risk appears when implementation details require stack-engine knowledge of node placement policy beyond opaque callback execution.
- Guardrail:
  - Keep stack API generic (`stack_ctx_defer(cb, arg)` + opaque storage).
  - Do not make "intrusive node on stack" part of public contract.

2. Ownership-model drift risk (medium/high if unchecked)
- Fiber-Temp language can drift toward a parallel lifetime authority.
- Guardrail:
  - Treat fiber temp as allocator backing only.
  - Lifetime authority remains region/boundary policy (`TEMP`/`ESCAPE`, retain/release invariants).

3. Clone mechanics drift risk (medium)
- Pointer-fixup-heavy design is mechanically possible but raises fragility and maintainability risk.
- Guardrail:
  - Clone semantics should be explicit, testable, and independent of node memory placement assumptions where possible.

4. Determinism drift risk (low if constrained)
- Finalizer determinism remains aligned if all finalizer-bearing objects stay in audited ESCAPE paths.
- Guardrail:
  - No FIBER_TEMP routing for finalizer-bearing values.

### Revision Actions (This Iteration)

- Keep recommendation as: **generic defer substrate first**, but relax storage strategy to "implementation detail".
- Add explicit note: intrusive stack nodes are an optimization candidate, not an architectural requirement.
- Keep Fiber-Temp in RnD lane only until:
  - boundary invariants are centrally enforced,
  - ASAN teardown matrix is stable,
  - clone/discard stress tests are green.

### Quick Go/No-Go Checklist

- Go:
  - Stack layer remains opaque and callback-driven.
  - Boundary layer owns ownership policy.
  - ASAN + targeted suspend/clone tests pass.
- No-Go:
  - Stack layer directly manipulates `ScopeRegion`.
  - New per-type RC appears for language graph values.
  - Fiber-Temp is introduced as a third ownership system.
