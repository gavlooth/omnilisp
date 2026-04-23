# Memory, Ownership & Lifetime Notes

**[Back to Index](../OMNI_REFERENCE.md)**

---

This note records runtime-memory design proposals and research directions that
do not fit cleanly under a single language-surface chapter.

## Boundary Ownership Transplant Envelope

Status:

- proposal only
- not current runtime behavior

### Current Runtime Shape

Current escape handling is `ScopeRegion`-centric:

- short-lived values are allocated in a child scope's `TEMP` lane
- escaping values are promoted into the current `ESCAPE` lane or copied into a
  surviving lifetime
- ownership authority remains the region/runtime boundary, not a per-value
  handle system

This keeps normal local access cheap, but can impose structural copy cost at
escape boundaries.

### Proposal

Introduce a boundary-only ownership envelope for selected escaping values.

The envelope would:

- be allocated in a surviving scope
- be associated with an escaping root at the boundary
- carry ownership-transfer semantics rather than acting as a plain pointer
- coordinate an ownership transplant from the releasing scope to the new owner
- disappear after eager normalization, or survive briefly as a deferred
  normalization record

This is intentionally not a universal handle model. Normal in-scope values
would still be accessed directly.

### Intended Benefit

- reduce or defer deep-copy cost for expensive escaping values
- preserve fast direct access for ordinary non-escaping values
- keep boundary machinery local to the escape path instead of shifting
  indirection into the whole runtime

### Soundness Constraints

The envelope must carry or transfer ownership semantics. A pointer-only handle
is not sound.

The sound variants are:

- owner-retaining envelope
  - retains the old ownership island until release
  - easier to prototype
  - weaker semantic cleanliness because old ownership survives past the
    boundary
- owner-replacing envelope
  - completes a graph-level ownership transplant so the new owner becomes
    authoritative
  - semantically cleaner
  - harder to implement because all reachable ownership metadata must be
    rehomed correctly

For Omni, owner replacement is the better semantic target if the runtime can
perform the transfer without violating boundary invariants.

### Implementation Sketch

The likely implementation shape is a boundary-local transfer coordinator:

1. Boundary classification

- only enter this path for values marked as transplant-eligible
- default to current promotion/copy behavior for all other values

2. Envelope allocation

- allocate a small boundary object in the surviving scope
- record the escaped root, source scope, target owner, transfer mode, and
  transfer state

3. Reachability walk

- traverse the reachable graph from the escaped root
- reject the transplant if forbidden edges into child `TEMP` remain
- maintain a visited set for cycle-safe traversal

4. Ownership rewrite

- restamp or rehome every reachable transplant-eligible node under the new
  owner
- transfer destruction authority for the reachable graph
- clear old-owner authority for the transplanted subgraph

5. Finalization

- on success: child scope no longer owns the transplanted graph
- on failure: abort and fall back to the current promotion/copy path
- if deferred mode is used, force normalization before the first unsafe
  mutation/traversal

### Best Initial Scope

The proposal is strongest for selected value families where metadata transfer is
cheaper than structural copy:

- tensor/shared-wrapper-style values
- foreign-resource wrappers
- payload-heavy values with comparatively small ownership metadata

It is a poor initial fit for:

- generic cons/list graphs
- closure/env graphs
- mixed pointer-rich graphs whose destructor and ownership bookkeeping are
  heavily region-scoped

### Why This Is Narrow

For general Lisp graphs, ownership transplant can approach copy-cost class work:

- full graph traversal is still required
- ownership metadata still needs rewrite
- boundary validation still needs to prove no child-`TEMP` edges survive

So this proposal should be treated as a selective boundary optimization, not as
a new default ownership model.

### Related Precedents

- Pony / ORCA
  - capability-guided zero-copy actor/message transfer and runtime co-design
- Web transferables
  - boundary-local owner replacement for expensive resources such as
    `ArrayBuffer`
- Erlang refc binaries
  - payload-specific no-copy sharing across process boundaries

### Research Pointers

- Sylvan Clebsch, *Pony: Co-designing a Type System and a Runtime*
- Clebsch et al., *ORCA: GC and Type System Co-Design for Actor Languages*
- Negara, Karmani, Agha, *Inferring ownership transfer for efficient message
  passing*
- WHATWG HTML structured clone / transfer specification
- Fähndrich and DeLine, *Adoption and Focus: Practical Linear Types for
  Imperative Programming*

### Open Questions

- What exact runtime metadata must move for each transplant-eligible value
  family?
- Can transplant eligibility be decided structurally, or must it be explicit
  per tag/type family?
- Is eager transplant simpler and sufficient, or is deferred normalization worth
  the extra complexity?
- Can the current graph-audit machinery be reused to validate transplant safety
  before commit?

