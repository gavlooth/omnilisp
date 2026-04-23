# Stable Escape Graph Publication Plan (2026-04-23)

Status: Finalized Proposal
Owner: Codex
Source of truth: `memory/DESTINATION_ARENA_PLAN.md`, `docs/areas/memory-runtime.md`, `docs/areas/memory-runtime-cycle.md`

This is a proposal-only note for now. It is not yet a live implementation plan
and does not have a TODO lane attached.

## Thesis

The current memory-boundary story is still framed as "transplant or copy the
escaping graph." That is the hard problem. A simpler model is to make escaping
values **publishable graphs** backed by stable handles and generation checks.

The boundary then becomes:

1. build locally,
2. prepare the reachable graph for escape,
3. publish it into stable storage,
4. return a stable handle,
5. resolve through the handle later.

This proposal is intentionally broader than a `StableIndexVector`-style
container because Omni must support nested composites, sharing, cycles,
closures, and foreign payloads.

## Decision Summary

The proposal decision is settled at the design level:

- Escaping values should be published as prepared graphs with stable handles,
  not transplanted raw pointer graphs.
- TEMP remains raw, mutable, and short-lived.
- ESCAPE becomes stable, handle-based, and generation-checked.
- Published graphs should default to sealed or mostly immutable semantics.
- Transplant/copy machinery remains only as a legacy compatibility path for
  cases that cannot yet be expressed as prepared stable graphs.

This note is therefore conclusive about direction, but still proposal-only:
it does not open a live implementation queue.

## External Patterns

The proposal is inspired by the same class of stable-handle structures used in
several existing systems:

- `stable_vec` documents stable indices with O(1) deletion and explicit index
  invalidation rules: https://docs.rs/stable-vec/latest/stable_vec/
- `dense-slotmap-mem` documents a generation-tracked stable-handle store with a
  dense array plus sparse bookkeeping:
  https://docs.rs/crate/dense-slotmap-mem/latest
- `deferred-map` documents deferred insertion plus generational handles for
  cyclic / complex builds:
  https://docs.rs/crate/deferred-map/latest
- `slotmap` documents the stable-key pattern directly:
  https://docs.rs/slotmap/latest/slotmap/
- Pointer swizzling is the classic publish/resolve model for replacing raw
  pointers with stable IDs:
  https://en.wikipedia.org/wiki/Pointer_swizzling
- Reference capability systems give the type-level shape of the same idea:
  https://arxiv.org/abs/2309.02983

The idea is not to copy any one library directly. It is to adopt the
stable-handle property and the generation check as the boundary contract.

## Core Boundary Idea

The boundary should not need to prove that a raw pointer graph can survive a
scope teardown. Instead, the runtime should turn the escaping graph into a
prepared, stable graph before publication.

That changes the boundary from:

```text
local temp graph -> transplant/copy -> parent owns raw pointers
```

to:

```text
local temp graph -> prepare -> stable publish -> caller holds handle
```

That is the simplest mental model for a dynamic runtime that still wants
deterministic ownership.

## Data Model Sketch

The proposed shape is a stable graph store, not just a stable vector.

```text
                +-------------------------------+
                |   StableGraphStore / Arena    |
                |-------------------------------|
                | slot 0 -> node A (gen 7)      |
                | slot 1 -> node B (gen 12)     |
                | slot 2 -> node C (gen 4)      |
                | slot 3 -> free                 |
                +-------------------------------+
                          ^         ^
                          |         |
                    handle |         | handle
                          |         |
        +-----------------+         +------------------+
        |                                                |
   caller gets                                   caller gets
   (table, slot, gen)                            (table, slot, gen)
```

Example handle:

```c
struct StableRef {
    uint table_id;
    uint slot;
    uint generation;
};
```

Prepared node metadata:

```c
struct StableNodeHeader {
    uint generation;
    uint flags;
    StableRef first_child;
    StableRef next_sibling;
};
```

The exact shape is still open, but the enforcement properties are fixed:

- a stale handle must fail closed,
- shared subgraphs must remain shared,
- cycles must survive preparation,
- complex values must keep their identity through publication.

## Escape Preparation Flow

```text
            local scope
                |
                v
        +-----------------+
        | build raw graph  |
        +-----------------+
                |
                v
        +------------------------+
        | prepare_for_escape(...)|
        | - memoize nodes        |
        | - assign stable slots  |
        | - rewrite child refs   |
        | - seal mutation policy |
        +------------------------+
                |
                v
        +----------------------+
        | publish stable root  |
        +----------------------+
                |
                v
        +----------------------+
        | caller resolves key  |
        +----------------------+
```

### Preparation rules

- Reuse a memo table so sharing and cycles do not explode.
- Preserve object identity through stable slot assignment.
- Rewrite interior references from raw pointers to stable references.
- Decide mutation policy at publish time, not ad hoc later.

## What This Buys Us

### Performance

- The boundary cost becomes a publish step instead of arbitrary graph surgery.
- Common returns can be constant-time handle publication after preparation.
- The system can budget preparation work explicitly instead of discovering it
  during teardown.

### Simplicity

- The runtime no longer has to preserve raw pointer shape across scope exit.
- The caller sees a stable reference, not a half-transplanted object graph.
- Stale references become generation mismatches, which are easy to diagnose.

### Correctness

- Cycles become a first-class preparation case rather than a special transplant
  case.
- Shared subgraphs remain shared because the memo table is part of the
  publication algorithm.
- Cross-scope safety is encoded in the handle contract, not in emergent
  pointer liveness.

## The Hard Parts

This does not eliminate boundary complexity. It relocates it.

The hard parts are:

1. preparing complex graphs without losing sharing or cycles,
2. deciding when a node must be sealed versus copied-on-write,
3. making the generation / validity story cheap enough for hot paths,
4. defining how foreign handles and raw native pointers participate,
5. preserving deterministic teardown for stable stores.

That is why the proposal is a graph store, not a “fancier vector.”

## Next-Step Boundary

The next action is a shape audit, not implementation.

That audit should answer:

- which escaping graphs are common enough to justify a prepared stable store,
- which of those graphs can be sealed by default without copy-on-write,
- whether closure environments should share the same stable store or use a
  narrow env-specific layer,
- and how much of the current transplant machinery can be reduced to a
  compatibility path only.

If the audit does not show a clear performance and simplicity win, this note
stays a proposal and no rollout should be opened.

## Omni-Specific Boundary Hypothesis

The runtime likely needs three layers:

1. **Local layer**: ordinary mutable construction inside the current scope.
2. **Prepared layer**: escape-eligible graphs with stable references.
3. **Resolved layer**: caller-visible handles that resolve through the stable
   store.

```text
local mutable graph
   -> prepare_for_escape
      -> prepared stable graph
         -> return StableRef
            -> caller resolve
```

If this works, the transplant architecture becomes an implementation detail for
legacy boundary cases, not the general strategy.

## Suggested Phases

### Phase 0: Shape audit

- Measure which escaping values are most common.
- Classify graphs by shape:
  - flat list-like,
  - array-like,
  - dict-like,
  - closure/env-like,
  - cyclic/shared graphs.
- Identify which ones are easiest to seal and publish.

### Phase 1: Stable store skeleton

- Add a stable-handle store with generation counters.
- Make stale handles fail closed.
- Keep the store private to runtime internals.

### Phase 2: Graph preparation

- Add `prepare_for_escape(...)` with a memo table.
- Preserve sharing and cycles.
- Rewrite interior references into stable references.

### Phase 3: Boundary integration

- Return stable references from selected escape paths.
- Keep raw local allocation for non-escaping values.
- Use the published stable store only when a value crosses the boundary.

### Phase 4: Mutation policy

- Decide which prepared nodes are sealed.
- Add copy-on-write only if mutation after publication is actually required.
- Keep the default policy simple: prepared graphs are mostly immutable.

## Snippet: Boundary Publication

```c
StableRef publish_escape(Value* value, Interp* interp) {
    PreparedGraph graph = prepare_for_escape(value, interp);
    if (graph.error != null) return stable_ref_error(graph.error);
    return stable_graph_store_publish(graph.store, graph.root);
}
```

## Snippet: Handle Resolution

```c
Value* resolve_stable_ref(StableRef ref, Interp* interp) {
    if (!stable_graph_ref_alive(interp->stable_store, ref)) {
        return raise_error_with_payload_names(
            interp,
            "memory",
            "memory/stale-handle",
            "escaped value reference is stale",
            null
        );
    }
    return stable_graph_ref_value(interp->stable_store, ref);
}
```

## Recommendation

Adopt the stable-handle prepared-graph model as the preferred boundary
architecture. Treat transplant-style boundary surgery as a compatibility path
only. Keep the note proposal-only until a separate TODO-backed rollout is
approved.

## Validation

This is a design note, so the only immediate gates are:

- `git diff --check`
- `scripts/check_status_consistency.sh`

Signature: GPT-5 Codex
