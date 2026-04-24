# Stable Escape Graph Publication Plan (2026-04-23)

Status: Finalized Proposal
Owner: Codex
Source of truth: `memory/DESTINATION_ARENA_PLAN.md`, `docs/areas/memory-runtime.md`, `docs/areas/memory-runtime-cycle.md`

This remains a proposal-level design note, but the implementation queue now
starts from the live TODO items recorded for the shape-audit, store skeleton,
and prepared-publication slices below.

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

This note is therefore conclusive about direction, and the live implementation
queue is now listed below.

## Implementation Queue

The first live slices are intentionally narrow:

- `STABLE-ESCAPE-SHAPE-001`: measure the shapes that actually reach the escape
  boundary and publish a baseline summary. This slice is now landed and
  closed.
- `STABLE-ESCAPE-STORE-001`: introduce a runtime-private stable-handle store
  skeleton with generation checks. This slice is now landed and closed.
- `STABLE-ESCAPE-PREP-001`: route one boundary path through a prepared-graph
  publication hook and pin it with regression coverage. This slice is now
  landed and closed.
- `STABLE-ESCAPE-GRAPH-001`: replace the registry-only placeholder with the
  first real prepared graph representation in the stable store. This slice is
  now landed and closed for reusable `CONS` roots with scalar leaves and
  `ARRAY` roots with stable child-index sharing.
- `STABLE-ESCAPE-OBSERVE-001`: make prepared-publication outcomes observable so
  unsupported prepared graphs, raw compatibility fallback, alias-unsafe skips,
  and resolve failures cannot be mistaken for prepared success. This slice is
  now landed and closed.
- `STABLE-ESCAPE-DICT-001`: extend prepared graph metadata to dictionary roots
  by recording key/value child-index pairs in hash-table slot order while
  preserving shared value identity. This slice is now landed and closed.
- `STABLE-ESCAPE-SET-001`: extend prepared graph metadata to set roots by
  recording member child indices only, matching the boundary graph audit's set
  semantics. This slice is now landed and closed.
- `STABLE-ESCAPE-CYCLE-001`: pin cycle representation by verifying that a
  self-referential cons graph publishes back-edges to the already-registered
  root node. This slice is now landed and closed.
- `STABLE-ESCAPE-CLOSURE-001`: extend prepared graph metadata to closure roots
  by recording captured environment binding values up to, but not including,
  the global environment. This slice is now landed and closed.
- `STABLE-ESCAPE-MUTATION-001`: seal prepared graph structure at publication by
  making handles fail closed when current child pointers no longer match the
  prepared child-index snapshot. This slice is now landed and closed.
- `STABLE-ESCAPE-COMPAT-001`: make boundary commit results explicitly report
  whether they used the stable prepared-publication route or an older
  compatibility promotion/splice route. This slice is now landed and closed.
- `STABLE-ESCAPE-ARENA-001`: materialize prepared `CONS`/immediate-scalar
  graphs into a target build scope for TEMP returns before falling back to the
  older compatibility builder. This slice is now landed and closed.
- `STABLE-ESCAPE-ARENA-002`: extend prepared materialization to `ARRAY`,
  dictionary, set, untyped closure, string/error payload, and time-point roots
  or nested nodes. This slice is now landed and closed.
- `STABLE-ESCAPE-ARENA-003`: close the remaining prepared-materialization
  payload exceptions by cloning big-number handles and copying typed-closure
  signatures into the destination ESCAPE lane. This slice is now landed and
  closed.

No open stable-escape prepared-materialization rollout item remains. Future work
should be opened as a new semantic boundary if another runtime invariant or
capability gap appears.

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

## Current Landed Runtime Slice

The runtime now contains more than a handle registry:

- the stable store can materialize an internal prepared graph for reusable
  `CONS` roots whose reachable children are scalar leaves,
- the stable store can also materialize `ARRAY` roots while preserving shared
  child identity through repeated stable child indices,
- the stable store can materialize dictionary roots as key/value child-index
  pairs in hash-table slot order while preserving shared key/value identity,
- the stable store can materialize set roots as member child indices only,
  matching set language semantics rather than leaking backing true-values,
- cyclic container graphs are represented as back-edges to already-registered
  prepared node indices rather than recursive expansion,
- closure roots record captured environment binding values in local-to-parent
  frame order while excluding the global environment,
- prepared nodes keep stable child indices instead of only preserving the raw
  root pointer,
- prepared handles fail closed after structural mutation drift in prepared
  child pointers or captured binding values,
- the already-safe boundary reuse path runs through that prepared graph route,
- unsupported prepared graphs fall back through the raw compatibility path with
  explicit counters instead of silent fallback,
- boundary commit results expose whether a commit used the stable
  prepared-publication route, stable prepared materialization, or an older
  compatibility promotion/splice route,
- TEMP `CONS`, `ARRAY`, dictionary, set, closure, string/error payload,
  time-point, and big-number return graphs can materialize from prepared graph
  metadata into a target build scope before compatibility promotion is tried,
- tests can inspect the prepared node graph, publication outcome counters, and
  commit-route flags directly to prove the shape and route that were published.

The current shipped contract is still intentionally narrow:

```text
reusable ESCAPE cons, array, dictionary, set, or closure root
    -> prepare nodes for cons/array/dictionary/set/closure/scalar graph
    -> publish root handle + prepared node table
    -> resolve original root for current callers while structure matches snapshot
    -> fail closed after structural mutation drift
unsupported prepared graph
    -> count prepare-failure fallback
    -> raw compatibility publish/resolve route
```

This is the first real data-structure step, not the final generalized escape
graph arena.

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

The initial shape audit, store skeleton, prepared publication hook,
`CONS`/`ARRAY` prepared-node slice, fallback-observability slice, dictionary
prepared-node slice, set prepared-node slice, cycle-representation slice,
closure-env metadata slice, mutation-policy slice, compatibility-route
visibility slice, `CONS`/immediate-scalar materialization slice, and broad
prepared-materialization slice for arrays/dictionaries/sets/untyped closures,
and payload-exception closure slice for typed signatures and big-number handles
are closed. No open stable-escape prepared-materialization rollout item remains.

If a future slice cannot make one of those semantic boundaries observable in
code and tests, it should stay as design work rather than opening another
case-specific TODO.

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

Status: landed as an interpreter-owned stable registry with
publish/resolve/retire/reset operations and smoke coverage. The remaining live
boundary work starts at prepared publication.

### Phase 2: Graph preparation

- Add `prepare_for_escape(...)` with a memo table.
- Preserve sharing and cycles.
- Rewrite interior references into stable references.

Status: the first boundary path now routes through a prepare-gated stable
publication hook. Full graph rewriting remains a future expansion if the
runtime needs it for more complex shapes.

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

Continue the stable-handle prepared-graph model as the preferred boundary
architecture. Treat transplant-style boundary surgery as a compatibility path
only, and require explicit observability for every compatibility fallback so it
cannot masquerade as prepared publication success.

## Validation

Current closeout evidence for the observability slice:

- C3 diagnostics passed for touched runtime/test C3 files.
- Host shell build/test validation is currently blocked by
  `bwrap: loopback: Failed RTM_NEWADDR` before command execution.

When shell validation is restored, rerun:

- `c3c build --obj-out obj`
- `scripts/run_validation_container.sh env OMNI_TEST_VERBOSE=0 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
- `scripts/check_status_consistency.sh`
- `git diff --check`

Signature: GPT-5 Codex
