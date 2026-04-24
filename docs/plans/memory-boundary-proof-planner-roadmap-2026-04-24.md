# Memory Boundary Proof Planner Roadmap

Date: 2026-04-24
Status: active roadmap

## Objective

Maximize the TEMP/ESCAPE memory model by making boundary movement proof-driven:
reuse by stable index when the graph is already safe, transplant a whole region
when the entire ownership island can legally survive, materialize only when
those proofs are weaker than a copy proof, and fail closed otherwise.

## Active Hypothesis

The fastest correct boundary model is not "copy less" or "transplant more". It
is a planner that chooses the cheapest proven-safe outcome from one shared
decision ladder:

1. stable indexed publish/reuse,
2. whole-region transplant/splice,
3. prepared destination materialization,
4. fail closed.

The stable index container is therefore a graph passport and invalidation layer,
not a new ownership authority. `ScopeRegion` remains the owner of ordinary Omni
language values.

## Current Approach

Introduce a `BoundaryPlanner` layer that produces explicit decisions and reasons
before commit code mutates scope state:

```text
boundary_plan_commit(value, target_scope, releasing_scope)
  -> classify provenance
  -> prepare or lookup StableGraphPassport
  -> try stable publish/reuse proof
  -> try BoundaryTransplantProof
  -> try prepared materialization proof
  -> fail closed with reason
```

The planner must be observable. Every decision should expose the selected route,
the proof that made it legal, and the reason earlier cheaper routes were not
chosen.

## Core Concepts

### Stable Graph Passport

The stable store should converge from a handle registry into a graph passport:

```text
root handle: slot + generation
owner scope: retained ScopeRegion authority
owner generations: TEMP and ESCAPE generation stamps
prepared graph: stable node table + edge table + root index
epoch snapshot: root/container/env mutation epochs
policy summary: ValueTag ownership and edge policy
risk flags: FFI/native opacity, closure/env detach needs, unsupported wrappers
```

The passport lets boundary commit reuse a graph without copying when the owner
scope chain and mutation state still prove safety.

### Boundary Transplant Proof

Transplanting is legal only with an explicit proof object:

```text
source region is the right child/island for the target
owner thread and refcount constraints are satisfied
all reachable TEMP edges are inside the surviving island
closure env scopes are retained, detached, or inside the island
FFI/native edges are opaque or bridge-declared safe
mutation epochs are stable during planning and commit
```

If this proof fails, transplant is not a neutral fallback. The planner must use
materialization or fail closed.

### Prepared Materialization

Prepared materialization remains the universal safe route when reuse/transplant
proofs are weaker than graph-copy proof:

```text
prepare graph metadata
allocate destination shells in target build scope
clone heap/native-owned payloads by policy
wire edges by prepared child indices
copy/detach closure envs
commit atomically
```

### Mutation Epochs

Mutable graph-carrying values should expose cheap invalidation:

```text
ARRAY mutation_epoch
HASHMAP mutation_epoch
SET mutation_epoch
mutable CONS mutation_epoch
CLOSURE env_epoch / captured binding epoch
```

Stable passports store epoch snapshots. Resolve can fail closed on epoch drift
without deep-walking every edge.

### FFI Bridge Contracts

Foreign state must be declared before boundary traversal:

```text
OPAQUE_FOREIGN          no Omni traversal
FOREIGN_WITH_KEEPALIVE  retains ScopeRegion for callback/env lifetime
FOREIGN_WITH_COPY_HOOK  bridge supplies payload copy
FOREIGN_WITH_TRACE_HOOK bridge supplies Omni edge traversal
UNSAFE_FOREIGN          fail closed at boundary
```

Default remains opaque. No bridge declaration means no traversal, no stable
materialization, and no generic ownership claim.

### Copy Debt Telemetry

Every non-reuse route should report why cheaper routes failed:

```text
stable_publish_success
stable_publish_generation_miss
stable_publish_epoch_miss
stable_publish_owner_scope_miss
transplant_success
transplant_refcount_blocked
transplant_temp_edge_blocked
materialization_success
materialization_node_count
materialization_copy_bytes
fail_closed_reason
```

This turns optimization into a measured loop: reduce the largest copy-debt
buckets first.

## Implementation Phases

### Phase 1: Planner Skeleton

Add `BoundaryPlanner` decision structs and route-reason enums without changing
successful behavior. Commit paths should be able to ask the planner for a route
and still execute the existing implementation.

Closed 2026-04-24 by `MEM-BOUNDARY-PLANNER-001`: the planner decision API,
route/reason enums, commit-result planned/selected route fields, graph-audit
route logging, and behavior-preserving route-ladder tests are implemented.

### Phase 2: Stable Graph Passport

Promote the stable store metadata into a reusable passport shape with owner
scope proof, prepared graph summary, policy summary, and route reason fields.

Closed 2026-04-24 by `MEM-BOUNDARY-PASSPORT-001`: prepared stable-store entries
now carry a `StableGraphPassport` with owner-scope proof stamps, prepared graph
summary, policy/risk flags, and broad invalidation reason snapshots.

### Phase 3: Mutation Epoch Invalidation

Add epoch snapshots to passport validation for graph-carrying mutable values,
then route prepared-handle liveness through epoch checks before deeper shape
validation.

Closed 2026-04-24 by `MEM-BOUNDARY-EPOCH-001`: prepared nodes now snapshot a
pointer-keyed graph mutation epoch, env/hashmap/array mutation helpers stamp
their mutated frame/container, and passport validation rejects stale prepared
graphs before relying on the deeper shape walk. The pointer-keyed design avoids
invalidating unrelated prepared graphs when an independent env or container is
mutated.

### Phase 4: Transplant Proof Object

Replace ad hoc splice/transplant eligibility checks with a
`BoundaryTransplantProof` result. Transplant remains legal only when the proof
states that the whole source ownership island survives without leaking
non-surviving TEMP edges.

Closed 2026-04-24 by `MEM-BOUNDARY-TRANSPLANT-001`: scope-transfer legality now
builds a `BoundaryTransplantProof` with parent/child, refcount, owner-thread,
lane-shape, source-root, root-audit, closure-env, FFI-opaque, mutation, and
stamp-rewrite flags. Commit splice candidates consume a proof object instead of
the former standalone boolean precheck, while preserving existing transplant
success behavior.

### Phase 5: FFI Bridge Boundary Declarations

Represent bridge behavior explicitly: opaque, keepalive, copy hook, trace hook,
or unsafe. Keep `FFI_HANDLE` opaque by default and fail closed for ambiguous
foreign provenance.

Closed 2026-04-24 by `MEM-BOUNDARY-FFI-BRIDGE-001`: FFI boxes now carry an
explicit `FfiBridgeBoundaryMode`, defaulting to opaque. Boundary proof consumes
that declaration and fails closed for trace/copy-hook modes until bridge
traversal/copy hooks are implemented.

### Phase 6: Copy Debt Telemetry

Expose route decisions and copy debt through runtime memory stats so future
optimization follows measured failure reasons instead of ad hoc guessing.

Tracked by `MEM-BOUNDARY-COPY-DEBT-001`.

### Phase 7: Commit Path Migration

Migrate boundary commit paths to consume planner decisions as the source of
truth. Existing stable publication, destination materialization, and
compatibility routes should become planner-selected outcomes.

Tracked by `MEM-BOUNDARY-PLAN-MIGRATE-001`.

## Validation Path

Minimum validation per implementation phase:

- C3 diagnostics for touched runtime/test files.
- `c3c build --obj-out obj`.
- bounded `memory-lifetime-smoke`.
- bounded container Valgrind `memory-lifetime-smoke` for route/ownership
  changes.
- `scripts/check_status_consistency.sh`.
- `git diff --check`.

Broader validation is required when a phase changes shared wrapper, closure/env,
FFI, JIT/eval boundary, or mutation semantics.

## Negative-Memory Constraints

- Do not introduce per-language-value RC.
- Do not treat stable handles as ownership authority.
- Do not use root pinning as a general correctness escape hatch.
- Do not silently fall back from a failed proof to an unsafe compatibility path.
- Do not traverse FFI/native payloads without bridge declarations.
- Do not weaken prepared-graph or boundary-audit tests to make route selection
  pass.

## Next Checkpoint

The next concrete deliverable is `MEM-BOUNDARY-COPY-DEBT-001`: expose route
failure and copy-debt telemetry without changing route behavior.

## Agent Assignments

Single-agent local implementation unless explicitly parallelized later. If
parallelized, keep write scopes disjoint:

- planner API and route enums,
- stable passport metadata,
- mutation epoch wiring,
- FFI bridge declarations,
- telemetry and docs/tests.
