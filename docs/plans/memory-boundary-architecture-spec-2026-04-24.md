# Memory Boundary Architecture Spec

Date: 2026-04-24
Status: active implementation spec

## Objective

Make Omni's memory boundary model explicit enough that future runtime values,
foreign handles, closure captures, and root-store paths can be reviewed against
one contract instead of scattered switch statements.

This spec is normative for new memory/lifetime work. Historical proposals under
`memory/` remain useful context, but current implementation truth still lives in
`memory/CHANGELOG.md`.

Detailed rollout plan: `docs/plans/memory-boundary-proof-planner-roadmap-2026-04-24.md`.

## Core Ownership Model

- `ScopeRegion` is the owner of ordinary Omni language allocations.
- `ScopeRegion` lifetime is reference-counted at the region level.
- Ordinary language values do not own themselves through per-object RC.
- `TEMP` is the default lane for local evaluator work.
- `ESCAPE` is the lane for values that may survive the current dynamic
  boundary.
- `scope_adopt` is not part of normal return flow.

Region RC answers whether a whole scope island remains alive. It must not be
used as a substitute for explicit boundary materialization when a value is
moving from TEMP into a different surviving scope.

## Boundary Rule

A committed ESCAPE root must not retain any reachable Omni-owned edge into TEMP
unless the entire source region is intentionally retained as a surviving scope
island.

Boundary operations therefore have only these legal outcomes:

1. Reuse an already-safe value whose lifetime is proven to dominate the target.
2. Materialize a fresh graph in the destination ESCAPE lane.
3. Retain a whole source region intentionally, with the TEMP/ESCAPE mix treated
   as an unsplittable surviving island.
4. Reject/fail closed.

Silent fallback that hides a broken boundary contract is not allowed.

## Stable Index And Transplant Tier

Stable indexed publication and region transplanting remain part of the memory
model, but they are constrained fast paths rather than replacements for
materialization.

Use the stable store/index path when a root and its prepared graph can be proven
publishable from a retained owner scope chain. The stable handle records the
source graph shape and invalidation state; it does not create per-value
ownership authority.

Use region transplant/splice only when the whole source region can legally move
or survive as one ownership island. A transplant must preserve the boundary
invariant that committed ESCAPE roots do not retain reachable Omni-owned edges
into non-surviving TEMP storage.

The current implementation records this through `BoundaryTransplantProof`.
Scope splices, root splices, and commit splice candidates now share proof
fields for parent/child legality, source-root shape, root graph audit,
closure-env safety, FFI/native opacity, mutation applicability, and stamp
rewrite safety before mutating scope state.

If the proof is weaker than that, prefer stable materialization. Copying is not
the goal; preserving the explicit region ownership contract is.

## Boundary Planner Ladder

Boundary commit should converge on one explicit planner ladder:

1. Stable indexed publish/reuse if a stable graph passport proves the root is
   safe in a retained owner scope chain and mutation epochs still match.
2. Whole-region transplant/splice if a `BoundaryTransplantProof` proves the
   source region can survive as one ownership island.
3. Prepared destination materialization if reuse/transplant proofs are weaker
   than graph-copy proof.
4. Fail closed with a reason code.

The planner must report why earlier cheaper routes failed. Route choice should
be data, not implicit control flow.

## Stable Graph Passport

The stable index container should converge on a graph passport shape:

- stable root handle: slot plus generation,
- retained owner scope and generation stamps,
- prepared node/edge tables and root index,
- mutation epoch snapshots for mutable graph carriers,
- `ValueTag` ownership and edge-policy summary,
- FFI/native risk flags,
- route and invalidation reason counters.

The passport proves graph identity and liveness. It does not own ordinary Omni
language values.

## Mutation Epoch And Copy Debt Policy

Mutable graph-carrying values should expose cheap invalidation epochs. Prepared
handles should check epoch snapshots before expensive graph validation.

Every materialization should record copy debt:

- why stable publication failed,
- why transplant failed,
- graph node count,
- estimated copied payload bytes,
- selected route and failure reason.

Status 2026-04-24: `MEM-BOUNDARY-COPY-DEBT-001` exposes route selection,
fail-closed reason, graph-node, and estimated payload-byte counters.
`MEM-BOUNDARY-PLAN-MIGRATE-001` makes `boundary_commit_escape` dispatch from
planner-selected routes instead of mirroring legacy provenance branches.

The optimization loop is to reduce measured copy debt without weakening the
boundary invariant.

## Stable Materialization Contract

Prepared stable-escape materialization is the preferred boundary path for TEMP
graphs that must survive:

1. Prepare metadata for the source graph.
2. Allocate destination shells in a build scope's ESCAPE lane.
3. Clone or rebuild heap-backed payloads into the destination owner.
4. Wire destination edges by prepared child indices.
5. Validate the graph.
6. Consume promotion budget and commit atomically.

Materialization is not a raw byte copy. Every `ValueTag` needs a declared policy:

- immediate/by-value copy,
- heap payload clone,
- Omni-owned child graph traversal,
- closure/env/signature copy,
- opaque foreign handle policy,
- unsupported/fail-closed.

## FFI And Foreign Handles

Foreign handles are deliberately special, but they are not a loophole.

- `FFI_HANDLE` values are opaque to Omni graph traversal.
- Raw native pointers are not followed by TEMP/ESCAPE reachability scans.
- A foreign handle may use its own narrow payload reference count or release
  protocol, but only for the foreign wrapper/payload.
- Foreign handle RC/finalizers must not become ownership authority for ordinary
  Omni `Value` graphs.
- Foreign callback keepalive may retain a `ScopeRegion` when the callback
  closes over Omni values; that is region ownership, not per-value ownership.
- A foreign bridge that can store Omni `Value*`, `Env*`, or scope-owned pointers
  must expose explicit traversal/copy/release hooks or be boundary-unsafe.
- If FFI provenance is ambiguous, the boundary must fail closed or force an
  explicit bridge-level copy. It must not assume transitive safety through a
  native pointer.

Bridge declarations classify foreign state as opaque, keepalive, copy-hook,
trace-hook, or unsafe. Default remains opaque. The current runtime stores this
as `FfiBridgeBoundaryMode` on each `FfiHandle`; trace/copy-hook declarations
fail closed in boundary transplant proof until the corresponding bridge
traversal or copy hook exists.

Current implementation note: existing FFI wrapper construction is already tied
to scope teardown through `Value` destructors and foreign payload release
authority. Memory architecture work must preserve that behavior instead of
replacing it with generic language-value RC.

## ValueTag Policy Table

The runtime should converge on one source of truth for boundary ownership:

| Policy | Meaning |
| --- | --- |
| Immediate | Value has no owned payload or Omni child edges. |
| Heap clone | Value has heap-backed non-Omni payload that must be cloned or rebuilt. |
| Graph container | Value owns or references child `Value*` edges that must be traversed. |
| Closure graph | Value carries callable payload plus env/signature ownership. |
| Shared runtime wrapper | Value needs a specialized copy/retain path. |
| Opaque foreign | Value is native/foreign resource; no Omni traversal without bridge hooks. |
| Interpreter-stable | Value refers to interpreter-global metadata and may be reused when safe. |
| Unsupported | Boundary crossing must use an explicit specialized path or fail closed. |

Each new graph-carrying type must name:

- owned Omni edges,
- materialization/copy path,
- destructor authority,
- rollback behavior,
- graph audit coverage,
- FFI/native exclusion, if any,
- tests for return, closure capture, mutation/root store, and teardown.

## Current Implementation Slice

Closed 2026-04-24:

- `MEM-BOUNDARY-POLICY-001` added a runtime `ValueTag` boundary ownership policy
  helper, routed stable materialization and graph-audit edge decisions through
  it, and made `FFI_HANDLE` opacity explicit.
- `MEM-BOUNDARY-VERIFY-001` fixed the stable-store publishability and closure
  rollback blockers, leaving the bounded memory-lifetime smoke gate green.

Next roadmap:

- `MEM-BOUNDARY-PLANNER-001`
- `MEM-BOUNDARY-PASSPORT-001`
- `MEM-BOUNDARY-EPOCH-001`
- `MEM-BOUNDARY-TRANSPLANT-001`
- `MEM-BOUNDARY-FFI-BRIDGE-001`
- `MEM-BOUNDARY-COPY-DEBT-001` (closed 2026-04-24)
- `MEM-BOUNDARY-PLAN-MIGRATE-001` (closed 2026-04-24)
- `MEM-LIFETIME-TEARDOWN-001` (closed 2026-04-24): active child tracking and
  owned-root teardown sweep close retained descendant regions at interpreter
  destruction without adding per-value RC.

## Validation Path

Minimum validation for this slice:

- `c3c build --obj-out obj`
- targeted memory lifetime tests for boundary commit/stable escape
- bounded container Valgrind for memory-lifetime smoke when the validation image
  includes `valgrind`
- use `--trace-children=yes` when the Valgrind command wraps the runtime in
  `env`, otherwise the report may cover only `/usr/bin/env`
- `scripts/check_status_consistency.sh`
- `git diff --check`

For memory/lifetime behavior changes, prefer an ASAN pass when available:

- `c3c build --sanitize=address --warn-deprecation=no`
