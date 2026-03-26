# Deduce Parallel Recursive First Runtime Shape Decision (2026-03-25)

## Decision

The first real parallel recursive execution slice will not run the existing
LMDB-writing component evaluators directly on worker threads.

The chosen first runtime shape is:

- same-wave recursive SCC batches only
- positive non-aggregate components only
- worker-local scratch computation only
- main-thread publish and validation

This is the first honest runtime widening after the already shipped topology
and admin-truth slices.

## Why This Shape

The current recursive executor is not worker-safe as-is:

- component evaluation mutates LMDB write transactions
- component evaluation also mutates shared relation-schema/admin state
- LMDB is single-writer, so naive same-wave worker fanout would either block
  behind one writer or become misleadingly “parallel” only in queueing
- derived write validation still needs the current main-thread integrity path
  for `key`, `unique`, `ref`, and `check`

That means direct worker-owned publish is not the right first slice.

## Chosen Execution Model

For the first runtime slice:

1. Build same-stratum, same-wave recursive batches from the existing SCC plan.
2. Restrict eligibility to positive non-aggregate recursive components whose
   runtime still uses the ordinary seminaive recursive path.
3. Fan out one worker per eligible component in the chosen batch.
4. Each worker opens its own read snapshot and computes candidate derived
   tuple additions/removals into worker-owned scratch buffers only.
5. The main thread joins workers, opens the normal write path, and publishes
   those component-local deltas in deterministic order through the existing
   validation/integrity machinery.

The existing scheduler/offload seam is already sufficient for that first
shape:

- worker callbacks can be queued through `OffloadWork.custom_fn`
- worker-owned scratch results can return through the existing
  `OFFLOAD_RES_BYTES` completion path as an internal serialized delta payload
- no new scheduler completion kind is needed for the first runtime slice

This keeps the first runtime widening honest:

- parallel compute happens off-thread
- DB mutation and admin counters stay serialized and truthful
- existing integrity checks remain authoritative

## Explicit Non-Goals For The First Slice

- no worker-owned LMDB write transactions
- no aggregate-bearing recursive components
- no negated recursive components
- no same-wave publish in parallel
- no claim that the whole recursive engine is parallel

## Rejected Alternatives

### Direct Worker-Owned Component Publish

Rejected because the current evaluator writes through LMDB and shared admin
state. With a single-writer store, this would be queued serialization with
extra complexity, not a truthful first parallel runtime shape.

### Topology Metadata Marketed As Runtime Parallelism

Rejected because `parallel-batch-topology` is already shipped as metadata and
must stay distinct from actual runtime execution.

### Aggregate-Or-Negated Recursive Components First

Rejected because those components already have more restrictive execution
semantics and would widen the first runtime slice too aggressively.

## Backlog Consequence

`B6.11b1` now specifically means:

- worker-scratch same-wave parallel recursive execution for positive
  non-aggregate SCC batches

`B6.11b2` is now closed with explicit public runtime/admin truth:
`parallel-runtime-mode = 'metadata-only` on `deduce/analyze`,
relation-local `deduce/stats`, and selector-scoped `deduce/explain`.

Current shipped progress inside `B6.11b1`:

- `B6.11b1a` is closed with the versioned serialized component-delta payload
  seam
- `B6.11b1b1` is closed with a single read-only seminaive scratch pass over
  the current component snapshot, producing that serialized payload without
  LMDB publish
- `B6.11b1b2a` is closed with multi-iteration scratch closure for the
  current single-recursive-atom positive SCC shape
- `B6.11b1b2b` is closed with broader positive multi-atom recursive SCC
  closure:
  - non-anchor recursive atoms in worker-scratch closure now read from
    `LMDB + prior-iteration worker-visible additions`, rather than being
    limited to single-recursive-atom rule shapes
  - same-iteration worker outputs stay invisible until the iteration closes,
    preserving the seminaive boundary instead of collapsing into same-wave
    naive re-read
  - a direct C-level Deduce regression now proves transitive multi-atom
    closure payloads serialize the full fixpoint additions
- `B6.11b1c` is closed with the main-thread publish/apply path:
  - serialized worker-computed component deltas now route back through the
    existing relation integrity checks and LMDB write path
  - a direct C-level Deduce regression now proves those serialized deltas can
    be published into the target relation on the main thread
- the remaining runtime work is broader worker-side recursive batch compute
