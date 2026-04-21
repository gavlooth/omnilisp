# Memory Tier 3 Evaluation - 2026-04-21

Status: decision note for the Tier 3 memory proposal items in
`memory/MEMORY_IMPROVEMENTS_PROPOSAL.md`.

## Scope

This note closes the proposal-scoped evaluation work for:

- `MEMORY-P3-INLINE-SMALL-COLLECTIONS`
- `MEMORY-P3-SCOPE-PAGING`
- `MEMORY-P3-PER-THREAD-SCOPE-POOLS`

It does not authorize immediate representation or threading rewrites. Those
changes are deliberately gated by telemetry/benchmark evidence because they
cross central ABI, ownership, and concurrency contracts.

## Current Runtime Facts

- `Value` is a 56-byte tagged union with `array_val` and `hashmap_val` pointer
  payloads in `src/lisp/value_core_types.c3`.
- `Array` is a 24-byte heap-backed payload with `Value** items`, `length`, and
  `capacity` in `src/lisp/value_runtime_types.c3`.
- `HashMap` is a 24-byte open-addressed heap-backed payload with external
  `HashEntry* entries` in `src/lisp/value_runtime_types.c3`.
- Array and hashmap destructors free the payload object and its external item or
  entry storage in `src/lisp/value_constructors_lifecycle.c3`.
- `ScopeRegion` is a 144-byte owner-thread-affine region with TEMP and ESCAPE
  chunk lanes in `src/scope_region.c3`.
- TEMP chunk growth currently doubles from `SCOPE_CHUNK_INITIAL = 512` to
  `SCOPE_CHUNK_MAX = 65536`; both TEMP and ESCAPE slow paths use the same
  doubling shape in `src/scope_region_allocators.c3`.
- Raw TEMP chunk allocation and fiber-temp fallback are centralized in
  `src/scope_region_chunk_helpers.c3` and `src/scope_region_temp_pool.c3`.
- `scope_create` currently uses a global freelist and global generation counter
  under `scope_global_lock` in `src/scope_region_lifecycle.c3`.
- Scope ownership is thread-affine via `owner_thread_token` and
  `scope_guard_owner`; cross-thread scope use is a hard invariant violation in
  `src/scope_region_global_guards.c3`.
- The JIT runtime is also owner-thread-only; `jit_require_owner_thread` guards
  process-wide JIT state in `src/lisp/jit_compiler_runtime_identity.c3`.
- The scheduler state is single-runtime-thread-owned; worker/offload
  completion crosses threads through scheduler mutex/CV/wakeup paths, not by
  handing live runtime scopes to workers.

## Decision Summary

### Inline Small Collections

Decision: do not implement inline `Array` or `HashMap` payloads in this wave.

Reason:
- The current proposal sketch does not fit the existing `Value` layout without
  increasing `Value.sizeof` or replacing pointer payload semantics. Either
  option is a runtime ABI change touching value copying, boundary promotion,
  JIT payload access, printing, destructors, JSON/tensor graph code, parser
  literals, and generic collection primitives.
- Small dictionaries are more complex than small arrays because open addressing
  stores key/value pairs, tombstone/removal behavior, canonical keys/values
  traversal, and mutation semantics in the `HashMap` payload.
- The most useful evidence is not available yet: allocation-size histograms by
  collection length and benchmark deltas for small collection-heavy programs.

Required next evidence before implementation:
- Add or collect telemetry for array length at construction, hashmap requested
  capacity/count at construction, and mutation growth events.
- Compare allocation bytes, destructor count, and runtime of collection-heavy
  benchmarks before changing representation.

Future implementation boundary if evidence supports it:
- Prefer an `Array`-only small-buffer optimization inside the `Array` payload
  first, behind an internal representation helper API, before touching
  dictionaries. Keep `Value` unchanged for that pilot.
- Do not add inline dictionary storage until hashmap lookup/set/remove and
  canonical traversal have a shared access layer that hides inline vs heap
  entries from callers.
- Do not treat lists as part of this item. Lists are currently cons spines, not
  a distinct small-container payload.

Validation required for a future implementation:
- `c3c build`
- basic and compiler Lisp slices
- array/dictionary core tests
- memory-lifetime smoke in the bounded container
- JIT array constructor/access tests
- primitive docs/source parity checks if any public behavior changes

### Scope Paging / Size Classes

Decision: do not implement TEMP lane size classes in this wave.

Reason:
- The current allocator has a single simple growth rule. Replacing it with
  4KB/16KB/64KB classes without workload evidence risks increasing memory use
  for short-lived scopes or reducing fiber-temp reuse efficiency.
- `runtime-memory-stats` now exposes enough scope/fiber-temp counters to start
  collecting evidence, but it does not yet expose a per-scope allocation-count
  histogram, slow-path requested sizes, or chunk-class hit/miss data.

Required next evidence before implementation:
- Add a benchmark or telemetry run that records:
  - slow-path allocation requested sizes,
  - selected chunk capacities,
  - per-scope `alloc_count`,
  - unused bytes at scope release/reset,
  - fiber-temp take/return/drop behavior by chunk capacity.
- Compare cons-heavy, env-heavy, tensor metadata, and ordinary basic/compiler
  slices under the current doubling policy.
- Add class-level telemetry before changing allocator policy; otherwise a
  smaller page can look better in total bytes while increasing chunk count and
  scope membership scan/teardown work.

Future implementation boundary if evidence supports it:
- Add chunk-class selection as an internal allocator policy only; do not change
  public language behavior.
- Keep ESCAPE lane conservative unless telemetry separately shows ESCAPE chunk
  fragmentation is the bottleneck.

Validation required for a future implementation:
- `c3c build`
- scope suite
- bounded container memory-lifetime smoke
- `tco-recycling` slice
- memory telemetry comparison before/after on the selected benchmark set

### Per-Thread Scope Pools

Decision: do not implement per-thread scope pools in this wave.

Reason:
- The current scope model is explicitly owner-thread-affine. A per-thread pool
  is feasible only if scope creation/release, generation stamping, freelist
  ownership, scheduler offload, and cross-thread return/transfer have one
  shared contract.
- The proposal's `tlocal ScopeRegion* g_thread_scope_pool` sketch is
  insufficient because scopes can be created, retained, released, and destroyed
  through runtime paths that assume `scope_guard_owner` and a globally unique
  generation counter.
- Cross-thread scheduler work currently needs a transfer policy, not just a
  faster freelist.

Required next evidence/design before implementation:
- Measure actual contention on `scope_global_lock` during scheduler and thread
  spawn stress tests.
- Define whether non-owner release is forbidden, queued to owner, or routed
  through a transfer pool.
- Define how generation stamps remain globally unique without reintroducing a
  hot global lock for every reused scope.
- Define ASAN behavior, teardown cleanup, and maximum per-thread hoarding.
- Define the relationship between any per-thread scope cache and the existing
  per-`StackCtx` fiber-temp chunk cache.

Future implementation boundary if evidence supports it:
- First add diagnostics for global scope lock contention and pool hit/miss rates.
- Then add a per-thread cache only for owner-thread create/release paths, with a
  global transfer fallback for scopes released by non-owner scheduler paths.
- Do not weaken `scope_guard_owner`; cross-thread transfer must be explicit.

Validation required for a future implementation:
- `c3c build`
- scope suite
- scheduler slice and scheduler concurrency stress tests
- bounded container memory-lifetime smoke
- ASAN build when practical

## Negative Constraints

- Do not increase `Value.sizeof` or rewrite collection payload semantics just to
  satisfy the inline-capacity sketch without benchmark evidence.
- Do not treat scope paging as a correctness fix. It is a tuning policy and
  needs before/after telemetry.
- Do not bypass `scope_guard_owner` or JIT owner-thread checks to make
  per-thread pools easier. That would convert a performance experiment into a
  concurrency contract change.

## Recommendation

The Tier 3 items are evaluated and should be considered closed at proposal
scope. The next useful work is measurement:

1. Add collection length/capacity telemetry and run collection-heavy benchmarks.
2. Add chunk-class allocation telemetry and run cons/env/tensor metadata slices.
3. Add scope global-lock contention telemetry and run scheduler stress tests.

Only after one of those measurements identifies a real bottleneck should the
corresponding representation or pool implementation be opened as a new
implementation item.
