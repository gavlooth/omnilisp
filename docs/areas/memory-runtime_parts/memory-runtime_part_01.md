# memory-runtime Part 01

Source: `docs/areas/memory-runtime.md`

# Memory and Runtime

Status: `green` (boundary hardening, nested fail-closed wrapper promotion, bounded runtime/JIT gates, signal-handle rollback cleanup, lazy Tensor cleanup, structured-error payload cleanup, checked-hashmap rollback cleanup, transactional destination materialization, memory workload coverage, and scope allocator sequence telemetry are all currently validated)
As of: 2026-04-26

## Canonical Sources

- `memory/CHANGELOG.md` (**primary current-state source of truth**)
- `memory/DESTINATION_ARENA_PLAN.md` (target architecture + closure markers)
- `docs/BOUNDARY_ARCHITECTURE_AUDIT_2026-03-10.md` (Session 44 architecture audit + invariants contract)
- `docs/BOUNDARY_RUNTIME_AUDIT_2026-03-05.md` and `docs/BOUNDARY_SURFACE_AUDIT.md`
- `docs/areas/memory-runtime-cycle.md` (end-to-end architecture and cycle diagrams)
- `AGENTS.md` runtime invariants and ownership guardrails

Conflict rule: if current status in `DESTINATION_ARENA_PLAN.md` disagrees with
validated runtime behavior, follow `memory/CHANGELOG.md` and this area doc.

## Current State

- Dual-lane `ScopeRegion` (`TEMP` and `ESCAPE`) is implemented.
- Boundary facade is active (`boundary_*` helpers).
- `scope_splice_escapes` and `scope_reset_temp_lane` are live in runtime paths.
- `scope_adopt` is not present in current `src/` runtime callsites.
- Scoped finalize unification is live via `boundary_finalize_scoped_result(...)`, and eval/JIT finalize flows share that helper.
- Boundary state restore uses `BoundarySession` (`boundary_session_begin/end`) in audited helpers and regression probes.
- The stable-escape graph work is implemented through interpreter-owned stable
  store metadata, prepared publication, destination materialization, and
  bounded memory-lifetime smoke coverage.
- The memory-model improvement lane is closed. `MEM-MODEL-IMPROVE-006` is
  closed with product, closure-iterator, tensor-metadata, and nested-module
  return workloads. `MEM-MODEL-IMPROVE-002` added allocator histograms,
  per-scope sequence evidence, request/unused buckets, and source/site
  attribution; it closed without a chunk-policy change because the remaining
  ESCAPE no-follow-up bucket is synthetic direct benchmark traffic rather than
  runtime boundary/promotion pressure.
- The active next architecture roadmap is
  `docs/plans/memory-boundary-proof-planner-roadmap-2026-04-24.md`: centralize
  route choice in a proof-driven `BoundaryPlanner`, then expand stable graph
  passports, mutation epochs, transplant proofs, FFI bridge declarations, and
  copy-debt telemetry.
- Runtime intern and unhandled-effect payload guard cleanup is current as of
  2026-04-12:
  - `register_language_constants(...)` rejects failed `nil` symbol interning
    before defining the language constant.
  - JIT promise env matching treats failed env-tag interning as a non-match.
  - unhandled-effect raise payload construction uses the non-raising hashmap
    path and checks payload-key interning before publication.
- Parser AST call-argument sizing now fails closed before precomputed
  allocation sizes can wrap:
  - relation definition role/constraint argument counts, generic call
    argument arrays, FFI parameter counts, and selective import counts now
    guard their count arithmetic before AST arena allocation or capacity
    checks.
  - validation: bounded compiler slice `pass=191 fail=0`; bounded deduce
    slice `pass=330 fail=0`
- Parser AST array allocation now has a shared overflow guard:
  - `Parser.alloc_ast_array_bytes(...)` checks `elem_size * count` before arena
    allocation, and dynamic parser array allocations for calls, literals,
    patterns, type annotations, module bodies, lambda params, macro clauses,
    blocks, and pipe rewrites now route through it.
  - pipe rewrites also reject overflowing `arg_count + 1` before growth.
  - validation: bounded compiler slice `pass=191 fail=0`; bounded deduce
    slice `pass=330 fail=0`
- AOT type-definition and runtime FFI staging paths now fail closed on
  cleanup-state and allocation-size hazards:
  - AOT type fields and union variants are initialized before later fallible
    allocations can trigger deferred cleanup, and AOT field/variant/type-param
    allocation sizes are checked before allocation.
  - runtime FFI bound calls reject unsupported/narrowing argument counts and
    overflowing libffi staging buffers before preparing call storage.
  - validation: bounded compiler slice `pass=191 fail=0`; bounded
    memory-lifetime smoke slice `pass=189 fail=0`
- Deduce SCC and seminaive planning allocation bounds now fail closed:
  - SCC and reachability square matrices, stratum relaxation bounds, proof-key
    vectors, aggregate batch growth, decoded component-delta entries, and
    relation schema `count + 1` capacity requests now guard arithmetic before
    allocation or ensure-capacity dispatch.
  - validation: bounded deduce slice `pass=330 fail=0`
- Runtime promotion/copy allocation staging now rejects overflow and avoids
  partially published failure state:
  - boundary copy, ESCAPE promotion, root clone, env-copy, and sequence pattern
    matching now guard array/hashmap/method-table/signature/binding/element
    buffer sizes before allocation.
  - method-signature escape copies reset staged state on dependent allocation
    failure, env-copy allocates non-inline binding storage before publishing
    the copied frame, and closure escape promotion delays result wrapper
    publication until fallible clone/env work succeeds.
  - validation: bounded memory-lifetime smoke slice `pass=189 fail=0`
- Shared async string/array helper sizing now fails closed before allocation:
  - shared blob copies, process argv/env string tables, TLS duplicate strings,
    filesystem result array growth, and compression output buffers now guard
    `len + 1`, `count * sizeof`, and capacity doubling arithmetic.
  - validation: bounded async slice `pass=61 fail=0`; bounded scheduler slice
    `pass=111 fail=0`
- Async file I/O payload, temp-path, and read-buffer allocations now check
  `len + 1` sizing before allocation:
  - `write-file` internal offload payload construction rejects overflowing
    path/content payload lengths.
  - atomic temp-path construction rejects overflowing suffix addition.
  - read-file scratch buffers reject max-sized `cap + 1` allocation requests.
  - validation: bounded async slice `pass=61 fail=0`
- Scheduler offload completion projection now returns directly per completion
  kind, including `OFFLOAD_RES_NIL`, so async read-file/read-lines
  missing-path cases remap nil offload results to I/O payload codes instead of
  leaking `scheduler/offload-invalid-completion-kind`.
  - validation: bounded async slice `pass=61 fail=0`
- Scheduler non-fiber task join timeout cleanup now handles timer-start failure
  with single close authority: the explicit failure-path `uv_close(...)` clears
  the local timer pointer before the deferred cleanup runs.
  - validation: bounded scheduler slice `pass=113 fail=0`
- Signal-handle construction now rolls back registered runtime state on final
  `ForeignHandle` wrapper allocation failure:
  - the failure path detaches the native watcher, registry entry, and retained
    callback owner scope instead of leaving an unreachable registered handle.
  - validation: bounded memory-lifetime smoke slice `pass=210 fail=0`
- Lazy Tensor `ref` now cancels and destroys the concrete materialization
  temporary after extracting the scalar result:
  - active tensor destructor regression coverage verifies repeated lazy Tensor
    indexing does not leave a live temporary tensor destructor behind.
  - validation: host advanced collections module group `pass=203 fail=0`;
    bounded memory-lifetime smoke slice `pass=210 fail=0`
- Nested lazy Tensor `materialize` now cleans temporary child tensors created
  while resolving nested `map`/`contract` expression operands:
  - regression coverage materializes a nested lazy `map` expression and checks
    that only the top-level concrete result remains live until caller cleanup.
  - follow-up coverage also materializes a nested lazy `contract` expression
    and verifies failed lazy `map` materialization destroys the fresh concrete
    result tensor instead of leaving a live scope destructor.
  - validation: bounded memory-lifetime smoke slice `pass=215 fail=0`
- Structured error payloads are no longer built unless a matching `raise`
  handler can consume them:
  - ordinary unhandled `io_raise(...)` failures no longer leave root-scoped
    payload dictionaries behind.
  - `process-wait` result projection also cleans a partially built result
    hashmap when insertion fails.
  - `make_raise_payload(...)` also cleans a partially built root dictionary if
    payload key/value construction fails mid-build.
  - checked hashmap insertion now also rolls back newly promoted root key/value
    copies if publication fails before a slot is claimed, including the
    full-table/no-slot failure path.
  - validation: bounded memory-lifetime smoke slice `pass=216 fail=0`
- Lazy Tensor `materialize` into an explicit destination is now staged:
  - lazy `map`/`contract` expressions evaluate into a temporary concrete
    Tensor first and copy into the caller destination only after success, so a
    failed element computation leaves the destination unchanged.
  - contract destination/source aliasing is still rejected before staging.
  - failed Tensor constructor data validation now cleans the unreturned tensor
    wrapper instead of leaving a live scope destructor.
  - validation: host advanced collections module group `pass=206 fail=0`;
    bounded memory-lifetime smoke slice `pass=216 fail=0`
- I/O and string-buffer growth paths now fail closed on overflow-before-
  allocation and invalid capacity state:
  - console capture/copy, input-state append, CSV field/row growth, REPL
    session strings/capacity, and Unicode case-mapping append growth all use
    checked sizing before mutating live buffers.
  - validation notes from the latest pass:
    - `c3c build --warn-deprecation=no`
    - bounded unicode slice: `pass=25 fail=0`
    - bounded data-format slice: `pass=59 fail=0`
    - bounded async slice after scheduler nil-completion projection fix:
      `pass=61 fail=0`
    - bounded advanced unicode iterator group: `pass=129 fail=0`
- Nested `CONS` / `PARTIAL_PRIM` / `ITERATOR` boundary-copy and ESCAPE
  promotion paths now fail closed when they encounter opaque primitive payload
  state transitively; they no longer silently embed null/error payloads into
  rebuilt structured wrappers.
- Shared-wrapper abort paths for `ARRAY`, `HASHMAP` / `SET`, and
  `METHOD_TABLE` now unwind already-copied child ownership side effects before
  freeing the partially built heap wrapper, so failed late-element copy does
  not leak retained closure env scopes or other copied child resources.
- That cleanup path now also descends through copied nested `CONS` shells, so
  partial cleanup remains correct even when the already-copied retained child
  was itself wrapped in another copied structured value before the later
  failure.
- Shared rollback/partial-cleanup helpers now tombstone the copied target-scope
  dtor entry before manually unwinding a materialized child, so env-copy,
  ordinary boundary copy, root-store clone, and ESCAPE-promotion late-failure
  cleanup no longer consume copied closure/env retains and then replay the
  same destructor again during later scope teardown.
- The same late-abort cleanup contract now also holds for root-store
  `ARRAY` / `HASHMAP` / `SET` clone routing, not just ordinary boundary copy
  and ESCAPE promotion.
- Destination-escape commit builders now fail closed on boundary-generated
  nested child copy faults: `CONS`, `PARTIAL_PRIM`, and `ITERATOR` destination
  builds bubble those faults back out as top-level error commits instead of
  rebuilding wrappers that contain embedded boundary `ERROR` children.
- Env-copy iterator rollback now unwinds already-copied iterator payloads even
  when the payload was materialized as a copied `PARTIAL_PRIM`, so wrapper
  construction failure does not leave nested closure retains live in the target
  scope.
- Env-copy mid-frame rollback now descends into copied `CONS`, `ARRAY`,
  `HASHMAP` / `SET`, and `METHOD_TABLE` bindings as well, so nested copied
  closure/env retains are unwound immediately when one later binding fails
  instead of surviving until the abandoned target scope is released.
- Env-copy rollback now also tombstones the copied target-scope dtor entry
  before manually unwinding a materialized value, so failed env-copy no longer
  consumes a copied closure/env retain during rollback and then replays the
  same destructor again when the abandoned target scope is later released.
- Copied/promoted `CONTINUATION` wrappers now carry an explicit
  boundary-owned marker when they were the wrapper that first retained
  handle-state for boundary escape, and env-copy/partial-cleanup rollback uses
  that marker to release the retained handle-state immediately when the copied
  wrapper is abandoned.
- The same continuation ownership contract now covers successful teardown and
  post-retain allocation failure as well:
  - boundary-owned continuation wrappers register normal scope dtors when they
    actually introduced the retain,
  - normal target-scope teardown releases that retain exactly once, and
  - copy / ESCAPE allocation failure after the retain step now releases
    immediately instead of depending on a wrapper that never got materialized.
- `jit_resolve_value(...)` now decrements shared continuation handle-state
  refcount only when the resolved continuation actually owned the retained
  slot, so shared handle-state continuations no longer consume each other’s
  retains during `resolve`.
- Handled-effect and capture dispatch now fail closed on continuation
  allocation failure instead of dereferencing null continuation state:
  `alloc_lisp_continuation(...)` exposes a narrow failure seam, and both
  handle/capture dispatch lanes return explicit runtime errors when the
  continuation wrapper cannot be materialized.
- Runtime effect publication no longer degrades payload-construction failure
  into normal business-visible handled/unhandled effect results:
  handled raises now reject payload map construction failure before handler
  bind, and unhandled-effect diagnostics now return
  `"runtime effect payload: out of memory"` instead of silently dropping the
  structured payload.
- `make_array(...)` is now just the checked array constructor contract, so raw
  array creation no longer bypasses allocator failure handling and return a
  partially initialized wrapper.
- Iterator terminal and coroutine helper paths now reject malformed iterator
  tails consistently:
  - `collect` / `to-array` no longer silently truncate malformed iterator
    state as normal completion.
  - `map`, `filter`, `take`, `zip`, and `foldl` now reject malformed iterator
    tails directly instead of truncating or deferring broken iterator state as
    a later partial success.
- Iterator/coroutine malformed internal state is now fail-closed as well:
  - missing internal thunk args for list/array iterator sources and lazy
    iterator combinators no longer normalize to `nil`,
  - null iterator pairs returned from internal thunk dispatch no longer look
    like clean exhaustion to terminal consumers, and
  - `resume` no longer fabricates successful `nil` values when a coroutine is
    missing its yielded value or completed result.
- Iterator coroutine helpers now also fail closed on internal cons
  construction:
  - `zip` no longer publishes constructor-failed item pairs as iterator data.
  - `foldl` no longer degrades arg-list constructor failure into later
    malformed-apply errors.
- Scheduler OS-thread completion now fails closed on double-allocation failure:
  - if both the original completion and the alloc-failure completion are
    unavailable, `scheduler_complete_os_thread(...)` now drops the OS-thread
    entry and wakes the waiter instead of leaving a running entry stranded.
- Pending raise publication and dispatch now fail closed on payload/message
  materialization failure:
  - pending raise state no longer stores `boundary_promote_to_root(...)`
    null/error results as ordinary payload.
  - JIT handle raise dispatch no longer binds or applies fallback
    `make_string(...)` / `make_cons(...)` constructor failures as handler
    arguments.
- `deduce 'match` result dictionary materialization now uses checked hashmap
  construction and checked insertion, so match-result building no longer
  dereferences a raw hashmap shell or embed builder `ERROR`s into a successful
  result list.
- The remaining guarded deduce/unify hashmap-constructor family is now closed:
  - row materialization, integrity payload builders, deduce runtime helper
    state maps, deduce explain/schema/analyze payload maps, and deduce
    why-result path/payload builders no longer use raw `make_hashmap(...)`
    in `src/lisp/deduce_*` / `src/lisp/unify_*`.
  - scoped grep verification is now clean for that family, and the bounded
    `deduce` slice passes on the shipped tree (`pass=328 fail=0`).
- That deduce payload family now also treats checked insertion failure as a
  first-class error:
  - explain/analyze/schema/stats/why-result payload builders no longer ignore
    `explain_dict_set*` return values after checked constructor success.
  - insertion/grow failure now propagates as the typed deduce/schema explain
    error already owned by the enclosing builder instead of returning a
    partially populated payload map.
- Fast reuse for target-chain shared wrappers now walks nested `ARRAY`,
  `HASHMAP` / `SET`, and `METHOD_TABLE` payload edges before returning wrapper
  identity, so target-chain reuse no longer aliases a wrapper whose nested
  child still points into the releasing scope.
- Target-chain `CONS` reuse now applies the same rule to each nested cons
  shell in the cdr spine, so boundary copy, ESCAPE promotion, and env-copy no
  longer reuse an outer cons by identity when a nested cons wrapper still
  belongs to the releasing/source scope but only exposes scalar leaves.
- `PARTIAL_PRIM` / iterator reuse now applies the same nested payload check to
  `first_arg` / `second_arg`, so target-chain partial wrappers no longer alias
  a shared-wrapper argument whose nested child still belongs to the
  releasing/source scope.
- JIT TCO env-frame transfer now applies that same nested alias-safety rule
  before keeping a target-chain binding by identity, so target-chain
  partial/iterator bindings no longer bypass copying when their shared-wrapper
  args still point into the releasing scope.
- The shared fast-reuse precheck now applies the same rule to foreign
  `PARTIAL_PRIM` wrappers as well, so JIT TCO copy cannot bypass nested
  releasing-scope payload checks merely because the wrapper itself sits in a
  sibling/foreign scope.
- JIT TCO env-frame copy now also treats any disjoint graph-carrying wrapper
  as copy-required, aligning the JIT lane with the normal boundary/env-copy
  rule that disjoint `ARRAY` / `HASHMAP` / `SET` / `METHOD_TABLE` / similar
  wrappers are not reused by identity.
- The TCO recycle fast-reset gate now walks nested graph payloads
  transitively before calling `scope_reset_temp_lane(...)`, so an owner-scope
  or target-chain wrapper that still points into the recycle-scope TEMP lane
  can no longer survive an in-place reset merely because the outer wrapper
  shell itself lives outside TEMP.
- The supporting TCO graph scanner no longer carries its bounded traversal
  state on the runtime stack. `jit_graph_binding_reaches_temp_scope(...)` now
  allocates one heap-backed scan state object, closing the full-slice
  `jit-policy` crash where the previous four `4096`-entry local pointer arrays
  could overflow a smaller runtime stack before the scan body even ran.
- Checked collection mutators now fail closed on grow pressure too:
  - `hashmap_set(...)` / `hashmap_set_symbol(...)` no longer discard their
    checked insertion result,
  - `set!` on dictionary targets and `set-add` on sets now raise
    `runtime/out-of-memory` instead of returning `Void` after silently
    dropping the write.
- Core symbol/type registry growth now fails closed on invalid sizing and
  unavailable replacement allocation:
  - symbol table and type registry init/grow paths now reject overflowed
    table/hash byte-size arithmetic before allocation and avoid mutating live
    table state until replacement storage is confirmed.
- Match/gensym and collection grow helpers now gate invalid capacity state:
  - match-result binding and gensym mapping growth now guard doubling and
    `sizeof * capacity` overflow before replacing backing arrays,
  - hashmap setters/growers now validate power-of-two capacity invariants
    before probing/growing so invalid capacity state cannot silently corrupt
    probe masking.
- Deduce direct schema/rule mutation allocation sites now fail closed:
  - rule-signature registration rejects overflowing count increments,
    offset/count ranges, and direct metadata array allocation sizes before
    copying,
  - relation schema/index initialization rejects overflowing column/index
    allocation sizes before publishing schema counts,
  - transaction insert accounting increments only after tuple delta append
    succeeds.
- Schema-explain list assembly now fails closed too:
  - helper-level list accumulation and reversal for signature params,
    constraints, arg types, handler tag lists, and candidate lists now route
    through one checked prepend helper,
  - so schema-explain payload builders now return the existing
    `"schema explain: out of memory"` error instead of hard-aborting on an
    internal cons allocation failure.
- Pure string/list materializers now fail closed on `make_string(...)` too:
  - `string-upcase` / `string-downcase` no longer mutate a constructor
    `ERROR` result,
  - `List(String)`, `string-split`, `string-graphemes`, and `read-lines`
    now propagate per-element string-construction failure directly instead of
    returning successful lists that embed `ERROR` values as ordinary items.
- Top-level string-backed runtime payload/list helpers now follow the same
  fail-closed rule:
  - `(shell cmd true)` no longer returns `(ERROR exit-code)`,
  - `fs-readdir` no longer stores `ERROR` directory entries,
  - `http-get` / `http-request` no longer pass string-constructor `ERROR`
  values into transport setup/write,
  - `schema-explain` no longer returns a singleton explanation list whose
  only element is an `ERROR`.
- Scheduler wakeup publication now fails closed too:
  - timer, sleep, and poll-error callbacks no longer drop blocked-fiber
    completion when reliable wakeup enqueue fails,
  - non-task offload worker completion no longer frees the payload and strands
    the waiter on the same enqueue-failure seam,
  - those paths now fall back to the same direct wakeup handlers used by the
    drained queue path.
- Scheduler non-task offload completion now also handles the double-allocation
  failure case explicitly:
  - if both the worker callback and fallback alloc-failure completion return
    `null`, the waiter is still woken with a completed-null slot,
  - consuming that slot returns `"offload: missing completion"` and clears the
    pending offload state instead of leaving an active completed slot behind.
- Deduce/JIT capacity growth now fails closed before byte-size arithmetic can
  wrap:
  - relation schema vectors, aggregate groups, delta tuple sets, query-demand
    case storage, transaction mutation logs, incremental dirty predicates,
    rule signatures, relation-schema tables, and persisted-rule-catalog tables
    now guard `sizeof * new_cap` before `malloc` / `realloc`,
  - source-dir vector growth in module setup rejects oversized capacities and
    length increments before allocation-size arithmetic can wrap.
- Runtime module export publication now fails closed on export-table growth
  failure:
  - module export allocation/growth is routed through a checked helper with a
    deterministic failure seam,
  - re-export and implicit module definition export paths now propagate growth
    failure instead of writing through a null replacement table,
  - failed export growth preserves the previous export table and count.
- The next overflow-hardening batch is closed:
  - persisted deduce rule signature/catalog record sizing and restore cursor
    movement now use checked add/mul/cursor helpers,
  - AST arena alignment/chunk accounting and parser import/module/export
    growth now fail closed before allocation-size arithmetic can wrap,
  - JIT arg staging, effect handler clause arrays, handle-state snapshots,
    interpreter macro/module/handler tables, env bindings, and method-table
    growth now reject overflow before allocation or table mutation.
- Collection/apply array helpers now fail closed on both constructor and
  boundary-promotion faults:
  - `array`, `list->array`, `set!` on arrays, `push!`, `collect`, and
    `to-array` no longer treat `boundary_promote_to_root(...)` failure as
    ordinary data,
  - `push!` no longer null-dereferences on grow allocation failure,
  - `sort-by` no longer swallows comparator runtime errors and return a
    partial sort.
- The staged internal collection-constructor OOM migration is now closed
  across the live runtime-facing families:
  - shared constructor substrate,
  - data-format bridges,
  - schema-explain payload builders,
  - runtime/status payload builders.
- Process/fs status payloads, dispatch auxiliary payload builders, and ctor
  mismatch payload helpers now fail closed under collection-constructor OOM
  instead of mutating unchecked `HASHMAP` / `ARRAY` results.
- Destination-builder memo entries are now explicitly treated as temporary
  build-scope state: nested child routing may memoize within one builder
  invocation, but those memo nodes are discarded when the builder returns or
  aborts and are not part of same-epoch reuse semantics.
- The same builder-local teardown now restores the small scope-chain cache
  snapshot too, so temporary destination-build membership probes do not leak
  stale cache entries back into the caller’s longer-lived `PromotionContext`.
- Destination iterator detachment now participates in that same builder
  promotion context instead of bypassing it on the detach subpath, so
  destination-built iterator copies reuse same-epoch memoized children and do
  not silently drift around context budget or abort state.
- Direct destination escape promotion now also routes through the explicit
  caller-owned `PromotionContext`, so releasing-scope retry, mixed-destination
  retry, and direct destination promotion all participate in one coherent
  memo/budget/abort epoch instead of falling back to ambient interpreter
  state.
- Shared-wrapper copy and root-store clone helpers now allocate/register the
  destination wrapper only at the commit point after child-copy and payload
  clone work succeeds, so repeated partial-abort attempts no longer leave
  unreachable wrapper slots behind in the surviving target/root scope.
- Direct iterative `CONS` boundary copy and ESCAPE promotion now apply the
  same transactional rollback rule, so a copied/promoted car retain is
  unwound immediately if a later cdr copy/promotion step fails.
- Splice legality checks are reason-coded (`BoundaryScopeTransferReason`) and enforced through `boundary_check_scope_transfer(...)`.
- `ScopeRegion` escape splice uses O(1) tail-link concatenation (`escape_chunks_tail`, `escape_dtors_tail`) with consistency assertions.
- Boundary guard scripts exist and are wired:
  - `scripts/check_boundary_facade_usage.sh`
  - `scripts/check_boundary_change_policy.sh`
  - `scripts/run_boundary_hardening.sh`
- Focused runtime follow-up guard scripts also exist:
  - `scripts/check_scheduler_state_guards.sh`
  - `scripts/check_jit_env_scope_guards.sh`
- Verification status for the already-closed hardening profile remains current:
  - `c3c build` passed.
  - bounded `memory-lifetime-smoke` passed at `pass=102 fail=0`.
  - `rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1678/0`, `compiler: 85/0`).
  - `scripts/run_boundary_hardening.sh` passed end-to-end (Stage 0 through Stage 8, including Stage 4 ASAN with leak detection enabled).
- Latest boundary smoke regression evidence:
  - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` passed (`unified: 118/0`).
- Latest continuation-focused JIT evidence:
  - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=multi-interp-lifetime,continuation-teardown,shared-handle-state-teardown,cross-interp-continuation-guard,escaped-handle-continuation-guard,side-effect-escaped-handle-continuation-guard ./build/main --test-suite lisp'` passed (`6 passed, 0 failed`).
- Latest full JIT-policy evidence:
  - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp'` passed (`unified: 41/0`).
- Boundary return-path telemetry now reports zero copy fallback pressure in both profiles (`copy_fallback_total=0` in normal and ASAN boundary hardening runs).
- Env-copy iterator payloads now route closure thunks through the same safe
  undelimited global-env clone helper as plain closure bindings, so iterator
  wrappers no longer fail closed more aggressively than the underlying closure
  contract.
- Detached recursive closure publication now fails closed if typed
  method-signature cloning into the detached env scope fails; it no longer
  silently publishes a downgraded closure with `type_sig = null`.
- Session 44 docs closure artifact is published:
  - `docs/BOUNDARY_ARCHITECTURE_AUDIT_2026-03-10.md` defines boundary invariants contract and residual risk list.
- Memory/ownership test policy is split into explicit lanes:
  - `memory-lifetime` maps to `memory-lifetime-smoke`.
  - `memory-lifetime-smoke` owns boundary/scoping/coroutine ownership correctness and is the minimum container lane for boundary/lifetime changes.
  - `memory-lifetime-policy` owns boundary-policy parser/config contracts; it is not a generic syntax/compiler lane.
  - `memory-lifetime-bench` owns boundary allocation/perf coverage when the touched change is boundary/lifetime allocation-sensitive.
  - `memory-lifetime-soak` owns explicit heavy saturation/stress ownership probes.
  - `memory-stress` owns the global stress profile.
  - `allocator-validation` owns AST allocator correctness (non-benchmark) and is separate from boundary/lifetime ownership coverage.
  - `allocator-bench` owns AST throughput/benchmark-only coverage and is only implied by allocator benchmark-sensitive work.
- Contributor/container guidance is explicit by ownership family:
  - boundary/lifetime lanes (`memory-lifetime*`, `memory-stress`) require `scripts/run_validation_container.sh`.
  - allocator lanes (`allocator-validation`, `allocator-bench`) are separate from boundary/lifetime lanes and should be chosen only when allocator behavior or benchmarks changed.
  - syntax/compiler-only changes should stay on their own non-memory lanes and do not implicitly require memory-ownership coverage.
  - do not route syntax/compiler-only work through a memory lane by default; pick the lane that matches the touched semantics.
- Latest memory-boundary migration evidence:
  - the default bounded validation image includes Valgrind;
  - bounded container Valgrind `memory-lifetime-smoke` passed
    (`253 passed, 0 failed`);
  - stable indexed publication and region transplanting are constrained
    boundary fast paths, while destination materialization remains the default
    when lifetime proof is weaker than graph-copy proof.
- Parser/AST benchmark instrumentation is available under `OMNI_AST_ARENA_BENCH=1`, with dedicated parser and compiler smoke summaries (`ast_parser_smoke`, `ast_compiler_smoke`) for AST allocator shape validation.
- Boundary graph traversal/copy-fallback routing is no longer a production return-path mechanism for eval/JIT finalize flows:
  - boundary commit paths now return explicit hard outcomes for disallowed fallback classes,
  - root/persistent-store mutation paths are routed through explicit destination-aware helpers (not deep-copy fallback branches),
  - graph traversal remains a debug/audit surface only.
- Env-copy now keeps closure/iterator special-cases inside one shared promotion epoch:
  - direct bindings and iterator-reached payloads reuse the same copied closure
    / partial graphs when they originate from the same source wrapper,
  - so env-copy no longer drifts away from the repo-wide shared promotion
    context contract on those special-case paths.
- JIT TCO env-copy is now aligned with the generic copy fast-reuse gate for
  direct `CONS` scalar edges:
  - when a surviving target-chain `CONS` still points directly at a
    releasing-scope scalar child, the shared `copy_to_parent(...)` fast-reuse
    gate now rejects wrapper reuse before the JIT lane can accidentally keep
    the old wrapper by identity.
- Iterator alias safety now recurses into target-chain non-closure,
  non-partial payload graphs:
  - a surviving `ITERATOR` whose payload is a target-chain `ARRAY`, `HASHMAP`,
    `SET`, `CONS`, or similar graph now clones when that payload still reaches
    the releasing scope, instead of relying on a shallow payload pointer test.
- Shared-wrapper late-failure cleanup now also descends through copied nested
  `PARTIAL_PRIM` and `ITERATOR` payloads, so copied closure/env retains do not
  survive abort just because the already-materialized child sat behind a
  partial or iterator wrapper.
- Committed-root graph-reachability validation is now debug-only and threshold-gated:
  - controlled by `OMNI_BOUNDARY_GRAPH_AUDIT`,
  - sampled by `OMNI_BOUNDARY_GRAPH_AUDIT_RATE` (default 1),
  - bounded by `OMNI_BOUNDARY_GRAPH_AUDIT_MAX_ROOTS` (default unlimited),
  - and reported through boundary telemetry counters (`graph_audit_*`) when verbose diagnostics are enabled.
