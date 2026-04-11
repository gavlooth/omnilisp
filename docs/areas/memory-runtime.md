# Memory and Runtime

Status: `green` (boundary hardening, nested fail-closed wrapper promotion, bounded runtime/JIT gates, and release-signal cleanup are all currently validated)
As of: 2026-04-11

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
- Runtime intern and unhandled-effect payload guard cleanup is current as of
  2026-04-11:
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

## Known Drift

- `memory/DESTINATION_ARENA_PLAN.md` closure language must remain synchronized with changelog-backed validation.
- Historical ASAN repro artifacts are retained for forensics; current reruns are fully green for the active hardening profile.
- `scripts/check_jit_env_scope_guards.sh` is now the narrow JIT env/scope regression gate; it is green again after the iterator closure-boundary fix and the later continuation/effect replay repair.
- The bounded `basic` slice is green again after aligning explicit JIT parity checks with the top-level finalize/promote contract used by `run(...)`.
- The former `advanced` continuation/effect failure cluster is fully closed:
  - exact bounded repros are green,
  - the full bounded `advanced` slice is green again,
  - and the container stack-budget mismatch that used to make the runner look unstable is fixed in `scripts/container_exec.sh`.
- Telemetry visibility for boundary policy gates is now tracked in verbose dumps (`graph_audit_invoked`, `graph_audit_skipped_rate`, `graph_audit_skipped_max_roots`) so hot-path scan suppression is observable.

Repro artifacts:

- `build/asan_sequential_repro.log`
- `build/asan_fiber_temp_repro.log`
- `build/boundary_hardening_asan.log`
- `build/boundary_hardening_summary.json`

## Non-Negotiable Architecture Constraints

- RC scope/region ownership remains primary authority.
- No per-type RC lifetime model for language values.
- No root pinning as a general correctness workaround.
- Boundary paths remain source of truth for return/env/mutation/promotion semantics.

## Next Steps

1. Use `scripts/run_validation_status_summary.sh build/validation_status_summary.json` as the broad bounded-gate snapshot before drilling into narrower runtime guards.
2. Keep `memory/CHANGELOG.md`, `TODO.md`, and `memory/DESTINATION_ARENA_PLAN.md` closure wording synchronized per landing.
3. Keep `scripts/run_boundary_hardening.sh` and policy checks as required gate runs for boundary-sensitive changes.
4. Keep nested wrapper fail-closed coverage (`CONS` / `PARTIAL_PRIM` / `ITERATOR` with opaque primitive payloads) in the bounded smoke lane when touching boundary-copy or ESCAPE promotion code.
5. Use `scripts/check_scheduler_state_guards.sh` and `scripts/check_jit_env_scope_guards.sh` as the narrow release-signal reruns before escalating to broader runtime slices.
6. Treat any new bounded `advanced`, `basic`, or `memory-lifetime-smoke` regression as a fresh blocker instead of reopening stale historical notes here.
7. Keep runtime modularization queue updates in sync with `docs/plans/runtime-modularization-split-2026-03-11.md` and `memory/CHANGELOG.md` when deduce/runtime test splits land.
8. Keep contributor guidance aligned with lane ownership: boundary/lifetime lanes stay container-bound, allocator lanes stay separate, and syntax/compiler-only work should not inherit memory lanes by convenience.
9. Keep any future follow-up blocker explicit in `TODO.md`; the latest focused
   audit wave is currently back to zero live items.

## Concurrency Boundary Plan Alignment

`docs/plans/concurrency-hybrid-memory-checklist.md` is the governing checklist
for future concurrency ownership evolution.

- Worker/scheduler offload boundaries route byte payloads through
  `SharedHandle(kind=BLOB)` and no longer expose raw `SharedBlob*` at the
  production concurrency boundary.
- The explicit local/sendable/shared bridge API is introduced for offload byte
  payloads, and a scheduler-owned `SharedHandle` registry with generation
  validation is now present for persistent shared-object identity.
- Shared-handle projection into local `Value*` is now one-way and explicit at
  scheduler boundary completion consumption.
- Remaining concurrency ownership work is focused on later-phase
  shared-object/domain lifecycle consolidation.
- Offload path behavior changed in this migration; this section tracks phase
  sequencing while further shared-object consolidation proceeds.
## 2026-04-10 follow-up

- Runtime helper-owned list builders now fail closed on cons-constructor
  faults:
  - `src/lisp/prim_string_transform.c3`
    now rejects `string-split` internal result-list construction failure.
  - `src/lisp/prim_io_file.c3`
    now rejects `read-lines` internal result-list construction failure.
  - `src/lisp/prim_collection_hashmap_key_helpers.c3`
    now makes `keys` / `values` canonical list assembly fail closed in both
    sorted and fallback paths.
- Latest bounded evidence for that lane:
  - bounded `memory-lifetime-smoke`: `pass=162 fail=0`

- JIT helper arg construction now fails closed in the remaining helper-owned
  list-materialization paths:
  - `src/lisp/jit_jit_apply_helpers.c3` and
    `src/lisp/jit_jit_apply_runtime.c3`
    now reject variadic zero-fixed-arg rest-list construction failure before
    binding the rest parameter environment.
  - `src/lisp/jit_jit_dispatch_helpers.c3`
    now routes instance `ref` dispatch arg-list materialization through the
    shared checked helper instead of nested raw `make_cons(...)`.
- Latest bounded evidence for that lane:
  - focused `jit-policy` (`variadic-rest-list-alloc-failure`): `pass=1 fail=0`

- JIT quasiquote expansion now fails closed on pair-construction faults:
  - `src/lisp/jit_jit_quasiquote_macros.c3`
    routes internal quasiquote pair construction through one checked helper
    instead of letting `make_cons(...)` faults leak through `eval_ok(...)` as
    successful quasiquote values.
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
    pins both nested and list quasiquote allocation-failure paths in the
    bounded `jit-policy` slice.
- Latest bounded evidence for that lane:
  - focused `jit-policy` (`quasiquote-cons-alloc-failure`): `pass=1 fail=0`

- Iterator thunks no longer hide tail constructor errors behind normal-looking
  `(item . next)` pairs:
  - `primitives_iter_state.c3` now routes shared pair construction through one
    helper that propagates tail `ERROR` values directly,
  - and both source iterator thunks and coroutine/transform thunks now use it.
- Latest bounded evidence for that lane:
  - bounded `memory-lifetime-smoke`: `pass=128 fail=0`

- The shared `StringVal` builder contract is now fail-closed:
  - builder creation returns `null` on OOM,
  - growth returns failure instead of writing through null grow buffers,
  - and the append/padding helpers stop mutating builder state after a failed
    grow attempt.
- Runtime string helpers and parser string literal construction now share that
  checked builder path:
  - `prim_string_ops.c3`, `prim_string_format.c3`,
    and `prim_string_format_directives.c3` surface runtime OOM instead of
    continuing with invalid builders,
  - parser string literal builders now set parser errors instead of
    dereferencing failed builder allocation.
- Latest bounded evidence for this lane:
  - bounded `memory-lifetime-smoke`: `pass=127 fail=0`
  - bounded ASAN `memory-lifetime-smoke`: `pass=127 fail=0`
  - bounded `compiler`: `pass=191 fail=0`
- Residual adjacent runtime lanes are no longer in `StringVal` itself:
  - unchecked shared error/collection constructors (`make_error`,
    `make_array`, `hashmap_new` / `make_hashmap`)
  - iterator tail error propagation that can still degrade faults into
    truncation semantics

- Runtime helper allocation failures now fail closed in adjacent non-boundary
  helpers:
  - `apply_partial(...)` no longer dereferences a null wrapper allocation while
    chaining partials,
  - iterator thunk/wrapper construction now returns runtime OOM errors instead
    of writing through null `PARTIAL_PRIM` / `ITERATOR` wrappers,
  - string replace/repeat/format result materialization now disposes transient
    `StringVal` builders if the final `STRING` wrapper allocation fails, and
  - HTTP response parsing now fails closed if `status` / `headers` / `body`
    field-key allocation fails.
- Bounded memory-lifetime smoke and bounded ASAN smoke are green for this lane:
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
    -> `pass=126 fail=0`
  - `scripts/run_validation_container.sh ... c3c build --sanitize=address ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
    -> `pass=126 fail=0`
- Residual adjacent audit lane:
  - the shared `StringVal` builder contract itself is still fail-open in
    `strval_new(...)` / `strval_ensure(...)`; current hardening only closed
    final wrapper materialization at the current callsites.

- Env-copy and return-boundary closure wrapper allocation now fail closed:
  - closure wrapper cloning no longer dereferences a null `interp.alloc_value()`
    result in the shared closure-copy helper path,
  - env-copy iterator wrapper construction now rolls back inner payload
    materialization if the outer iterator wrapper allocation fails, and
  - env-copy `TIME_POINT` copy now returns failure instead of writing through a
    null wrapper on allocation failure.
- Memory-lifetime coverage now includes direct regressions for:
  - `copy_to_parent(...)` closure wrapper alloc failure teardown symmetry
  - env-copy closure wrapper alloc failure teardown symmetry
- Boundary leaf-wrapper copies now also fail closed for `INSTANCE`,
  `FFI_HANDLE`, and `TIME_POINT`:
  - failed wrapper allocation no longer dereferences null in those copy paths,
  - failed `INSTANCE` / `FFI_HANDLE` wrapper allocation no longer risks
    introducing a leaked retain before abort,
  - and `TIME_POINT` boundary copies now surface a typed error on allocation
    failure.

- The live runtime constructor substrate is now fail-closed where it matters for
  shared error/iterator and language-facing collection surfaces:
  - `make_error(...)` now returns a printable fallback `ERROR` when its message
    buffer allocation fails,
  - checked `ARRAY` / `HASHMAP` / `SET` constructor helpers now exist for
    runtime-facing callers that need an explicit OOM channel, and
  - raise payload construction, `Dictionary`, `Set`, and `to-array` now use
    that checked path instead of dereferencing failed constructor internals.
- The remaining open runtime memory item is now broader constructor-callsite
  migration from unchecked `make_array(...)` / `make_hashmap(...)` users rather
  than the shared constructor substrate itself.
- The data-format bridge slice of that migration is now closed:
  - JSON and TOML recursive decode paths now use checked `ARRAY` / `HASHMAP`
    constructors plus checked hashmap insertion,
  - CSV row/result construction now uses checked array constructors, and
  - nested conversion errors in JSON/TOML no longer get embedded into partial
    collections.
- The residual runtime constructor migration is now split by real callsite
  family:
  - schema explain payload-map builders
  - remaining runtime/status payload builders
- The schema-explain family is now closed too:
  - explain entrypoint/result/candidate/source payload maps now use checked map
    construction and checked insertion through one explicit OOM contract.
- The only remaining unchecked collection-constructor migration lane is now the
  runtime/status payload-builder family.

- Adjacent apply/promotion helper surfaces are now fail-closed too:
  - `apply_partial(...)` rejects malformed `PARTIAL_PRIM` state before
    function-pointer dispatch, including impossible `remaining` values and null
    function pointers.
  - checked hashmap insertion rejects promoted `ERROR` values from
    `boundary_promote_to_root(...)` instead of storing them as keys or values.
  - `fs_array_push(...)` and `csv_array_push(...)` apply the same contract, so
    helper-level array materializers no longer append promoted `ERROR` values
    as ordinary data.
- Bounded validation after this slice:
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
    -> `pass=152 fail=0`

- The malformed primitive-call dispatch lane is now fail-closed:
  - `apply_primitive(...)` rejects null/wrongly-tagged primitive wrappers and
    missing primitive function pointers before call-through.
  - `apply_partial(...)` also rejects missing `first_arg` so impossible
    partially-captured state does not flow into primitive dispatch.
  - `jit_apply_value_primitive(...)` now applies the same malformed primitive
    guard on the JIT helper path.
- Focused validation after this slice:
  - `OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=invalid-primitive-state-fails-closed`
    -> `pass=1 fail=0`

- Coroutine thunk publication is now fail-closed too:
  - `prim_coroutine_prepare_thunk(...)` and `prim_coroutine_create_ctx(...)`
    reject null promotion results, promoted `ERROR` values, and invalid
    post-promotion closure state before allocating any `StackCtx`.
  - the memory-lifetime runtime alloc lane now pins the forced closure-wrapper
    promotion seam and proves coroutine creation leaves `stack_ctx_pool`
    counters unchanged when thunk promotion fails.
- Bounded validation after this slice:
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
    -> `pass=153 fail=0`

- Data-format array assembly is now fail-closed on promoted boundary faults:
  - `json_val_to_omni(...)` and `toml_datum_to_omni(...)` now reject promoted
    `ERROR` values from `boundary_promote_to_root(...)` instead of storing
    them as array elements.
  - the runtime alloc lane now pins TOML array-element promotion failure under
    a non-root scope through the existing `TIME_POINT` copy-wrapper seam.
- Bounded validation after this slice:
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
    -> `pass=154 fail=0`

- Escape-lane cons publication is now fail-closed for string/error promotion:
  - `make_cons(...)` now stages escape promotion before pair allocation,
    rejects null / `ERROR` promotion results, and rejects the string/error
    fallback case where promotion returns the original non-escape field.
  - staged promoted fields are cleaned up if escape-pair allocation itself
    fails.
- Bounded validation after this slice:
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
    -> `pass=155 fail=0`

- Scheduler shared/offload projection now fails closed too:
  - `scheduler_project_shared_to_local_value(...)` now returns scheduler
    `ERROR`s for missing or unmaterializable shared handles instead of empty
    strings.
  - `scheduler_offload_read_file(...)` and
    `scheduler_offload_file_exists(...)` now return offload error completion
    state for missing/invalid projected path payloads instead of synthetic
    success values.
- Bounded validation after this slice:
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
    -> `pass=156 fail=0`

- Scheduler batch result-list publication is now fail-closed too:
  - `__raw-offload-batch` and `__raw-thread-spawn-batch` now route final list
    assembly through a checked scheduler-local prepend helper and surface
    typed out-of-memory errors instead of publishing partial success lists.
  - `__raw-task-spawn-batch` now drops already-spawned live thread-task
    entries if result-list publication fails after task creation.
  - the bounded scheduler slice now pins forced result-list cons allocation
    failure for all three batch primitives and proves active thread-task count
    stays unchanged across the failure path.
- Bounded validation after this slice:
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=scheduler ...`
    -> `pass=109 fail=0`

- Shared two-arg list materialization is now fail-closed too:
  - `make_list2_or_error(...)` now provides one checked two-value list helper
    for runtime/JIT helper paths that previously nested raw `make_cons(...)`
    calls.
  - `(shell cmd true)` now fails closed if final result-list construction
    fails.
  - pending-raise and effect-handler arg-pair construction now fail closed
    before handler call-through if the `(k arg)` pair cannot be built.
- Bounded validation after this slice:
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=handler-arg-list-alloc-failure ...`
    -> `pass=1 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
    -> `pass=160 fail=0`

- JIT multi-arg call construction now fails closed too:
  - continuation-safe multi-arg call-list assembly now rejects `make_cons(...)`
    failure while building the arg list.
  - `jit_apply_multi_args_iterative(...)` now rejects malformed/truncated arg
    lists instead of returning a partial multi-arg apply result as success.
- Bounded validation after this slice:
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=multi-arg-malformed-list-fails-closed ...`
    -> `pass=1 fail=0`
## 2026-04-10

- Scheduler result publication is now fail-closed for root-promotion faults:
  legitimate user `ERROR` completions still round-trip, but non-error fiber
  results that fail root publication are rewritten to scheduler-owned root
  errors.
- Scheduler offload completion decoding now rejects invalid completion kinds
  explicitly instead of falling through with undefined pointer state.
- Iterator source/coroutine state normalization is now fail-closed too:
  malformed internal thunk state for `__iterator-range-from`,
  `__iterator-repeat`, `__iterator-cycle`, and `__iterator-from-list` now
  returns typed iterator constructor/state errors instead of silently
  terminating the stream.
- `__iterator-cycle` now validates both active and reset iterator tails before
  re-publication, and `__iterator-foldl` now rejects `null` next-results as
  malformed iterator pairs instead of treating them as normal completion.
- Boundary wrapper allocation is now fail-closed too:
  boundary alloc helpers no longer register destructors on null wrappers, and
  array/dictionary/module/coroutine/primitive/FFI wrapper constructors now
  reject wrapper-allocation failure explicitly instead of dereferencing the
  missing wrapper.
- Coroutine publication now cleans up the newly created `StackCtx` when
  wrapper allocation fails after context creation, so the failure returns as a
  runtime error instead of stranding pool state.
- Coroutine boundary copy/escape lanes now apply the same rule:
  coroutine copy-to-parent destroys cloned context state when destination
  wrapper allocation fails, and coroutine escape promotion now rejects
  escape-wrapper allocation failure explicitly before mutating source
  ownership.
- Deduce row materialization now applies the same wrapper-allocation rule:
  cached relation column-key symbol materialization rejects root wrapper
  allocation failure explicitly instead of dereferencing a null root wrapper
  during lazy key-cache creation.
- Destination ESCAPE builders now apply the same wrapper-allocation rule:
  `boundary_build_destination_error_escape(...)`,
  `boundary_build_destination_cons_escape(...)`,
  `boundary_build_destination_partial_escape(...)`, and
  `boundary_build_destination_iterator_escape(...)`
  now fail closed when ESCAPE wrapper publication in the temporary build scope
  cannot allocate the destination wrapper.
- Generic ESCAPE promotion now rejects or fails closed on wrapper allocation
  for string/error publication, scalar wrappers, instance / FFI handle /
  time-point wrappers, and closure publication instead of dereferencing a null
  ESCAPE wrapper.
- Root-store clone publication now applies the same rule:
  primitive/array/hashmap/set/method-table root-store clone helpers now route
  final ESCAPE wrapper publication through the shared checked wrapper allocator
  instead of raw `alloc_value_escape()` publication.
- Structured ESCAPE publication now applies the same rule too:
  string/error ESCAPE promotion no longer fails open by returning the original
  TEMP-lane value when chars or wrapper publication cannot allocate, and
  shared ARRAY / HASHMAP / SET / METHOD_TABLE ESCAPE promotion now delays
  final wrapper publication until payload promotion succeeds.
- Repeated shared ARRAY child-promotion faults now show only stable
  PromotionContext memo overhead across attempts, with no accumulating ESCAPE
  wrapper-slot growth.
- Core constructor publication now applies the same rule too:
  `make_cons(...)` ESCAPE publication uses the checked boundary allocator, and
  `make_closure(...)` / `make_closure_no_param(...)` allocate closure payload
  storage before publishing/registering the wrapper.
- Partial primitive and opaque-payload constructors now follow the same
  publication ordering: `PARTIAL_PRIM` ESCAPE promotion uses the checked
  boundary allocator, and primitive / FFI handle constructors allocate
  subordinate payloads before publishing wrapper slots.
- Runtime helper allocation diagnostics now keep temporary-interpreter values
  alive until after failure reporting, and coroutine stack-pool cleanup tests
  distinguish normal reuse from ASAN's direct-free pool mode.
- Iterator/coroutine constructor guards now reject malformed wrappers before
  publication/terminal use: iterator terminal paths validate callable thunks,
  `coroutine?` handles empty argument lists, and `make_coroutine(...)` rejects
  null stack contexts.
- Generic list materializers touched in the runtime audit now use checked cons
  construction for `List(String)` and `list`, and JSON pointer lookup now
  propagates key string materialization failure instead of falling through to a
  different lookup mode.
- `StringVal` capacity growth now computes the target size through checked
  overflow addition instead of a `usz.max` comparison, so `%s` display
  formatting for ordinary values such as `nil` and `Void` reaches the normal
  append/growth path.
- Runtime allocation-size hardening continued on 2026-04-11:
  - AOT bridge symbol interning and environment hash/define/set paths now reject
    `INVALID_SYMBOL_ID` before lookup or mutation, so intern allocation failure
    cannot alias env empty-slot sentinels.
  - module load publication now rolls back failed module body/path/top-level
    loads and rebuilds module hash state, preserving retry semantics after
    failed imports.
  - deduce persisted rule-catalog/signature restore now distinguishes missing
    DBIs from other LMDB open errors, rejects invalid restored symbols, and
    rolls back partially restored rule signatures/schemas.
  - raise payload construction, method-table root cloning, and collection
    primitives now fail closed on invalid symbol IDs, method-table allocation
    overflow, or nullable array/dict/set backing storage.
  - environment hash-table sizing, closure wrapper parameter copying, and async
    process argv/env staging now reject overflowing allocation arithmetic before
    allocating or publishing staged state.
  - JIT effect handle/signal paths now guard clause and argument buffer sizes.
  - deduce aggregate, relation materialization, rule IR, and goal-directed
    read-tracking staging now guard derived buffer sizes and capacity
    arithmetic before writes.
  - AOT/JIT method-signature staging now guards parameter/constraint buffers and
    publishes counts only after staged storage succeeds.
  - deftype registration now rolls back a just-added type if constructor/global
    binding/type-value publication fails after registry insertion.
  - type-symbol and dispatched-primitive bootstrap now clean up empty heap
    method-table payloads on root-wrapper/global binding publication failure.
  - symbol/type registry insertion now rejects exhausted ID spaces before
    narrowing counts, and failed just-added type rollback rebuilds the type hash
    table so open-address probe chains remain sound.
  - interpreter bootstrap symbols now fail fast on intern failure instead of
    publishing invalid sentinel IDs into runtime state.
  - Dictionary constructor staging now checks capacity arithmetic and allocates
    hashmap payload storage before root-wrapper publication.
  - unicode case mapping, read-file, console emit, and TLS offload cleanup now
    reject or fail closed on API-width and initialization-state boundaries.
  - macro/parser symbol paths now reject intern failure instead of propagating
    invalid sentinel IDs, and macro expansion block/call AST builders now check
    array-size multiplication and arena allocation before writing.
  - numeric/string/data-format guard paths now reject signed overflow,
    overflowing format widths/precisions, improper string/data-format list
    tails, and failed RNG reads instead of truncating or using invalid state.
  - async TCP read option parsing now rejects non-positive max byte counts, and
    resumed-before-completion async cleanup paths close pending DNS/connect/
    accept state before returning the invariant error.
- Bounded validation after this slice:
  - host `c3c build --warn-deprecation=no`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=compiler ...`
    -> `pass=191 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=deduce ...`
    -> `pass=330 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
    -> `pass=189 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=async ...`
    -> `pass=61 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=scheduler ...`
    -> `pass=111 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=advanced ...`
    -> `pass=1183 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=compiler ...`
    -> `pass=191 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=deduce ...`
    -> `pass=330 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=string-type ...`
    -> `pass=40 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
    -> `pass=189 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=advanced ...`
    with `OMNI_ADVANCED_GROUP_FILTER=advanced-unicode-iterator`
    -> `pass=130 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=arithmetic-comparison ...`
    -> `pass=45 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=string-type ...`
    -> `pass=40 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=data-format ...`
    -> `pass=62 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=advanced ...`
    with `OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-string-predicate-format`
    -> `pass=59 fail=0`
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=advanced ...`
    with `OMNI_ADVANCED_GROUP_FILTER=advanced-macro-hygiene`
    -> `pass=82 fail=0`
  - AOT generated type/type-spec and runtime helper paths now reject invalid
    symbol IDs before publishing into type metadata, method signatures, match
    constructor lookup, dictionary-key lookup, and effect explain payloads.
  - compiled list helpers now reject negative indexes before unsigned
    conversion.
  - direct and buffered printing now handles nullable dictionary/set backing
    storage, and buffered printing rejects null/zero-capacity buffers before
    writing.
  - constructor constraint diagnostics now use guarded type-registry lookups,
    and instance type inference rejects invalid instance type IDs.
  - deduce tuple storage now persists full-width 32-bit symbol IDs and rejects
    invalid/out-of-range decoded symbol IDs.
  - deduce metadata delete, DBI name/path allocation, relation schema
    publication, and rule signature install paths now fail closed on DBI-open
    errors, arithmetic overflow, and post-publication failures.
  - validation:
    - host `c3c build --warn-deprecation=no`
    - bounded `compiler`: `pass=191 fail=0`
    - bounded `deduce`: `pass=330 fail=0`
    - bounded `advanced`: `pass=1183 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`
  - parser/compiler and runtime boundary hardening now rejects invalid symbol
    IDs before AST/runtime publication, checks synthetic AST wrapper
    allocations before rewriting effect bodies, rejects macro splice improper
    tails, guards boundary `len + 1` string/error copies, restores interpreter
    state on JIT continuation yield failure, stages pending-raise state clear
    after fallible setup, and rejects null non-empty runtime handle arrays.
  - async/TLS hardening now closes pending offloads on TLS yield errors,
    validates TCP/UDP ports and signal numbers before `int` narrowing, and
    reports file-read close failure.
  - validation:
    - host `c3c build --warn-deprecation=no`
    - bounded `compiler`: `pass=191 fail=0`
    - bounded `async`: `pass=61 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`
    - bounded `advanced` macro hygiene group: `pass=82 fail=0`
  - schema/deduce payload publication now rejects failed symbol interning and
    propagates concrete payload assembly errors instead of publishing invalid
    symbols or returning partial/null payloads.
  - checked array construction now stages payload allocation before root-wrapper
    publication, and closure escape promotion now releases retained/detached
    environment scopes if wrapper allocation fails.
  - external boundary width guards now cover `exit`, `TimePoint`, Unicode
    codepoint predicates, `fs-open`, `fs-stat`, `tcp-listen`, JSON pointer
    symbol fallback, process-handle lookup, and zlib expansion sizes.
  - validation:
    - host `c3c build --warn-deprecation=no`
    - bounded normal+ASAN `data-format`: `pass=64 fail=0`
    - bounded normal+ASAN `unicode`: `pass=27 fail=0`
    - bounded normal+ASAN `compression`: `pass=27 fail=0`
    - bounded normal+ASAN `async`: `pass=65 fail=0`
    - bounded normal+ASAN `compiler`: `pass=194 fail=0`
    - bounded normal+ASAN `memory-lifetime-smoke`: `pass=190 fail=0`
    - bounded normal `advanced`: `pass=1185 fail=0`
    - bounded ASAN `advanced`: `pass=1172 fail=0`
    - bounded normal+ASAN `deduce`: `pass=330 fail=0`
  - FTXUI `smoke.omni` no longer crashes on nested `ui.graph` effect payloads:
    boundary provenance now uses a bounded iterative alias-safety worklist
    instead of recursive graph walking for nested arrays, dicts, sets, method
    tables, partials, iterators, and cons payloads; scalar leaves are skipped
    before consuming worklist/visited capacity so wide scalar-only payloads do
    not fail closed as graph overflows.
  - The same slice also hardened FTXUI helper allocations and teardown:
    child pointer-array sizing, helper-array growth, and graph-series
    allocation arithmetic are checked, oversized menu selected-index counts are
    rejected before `int` narrowing, shim child/table selector inputs are
    guarded before narrowing, shim `keep_alive` data now outlives component
    teardown, and quit-key wrappers retain shared ownership of the screen loop
    object instead of capturing a raw screen handle. Status-returning FTXUI C
    ABI entrypoints now use fail-closed exception guards around backend work,
    and deferred graph/render/event/quit callbacks catch callback exceptions
    locally.
  - validation:
    - host `c3c build --warn-deprecation=no`
    - host `scripts/run_ftxui_smoke.sh`
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=192 fail=0`
  - Follow-up regression evidence on 2026-04-11:
    - the smoke lane now also covers a shared composite cycle payload
      regression, validating repeated composite alias traversal and cycle
      identity tracking.
    - boundary alias visited tracking now fronts the authoritative linear
      `seen` list with a small bounded `ushort` index-table accelerator for
      repeated composite alias checks.
    - the accelerator saturates into the existing linear scan to preserve the
      no-false-negative contract and fail-closed graph caps.
    - scalar/non-graph roots now return before entering the large traversal
      frame, and the large-array walker sits behind a small stack-headroom
      wrapper that fails closed to copy-required if the current stack context is
      too shallow.
    - larger local pointer/index-table attempts regressed FTXUI smoke with a
      `smoke.omni` boundary resolve stack overflow and were not kept; the
      landed table stays deliberately small to fit the effect/FTXUI stack
      budget.
    - host `c3c build --warn-deprecation=no`: green
    - host `scripts/run_ftxui_smoke.sh`: green
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=193 fail=0`
  - Destination build-scope commit follow-up:
    - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` no longer
      calls `main::scope_splice_escapes(...)` directly.
    - `boundary_destination_build_scope_splice(...)` in the allowlisted
      boundary builder implementation file owns the single low-level splice
      seam for destination `cons`, `partial`, `iterator`, and `error` wrapper
      commits.
    - validation: boundary facade usage, boundary change policy, status
      consistency, `git diff --check`, and bounded normal+ASAN
      `memory-lifetime-smoke` with FTXUI smoke enabled are green
      (`pass=193 fail=0`).
  - Boundary alias graph coverage follow-up:
    - `INSTANCE` and `MODULE` are now treated as graph-bearing alias payloads,
      using a heap-backed rare-path value/env reachability scan so the hot
      alias walker stack frame does not grow.
    - instance field and module/env graphs that still reach the releasing
      scope now force copy-required classification instead of reuse.
    - root-persistent env boxes are still traversed for parent/binding edges;
      they are only excluded from direct temp-frame ownership checks.
    - validation: host build, boundary facade usage, status consistency,
      `git diff --check`, and bounded normal+ASAN `memory-lifetime-smoke` with
      FTXUI smoke enabled are green (`pass=194 fail=0`).
  - JIT/module value growth and promotion fail-closed follow-up:
    - first-class `MODULE` values now hold root-scope descriptor snapshots
      instead of raw pointers into the reallocating interpreter module table.
    - module path access now fails closed when a module descriptor has invalid
      export/env storage.
    - JIT cons/instance field mutation and JIT/AOT method publication now
      reject null promotion/copy results before mutating storage.
    - validation: host build, boundary facade usage, boundary change policy,
      status consistency, `git diff --check`, and bounded normal+ASAN
      `memory-lifetime-smoke` with FTXUI smoke enabled are green
      (`pass=196 fail=0`).
  - TCO env-copy and recycle-hook fail-closed follow-up:
    - TCO env-frame binding copy now uses checked boundary-copy results and
      aborts the copied frame on copy faults, null results, or copied `ERROR`
      values before binding poisoned state.
    - TCO recycle preparation now preserves the previous `*env_io`, restores
      the old call scope, releases the fresh scope, and returns an explicit
      error when env-copy fails.
    - the direct env-copy failure and recycle-hook state-restore regressions
      are wired into the bounded smoke lane.
    - validation: host build, boundary facade usage, boundary change policy,
      status consistency, `git diff --check`, and bounded normal+ASAN
      `memory-lifetime-smoke` with FTXUI smoke enabled are green
      (`pass=198 fail=0`).
  - JIT mutable-local, raise-payload, dispatch-payload, constructor-payload,
    deduce-integrity payload, and goal-directed deduce diagnostic payload
    fail-closed follow-up:
    - mutable-local helper lookup now rejects null env boxes and missing
      bindings with an explicit error instead of returning null into compiled
      code paths.
    - mutable-local env reparenting now returns the effective env, allowing
      capture setup to preserve the checked helper result.
    - handled raise payload construction now uses the non-raising checked
      hashmap allocation path, so payload allocation failure cannot publish a
      stale nested pending raise before the outer raise path reports the
      intended construction error.
    - optional dispatch diagnostic payload construction now uses the same
      non-raising hashmap helper, preventing failed ancillary payload
      allocation from pre-seeding `raise_pending` under an active handler.
    - optional constructor mismatch diagnostic payload construction also uses
      the non-raising hashmap helper and validates payload key interning before
      constructing key symbols.
    - optional deduce integrity/check-context diagnostic payload construction
      now uses the non-raising hashmap helper and no-raise local setters, so
      failed ancillary integrity payload allocation cannot pre-seed
      `raise_pending` before the intended integrity violation raise.
    - optional iteration-limit diagnostic payload construction also uses the
      non-raising hashmap helper before the later iteration-limit raise.
    - goal-directed selector analysis and selector/relation surface diagnostic
      payload construction also uses the non-raising hashmap helper and local
      checked insertion before publishing the existing deduce OOM fallback.
    - goal-directed explain snapshot/component payloads now reject failed
      payload-symbol interning before constructing payload `SYMBOL` values,
      and why-result path dictionary lookup rejects invalid temporary key
      symbols before probing.
    - dispatch diagnostic payload insertion, process-spawn result maps, HTTP
      response maps, and FTXUI dictionary lookup now also guard failed symbol
      interning before constructing payload or lookup key symbols.
    - validation: host build, boundary facade usage, boundary change policy,
      status consistency, `git diff --check`, bounded normal `jit-policy` with
      FTXUI smoke enabled (`pass=51 fail=0`), and bounded ASAN `jit-policy`
      (`pass=50 fail=0`), plus bounded normal+ASAN `memory-lifetime-smoke`
      with FTXUI smoke enabled (`pass=200 fail=0`) and bounded normal+ASAN
      `deduce` slice (`pass=330 fail=0`), plus bounded normal+ASAN `async`
      with FTXUI smoke enabled (`pass=65 fail=0`) are green.
