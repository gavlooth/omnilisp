# Memory and Runtime

Status: `green` (boundary hardening, nested fail-closed wrapper promotion, bounded runtime/JIT gates, and release-signal cleanup are all currently validated)
As of: 2026-04-10

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
- `make_array(...)` is now just the checked array constructor contract, so raw
  array creation no longer bypasses allocator failure handling and return a
  partially initialized wrapper.
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
  - `string->list`, `string-split`, `string-graphemes`, and `read-lines`
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
9. Open follow-up blockers after the latest focused audit are explicit again in
   `TODO.md`: direct destination-promotion context drift and partial-abort
   wrapper-slot leaks are not closed by the current tree.

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
