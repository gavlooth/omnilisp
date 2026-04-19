# memory-runtime Part 02

Source: `docs/areas/memory-runtime.md`

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

1. Run `scripts/run_validation_status_summary.sh build/validation_status_summary.json` before treating `build/validation_status_summary.json` as current broad validation output; it includes host-local build/e2e/FTXUI smoke checks plus container-bound runtime slices, and should be used before drilling into narrower runtime guards.
2. Keep `memory/CHANGELOG.md`, `TODO.md`, and `memory/DESTINATION_ARENA_PLAN.md` closure wording synchronized per landing.
3. Keep `scripts/run_boundary_hardening.sh` and policy checks as required gate runs for boundary-sensitive changes.
4. Keep nested wrapper fail-closed coverage (`CONS` / `PARTIAL_PRIM` / `ITERATOR` with opaque primitive payloads) in the bounded smoke lane when touching boundary-copy or ESCAPE promotion code.
5. Use `scripts/check_scheduler_state_guards.sh` and `scripts/check_jit_env_scope_guards.sh` as the narrow release-signal reruns before escalating to broader runtime slices.
6. Treat any new bounded `advanced`, `basic`, or `memory-lifetime-smoke` regression as a fresh blocker instead of reopening stale historical notes here.
7. Keep runtime modularization queue updates in sync with `docs/plans/runtime-modularization-split-2026-03-11.md` and `memory/CHANGELOG.md` when deduce/runtime test splits land.
8. Keep contributor guidance aligned with lane ownership: boundary/lifetime lanes stay container-bound, allocator lanes stay separate, and syntax/compiler-only work should not inherit memory lanes by convenience.
9. Do not treat stale historical follow-up wording as live work unless a fresh failing signal or owner request promotes it into `TODO.md`; the current live queue is authoritative for active blockers.

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
- The broader constructor-callsite migration from unchecked `make_array(...)` /
  `make_hashmap(...)` users is now split and closed by callsite family rather
  than carried as one open residual.
- The data-format bridge slice of that migration is now closed:
  - JSON and TOML recursive decode paths now use checked `ARRAY` / `HASHMAP`
    constructors plus checked hashmap insertion,
  - CSV row/result construction now uses checked array constructors, and
  - nested conversion errors in JSON/TOML no longer get embedded into partial
    collections.
- The residual runtime constructor migration was split by real callsite family:
  - schema explain payload-map builders
  - remaining runtime/status payload builders
- The schema-explain family is now closed too:
  - explain entrypoint/result/candidate/source payload maps now use checked map
    construction and checked insertion through one explicit OOM contract.
- The runtime/status payload-builder family is now closed too:
  - async process/DNS/process-wait payload builders, fs-handle payload builders,
    and parsed HTTP response payloads now use checked `ARRAY` / `HASHMAP`
    construction plus checked insertion, and
  - `process-spawn` closes live process/fs handles if final success-payload map
    construction fails, so constructor OOM cannot strand open resources behind a
    half-built success result.

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

- OS-thread work admission failure now releases shared payloads before
  returning scheduler errors:
  - `scheduler_admit_os_thread_work(...)` releases `work.shared` for missing
    handle, full thread table, invalid generation lookup, and OS-thread start
    failure exits.
  - the bounded scheduler slice now pins the direct invalid-handle failure path
    and verifies the shared registry no longer resolves the payload after
    admission failure.
- Bounded validation after this slice:
  - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=scheduler ...`
    -> `pass=112 fail=0`

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
