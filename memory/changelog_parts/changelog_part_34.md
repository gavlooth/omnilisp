# Memory Changelog Index Part 34

Source: `memory/CHANGELOG.md`

    to synthetic `nil` / `0` business results for `read-file` and
    `file-exists`.
  - shipped consequence:
    - scheduler shared/offload projection faults now stay visible as typed
      scheduler/offload errors instead of being normalized into valid data.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=156 fail=0`
- 2026-04-10 (pending raise payload materialization fail-closed follow-up):
  - pending raise publication now rejects `boundary_promote_to_root(...)`
    null/error results before storing payload state in the interpreter.
  - JIT handle raise dispatch now rejects fallback message-string and
    arg-list constructor failure before calling or binding the raise handler.
  - shipped consequence:
    - pending raise message/payload materialization failure now remains a
      top-level eval/runtime error instead of running the raise clause with a
      constructor-failed value as ordinary data.
  - validation:
    - `c3c build`
    - targeted `jit-policy`: `pass=1 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=157 fail=0`
- 2026-04-10 (iterator coroutine cons-construction fail-closed follow-up):
  - iterator coroutine helpers now route internal pair/list construction
    through a checked iterator-local cons helper.
  - `zip` now rejects item-pair construction failure directly instead of
    publishing the constructor-failed value as an iterator item.
  - `foldl` now rejects arg-list construction failure directly instead of
    remapping the same fault into a later malformed-args apply error.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=159 fail=0`
- 2026-04-10 (iterator source malformed-state fail-closed follow-up):
  - iterator source thunks no longer normalize malformed internal state into
    successful exhaustion.
  - `__iterator-range-from`, `__iterator-repeat`, `__iterator-cycle`, and
    `__iterator-from-list` now surface typed constructor/state errors when
    thunk state is missing, null, or not the expected iterator/list payload.
  - `__iterator-cycle` now validates iterator tails through
    `iterator_tail_or_error(...)` for both the active and reset iterator
    branches.
  - `__iterator-foldl` now rejects `null` next-results as malformed iterator
    pairs instead of treating them as clean completion.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=169 fail=0`
- 2026-04-10 (boundary wrapper allocation fail-closed follow-up):
  - boundary value allocation now fails without registering a destructor on a
    null wrapper target.
  - root/scope wrapper constructors now reject wrapper-allocation failure
    explicitly for arrays, dictionaries, modules, coroutines, primitives, and
    FFI handle wrappers instead of dereferencing the missing wrapper.
  - coroutine publication now cleans up the freshly created `StackCtx` if
    wrapper allocation fails after context creation.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=171 fail=0`
- 2026-04-10 (coroutine boundary wrapper allocation fail-closed follow-up):
  - coroutine copy-to-parent now destroys the cloned `StackCtx` and returns a
    boundary error if the destination wrapper cannot be allocated.
  - coroutine escape promotion now rejects escape-wrapper allocation failure
    explicitly instead of dereferencing a null escape wrapper.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=173 fail=0`
- 2026-04-10 (deduce row key wrapper allocation fail-closed follow-up):
  - deduce relation row materialization now rejects root wrapper allocation
    failure while building cached column-key symbols instead of dereferencing a
    null root wrapper.
  - validation:
    - `c3c build`
    - bounded `deduce`: `pass=329 fail=0`
- 2026-04-10 (destination and escape wrapper allocation fail-closed follow-up):
  - destination ESCAPE builders for `ERROR`, `CONS`, `PARTIAL_PRIM`, and
    `ITERATOR` now fail closed when ESCAPE wrapper publication cannot allocate
    the destination wrapper in the temporary build scope.
  - generic ESCAPE promotion now rejects or fails closed on wrapper allocation
    failure for strings/errors, shared-wrapper leaf copies, instances, FFI
    handles, scalars, time points, and closures instead of dereferencing a
    null ESCAPE wrapper.
  - closure ESCAPE promotion now rejects closure-wrapper and closure-payload
    allocation failure before publishing a partially built closure.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=176 fail=0`
- 2026-04-10 (root-store clone wrapper allocation fail-closed follow-up):
  - root-store clone helpers for primitives, arrays, hashmaps/sets, and method
    tables now use the shared checked ESCAPE wrapper allocator instead of raw
    `alloc_value_escape()` publication.
  - failed root-store ARRAY wrapper publication now proves copied nested
    closure retains are unwound immediately instead of being pinned into root
    lifetime until owner release.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=177 fail=0`
- 2026-04-10 (escape structured publication fail-closed follow-up):
  - string/error ESCAPE promotion now returns a boundary error when chars or
    wrapper publication cannot allocate, instead of failing open by reusing the
    original TEMP-lane value.
  - shared ARRAY / HASHMAP / SET / METHOD_TABLE ESCAPE promotion now delays
    final wrapper publication until payload promotion succeeds, and wrapper
    allocation failure cleans up the materialized payload before returning a
    boundary error.
  - repeated shared ARRAY child-promotion failures now prove stable
    PromotionContext memo overhead without accumulating extra ESCAPE wrapper
    slots across attempts.
  - validation:
    - bounded `memory-lifetime-smoke`: `pass=179 fail=0`
- 2026-04-10 (core constructor publication fail-closed follow-up):
  - `make_cons(...)` ESCAPE publication now routes wrapper allocation through
    the checked boundary allocator instead of the assert-only
    `alloc_value_escape()` path.
  - `make_closure(...)` and `make_closure_no_param(...)` now allocate closure
    payload storage before publishing/registering the wrapper, so closure
    payload OOM returns `closure: out of memory` instead of publishing a
    half-built closure or dereferencing null payload storage.
  - validation:
    - bounded `memory-lifetime-smoke`: `pass=182 fail=0`
- 2026-04-10 (partial primitive and opaque payload publication follow-up):
  - `PARTIAL_PRIM` ESCAPE promotion now uses the checked boundary allocator for
    final wrapper publication, so wrapper allocation failure returns a typed
    boundary error instead of relying on assert-only allocation.
  - primitive and FFI-handle constructors now allocate subordinate heap payloads
    before publishing/registering root/current-scope wrappers, so repeated
    payload allocation failures do not consume wrapper slots.
  - coroutine wrapper allocation-failure diagnostics now read result state before
    releasing the temporary interpreter, and the stack-pool cleanup expectation
    accounts for ASAN's no-reuse pool mode.
  - validation:
    - bounded `memory-lifetime-smoke`: `pass=185 fail=0`
    - bounded ASAN `memory-lifetime-smoke`: `pass=185 fail=0`
- 2026-04-10 (iterator/coroutine and list/json fail-closed follow-up):
  - `Iterator` constructor and terminal gates now validate that existing
    iterator wrappers contain a callable thunk, instead of accepting malformed
    `ITERATOR` wrappers by tag alone.
  - `coroutine?` now handles zero-argument calls without an out-of-bounds read,
    and `make_coroutine(...)` rejects null stack contexts before publishing a
    malformed wrapper.
  - `string->list` and `list` now route internal result-list construction
    through the checked cons helper, so forced cons allocation failure returns a
    runtime error instead of embedding constructor failures as ordinary list
    data.
  - JSON pointer string-key lookup now propagates key materialization failure
    instead of falling through to symbol lookup, and JSON emit/list conversion
    now rejects improper list tails instead of silently truncating them.
  - validation:
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`
    - bounded ASAN `memory-lifetime-smoke`: `pass=189 fail=0`
    - bounded `json`: `pass=39 fail=0`
    - direct eval regressions for `(list->string (cons "a" 2))`,
      `(json-emit (cons 1 2))`, and
      `(json-emit 1 (cons (list 'pretty true) 2))`
  - residual:
    - `AUDIT-FORMAT-DISPLAY-TEMP-BUILDER-FAILCLOSED-037` tracks the unrelated
      `%s` display formatter failure surfaced by the advanced unicode iterator
      validation group.
- 2026-04-10 (`%s` display formatter builder-growth guard):
  - `StringVal` target-capacity calculation now uses checked overflow addition
    instead of comparing ordinary small appends against `usz.max`, avoiding the
    signed-lowered `usz.max` comparison that made `(format "%s" nil)` and
    `(format "%s" (Void))` fail with `"format: failed to grow temporary builder"`.
  - the advanced unicode/type constructor surface now pins `(format "%s" nil)`
    beside the existing `(format "%s" (Void))` regression.
  - validation:
    - `c3c build`
    - bounded `advanced` slice with
      `OMNI_ADVANCED_GROUP_FILTER=advanced-unicode-iterator`: `pass=129 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`
    - bounded ASAN `memory-lifetime-smoke`: `pass=189 fail=0`
    - direct JSON REPL probes for `(format "%s" nil)`, `(format "%s" (Void))`,
      and a long `%s` string that requires real builder growth
  - backlog:
    - closed `AUDIT-FORMAT-DISPLAY-TEMP-BUILDER-FAILCLOSED-037`; actionable
      backlog returns to `0`.
## 2026-04-10

- Scheduler completion publication now distinguishes legitimate user `ERROR`
  results from root-promotion failures. `scheduler_mark_done(...)` and
  `scheduler_complete_fiber(...)` normalize non-error values whose
  `boundary_promote_to_root(...)` result is `null` or `ERROR` into explicit
  scheduler root errors instead of publishing internal promotion faults as if
  they were the fiber's real result.
- Offload completion materialization now fails closed on invalid
  `OffloadResultKind` tags instead of returning an uninitialized pointer.

## 2026-04-11

- Environment/process-spawn allocation sizing now rejects overflow before
  staging:
  - environment hash-table rebuild/capacity arithmetic now guards `binding_count`
    and load-factor multiplication before deriving allocation sizes.
  - closure boundary wrapper parameter copying now rejects overflowing
    `SymbolId` buffer sizes.
  - async process spawn staging now checks argv/env pointer-table count and byte
    sizes before allocating C pointer arrays.
- JIT effect and deduce allocation sizing now fail closed on oversized internal
  buffers:
  - JIT handle/effect-signal paths now guard effect clause and signal argument
    array counts before allocation.
  - deduce aggregate group state, encoded tuple staging, rule IR term/atom
    staging, relation column-key materialization, and goal-directed read
    tracking buffers now guard derived allocation arithmetic before writes.
- AOT/JIT type-dispatch signature staging now rejects overflow and late
  publication drift:
  - AOT/JIT method signatures guard parameter and constraint allocation sizes
    and delay count publication until staged storage succeeds.
  - deftype registration now checks derived type-info allocation sizes and rolls
    back a just-added type if constructor/global binding/type-value publication
    fails after registry insertion.
  - type and dispatched primitive bootstrap now clean up empty heap method-table
    payloads when root-wrapper/global binding publication fails.
- validation:
  - host `c3c build --warn-deprecation=no`
  - bounded compiler slice: `pass=191 fail=0`
  - bounded deduce slice: `pass=330 fail=0`
  - bounded memory-lifetime-smoke slice: `pass=189 fail=0`
  - bounded async slice: `pass=61 fail=0`
- Registry, bootstrap, unicode, collection, I/O, and TLS guard hardening:
  - symbol/type registry insertion now rejects exhausted ID spaces before
    narrowing counts to `SymbolId` / `TypeId`; symbol probing skips stale
    out-of-range indices, and failed just-added type rollback rebuilds the type
    hash table to preserve open-address probe chains.
  - interpreter core/type/misc symbol bootstrap now uses checked intern helpers
    and fails fast if a required bootstrap symbol cannot be interned.
  - unicode case mapping now rejects inputs too large for `utf8proc`'s `long`
    length API before narrowing `src.len`.
  - TLS offload shutdown now closes `br_sslio_context` only after successful
    `br_sslio_init(...)`.
  - Dictionary construction now computes initial capacity with checked
    arithmetic and allocates hashmap payload storage before publishing the root
    wrapper.
  - read-file now rejects file sizes that cannot fit in `usz`, and console
    emit now turns render/write failures into typed I/O errors.
  - validation:
    - host `c3c build --warn-deprecation=no`
    - bounded compiler slice: `pass=191 fail=0`
    - bounded memory-lifetime-smoke slice: `pass=189 fail=0`
    - bounded async slice: `pass=61 fail=0`
    - bounded scheduler slice: `pass=111 fail=0`
    - bounded advanced unicode iterator slice: `pass=129 fail=0`
- Numeric, macro/parser, data-format, and async guard hardening:
  - RNG primitives now loop until the full random buffer is read and report a
    runtime error on `getrandom` failure instead of consuming uninitialized stack
    bytes.
  - integer arithmetic now rejects overflow in `+`, `-`, `*`, `long.min / -1`,
    `long.min % -1`, `abs(long.min)`, `gcd(long.min, ...)`, and
    `lcm(long.min, ...)`.
  - format parsing now rejects width/precision values that overflow `int`, and
    `%b` rejects `long.min` instead of negating it.
  - `list->string` now preserves full multibyte string elements, and
    `string-join`, CSV option parsing, and TOML option parsing reject improper
    tails instead of silently truncating.
  - `string->symbol`, macro gensym expansion, parser symbol intern, placeholder
    gensym intern, and macro AST block/call allocation now fail closed on
    sentinel/allocation failure.
  - TCP read option parsing rejects non-positive `max-bytes`, resumed-before-
    completion async branches close their pending DNS/connect/accept state, and
    writable wakeup coalesces are tracked separately from readable coalesces.
  - validation:
    - host `c3c build --warn-deprecation=no`
    - bounded arithmetic-comparison slice: `pass=45 fail=0`
    - bounded string-type slice: `pass=40 fail=0`
    - bounded data-format slice: `pass=62 fail=0`
    - bounded async slice: `pass=61 fail=0`
    - bounded scheduler slice: `pass=111 fail=0`
    - bounded advanced numeric string/predicate/format group: `pass=59 fail=0`
    - bounded advanced unicode iterator group: `pass=130 fail=0`
    - bounded advanced macro hygiene group: `pass=82 fail=0`
- AOT, value printing, and deduce persistence soundness hardening:
  - AOT type/type-spec and generated runtime helper paths now reject invalid
    symbol interning before constructing type metadata, method signatures,
    match constructor lookups, dictionary symbol keys, and effect explain
    payloads.
  - compiled list helpers now guard negative indexes before unsigned
    conversion.
  - direct and buffered printers now tolerate nullable dictionary/set backing
    storage, and `print_value_to_buf` rejects null/zero-capacity output
    buffers.
  - constructor constraint diagnostics now use guarded type registry lookups,
    and instance type inference rejects invalid/out-of-range instance type IDs.
  - deduce tuple persistence now encodes full-width 32-bit `SymbolId` values
    and rejects invalid/out-of-range decoded symbol IDs.
  - materialized metadata deletion now distinguishes `MDB_NOTFOUND` from real
    DBI-open errors, and DBI name/path allocation uses checked addition.
  - relation and rule install failure paths now roll back newly appended
    in-memory schemas/rule signatures when later metadata, handle, or
    persistence steps fail.
  - validation:
    - host `c3c build --warn-deprecation=no`
    - bounded compiler slice: `pass=191 fail=0`
    - bounded deduce slice: `pass=330 fail=0`
    - bounded advanced slice: `pass=1183 fail=0`
    - bounded memory-lifetime-smoke slice: `pass=189 fail=0`
- Parser/compiler, JIT boundary, macro splice, and async/TLS soundness hardening:
  - parser surface interning now rejects invalid symbol IDs before publishing
    import/path/type-annotation/collection/explain/relation/template symbols.
  - compiler synthetic effect-wrapper construction now checks AST arena
    allocation and symbol validity before publishing rewritten bodies.
  - primitive hash bootstrap now fails initialization on invalid symbol keys.
  - compiler integer output avoids `long.min` negation and `usz` through-`long`
    narrowing.
  - macro splice append now rejects improper lists and recursion-limit
    exhaustion.
  - boundary string/error copies guard `len + 1` allocation arithmetic, policy
    `usz` parsing rejects overflow, and JIT continuation yield-failure paths
    restore saved interpreter state.
  - pending raise dispatch now stages payload/list/env construction before
    clearing raise state, and runtime handle setup rejects null non-empty
    clause arrays.
  - TLS offload yield-error paths close pending offload state; TCP/UDP ports
    and signal numbers are checked before `int` narrowing; read-file now fails
    on close failure.
  - validation:
    - host `c3c build --warn-deprecation=no`
    - bounded compiler slice: `pass=191 fail=0`
    - bounded async slice: `pass=61 fail=0`
    - bounded memory-lifetime-smoke slice: `pass=189 fail=0`
    - bounded advanced macro hygiene group: `pass=82 fail=0`
- Schema/deduce payload, external width, and escape lifetime hardening:
  - schema explain/deduce payload paths now reject failed key/value symbol
    interning before publishing dictionary keys or symbol payload values.
  - deduce materialized refresh policy persistence now validates the implicit
    `"manual"` policy symbol before updating relation/schema metadata.
  - deduce integrity payload builders propagate concrete allocation/intern/set
    errors and stop on cons allocation failures instead of returning partial
    payloads or `null`.
  - primitive name matching now guards null primitive payloads and overlong
    expected names before reading the fixed primitive name buffer.
  - checked array construction now stages payload allocation before root-wrapper
    publication; closure escape promotion now releases retained/detached
    environment scope ownership on final wrapper allocation failure.
  - external integer narrowing now validates `exit`, `TimePoint`, Unicode
    codepoint, `fs-open`, `fs-stat`, `tcp-listen`, process-handle lookup, JSON
    pointer symbol fallback, and zlib size-expansion boundaries.
  - validation:
    - host `c3c build --warn-deprecation=no`
    - bounded normal+ASAN data-format slice: `pass=64 fail=0`
    - bounded normal+ASAN unicode slice: `pass=27 fail=0`
    - bounded normal+ASAN compression slice: `pass=27 fail=0`
    - bounded normal+ASAN async slice: `pass=65 fail=0`
    - bounded normal+ASAN compiler slice: `pass=194 fail=0`
    - bounded normal+ASAN memory-lifetime-smoke slice: `pass=190 fail=0`
    - bounded normal advanced slice: `pass=1185 fail=0`
    - bounded ASAN advanced slice: `pass=1172 fail=0`
    - bounded normal+ASAN deduce slice: `pass=330 fail=0`
- FTXUI smoke crash and sidecar hardening:
  - the FTXUI `smoke.omni` crash was a boundary provenance/reuse stack
    overflow on nested effect payload graphs, not an FTXUI lowering defect.
  - `src/lisp/eval_boundary_provenance.c3` now uses a bounded iterative
    alias-safety worklist with visited tracking for nested arrays, dicts, sets,
    method tables, partials, iterators, and cons payloads.
  - scalar leaves no longer consume boundary alias worklist/visited capacity,
    so wide scalar-only payloads do not falsely hit the fail-closed graph cap.
  - `src/lisp/tests_memory_lifetime_boundary_state_groups.c3` adds the nested
    effect-payload regression plus a wide scalar payload regression, and
    `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` keeps both in the
    bounded smoke lane.
  - The bounded smoke lane now also covers a shared composite cycle payload
    regression so repeated composite aliases do not consume visit capacity while
    real cyclic graph identity is still tracked.
  - `src/lisp/prim_ui_ftxui_helpers.c3` now guards FTXUI helper-array growth
    and graph-series allocation arithmetic, while
    `src/lisp/prim_ui_ftxui_lowering.c3` rejects oversized menu item counts
    before narrowing selected indexes to `int`.
  - `src/lisp/prim_ui_ftxui.c3` now checks child component count arithmetic
    before allocating the FTXUI child pointer array.
  - `csrc/ftxui_shim.cpp` now declares `keep_alive` before `component`, so
    component teardown happens before retained borrowed backing data is
    released; the shim also rejects nonzero child counts with null child arrays,
    checks table `rows * cols` overflow, and rejects table selector indexes
    outside FTXUI `int` range.
  - `omni_ftxui_component_wrap_quit_keys(...)` now captures and retains shared
    ownership of the screen loop object in the wrapped component keep-alive
    list instead of capturing a raw screen handle.
  - Status-returning FTXUI C ABI entrypoints now route backend work through a
    shared fail-closed exception guard: `std::bad_alloc` maps to
    `OMNI_FTXUI_STATUS_OUT_OF_MEMORY`, other C++ exceptions map to
    `OMNI_FTXUI_STATUS_INTERNAL_ERROR`, and deferred graph/render/event/quit-key
    callback adapters catch callback exceptions before they can escape through
    FTXUI render/event frames.
  - validation:
    - host `c3c build --warn-deprecation=no`
    - host `scripts/run_ftxui_smoke.sh`
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=192 fail=0`
  - Follow-up on 2026-04-11: `src/lisp/eval_boundary_provenance.c3` now fronts
    the authoritative linear `seen` list with a small bounded `ushort`
    index-table accelerator for repeated composite alias checks. The fast table
    saturates into the existing linear scan, preserving the no-false-negative
    contract and fail-closed graph caps.
  - Scalar/non-graph roots now return before entering the large traversal frame,
    and the large-array walker is isolated behind a small stack-headroom wrapper
    that fails closed to copy-required if the current stack context is too
    shallow.
  - Larger local pointer/index-table attempts regressed FTXUI smoke with a
    `smoke.omni` boundary resolve stack overflow and were not kept; the landed
    table stays deliberately small to fit the effect/FTXUI stack budget.
  - validation after the follow-up fast-set and regression-test addition:
    - host `c3c build --warn-deprecation=no`
    - host `scripts/run_ftxui_smoke.sh`
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=193 fail=0`
  - Destination build-scope commit follow-up:
    - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` no longer
      calls `main::scope_splice_escapes(...)` directly.
    - The narrow low-level splice is now confined to the allowlisted
      `boundary_destination_build_scope_splice(...)` shim in
      `src/lisp/eval_boundary_commit_escape_builders.c3`, preserving the
      previous destination build-scope commit semantics for `cons`, `partial`,
      `iterator`, and `error` wrappers while keeping the boundary facade policy
      gate clean.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - host `scripts/check_boundary_facade_usage.sh`
      - host `scripts/check_boundary_change_policy.sh`
      - host `scripts/check_status_consistency.sh`
      - host `git diff --check`
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=193 fail=0`
  - Boundary alias graph coverage follow-up:
    - `src/lisp/eval_boundary_provenance.c3` now treats `INSTANCE` and
      `MODULE` as graph-bearing alias payloads, matching committed-root graph
      audit coverage.
    - The rare `INSTANCE` / `MODULE` path uses a heap-backed reachability scan
      for value/environment edges instead of growing the hot alias walker stack
      frame.
    - The scan rejects reuse when by-value instance fields or module/env
      bindings still reach the releasing scope, including stale `scope_gen`
      stamps and nested graph payloads.
    - Root-persistent env boxes are still traversed for parent/binding edges;
      they are only excluded from direct temp-frame ownership checks.
    - `src/lisp/tests_memory_lifetime_boundary_state_groups.c3` now covers an
      instance field graph retaining a releasing-scope array payload.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - host `scripts/check_boundary_facade_usage.sh`
      - host `scripts/check_status_consistency.sh`
      - host `git diff --check`
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=194 fail=0`
  - JIT/module value growth and promotion fail-closed follow-up:
    - `src/lisp/value_predicates_accessors_basic.c3` now stores first-class
      `MODULE` values as root-scope descriptor snapshots instead of raw
      addresses into the reallocating interpreter module table.
    - `src/lisp/eval_path.c3` now rejects invalid module descriptors before
      reading module exports or env bindings, covering the AOT bridge path that
      routes through `eval_path_step(...)`.
    - `src/lisp/jit_jit_closure_let_set_helpers.c3` now fails closed when
      cons-field promotion or instance-field boundary copy returns null instead
      of mutating to a null payload.
    - `src/lisp/jit_jit_define_method_table.c3` and
      `src/lisp/aot_type_definitions.c3` now reject null typed-method/global
      define promotion results before mutating method tables or fallbacks.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now covers
      module table growth after first-class module value creation and the JIT
      instance-field boundary-copy fault path.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - host `scripts/check_boundary_facade_usage.sh`
      - host `scripts/check_boundary_change_policy.sh`
      - host `scripts/check_status_consistency.sh`
      - host `git diff --check`
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=196 fail=0`
  - TCO env-copy/recycle fail-closed follow-up:
    - `src/lisp/jit_jit_eval_scope_copy.c3` now uses checked boundary-copy
      results while copying TCO env-frame bindings and aborts the copied frame
      when the boundary copy reports a fault, null result, or `ERROR`.
    - Root-persistent env-box parent rewrites now fail closed if the required
      parent-chain copy fails instead of committing a null or poisoned parent.
    - `src/lisp/runtime_backend_hooks.c3` now preserves the previous `*env_io`
      value when recycle env-copy fails, releases the fresh recycle scope,
      restores the old call scope, retargets any active defer back to that
      scope, and returns an explicit
      `jit: failed to copy TCO recycle env` error.
    - `src/lisp/tests_memory_lifetime_tco_budget_groups.c3` now covers direct
      TCO env-copy failure on an opaque primitive payload and the recycle-hook
      failure path that must preserve env/scope state.
    - `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` wires those two
      regressions into the bounded smoke lane.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - host `scripts/check_boundary_facade_usage.sh`
      - host `scripts/check_boundary_change_policy.sh`
      - host `scripts/check_status_consistency.sh`
      - host `git diff --check`
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=198 fail=0`
  - JIT mutable-local, raise-payload, dispatch-payload, constructor-payload,
    and deduce-integrity payload fail-closed follow-up:
    - `src/lisp/jit_jit_apply_multi_prims_tail.c3` now makes
      `jit_env_lookup_local(...)` fail closed with
      `jit: missing mutable local binding` when a mutable-local helper receives
      a null env or cannot find the requested binding.
    - `jit_env_reparent(...)` now returns the effective env and treats null env
      reparenting as a no-op to the requested parent, allowing compiled capture
      setup to keep using the checked helper result instead of reloading a
      known-null env box.
    - `src/lisp/jit_jit_compile_expr_basic.c3` and
      `src/lisp/jit_jit_emit_helpers.c3` now route mutable-local lookup/capture
      through those checked helper contracts.
    - `src/lisp/prim_collection_hashmap.c3` now exposes
      `make_hashmap_no_raise(...)` for optional dictionary payload storage that
      must not publish a runtime raise as an allocation-failure side effect.
    - `src/lisp/value_constructors.c3` now builds handled raise payload
      dictionaries through that non-raising helper instead of the raising
      `make_hashmap(...)` constructor path, preventing allocation failure
      during payload construction from publishing stale nested `raise_pending`
      state before `raise_error_pending_impl(...)` can return the intended
      top-level error.
    - `src/lisp/eval_dispatch_error_payloads.c3` now also uses the non-raising
      helper for optional dispatch diagnostic payload maps, so a failed
      ancillary payload allocation cannot pre-seed `raise_pending` before the
      intended dispatch/type error is raised.
    - `src/lisp/primitives_meta_types_ctor_helpers.c3` now uses the same
      non-raising helper for optional constructor mismatch diagnostic payload
      maps, and checks constructor payload key interning before constructing key
      symbols.
    - `src/lisp/deduce_relation_ops_validation_payload.c3` now uses the
      non-raising helper for optional integrity/check-context diagnostic
      payload maps and routes payload field insertion through no-raise local
      setters, so a failed ancillary integrity payload allocation cannot
      pre-seed `raise_pending` before the intended integrity violation raise.
    - `src/lisp/deduce_rule_eval_exec_component_state.c3` now uses the same
      non-raising helper for the iteration-limit payload-or-null builder before
      the later iteration-limit raise.
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3` now covers the
      mutable-local helper null-env contracts directly, covers dispatch payload
      allocation failure under an active handler, and the existing pending-raise
      payload allocation failure regression now passes again in the full
      bounded `jit-policy` slice.
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now covers
      constructor mismatch, deduce integrity, and iteration-limit diagnostic
      payload allocation failure under an active raise handler and verifies
      they do not pre-seed pending raise state.
    - validation:
      - host `c3c build --warn-deprecation=no`
      - host `scripts/check_boundary_facade_usage.sh`
      - host `scripts/check_boundary_change_policy.sh`
      - host `scripts/check_status_consistency.sh`
      - host `git diff --check`
      - bounded normal `jit-policy` with FTXUI smoke enabled:
        `pass=51 fail=0`
      - bounded ASAN `jit-policy`: `pass=50 fail=0`
      - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
        `pass=200 fail=0`
- Offload width/narrowing hardening:
  - `src/lisp/scheduler_offload_network.c3` now rejects listener file
    descriptors outside `int` range before calling `tcp_accept_fd(...)`.
  - `src/lisp/eval_repl_server_state.c3` now formats REPL session IDs from a
    guarded `long` value instead of narrowing `next_session_id` through `int`.
  - `src/lisp/scheduler_offload_ops.c3` now formats atomic temp-path
    `unique_id` suffixes from a guarded `long` value instead of truncating to
    `uint`.
  - `src/lisp/scheduler_state_support_types.c3` now asserts the current
    `OffloadWork` pointer-through-`long` payload width contract at compile
    time.
  - validation:
    - host `c3c build --warn-deprecation=no`
    - host `scripts/run_ftxui_smoke.sh`
    - bounded normal+ASAN `async`: `pass=65 fail=0`

- Vulkan math library and parallel solver roadmap:
  - Added `docs/plans/vulkan-math-library-roadmap-2026-04-17.md` as the
    durable forward plan for `TENSOR-100F` / `TENSOR-100G`.
  - The roadmap keeps Vulkan behind backend-neutral `Tensor`, `map`,
    `contract`, `matrix/*`, `to-device`, `device`, and `tensor-backends`
    surfaces. It rejects public `VulkanTensor`, backend-named solver APIs,
    hidden CPU/GPU transfers, and silent dtype downcasts.
  - `Float64` dense row-major remains the first Vulkan math-library tier.
    `Float32` and fixed-width complex are deferred until native Tensor storage
    and public dtype semantics exist. `BigInteger`, `BigFloat`, and
    `BigComplex` must not be lowered to Vulkan.
  - The current serial Vulkan `matrix/solve`, `matrix/lu`, and
    `matrix/inverse` shaders are recorded as correctness-preserving
    small-system/backend bring-up paths, not the performance solver target.
  - `TENSOR-100G` now requires a separate thresholded parallel solver helper
    with a typed buffer/status contract, staged helper shape, parallel pivot
    search, row swaps, elimination, RHS-column back-substitution, and tests
    proving both serial and parallel Vulkan solve paths execute without LAPACK.
  - Updated `TODO.md`, `.agents/PLAN.md`, session reports, Vulkan plan index,
    dtype/layout policy, backend decision note, matrix solver surface decision,
    Tensor scientific plan, and Tensor area status to point at the roadmap.

- Vulkan lazy contract and singular-norm audit hardening:
  - `src/lisp/prim_tensor.c3` now realizes lazy `TENSOR_PAYLOAD_CONTRACT`
    operands through any-device resolution before CPU fallback. Lazy contract
    operands that materialize to Vulkan/CUDA storage can route through backend
    contract helpers, and any unresolved non-CPU operand path fails closed with
    `tensor/backend-unsupported` instead of falling into CPU-only expression
    evaluation.
  - `src/lisp/tests_advanced_stdlib_module_groups.c3` now covers the public
    eager Vulkan map/contract route and internal C3 harness construction of
    lazy map/contract payloads that directly exercise
    `tensor_contract_try_device_value`, including the mixed CPU/Vulkan
    rejection path.
  - `csrc/tensor_vulkan_helpers.c` now validates Vulkan availability, Float64
    support, and shared singular-values shape metadata before the
    `omni_tensor_backend_vulkan_singular_norm_f64` zero-size success path. A
    test-only probe verifies a zero-byte invalid-shape call no longer returns
    success ahead of backend/status validation.
  - `TODO.md` now reports one live actionable item: `TENSOR-100F`. `TENSOR-100E`
    is closed as the correctness-first Vulkan baseline and `TENSOR-100G` is
    closed as the measured parallel-solve baseline; plan and area docs now
    point remaining live Vulkan work at `TENSOR-100F`.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - host focused `advanced-collections-module`: `pass=913 fail=0`
    - `./scripts/check_primitive_docs_parity.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - targeted `git diff --check`

- Direct Vulkan symmetric eigen checkpoint:
  - `csrc/tensor_vulkan_symmetric_eigen_f64.comp` and generated
    `csrc/tensor_vulkan_symmetric_eigen_f64_spv.c` now implement the first
    dense row-major Vulkan `Float64` Jacobi symmetric eigensolver helper.
  - `omni_tensor_backend_vulkan_symmetric_eigen_f64` returns separate
    Vulkan-placed values and vector buffers, maps shader status
    `not-symmetric` to `tensor/not-symmetric`, and is bounded to `n <= 64`.
  - Public `matrix/eigenvalues` and `matrix/eigenvectors` now route concrete
