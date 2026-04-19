# Active TODO Index Part 06

Source: `TODO.md`

- [x] `AUDIT-HASHMAP-PROMOTION-ROLLBACK-116` clean checked hashmap promotion
  copies on failed publication
  - closure evidence:
    - `src/lisp/prim_collection_hashmap.c3` now rolls back newly promoted root
      key/value copies from `hashmap_set_checked(...)` and
      `hashmap_set_symbol_checked(...)` when insertion fails before
      publication.
    - regression coverage forces the checked no-slot failure path after child
      promotion and verifies root value destructor counts remain unchanged.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded container `memory-lifetime-smoke` slice: `pass=216 fail=0`

- [x] `TENSOR-060J` stage lazy destination materialization
  - closure evidence:
    - `src/lisp/prim_tensor.c3` now evaluates lazy Tensor expressions into a
      temporary concrete Tensor before copying into an explicit destination, so
      failed `materialize` leaves the caller destination unchanged.
    - the contract destination/source alias rejection now walks lazy expression
      operands before the staged write path, ignores zero-byte tensor storage,
      and failed Tensor constructor data validation cleans the unreturned tensor
      wrapper.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-collections-module` group: `pass=207 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=216 fail=0`

- [x] `AUDIT-DICT-DESCRIPTOR-115` canonicalize `Dict` descriptor printing
  - closure evidence:
    - `src/lisp/value_print_helpers.c3` now prints `Dict` type descriptors as
      the canonical `#<type Dictionary>` while keeping `Dict` as the approved
      shorthand alias.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-unicode-iterator` group: `pass=151 fail=0`

- [x] `FFI-MANIFEST-006` harden compile-side FFI sidecar manifests
  - closure evidence:
    - `src/entry_compile_manifest.c3` now escapes every JSON C0 control byte
      in compile-side `.ffi-manifest.json` string fields.
    - compile-side manifest writes now publish through a sibling temp file and
      final rename, preserving an existing final manifest if a pre-rename
      failure occurs.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice: `pass=272 fail=0`

- [x] `AUDIT-VALUE-NIL-LITERAL-113` parse `^(Value nil)` as the nil literal
  - closure evidence:
    - `src/lisp/parser_type_annotation_helpers.c3` now treats `nil` inside
      `^(Value ...)` as the NIL literal rather than the symbol named `nil`.
    - runtime dispatch coverage verifies `^(Value nil)` matches `nil` and
      does not match `'nil`.
    - compiler coverage verifies generated AOT metadata emits
      `.val_tag = lisp::ValueTag.NIL` for `^(Value nil)`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-type-dispatch-mutation-chain` group:
      `pass=240 fail=0`
    - host targeted `compiler` slice: `pass=270 fail=0`

- [x] `AUDIT-ASYNC-SIGNAL-HANDLE-OOM-111` clean `signal-handle`
  registered runtime state on wrapper allocation failure
  - closure evidence:
    - `src/lisp/async_process_signal_runtime.c3` now detaches the registered
      signal handle state and frees the `SignalHandle` if the final
      `ForeignHandle` box allocation fails.
    - value-wrapper allocation failure now releases the box, letting the
      signal-handle finalizer detach the native watcher, registry entry, and
      retained callback owner scope exactly once.
    - regression coverage forces `signal-handle` wrapper allocation failure
      and verifies root-scope refcount plus signal registry count return to
      their pre-call values.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded container `memory-lifetime-smoke` slice: `pass=210 fail=0`

- [x] `TENSOR-060G` clean lazy Tensor `ref` materialization temporaries
  - closure evidence:
    - `src/lisp/prim_tensor.c3` now cancels and destroys the concrete
      temporary when `ref` forces a lazy Tensor expression.
    - regression coverage counts active tensor destructors around a lazy
      `map` `ref` and verifies no temporary materialized tensor remains live.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-collections-module` group: `pass=203 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=210 fail=0`

- [x] `AUDIT-TYPE-ANNOTATION-DICT-AOT-110` align `Dict` annotation aliasing
  and AOT dictionary metadata annotation materialization
  - closure evidence:
    - `src/lisp/parser_type_annotation_helpers.c3` now normalizes `Dict` to
      the canonical `Dictionary` type symbol for annotation parsing.
    - `src/lisp/aot_type_spec_helpers.c3` now accepts metadata-dictionary
      annotations with no base type, preserving the metadata payload for
      generated AOT type forms.
    - regression coverage now pins `^Dict` dispatch and AOT materialization of
      dict metadata annotations such as `^{'T Number}`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice: `pass=267 fail=0`
    - host targeted `advanced-collections-module` group: `pass=203 fail=0`

- [x] `AUDIT-SCHEDULER-TASK-JOIN-TIMER-START-109` close failed
  non-fiber task-join timeout timers exactly once
  - closure evidence:
    - `src/lisp/scheduler_primitives_task_wait_join.c3` now clears the local
      timeout timer pointer after explicit failure-path close, so deferred
      cleanup cannot double-close the libuv handle.
    - `src/lisp/tests_scheduler_groups_more.c3` now pins the timer-start
      failure path through a narrow scheduler fault-injection seam.
  - validation:
    - bounded container `scheduler` slice: `pass=113 fail=0`

- [x] `AUDIT-SCHEDULER-OS-THREAD-ADMIT-SHARED-108` release shared payloads on
  OS-thread work admission failure exits
  - closure evidence:
    - `src/lisp/scheduler_primitives_thread_scaffold.c3` now releases
      `work.shared` before returning scheduler errors for missing handle,
      full thread table, invalid generation lookup, and OS-thread start
      failure exits.
    - `src/lisp/tests_scheduler_groups_more.c3` now pins the direct invalid
      handle path and verifies the shared registry no longer resolves the
      payload after admission failure.
    - validation:
      - bounded container `scheduler` slice: `pass=112 fail=0`

- [x] `LANG-TENSOR-SCIENTIFIC-SURFACE-091` implement the canonical Tensor
  scientific-computing surface
  - design note: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`
    locks the proposed surface around `Tensor`, tensor-dispatched `map`,
    `contract`, and `materialize`.
  - shipped slice: `TENSOR-010` runtime representation, destructor, printing,
    type identity, boundary copy/promotion support, and `tensor?`, `dtype`,
    `shape`, `rank`, and `length`.
  - shipped slice: `TENSOR-020` constructor and indexing:
    `(Tensor Float64 shape data-or-scalar)` constructs native Float64 tensors
    from scalar fills or exact-length array/proper-list data, and
    `(ref tensor index-array)` indexes tensors with per-axis negative index
    support.
  - shipped slice: `TENSOR-030` concrete tensor materialization:
    `(materialize tensor)` returns the already-concrete tensor,
    `(materialize tensor out)` copies into a mutable exact-shape/dtype
    destination, and `(materialize scalar out)` fills a destination tensor.
  - shipped slice: `TENSOR-040` elementwise `map`:
    unary tensor, tensor-scalar, scalar-tensor, and exact-shape tensor-tensor
    `Float64` inputs are routed through generic `map` dispatch.
  - shipped slice: `TENSOR-050` pure `Float64` `contract`:
    `(contract a b left-axes right-axes)` supports axis-list contraction,
    rank-0 scalar results, multi-axis contractions, and deterministic
    axis/dimension diagnostics.
  - shipped slice: `TENSOR-060A` destination-fusion audit:
    true `(materialize (map ...) out)` / `(materialize (contract ...) out)`
    fusion requires a lazy tensor-expression value or a macro/special-form
    rewrite because eager evaluation materializes the source before
    `materialize` receives it.
  - shipped slice: `TENSOR-060B-prep` destination-ready kernels:
    the first pure `map` and `contract` paths were split through internal
    helpers that can evaluate into staged concrete tensor storage before
    committing successful destination materialization to caller-provided
    tensors.
  - shipped slice: `TENSOR-060B` lazy tensor-expression protocol:
    Tensor-dispatched `map` and `contract` now return lazy Tensor expression
    payloads under the existing `Tensor` value, and `materialize` can allocate
    a concrete result or stage evaluation before copying into a destination
    tensor after success.
  - shipped slice: `TENSOR-060C` tensor shape/zero-contraction hardening:
    valid non-negative Tensor dimensions no longer trip the platform-size
    overflow guard on 64-bit builds, and zero-size contracted axes produce the
    additive identity instead of reaching divide/modulo-by-zero during
    contraction materialization.
  - shipped slice: `TENSOR-060D` materialize edge hardening:
    rank-0 concrete destination materialization, zero-size destination paths,
    aliased elementwise `map` destination materialization, and duplicate-axis
    detection after negative-axis normalization are now covered.
  - shipped slice: `TENSOR-060E` boundary rollback cleanup:
    tensor expression child values copied or promoted across boundary paths are
    rolled back if a later tensor copy/promotion or wrapper allocation step
    fails; generic materialized-value cleanup also recurses through tensor
    expression edges.
  - shipped slice: `TENSOR-060F` boundary/materialize fail-closed hardening:
    tensor values now use the non-unique destination retry promotion path at
    return boundaries, and concrete `materialize` fast paths validate source
    backing storage before returning or copying malformed tensors.
  - shipped slice: `TENSOR-110` cleanup lane completion:
    - `examples/scicomp_demo.omni` now uses canonical `Tensor`, `map`,
      `contract`, and `materialize` forms; old `vec-*`, `mat-*`, and
      `mat-mul` prototype forms are no longer part of the shipped surface.
    - `map` and `contract` now return lazy Tensor expressions through the
      existing `Tensor` value, and `materialize` controls when concrete
      storage is allocated.
    - regression coverage now includes lazy-map/contract return tests and
      closure-capture behavior.
    - metadata examples intentionally keep `Array` where required (shape,
      axes, and metadata payload transport), so the cleanup does not alter
      metadata-shape conventions.
  - closure note:
    - the pure public Tensor surface was split into separate backend-boundary
      and broadcasting lanes; both are now closed above.

- [x] `AUDIT-IMMER-FFI-COMPAT-101` retire the optional Immer bridge instead
  of extending FFI around it
  - closure evidence:
    - the owner clarified that C++/Immer support was not intended as core
      language infrastructure.
    - deleted the unsupported `lib/immer.omni` wrapper and the `lib/immer/`
      C++ bridge tree, including the tracked nested `lib/immer/immer` gitlink.
    - deleted the obsolete `docs/plans/immer-ffi-compat-plan-2026-04-11.md`
      compatibility plan.
    - no `^Value`, automatic value-handle, or pointer-only rewrite was added
      for this legacy optional library.
  - validation:
    - active source/reference search confirms no supported surface references
      remain outside historical TODO/changelog/plans.
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- [x] `AUDIT-LET-BRACKET-SHORTHAND-102` remove legacy outer `let [...]`
  shorthand from live library code
  - closure evidence:
    - `lib/core.omni` macro expansions now emit flat-pair `let` and named
      `let` binding lists.
    - `lib/test-utils.omni` now uses flat-pair `let` binding syntax.
    - the remaining `let [` text matches are only the syntax decision note and
      the negative parser regression that verifies the shorthand is rejected.
  - validation:
    - `rg "\(let\s*\[" -n lib stdlib tests examples docs src` returns only
      the syntax decision note and the negative parser regression.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --check lib/test-utils.omni`
    - bounded `basic` slice: `pass=142 fail=0`
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- [x] `AUDIT-IMMER-PERSISTENT-DISPATCH-088` harden persistent collection
  wrappers before adding generic dispatch
  - closure evidence:
    - `persistent-dictionary` now rejects odd key/value argument lists instead
      of silently dropping the final key.
    - persistent collection values are now tagged Omni wrapper dictionaries
      around the raw bridge handle, and public operations unwrap by expected
      family before calling the C++ bridge.
    - `persistent-array?`, `persistent-dictionary?`, and `persistent-set?`
      predicates now exist, while generic `count`/`conj`/`into` dispatch
      remains intentionally frozen.
    - residual runtime bridge compatibility is split into
      `AUDIT-IMMER-FFI-COMPAT-101`.
    - superseded by `AUDIT-IMMER-FFI-COMPAT-101`, which retires the optional
      Immer bridge entirely instead of adding FFI machinery around it.
  - historical validation before the bridge was retired:
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --check lib/immer.omni`
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - runtime preload remains blocked by `AUDIT-IMMER-FFI-COMPAT-101`
      because `ffi-declare` is no longer bound.
    - bridge build remains blocked locally: `make -C lib/immer test_bridge`
      fails on missing `immer/flex_vector.hpp`.

- [x] `AUDIT-LIST-HELPER-ALIAS-086` keep lowercase `list` as an approved public
  helper
  - closure evidence:
    - `List` remains the canonical constructor/conversion surface.
    - lowercase `list` is explicitly approved as an idiomatic Lisp
      list-builder/conversion helper, not a new canonical constructor family.
    - runtime primitive registration and compiler primitive hash coverage
      already route `List` and `list` through the same implementation.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- [x] `AUDIT-PROCESS-WRAPPER-PAYLOAD-FALLBACK-100` preserve process
  runtime payload errors through stdlib wrappers
  - closure evidence:
    - `process-spawn` and `process-kill` now have untyped fallbacks after their
      typed wrapper methods, matching the `tcp-*`, `offload`, and
      `thread-spawn` pattern.
    - invalid command and signal arguments now reach the runtime
      `io/process-*` payload contracts instead of being intercepted by generic
      typed dispatch.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `advanced-effect-union-limit` subgroup: `pass=67 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-FILESYSTEM-SURFACE-087` canonicalize filesystem wrappers on
  `fs-*`
  - closure evidence:
    - stdlib no longer exports `filesystem-*` compatibility aliases.
    - runtime, compiler primitive hash entries, docs, and tests now agree on
      the canonical `fs-*` wrapper family.
    - regression coverage verifies representative long-form filesystem aliases
      are unbound.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `advanced-effect-union-limit` subgroup: `pass=65 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- [x] `AUDIT-COMPILER-PRIMITIVE-HASH-COVERAGE-099` complete compiler
  primitive hash coverage for public runtime primitives
  - closure evidence:
    - public runtime primitive value references now have compiler primitive hash
      entries, reducing the public-ish missing-hash audit set to zero.
    - the primitive hash table was resized from 256 to 512 to preserve a safe
      load factor after expanding coverage to 195 entries.
    - regression coverage now verifies `sin`, `string-byte-length`, and
      `sort-by` are looked up as primitives, not captured as C3 locals.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - audit script: `hash entries=195`, `focus missing public-ish hash entries=0`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`
