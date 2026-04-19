# Memory Changelog Index Part 09

Source: `memory/CHANGELOG.md`

## 2026-04-14

- Added standard-normal Boost.Math distribution wrappers:
  - Added `stats/normal-cdf` and `stats/normal-quantile` as one-argument
    standard normal distribution helpers backed by `boost::math::cdf` and
    `boost::math::quantile`.
  - Reused the C++17 Boost.Math C-ABI status-code shim pattern. `cdf` accepts
    finite numeric inputs that can narrow to `Double`; `quantile` accepts finite
    probabilities strictly between `0` and `1`; both return `Double` and fail
    closed on invalid, non-finite, or out-of-Double-range inputs.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `79 passed, 0 failed`
    - direct smokes:
      - `(stats/normal-cdf 0.0)` -> `0.5`
      - `(stats/normal-cdf 1.96)` -> `0.97500210485178`
      - `(stats/normal-quantile 0.975)` -> `1.95996398454005`
      - `(stats/normal-quantile 0.0)` -> `stats/normal-quantile: probability must be between 0 and 1`
      - out-of-range `BigInteger` input to `stats/normal-cdf` -> `stats/normal-cdf: value out of Double range`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`

- Closed the container-only memory-smoke validation gap for the range/TCO fix:
  - Built the local bounded validation image `omni-validation:2026-03-10`.
  - Fixed a StackCtx overflow in nested effect payload return copying. The
    recursive copy path was spending continuation stack on full reuse
    classification for scalar leaves and ordinary data containers while copying
    a small nested dict/list graph. Inside an active StackCtx, copy-to-parent now
    directly copies leaf values and list/array/dict/set data containers instead
    of running the boundary reuse classifier first.
  - validation:
    - `c3c build --obj-out obj`
    - direct nested effect payload predicate -> `true`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
      -> `225 passed, 0 failed`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-tco OMNI_TEST_SUMMARY=1 OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib:/home/christos/Omni/build ./build/main --test-suite lisp`
      -> `1 passed, 0 failed`, `copy_tag_cons=0`, `copy_site_tco=0`
    - `OMNI_LISP_TEST_SLICE=limit-busting OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib:/home/christos/Omni/build ./build/main --test-suite lisp`
      -> `17 passed, 0 failed`
    - `OMNI_LISP_TEST_SLICE=tco-recycling OMNI_TEST_SUMMARY=1 OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib:/home/christos/Omni/build ./build/main --test-suite lisp`
      -> `11 passed, 0 failed`
    - `(length (range 4000))` -> `4000`, about 0.31s; `(length (range 16000))`
      -> `16000`, about 4.17s.

- Extended the Boost.Math scalar wrapper lane with `math/erf` and `math/erfc`:
  - Reused the validated C++17 Boost.Math C-ABI shim pattern from
    `math/lgamma`, adding `boost::math::erf` and `boost::math::erfc` behind
    stable status-code returns.
  - Added the `math/erf` and `math/erfc` primitives, primitive-table
    registration, and AOT lookup entries. Both primitives accept Omni numeric
    values that can narrow to finite `Double`, return `Double`, and fail
    closed on non-finite or out-of-Double-range input.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `73 passed, 0 failed`
    - direct smokes:
      - `(math/erf 1.0)` -> `0.842700792949715`
      - `(math/erfc 1.0)` -> `0.157299207050285`
      - out-of-range `BigInteger` input to `math/erf` -> `math/erf: value out of Double range`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`

- Landed the first Boost.Math scalar wrapper:
  - Added a small C++17 Boost.Math shim for `boost::math::lgamma` with stable
    integer status codes instead of exposing C++ exceptions across the C3
    boundary.
  - Added the `math/lgamma` primitive, primitive-table registration, and AOT
    lookup entry. The primitive accepts Omni numeric values that can narrow to a
    finite `Double`, returns a `Double`, and reports deterministic domain,
    range, finite-input, and out-of-Double-range errors.
  - Kept the public surface backend-neutral; Boost.Math remains an
    implementation backend, not a user-facing package prefix.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `70 passed, 0 failed`
    - direct smokes:
      - `(math/lgamma 6.0)` -> `4.78749174278205`
      - `(math/lgamma 0.5)` -> `0.5723649429247`
      - `(math/lgamma 0.0)` -> `math/lgamma: domain error`
      - out-of-range `BigInteger` input -> `math/lgamma: value out of Double range`

- Fixed the default-stack TCO range headroom crash left after the first
  BigInteger/Tensor integration pass:
  - Root cause: `promote_escape_cons` treated target-chain cons cdr tails as
    reusable without rechecking whether alias analysis would reject the long
    tail for safe reuse. For `(length (range 4000))`, the long target-chain
    tail exceeded `BOUNDARY_ALIAS_MAX_DEPTH`, fell back into recursive disjoint
    promotion, and could exhaust the normal stack.
  - Updated `promote_escape_should_iterate_cons_tail` so releasing/current-scope
    cons tails stay iterative and target-chain tails iterate when
    `boundary_graph_alias_unsafe_for_reuse` classifies reuse as unsafe.
  - Removed the follow-on performance bottleneck by fast-reusing current
    ESCAPE-lane cons values before the full alias scan and by making the TCO
    temp-graph scanner walk cons spines iteratively while only pushing
    graph-carrying child values. A separate cons-spine scan cap keeps long
    proper lists from falling back to the generic graph worklist cap.
  - validation:
    - `c3c build --obj-out obj`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-tco OMNI_TEST_SUMMARY=1 OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `1 passed, 0 failed`, `copy_tag_cons=0`, `copy_site_tco=0`
    - `OMNI_LISP_TEST_SLICE=limit-busting OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `17 passed, 0 failed`
    - `OMNI_LISP_TEST_SLICE=tco-recycling OMNI_TEST_SUMMARY=1 OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `11 passed, 0 failed`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(length (range 4000))'`
      -> `4000` on the normal stack, about 0.32s in this workspace
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(length (range 8000))'`
      -> `8000`, about 1.15s
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(length (range 16000))'`
      -> `16000`, about 4.14s

- Landed the first Boost.Multiprecision scalar slice:
  - Added Boost-backed `BigInteger` storage through a small owned C++ shim over
    `boost::multiprecision::cpp_int`; Omni owns the handle through the normal
    scope/region value lifecycle and does not route it through user-visible FFI
    handles.
  - Added the `BigInteger` constructor for `Integer`, decimal `String`, and
    existing `BigInteger` values, plus `String`, `Integer`, and `Double`
    conversion paths with deterministic `type/arg-mismatch` failures on
    invalid decimal input or out-of-range narrowing.
  - Registered `BigInteger` as a builtin `Number` subtype and updated printing,
    hashing/equality, copy-to-parent, escape/root promotion, and env-copy
    paths for a leaf exact-integer value.
  - Updated `+`, `-`, and `*` so `Integer` overflow promotes to `BigInteger`
    and `BigInteger` combines with `Integer`/`BigInteger`; mixed `Double`
    operations convert through finite `Double` when possible.
  - Deferred `/`, `%`, ordering comparisons, bitwise operations, `gcd`/`lcm`,
    and `parse-number` arbitrary-precision parsing until they have explicit
    surface contracts.
  - validation:
    - installed `libboost-dev` to provide the Boost.Multiprecision headers
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - direct smokes for constructor/stringification, overflow promotion,
      multiplication, type identity, `number?`, invalid decimal failure, and
      out-of-range `Integer` narrowing failure
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `66 passed, 0 failed`
    - all advanced numeric child groups passed; initial validation found a
      normal-stack TCO headroom crash in deep recursive escape promotion for
      `(length (range 4000))`, while the full `advanced-stdlib-numeric` filter
      passed under `prlimit --stack=67108864` with `295 passed, 0 failed`;
      the follow-up fix above resolves the default-stack crash
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed

- Recorded the LAPACK/LAPACKE solver naming checkpoint:
  - a public solver convenience must not be bare `solve`;
  - `linalg/` is not yet accepted as the base namespace;
  - keep solver/decomposition naming unresolved until the public convenience
    layer has a qualifier that fits Omni's tensor/module naming.

- Landed `TENSOR-090B` as the transpose-capable rank-2 BLAS contract slice:
  - Extended the private `cblas_dgemm` shim to accept transpose flags without
    adding a public `matmul`/`linalg-matmul` surface.
  - Broadened the optional BLAS fast path from only `(contract a b [1 0])` to
    all contiguous row-major rank-2 single-axis `Double` contractions:
    `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`.
  - Kept unsupported ranks, vector-specialized cases, zero-size dimensions,
    aliasing, missing BLAS symbols, and non-compatible layouts on the pure C3
    fallback path.
  - validation:
    - `c3c build --obj-out obj`
    - direct Tensor smokes:
      - `[0 0]` transpose-left case -> `84.0`
      - `[1 1]` transpose-right case -> `68.0`
      - `[0 1]` transpose-both case -> `123.0`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `221 passed, 0 failed`

- Landed `TENSOR-090A` as the first direct native BLAS backend slice:
  - Added a private C shim for optional runtime `cblas_dgemm` discovery through
    `dlopen`/`dlsym`, avoiding a hard OpenBLAS/LAPACKE header or link
    dependency on hosts that do not provide those development files.
  - Wired dense rank-2 row-major `Double` contraction equivalent to
    `(contract a b [1 0])` through the `dgemm` fast path when a compatible BLAS
    symbol is available, while preserving the pure C3 contraction kernel as the
    fallback for missing libraries, zero-size dimensions, non-rank-2 cases,
    unsupported axes/layouts, and aliasing failures.
  - Kept ordinary Tensor storage native/scoped under `TensorVal`; this does not
    introduce a public `TensorHandle` or route normal Tensor values through
    user-visible `ForeignHandle` reference counting.
  - Added path-sensitive regression coverage for the BLAS fast path when
    available, with fallback retention when unavailable.
  - validation:
    - `c3c build --obj-out obj`
    - `./scripts/build_omni_chelpers.sh`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - direct Tensor smoke for `(ref (realize (contract a b [1 0])) [1 1])`
      -> `154.0`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `217 passed, 0 failed`

- Renamed the public Tensor expression-to-storage boundary from `materialize`
  to `realize` with no compatibility alias:
  - `realize` now registers as the Tensor boundary primitive and AOT lookup
    target, with Tensor-facing error messages updated to `realize: ...`.
  - Tensor docs, examples, and runtime tests now use `(realize expr)` and
    `(realize expr out)`.
  - Deduce materialized-view surfaces such as `deduce/materialize!` and
    `[relation ... materialized]` remain unchanged and intentionally separate.
- Added the paired-axis shorthand for Tensor `contract`:
  - `(contract a b [1 0])` is the canonical rank-2 contraction spelling for
    contracting left axis `1` with right axis `0`.
  - `(contract a b [[1 0] [2 3]])` covers multi-axis paired contractions.
  - The explicit `(contract a b [1] [0])` left/right axis-list form remains
    accepted.
  - validation:
    - `c3c build --obj-out obj`
    - direct Tensor smokes for `[1 0]`, `[[1 0]]`, `realize (map ...)`, and
      old `materialize` removal
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `216 passed, 0 failed`
    - `OMNI_LISP_TEST_SLICE=reader-dispatch OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `16 passed, 0 failed`
    - `git diff --check`

## 2026-04-13

- Added constrained hybrid reader-tag syntax:
  - `#tag form` now parses as `(tag form)` for symbol-like tags while
    preserving existing built-in hash dispatch forms: `#r"..."`, `#_`, `#N_`,
    and `#| ... |#`.
  - Ordinary reader tags can be implemented as normal one-argument functions,
    for example `(define (reader-inc x) (+ x 1))` with `#reader-inc 41`.
  - Reader tag macros use the canonical declaration surface
    `(define [reader tag] name (syntax-match ...))`, which reuses the existing
    single-transformer macro contract instead of introducing a Common
    Lisp-style readtable hook.
  - Non-tag hash dispatch remains fail-closed, including bare `#` and numeric
    non-comment forms such as `#1x`.
  - validation:
    - `c3c build --obj-out obj`
    - `OMNI_LISP_TEST_SLICE=reader-dispatch LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `16 passed, 0 failed`

## 2026-04-12

- Closed the remaining active TODO lanes for grouped FFI, foreign runtime core,
  Tensor backend boundary, and Tensor broadcasting:
  - `FFI-TOML-004AB` adds `exclude-functions = [...]` to
    `[dependencies.ffi.NAME]`, with strict string-array parsing, generated
    manifest recording, and bind output filtering so denied functions are
    omitted from both raw and facade output. The libclang visitor skips denied
    functions before type mapping while a separate filtered validation pass
    still proves every denied function exists in the headers.
  - `FOREIGN-CORE-002R` adds deterministic async process-spawn cleanup
    failure-injection coverage for `stdin`/`stdout`/`stderr`
    handle-allocation failures after `uv_process` creation, proving process
    state and opened pipe handles are cleaned up on each intermediate failure.
  - `TENSOR-070` is now implemented for tensor-tensor `map`: shapes are
    right-aligned, missing leading axes behave as `1`, singleton axes expand
    by max result dimension, rank-0 tensors broadcast as tensor scalars, and
    incompatible axes raise deterministic `tensor/shape-mismatch` failures.
  - `TENSOR-080` is closed as a design/contract slice only: backend work must
    preserve the pure `Tensor` fallback as oracle, use capability-driven
    optional BLAS/LAPACK/CUDA/cuBLAS routing, avoid implicit CPU/GPU transfer,
    keep ordinary Tensor storage native/scoped, and require explicit
    ownership/finalizer policy for genuinely opaque backend resources.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=276 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=75 fail=0`
    - host targeted `advanced-collections-module` group: `pass=212 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=225 fail=0`
    - `git diff --check`

- Added grouped FFI and Tensor materialization audit regressions:
  - `src/lisp/tests_advanced_io_effect_ffi_groups.c3` now covers grouped
    `[ffi module]` `ForeignHandle` parameter metadata dictionaries by binding
    `fclose` with a borrowed, non-null `File` handle parameter and verifying
    `(fclose nil)` fails closed with the shared non-null handle check.
  - `src/lisp/tests_advanced_stdlib_module_groups.c3` now covers failed
    `(materialize (contract ...) out)` transactionality when a nested lazy
    `map` operand fails during contract materialization, verifying the explicit
    destination remains unchanged before any BLAS/CUDA backend routing lands.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=75 fail=0`
    - host targeted `advanced-collections-module` group: `pass=207 fail=0`
    - `scripts/check_status_consistency.sh`
    - `git diff --check`

- Fixed lazy Tensor root promotion for closure-mapped callables:
  - `src/lisp/eval_promotion_escape_structured.c3` now checks
    `interp.releasing_scope` when deciding whether closure environments need
    detaching during escape promotion, so Tensor expression promotion cannot
    leave an ESCAPE Tensor map callable pointing at a released child env frame.
  - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now mirrors the same
    releasing-scope-first closure env detach predicate for copy-to-parent
    closure clones, so standalone `env_scope` closures are detached when their
    parent env chain still reaches the releasing source scope.
  - `src/lisp/eval_boundary_graph_audit_meta.c3` now treats the root scope as
    persistent rather than TEMP when walking target-scope ancestors, so detached
    closure envs may keep their safe global parent without tripping the TEMP
    reachability audit.
  - committed escape-root graph audit now threads explicit releasing-scope and
    global-env context through `boundary_graph_audit_escape_reachability_result`,
    so the audit rejects releasing-child TEMP edges that are not in the target
    ancestry while stopping traversal at terminal/global env frames.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now covers root
    promotion of a lazy Tensor `map` whose callable captures a local variable,
    asserting both the callable wrapper and captured env are outside the child
    scope after promotion.
  - `src/lisp/tests_memory_lifetime_env_copy_closure_groups.c3` now covers the
    non-null `env_scope` plus source-scope parent-chain copy-to-parent case.
  - `src/lisp/tests_memory_lifetime_boundary_graph_txn_groups.c3` now covers
    commit-scoped releasing-child TEMP edge detection and global-env traversal
    termination in graph audit.
  - `src/lisp/tests_memory_lifetime_groups.c3` now treats zero-copy direct
    promotion as valid for the long cons return fallback gate while still
    rejecting generic fallback copying.
  - `docs/areas/tensor-scientific.md` now reports the latest lazy Tensor
    edge/root-promotion validation count from the changelog-backed slice.
  - validation:
    - `c3c build --warn-deprecation=no`
    - graph-audit repro for lazy Tensor return: `12.0` with no boundary
      violation output
    - advanced FFI capability regression: `pass=74 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=222 fail=0`
    - bounded ASAN container `memory-lifetime-smoke` slice: `pass=222 fail=0`
    - `git diff --check`

- Landed `FOREIGN-CORE-002Q` C ABI library capability reflection tightening:
  - C ABI library `ForeignHandle` construction no longer marks library handles
    with the callable `'call` capability; bound C ABI functions keep reflecting
    `'call` through their `ForeignCallable` descriptors.
  - AOT FFI library registration now uses the same `[load resolve reflect]`
    capability set as the interpreter library constructor.
  - The advanced FFI surface regression now pins library handle capabilities
    to `[load resolve reflect]`, keeping `foreign-describe` capability payloads
    aligned with actual handle behavior.
  - The callable reflection regressions now pin bound C ABI function
    capabilities to `[call reflect]` for direct, post-call, and grouped module
    metadata paths.
  - `docs/LANGUAGE_SPEC.md`, `TODO.md`,
    `docs/plans/foreign-runtime-core-plan-2026-04-11.md`, and
    `docs/areas/ffi-foreign-runtime.md` were updated to carry the same
    `FOREIGN-CORE-002Q` status and foreign-handle wording.

- Synced the foreign-runtime core plan status with the already-landed
  `FOREIGN-CORE-002P` capability reflection authority slice:
  - `docs/plans/foreign-runtime-core-plan-2026-04-11.md` was brought up to
    `FOREIGN-CORE-002P`; the later `FOREIGN-CORE-002Q` entry above carries the
    current status.
  - The plan now records that `foreign-describe` omits reflected `'release`
    capability unless a handle has actual release authority, matching the live
    `TODO.md` entry and prior changelog record.
  - `docs/areas/ffi-foreign-runtime.md` now carries the same operator-facing
    status note.

- Closed `LANG-TENSOR-LAZY-EDGE-ROLLBACK-094` with deterministic lazy Tensor
  expression edge rollback coverage and a root-promotion graph-audit fix:
  - `src/lisp/value_tensor.c3` now has deterministic tensor expression
    edge-failure counters for copy-to-parent and escape-promotion paths, and
    `src/lisp/tests_tests.c3` resets those counters between isolated test
    groups.
  - memory-lifetime coverage now forces a lazy `map` expression edge copy to
    fail after a prior tensor child edge has committed, verifying rollback
    cleans the partially copied tensor edge from the destination scope.
  - lazy Tensor root promotion now promotes tensor operand edges into the
    destination ESCAPE lane instead of leaving the returned Tensor expression
    wrapper pointing at child TEMP operands.
  - boundary graph audit now treats shared dispatch method tables captured as
    Tensor `map` callables as callable descriptors rather than Tensor-owned
    expression data, while still walking non-method-table callable captures and
    Tensor operands.
  - Validation:
    - bounded container graph-audit repro for lazy Tensor return: `5.0` with no
      boundary violation output
    - bounded container `memory-lifetime-smoke` slice: `pass=218 fail=0`

- Landed e2e/AOT compiler audit fixes for entry-helper visibility and Tensor
  primitive lookup:
  - Stage 3 e2e compilation now includes `src/entry_*.c3`, matching the AOT
    backend source set so compiler tests that call entry helpers such as
    `process_bind_dependency`, `build_bind_output_path`, and
    `write_compile_ffi_manifest` have the same `module main` helper surface
    available as the normal build.
  - The e2e source parity guard now pins the `src/entry_*.c3` helper glob so
    the standalone e2e compile path cannot silently drift from the AOT backend
    compile source list again.
  - The compiler primitive hash now treats Tensor surface/runtime primitives,
    including `__tensor-map`, as AOT runtime lookups. This prevents stdlib
    Tensor `map` overload closures from capturing a bare sanitized
    `__tensor_map` C3 identifier instead of using the cached primitive global.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `OMNI_E2E_COMPILE_ONLY=1 ./scripts/run_e2e.sh`
    - full `./scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed`
    - host targeted `compiler` slice: `pass=272 fail=0`
    - host targeted `advanced-collections-module` group: `pass=206 fail=0`

- Landed general audit fixes for structured error payloads, process-wait
  cleanup, Tensor materialization cleanup, `Dict` descriptor printing,
  compile-side FFI sidecar manifests, and `^(Value nil)` annotations:
  - `raise_error_with_payload(...)` now skips structured payload construction
    when no matching `raise` handler can consume it, avoiding root-scoped
    payload dictionaries for ordinary unhandled error returns.
  - `make_process_wait_result(...)` now cleans its partially built root
    dictionary if result insertion fails after publishing an entry, so an OOM
    during status projection cannot leave a half-populated root payload behind.
  - `make_raise_payload(...)` now tracks inserted root dictionary key/value
    pairs and rolls them back if payload key/value insertion fails mid-build.
  - `hashmap_set_checked(...)` and `hashmap_set_symbol_checked(...)` now roll
    back newly promoted root key/value copies when a checked insertion fails
    before publication, including the full-table/no-slot failure path.
    Rollback preserves the original source value so composite promotions clean
    nested materialized children instead of only freeing the top-level wrapper.
  - Nested lazy Tensor `materialize` now cleans temporary child tensors created
    while resolving nested `map`/`contract` expression operands, leaving only
    the top-level materialized result live until caller cleanup.
  - Fresh lazy Tensor materialization cleanup now uses fresh-value cleanup
    instead of interpreting the lazy source graph as expression edges on the
    concrete result; regressions cover nested `contract` materialization and
    failed `map` materialization cleanup.
  - Lazy Tensor `materialize` into an explicit destination now evaluates into a
    staged temporary and commits to the destination only after success. The
    contract aliasing rejection now walks lazy expression operands while
    ignoring zero-byte tensor storage, and failed Tensor constructor data
    validation cleans the unreturned tensor wrapper.
  - `format "%s" Dict` now prints the canonical descriptor
    `#<type Dictionary>`.
  - Compile-side `.ffi-manifest.json` sidecars now escape all JSON C0 control
    bytes and write through a temp file plus final rename so failed pre-rename
    writes preserve an existing final manifest.
  - Parser/runtime dispatch and compiler metadata now treat `^(Value nil)` as
    the nil literal rather than the symbol named `nil`.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-unicode-iterator` group: `pass=151 fail=0`
    - host targeted `advanced-collections-module` group: `pass=206 fail=0`
    - host targeted `advanced-type-dispatch-mutation-chain` group:
      `pass=240 fail=0`
    - host targeted `compiler` slice: `pass=272 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=216 fail=0`

- Landed `AUDIT-ASYNC-SIGNAL-HANDLE-OOM-111` signal-handle allocation
  rollback cleanup:
  - `make_signal_handle(...)` now cleans already-registered signal runtime
    state if the final `ForeignHandle` box/value wrapper allocation fails.
    The value-allocation failure path releases the box, which runs the
    signal-handle finalizer and detaches the native watcher, registry link, and
    retained callback owner scope exactly once.
  - The memory-lifetime runtime allocation regression now forces
    `signal-handle` wrapper allocation failure and verifies the error remains
    `signal-handle: out of memory` while root-scope refcount and signal-handle
    registry count return to their pre-call state.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - bounded container `memory-lifetime-smoke` slice: `pass=210 fail=0`

- Landed `TENSOR-060G` lazy tensor `ref` temporary cleanup:
  - `tensor_ref_value(...)` now cancels and destroys the concrete temporary
    produced when `ref` forces a lazy Tensor expression, so repeated indexing
    of lazy `map`/`contract` expressions does not retain native tensor backing
    storage until scope teardown.
  - The memory-lifetime runtime allocation regression now counts active tensor
    destructors around a lazy `map` `ref` and verifies the temporary
    materialized Tensor is not left as a live scope destructor.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-collections-module` group: `pass=203 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=210 fail=0`

- Updated Tensor residual-lane docs:
  - The plan index, Tensor plan, and Tensor area doc now list
    `LANG-TENSOR-LAZY-EDGE-ROLLBACK-094` alongside
    `LANG-TENSOR-BACKEND-BOUNDARY-092` and
    `LANG-TENSOR-BROADCASTING-093`, so the deterministic rollback test seam
    remains visible without reopening the completed `TENSOR-110` surface lane.
  - Validation:
    - `scripts/check_status_consistency.sh`
    - targeted `rg` over `TODO.md`, `docs/plans/README.md`,
      `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
      `docs/areas/tensor-scientific.md`

- Landed `FOREIGN-CORE-002P` foreign-describe capability authority alignment:
  - `foreign-describe` now reflects the release capability only when the
    handle has actual release authority, so malformed handles with a stray
    `FOREIGN_CAP_RELEASE` bit no longer advertise `'release` unless
    `foreign-release` can legally succeed.
  - The existing malformed opaque-handle regression now asserts both release
    rejection and the absence of reflected release capability.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=74 fail=0`

- Landed `FFI-TOML-004Z` raw control-byte rejection for bind TOML metadata:
  - Strict quoted-string parsing for bind dependency metadata now rejects
    unescaped bytes below `0x20`, so raw tabs/carriage returns cannot enter
    fields such as `library`, `headers`, `functions`, or `strip-prefixes`
    through quoted TOML values.
  - The bindgen regression asserts a dependency containing a raw tab in a
    quoted `library` value is marked invalid and produces no raw, facade, or
    manifest output.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice: `pass=269 fail=0`

- Landed `FFI-TOML-004AA` escaped NUL rejection for bind TOML metadata:
  - Strict quoted-string parsing now rejects `\u0000` and `\U00000000`
    during TOML string decode instead of materializing a C NUL that truncates
    dependency metadata before later validation.
  - The bindgen regression asserts a dependency containing `library =
    "m\u0000evil"` is marked invalid and produces no raw, facade, or manifest
    output.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice: `pass=269 fail=0`

- Landed general audit fixes for type annotations and scheduler task-join
  timeout cleanup:
  - AOT type annotation materialization now accepts dictionary metadata
    annotations such as `^{'T Number}` without trying to intern an empty base
    type.
  - Parser type annotations now normalize the approved `Dict` shorthand to the
    canonical `Dictionary` type in both `^Dict` and compound annotation input.
  - Non-fiber `task-join-timeout` timer-start failure cleanup now clears the
    local timer pointer after closing it, so the deferred cleanup path cannot
    close the same libuv handle twice.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice: `pass=267 fail=0`
    - host targeted `advanced-collections-module` group: `pass=203 fail=0`
    - bounded container `scheduler` slice: `pass=113 fail=0`

- Landed general audit fixes for generated FFI manifest escaping and scheduler
  OS-thread admission cleanup:
  - Compile-side FFI contract JSON string emission now escapes `\b`, `\f`, and
    the remaining C0 control characters as JSON-safe `\u00XX` sequences.
  - Bindgen generated manifest TOML string emission now uses the same
    fail-closed control-character escaping for dependency/path/prefix strings.
  - `scheduler_admit_os_thread_work(...)` now releases any shared payload on
    invalid-handle, thread-table, generation, and OS-thread start failure exits
    before returning the scheduler error.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice: `pass=264 fail=0`
    - bounded container `scheduler` slice: `pass=112 fail=0`

- Landed `TENSOR-060F` tensor boundary/materialize fail-closed hardening:
  - `boundary_commit_escape(...)` now allows tensor values to use the existing
    non-unique destination retry promotion route, matching the dedicated tensor
    escape-copy path.
  - Concrete tensor `materialize` fast paths now validate source backing
    storage before returning or copying malformed tensors.
  - The TOML bind parser also now accepts trailing commas in string arrays and
    decodes the remaining TOML basic-string escapes used by bind metadata,
    including `\b`, `\f`, `\uXXXX`, and `\UXXXXXXXX`.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice: `pass=263 fail=0`
    - host targeted `advanced-collections-module` group: `pass=202 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=208 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-collections-module` group: `pass=202 fail=0`

- Landed `FFI-TOML-004W` malformed dependency section fail-closed cleanup and
  validation-status checker coverage:
  - Malformed section-header lines starting with `[` now mark the currently
    active FFI dependency invalid before resetting parser context, so a broken
    `[dependencies.ffi.NAME` line cannot leave the previous dependency looking
    valid while subsequent keys are ignored.
  - `scripts/check_status_consistency.sh` now covers the FFI foreign-runtime
    and validation-status area docs, including their current status and `As of`
    freshness against the latest changelog date.
  - The validation all-slice notes now explicitly label runs using
    `OMNI_SKIP_TLS_INTEGRATION=1` as bounded all-slice-without-TLS baselines,
    not replacements for the TLS integration gate.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice: `pass=260 fail=0`
    - `scripts/check_status_consistency.sh`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=74 fail=0`

- Landed general audit fixes for grouped FFI manifests, bind TOML escaping, and
  tensor boundary cleanup:
  - Compile-mode FFI sidecar manifest emission now recurses into lowered
    `[ffi module]` blocks, so grouped C ABI modules emit the same
    `.ffi-manifest.json` library/function descriptors as the single-function
    declarative forms.
  - The bind-path minimal TOML scanner now counts contiguous backslashes before
    quote delimiters and decodes strict quoted string escapes for scalar and
    string-array values. Inline comments after values such as `"C:\\tmp\\"`
    no longer confuse the scanner.
  - Tensor boundary rollback now cleans already-copied/promoted expression
    child values before freeing a cloned tensor payload, and generic
    materialized-value cleanup recurses through tensor expression edges.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice: `pass=260 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=74 fail=0`
    - host targeted `advanced-collections-module` group: `pass=200 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=207 fail=0`

- Landed `FOREIGN-CORE-002J` single-release foreign handle authority:
  - `make_ffi_box` now normalizes mixed release authority by making the
    explicit finalizer callback authoritative when both a finalizer and
    `free_lib_handle` are supplied.
  - This prevents `foreign-release` / scope teardown from executing both a
    native finalizer and `mem::free` for the same foreign payload.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=67 fail=0`

- Landed `FOREIGN-CORE-002K` manual-return ownership preservation:
  - Return-handle construction now preserves explicit ownership in the
    `FfiHandle` descriptor (including `manual`) when an FFI binding supplies
    `'ownership` metadata.
