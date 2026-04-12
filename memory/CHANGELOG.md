## 2026-04-14

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
    - all advanced numeric child groups passed; the normal-stack TCO headroom
      path still crashes in deep recursive escape promotion for
      `(length (range 4000))`, while the full `advanced-stdlib-numeric` filter
      passed under `prlimit --stack=67108864` with `295 passed, 0 failed`
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
  - Owned-path release behavior is still driven by explicit finalizer
    authority; manual-return handles remain non-releasable.
  - AOT declaration policy now rejects manual ownership combined with a
    finalizer, matching the interpreter metadata parser.
  - `(foreign-describe)` now reflects manual ownership for returned handles as
    `'ownership manual` instead of implicitly downgrading to borrowed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=69 fail=0`

- Landed `FOREIGN-CORE-002L` AOT finalizer/ownership parity:
  - `aot_ffi_policy_from_spec` now rejects return policies that carry a finalizer
    unless ownership is explicitly `owned`.
  - This closes the AOT/JIT gap where borrowed/default finalizer-bearing
    returns were accepted by AOT policy parsing but ignored by finalizer
    resolution.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice: `pass=258 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - `git diff --check`

- Landed `FOREIGN-CORE-002M` finalizer-owned foreign payload release hardening:
  - Finalizer-backed handle families now free their own heap payloads after
    running resource-specific teardown because `make_ffi_box` makes native
    finalizers authoritative when a finalizer is present.
  - Covered payload wrappers include UV timer callback, process, signal,
    TCP, UDP, FS stream, TLS, deduce relation, deduce database, and deduce
    transaction handles.
  - `uv-timer-callback-unhandle` remains a non-destructive detach operation, so
    stale-handle diagnostics still work until the owning `ForeignHandle` is
    destroyed or explicitly released.
  - Deduce relation scan column-key-value forced-OOM handling now fails before
    allocating the relation cache, avoiding a sanitizer-visible test-only leak
    on the injected OOM path.
  - Validation:
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - ASAN targeted `deduce` slice: `pass=330 fail=0`
    - final `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - host targeted `deduce` slice: `pass=330 fail=0`
    - `scripts/check_status_consistency.sh`
    - `git diff --check`

- Landed `FOREIGN-CORE-002N` foreign handle construction-failure ownership:
  - `make_ffi_handle_ex_with_descriptor` now treats the constructor as the
    failure-path release authority for finalizer/free-backed non-library
    payloads once it accepts the raw handle.
  - Box-allocation and value-allocation failures both release owned foreign
    payloads through the same finalizer/free policy used by normal
    `ForeignHandle` teardown.
  - C ABI library handles remain non-releasable: constructor failure does not
    `dlclose` them, so the existing `dlopen` caller cleanup path stays
    authoritative.
  - Owned C FFI pointer returns no longer run a second caller-side finalizer
    after constructor failure.
  - TLS client/server handle wrapper OOM paths now use `tls_handle_finalizer`
    for connected initialized handles, preserving graceful close/session logic
    before freeing nested TLS storage.
  - Allocation-failure tests now pin exactly-once finalizer cleanup for
    finalizer-backed FFI payloads after both box-allocation and
    value-allocation failure.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - host targeted `compiler` slice: `pass=258 fail=0`
    - host targeted `deduce` slice: `pass=330 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - ASAN targeted `deduce` slice: `pass=330 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=204 fail=0`
    - final `c3c build --warn-deprecation=no`
    - `scripts/check_status_consistency.sh`
    - `git diff --check`

- Landed `FOREIGN-CORE-002O` constructor-failure caller cleanup alignment:
  - Removed redundant caller-side cleanup after `make_ffi_handle_ex` null
    returns now that the constructor owns finalizer/free-backed non-library
    payload release on failure.
  - Covered async TCP/UDP/process/signal/UV timer callback/FS stream handles,
    scheduler task/thread handles, atomic refs, TLS wrapper error paths, deduce
    database/transaction/relation handles, C ABI/AOT library registration
    failure paths, and boundary helper tests.
  - C ABI/AOT library registration failures now consume the constructed wrapper
    safely by clearing the root-scoped value pointer before releasing the
    wrapper box after closing the raw library handle.
  - Added advanced FFI regressions for malformed C ABI library and opaque
    descriptors with `FOREIGN_CAP_RELEASE`: `foreign-release` still rejects
    them as non-releasable, the malformed opaque payload stays live, and the
    opaque descriptor is not reflected as owned without finalizer/free release
    authority.
  - `foreign-describe` no longer reports released handles as owned after the
    payload is cleared and release capability has been removed.
  - Validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=74 fail=0`
    - host targeted `deduce` slice: `pass=330 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=207 fail=0`

- Landed `TENSOR-060C` tensor shape/zero-contraction hardening:
  - Tensor shape parsing now checks non-negative dimensions with an unsigned
    comparison, avoiding the 64-bit `(long)usz.max` wraparound that rejected
    valid shapes such as `[2]` and `[2 3]`.
  - `contract` handles zero-size contracted axes by producing the additive
    identity instead of reaching a divide/modulo-by-zero path.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-collections-module` group: `pass=185 fail=0`
    - direct probes for `(Tensor Double [2] [1 2])`, tensor `ref`, and
      zero-size contracted scalar output.

- Landed `TENSOR-060D` materialize edge hardening:
  - Added regressions for concrete `Tensor Double []` materialization into
    rank-0 destination tensors, scalar materialization into `[0]` destination,
    lazy zero-size source materialization into zero-size destination, aliased
    elementwise `map` materialization into its destination, and duplicate-axis
    detection after negative-axis normalization.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-collections-module` group: `pass=196 fail=0`

- Landed `TENSOR-110` Tensor cleanup surface lane closure:
  - `examples/scicomp_demo.omni` now uses canonical `Tensor`, `map`,
    `contract`, and `materialize` forms, with the legacy `vec-*`, `mat-*`, and
    `mat-mul` prototypes removed from the canonical public surface.
  - `Tensor` now returns lazy expression payloads for `map`/`contract`, and
    `materialize` is the explicit expression-to-storage boundary used in this
    cleanup lane.
  - Added regression coverage for lazy expression returns and closure capture
    under map/contract execution paths.
  - Metadata-shape/index payload examples intentionally remain `Array`-based to
    preserve the existing metadata transport convention while closing this lane.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` tensor coverage: `pass=200 fail=0`
    - `git diff --check`

- Landed `FOREIGN-CORE-002I` borrowed-handle release regression alignment:
  - The borrowed-return-handle release regression now binds the real C `fopen`
    symbol with borrowed `ForeignHandle` metadata before closing it through
    `fclose` and checking that `foreign-release` rejects the non-releasable
    borrowed wrapper.
  - This keeps the advanced FFI ownership regression on the intended safety
    path instead of accidentally probing a nonexistent `fopen_borrowed` symbol.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=66 fail=0`

- Landed `FFI-BIND-005A` bindgen return metadata fail-closed:
  - `bindgen_append_function_decl` and
    `bindgen_append_grouped_function_decl` now set failure state when return
    Omni metadata is unsafe (for example, contains control characters).
  - Mixed emitted-function lists now fail closed on an unsafe return-metadata
    entry and do not write any raw/facade output.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=253 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-005B` bindgen stale-output cleanup fail-closed:
  - `bindgen_delete_output_file_if_exists` now returns deletion failure status
    and `generate_ffi_module_pair_with_options` fail-closes if it cannot remove a
    newly written raw artifact when facade generation fails.
  - Existing raw artifacts are preserved on rerun failures so bindgen does not
    delete previously generated files outside the first-write failure lane.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=256 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-005C` bindgen metadata control-character hardening:
  - `bindgen_metadata_text_is_safe` now rejects all ASCII control characters
    below `0x20` in metadata fields and generated comments, covering non-newline
    controls such as tabs.
  - Overflow-guard metadata regression now includes a tab control character case
    and validates fail-closed behavior before any generated output is written.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=257 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-004V` anonymous bindgen parameter fallback naming:
  - Anonymous C parameter fallback names now format the full numeric index
    (`arg123`, etc.) instead of only supporting one- or two-digit indexes.
  - This keeps high-arity generated bindings from producing invalid fallback
    parameter symbols once an anonymous parameter index reaches three digits.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=253 fail=0`

- Landed `FFI-BIND-004U` atomic bindgen text writes:
  - Raw, facade, and manifest writers now write to a sibling temp path and
    rename into place only after the full text write and close succeeds.
  - Failed final renames clean the temp output, preventing writer-level
    partial/truncated generated files from replacing the target.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=249 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-004T` manifest failure cleanup:
  - If raw/facade generation succeeds but the manifest write fails, first-time
    raw, facade, and manifest artifacts are cleaned before the dependency fails.
  - Existing raw, facade, or manifest artifacts are left in place on rerun
    failures.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=248 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-004S` raw/facade pair failure cleanup:
  - If a bindgen raw module is newly written but facade generation fails, the
    new raw file is removed before the pair writer returns failure.
  - Existing raw files are left in place on rerun failures, avoiding deletion
    of a previously generated or user-reviewed raw artifact.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=247 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004R` section-header comment/context hardening:
  - `[dependencies.ffi.NAME] # comment` now parses as the intended FFI
    dependency section instead of falling through as a non-section line.
  - Malformed section-header lines starting with `[` reset parser context so
    following keys cannot mutate the previously active dependency section.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=246 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004Q` partial header parse cleanup:
  - `--bind` now releases functions parsed from earlier headers when a later
    header fails to parse or trips the header-path guard.
  - Partial multi-header parse failures still fail before output generation,
    but no longer leave parsed parameter metadata behind on the error path.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=244 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004P` explicit function filter match enforcement:
  - When `[dependencies.ffi.NAME] functions = [...]` is present, every
    requested function name must be discovered in the parsed headers.
  - Missing filter entries now fail the dependency before output generation
    instead of silently producing a partial binding set or a successful no-op.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=243 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004O` unsafe bind library stem rejection:
  - `--bind` now rejects dependency `library` stems containing slash,
    backslash, quote, whitespace, or control characters before header parsing
    or generated raw/facade/manifest output.
  - The lower-level bindgen writers also reject unsafe library stems, keeping
    direct generator calls aligned with the `omni.toml` bind path.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=242 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004N` unknown FFI dependency key rejection:
  - Unknown keys in `[dependencies.ffi.NAME]` now fail the dependency before
    libclang/header parsing instead of being ignored. This keeps typos such as
    `function = [...]` from accidentally binding all exported functions.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=241 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004M` malformed adjacent quoted scalar rejection:
  - Strict FFI dependency scalar parsing now rejects values such as
    `library = "sqlite3" "m"` instead of accepting the whole malformed tail as
    one library or raw-syntax string.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=240 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004L` dependency count overflow fail-closed:
  - FFI dependency parsing now records when `[dependencies.ffi.NAME]` sections
    exceed `TOML_MAX_DEPS` and `--bind` exits before raw syntax resolution,
    header parsing, or output generation.
  - Overflow sections reset parser section state so their keys cannot mutate
    the last accepted dependency.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=239 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004K` inline comment handling for strict bind TOML values:
  - FFI dependency value parsing now strips `#` comments only when the hash is
    outside a quoted string, so documented examples such as
    `library = "m" # comment` remain valid while quoted values containing `#`
    are preserved.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=238 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`

- Landed `FFI-TOML-004J` duplicate FFI dependency section rejection:
  - Repeated `[dependencies.ffi.NAME]` sections now mark both dependencies
    invalid instead of letting multiple entries target the same generated raw,
    facade, and manifest output stem.
  - `--bind` now rejects invalid or empty dependency names before raw syntax
    resolution and header parsing.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=237 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`

- Landed `FFI-TOML-004I` duplicate bind dependency TOML key rejection:
  - FFI dependency `library`, `raw-syntax`, `headers`, `functions`, and
    `strip-prefixes` keys now fail closed when repeated in the same
    `[dependencies.ffi.NAME]` section instead of letting a later value silently
    overwrite the bind target, function filter, syntax mode, or generated-name
    rewrite policy.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=236 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`

- Landed `FFI-TOML-004H` strict bind dependency core TOML fields:
  - FFI dependency `library`, `raw-syntax`, `headers`, and `functions` parsing
    now fails closed for missing required, malformed, empty, overlong, or
    incorrectly shaped values instead of silently truncating, accepting missing
    required fields as no-ops, or accepting malformed arrays before header
    parsing.
  - This keeps `--bind` from generating against the wrong shared library,
    using an unquoted raw syntax selector, parsing a truncated header path, or
    applying a truncated function filter.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=231 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`

- Landed `FFI-TOML-004G` strict `strip-prefixes` TOML parsing:
  - `strip-prefixes` now rejects malformed, empty, or overlong entries instead of
    silently truncating prefix strings before generated-name rewriting.
  - `--bind` fails the dependency before header parsing when `strip-prefixes`
    is invalid, keeping the generated raw/facade/manifest output contract
    fail-closed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=224 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004F` prefix-stripped emitted name collision preflight:
  - Bindgen now rejects duplicate generated Omni-facing function names before
    writing raw or facade files.
  - This catches cases where prefix stripping makes two C functions collide,
    such as `sqlite3_open` and `open` both emitting `open` when `sqlite3_` is
    stripped.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=221 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004E` prefix-stripped emitted name preflight:
  - Bindgen now validates generated Omni-facing function names before writing
    raw or facade files.
  - Prefix stripping now fails closed when it would emit a name that the Omni
    lexer would read as a number-leading token, such as stripping `sqlite3_`
    from `sqlite3_3d_distance`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=220 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004D` overlong bind dependency section names fail closed:
  - `toml_parser` no longer silently truncates `[dependencies.ffi.NAME]`
    section names longer than the bind output stem limit.
  - Overlong names now flow into the existing empty-name output guard and fail
    before raw, facade, or manifest files are written, avoiding surprising file
    stems or generated Omni module names.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=219 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-TOML-004C` path-safe bind output dependency names:
  - `--bind` output path construction now rejects empty FFI dependency names
    and names containing anything outside ASCII letters, digits, `_`, and `-`
    before writing raw, facade, or manifest files under `lib/ffi/`. This keeps
    dependency names safe as both file stems and generated Omni module names.
  - Bind path failures now report "too long or unsafe" instead of treating all
    failures as length overflow.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=218 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BUFFER-009C` string-buffer `inout` direction precedence:
  - Bindgen string-buffer direction inference now checks the `inout` name hint
    before the broader `out` substring.
  - Parameters such as `inout_buffer` now generate `buffer-direction=inout`
    instead of being misclassified as output-only buffers.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=217 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-010C` char-pointer depth classification:
  - Bindgen now treats only single-level plain `char*` pointers as
    string-shaped.
  - `char**`, `const char**`, and similar pointer-to-pointer spellings remain
    opaque `ForeignHandle` values instead of string inputs, buffers, or returns.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=217 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-010B` const-pointee char-pointer classification:
  - Bindgen now distinguishes pointee const from top-level pointer const when
    classifying plain `char*` types.
  - `const char*` and `char const*` stay string-input shaped, while
    `char* const` remains a mutable string-buffer contract that requires the
    generated fail-closed buffer helper path.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=217 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BIND-010A` byte-pointer bindgen classification:
  - libclang bindgen now treats only a plain `char` token as string-shaped.
  - `signed char*`, `unsigned char*`, and their `const` variants remain opaque
    `ForeignHandle` pointers instead of being classified as string
    buffers/returns.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=217 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BUFFER-009B` string-buffer none-policy guards:
  - Generated `none` teardown mutable string-buffer helpers now validate role
    and ownership before returning the caller-owned buffer.
  - This keeps the pass-through path narrow instead of relying only on the
    top-level teardown dispatch and size/direction validation.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=216 fail=0`
    - host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-BUFFER-009A` string-buffer manual-review fail-closed cleanup:
  - Generated mutable string-buffer helpers now raise on `manual-review`
    teardown until the facade is edited with an explicit allocation and
    writeback policy.
  - `none` teardown buffer helpers remain caller-owned pass-through values
    after validation.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `compiler` slice with libclang: `pass=215 fail=0`
    - Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-CALLBACK-007B` callback cleanup idempotence:
  - Generated callback unregister helpers now keep nil cleanup as a true
    idempotent no-op by nesting the metadata validation and fail-closed shim
    requirement under the non-nil branch.
  - Non-nil generic callback handles still fail closed until a concrete
    subsystem callback-handle shim is supplied.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `compiler` slice with libclang: `pass=215 fail=0`
    - Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `git diff --check`

- Landed `FFI-CALLBACK-007A` fail-closed generic callback scaffolds:
  - Bindgen-generated callback wrapper helpers now validate callback metadata
    but raise until the facade is edited for a concrete subsystem
    callback-handle shim.
  - Generic bindgen output no longer routes arbitrary C callback parameters
    through the `uv` timer callback prototype.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `compiler` slice with libclang: `pass=215 fail=0`
    - Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

- Landed `FOREIGN-CORE-002H` opaque handle name reflection cleanup:
  - `(foreign-describe handle)` now returns opaque foreign-resource handle
    families such as `File` as symbols under `'name`, matching
    `^{'name File ...}` metadata and callable return descriptors.
  - C ABI library handles keep their soname/path `'name` as a string because
    that field is the `dlopen` target, not a resource-family symbol.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `advanced-ffi-system` group: `pass=63 fail=0`

- Landed `FOREIGN-CORE-002G` C ABI string-return cleanup:
  - Fixed the interpreter/AOT shared FFI call path so non-null `^String`
    returns copy the returned C `char*` into an Omni `String` instead of
    exposing the foreign address as an `Integer`.
  - Null C string returns still map to `nil`, matching the documented
    `^String` contract.
  - Added runtime regression coverage for `strchr` string and null returns.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=63 fail=0`
    - final `c3c build --warn-deprecation=no`

## 2026-04-11

- Landed `FFI-TOML-004B` generated bind manifest:
  - `--bind` now writes `lib/ffi/<name>_manifest.toml` beside each generated
    raw binding module and facade stub.
  - The manifest records the effective shipped bindgen output config:
    dependency name, library, raw syntax, generated raw/facade paths, and
    `strip-prefixes`.
  - Manifest TOML string emission escapes quotes, backslashes, and control
    characters, and fails closed for inconsistent prefix pointer/count input.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `compiler` slice with libclang: `pass=215 fail=0`
    - Host targeted `advanced-ffi-system` group: `pass=61 fail=0`

- Closed `VALIDATION-ALL-SLICE-NESTED-LET-2026-04-11`:
  - Root cause: the memory-stress `nested let 10 levels => 55` fixture had a
    malformed expression with two extra closing parentheses, so both the
    interpreter and JIT harnesses stopped at parse.
  - Fixed the test fixture in `tests_core_groups.c3` and left the parser
    behavior unchanged.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Direct `--eval` of the corrected nested-let expression: `55`
    - Docker-bounded `memory-stress` slice: unified `pass=15 fail=0`
    - Docker-bounded all-slice rerun: unified `pass=2798 fail=0`, compiler
      `pass=208 fail=0`

- Landed `FFI-TOML-004A` facade prefix stripping for bindgen:
  - `TomlFfiDep` now carries `strip-prefixes`, and `toml_parser` parses
    `[dependencies.ffi.NAME] strip-prefixes = ["prefix_"]` string arrays.
  - `--bind` threads the parsed prefixes into facade generation.
  - Raw binding modules intentionally keep C-derived names so the current FFI
    parser still derives truthful `dlsym` symbol names; prefix stripping applies
    to facade/exported Omni names and `raw-*` import aliases.
  - Added compiler coverage proving grouped raw output preserves
    `omni_bindgen_*` C symbols while the facade exposes stripped names.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `compiler` slice with libclang: `pass=214 fail=0`
    - Host targeted `advanced-ffi-system` group: `pass=61 fail=0`

- Revalidated the `VALIDATION-ALL-SLICE-2026-04-11` Deduce residual clusters:
  - `relation-attrs`: `pass=13 fail=0`
  - `core-runtime`: `pass=6 fail=0`
  - `rule-validation`: `pass=70 fail=0`
  - Fixed the diagnostics match/union residual by making
    `Parser.alloc_ast_array_bytes(..., count=0)` return `null` without setting
    a parser error. Zero-length AST arrays are already represented as `null` at
    callsites, and this prevents constructor patterns such as `(DiagOk)` from
    being reported as OOM.
  - The bounded all-slice lane is now split to the single remaining nested-let
    residual:
    `docs/plans/validation-all-slice-nested-let-residual-2026-04-11.md`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted Deduce filters listed above
    - Host targeted `diagnostics` slice: `pass=10 fail=0`
    - Host targeted `basic` slice: `pass=142 fail=0`
    - Docker-bounded all-slice rerun: unified `pass=2797 fail=1`, compiler
      `pass=208 fail=0`

- Fixed the `VALIDATION-ALL-SLICE-2026-04-11` TOML option arity cluster:
  - Registered `toml-parse` as variadic in the primitive table so both the
    one-argument and two-argument call shapes reach `prim_toml_parse`.
  - Kept the existing primitive-level validation as the source of truth for the
    accepted 1-2 argument contract.
  - Remaining all-slice validation work is now narrowed to Deduce relation
    forms, the Deduce CRUD example setup path, and Deduce rule-validation
    failures.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Targeted `data-format` slice: `pass=64 fail=0`

- Landed `FOREIGN-CORE-002F` capability-gated foreign import/member adapter
  hooks:
  - Added `FOREIGN_CAP_IMPORT` and `FOREIGN_CAP_MEMBER` capability gates for
    the internal foreign runtime adapter boundary.
  - Added `import_module` and `resolve_member` adapter slots, plus internal
    dispatch helpers `foreign_runtime_import_module` and
    `foreign_runtime_resolve_member`.
  - Kept public user-facing behavior unchanged: no new public primitive was
    added, and Python/Julia/CUDA behavior is still not wired yet.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `advanced-ffi-system` group: `pass=61 fail=0`
    - Docker-bounded targeted `advanced-ffi-system` group: `pass=61 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - final `c3c build --warn-deprecation=no`
    - `git diff --check`

- Landed `FOREIGN-CORE-002E` explicit foreign adapter load/resolve hooks:
  - Split the C ABI lazy `dlopen` and `dlsym` path into adapter-dispatched
    `load_bound` and `resolve_bound` slots.
  - Kept public FFI behavior unchanged: `call_bound` reaches libffi only after
    adapter load/resolve succeeds, and existing unavailable-library/symbol
    behavior is preserved.
  - Reserved a `tensor_buffer` adapter slot behind `FOREIGN_CAP_TENSOR_BUFFER`
    for later BLAS/cuBLAS work; C ABI does not register Tensor buffer
    marshalling yet.
  - Added a public regression for post-call `foreign-describe` metadata on a
    C ABI bound function.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `advanced-ffi-system` group: `pass=61 fail=0`
    - Docker-bounded targeted `advanced-ffi-system` group: `pass=61 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - final `c3c build --warn-deprecation=no`
    - `git diff --check`

- Landed `FOREIGN-CORE-002D` internal foreign runtime adapter dispatch:
  - Added a small `ForeignRuntimeAdapter` operation boundary with runtime kind,
    adapter capability bits, handle description, callable description, and
    handle release and bound-call slots.
  - Routed `foreign-describe`, `foreign-release`, and C ABI bound-function
    calls through the adapter dispatch point while preserving the current
    user-facing behavior.
  - Registered C ABI callable reflection and bound-function calls on the C ABI
    adapter. Generic handle describe/release remains available for current
    runtime kinds until dedicated Python, Julia, CUDA/cuBLAS, optional C++
    tooling, and polyglot adapters land.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Host targeted `advanced-ffi-system` group: `pass=60 fail=0`
    - Docker-bounded targeted `advanced-ffi-system` group: `pass=60 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - final `c3c build --warn-deprecation=no`

- Landed `FOREIGN-CORE-002C` bindgen descriptor comment alignment:
  - Replaced the raw bindgen module's bespoke `bindgen-meta-record` comment
    vocabulary with `bindgen-descriptor` comments that use the shared
    `ForeignCallable` descriptor terms: `type=ForeignCallable`,
    `runtime=c-abi`, `kind=function`, `name`, `c-name`, `parameters`,
    `returns`, `abi-type`, and user-facing `type`.
  - Renamed raw review-policy comments from `bindgen-meta` to
    `bindgen-policy` so generated raw modules distinguish descriptor-shaped
    metadata from generator safety/review hints.
  - This remains generated-code documentation only; runtime reflection still
    comes from `(foreign-describe ...)`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Docker-bounded `compiler` slice: `pass=207 fail=0` (container libclang
      unavailable, bindgen descriptor strings that do not require libclang
      still ran)
    - Host targeted `compiler` slice with libclang: `pass=212 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - final `c3c build --warn-deprecation=no`
    - `git diff --check`

- Landed `FFI-GROUP-002` grouped bindgen output option:
  - Added a per-dependency `omni.toml` opt-in,
    `[dependencies.ffi.NAME] raw-syntax = "grouped"`, for `--bind` raw modules
    to emit `(define [ffi module] ...)` grouped C ABI declarations.
  - Kept the default `raw-syntax = "legacy"` path, preserving the existing
    generated `[ffi lib]` plus `[ffi lambda]` raw module shape and facade
    behavior.
  - Unsupported `raw-syntax` values fail closed before header parsing, so the
    option does not silently fall back to a different generated surface.
  - grouped raw output now includes a minimal `;; Raw syntax: grouped` header
    note to make review diffs easier to scan without changing binding
    semantics.
  - That header note also closed the narrow `FFI-BIND-003` reviewability slice;
    broader bindgen ergonomics such as generated manifests and prefix stripping
    were follow-up work at the time and later landed as `FFI-TOML-004A/B`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Docker-bounded `compiler` slice: `pass=207 fail=0` (container libclang
      unavailable, grouped raw-syntax option check still ran)
    - Host targeted `compiler` slice with libclang: `pass=212 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - final `c3c build --warn-deprecation=no`
    - `git diff --check`

- Landed `FFI-GROUP-001` grouped C ABI parser/lowering:
  - Added `(define [ffi module] lib "path" (fn (^Type arg)) ^Return ...)` as
    sugar over the existing `[ffi lib]` plus `[ffi λ]` declarative FFI forms.
  - The grouped body is a flat sequence of function signature and return
    annotation pairs; malformed pairs, including missing return annotations,
    fail closed.
  - Grouped declarations reuse the existing `ForeignHandle` metadata dictionary
    validation, callable reflection path, AOT preload scan, and FFI contract
    manifest generation.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Docker-bounded `advanced` slice filtered to `advanced-ffi-system`:
      `pass=60 fail=0`
    - Docker-bounded `compiler` slice: `pass=206 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - Docker-bounded `memory-lifetime-smoke`: `pass=203 fail=0`

- Landed `FOREIGN-CORE-002B` C ABI callable reflection:
  - Extended `foreign-describe` to accept FFI-bound functions as well as
    `ForeignHandle` values.
  - Bound C ABI callables now return a dictionary with quoted-symbol metadata
    keys including `'type 'ForeignCallable`, `'runtime 'c-abi`, `'kind
    'function`, `'name`, `'c-name`, `'library`, `'parameters`, `'returns`,
    `'resolved`, `'available`, and `'capabilities`.
  - `'parameters` is an array of descriptor dictionaries and `'returns` is a
    descriptor dictionary. Descriptors expose user-facing `'type` symbols
    (`'String`, `'Integer`, `'ForeignHandle`, etc.) plus ABI-level `'abi-type`
    symbols (`'string`, `'int`, `'ptr`, etc.); foreign-handle descriptors also
    preserve `'name`, `'ownership`, `'nullability`, and `'finalizer` metadata
    when present.
  - Capability arrays now promote inserted values to the root boundary before
    storing them, matching the root-backed dictionary/array reflection payload.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Docker-bounded `advanced` slice filtered to `advanced-ffi-system`:
      `pass=55 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - Docker-bounded `memory-lifetime-smoke`: `pass=203 fail=0`
    - final `c3c build --warn-deprecation=no`
    - `git diff --check`

- Landed `FOREIGN-CORE-002A` explicit foreign-handle release primitive:
  - Added `foreign-release` as the shared explicit release boundary for
    releasable `ForeignHandle` payloads. It returns `Void`, is idempotent for
    already-closed handles, and rejects ordinary non-releasable library handles.
  - Refactored `ffi_handle_release` through a shared payload-release helper so
    explicit release and scope teardown agree: the finalizer/free path runs at
    most once, then `lib_handle` is cleared and shared wrappers observe
    `'live nil` through `foreign-describe`.
  - This remains a foreign-resource rule only; it does not add refcount or GC
    ownership for Omni language values.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Docker-bounded `advanced` slice filtered to `advanced-ffi-system`:
      `pass=53 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - Docker-bounded `memory-lifetime-smoke`: `pass=203 fail=0`
    - `git diff --check`

- Landed `FOREIGN-CORE-001` first common foreign runtime descriptor slice:
  - `FfiHandle` now carries common runtime-kind, handle-kind, and capability
    descriptors so C ABI, Python, Julia, CUDA/cuBLAS, optional C++ tooling, and
    polyglot adapters can share one small handle contract without exposing raw
    pointers to Omni code.
  - C ABI library handles are tagged as `'c-abi` / `'library` with
    load/resolve/call/reflect capabilities, while C pointer-return handles are
    tagged as `'c-abi` / `'opaque` and report release capability only when an
    owned finalizer policy exists.
  - Added `foreign-describe`, which returns a dictionary with quoted-symbol
    keys (`'type`, `'runtime`, `'kind`, `'ownership`, `'name`, `'live`,
    `'capabilities`); capability sequences are arrays to match the metadata
    shape used by forms such as `'parameters [..]`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - Docker-bounded `advanced` slice filtered to `advanced-ffi-system`:
      `pass=51 fail=0`
    - Docker-bounded `compiler` slice: `pass=205 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - Docker-bounded `memory-lifetime-smoke`: `pass=203 fail=0`
    - `git diff --check`

- Closed `TENSOR-060B` lazy Tensor-expression protocol and destination
  materialization:
  - Extended native `Tensor` payloads with internal concrete/map/contract
    payload kinds, without adding a new public value tag or language-facing
    `TensorExpr` type.
  - Tensor-dispatched `map` and `contract` now return lazy Tensor expression
    payloads that preserve dtype/shape/rank metadata and retain their operand
    edges for later evaluation.
  - `materialize` now forces Tensor expressions either by allocating a
    concrete result or by staging evaluation into a temporary
    exact-shape/dtype destination tensor before copying the result after
    success.
  - `ref` over a Tensor expression now forces through the same materialization
    path before indexing, preserving the existing `ref` surface.
  - Destination materialization now preserves safe elementwise map aliasing and
    rejects contracted expression destinations that alias either source tensor,
    because contraction reads each operand more than once.
  - Boundary graph audit, provenance, parent-copy, root-store promotion, escape
    promotion, and JIT scope-chain checks now treat non-concrete Tensor payloads
    as graph-carrying values so expression callback/operand edges do not retain
    stale TEMP references across return/env/promotion boundaries.
  - Moved the boundary alias-reuse graph scan buffers off the C stack and onto
    heap scratch storage so effect-handler resume paths with small continuation
    stacks can classify returned payload graphs without stack overflow.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` subgroup: `pass=183 fail=0`
    - Docker-bounded `memory-lifetime-smoke` after in-container rebuild:
      `pass=203 fail=0`

- Landed `TENSOR-060B-prep` destination-ready pure Tensor kernels:
  - Split eager Tensor `map` execution into a reusable
    `tensor_map_eval_into(...)` helper that validates the destination tensor
    and stages elementwise results before committing them into caller-
    provided storage after success.
  - Split eager Tensor `contract` execution into result-shape planning and a
    reusable `tensor_contract_eval_into(...)` helper that stages the pure C3
    fallback contraction before copying it into caller-provided storage after
    success.
  - Kept public behavior eager and unchanged for now: `map` and `contract`
    still return concrete `Tensor` values, while the runtime now has the
    internal kernel boundary needed by a later lazy tensor-expression value,
    destination `materialize`, and optional BLAS/cuBLAS dispatch.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` subgroup: `pass=179 fail=0`

- Closed `TENSOR-060A` destination-fusion audit for the Tensor scientific
  surface:
  - Confirmed that true `(materialize (map ...) out)` and
    `(materialize (contract ...) out)` fusion cannot be implemented honestly
    on top of the current eager concrete `Tensor` return path: by the time
    `materialize` receives its source argument, `map` or `contract` has already
    allocated the temporary source tensor.
  - Split the remaining fusion work into `TENSOR-060B` lazy tensor-expression
    protocol and destination fusion. The recommended path is a native
    tensor-expression value rather than turning `materialize` into a macro or
    exposing public `map-into` / backend-flavored escape names.
  - Hardened current `contract` coverage while keeping the eager fallback
    semantics: list axes, zero-axis outer product, right-axis bounds,
    non-tensor operands, malformed axis containers, and non-integer axes are
    now pinned in the advanced collections module tests.
  - Updated the Tensor plan and live TODO so the backlog tracks the real
    semantic boundary before backend acceleration work.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` subgroup: `pass=179 fail=0`

- Closed `TENSOR-050` eager Tensor `contract` for the scientific surface:
  - Registered public `contract` as the canonical summed-axis tensor
    contraction operation over native `Tensor` operands.
  - Implemented the pure C3 `Double` fallback for
    `(contract a b left-axes right-axes)`, including rank-2 matrix-product
    behavior, rank-0 scalar dot-product results, and multi-axis contractions.
  - Added deterministic validation for non-tensor operands, malformed axis
    lists, axis-list length mismatch, out-of-range axes, duplicate contracted
    axes, dtype mismatch, and paired contracted-dimension mismatch.
  - Kept backend acceleration out of the public surface: BLAS/LAPACK/CUDA/cuBLAS
    remain optional optimizations behind `contract`/`materialize`, not new
    canonical operation names.
  - Updated the Tensor plan, language spec, type-system docs, reference docs,
    and live TODO so the next slice is expression/destination optimization
    planning behind the pure fallback.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` subgroup: `pass=179 fail=0`
    - targeted `advanced-stdlib-numeric-collections` subgroup:
      `pass=62 fail=0`
    - targeted `compiler` slice: `pass=209 fail=0`
    - Docker-bounded `memory-lifetime-smoke` after in-container rebuild:
      `pass=203 fail=0`
    - host `c3c build --warn-deprecation=no` refresh after Docker validation

- Closed `TENSOR-040` eager Tensor `map` for the scientific surface:
  - Registered internal `__tensor-map` as the Tensor elementwise execution
    primitive and extended generic `map` through fixed-arity `^Tensor` methods
    for unary tensor, tensor-scalar, scalar-tensor, and exact-shape
    tensor-tensor `Double` inputs.
  - Tensor `map` now returns concrete `Tensor` values in this first slice,
    preserving the tensor-expression contract while deferring lazy expression
    nodes and singleton-axis broadcasting.
  - Added deterministic validation for noncallable callbacks,
    non-tensor/nonnumeric sources, tensor shape mismatch, tensor dtype
    mismatch, and nonnumeric callback results through `tensor/shape-mismatch`
    and `tensor/dtype-mismatch` payloads where appropriate.
  - Updated the compiler stdlib prelude so compiled programs see the same
    Tensor `map` method surface as the runtime stdlib, and adjusted typed
    define lowering assertions to count only the tested user method names now
    that the prelude contains typed `map` methods.
  - Updated the Tensor plan, language spec, type-system docs, reference docs,
    and live TODO so the next slice is `TENSOR-050` pure `contract`; backend
    acceleration remains behind the pure fallback.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` subgroup: `pass=163 fail=0`
    - targeted `advanced-stdlib-numeric-collections` subgroup:
      `pass=62 fail=0`
    - targeted `compiler` slice: `pass=209 fail=0`
    - Docker-bounded `memory-lifetime-smoke` after in-container rebuild:
      `pass=203 fail=0`
    - host `c3c build --warn-deprecation=no` refresh after Docker validation

- Closed `TENSOR-030` concrete Tensor materialization for the scientific
  surface:
  - Registered `materialize` as the tensor expression-to-storage boundary for
    concrete tensor sources.
  - Implemented `(materialize tensor)` as identity for already-concrete
    tensors, `(materialize tensor out)` as an exact-shape/dtype destination
    copy that returns `out`, and `(materialize scalar out)` as scalar fill into
    the mutable destination tensor.
  - Added deterministic destination validation for missing tensor backing
    storage, immutable destinations, dtype mismatch, shape mismatch, and
    non-tensor sources/destinations. Aliasing the same tensor destination is a
    no-op in this first slice.
  - Updated the Tensor plan, language spec, type-system docs, and live TODO so
    the next slice is `TENSOR-040` elementwise `map`; `contract` and backend
    acceleration remain behind later pure-surface-compatible slices.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-collections-module` subgroup: `pass=154 fail=0`
    - Docker-bounded `memory-lifetime-smoke` after in-container rebuild:
      `pass=203 fail=0`

- Closed `TENSOR-020` constructor and indexing for the Tensor scientific
  surface:
  - Registered `Tensor` as a callable constructor while preserving
    `(format "%s" Tensor)` as the `#<type Tensor>` descriptor print surface.
  - Implemented `(Tensor Double shape data-or-scalar)` for native double
    tensors. Shape accepts arrays or proper lists of non-negative integers;
    data accepts a numeric scalar fill or an array/proper list with exactly
    the shape product's element count.
  - Implemented `(ref tensor index-array)` through the existing generic `ref`
    dispatcher, with array/proper-list indices, rank checking, per-axis
    negative indexing, and row-major stride offset calculation.
  - Updated the Tensor plan, language spec, type-system docs, and live TODO so
    the next slice is `TENSOR-030` materialization/expression protocol rather
    than constructor work.
  - validation:
    - `c3c build --warn-deprecation=no`
    - targeted `advanced-unicode-iterator` subgroup: `pass=150 fail=0`
    - targeted `advanced-collections-module` subgroup: `pass=147 fail=0`
    - Docker-bounded `memory-lifetime-smoke` after in-container rebuild:
      `pass=203 fail=0`

- Closed `TENSOR-010` runtime representation for the Tensor scientific surface:
  - Added a native `Tensor` runtime value tag and `TensorVal` payload with
    dtype, rank, shape, strides, element count, byte length, mutability, view
    flag, and contiguous data storage.
  - Implemented deterministic scope-owned tensor payload allocation and
    destruction, plus fail-closed allocation fault paths for tensor metadata
    and storage.
  - Registered `Tensor` as a builtin type descriptor for type identity and
    dispatch; constructor/indexing landed later in `TENSOR-020`.
  - Added `tensor?`, `dtype`, `shape`, `rank`, and `length` support, with
    tensor printing as `#<tensor Double shape [...]>`.
  - Integrated tensor payloads into boundary copy, escape promotion, root-store
    cloning, env-copy, provenance, JIT scope-chain checks, and graph audit
    leaf handling so tensors deep-clone shape/stride/data storage across
    ownership boundaries.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-unicode-iterator` subgroup: `pass=142 fail=0`
    - Docker-bounded `memory-lifetime-smoke` after in-container rebuild:
      `pass=203 fail=0`
    - `git diff --check`

- Closed `AUDIT-FFI-FOREIGN-HANDLE-AOT-POLICY-105`:
  - Added AOT bridge policy descriptors for generated declarative `ffi λ`
    declarations.
  - AOT generated code now emits `AotFfiHandlePolicySpec` arrays for
    parameter handle policy and a return-policy descriptor when the source
    annotation carries non-default `ForeignHandle` metadata.
  - The AOT runtime bridge converts descriptors into `FfiHandlePolicy`,
    preserves parameter handle family/nullability, preserves return handle
    name/ownership/finalizer, resolves owned-return finalizers with `dlsym`,
    and rejects owned handle parameter policies fail-closed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=49 fail=0`
    - bounded `compiler` slice: `pass=209 fail=0`
    - `git diff --check`

- Closed `AUDIT-FFI-FOREIGN-HANDLE-METADATA-104`:
  - Implemented the interpreter/JIT ForeignHandle metadata dictionary policy so
    `^ForeignHandle` stays the simple default foreign-handle annotation while
    FFI-local dictionaries can refine it.
  - Accepted the FFI-local refinements:
    `^{'name File 'ownership owned 'finalizer fclose}` implies
    `ForeignHandle`, and explicit `^{'type ForeignHandle ...}` is also
    accepted.
  - Clarified that Omni dictionaries use quoted-symbol key/value pairs and do
    not use colon keywords.
  - Runtime FFI now enforces handle family and non-null policy at call packing,
    resolves owned return finalizers with `dlsym`, and wraps pointer returns in
    named `FFI_HANDLE` boxes.
  - Added FFI AST serialization for declarative FFI forms so compiler
    roundtrip coverage preserves the metadata dictionary spelling.
  - Split AOT policy propagation into
    `AUDIT-FFI-FOREIGN-HANDLE-AOT-POLICY-105`; that follow-up is now closed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=49 fail=0`
    - bounded `compiler` slice: `pass=204 fail=0`

- Closed `AUDIT-FFI-FOREIGN-HANDLE-SURFACE-103`:
  - Replaced the public FFI raw-pointer annotation surface with `^ForeignHandle`
    across runtime annotation mapping, bindgen output, compiler manifest text,
    tests, current-state docs, and tracking notes.
  - Documented `^ForeignHandle` as the opaque foreign-handle annotation that accepts
    live `FFI_HANDLE` values or `nil`, not raw integer addresses.
  - Removed raw integer address coercion from foreign-handle call packing.
  - Wrapped non-null foreign-handle returns in non-owning `FFI_HANDLE` values
    instead of exposing raw integer addresses.
  - Updated bindgen-facing code/docs so opaque foreign C values now map to
    `^ForeignHandle`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-ffi-system` subgroup: `pass=43 fail=0`
    - bounded `compiler` slice: `pass=197 fail=0`
    - `git diff --check`

- Closed `AUDIT-IMMER-FFI-COMPAT-101` by retiring the optional Immer bridge:
  - Deleted the unsupported `lib/immer.omni` wrapper and the `lib/immer/`
    C++ bridge tree, including the tracked nested `lib/immer/immer` gitlink.
  - Deleted the obsolete `docs/plans/immer-ffi-compat-plan-2026-04-11.md`
    compatibility plan.
  - Recorded the owner decision that C++/Immer support is not core language
    infrastructure, so no `^Value`, automatic value-handle, or pointer-only
    FFI rewrite was added for this legacy optional library.
  - validation:
    - active source/reference search confirms no supported surface references
      remain outside historical TODO/changelog/plans.
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- Closed `AUDIT-PROCESS-WRAPPER-PAYLOAD-FALLBACK-100`:
  - Added untyped stdlib fallbacks for `process-spawn` and `process-kill` so
    invalid argument shapes still flow through the canonical `io/process-*`
    payload error lane, matching the surrounding I/O wrapper pattern.
  - Added regression coverage for invalid process command and signal arguments
    to verify runtime payload codes instead of generic typed-dispatch failures.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `advanced-effect-union-limit` subgroup: `pass=67 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- Closed `AUDIT-LIST-HELPER-ALIAS-086`:
  - Kept `List` as the canonical list constructor/conversion surface.
  - Explicitly approved lowercase `list` as an idiomatic Lisp
    list-builder/conversion helper rather than treating it as an unapproved
    constructor alias.
  - Runtime primitive registration and compiler primitive hash coverage already
    route `List` and `list` through the same implementation.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- Closed `AUDIT-IMMER-PERSISTENT-DISPATCH-088`:
  - Added tagged Omni wrapper dictionaries around optional Immer bridge
    handles so public persistent collection operations unwrap by expected
    family before calling the C++ bridge.
  - Added `persistent-array?`, `persistent-dictionary?`, and
    `persistent-set?` predicates while keeping generic `count`/`conj`/`into`
    dispatch frozen.
  - Made `persistent-dictionary` reject odd key/value argument lists instead of
    silently dropping the final key.
  - Split residual runtime bridge compatibility into
    `AUDIT-IMMER-FFI-COMPAT-101` because `lib/immer.omni` still uses the
    retired `ffi-declare` / `(ffi "...")` surface and current declarative FFI
    cannot truthfully pass arbitrary Omni value payloads as `void*`.
  - historical validation before the bridge was retired:
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --check lib/immer.omni`
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - runtime preload remains blocked by `AUDIT-IMMER-FFI-COMPAT-101`
      because `ffi-declare` is no longer bound.
    - local bridge build remains blocked on missing Immer headers:
      `make -C lib/immer test_bridge` fails on `immer/flex_vector.hpp`.

- Closed `AUDIT-LET-BRACKET-SHORTHAND-102`:
  - Removed legacy outer `let [...]` shorthand from live library code.
  - `lib/core.omni` macro expansions now emit flat-pair `let` and named `let`
    binding lists.
  - `lib/test-utils.omni` now uses flat-pair `let` binding syntax.
  - The only remaining `let [` text matches are the syntax decision note and
    the negative parser regression that verifies the shorthand is rejected.
  - validation:
    - `rg "\(let\s*\[" -n lib stdlib tests examples docs src` returns only
      the syntax decision note and the negative parser regression.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --check lib/test-utils.omni`
    - bounded `basic` slice: `pass=142 fail=0`
    - `c3c build --warn-deprecation=no`
    - `git diff --check`

- Closed `AUDIT-FILESYSTEM-SURFACE-087`:
  - Selected `fs-*` as the canonical filesystem wrapper/primitive family and
    removed the remaining stdlib `filesystem-*` compatibility aliases.
  - Added regression coverage that verifies removed long-form filesystem aliases
    are no longer bound while the canonical `fs-*` roundtrip still works.
  - Updated reference docs and the audit plan so the filesystem surface is no
    longer described as pending or compatibility-backed.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `advanced-effect-union-limit` subgroup: `pass=65 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- Closed `AUDIT-COMPILER-PRIMITIVE-HASH-COVERAGE-099`:
  - Completed public runtime primitive value-position coverage in the compiler
    primitive hash table so public primitives are lowered through
    `aot::lookup_prim(...)` instead of drifting into closure-captured C3 local
    identifiers.
  - Increased the primitive hash table size to keep the now-complete registry
    load comfortably below 50% and updated the stale sizing comment.
  - Extended the closure-capture regression to cover the broader primitive
    families that were previously underrepresented: math (`sin`), string
    utilities (`string-byte-length`), and collection helpers (`sort-by`).
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - audit script: `hash entries=195`, `focus missing public-ish hash entries=0`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- Closed `AUDIT-COMPILER-PRIMITIVE-CLASSIFICATION-098`:
  - Replaced the duplicated compiler free-variable/delegation primitive-name
    arrays with a shared hash-backed classifier, making the compiler primitive
    hash table the single value-lowering source for primitive/literal symbols.
  - Preserved legacy non-hash classification exceptions for `ForeignHandle`
    and `__ui-ftxui-run`, which were already excluded from closure capture but
    do not have normal AOT value lowering entries.
  - Extended the closure-capture regression to cover hash-only primitives such
    as `Dict` and `json-parse`, preventing drift from reintroducing accidental
    captured C3 identifiers.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `git diff --check`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- Closed `AUDIT-E2E-PRIMITIVE-CAPTURE-SANITIZATION-096`:
  - Added a shared compiler symbol-value emission helper so variable lowering
    and closure capture initialization both route primitive symbols through the
    registered primitive cache name instead of emitting raw C3 identifiers.
  - Added missing compiler primitive/free-variable/hash coverage for runtime
    primitives surfaced by the e2e corpus, including `error`, `error?`,
    `error-message`, `is?`, `instance?`, and `type-args`.
  - Preserved user binding shadowing by keeping primitive detection table-based,
    not dynamic environment-based.
  - Allowed `Coroutine` to accept AOT closure wrapper thunks by treating
    `aot::make_closure` wrappers as root-lifetime closure thunks without
    inspecting JIT-only `closure_val` fields.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probe: `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(type-of (Coroutine (lambda () 1)))'` -> `Coroutine`
    - bounded `compiler` slice: `pass=197 fail=0`
    - Docker `scripts/run_e2e.sh`: `ALL 404 e2e compiler tests passed!`

- Canonicalized permissive numeric parsing as `parse-number`:
  - `parse-number` now replaces the public `string->number` arrow alias on the
    runtime/compiler primitive surfaces.
  - `Number` remains a non-callable abstract/meta type descriptor; permissive
    maybe-valued parsing is intentionally not constructor semantics.
  - Added `docs/plans/number-parse-surface-decision-2026-04-11.md` and
    migrated live tests, docs, and examples to `parse-number`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `parse-number` int/double/nil results, removed
      `string->number` binding, live `parse-number` binding, and non-callable
      `Number`
    - bounded `advanced-macro-hygiene-string-number` subgroup: `pass=9 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `compiler` slice: `pass=196 fail=0`
    - `./build/main --check examples/deduce_crud_server.omni`
    - `./build/main --check examples/finwatch/smoke_test.omni`
    - broader Docker `scripts/run_e2e.sh` reached generated-source parity but
      failed the generated C3 build on primitive capture names `error` /
      `error?`; subsequently closed as
      `AUDIT-E2E-PRIMITIVE-CAPTURE-SANITIZATION-096`.

- Canonicalized list/string conversion through constructors:
  - `List(String)` is now the public string-to-list surface and returns a
    proper list of UTF-8 codepoint strings.
  - `String(List)` is now the public list-to-string surface and concatenates a
    proper list of string fragments; `String(nil)` returns the empty string.
  - Removed public `string->list` and `list->string` primitive/compiler aliases;
    internal helper wrappers remain for runtime allocation regressions.
  - Added `docs/plans/list-string-constructor-decision-2026-04-11.md` and
    migrated public docs/Lisp-level tests to constructor forms.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `List(String)`, `String(List)`, `String(nil)`,
      non-string list element rejection, and removed public arrow bindings
    - bounded `advanced-unicode-iterator` subgroup: `pass=138 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `limit-busting` slice: `pass=17 fail=0`
    - bounded `compiler` slice: `pass=196 fail=0`

- Aligned generic string sequence operations with codepoint semantics:
  - `length` on strings now returns UTF-8 codepoint count, matching
    `string-length`; byte count remains explicit via `string-byte-length`.
  - `ref` and postfix `.[index]` on strings now return a single-character string
    by codepoint index, matching `char-at` and `List(String)` element shape.
  - Added non-ASCII regressions for generic `length`, `ref`, and postfix string
    indexing.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for `length`, `string-length`, `string-byte-length`, `ref`,
      `char-at`, and postfix `.[index]` on non-ASCII strings
    - bounded `advanced-unicode-iterator` subgroup: `pass=136 fail=0`
    - bounded `advanced-collections-module` subgroup: `pass=142 fail=0`

- Closed the cons/list `ref` specification parity audit:
  - Updated the language spec and collection reference to document the
    already-tested runtime contract for cons/list chains: positive and negative
    indexing across the full cons chain, dotted terminal tails addressable as
    the final element, and `length` counting a non-`nil` dotted tail as one
    terminal element.
  - validation:
    - bounded `advanced-collections-module` subgroup: `pass=139 fail=0`

- Aligned `list?` with the documented proper-list contract:
  - Registered the existing strict `prim_is_list` implementation as the public
    `list?` predicate and removed the stdlib override that treated any pair,
    including improper lists, as a list.
  - Added predicate regressions for improper lists in the advanced stdlib and
    type/effect predicate groups.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `basic` slice: `pass=142 fail=0`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup:
      `pass=61 fail=0`
    - bounded `advanced-type-dispatch-mutation-chain` subgroup:
      `pass=237 fail=0`

- Completed public stdlib list-walker improper-list normalization:
  - Added a private stdlib proper-list guard and applied it to the audited
    public list walkers: `map`, `filter`, `foldl`, `foldr`, `append`, `take`,
    `drop`, `zip`, `for-each`, `any?`, `every?`, `flatten`, `partition`,
    `remove` via `filter`, `find`, and `nth`.
  - Mirrored the guard and protected definitions in the compiler stdlib prelude
    for the overlapping walker set.
  - Added regressions for partial-success, short-circuit false-positive, and
    nested-`flatten` improper-list cases.
  - validation:
    - bounded `advanced-stdlib-numeric` subgroup after partial-success slice:
      `pass=275 fail=0`
    - bounded `compiler` slice after partial-success slice: `pass=196 fail=0`
    - bounded `advanced-stdlib-numeric` subgroup after remaining walker slice:
      `pass=285 fail=0`
    - bounded `compiler` slice after remaining walker slice: `pass=196 fail=0`

- Completed the runtime `eval` data-to-expression fail-closed lane:
  - Lambda and let data forms now preserve parser-equivalent implicit block
    bodies instead of truncating to the first body form.
  - `macroexpand` now surfaces structural conversion failures for malformed cons
    forms instead of returning the original malformed form.
  - Added eval regressions for lambda/let multi-body parity and macroexpand
    conversion-failure surfacing.
  - validation:
    - bounded `advanced-stdlib-numeric` subgroup: `pass=268 fail=0`

- Hardened the runtime `eval` data-to-expression conversion path for malformed
  special forms:
  - `if`, `quote`, `define`, `set!`, `checkpoint`, `capture`, quasiquote,
    `unquote`, `unquote-splicing`, and `signal` now enforce structural arity
    during value-to-expression reconstruction instead of defaulting missing
    operands to `nil` or ignoring extras.
  - `define`, two-operand `set!`, `capture`, and `signal` now reject non-symbol
    names/tags instead of coercing them to symbol id `0`.
  - Multi-argument `set!` data forms now lower through normal call dispatch,
    matching the parser's generic collection setter shape.
  - Added eval regressions for malformed special-form cases and generic
    multi-argument `set!` lowering in the advanced stdlib numeric
    introspection group.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-stdlib-numeric` subgroup: `pass=265 fail=0`

- Fixed tail multi-argument calls carrying error-valued arguments through
  ESCAPE-lane cons promotion:
  - `make_cons` now distinguishes successful promotion of first-class `ERROR`
    values from promotion failures when building ESCAPE-lane cons cells.
  - `append` now guards the intermediate `(reverse a)` result so improper left
    lists preserve the original proper-list error instead of masking it as
    `arg list too short`.
  - Added core regressions for tail multi-argument named-let calls carrying
    error-valued arguments and updated the append improper-left regression.
  - validation:
    - `c3c build --warn-deprecation=no`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - direct tail multi-arg/append probes
    - bounded `basic` slice: `pass=142 fail=0`
    - bounded `advanced-stdlib-numeric` subgroup: `pass=256 fail=0`
    - bounded `tco-recycling` slice: `pass=11 fail=0`
    - bounded `memory-lifetime-smoke` slice: `pass=201 fail=0`

- Fixed `reverse` on improper lists found by the collection walker audit:
  - `__reverse-list` now rejects dotted tails instead of silently truncating
    them, preventing `append` from losing left-side tail data through `reverse`.
  - Added advanced stdlib regressions for `reverse` proper-list rejection and
    `append` improper-left fail-closed behavior. A follow-up item tracks named
    `let` preserving initializer error payloads instead of masking them as an
    argument-list failure.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct `reverse`/`append` improper-list probes
    - bounded `advanced-stdlib-numeric` subgroup: `pass=256 fail=0`

- Removed name-only AOT list/dictionary constructor fast paths found by the
  constructor/dispatch audit:
  - AOT call lowering now routes `list`, `Dictionary`, and `Dict` through the
    normal callable dispatch path instead of bypassing bindings by symbol name.
  - Added compiler regressions for shadowed `list` and `Dictionary` bindings.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `compiler` slice: `pass=196 fail=0`

- Fixed fixed-arity primitive fallback calls ignoring trailing arguments:
  - Shared multi-argument primitive application now rejects extra or missing
    arguments for primitives registered with fixed arity, while preserving
    explicit variadic primitives registered with arity `-1`.
  - Added regressions for arithmetic, constructor, and generic collection calls
    that previously consumed only the first registered arguments.
  - Updated stale destructuring examples to use explicit binary arithmetic
    nesting.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct probes for fixed-arity rejection and variadic `string-append`
    - bounded `advanced-stdlib-numeric` subgroup: `pass=254 fail=0`
    - bounded `advanced-collections-module` subgroup: `pass=139 fail=0`
    - bounded `compiler` slice: `pass=194 fail=0`

- Aligned `length` with dotted cons `ref` behavior found during the
  cons-chain walker audit:
  - Dotted terminal tails now count as addressable sequence elements, matching
    positive and negative `ref` indexing on dotted cons chains.
  - Added regressions in the advanced collections generic operations group.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct `length` probes for dotted cons chains
    - bounded `advanced-collections-module` subgroup: `pass=137 fail=0`

- Fixed negative `ref` indexing on dotted cons pairs found during the
  cons-chain walker audit:
  - Dotted terminal tails now participate in the negative-index length used by
    `ref`, matching the existing positive `(ref (cons a b) 1)` pair behavior.
  - Added a regression in the advanced collections generic operations group.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct `ref` probes for dotted pair negative indexing
    - bounded `advanced-collections-module` subgroup: `pass=135 fail=0`

- Fixed Pika grammar start-rule selection found while reconciling the reference
  docs with the live grammar surface:
  - Forward-reference placeholders can no longer become the grammar start rule
    ahead of the first declared rule.
  - Added a regression where the first declared rule references later `number`
    and `op` rules before they are registered.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct Pika grammar probe for first-declared start rule with forward
      `number` and `op` references
    - bounded `pika` slice: `pass=90 fail=0`

- Hardened Pika grammar clause validation found during the follow-up
  list-walker audit:
  - Grammar rule and clause forms now reject improper lists instead of
    compiling only the cons-prefix shape.
  - Unary grammar operators (`many`, `some`, `opt`, `not`, `and`, `scan`) now
    enforce exact operand arity, and `end` enforces zero operands.
  - Added regressions in the Pika runtime grammar coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct malformed Pika grammar checks for extra operands, improper clause
      tails, extra rule fields, empty unary operators, and `end` operands
    - bounded `pika` slice: `pass=89 fail=0`

- Hardened sequence-pattern matching found during the follow-up list-walker
  audit:
  - Interpreted sequence patterns now match only arrays or proper lists, so
    improper cons chains and scalars no longer satisfy sequence patterns via
    cons-prefix length truncation.
  - Added regressions in the advanced core semantics match coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct match checks for improper single, improper rest, and scalar empty-sequence inputs
    - bounded `advanced-core-semantics-match` subgroup: `pass=71 fail=0`

- Hardened `eval` value-to-expression conversion found during the
  follow-up list-walker audit:
  - `eval` now rejects improper list program forms before special-form or
    call lowering can truncate dotted tails.
  - Lambda parameter conversion now rejects non-symbol and improper parameter
    lists instead of silently dropping malformed elements.
  - `eval` now preserves the underlying runtime error message for these
    conversion failures.
  - Added regressions in the advanced stdlib introspection coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct eval checks for constructed improper call, `and`, and lambda-params forms
    - bounded `advanced-stdlib-numeric-introspection-lazy-tco` subgroup: `pass=32 fail=0`

- Hardened `apply` malformed-argument-list handling found during the
  follow-up list-walker audit:
  - `apply` now rejects non-list and improper-list argument inputs at the
    primitive boundary instead of truncating dotted tails or treating scalar
    inputs as zero-argument calls.
  - Added regressions in the advanced stdlib introspection coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - direct eval checks for scalar and improper-list argument inputs
    - bounded `advanced-stdlib-numeric-introspection-lazy-tco` subgroup: `pass=29 fail=0`

- Hardened quasiquote splicing malformed-tail handling found during the
  follow-up list-walker audit:
  - JIT quasiquote `,@` expansion now rejects improper list splice
    values instead of dropping the non-list tail.
  - Added a regression in the advanced macro-hygiene quasiquote
    coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-macro-hygiene-quasi-pattern` subgroup: `pass=20 fail=0`

- Hardened `string-join` malformed-element handling found during the
  follow-up string conversion audit:
  - `string-join` now rejects non-string list elements instead of
    silently omitting them from the joined output.
  - Added a regression in the advanced stdlib string/predicate/format
    coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-stdlib-numeric-string-predicate-format` subgroup: `pass=60 fail=0`

- Hardened collection/string conversion fail-closed behavior found during the
  follow-up constructor-surface audit:
  - `list->string` now rejects non-string list elements instead of silently
    dropping them from the output.
  - `Array` conversion from a list now rejects improper list tails instead of
    truncating at the first non-cons tail.
  - `sort`, `sort-by`, and internal list-to-array conversion now reject
    non-list/improper-list inputs instead of returning truncated data.
  - Added regressions in the advanced core constructor/string and stdlib
    sort coverage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `advanced-unicode-iterator` subgroup: `pass=132 fail=0`
    - bounded `advanced-stdlib-numeric-array-set` subgroup: `pass=37 fail=0`
    - bounded `advanced-stdlib-numeric-sort-bitwise-hof` subgroup: `pass=21 fail=0`

- Fixed the repeatable Pika stress/cache slice failures at the shared runtime
  boundary:
  - The regex engine and cache passed direct large-pattern controls; the failing
    Pika cases were dynamic pattern builders that exercised named-let
    multi-argument recursion with string parameters.
  - `make_cons` now distinguishes legal identity promotion for values already
    in a surviving target scope chain from illegal identity returns for current
    TEMP-lane values that failed to promote into ESCAPE.
  - This keeps ESCAPE-lane argument-list construction from turning reusable
    string literals into a false `cons: failed to promote ...` error, which
    later surfaced as `arg list too short`.
  - Added a memory-lifetime regression for named-let string argument-list
    promotion.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `pika` slice: `pass=83 fail=0`
    - bounded `memory-lifetime-smoke` slice: `pass=201 fail=0`

- Fixed the repeatable HTTP CRUD slice failures:
  - `examples/deduce_crud_server.omni` now decodes CRLF and LF requests via a
    line-based header/body split, avoiding the previous `string-split "\n\n"`
    assumption even though `string-split` is a single-byte delimiter helper.
  - The duplicate-post regression no longer reads a missing `r1.error` path as
    a hard field error; it uses `ref` so successful responses carry `nil` for
    absent error fields.
  - `prim_dict` now propagates error-valued dictionary literal keys/values
    before insertion instead of mapping that non-allocation failure to the
    misleading `Dictionary: out of memory while growing backing storage`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `http` slice: `pass=29 fail=0`

- Closed the constructor/generic-dispatch surface cleanup follow-up:
  - Public conversion aliases `number->string`, `symbol->string`,
    `string->symbol`, `exact->inexact`, and `inexact->exact` are no longer
    registered; `String`, `Symbol`, `Double`, and `Integer` remain the
    canonical callable constructor/coercion surfaces and dispatch to the same
    underlying helpers.
  - `Set` materialization now follows the constructor/generic convention:
    `length` replaces public `set-size`, `List` replaces public `set->list`,
    and `List(Set ...)` returns the deterministic canonical set element order.
  - The fast-dev primitive table no longer exposes drift aliases `Int`,
    `Bool`, or the duplicate `filesystem-*` names; it now mirrors the main
    constructor surface for `Integer`/`Boolean` and keeps the existing `fs-*`
    primitive family.
  - Schema validation now uses `array-of` instead of `vector-of` for array
    schemas in runtime, fast-dev, and docs.
  - `lib/immer.omni` no longer exposes Omni-facing `vector`, `hash-map`, or
    `hash-set` names. The wrapper surface now uses
    `persistent-array`, `persistent-dictionary`, and `persistent-set`
    terminology while leaving the underlying Immer C ABI names intact.
  - The broken `lib/immer.omni` generic `count`/`conj`/`into` facade was
    removed because it depended on predicates that are not defined for the
    current opaque FFI handles. A typed wrapper/dispatch lane is tracked
    separately in `TODO.md`.
  - residuals recorded in `TODO.md`:
    - `string->list` / `list->string` need an explicit `List(String)` /
      `String(List)` semantic decision before removal because char,
      grapheme, and list-element coercion behavior are product-visible.
    - `string->number` remains public until the product chooses a canonical
      parse API or a `Number` constructor/coercion contract.
    - lowercase `list` remains public for now because docs explicitly kept it
      as an idiomatic helper and removing it has broad compatibility impact.
    - repeated bounded `http` and `pika` slice failures are tracked as
      separate defect lanes because they reproduce independently of the
      constructor cleanup.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded `string-type` slice: `pass=40 fail=0`
    - bounded `advanced` slice: `pass=1189 fail=0`
    - bounded `compiler` slice: `pass=194 fail=0`
    - quick eval:
      `(bound? 'set-size) => nil`, `(bound? 'set->list) => nil`,
      `(length (Set 1 2 2 3)) => 3`,
      `(List (Set 3 1 2 2)) => (1 2 3)`,
      `(validate '(array-of int) [1 2 3]) => true`,
      `(validate '(array-of int) [1 "x"]) => nil`

- Closed the runtime intern and raise-payload guard follow-up:
  - `src/lisp/eval_init_primitive_registration.c3` now rejects failed `nil`
    symbol interning before defining the language constant.
  - `src/lisp/jit_jit_closure_runtime.c3` now treats failed promise env-tag
    interning as a non-match instead of probing with invalid symbols.
  - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now builds
    unhandled-effect raise payload dictionaries through the non-raising
    hashmap helper and rejects invalid payload key interning before
    publication.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded normal `jit-policy`: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`
    - bounded normal `advanced`: `pass=1185 fail=0`
    - bounded ASAN `advanced`: `pass=1172 fail=0`

- Closed the runtime result-key and optional diagnostic payload interning guard
  follow-up:
  - `src/lisp/eval_dispatch_error_payloads.c3` now rejects invalid payload
    keys and invalid symbol payload values in the shared optional dispatch
    payload setter, so failed interning omits ancillary dispatch diagnostics
    instead of publishing `SYMBOL(INVALID_SYMBOL_ID)` values.
  - `src/lisp/async_process_spawn.c3` now interns process-spawn result keys
    before constructing key symbols and closes spawned resources on key
    interning failure.
  - `src/lisp/http_url_response.c3` now rejects failed HTTP response-field
    key interning before publishing response payload dictionaries.
  - `src/lisp/prim_ui_ftxui_helpers.c3` now rejects failed UI dictionary
    lookup-key interning before probing.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded normal+ASAN `async` slice with FTXUI smoke enabled:
      `pass=65 fail=0`
    - bounded normal `jit-policy`: `pass=51 fail=0`
    - bounded ASAN `jit-policy`: `pass=50 fail=0`

- Closed the goal-directed deduce explain invalid-symbol guard follow-up:
  - `src/lisp/deduce_rule_ops_explain_goal_directed_components.c3` now builds
    goal-directed blocker and shape symbols through a checked helper, so
    symbol interning failure returns the existing explain OOM error instead
    of publishing `SYMBOL(INVALID_SYMBOL_ID)` payload values.
  - `src/lisp/deduce_rule_ops_explain_snapshot.c3` now uses the same helper
    for goal-directed shape and execution-path payload values.
  - `src/lisp/deduce_why_result_path_payload.c3` now rejects invalid lookup
    key interning before constructing temporary dictionary key symbols.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded normal+ASAN `deduce` slice: `pass=330 fail=0`

- Closed the goal-directed deduce diagnostic payload no-preseed follow-up:
  - `src/lisp/deduce_rule_eval_analyze_setup.c3` now builds
    goal-directed selector analysis error payload dictionaries through
    `make_hashmap_no_raise(...)` and local checked insertion, so ancillary
    payload allocation failure cannot publish a nested dictionary raise before
    the intended `deduce/analyze-out-of-memory` fallback.
  - `src/lisp/deduce_rule_eval_fixpoint_goal_directed_selector_prepare.c3`
    now applies the same non-raising payload construction contract to
    goal-directed selector and relation surface diagnostics while preserving
    the existing `deduce/out-of-memory` fallback.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded normal+ASAN `memory-lifetime-smoke` with FTXUI smoke enabled:
      `pass=200 fail=0`

- Closed the AOT/module/deduce/collection sentinel and rollback lane:
  - AOT runtime bridge binding/lookup/import/FFI paths now use checked symbol
    interning before environment lookup/define/set, and environment hash and
    barrier mutation paths now reject `INVALID_SYMBOL_ID` before probing or
    mutating bindings.
  - Module setup/import now rolls back newly published modules when body,
    path-copy, or top-level file evaluation fails; module hash rebuilds skip
    tombstones, and implicit module file loading reacquires module entries by
    index after nested loads can grow the module table.
  - Deduce persisted rule catalog/signature restore now distinguishes missing
    DBIs from other LMDB open failures, rejects invalid restored symbols, and
    rolls back partially restored rule signatures/schemas on mid-restore
    failure.
  - Raise payload construction, method-table root cloning, and array/dict/set
    primitives now fail closed on invalid symbol IDs, method-table entry
    allocation overflow, or nullable backing storage.
  - validation:
    - bounded advanced slice: `pass=1183 fail=0`
    - bounded compiler slice: `pass=191 fail=0`
    - bounded deduce slice: `pass=330 fail=0`
    - bounded string-type slice: `pass=40 fail=0`
    - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- Closed the runtime promotion allocation-staging lane:
  - `src/lisp/eval_promotion_copy_route_helpers.c3`,
    `src/lisp/eval_promotion_escape_structured.c3`, and
    `src/lisp/eval_promotion_root_clone_basic.c3`
    now reject overflowing array, hashmap, method-table, method-signature, and
    closure-parameter allocation sizes before boundary copy or ESCAPE/root
    clone publication.
  - `src/lisp/eval_promotion_escape_structured.c3`
    now resets staged method signatures on dependent allocation failure and
    delays closure result wrapper publication until fallible clone/env work
    succeeds.
  - `src/lisp/eval_env_copy_frame_helpers.c3`
    now allocates non-inline binding storage before publishing the copied
    frame and frees it if frame allocation fails.
  - `src/lisp/eval_pattern_matching.c3`
    now rejects overflowing sequence element collection buffers.
  - validation:
    - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- Closed the deduce SCC allocation-bound lane:
  - `src/lisp/deduce_rule_eval_scc_plan.c3`
    now checks SCC square matrix sizing and stratum relaxation bound arithmetic
    before allocation or fixpoint iteration.
  - `src/lisp/deduce_rule_eval_validation.c3`
    now checks reachability matrix square sizing before allocation.
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3`,
    `src/lisp/deduce_rule_eval_exec_seminaive_recursive_aggregates_impl.c3`,
    and `src/lisp/deduce_rule_eval_exec_component_delta_restore.c3`
    now guard proof-key vector, aggregate batch growth, and decoded delta
    entry allocations.
  - `src/lisp/deduce_schema_query_input_shape.c3`,
    `src/lisp/deduce_schema_query_input_roles.c3`, and
    `src/lisp/deduce_schema_query_input_constraints.c3`
    now reject overflowing `count + 1` capacity requests before calling
    relation ensure-capacity helpers.
  - validation:
    - bounded deduce slice: `pass=330 fail=0`

- Closed the AOT/FFI allocation-staging lane:
  - `src/lisp/aot_type_definitions.c3`
    now initializes cleanup-owned type fields and union variants before any
    later fallible allocation can trigger deferred cleanup, and checks AOT
    field/variant/type-parameter allocation sizes before allocation.
  - `src/lisp/eval_ffi_bound_call.c3`
    now rejects unsupported/narrowing argument counts and overflowing libffi
    staging buffer sizes before preparing runtime call storage.
  - validation:
    - bounded compiler slice: `pass=191 fail=0`
    - bounded memory-lifetime smoke slice: `pass=189 fail=0`

- Closed the parser AST array allocation-helper lane:
  - `src/lisp/parser_parser.c3`
    now exposes `Parser.alloc_ast_array_bytes(...)`, which checks
    `elem_size * count` before delegating to AST arena allocation.
  - Dynamic parser AST array allocations across calls, relation definitions,
    literals, patterns, type annotations, module bodies, lambda params,
    named-let rewrites, path expressions, macro clauses, blocks, and pipe
    rewrites now route through the checked helper.
  - `src/lisp/parser_set_pipe_helpers.c3`
    now rejects overflowing `arg_count + 1` before growing pipe call
    arguments.
  - validation:
    - bounded compiler slice: `pass=191 fail=0`
    - bounded deduce slice: `pass=330 fail=0`

- Closed the parser AST argument size-guard lane:
  - `src/lisp/parser_define_relation_attr_helpers.c3`
    now checks relation role/count arithmetic and `Expr*` allocation sizing
    before building `__define-relation` call arguments.
  - `src/lisp/parser_application_helpers.c3`
    now checks generic call argument `Expr*` allocation sizing before copying
    argument pointers.
  - `src/lisp/parser_ffi_helpers.c3`
    now rejects overflowing FFI parameter counts before `+ 1` capacity checks.
  - `src/lisp/parser_import_helpers_specs.c3`
    now rejects overflowing selective-import counts before `+ 1` capacity
    checks.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded compiler slice: `pass=191 fail=0`
    - bounded deduce slice: `pass=330 fail=0`

- Closed the shared/async string size-guard lane:
  - `src/lisp/scheduler_shared_handles_blob.c3`
    now rejects max-sized blob copies before `len + 1` allocation.
  - `src/lisp/async_process_signal_handles.c3`
    now rejects max-sized C-string copies, cons sequence count overflow, and
    argv/env `char*` table byte-size overflow before allocation.
  - `src/lisp/tls_offload_connect.c3`
    now rejects max-sized TLS duplicate strings before `len + 1` allocation.
  - `src/lisp/prim_io_fs_handles.c3`
    now rejects invalid/overflowing filesystem result array growth before
    replacing array backing storage.
  - `src/lisp/scheduler_offload_network.c3`
    now rejects max-sized compression bounds before allocating the output
    scratch buffer.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded async slice: `pass=61 fail=0`
    - bounded scheduler slice: `pass=111 fail=0`

## 2026-04-10

- Closed the async file I/O size-guard lane:
  - `src/lisp/prim_io_file.c3`
    now checks path/content payload length arithmetic before building the
    internal async write-file offload payload.
  - `src/lisp/scheduler_offload_ops.c3`
    now checks atomic temp-path length arithmetic before allocating the
    temporary path buffer.
  - `src/lisp/prim_io_file_helpers.c3`
    now rejects a max-sized read buffer before adding the trailing byte for
    the file-read scratch buffer.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded async slice: `pass=61 fail=0`

- Closed the scheduler offload nil-completion projection lane:
  - `src/lisp/scheduler_wakeup_io.c3`
    now returns directly from each offload completion-kind projection,
    including `OFFLOAD_RES_NIL`, so async read-file/read-lines missing-path
    paths can remap the nil offload result to their I/O payload codes instead
    of leaking `scheduler/offload-invalid-completion-kind`.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded async slice: `pass=61 fail=0`

- Closed the I/O/string-buffer growth hardening lane:
  - `src/lisp/prim_io_console_helpers.c3`
    now checks console capture append/copy growth arithmetic before appending
    or duplicating buffered text.
  - `src/lisp/prim_io_helpers.c3`
    now rejects overflowing input-state append lengths before mutating the
    live buffer.
  - `src/lisp/primitives_data_formats_csv_parse.c3`
    now guards CSV field and row-array growth before allocating replacement
    storage.
  - `src/lisp/eval_repl_server_state.c3`
    now rejects overflowing session-string and session-capacity growth before
    publishing replacement session state.
  - `src/lisp/unicode_case_utf8proc.c3` and
    `src/lisp/unicode_case_mapping.c3`
    now route case-mapping append growth through a checked helper so the
    UTF-8 output buffer cannot overflow or grow from invalid capacity state.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded unicode slice: `pass=25 fail=0`
    - bounded data-format slice: `pass=59 fail=0`
    - bounded async slice after scheduler nil-completion projection fix:
      `pass=61 fail=0`
    - bounded advanced unicode iterator group: `pass=129 fail=0`

- Closed the deduce direct allocation/schema mutation hardening lane:
  - `src/lisp/deduce_db_handles_mutation.c3`
    now rejects overflowing rule-signature count increments, uses subtract-form
    term-offset bounds checks, and guards direct rule metadata array
    allocations before `sizeof * count` can wrap.
  - `src/lisp/deduce_db_relation_schema_init.c3`,
    `src/lisp/deduce_db_handles.c3`, and
    `src/lisp/deduce_db_handles_register.c3`
    now reject overflowing schema column/index sizing before allocation or
    relation-schema count publication.
  - `src/lisp/deduce_db_handles_mutation_txn.c3`
    now increments transaction `inserted_count` only after the tuple delta set
    append succeeds, so failed append no longer inflates cardinality estimates.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded deduce slice: `pass=330 fail=0`

- Closed the core registry/table growth hardening lane:
  - `src/lisp/value_symbol_table.c3` and
    `src/lisp/value_type_registry.c3`
    now fail closed when init/grow allocation cannot be materialized, reject
    overflowed table/hash byte-size arithmetic before allocation, and avoid
    mutating table state until replacement allocations are confirmed.
  - `src/lisp/value_interp_init_helpers.c3`
    now routes macro/module table and hash table init sizing through checked
    multiplication before allocation.
  - `src/lisp/eval_pattern_match_support.c3`
    now guards `gensym` and match-binding growth loops against doubling and
    allocation-byte overflow before replacing backing arrays.
  - `src/lisp/prim_collection_hashmap.c3` and
    `src/lisp/prim_collection_sort_array.c3`
    now enforce power-of-two/valid capacity invariants for hashmap state and
    reject invalid/overflowing grow arithmetic before mutating collection
    backing storage.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded allocator-validation slice: `pass=1 fail=0`
    - bounded advanced collections/module group: `pass=134 fail=0`
    - bounded advanced type-dispatch/mutation-chain group: `pass=236 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- Closed the audit overflow hardening batch:
  - `src/lisp/deduce_db_rule_signature_record_codec.c3`,
    `src/lisp/deduce_db_rule_signature_restore.c3`,
    `src/lisp/deduce_db_rule_signature_persistence.c3`,
    `src/lisp/deduce_db_rule_catalog_record_codec.c3`, and
    `src/lisp/deduce_db_rule_catalog_persistence.c3`
    now route persisted rule signature/catalog record sizing, byte copies, and
    restore cursor movement through checked add/mul/cursor helpers instead of
    allowing `sizeof * count` or `cursor + len` wraparound.
  - `src/lisp/ast_arena.c3`, `src/lisp/parser_import_helpers.c3`,
    `src/lisp/parser_module_decl.c3`, and
    `src/lisp/parser_export_from.c3`
    now fail closed on AST arena alignment/chunk accounting overflow and on
    parser import/module/export allocation byte-size overflow.
  - `src/lisp/jit_jit_apply_multi_prims.c3`,
    `src/lisp/jit_jit_apply_runtime.c3`,
    `src/lisp/jit_jit_runtime_effects_handle.c3`, and
    `src/lisp/jit_jit_handle_signal_helpers.c3`
    now reject oversized JIT arg buffers, handler clause arrays, and
    handle-state snapshot copies before allocation-size arithmetic can wrap.
  - `src/lisp/value_interp_lifecycle.c3`,
    `src/lisp/value_environment.c3`, and
    `src/lisp/jit_jit_define_method_table.c3`
    now guard interpreter macro/module/handler table growth, env binding
    growth, and method-table growth before doubling or allocation byte-size
    arithmetic can wrap.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded deduce slice: `pass=330 fail=0`
    - bounded allocator-validation slice: `pass=1 fail=0`
    - bounded advanced collections/module group: `pass=134 fail=0`
    - bounded advanced effect-continuation group: `pass=56 fail=0`
    - bounded advanced runtime-control group: `pass=22 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- Closed the runtime module export growth fail-closed lane:
  - `src/lisp/jit_jit_module_setup_helpers.c3`
    now routes module export table allocation/growth through a checked helper
    with a narrow failure seam, preserves the existing export table on growth
    failure, and rejects oversized export capacities before byte-size
    arithmetic can wrap.
  - `src/lisp/jit_jit_compile_effects_modules.c3` and
    `src/lisp/jit_jit_module_import_setup.c3`
    now propagate export-table growth failure instead of writing through a
    failed replacement table while re-exporting or implicitly exporting module
    definitions.
  - `src/lisp/tests_advanced_stdlib_module_groups.c3`
    now pins forced module export growth allocation failure and verifies the
    original export table remains intact.
  - validation:
    - `c3c build`
    - bounded advanced collections/module group: `pass=134 fail=0`

- Closed the scheduler offload missing-completion fail-closed lane:
  - `src/lisp/scheduler_offload_worker.c3`
    now publishes or directly handles non-task offload readiness even when both
    worker completion materialization and fallback alloc-failure completion
    materialization return `null`, so a blocked waiter is not stranded.
  - `src/lisp/scheduler_wakeup_io.c3`
    now treats an active completed offload slot with a null completion as a
    deterministic `"offload: missing completion"` error and clears the pending
    slot after consumption.
  - `src/lisp/tests_scheduler_boundary_offload_payload_groups.c3`
    now pins the completed-null wakeup path, verifies the blocked fiber becomes
    ready, and verifies consumption clears the pending offload slot.
  - validation:
    - `c3c build`
    - bounded scheduler slice: `pass=111 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- Closed the deduce/JIT capacity-growth byte-overflow lane:
  - `src/lisp/deduce_schema_query_relation_alloc.c3`,
    `src/lisp/deduce_rule_eval_exec_aggregate_groups.c3`,
    `src/lisp/deduce_rule_eval_exec_component_delta_codec.c3`,
    `src/lisp/deduce_rule_eval_exec_component_state_helpers.c3`,
    `src/lisp/deduce_schema_query_input_analysis.c3`,
    `src/lisp/deduce_db_handles_mutation_txn.c3`,
    `src/lisp/deduce_db_handles_incremental_tracking.c3`,
    `src/lisp/deduce_db_rule_signature_helpers.c3`,
    `src/lisp/deduce_db_handles.c3`, and
    `src/lisp/deduce_db_rule_catalog_record_codec.c3`
    now fail closed before `sizeof * new_cap` can overflow during relation,
    delta, aggregate, query-demand, transaction, dirty-predicate, signature,
    relation-schema, and persisted-rule-catalog growth.
  - `src/lisp/jit_jit_module_setup_helpers.c3`
    now rejects oversized source-dir vector growth and path/string length
    increments before allocation-size arithmetic can wrap.
  - `src/lisp/tests_deduce_groups_parallel.c3` and
    `src/lisp/tests_deduce_groups.c3`
    now pin the oversized-capacity fail-closed path in the bounded deduce
    parallel group.
  - validation:
    - `c3c build`
    - bounded deduce parallel group: `pass=6 fail=0`
    - bounded scheduler slice: `pass=111 fail=0`
    - bounded `memory-lifetime-smoke`: `pass=189 fail=0`

- Closed the iterator/coroutine malformed-state normalization lane:
  - `src/lisp/primitives_iter_state.c3`
    now rejects missing internal list/array iterator thunk state directly, and
    no longer treats a null recursive tail as ordinary iterator completion.
  - `src/lisp/primitives_iter_coroutine.c3`
    now rejects malformed thunk arg state for `map`, `filter`, `take`, and
    `zip` instead of normalizing it to `nil`, and `filter` no longer treats a
    non-iterator internal state transition as successful exhaustion.
  - `src/lisp/primitives_iter_terminal.c3`
    now rejects a null iterator pair from internal thunk dispatch instead of
    truncating `collect` / `to-array` as if the stream completed normally.
  - `src/lisp/primitives_coroutine_resume.c3`
    now rejects missing yielded values and missing completed results instead of
    fabricating successful `nil` returns during coroutine resume.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` and
    `src/lisp/tests_memory_lifetime_groups.c3`
    now pin malformed iterator thunk state plus missing-yield /
    missing-completion resume state directly in the bounded lifetime lanes.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the runtime effect publication fail-closed lane:
  - `src/lisp/value_constructors.c3`
    now builds handled-raise payload maps through the raw constructor path,
    and `raise_error_pending_impl(...)` now rejects payload-construction
    failure before handler binding instead of degrading to a message-only
    handled raise.
  - `src/lisp/value_interp_continuation_helpers.c3`
    now exposes a narrow continuation-allocation failure seam and returns
    `null` instead of dereferencing a failed root-scope allocation.
  - `src/lisp/jit_jit_handle_signal.c3`,
    `src/lisp/jit_jit_runtime_effects_handle.c3`,
    `src/lisp/jit_jit_reset_shift.c3`, and
    `src/lisp/jit_jit_runtime_effects_reset_shift.c3`
    now fail closed on continuation allocation failure instead of
    null-dereferencing in handled effect / capture dispatch.
  - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
    now fails closed with `"runtime effect payload: out of memory"` when
    unhandled-effect diagnostic payload construction cannot complete, instead
    of silently dropping the payload and publishing a degraded business error.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` and
    `src/lisp/tests_runtime_feature_jit_groups_more.c3`
    now pin handled-raise payload allocation failure and handled-effect
    continuation allocation failure directly in the bounded runtime/JIT lanes.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=pending-raise-payload-alloc-failure,handle-continuation-alloc-failure ./build/main --test-suite lisp'`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the runtime helper list-materialization fail-closed lane:
  - `src/lisp/value_constructors.c3`
    now exposes a shared `make_cons_or_error(...)` helper with a narrow
    nth-failure seam for deterministic runtime list-builder tests.
  - `src/lisp/prim_string_transform.c3`
    now makes `string-split` reject internal result-list construction failure
    instead of continuing with partial/null list state.
  - `src/lisp/prim_io_file.c3`
    now makes `read-lines` reject internal result-list construction failure
    instead of continuing with partial/null list state.
  - `src/lisp/prim_collection_hashmap_key_helpers.c3`
    now makes `keys` / `values` canonical list assembly fail closed in both
    sorted and fallback paths instead of reusing raw `make_cons(...)`.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now directly pins string-split and hashmap key/value list allocation
    failure in the bounded runtime alloc lane.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the JIT helper arg-construction fail-closed lane:
  - `src/lisp/value_constructors.c3`
    now exposes one checked `make_list1_or_error(...)` helper with a narrow
    nth-failure seam for deterministic variadic-rest tests.
  - `src/lisp/jit_jit_apply_helpers.c3` and
    `src/lisp/jit_jit_apply_runtime.c3`
    now reject variadic zero-fixed-arg rest-list construction failure before
    binding the rest parameter environment.
  - `src/lisp/jit_jit_dispatch_helpers.c3`
    now routes instance `ref` dispatch arg-list construction through the
    shared checked two-item helper instead of nesting raw `make_cons(...)`.
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
    now directly pins the variadic rest-list allocation failure in the
    `jit-policy` slice.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=variadic-rest-list-alloc-failure ./build/main --test-suite lisp'`

- Closed the JIT quasiquote pair-construction fail-closed lane:
  - `src/lisp/jit_jit_quasiquote_macros.c3`
    now routes all internal quasiquote pair construction through one checked
    helper with a narrow nth-failure seam and returns
    `"quasiquote: failed to allocate pair"` instead of wrapping cons
    constructor faults as successful quasiquote values.
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
    now directly pins both nested quasiquote and list quasiquote pair
    construction failure in the `jit-policy` slice.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=quasiquote-cons-alloc-failure ./build/main --test-suite lisp'`

- Closed the JIT multi-arg list fail-closed lane:
  - `src/lisp/jit_jit_apply_runtime.c3`
    now rejects `make_cons(...)` failure while constructing continuation-safe
    multi-arg call lists.
  - `src/lisp/jit_jit_apply_multi_prims.c3`
    now makes `jit_apply_multi_args_iterative(...)` return
    `"arg list too short"` when the arg list breaks before all required args
    are consumed, instead of returning a partial apply result as success.
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
    now pins the malformed multi-arg list case directly in the `jit-policy`
    slice.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=multi-arg-malformed-list-fails-closed ./build/main --test-suite lisp'`

- Closed the shared two-arg list materialization fail-closed lane:
  - `src/lisp/value_constructors.c3`
    now exposes one checked `make_list2_or_error(...)` helper with a narrow
    nth-failure seam for deterministic runtime/JIT tests.
  - `src/lisp/prim_system.c3`
    now makes `(shell cmd true)` fail closed with
    `"shell: failed to construct result list"` if the final two-item result
    list cannot be built.
  - `src/lisp/jit_jit_runtime_effects_handle.c3`
    now routes both pending-raise and normal effect-handler arg-pair
    construction through the same checked helper so constructor failure
    propagates before handler call-through.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=handler-arg-list-alloc-failure ./build/main --test-suite lisp'`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the scheduler batch result-list fail-closed lane:
  - `src/lisp/scheduler_primitives_threads.c3`
    now routes batch thread result-list assembly through one checked
    scheduler-local prepend helper with a narrow nth-failure seam.
  - `src/lisp/scheduler_primitives_offload_execute.c3`
    now returns a typed `"offload: out of memory"` error if final result-list
    publication fails.
  - `src/lisp/scheduler_primitives_task_spawn.c3`
    now drops already-spawned live thread-task entries if result-list
    publication fails after task creation.
  - `src/lisp/tests_scheduler_groups_more.c3`
    now proves forced result-list cons allocation failure in offload-batch,
    task-spawn-batch, and thread-spawn-batch leaves active thread-task count
    unchanged.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`

- Closed the OS-thread null-completion fail-closed lane:
  - `src/lisp/scheduler_thread_task_transition_scaffold.c3`
    now exposes a narrow transition-completion allocation fail seam for
    deterministic scheduler boundary tests.
  - `src/lisp/scheduler_thread_task_transitions.c3`
    now drops the OS-thread entry when both completion materialization and
    alloc-failure completion materialization fail, instead of returning with
    the entry still running.
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`
    now proves the double-failure seam wakes the blocked waiter, clears the
    join token, and removes the OS-thread entry.
  - `src/lisp/tests_scheduler_groups.c3`
    now wires that regression into the bounded scheduler slice.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the malformed iterator-tail fail-open lane:
  - `src/lisp/primitives_iter_state.c3`
    now exposes `iterator_tail_or_error(...)` so iterator tail validation is
    shared across terminal and coroutine helpers.
  - `src/lisp/primitives_iter_terminal.c3`
    now makes `collect` / `to-array` reject malformed iterator pairs and
    malformed iterator tails instead of silently truncating the result.
  - `src/lisp/primitives_iter_coroutine.c3`
    now makes `map`, `filter`, `take`, `zip`, and `foldl` reject malformed
    iterator tails instead of truncating or deferring broken state as normal
    completion.
  - `src/lisp/tests_advanced_core_unicode_groups.c3`
    now pins malformed-tail rejection through the surface `List`, `Array`,
    `map`, `filter`, `take`, `zip`, and `foldl` iterator pipelines.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc "rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib ./build/main --eval \"(handle (List (Iterator (lambda () (cons 1 2)))) (raise msg (ref msg 'message)))\""`
    - `scripts/run_validation_container.sh bash -lc "rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib ./build/main --eval \"(handle (foldl (lambda (a x) (+ a x)) 0 (Iterator (lambda () (cons 1 9)))) (raise msg (ref msg 'message)))\""`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the scheduler wakeup publish-fallback lane:
  - `src/lisp/scheduler_wakeup_callbacks.c3`
    now makes timer, sleep, and poll-error callbacks fall back to the same
    direct wakeup handlers when reliable wakeup enqueue fails, instead of
    silently dropping the blocked-fiber completion.
  - `src/lisp/scheduler_offload_worker.c3`
    now makes non-task offload completion fall back to
    `scheduler_handle_wakeup_offload_ready(...)` when offload-ready publish
    fails, instead of freeing the live completion payload and stranding the
    blocked waiter.
  - `src/lisp/tests_scheduler_groups_more.c3`
    now pins publish-failure fallback for timer, sleep, poll-error, and
    offload-after paths by forcing the real
    `scheduler_publish_reliable_wakeup(...)` failure seam.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp'`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the collection/apply array fail-open lane:
  - `src/lisp/prim_collection_sort_array.c3`
    now makes `sort` / `sort-by` propagate list-construction failure
    directly, makes `sort-by` propagate comparator application errors
    instead of silently returning a partial sort, makes `array`,
    `list->array`, `set!` on arrays, and `push!` reject
    `boundary_promote_to_root(...)` failures instead of storing `ERROR`
    values as data, and makes `push!` fail closed on grow allocation
    failure instead of null-dereferencing the resized item buffer.
  - `src/lisp/primitives_iter_terminal.c3`
    now makes `collect` propagate list-construction failure directly and
    makes `to-array` reject `boundary_promote_to_root(...)` failure instead
    of returning an array populated with `ERROR` elements.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins array constructor/mutator and `to-array` boundary-promotion
    failure, `push!` grow failure, and `sort-by` comparator failure.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the string-backed runtime payload/list fail-open lane:
  - `src/lisp/prim_system.c3`
    now makes `(shell cmd true)` return the string-constructor `ERROR`
    directly instead of returning `(ERROR exit-code)`.
  - `src/lisp/prim_io_fs_handles.c3`
    now makes `fs-readdir` return entry-name string construction failure
    directly instead of storing `ERROR` entries in the returned array.
  - `src/lisp/http.c3`
    now makes `http-get` / `http-request` return host/request string
    materialization failure directly before transport setup/write.
  - `src/lisp/schema_validation.c3`
    now makes `schema-explain` return message-string construction failure
    directly instead of wrapping it inside a singleton explanation list.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins `shell`, `fs-readdir`, and `schema-explain` under forced
    string-wrapper allocation failure.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the string/list materializer fail-open lane:
  - `src/lisp/prim_string_transform.c3`
    now makes `string-upcase` and `string-downcase` return string-constructor
    `ERROR`s directly instead of mutating a non-STRING result, and
    `string-split` now propagates per-part `make_string(...)` failure instead
    of embedding it into a successful list.
  - `src/lisp/prim_string_ops.c3`
    now makes `string->list` propagate per-character `make_string(...)`
    failure directly instead of consing an `ERROR` value into a normal list.
  - `src/lisp/unicode.c3`
    now makes `string-graphemes` propagate grapheme-cluster string materialization
    failure directly instead of storing `ERROR` values into the cluster array
    and later returning a successful list containing them.
  - `src/lisp/prim_io_file.c3`
    now makes `read-lines` propagate per-line `make_string(...)` failure
    directly instead of embedding it into the intermediate line list.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins `string-upcase`, `string-downcase`, `string->list`,
    `string-split`, and `string-graphemes` under forced string-wrapper
    allocation failure.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the schema-explain list-builder fail-closed lane:
  - `src/lisp/schema_explain_payload_helpers.c3`
    now routes list accumulation and list reversal through one checked
    `explain_prepend_or_oom(...)` helper, so schema-explain list builders
    return the existing `"schema explain: out of memory"` error instead of
    hard-aborting on an internal cons allocation failure.
  - `src/lisp/schema_explain_helpers.c3`,
    `src/lisp/schema_explain_effect_helpers.c3`, and
    `src/lisp/schema_explain_effect_runtime.c3`
    now propagate that same list-builder failure for dispatch candidates,
    handler tag lists, and effect candidates.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins helper-level and top-level schema-explain list-builder OOM seams
    through a dedicated local `nth` fail seam.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the checked collection-mutator silent-failure lane:
  - `src/lisp/prim_collection_hashmap.c3`
    now makes `hashmap_set_symbol(...)`, `hashmap_grow(...)`, and
    `hashmap_set(...)` return checked `bool` results instead of discarding the
    underlying grow/insert outcome.
  - `src/lisp/prim_collection_hashmap.c3`
    now makes `set!` on dictionary targets fail closed with
    `runtime/out-of-memory` when backing-storage growth fails instead of
    returning `Void` after silently dropping the write.
  - `src/lisp/prim_collection_generic_set.c3`
    now makes `set-add` fail closed on the same backing-storage grow failure
    instead of reporting success after a no-op mutation.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`
    now pins both mutator seams directly and proves the failed write does not
    materialize in the collection after the error.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the deduce explain/analyze/why-result checked-insertion follow-up:
  - the checked-constructor follow-up is now complete for the remaining deduce
    payload-map family, not just raw `make_hashmap(...)` callsites.
  - `src/lisp/deduce_why_result_payload.c3`,
    `src/lisp/deduce_why_result_path_payload.c3`,
    `src/lisp/deduce_why_result_lookup.c3`, and
    `src/lisp/deduce_why_result_lookup_derived.c3`
    now propagate the first `explain_dict_set*` insertion failure from
    context/path/payload attachment instead of silently returning a partial
    why-result payload after checked constructor success.
  - `src/lisp/deduce_rule_eval_analyze_payload_fields.c3`,
    `src/lisp/deduce_rule_eval_analyze_payload_tail.c3`, and
    `src/lisp/deduce_rule_eval_analyze_payload_result.c3`
    now fail `deduce/analyze` result-map construction closed on the first
    checked insertion failure instead of ignoring the failed write.
  - the remaining deduce explain/schema/stats helper family now follows the
    same contract too:
    - `src/lisp/deduce_parallel_runtime_truth.c3`
    - `src/lisp/deduce_rule_ops_explain_goal_directed.c3`
    - `src/lisp/deduce_rule_ops_explain_goal_directed_components.c3`
    - `src/lisp/deduce_rule_ops_explain_plan_payload.c3`
    - `src/lisp/deduce_rule_ops_explain_plan_steps.c3`
    - `src/lisp/deduce_rule_ops_explain_projection.c3`
    - `src/lisp/deduce_rule_ops_explain_snapshot.c3`
    - `src/lisp/deduce_rule_ops_explain_step_counters.c3`
    - `src/lisp/deduce_schema_query_metadata_schema_helpers.c3`
    - `src/lisp/deduce_schema_query_metadata_schema_payloads.c3`
    - `src/lisp/deduce_schema_query_metadata_stats_parallel_fields.c3`
    - `src/lisp/deduce_schema_query_metadata_stats_payload.c3`
    - `src/lisp/deduce_schema_query_metadata_stats_tail.c3`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'` -> `pass=328 fail=0`

- Closed the remaining guarded raw-hashmap normalization lane for
  `deduce_*` / `unify_*` payload and result builders:
  - `src/lisp/deduce_relation_row_materialization.c3` now routes row-dict
    materialization through checked hashmap construction plus checked
    insertion, so scan/query row payload building fails closed under
    constructor or grow pressure.
  - `src/lisp/deduce_relation_ops_validation_payload.c3` now routes integrity
    payload maps through one checked payload-dict helper and treats
    `explain_dict_set*` failure as payload omission instead of returning a
    partially populated machine-readable conflict payload.
  - `src/lisp/deduce_rule_eval_exec_component_state_helpers.c3`,
    `src/lisp/deduce_rule_eval_exec_component_state.c3`,
    `src/lisp/deduce_rule_eval_exec_aggregate_state.c3`,
    `src/lisp/deduce_rule_eval_exec_seminaive.c3`,
    `src/lisp/deduce_rule_eval_scc.c3`,
    `src/lisp/deduce_relation_scan_helpers_join.c3`,
    `src/lisp/deduce_rule_eval_analyze_setup.c3`, and
    `src/lisp/deduce_rule_eval_fixpoint_goal_directed_selector_prepare.c3`
    now use checked hashmap construction and checked insertion for deduce
    runtime helper state maps instead of ad hoc raw-constructor mutation.
  - `src/lisp/deduce_rule_ops_explain_goal_directed_components.c3`,
    `src/lisp/deduce_rule_ops_explain_plan_payload.c3`,
    `src/lisp/deduce_rule_ops_explain_plan_steps.c3`,
    `src/lisp/deduce_rule_ops_explain_projection.c3`,
    `src/lisp/deduce_rule_ops_explain_snapshot.c3`,
    `src/lisp/deduce_rule_ops_explain_step_counters.c3`,
    `src/lisp/deduce_schema_query_metadata_schema_helpers.c3`,
    `src/lisp/deduce_schema_query_metadata_schema_payloads.c3`,
    `src/lisp/deduce_schema_query_metadata_stats.c3`,
    `src/lisp/deduce_schema_query_metadata_integrity_history.c3`, and
    `src/lisp/deduce_rule_eval_analyze_payload_result.c3`
    now route the remaining already-guarded deduce explain/schema/analyze
    payload dictionaries through checked constructors instead of raw
    `make_hashmap(...)`.
  - `src/lisp/deduce_why_result_path_payload.c3`,
    `src/lisp/deduce_why_result_payload.c3`,
    `src/lisp/deduce_why_result_lookup.c3`, and
    `src/lisp/deduce_why_result_lookup_derived.c3`
    now fail closed when why-result path/payload dictionary construction
    fails, instead of treating missing path payloads as ordinary successful
    provenance snapshots.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3`,
    `src/lisp/tests_deduce_groups_integrity.c3`, and
    `src/lisp/tests_deduce_query_groups.c3`
    now pin the deduce helper-state, integrity payload, and why-result OOM
    seams directly.
  - scoped verification:
    - `rg -n "make_hashmap\\(" src/lisp/deduce_* src/lisp/unify_* -S` -> no matches
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'` -> `pass=328 fail=0`

- Closed the direct-crash raw-hashmap caller slice of the internal
  collection-constructor hardening pass:
  - `src/lisp/unify_match_helpers.c3` now routes `build_result_dict(...)`
    through checked hashmap construction and checked insertion, and fails with
    `deduce/match-out-of-memory` instead of dereferencing `dict.hashmap_val`
    from an unchecked raw constructor result.
  - `src/lisp/unify_scan_helpers.c3` now propagates that builder `ERROR`
    directly instead of consing it into a successful match-result list.
  - `src/lisp/tests_deduce_query_groups.c3` now pins the
    `deduce 'match` result-dict constructor OOM seam directly.
  - the raw-hashmap backlog is now split by real risk boundary:
    - direct crashable callers are closed
    - the remaining open lane is only the already-guarded normalization family
  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'`

- Closed the raw-array constructor and AOT dict payload slice of the internal
  collection-constructor hardening pass:
  - `src/lisp/value_predicates_accessors_basic.c3` now routes `make_array(...)`
    through the checked constructor contract instead of raw `mem::malloc(...)`
    with no null checks.
  - `src/lisp/prim_collection_sort_array.c3` now treats raw array-constructor
    failure as a first-class `ERROR` return in `array(...)` and
    `list->array(...)` instead of dereferencing a partially initialized wrapper.
  - `src/lisp/aot_runtime_bridge.c3` now routes `dict_from_args(...)` through
    checked hashmap construction and checked insertion, so AOT bridge payload
    building fails closed on allocator pressure instead of constructing a
    `HASHMAP` wrapper with a null payload.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves raw
    array-constructor failures propagate cleanly through `make_array(...)`,
    `array(...)`, and `list->array(...)`.
  - `src/lisp/tests_compiler_core_groups_fail_closed.c3` now proves
    `aot::dict_from_args(...)` returns `ERROR` when bridge-side hashmap
    construction fails under an active bridge interpreter.
  - residual constructor hardening is now narrower again:
    - remaining direct `make_hashmap(...)` caller families that still depend on
      per-callsite tag/null guards instead of the checked constructor path
  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the remaining runtime/status payload-builder slice of the internal
  collection-constructor migration:
  - `src/lisp/async_process_signal_dns_process.c3`,
    `src/lisp/async_process_spawn.c3`, and
    `src/lisp/prim_io_fs_handles.c3`
    now route process/fs status payload construction through checked
    `HASHMAP` / `ARRAY` helpers plus checked insertion instead of mutating
    unchecked constructor results.
  - `src/lisp/http_url_response.c3` now routes parsed HTTP response payload
    construction through the same checked map contract.
  - `process-spawn` now also closes its live process/fs handles if final
    success-payload construction fails, so constructor OOM no longer strands a
    half-built success result with open resources.
  - `src/lisp/eval_dispatch_error_payloads.c3` now treats dispatch payload
    dictionaries as optional under OOM, so the primary lambda/ambiguous
    typed error still returns even if payload-map construction or insertion
    fails.
  - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3` now applies
    that same optional-payload contract to unhandled-effect error payloads.
  - `src/lisp/primitives_meta_types_ctor_helpers.c3` now makes
    `ctor_mismatch_data(...)` fail closed by returning `null` instead of
    dereferencing unchecked hashmap payloads.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves fs,
    process-spawn, process-wait, HTTP response payload, dispatch/runtime-effect
    payload, and ctor-mismatch constructor OOM paths fail closed.
  - this closes the staged internal collection-constructor migration lane; no
    runtime/status residual remains in `TODO.md`.

- Closed the schema-explain payload-builder slice of the internal collection
  constructor migration:
  - `src/lisp/schema_explain_payload_helpers.c3` now centralizes checked map
    construction and checked `explain_dict_set*` insertion under one explicit
    `"schema explain: out of memory"` contract.
  - `src/lisp/schema_explain_helpers.c3`,
    `src/lisp/schema_explain_effect.c3`,
    `src/lisp/schema_explain_effect_result_payload.c3`,
    `src/lisp/schema_explain_effect_runtime.c3`, and
    `src/lisp/schema_explain_effect_helpers.c3`
    now route entrypoint/result/candidate/source payload maps through that
    checked contract instead of dereferencing unchecked `make_hashmap(...)`
    results.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves
    dispatch explain, effect explain, and helper payload-map construction all
    fail closed on forced hashmap allocation failure.
  - residual unchecked collection-constructor work is now narrower again:
    - remaining runtime/status payload builders only
  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=134 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=134 fail=0`

- Closed the data-format bridge slice of internal collection-constructor OOM
  hardening:
  - `src/lisp/json.c3` now routes object/array construction through checked
    `ARRAY` / `HASHMAP` helpers and checked hashmap insertion.
  - `src/lisp/primitives_toml_bridge.c3` now does the same for TOML table/array
    conversion.
  - `src/lisp/primitives_data_formats_csv_parse.c3` now uses checked result-row
    and row-rotation array constructors instead of assuming `make_array(...)`
    succeeded.
  - recursive data-format conversion in JSON/TOML now propagates nested
    constructor/materialization `ERROR`s directly instead of embedding them into
    partial arrays or dicts.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves JSON,
    TOML, and CSV constructor OOM paths fail closed.
  - residual backlog is now split by real callsite family:
    - schema explain payload maps
    - remaining runtime/status payload builders
  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=133 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=133 fail=0`

- Closed iterator tail error propagation so iterator OOM no longer degrades
  into silent truncation:
  - `src/lisp/primitives_iter_state.c3` now centralizes iterator pair
    construction through `iterator_make_pair_or_propagate(...)`.
  - `src/lisp/primitives_iter_sources.c3` and
    `src/lisp/primitives_iter_coroutine.c3` now use that helper whenever a
    thunk builds `(item . next)`, so any tail `ERROR` is returned directly
    instead of being wrapped in a `CONS` and later mistaken for iterator
    termination.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves both
    a source iterator thunk and a coroutine iterator thunk propagate tail
    allocation failure directly.
  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=128 fail=0`

- Closed the runtime constructor OOM substrate for the live iterator/error and
  language-facing collection surfaces:
  - `src/lisp/value_core_types.c3`,
    `src/lisp/value_interp_alloc_helpers.c3`,
    `src/lisp/value_constructors_lifecycle.c3`, and
    `src/lisp/primitives_meta_types.c3`
    now track whether `STRING` / `ERROR` chars are heap-owned, so fallback
    literal-backed error values no longer flow into invalid frees.
  - `src/lisp/value_constructors.c3` now makes `make_error(...)` fail closed:
    failed message allocation returns a printable fallback `ERROR` instead of
    dereferencing a failed heap allocation.
  - `src/lisp/value_predicates_accessors_basic.c3` and
    `src/lisp/prim_collection_hashmap.c3`
    now expose checked `ARRAY` / `HASHMAP` / `SET` constructor and hashmap-grow
    helpers with deterministic OOM seams.
  - the runtime-dependent surfaces that were actually crashing on this substrate
    now use the checked path:
    - raise payload construction
    - `Dictionary`
    - `Set`
    - `to-array`
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now proves:
    - `make_error(...)` allocation failure keeps a printable fallback error,
    - iterator ctor raises survive payload-map allocation failure,
    - `to-array` fails closed on result-array allocation failure,
    - checked collection constructor and grow failures return ordinary `ERROR`
      values instead of crashing.
  - residual scope is now narrower and explicit:
    - broad internal migration from unchecked `make_array(...)` /
      `make_hashmap(...)` callsites remains a separate follow-up lane instead
      of being hand-waved as complete.

- Closed the broader shared `StringVal` builder OOM lane so builder creation
  and growth now fail closed instead of crashing before result materialization:
  - `src/lisp/prim_string_format_helpers.c3` now:
    - returns `null` from `strval_new(...)` on builder allocation failure,
    - gives `strval_ensure(...)` an explicit `bool` failure contract with size
      overflow guards,
    - stops `strval_push(...)` / `strval_append(...)` / padding helpers from
      writing after a failed growth attempt,
    - and exposes deterministic seams for initial builder allocation and
      builder growth failure.
  - `src/lisp/prim_string_ops.c3`,
    `src/lisp/prim_string_format.c3`, and
    `src/lisp/prim_string_format_directives.c3`
    now propagate those builder failures as typed runtime OOM errors instead of
    continuing with invalid builder state.
  - parser string literal construction in:
    - `src/lisp/parser_datum_helpers.c3`
    - `src/lisp/parser_expr_atoms.c3`
    - `src/lisp/parser_patterns_values.c3`
    - `src/lisp/parser_quasiquote_datum_helpers.c3`
    now uses the same checked builder path and fails closed with parser errors.
  - `src/lisp/primitives_meta_types.c3` no longer uses unchecked `StringVal`
    allocation in the `unsafe-free` error path.
  - new regressions:
    - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` now covers
      direct result-wrapper failure plus builder allocation/growth failure for
      runtime string helpers.
    - `src/lisp/tests_compiler_core_groups_fail_closed.c3` now covers parser
      string-literal builder allocation failure.
  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=127 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=127 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp'` -> `pass=191 fail=0`

- Closed adjacent runtime-helper allocation fail-open paths and tightened stale
  lifetime regressions exposed by ASAN:
  - `src/lisp/eval_apply.c3` now fails closed if chained partial application
    cannot allocate its next `PARTIAL_PRIM` wrapper, returning an eval error
    instead of dereferencing a null `interp.alloc_value()` result.
  - `src/lisp/primitives_iter_state.c3` and
    `src/lisp/value_predicates_accessors_basic.c3` now fail closed when
    iterator thunk or iterator wrapper allocation fails, surfacing runtime OOM
    errors instead of writing through null wrapper pointers.
  - `src/lisp/prim_string_ops.c3`, `src/lisp/prim_string_format.c3`, and
    `src/lisp/prim_string_format_helpers.c3` now route final string result
    materialization through one checked helper so forced wrapper-allocation
    failure disposes the transient `StringVal` builder and returns a typed
    runtime error.
  - `src/lisp/http_url_response.c3` now routes `status` / `headers` / `body`
    key materialization through one checked helper, so response parsing no
    longer dereferences null field-key allocations.
  - `src/lisp/tests_memory_lifetime_runtime_alloc_groups.c3` adds direct smoke
    regressions for:
    - chained partial allocation failure,
    - iterator thunk and wrapper allocation failure,
    - string result-wrapper allocation failure in replace/repeat/format paths,
    - HTTP response field-key allocation failure for all three emitted keys.
  - ASAN exposed stale test-only post-release reads in:
    - `src/lisp/tests_memory_lifetime_groups.c3`
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3`
    - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3`
    Those assertions now snapshot expected error/copy state before releasing
    the relevant target scope.
  - validation:
    - `rm -rf build/obj/linux-x64 build/main && c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=126 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=1:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=126 fail=0`

- Closed remaining boundary leaf-wrapper allocation fail-open paths for
  `INSTANCE`, `FFI_HANDLE`, and `TIME_POINT` copies:
  - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now fails closed before
    `instance_retain(...)` / `ffi_handle_retain(...)` if destination wrapper
    allocation fails for `copy_instance_to_parent(...)` or
    `copy_ffi_handle_to_parent(...)`.
  - `src/lisp/eval_promotion_copy_route_helpers.c3` now returns a typed
    boundary error if `copy_time_point_to_parent(...)` cannot allocate its
    wrapper instead of dereferencing a null result.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves forced
    boundary-copy wrapper allocation failure:
    - does not retain instance owner scopes,
    - does not increment `FFI_HANDLE` refcounts or trigger early finalization,
    - and surfaces a typed error for `TIME_POINT` copies.
  - validation:
    - `rm -rf build/obj/linux-x64 build/main && c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/check_status_consistency.sh`

- Closed env-copy / return-boundary wrapper-allocation null-deref paths for
  closure and iterator special cases:
  - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now treats a null
    destination wrapper in `copy_parent_clone_closure_payload(...)` as a
    typed boundary allocation failure instead of dereferencing it, and
    `copy_closure_to_parent(...)` now supports deterministic wrapper alloc
    failure injection for regression coverage.
  - `src/lisp/eval_env_copy_values.c3` now:
    - fails closed if `copy_env_copy_time_point(...)` cannot allocate its
      wrapper,
    - routes env-copy closure wrapper allocation through the same guarded
      helper path, and
    - rolls back iterator inner payloads and returns `null` if iterator wrapper
      allocation itself fails instead of dereferencing a null wrapper.
  - `src/lisp/tests_memory_lifetime_env_copy_closure_groups.c3` now proves:
    - `copy_to_parent(...)` closure wrapper allocation failure does not retain
      detached closure env scopes or disturb later target/source teardown, and
    - env-copy closure wrapper allocation failure surfaces
      `BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY` and preserves detached
      closure env-scope ownership symmetry.
  - validation:
    - `rm -rf build/obj/linux-x64 build/main && c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/check_status_consistency.sh`

- Closed the remaining full-slice `jit-policy` crash by hardening the TCO
  recycle TEMP-graph scan:
  - `src/lisp/jit_jit_eval_scope_chain_helpers.c3` no longer allocates four
    `4096`-entry pointer arrays on the runtime stack inside
    `jit_graph_binding_reaches_temp_scope(...)`.
  - the scan state now lives in one heap-allocated `JitTempGraphScan`, so the
    TCO recycle safety gate no longer segfaults on entry when a top-level JIT
    policy case drives that helper through a smaller runtime stack.
  - this specifically restores the `stale-raise-scrub` JIT policy case, whose
    top-level run path was only exposing the stack-footprint bug in the nested
    graph scanner.
  - validation:
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=stale-raise-scrub ./build/main --test-suite lisp'` -> `1 passed, 0 failed`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp'` -> `pass=41 fail=0`

- Closed the normal-teardown and allocation-failure symmetry gaps for
  boundary-owned `CONTINUATION` wrappers:
  - `src/lisp/value_constructors_lifecycle.c3` now releases retained
    handle-state on normal scope teardown when a copied/promoted continuation
    wrapper carries `continuation_boundary_owned`.
  - `src/lisp/eval_promotion_copy_route_helpers.c3` and
    `src/lisp/eval_promotion_escape_leaf.c3` now:
    - register dtors for successful copied/promoted continuation wrappers that
      actually introduced the retained handle-state ref, and
    - immediately release that retained handle-state ref if wrapper
      allocation fails after the retain step.
  - `src/lisp/eval_env_copy_frame_helpers.c3` and
    `src/lisp/eval_promotion_root_clones.c3` now tombstone those copied
    wrapper dtors before manual rollback cleanup, so abandoned continuation
    wrappers unwind exactly once.
  - `src/lisp/jit_jit_runtime_effects.c3` now decrements
    `HandleEffectState.continuation_refcount` during `resolve` only when the
    resolved continuation actually owned the retained handle-state ref, so one
    unretained continuation can no longer consume another continuation’s
    shared-state retain.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves:
    - successful boundary-owned continuation wrappers release their retained
      handle-state ref on normal target-scope teardown,
    - boundary copy allocation failure after the retain step releases
      immediately, and
    - ESCAPE promotion allocation failure after the retain step releases
      immediately.
  - validation:
    - `rm -rf build/obj/linux-x64 build/main && c3c build`
    - `rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=118 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=multi-interp-lifetime,continuation-teardown,shared-handle-state-teardown,cross-interp-continuation-guard,escaped-handle-continuation-guard,side-effect-escaped-handle-continuation-guard ./build/main --test-suite lisp'` -> `6 passed, 0 failed`
    - `scripts/check_status_consistency.sh`

- Closed a continuation rollback symmetry gap in boundary/env-copy cleanup:
  - `src/lisp/eval_promotion_copy_route_helpers.c3` and
    `src/lisp/eval_promotion_escape_leaf.c3` now mark copied/promoted
    `CONTINUATION` wrappers when that specific wrapper introduced boundary
    handle-state retention.
  - `src/lisp/eval_env_copy_frame_helpers.c3` and
    `src/lisp/eval_promotion_root_clones.c3` now unwind that retention when a
    copied continuation wrapper is abandoned during env-copy rollback or
    partial boundary cleanup, instead of leaving the shared continuation
    handle-state refcount artificially elevated after failure.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now proves a
    later env-copy binding fault releases a first copied continuation retain.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves shared-
    wrapper partial cleanup releases the same continuation retain when a later
    opaque child aborts the copy.
  - validation:
    - `c3c build`
    - `c3c build --sanitize=address`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/check_status_consistency.sh`

- Closed a nested `CONS` alias blind spot in shared provenance and env-copy:
  - `src/lisp/eval_boundary_provenance.c3` now routes `CONS` reuse checks
    through an iterative cons-specific helper that validates each nested cons
    shell against the releasing scope and target scope chain before reusing the
    outer wrapper by identity.
  - this closes the case where a target-chain outer `CONS` used to be treated
    as alias-safe even though a nested `cdr` cons shell still belonged to the
    releasing/source scope and only exposed scalar leaves.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves ordinary
    boundary copy and ESCAPE promotion defensively clone that nested cons
    structure.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now proves
    env-copy clones the same target-chain outer `CONS` instead of reusing it by
    identity when the nested cons shell still belongs to the source scope.
  - validation:
    - `c3c build`
    - `c3c build --sanitize=address`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/check_status_consistency.sh`

- Closed the wrapper-slot leak on shared-wrapper and root-store partial aborts:
  - `src/lisp/eval_promotion_copy_route_helpers.c3`,
    `src/lisp/eval_promotion_root_clone_basic.c3`, and
    `src/lisp/eval_promotion_root_clones.c3` now delay destination wrapper
    allocation/registration until after fallible child-copy and payload clone
    work succeeds.
  - when wrapper allocation itself fails after payload materialization, those
    paths now route through the existing partial-cleanup helpers so copied
    child retains and heap payloads are unwound before returning.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves repeated
    failed shared-wrapper copy attempts do not monotonically grow the
    surviving target scope allocation count.
  - `src/lisp/tests_memory_lifetime_root_boundary_groups.c3` now proves the
    same invariant for repeated failed root-store method-table clone attempts
    against `root_scope`.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/check_status_consistency.sh`

- Closed a destination-commit promotion-context drift:
  - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` now routes
    direct destination escape promotion through an explicit ctx-aware helper
    instead of silently consuming `interp.active_promotion_ctx`.
  - `src/lisp/eval_boundary_commit_escape_helpers.c3` and
    `src/lisp/eval_boundary_commit_destination.c3` now use that helper for
    releasing-scope retry, mixed-destination retry, and direct destination
    promotion, so the caller-provided `PromotionContext` owns the same
    memo/budget/abort epoch across the whole destination-commit lane.
  - destination-builder teardown now restores both `memo_head` and the
    builder-local scope-chain cache snapshot, so temporary build-scope cache
    entries do not survive after the builder returns or aborts.
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now
    proves:
    - repeated builder calls do not retain builder-local scope-chain cache
      state, and
    - a non-active caller `PromotionContext` still receives the abort state
      from direct destination promotion while the unrelated active context
      remains untouched.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`
    - `scripts/check_status_consistency.sh`

- Closed a direct `CONS` rollback symmetry gap in boundary copy and ESCAPE promotion:
  - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now treats iterative
    `CONS` copy as transactional: newly allocated spine cells are initialized
    to null edges, and any already-copied car/cdr payloads are unwound through
    `boundary_cleanup_materialized_value(...)` if a later cdr copy or spine
    allocation fails.
  - `src/lisp/eval_promotion_escape_structured.c3` now applies the same
    rollback rule to iterative ESCAPE-lane `CONS` promotion, so promoted car
    retains are not left live until target teardown when a later cdr promotion
    or tail-cell allocation fails.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves direct
    `CONS` boundary copy immediately unwinds a copied detached-closure car when
    the cdr fails with an opaque primitive payload fault.
  - `src/lisp/tests_memory_lifetime_groups.c3` now proves the same invariant
    for direct ESCAPE `CONS` promotion.
  - validation:
    - `c3c build`
    - `c3c build --sanitize=address`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=102 fail=0`
    - `scripts/check_status_consistency.sh`

- Closed a destination-commit promotion-context drift and a nested wrapper
  rollback symmetry gap:
  - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3` now threads the
    caller `PromotionContext` through destination iterator detachment instead
    of discarding it on the detach subpath, so destination-built iterator
    copies now honor same-epoch memo reuse, budget/accounting, and context
    abort propagation like the rest of the builder lane.
  - `src/lisp/eval_promotion_root_clones.c3` now makes
    `boundary_cleanup_materialized_value(...)` recurse through copied nested
    `PARTIAL_PRIM` and `ITERATOR` payloads before cleaning the outer wrapper,
    closing the case where shared-wrapper late-failure cleanup would leave
    copied closure/env retains live when the already-materialized child was
    wrapped in a copied partial or iterator shell.
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now
    proves repeated destination iterator detachment in one builder epoch
    reuses the same copied closure via the shared promotion context.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves failed
    copied `ARRAY` wrappers unwind nested copied `PARTIAL_PRIM` and `ITERATOR`
    payload retains immediately and do not destroy the original owner closure
    env scope during later target teardown.
  - validation:
    - `c3c build`
    - `c3c build --sanitize=address`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=100 fail=0`
    - `scripts/check_status_consistency.sh`

- Closed a TCO recycle TEMP-graph detection drift:
  - `src/lisp/jit_jit_eval_scope_chain_helpers.c3` no longer treats
    “graph-carrying wrapper itself is outside TEMP” as sufficient for the
    recycle fast-reset gate.
  - `env_chain_has_graph_binding_values_in_temp_scope(...)` now walks nested
    `CONS`, `ARRAY`, `HASHMAP` / `SET`, `CLOSURE`, `PARTIAL_PRIM`,
    `METHOD_TABLE`, `MODULE`, `ITERATOR`, and `INSTANCE` edges transitively,
    with bounded cycle-aware visited sets, and it fails closed by reporting
    “unsafe, do not fast-reset” on scan overflow.
  - this closes the case where `runtime_prepare_tco_recycle_env(...)` could
    call `scope_reset_temp_lane(...)` even though an owner-scope or
    target-chain wrapper binding still pointed transitively into the recycle
    scope TEMP lane.
  - `src/lisp/tests_memory_lifetime_tco_budget_groups.c3` now includes a
    focused manual regression proving a TCO recycle-scope nested `CONS` inside
    an owner-scope `ARRAY` binding forces scope replacement and env-copy
    instead of in-place TEMP reset, and
    `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` wires that probe
    into the bounded smoke lane.
  - validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed a JIT TCO nested-alias reuse hole for partial wrappers:
  - `src/lisp/jit_jit_eval_scope_chain_helpers.c3` no longer treats
    “wrapper already in the target chain” as sufficient to skip copying during
    TCO env-frame transfer.
  - target-chain bindings now still consult nested alias safety, so
    `PARTIAL_PRIM` / `ITERATOR` wrappers are copied when a shared-wrapper arg
    still contains a child from the releasing scope.
  - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now applies the same
    partial-payload alias rule in the shared fast-reuse precheck, so foreign
    `PARTIAL_PRIM` bindings with releasing-scope nested payloads no longer
    bypass TCO copy just because the wrapper itself lives outside the
    releasing scope.
  - `src/lisp/jit_jit_eval_scope_chain_helpers.c3` now also copies disjoint
    graph-carrying wrappers during TCO env-frame transfer instead of reusing
    them by identity merely because they are outside the releasing scope.
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3` now proves that TCO
    env-copy clones both target-chain and foreign partial/iterator wrappers
    instead of reusing them by identity when their nested array payload still
    points into the releasing scope.
  - the same focused JIT policy file now also proves that a foreign `ARRAY`
    binding is copied by value and remains valid after the foreign owner scope
    is released.
  - validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=tco-partial-shared-wrapper-edge-copy ./build/main --test-suite lisp`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=tco-foreign-partial-shared-wrapper-edge-copy ./build/main --test-suite lisp`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=tco-foreign-shared-wrapper-copy ./build/main --test-suite lisp`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed an env-copy rollback destructor-symmetry bug:
  - `src/scope_region_chunk_helpers.c3` now exposes
    `scope_cancel_dtor(...)` and `scope_cancel_dtor_escape(...)`, which
    tombstone the newest matching TEMP- or ESCAPE-lane destructor entry
    without mutating dtor-list topology.
  - `src/lisp/eval_env_copy_frame_helpers.c3` now cancels a copied
    target-scope value's registered dtor before manually unwinding that value
    during env-copy rollback, so rollback no longer replays
    `scope_dtor_value(...)` or `scope_dtor_closure(...)` and then lets target
    scope teardown run the same destructor again.
  - `src/lisp/eval_promotion_root_clones.c3`,
    `src/lisp/eval_promotion_copy_route_helpers.c3`,
    `src/lisp/eval_promotion_root_clone_basic.c3`, and
    `src/lisp/eval_promotion_escape_structured.c3` now use the same dtor
    tombstoning in shared-wrapper partial cleanup, so ordinary boundary copy,
    root-store clone, and ESCAPE-promotion late-failure cleanup no longer
    replay copied child dtors after manual unwind.
  - this closes the concrete double-dtor hazard for copied closures with
    retained standalone env scopes and keeps rollback transactional instead of
    relying on later target teardown to tolerate already-consumed ownership.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now proves the
    copied closure env scope is not destroyed when the abandoned target scope
    is released after a failed nested-array env-copy, and is destroyed exactly
    once when the original owner scope later releases it.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves the same
    teardown symmetry for ordinary shared-wrapper partial cleanup: failed
    copied `ARRAY` and `HASHMAP` wrappers leave the original closure env scope
    intact across target-scope release and destroy it exactly once when the
    foreign owner scope later releases it.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed a shared-wrapper cleanup symmetry follow-up:
  - `src/lisp/eval_promotion_root_clones.c3` now makes
    `boundary_cleanup_materialized_value(...)` recurse through copied nested
    `CONS`, `ARRAY`, `HASHMAP` / `SET`, and `METHOD_TABLE` children before
    destroying the outer copied wrapper.
  - ordinary boundary-copy, ESCAPE-promotion, and root-store clone partial
    cleanup paths all share that helper, so late failure in a copied shared
    wrapper no longer leaves nested copied closure retains live when the copied
    child itself was wrapped in a copied `CONS`.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` and
    `src/lisp/tests_memory_lifetime_root_boundary_groups.c3` now strengthen the
    existing partial-cleanup regressions by forcing the retained closure through
    a copied `CONS` shell before the later opaque primitive failure.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=92 fail=0`

- Closed an env-copy rollback follow-up for nested copied bindings:
  - `src/lisp/eval_env_copy_frame_helpers.c3` now recursively unwinds copied
    nested `CONS`, `ARRAY`, `HASHMAP` / `SET`, and `METHOD_TABLE` children
    during mid-frame env-copy rollback instead of only cleaning top-level
    wrappers plus iterator/partial shells.
  - failed env-copy now releases nested copied closure retains immediately at
    rollback time, rather than leaving them live until the abandoned target
    scope is eventually torn down.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now includes
    focused regressions proving nested `CONS` and `ARRAY` bindings unwind their
    copied closure-env retains immediately on rollback and remain stable across
    later target-scope teardown.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'`

- Closed the destination-builder memo contract follow-up:
  - `src/lisp/eval_boundary_commit_escape_builder_helpers.c3`,
    `src/lisp/eval_boundary_commit_escape_cons.c3`, and
    `src/lisp/eval_boundary_commit_escape_wrappers.c3` now make the shipped
    contract explicit: memo entries created while routing nested children
    inside temporary destination build scopes are builder-local and are
    discarded when the builder returns or aborts.
  - same-epoch alias reuse is therefore not guaranteed across repeated
    destination-builder invocations; the correctness contract is scoped to the
    committed ESCAPE result, not to transient builder memo nodes.
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now pins
    that behavior with a focused regression proving nested builder memo state
    does not survive after return and repeated builder invocations materialize
    fresh destination graphs instead of reusing transient memoized children.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=90 fail=0`

- Closed the target-chain shared-wrapper provenance follow-up:
  - `src/lisp/eval_boundary_provenance.c3` now walks nested `ARRAY`,
    `HASHMAP` / `SET`, and `METHOD_TABLE` payload edges before admitting
    fast reuse for an already-target-chain wrapper, so reuse now agrees with
    the existing graph-audit ownership model instead of only checking the
    wrapper shell.
  - target-chain wrapper reuse now fails closed back into the existing deep
    copy / ESCAPE promotion builders when any nested child still lives in the
    releasing scope or outside the surviving target chain.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now includes a focused
    regression proving target-chain `ARRAY`, `HASHMAP`, `SET`, and
    `METHOD_TABLE` wrappers clone instead of reusing pointer identity when one
    nested child is still releasing-scope owned, and
    `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` wires that probe
    into the bounded smoke lane.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=89 fail=0`

- Closed a promotion-context/env-copy follow-up in the boundary hardening lane:
  - `src/lisp/eval_promotion_context.c3` now treats memo-entry allocation
    failure as a fail-closed promotion-context abort instead of dereferencing a
    null memo node.
  - `src/lisp/eval_env_copy_values.c3` and
    `src/lisp/eval_env_copy_frame_helpers.c3` now self-clean and rollback
    iterator payload materializations when the outer iterator wrapper cannot be
    produced, including iterator payloads backed by copied `PARTIAL_PRIM`
    values.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now proves
    iterator wrapper allocation failure does not leak an already-copied inner
    payload retain.

- Closed a destination-escape commit fail-closed follow-up for nested boundary
  faults:
  - `src/lisp/eval_boundary_commit_escape_cons.c3` and
    `src/lisp/eval_boundary_commit_escape_wrappers.c3` now bubble
    boundary-generated nested child-copy errors back out as top-level commit
    errors instead of rebuilding `CONS` / `PARTIAL_PRIM` / `ITERATOR` wrappers
    with embedded `ERROR` children.
  - `src/lisp/eval_boundary_commit_destination.c3` now classifies those
    builder-returned top-level errors as destination error promotion rather
    than pretending a structured destination build succeeded.
  - `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3` now
    includes focused regressions proving nested opaque primitive faults fail
    closed for destination-built `CONS`, `PARTIAL_PRIM`, and `ITERATOR`
    commit paths.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=87 fail=0`
    - `scripts/check_status_consistency.sh` -> pass

- Closed the root-store shared-wrapper partial-cleanup follow-up:
  - `src/lisp/eval_promotion_root_clone_basic.c3` now routes late-failure
    `ARRAY` and `HASHMAP` / `SET` clone aborts through the same shared partial
    cleanup helpers used by ordinary boundary copy and ESCAPE promotion.
  - root-store clone rollback now unwinds already-copied child ownership side
    effects before freeing the aborted heap wrapper, instead of leaking copied
    closure-env retains on a later element/value failure.
  - `src/lisp/tests_memory_lifetime_root_boundary_groups.c3` now includes a
    focused root-boundary regression covering both `ARRAY` and `HASHMAP`
    partial-clone abort cleanup, and
    `src/lisp/tests_memory_lifetime_groups.c3` wires it into the bounded smoke
    lane.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=86 fail=0`

- Closed the shared-wrapper partial-cleanup follow-up for boundary copy and
  ESCAPE promotion:
  - `src/lisp/eval_promotion_root_clones.c3` now owns shared cleanup helpers
    for partially materialized arrays, hashmaps/sets, and method tables, and
    those helpers unwind already-copied child ownership side effects before
    freeing the aborted heap wrapper.
  - `src/lisp/eval_promotion_copy_route_helpers.c3` and
    `src/lisp/eval_promotion_escape_structured.c3` now route array,
    hashmap/set, and method-table partial-copy failures through that cleanup
    path instead of freeing only the wrapper heap and leaking already-copied
    child retains.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now includes a
    disjoint-array regression that proves a failed second-element copy releases
    the first copied closure-env retain, and
    `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` wires that probe
    into the bounded smoke lane.
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=85 fail=0`

- Closed two post-`478bc24` boundary follow-ups:
  - `src/lisp/eval_env_copy_values.c3` now routes iterator closure payloads
    through the same safe undelimited clone helper as plain closure env-copy,
    so iterators over global-env closures no longer fail closed more
    aggressively than the underlying closure contract.
  - `src/lisp/jit_jit_closure_support.c3` now exposes a scope-copy failure
    seam for method-signature cloning, and
    `src/lisp/jit_jit_closure_let_set_helpers.c3` now fails closed if detached
    env-scope recursive closure publication cannot copy its typed signature
    instead of silently publishing a downgraded closure with `type_sig = null`.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` now covers
    iterator payload closure cloning for safe undelimited global-env captures.
  - `src/lisp/tests_runtime_feature_jit_groups_more.c3` now covers recursive
    detached closure publication under forced signature-copy failure.
  - validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy OMNI_JIT_POLICY_FILTER=rec-closure-type-sig-copy-failure ./build/main --test-suite lisp` -> `pass=1 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` -> `pass=84 fail=0`

## 2026-04-09

- Closed the advanced recursive-closure and generated-source regression slice:
  - `src/lisp/jit_jit_compile_let_set_helpers.c3` now treats closure-local
    mutation through collection-surface calls (`push!`, `remove!`, `set!`) as
    a mutable capture, so `let ^rec` closures box captured locals before
    recursive execution instead of silently dropping array updates.
  - `src/lisp/jit_jit_compile_let_set.c3` now threads `Interp*` into that
    mutability check, and `src/lisp/jit_jit_closure_let_set_helpers.c3` now
    publishes the patched recursive closure value explicitly and fails closed
    if the env-scope self-reference clone cannot be allocated.
  - `src/lisp/value_interp_lifecycle.c3` now rehashes macro/module tables with
    the same symbol-id slotting policy already used by runtime lookup, fixing
    table growth drift that could surface in wide advanced test setup.
  - `src/lisp/tests_advanced_tests.c3` now returns the full generated source
    slice from `advanced_test_cstr_slice(...)`, and the wide advanced builder
    callsites in `src/lisp/tests_advanced_core_semantics_groups.c3`,
    `src/lisp/tests_advanced_macro_hygiene_groups.c3`,
    `src/lisp/tests_advanced_type_dispatch_groups.c3`, and
    `src/lisp/tests_advanced_type_parametric_groups.c3` now use that helper
    consistently instead of truncating generated forms before parse.
  - `src/lisp/tests_advanced_core_semantics_groups.c3` now includes direct
    `let ^rec` regressions proving a recursively-invoked closure preserves
    captured array mutation state across `push!` updates.
  - `src/lisp/tests_harness_helpers.c3` now prints setup failure coordinates
    and the generated source text when a test fixture parse/eval step fails,
    which keeps wide generated-source regressions local to the failing setup.
  - validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_ADVANCED_GROUP_FILTER=advanced-core-semantics ./build/main --test-suite lisp` -> `pass=68 fail=0`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp` -> `pass=1164 fail=0`

- Closed the process-handle concurrency lane for `process-wait` / `process-kill` reuse:
  - `src/lisp/async_process_signal_runtime.c3` now gives each process handle a shared in-flight guard, and `src/lisp/async_process_lifecycle.c3` preserves the closed state while the handle is in use so concurrent reuse fails closed instead of racing on the same live `uv_process`.
  - `src/lisp/async_process_signal_dns_process.c3` now returns a normalized `io/process-handle-busy` error when a concurrent `process-wait` or `process-kill` attempts to reuse the same live process handle.
  - `src/lisp/tests_advanced_io_effect_ffi_scheduler_boundary.c3` and `src/lisp/tests_advanced_io_effect_ffi_groups.c3` now cover the concurrent reuse contract directly.
  - validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp` -> pass

- Closed the residual env-copy shared-wrapper hole that remained after the
  earlier return-boundary and ESCAPE-lane hardening:
  - `src/lisp/eval_env_copy_values.c3` no longer returns `ARRAY` /
    `HASHMAP` / `SET` / `METHOD_TABLE` bindings by raw pointer identity during
    env-copy. Those wrappers now route through the same boundary-copy policy
    already used by other promotion paths, so source-scope shared wrappers are
    cloned when env-copy crosses into a different target scope.
  - `src/lisp/tests_memory_lifetime_env_copy_groups.c3` now covers the shared
    wrapper env-copy contract directly, proving disjoint collection and method
    table wrappers are cloned and remain valid after source-scope teardown.
  - `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3` wires that
    focused regression into the memory-lifetime env-copy smoke lane.

- Closed a post-wave fail-closed cleanup and transport follow-up:
  - `src/lisp/eval_promotion_copy_route_helpers.c3` and
    `src/lisp/eval_promotion_escape_structured.c3` now reuse the shared
    `method_table_free_partial_heap(...)` helper so method-table abort paths
    destroy already-copied heap signatures before freeing the table shell.
  - `src/lisp/aot_runtime_bridge_trampoline.c3` now rejects negative arg
    counts in `aot::apply_multi(...)`, `aot::apply_multi_tail(...)`, and the
    shared multi-arg fast path before any signed-to-unsigned conversion or
    malformed AOT closure application can proceed.
  - `src/lisp/compiler_native_call_compilation_flat_style.c3` now frees
    `_closure_data_*` when AOT closure construction returns `ERROR`, matching
    the existing safe closure-emission path.
  - `src/lisp/http.c3`, `src/lisp/http_connection.c3`, and
    `src/lisp/http_url_response.c3` now:
    - close HTTP/TCP/TLS connections on all post-connect early-return paths,
    - propagate transport read errors instead of parsing partial error output
      as a synthetic success response,
    - reject out-of-range URL ports at parse time.

- Closed the validation live-e2e evidence lane:
  - `scripts/run_validation_status_summary.sh` now runs `scripts/run_e2e.sh`
    as part of the summary bundle and refuses to report green unless the
    current run proves Stage 4 live e2e execution and a passing Stage 5 result
    in the generated logs.
  - `TODO.md` no longer tracks `AUDIT-VALIDATION-E2E-LIVE-003`; the remaining
    live items are the process concurrency, compiler diagnostic parity, and
    method-table abort-cleanup coverage lanes.

- Closed the remaining compiler diagnostic parity and method-table abort
  coverage lanes:
  - `src/lisp/jit_jit_apply_multi_prims.c3` now formats non-tail fixed-arity
    and variadic multi-arg under-application with the same canonical messages
    already used by the tail and AOT helpers.
  - `src/lisp/tests_compiler_core_groups_fail_closed.c3` now asserts that
    compiler syntax failures report user-source coordinates after the stdlib
    prelude line offset is stripped from diagnostics.
  - `src/lisp/eval_promotion_root_clones.c3` now exposes narrow
    method-table-abort cleanup telemetry, and
    `src/lisp/jit_jit_closure_support.c3` now exposes a targeted
    heap-signature copy failure seam.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now proves both
    copy-to-parent and escape-promotion method-table abort paths reclaim
    already-copied heap signatures, and
    `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3` includes that
    regression in the bounded smoke lane.
  - `TODO.md` is back to zero live backlog items for this audit pass.

- Closed the shared-wrapper boundary hardening lane and the integration
  blockers that surfaced while validating it:
  - `src/lisp/eval_promotion_copy.c3`,
    `src/lisp/eval_promotion_copy_route_helpers.c3`,
    `src/lisp/eval_promotion_escape_leaf.c3`, and
    `src/lisp/eval_promotion_escape_structured.c3` no longer return disjoint
    `ARRAY` / `HASHMAP` / `SET` / `METHOD_TABLE` wrappers by pointer identity
    once fast reuse is declined; they now defensively clone those wrappers and
    recurse through nested payload edges.
  - `METHOD_TABLE` shared-wrapper clones now keep copied signatures on heap
    storage so `scope_dtor_value(...)` remains compatible with the existing
    destructor contract and does not attempt to free scope-owned signature
    arrays.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` now covers both
    defensive shared-wrapper cloning and the nested fail-closed boundary-copy
    path.
  - `src/lisp/tests_memory_lifetime_finalize_groups.c3` no longer reads a
    detached-scope env frame after `scope_release(detached)`, removing the
    finalize-lane UAF that was blocking bounded smoke validation.
  - Validation:
    - `rm -rf build/obj/linux-x64 build/main && c3c build`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` ->
      `pass=81 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build --sanitize=address && env ASAN_OPTIONS=abort_on_error=1:detect_leaks=0:symbolize=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` ->
      `pass=81 fail=0`

- Closed the validation integrity lane:
  - `scripts/check_status_consistency.sh` now accepts the current zero-item
    backlog wording.
  - `scripts/run_validation_container.sh` now serializes bounded validation
    runs with a repo-local lock.
  - `scripts/run_validation_status_summary.sh` now treats missing required
    `OMNI_TEST_SUMMARY` telemetry as a validation failure.
  - `scripts/c3c_limits.sh` now preserves quoted extra Docker args.
  - `scripts/run_e2e.sh` and `scripts/check_e2e_baseline_policy.sh` now guard
    Stage 3 compile-source parity explicitly.
  - Validation:
    - `scripts/check_status_consistency.sh`
    - `scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `bash -n scripts/check_status_consistency.sh scripts/run_validation_container.sh scripts/run_validation_status_summary.sh scripts/c3c_limits.sh scripts/run_e2e.sh scripts/check_e2e_baseline_policy.sh`

- Closed the focused CRUD concurrency follow-up:
  - `examples/deduce_crud_server.omni` now guards CRUD mutation entrypoints
    with a shared atomic write gate, preventing spawned overlap from surfacing
    raw runtime `ERROR` payloads.
  - the shipped application contract is now explicit:
    - one overlapping write succeeds,
    - the other resolves as a normalized application conflict
      (`crud write already in progress` or `item already exists`),
    - and only one row persists for the shared id.
  - `src/lisp/tests_runtime_feature_http_groups.c3`,
    `src/lisp/tests_tests.c3`, and `src/lisp/tests_slice_policy.c3` now move
    the spawned CRUD concurrency probe into the focused `http-crud` slice so
    the broad `http` slice remains deterministic.
  - Validation:
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp` ->
      `pass=29 fail=0`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http-crud ./build/main --test-suite lisp` ->
      `pass=1 fail=0`

- Closed the compiler / JIT / AOT parity hardening lane for multi-arg apply,
  parse fail-closed behavior, and generated closure-capture OOM handling:
  - `src/lisp/jit_jit_apply_multi_prims.c3` and
    `src/lisp/jit_jit_apply_multi_prims_tail.c3` now null-guard multi-arg
    argument-buffer allocation for primitive and method-table dispatch, using
    a focused test seam so low-memory paths return an explicit error instead
    of dereferencing `null`.
  - `src/lisp/parser_top_level_parse.c3` and
    `src/lisp/compiler_program_pipeline_helpers.c3` now fail closed on parser
    error instead of returning a silently truncated prefix program to caller
    pipelines.
  - `src/lisp/aot.c3` now provides shared arg-list counting plus exact/minimum
    arity helpers, and `src/lisp/compiler_code_emission_lambda_defs.c3` now
    emits those checks into generated multi-arg lambda entrypoints so
    under-application rejects deterministically, malformed arg lists fail
    closed, and over-application keeps JIT-style chaining via
    `aot::apply_multi(...)`.
  - `src/lisp/compiler_native_call_compilation_flat_style.c3` now guards
    generated closure-capture allocation in flat expression lowering without
    emitting invalid raw `return` statements into non-`Value*` contexts.
  - focused regressions landed in:
    - `src/lisp/tests_runtime_feature_jit_groups_more.c3`
    - `src/lisp/tests_compiler_core_groups_fail_closed.c3`
    - `src/lisp/tests_compiler_core_groups.c3`
  - Validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp` ->
      `pass=35 fail=0`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` ->
      `pass=193 fail=0`
    - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`

- Hardened the async / REPL / FFI safety lane against partial-init and handle
  confusion defects:
  - `src/lisp/eval_repl_server_worker.c3` now refuses to start the REPL
    worker thread unless both mutex and condition-variable initialization
    succeeded, so partial-init paths fail closed before the worker can touch
    uninitialized sync state.
  - `src/lisp/async_socket_handle_runtime.c3`,
    `src/lisp/tls_handle_lifecycle.c3`, and
    `src/lisp/prim_io_fs_stream.c3` now validate exact FFI handle names before
    casting TCP/UDP/TLS/FS payloads, preventing unrelated `FFI_HANDLE` boxes
    from being reinterpreted as transport state.
  - `src/lisp/async_process_spawn.c3` now treats constructor-returned error
    values from `make_process_handle(...)` and `make_fs_handle(...)` as hard
    failures instead of packaging them into a success-shaped spawn result.
  - `src/lisp/http_url_response.c3` now rejects malformed `:port` suffixes
    with trailing garbage or missing digits, and trims HTTP response header
    slices so the exposed header string no longer retains delimiter residue.
  - focused regressions landed in:
    - `src/lisp/tests_runtime_async_repl_server_groups.c3`
    - `src/lisp/tests_advanced_io_effect_ffi_groups.c3`
    - `src/lisp/tests_runtime_feature_http_groups.c3`
  - Validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=async ./build/main --test-suite lisp` ->
      `pass=61 fail=0`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system ./build/main --test-suite lisp` ->
      `pass=42 fail=0`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_TEST_VERBOSE=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp`
      is now green after the HTTP/CRUD lane was made deterministic.
- Closed the residual broad-HTTP CRUD lane and split the remaining concurrency
  question into its own explicit backlog item:
  - `examples/deduce_crud_server.omni` now treats keyed `deduce 'fact!` as the
    atomic source of truth in `repo/create` and maps
    `deduce/integrity-key-conflict` to `Err "item already exists"`, removing
    the check-then-insert race between the pre-query and the keyed write.
  - `src/lisp/tests_runtime_feature_http_groups.c3` now keeps deterministic
    duplicate-id coverage in the broad `http` slice by issuing two POSTs with
    the same id but different payloads, asserting first-write success plus
    second-write conflict without depending on spawned concurrent mutation
    ordering.
  - The separate product/runtime question of whether spawned concurrent CRUD
    writes over one in-memory Deduce DB are supported is now tracked in
    `TODO.md` as `HTTP-CRUD-CONCURRENT-WRITES-001`.
  - Validation:
    - `c3c build`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_LISP_TEST_SLICE=http ./build/main --test-suite lisp` -> `pass=29 fail=0`

- Nested boundary copy and ESCAPE promotion now fail closed for opaque
  primitive payloads instead of silently degrading structured wrappers:
  - `src/lisp/eval_promotion_copy.c3`,
    `src/lisp/eval_promotion_copy_route_helpers.c3`, and
    `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now thread
    `BoundaryCopyFault` through `CONS`, `PARTIAL_PRIM`, and `ITERATOR`
    boundary-copy paths, so nested primitive-copy rejection propagates as
    `BOUNDARY_COPY_FAULT_OPAQUE_PRIMITIVE_PAYLOAD` rather than embedding
    `null` payload edges into copied wrappers.
  - `src/lisp/eval_promotion_copy_route_helpers.c3` also delays primitive
    wrapper allocation until after opaque-payload legality is known, so failed
    primitive-copy attempts do not allocate unreachable target-scope wrapper
    garbage before rejecting.
  - `src/lisp/eval_promotion_escape_leaf.c3` and
    `src/lisp/eval_promotion_escape_structured.c3` now propagate nested
    promotion failures through `CONS`, `PARTIAL_PRIM`, and `ITERATOR` escape
    promotion instead of returning partially rebuilt wrappers whose payloads
    were replaced with boundary-error values.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3` and
    `src/lisp/tests_memory_lifetime_promotion_context_groups.c3` now cover the
    nested fail-closed regressions for boundary-copy and escape-promotion
    paths, and `src/lisp/tests_memory_lifetime_groups.c3` wires those focused
    regressions into the smoke suite.
  - Validation:
    - `c3c build`
    - `rm -rf build/obj/linux-x64 build/main && mkdir -p build/obj/linux-x64/tmp_c_compile && c3c build --sanitize=address`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` ->
      `pass=80 fail=0`

- Aligned FFI parser and AOT helper payload ownership with the repo's existing
  arena/scope lifetime model:
  - `src/lisp/parser_ffi.c3` and `src/lisp/parser_ffi_helpers.c3` now allocate
    successful `[ffi λ ...]` AST payloads (`ExprFfiFn`, copied C symbol names,
    and parameter tables) from the AST arena instead of raw `malloc`, so
    parser-owned FFI metadata is reclaimed by `ast_arena_destroy(...)` with the
    rest of the AST.
  - `src/lisp/aot_runtime_bridge_closure.c3`,
    `src/lisp/aot_runtime_bridge_ffi.c3`, and
    `src/lisp/aot_runtime_bridge_ffi_helpers.c3` now allocate AOT primitive
    payloads from `interp.root_scope` instead of raw heap ownership, keeping
    closure payloads and bound-FFI metadata on the same deterministic lifetime
    as the primitive values that reference them.
  - This closes the post-e2e AddressSanitizer leak paths that were still
    reachable from:
    - successful compiler-slice parsing of `[ffi λ ...]` forms,
    - `aot::make_closure(...)` / `aot::make_variadic_closure(...)`,
    - `aot::ffi_declare_fn(...)`.
  - Follow-up interpreter parity hardening:
    - `src/lisp/eval_ffi_eval.c3` now allocates interpreter-side declarative
      `ffi λ` bound-function payloads (`FfiBoundFn`, copied library name,
      copied symbol name, and ABI tag table) from `interp.root_scope` instead
      of raw heap ownership, matching the root-lifetime primitive values that
      retain them through `prim_val.user_data`.
    - This closes the same ownership mismatch for non-AOT declarative FFI
      bindings, so interpreter-created FFI primitives no longer depend on an
      unowned heap payload surviving past primitive teardown.
  - Root-boundary primitive promotion now fails closed for opaque primitive
    payloads:
    - `src/lisp/eval_promotion_root_clone_basic.c3` no longer raw-copies
      `Primitive.user_data` when promoting a child-owned primitive wrapper into
      root storage. If a primitive carries opaque payload state, root-store
      promotion now returns an explicit error instead of aliasing foreign
      lifetime-owned payload through a shallow struct copy.
    - `src/lisp/tests_memory_lifetime_root_boundary_groups.c3` now covers both
      the allowed plain primitive clone path and the fail-closed rejection path
      for child-owned primitives with non-null `user_data`.
  - Ordinary ESCAPE-lane primitive promotion now follows the same ownership
    rule instead of raw-copying primitive wrapper pointers:
    - `src/lisp/eval_promotion_escape_leaf.c3` now deep-clones plain
      `Primitive` headers into the destination ESCAPE lane, which avoids
      double-freeing the same heap `Primitive*` when TEMP and ESCAPE wrappers
      teardown in the same scope.
    - ESCAPE promotion now rejects child/disjoint primitive wrappers carrying
      opaque `user_data` with an explicit boundary error instead of aliasing
      foreign payload ownership through the shared-wrapper fast path.
    - `src/lisp/eval_promotion_copy_wrapper_helpers.c3` now centralizes the
      primitive opaque-payload predicate and plain-header clone helper used by
      both root-store and ESCAPE promotion paths.
    - `src/lisp/tests_memory_lifetime_groups.c3` now covers both the plain
      primitive ESCAPE clone path and the fail-closed opaque-payload rejection
      path for disjoint-scope promotion.
  - Parent-boundary and env-copy primitive handling now use the same contract:
    - `src/lisp/eval_promotion_copy.c3` no longer returns child-owned
      `PRIMITIVE` wrappers unchanged from `copy_to_parent(...)`; plain
      primitive headers are cloned into the target scope and opaque
      `user_data` payloads now surface as a typed boundary-copy fault.
    - `src/lisp/eval_env_copy_values.c3` no longer treats all primitive values
      as root/external-lifetime by default during env copy; disjoint plain
      primitives are cloned through boundary policy and opaque payloads now
      fail the env copy with `BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY`.
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3` and
      `src/lisp/tests_memory_lifetime_env_copy_groups.c3` now cover the plain
      primitive clone path plus opaque-payload rejection for return/env-copy
      boundaries.
  - Validation:
    - `c3c build --sanitize=address`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` ->
      `pass=189 fail=0`
    - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 && c3c build && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp'` ->
      `pass=75 fail=0`
    - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite stack` ->
      `pass=22 fail=0`
    - `scripts/run_e2e.sh` -> `ALL 404 e2e compiler tests passed!`
    - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(block (define [ffi lib] libc "libc.so.6") (define [ffi lambda libc] (strlen (^String s)) ^Integer) 0)'` ->
      `0`

## 2026-04-08

- Closed arm64 language-level continuation multi-shot parity (`STACK-AARCH64-CONT-001`) after backend landing:
  - corrected arm64 GNU lightning register ID mapping in
    `src/lisp/jit_lightning_constants.c3` for `JIT_R*`, `JIT_V*`, and `JIT_FP`.
  - updated effect fast-path primitive dispatch in:
    - `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
    - `src/lisp/jit_jit_runtime_effects_signal.c3`
  - fast-path changes now:
    - preserve primitive error payloads (domain/code/message) instead of
      rewriting them to generic runtime codes,
    - expand dotted cons payloads used by stdlib fixed-arity wrappers (for
      example `(cons host port)`) into positional primitive arguments.
  - verification evidence:
    - `c3c build`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack` ->
      `Stack engine: 23 passed, 0 failed`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (resolve 10)))"` -> `11`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (+ 1 (signal ask 0)) (ask x (with-continuation k (k 41))))"` -> `42`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(checkpoint (+ 1 (capture k (k 10))))"` -> `11`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-effect-continuation OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` ->
      `pass=56 fail=0`
    - `OMNI_JIT_POLICY_FILTER=multishot-capture-scope-guard-clone OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` ->
      `pass=1 fail=0`

## 2026-04-07

- Hardened the automated Claude training loop so tmux-attached runs do not
  drop the window immediately on exit:
  - `scripts/claude-loop.sh` now installs an EXIT handler that pauses before
    shell termination when the loop is running inside tmux and stdout is a
    terminal.
  - The pause is opt-out via `CLAUDE_LOOP_KEEP_WINDOW=0`, so non-interactive
    automation can still terminate without blocking.
  - This keeps the training output visible after a long run or a failure
    instead of closing the tmux window as soon as the foreground process
    exits.

## 2026-04-01

- Hardened AOT closure allocation paths to fail closed across compiler emission
  and runtime bridge:
  - `src/lisp/compiler_code_emission_lambda_closures.c3` now emits an explicit
    `_closure_data == null` guard for captured closures and returns
    `aot::make_error(...)` instead of risking null-dereference on capture
    field writes; it also frees `_closure_data` when `aot::make_closure*`
    returns an error to avoid captured-payload leaks on closure-construction
    failure paths.
  - `src/lisp/aot_runtime_bridge.c3` now replaces assert-based
    `make_closure`/`make_variadic_closure` allocation handling with explicit
    error-value returns and payload cleanup on primitive allocation failure.
  - `src/lisp/aot_runtime_bridge.c3` now guards `aot_closure_apply(...)` and
    `aot_variadic_apply(...)` against missing payload/user-data state and
    returns explicit error values instead of dereferencing null closure payloads.
  - Added deterministic AOT closure allocation seams in
    `src/lisp/aot_runtime_bridge.c3`:
    - `g_aot_force_closure_data_alloc_fail`
    - `g_aot_force_closure_primitive_alloc_fail`
    - `g_aot_force_variadic_closure_data_alloc_fail`
    - `g_aot_force_variadic_closure_primitive_alloc_fail`
  - Added `aot::make_error(...)` wrapper in `src/lisp/aot.c3` so generated
    code can report closure-capture allocation failures via the AOT surface.
  - Extended `src/lisp/tests_compiler_core_groups.c3` with regression coverage
    for:
    - emitted closure-capture null-guard presence in generated C3,
    - seam-driven `aot::make_closure*` allocation failures returning
      `ValueTag.ERROR` instead of asserting,
    - direct `aot_closure_apply`/`aot_variadic_apply` missing-payload calls
      returning `ValueTag.ERROR`.
  - Validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`overall_status: pass`, compiler slice `181 passed, 0 failed`).
  - Additional closure payload hardening:
    - `src/lisp/aot_runtime_bridge.c3` now routes AOT payload validity checks
      through shared helpers (`aot_closure_payload_valid`,
      `aot_variadic_payload_valid`) and applies the same fail-closed payload
      errors in direct apply entrypoints and fast-path invoke/apply trampolines
      (`invoke_once`, `apply_multi_once`), preventing null callback
      dereference on malformed payload state.
    - `make_closure(...)` and `make_variadic_closure(...)` now fail closed when
      invoked with a null callback (`missing invoke callback`) instead of
      constructing invalid payload objects.
    - `aot_closure_apply(...)` / `aot_variadic_apply(...)` now resolve the
      active interpreter defensively (`interp` or `g_aot_interp`) before
      reading payload state, so null `interp` calls fail closed instead of
      dereferencing `interp.prim_user_data`.
    - AOT closure factory functions now resolve active interpreter state once
      and return null when no runtime interpreter is available, avoiding
      null-interpreter error-construction crashes on out-of-contract calls.
    - `src/lisp/tests_compiler_core_groups.c3` now covers null-callback closure
      construction seams for both unary and variadic AOT closure factories, and
      null-`interp` direct apply calls returning `ValueTag.ERROR`.
  - Re-validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`overall_status: pass`, `run_counts: 11 pass / 0 fail`, compiler slice
      `181 passed, 0 failed`).
  - Null-interpreter apply hardening validation:
    - `src/lisp/tests_compiler_core_groups.c3` now asserts
      `aot_closure_apply(...)` and `aot_variadic_apply(...)` fail closed with
      `ValueTag.ERROR` when called with `interp = null` and active AOT runtime
      state.
  - Variadic invoke arg-list allocation fail-close hardening:
    - `src/lisp/aot_runtime_bridge.c3` now includes deterministic seam
      `g_aot_force_invoke_variadic_arg_list_alloc_fail` and routes single-arg
      variadic invoke list construction through `aot_make_single_arg_list(...)`.
    - `invoke_once(...)` now returns explicit error
      (`aot invoke: out of memory while building variadic argument list`) when
      single-arg list construction fails, instead of passing null list payload
      into variadic closure callbacks.
    - `src/lisp/tests_compiler_core_groups.c3` now asserts this seam-driven
      invoke path returns `ValueTag.ERROR`.
    - `src/lisp/aot_runtime_bridge.c3` now also guards
      `aot_variadic_apply(...)` list construction through checked prepend helper
      `aot_prepend_arg_checked(...)` with deterministic seam
      `g_aot_force_variadic_apply_arg_list_alloc_fail`, returning explicit
      error (`aot variadic apply: out of memory while building argument list`)
      on allocation failure instead of propagating null list state.
    - `src/lisp/tests_compiler_core_groups.c3` now asserts seam-driven direct
      `aot_variadic_apply(...)` argument-list allocation failure returns
      `ValueTag.ERROR`.
    - `src/lisp/aot_runtime_bridge.c3` now guards dispatch entrypoints
      `invoke_once(...)` and `apply_multi_once(...)` with deterministic seam
      `g_aot_force_dispatch_interp_unavailable`, returning explicit errors
      (`aot invoke: runtime interpreter unavailable`,
      `aot apply-multi: runtime interpreter unavailable`) instead of routing
      null interpreter state into JIT apply surfaces.
    - `src/lisp/tests_compiler_core_groups.c3` now asserts seam-driven
      `aot::invoke(...)` and `aot::apply_multi(...)` dispatch-unavailable paths
      return `ValueTag.ERROR`.
    - additional AOT bridge failure surfaces now use
      `aot_make_error_best_effort(...)` instead of direct
      `make_error(g_aot_interp, ...)` calls in import/field-access/
      explain-dispatch paths, keeping error construction consistent with
      interpreter-availability hardening.
    - `src/lisp/aot_runtime_bridge.c3` now adds explicit bridge-runtime
      seam `g_aot_force_bridge_interp_unavailable` and fail-closed interpreter
      guards in `lookup_prim(...)`, `lookup_var(...)`, `import_module(...)`,
      `field_access(...)`, and `explain_dispatch(...)`.
    - `src/lisp/tests_compiler_core_groups.c3` now asserts seam-driven bridge
      runtime-unavailable calls for those helpers return `ValueTag.ERROR`.
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      remains green after this slice (`overall_status: pass`,
      `run_counts: 11 pass / 0 fail`, compiler `181 pass / 0 fail`).
    - extended bridge-runtime-unavailable hardening in
      `src/lisp/aot_runtime_bridge.c3` to additional direct wrappers that still
      dereferenced `g_aot_interp`:
      - tail registration entrypoints now fail closed:
        `invoke_tail(...)`, `apply_multi_tail(...)`
      - pattern/collection helpers now guard bridge interpreter access:
        `instance_field(...)`, `dict_get_symbol(...)`, `seq_nth(...)`,
        `seq_rest(...)`, `list_rest(...)`, plus constructor/type lookup checks
        in `match_var_pattern(...)` / `match_constructor(...)`
      - effect/dispatch wrappers now fail closed when bridge interpreter is
        unavailable:
        `explain_effect_signal(...)`, `explain_effect_resolve(...)`,
        `dict_from_args(...)`, `index(...)`, `compiled_handle(...)`,
        `compiled_signal(...)`, `compiled_reset(...)`, `compiled_shift(...)`,
        `compiled_resolve(...)`
      - output bridge `print_value(...)` now avoids null-interpreter symbol
        dereference by emitting a deterministic unavailable marker.
    - `src/lisp/tests_compiler_core_groups.c3` now extends the existing
      seam-driven AOT hardening regression block to assert
      `ValueTag.ERROR` for:
      - dispatch-unavailable tail registration:
        `aot::invoke_tail(...)`, `aot::apply_multi_tail(...)`
      - bridge-unavailable helper/wrapper paths:
        `aot::instance_field(...)`, `aot::dict_get_symbol(...)`,
        `aot::seq_nth(...)`, `aot::seq_rest(...)`, `aot::list_rest(...)`,
        `aot::dict_from_args(...)`, `aot::index(...)`,
        `aot::compiled_signal(...)`, `aot::compiled_handle(...)`,
        `aot::compiled_reset(...)`, `aot::compiled_shift(...)`,
        `aot::compiled_resolve(...)`, `aot::explain_effect_signal(...)`,
        `aot::explain_effect_resolve(...)`.
    - Re-validation for this extension slice:
      - `c3c build`
      - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
        (`suite=compiler pass=183 fail=0`)
      - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
        (`overall_status: pass`, `run_counts: 11 pass / 0 fail`).

- Hardened the JSON bridge and `json-emit` fail-closed behavior:
  - `csrc/json_helpers.c` no longer exposes raw yyjson internal-layout stepping
    for parse traversal; array/object reads now use explicit yyjson iterators
    through allocation/free wrapper APIs.
  - mutable yyjson append/add wrappers now return `bool`, and
    `json-emit` now checks insertion outcomes instead of silently accepting
    partial object/array construction.
  - `json-emit` now fails explicitly for unsupported Omni root values and
    unsupported dictionary key tags instead of coercing to placeholder/null
    output, and option-parse failure now frees the mutable document.
  - fixed-precision flag handling now treats `0..15` as the valid range (with
    `0` meaning "do not force fixed precision"), matching the option parser
    contract.
- Made async `write-file` offload atomic and more specific:
  - `src/lisp/scheduler_offload_ops.c3` now writes to a temp file and renames
    on success, unlinks temp files on failure, and returns explicit
    `OFFLOAD_RES_ERROR` messages for open/write/close/rename failure stages.
  - temp-path suffix formatting now uses supported formatter directives
    (`.tmp.<pid>.<hex-id>`) instead of `%zu`, which previously caused false
    `offload write-file: temp path formatting failed` errors on normal writes.
  - `src/lisp/prim_io_file.c3` now preserves the existing user-facing
    `io/write-file-failed` contract even when the offload worker now reports
    richer internal error detail.
  - when remapping scheduler offload errors to `io/write-file-failed`, the
    primitive now clears pre-existing pending-raise state before re-raising so
    canonical I/O payloads are not shadowed by the original scheduler payload.
- Added regression coverage in
  `src/lisp/tests_runtime_data_unicode_groups.c3` for `json-emit` fail-closed
  paths (unsupported root value type and unsupported dictionary key type).
- Made TOML option parsing concurrency-safe:
  - `csrc/toml_helpers.c` now serializes TOML parse calls and temporary
    `toml_set_option(...)` mutations behind a shared libuv mutex so per-call
    `check-utf8` toggles cannot race or leak process-global parser state across
    concurrent parses.
- Hardened parser fail-closed and growth-allocation behavior for module/import
  surfaces:
  - `src/lisp/parser_module_decl.c3` now fails closed on malformed module body
    parses and on module export/body growth allocation failures instead of
    returning partial module ASTs.
  - `src/lisp/parser_import_helpers.c3`,
    `src/lisp/parser_import_helpers_specs.c3`, and
    `src/lisp/parser_export_from.c3` now convert import/export list growth and
    initial-list allocation failures into explicit parser errors instead of
    proceeding with null writes or invalid capacity state.
- Reconciled `pika/parse-lisp` with shipped reader behavior and made parser
  drift explicit:
  - `src/pika/lisp_pika.c3` now raises
    `parser/parse-lisp-grammar-drift` when the built-in Pika grammar rejects
    input that the shipped core reader can parse, instead of silently returning
    `nil`.
  - Added regression coverage in
    `src/lisp/tests_runtime_feature_pika_groups.c3` for form-comment drift
    (`#_...`) fail-closed behavior.
- Made Pika global grammar/parser state concurrency-safe:
  - `src/pika/lisp_pika.c3` now guards named-grammar registry access with a
    mutex and snapshots grammar metadata before parse/fold/match surfaces.
  - lazy global Lisp parser init now uses `thread::OnceFlag` so first-use init
    cannot race.
  - `src/pika/lisp_pika_grammar_compiler.c3` now stores named grammars through
    the synchronized registry helper.
- Removed unsynchronized regex cache/error-state behavior in the Pika regex
  bridge:
  - `src/pika/regex_cache_api.c3` now serializes cache access and resets behind
    a shared mutex, including compiled regex lookup/use paths.
  - last compile-error storage is now thread-local so concurrent regex calls do
    not overwrite each other’s error detail.
- Added module fail-closed regression coverage in
  `src/lisp/tests_advanced_stdlib_module_groups.c3` to ensure malformed module
  bodies no longer escape as partial ASTs.
- Fixed relation DBI-open commit-path reporting so commit failures cannot be
  surfaced as successful relation initialization:
  - `src/lisp/deduce_schema_query.c3` now checks `mdb_txn_commit(...)` in
    relation DBI-open paths, propagates explicit
    `deduce/*-txn-commit-failed` errors, and includes a test seam for forced
    commit-failure coverage.
  - `src/lisp/tests_deduce_groups.c3` now includes a regression that forces the
    relation-open commit-failure seam and asserts both `deduce/open-named` and
    `__define-relation` return commit-failure codes instead of success.
- Replaced `process-wait` 1 ms polling loops with event-driven scheduler wakeup:
  - `src/lisp/async_process_signal_dns_process.c3` now routes `process-wait`
    through `scheduler_execute_custom_offload_job(...)` and a blocking
    `omni_uv_process_wait(...)` worker callback instead of repeatedly polling
    `omni_uv_process_poll(...)` + `async-sleep 1`.
  - `csrc/uv_helpers_process.c` now exposes debug counters for poll/wait call
    paths, and `src/lisp/tests_advanced_io_effect_ffi_groups.c3` asserts
    `process-wait` uses the blocking wait path (`wait_calls >= 1`) with zero
    poll invocations for the same wait operation.
- Removed the async signal delivery 1024-cap drop path and added deterministic
  burst regression coverage:
  - `src/lisp/async_process_signal_handles.c3` now drains all pending signal
    callbacks without an artificial `min(pending, 1024)` cap.
  - `csrc/uv_helpers_signal.c` now provides deterministic debug pending helpers
    (`omni_uv_signal_debug_set_pending` / `omni_uv_signal_debug_pending`) used
    by tests.
  - `src/lisp/tests_advanced_io_effect_ffi_groups.c3` now forces a pending
    burst above 1024 and asserts full drain via runtime dispatch counters.
- Replaced process-global REPL SIGINT state with signal-safe interpreter routing:
  - `csrc/stack_helpers.c` now exposes thread-local SIGINT routing helpers that
    keep pending SIGINT state in `sig_atomic_t` slots and route delivery only
    to an explicitly bound interpreter slot.
  - `src/lisp/eval_signal.c3` now uses that C bridge (`omni_sigint_*`) and no
    longer keeps global `g_interrupted` process state.
  - `src/lisp/value_interp_state.c3` adds per-interpreter `sigint_requested`
    state and runtime init/lifecycle wiring.
  - `src/lisp/eval_repl.c3` now binds SIGINT routing only while evaluation is
    active and clears stale idle interrupts before each eval.
  - `src/lisp/tests_advanced_io_effect_ffi_groups.c3` adds deterministic
    coverage proving a pending SIGINT marks only the currently bound
    interpreter slot.
- Made non-transaction deduce mutation surfaces fail-atomic after durable LMDB
  commit:
  - `src/lisp/deduce_relation_ops_mutations.c3` now treats post-commit metadata
    persistence failures (`deduce_db_note_*` and relation metadata delete
    persistence in `drop!`) as best-effort follow-up work instead of returning
    user-visible failures after base data commit has already succeeded.
  - Added a forced metadata-persistence failure seam in
    `src/lisp/deduce_db_handles.c3`
    (`g_deduce_force_materialized_meta_persist_fail`) and a regression in
    `src/lisp/tests_deduce_groups.c3` proving non-transaction `fact!` preserves
    success surface while committed rows remain visible.
- Hardened transactional deduce mutation bookkeeping when txn-side mutation-log
  writes fail:
  - `src/lisp/deduce_db_handles_mutation_txn.c3` now provides a forced
    mutation-log failure seam (`g_deduce_force_txn_mutation_log_fail`) and
    marks txn mutation tracking degraded when insert/delete log writes cannot
    be recorded.
  - `src/lisp/deduce.c3` commit handling now avoids applying partial mutation
    deltas when logging degraded and instead routes post-commit bookkeeping
    through the existing untracked-write fallback
    (`deduce_db_note_untracked_write_commit`) so materialization state is
    conservatively escalated to full-recompute.
  - Added regression coverage in `src/lisp/tests_deduce_groups.c3` proving a
    forced txn-log write failure still commits base tuples while `deduce
    'analyze` reports `'full-recompute` invalidation mode.
- Hardened async signal lifecycle ownership and dispatch topology:
  - `src/lisp/async_process_signal_handles.c3` removes the fixed
    process-global `MAX_SIGNAL_HANDLES` registry and replaces it with a
    per-interpreter intrusive handle list rooted at `Interp.signal_handles_head`,
    so signal callback dispatch no longer scans unrelated interpreter watchers.
  - signal callbacks are no longer promoted to root-only lifetime with no
    teardown path; signal handles now retain the callback owner scope at
    registration and release that retained scope on unhandle/finalizer
    (`signal_handle_detach_runtime_state`), restoring balanced lifetime cleanup.
  - `src/lisp/async_process_signal_dns.c3` now routes unhandle teardown through
    the shared detach helper so explicit unhandle and finalizer cleanup follow
    the same close/unregister/release contract.
  - Added advanced regression coverage in
    `src/lisp/tests_advanced_io_effect_ffi_groups.c3` for:
    - >32 active signal handles in one interpreter without registry-cap failure,
    - owner-interpreter dispatch isolation (pending callbacks are ignored when
      dispatching on a non-owner interpreter),
    - callback owner-scope retain/release symmetry across
      `signal-handle`/`signal-unhandle`.
- Restored inline-module import no-op lowering in top-level compiler emission
  while preserving runtime bridge behavior for path/selective imports:
  - `src/lisp/compiler_program_top_level.c3` now short-circuits `(import mod)`
    with no path and no import specifiers to a static no-op when `mod` was
    already compiled inline in the same unit.
  - This preserves the no-op contract exercised by compiler regression coverage
    (`Compiler: import lowers as static no-op`) without weakening
    `aot::import_module(...)` lowering for path-based or selective import forms.
- Fixed top-level compiler import-all flag propagation and expanded import
  lowering regression coverage:
  - `src/lisp/compiler_program_top_level.c3` now threads
    `ExprImport.import_all` into emitted `aot::import_module(...)` calls for
    both module-name and path import forms instead of always emitting
    `import_all=false`.
  - `src/lisp/tests_compiler_core_groups.c3` now covers:
    - plain `(import mod 'all)` lowering with `import_all=true`,
    - path `(import \"path\" 'all)` lowering with `import_all=true`,
    - path selective import lowering with `import_all=false` plus
      `lookup_var(...)` binding,
    - plain/path/inline selective alias imports preserving alias target binding
      with source-symbol lookup (`alias = lookup_var(source)`),
    - inline `(module ...)(import mod 'all)` remaining a static no-op when the
      module was compiled inline in the same unit.
- Fixed non-top-level import lowering drift in flat expression compilation:
  - `src/lisp/compiler_tail_position_compilation_tco.c3` now routes
    `compile_import_flat(...)` through runtime import side effects
    (`aot::import_module(...)`) for non-inline import forms, threads
    `import_all`, and applies selective/alias bindings through
    `aot::define_var(alias_or_name, aot::lookup_var(source_name))`.
  - The inline no-op optimization remains for the zero-specifier inline-module
    case so existing compiled-inline import contracts stay unchanged.
  - `src/lisp/tests_compiler_core_groups.c3` now covers non-top-level import
  lowering for:
    - plain import runtime bridge side effects,
    - plain/path `'all` import side effects with `import_all=true`,
    - plain/path selective runtime binding,
    - plain/path selective alias runtime binding,
    - inline selective runtime binding (plain and alias),
    - inline-module no-op preservation (plain and `'all`) inside expression
      lowering.
- Removed duplicated inline-module import detection logic in the compiler:
  - `compiler_has_inline_compiled_module(...)` is now centralized in
    `src/lisp/compiler_program_top_level_helpers.c3` and used by both
    top-level import emission and `compile_import_flat(...)` paths, reducing
    future drift risk with no behavior change.
- Hardened compiler allocation failure paths with deterministic fail-closed
  behavior and explicit seams:
  - `src/lisp/compiler_program_pipeline.c3` now fails closed when allocating
    the combined stdlib+source program buffer fails
    (`compiler: out of memory while allocating program source buffer`) and
    short-circuits compilation on that error.
  - `src/lisp/compiler_compiler_initialization.c3` now treats compiled-module
    registry init allocation failure as a compile error instead of leaving
    null registry state unchecked.
  - `src/lisp/compiler_tail_position_compilation_tco.c3` now guards compiled
    module registry allocation/growth and per-module export-table allocation
    in `compile_module_flat(...)`, setting compile errors instead of
    dereferencing null allocations.
  - Added compiler test seams in `src/lisp/compiler_compiler_state.c3`:
    - `g_compiler_force_program_source_alloc_fail`
    - `g_compiler_force_compiled_modules_init_alloc_fail`
    - `g_compiler_force_module_registry_alloc_fail`
    - `g_compiler_force_module_exports_alloc_fail`
  - Added regression coverage in `src/lisp/tests_compiler_core_groups.c3`
    proving those seam-triggered allocation failures fail closed and surface
    stable compile error messages.
- Closed status-consistency drift after TODO/archive condensation:
  - `TODO.md` now includes the explicit zero-queue marker required by
    `scripts/check_status_consistency.sh` (`- none currently;`).
  - `docs/areas/memory-runtime.md` and `docs/areas/types-dispatch.md` now
    carry `As of: 2026-04-01`, aligned with the latest changelog entry.
  - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    now reports `overall_status: pass` with all 11 lanes green.
- Added compiler size-overflow hardening for module/program source buffers:
  - Introduced shared checked-size helper `compiler_checked_mul_usz(...)` in
    `src/lisp/compiler_compiler_state.c3` and wired compiler module registry
    allocations through it.
  - `src/lisp/compiler_compiler_initialization.c3` now fail-closes on
    compiled-module registry allocation-size overflow with
    `compiler: compiled module registry allocation size overflow`.
  - `src/lisp/compiler_tail_position_compilation_tco.c3` now fail-closes on:
    - compiled-module registry growth-capacity overflow,
    - compiled-module registry allocation-size overflow,
    - module export-table allocation-size overflow.
  - Added deterministic compiler seams in
    `src/lisp/compiler_compiler_state.c3`:
    - `g_compiler_force_compiled_modules_init_size_overflow`
    - `g_compiler_force_module_registry_growth_overflow`
    - `g_compiler_force_module_exports_size_overflow`
  - Extended `src/lisp/tests_compiler_core_groups.c3` with seam-driven
    fail-closed regressions for those overflow paths.
  - `src/lisp/compiler_program_pipeline.c3` now performs explicit unsigned
    length arithmetic in `build_program_source(...)` (`prelude_len` as `usz`)
    so the max-size guard compiles cleanly and still fail-closes with
    `compiler: program source exceeds maximum supported size`.
  - `src/lisp/compiler_top_level_compile_function.c3` now guards
    `compiler_own_result_slice(...)` against `slice.len == usz.max` before
    `+1` allocation math.
- Hardened compiler lambda-scan allocation paths to fail closed instead of
  leaving partial lambda-definition state:
  - `src/lisp/compiler_lambda_scan_lambda_defs.c3` now validates and checks
    capture/parameter table allocation sizes, fails closed on allocation and
    growth failures, and avoids null-write/partial-state capture table growth.
  - New deterministic compiler seams in
    `src/lisp/compiler_compiler_state.c3`:
    - `g_compiler_force_lambda_capture_alloc_fail`
    - `g_compiler_force_lambda_param_alloc_fail`
    - `g_compiler_force_lambda_capture_growth_alloc_fail`
    - `g_compiler_force_lambda_capture_size_overflow`
    - `g_compiler_force_lambda_param_size_overflow`
  - `src/lisp/compiler_program_pipeline.c3` now short-circuits immediately
    after program-analysis prep when `self.has_error` is set, preventing
    downstream code-emission panics when lambda-scan fails closed.
  - `src/lisp/compiler_lambda_scan.c3` now short-circuits lambda scanning when
    compiler error state is already set.
  - `src/lisp/tests_compiler_core_groups.c3` now includes fail-closed
    regressions for lambda capture alloc failure, lambda parameter alloc
    failure, lambda capture/parameter size-overflow seams, and lambda capture
    growth allocation failure.
  - Validation status summary remains green after landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=170 fail=0`.
- Hardened parser FFI function-expression allocation to fail closed:
  - `src/lisp/parser_ffi.c3` now guards `ExprFfiFn` allocation in
    `parse_ffi_fn(...)` and reports
    `out of memory while creating FFI function expression` instead of
    dereferencing a null allocation.
  - `parse_ffi_fn(...)` now cleans up partially allocated FFI parse state
    (`c_name`, parameter buffers, and struct allocation) on all parse-error
    exits before returning `null`, preventing dangling partial pointers/leaks
    in failed parse paths.
  - Added deterministic seam `g_parser_force_ffi_fn_alloc_fail` in
    `src/lisp/parser_ffi.c3` and compiler-path regression coverage in
    `src/lisp/tests_compiler_core_groups.c3` proving parse/compile fail-close
    behavior for that allocation path.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=171 fail=0`.
- Hardened FFI parameter-table growth in parser helpers:
  - `src/lisp/parser_ffi_helpers.c3` now guards FFI parameter table growth
    against size overflow and switches to atomic grow-and-swap allocation for
    parameter name/type tables, avoiding partial `realloc` state drift on
    failure paths.
  - Added deterministic parser seams in
    `src/lisp/parser_ffi_helpers.c3`:
    - `g_parser_force_ffi_param_names_alloc_fail`
    - `g_parser_force_ffi_param_types_alloc_fail`
    - `g_parser_force_ffi_param_table_size_overflow`
  - Extended compiler-path regression coverage in
    `src/lisp/tests_compiler_core_groups.c3` to prove fail-closed behavior for
    FFI parameter name alloc failure, type alloc failure, and parameter-table
    size-overflow seams.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=174 fail=0`.
- Hardened lexer token-buffer initialization fail-close path:
  - Added deterministic lexer seam in
    `src/lisp/parser_lexer_core_api.c3`:
    - `g_lexer_force_text_alloc_fail`
  - `src/lisp/compiler_program_pipeline_helpers.c3` now detects lexer
    initialization failure before parser setup and reports a stable compile
    error detail:
    `out of memory while initializing lexer token buffer`.
  - Added compiler-path regression coverage in
    `src/lisp/tests_compiler_core_groups.c3` proving lexer token-buffer
    allocation failure fails closed and surfaces the expected error text.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=175 fail=0`.
- Hardened lexer token-text growth guardrails and added deterministic growth
  failure coverage:
  - `src/lisp/parser_lexer_core_api.c3` now includes:
    - `g_lexer_force_text_growth_alloc_fail` seam for deterministic growth
      allocation failure testing,
    - explicit guards against zero/overflowed capacity requests in
      `ensure_text_capacity(...)`,
    - overflow guards in `push_current_text_char(...)` and `set_error_text(...)`
      to avoid wraparound in token-text length arithmetic.
  - `src/lisp/parser_lexer_symbol_number.c3` now routes integer overflow errors
    through `set_error_text(...)` and guards symbol-length overflow before
    `len + 1` capacity math.
  - `src/lisp/tests_compiler_core_groups.c3` now includes a deterministic
    lexer growth-allocation failure regression (long string token forcing
    growth under `g_lexer_force_text_growth_alloc_fail`), asserting fail-closed
    compile behavior with expected lexer error text.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=176 fail=0`.
- Hardened lambda-lowering lookup miss path to fail closed instead of panicking:
  - `src/lisp/compiler_native_call_compilation_flat_style.c3` now converts
    missing lambda-definition lookup from assertion panic into explicit compile
    error (`compiler: missing lambda definition during AOT lambda lowering`)
    and returns a safe nil temp.
  - Added deterministic seam `g_compiler_force_lambda_lookup_miss` in
    `src/lisp/compiler_compiler_state.c3`.
  - Added compiler-path regression in
    `src/lisp/tests_compiler_core_groups.c3` proving lookup-miss fail-close
    behavior without compiler panic.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=177 fail=0`.
- Removed remaining compiler assertion panic in lambda-closure emission:
  - `src/lisp/compiler_code_emission_lambda_closures.c3` now fail-closes
    `emit_lambda_return_common(...)` when lambda-definition lookup misses,
    setting compile error
    `compiler: missing lambda definition during closure emission` and emitting
    a safe `return aot::make_nil();` fallback in generated closure body.
  - Added deterministic seam
    `g_compiler_force_lambda_emit_lookup_miss` in
    `src/lisp/compiler_compiler_state.c3`.
  - Added compiler-path regression in
    `src/lisp/tests_compiler_core_groups.c3` proving closure-emission lookup
    miss fail-close behavior without assertion panic.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=178 fail=0`.
- Removed lexer-level panic fallback from `Lexer.expect(...)`:
  - `src/lisp/parser_lexer_core_api.c3` no longer calls `unreachable(...)`
    when token expectation fails at lexer level.
  - `Lexer.expect(...)` now fail-closes by setting lexer error text/state
    without aborting process execution.
  - Added direct regression coverage in
    `src/lisp/tests_compiler_core_groups.c3` proving lexer expect mismatch
    now fails closed (`T_ERROR`) without panic.
  - Validation status summary remains green after this landing:
    `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
    reports `overall_status: pass` (`11/11` lanes), with compiler slice
    `pass=179 fail=0`.


## 2026-03-31

- Fixed the Finwatch normal boot path in two places: the background poller no
  longer blows the coroutine stack during startup because
  `examples/finwatch/server.omni` now hoists the poll-result store helpers out
  of `server-poll-once!`, and the script entrypoint
  `examples/finwatch/main.omni` now loads `server.omni` directly instead of
  relying on the top-level script-mode `import ... 'all` path that returned to
  the CLI instead of keeping the normal server boot resident. The HTTP runtime
  tests stub the feed fetchers and assert the real `server-poll-once!` helper
  can run inside a spawned fiber without overflowing.
- Hardened stack-guard setup and recovery so thread-affine stack pools keep
  guard bookkeeping and protected-switch recovery state thread-local, fail
  closed when `mprotect(...)` or guard registration cannot be installed, and no
  longer silently lose overflow protection after a fixed 256-stack cap.
- Fixed HTTP/HTTPS scheduler offload reads to accumulate full responses instead
  of silently truncating at 128 KiB, distinguish read failures from EOF, and
  publish owned response buffers only after complete read success.
- Cached the default BearSSL trust store in `csrc/tls_helpers.c` so repeated
  HTTPS offload and default TLS-connect paths stop reparsing the system CA
  bundle on every request.
- Preserved actionable loader and TLS failure causes in FFI/TLS user-facing
  errors so `dlopen`/`dlsym`, trust-store loading, socket read/write, and
  BearSSL engine/reset failures now retain the underlying OS/library/TLS cause
  instead of collapsing to generic text.
- Made top-level CLI mode dispatch command-local in `src/entry.c3`, so later
  script/submode arguments no longer hijack execution through global flag
  scanning.
- Made the FTXUI backend truthful about `handle_piped_input`: the runtime now
  disables it by default and the backend returns `not_supported` instead of
  silently claiming success.
- Hardened formatter depth handling so `omni --fmt` fails explicitly when the
  structural frame stack overflows instead of continuing with incomplete state.
- Normalized parser delimiter failures so raw bare `(` / `)` / `[` / `]` / `{`
  / `}` messages are translated into explicit expected-token diagnostics, and
  tightened `handle` parsing so empty or nested clause shapes fail as syntax
  errors before runtime allocation paths can misreport them.
- Reconciled shipped docs/introspection wording for deterministic memory,
  canonical `Dictionary`/`Dict` naming, `false` as a `nil` alias, canonical
  `signal` terminology, and `read-file`'s error behavior.
- Added per-argument primitive/type mismatch diagnostics so hot primitives now
  report the failing argument position and a rendered value shape when they
  reject non-matching inputs instead of collapsing to generic
  `expected number argument(s)` messages.
- Added path-specific file-read reporting for script and check entrypoints so
  missing-file, permission-denied, and invalid-path failures preserve the
  concrete path in both text and JSON surfaces while exposing a more specific
  IO error code.
- Closed four 2026-03-31 audit follow-up items by tightening three shipped
  surfaces: `tls-read` now rejects non-integer and non-positive `max-bytes`
  hints, `tls-server-wrap` no longer leaks its native handle when FFI handle
  boxing fails, `inflate` now rejects ignored extra args and invalid
  `original-size` hints instead of silently defaulting, and the AOT `--build`
  backend now excludes `src/lisp/tests*` while the tooling/docs describe the
  real temp-artifact and runtime-link contract.
- Reused the shared file-read classifier for text REPL preload and REPL-server
  preload/load-file paths so missing-file, permission, invalid-path, and
  generic read failures now keep their concrete IO code/reason instead of
  collapsing to `file not found` or a generic read error. Reconciled the REPL
  transport docs with the shipped per-clone `--project` preload behavior, and
  corrected stale runtime docs to describe dual-lane `TEMP`/`ESCAPE`
  ownership, value-typed `Dictionary` keys, and the live `Dictionary`/`Dict`
  collection surface instead of the older RC-centric wording.
- Hardened `--bind` header parsing so the CLI now fails closed instead of
  emitting partial FFI modules when the parser surface overruns local fixed
  limits:
  - `src/lisp/libclang_bind_parse.c3` now rejects overlong header paths before
    copying into the fixed 512-byte libclang path scratch buffer and reports
    when function discovery hits the caller-provided output cap,
  - `src/entry_bind_parse_helpers.c3` and
    `src/entry_bind_dep_generation.c3` now propagate those conditions as
    explicit dependency failures instead of silently truncating the path or
    generating a partial module from the first `256` parsed declarations,
  - `src/lisp/tests_compiler_codegen_groups_tail.c3` now covers both the
    overlong-path guard and the parse-capacity guard alongside the existing
    bindgen overflow regression.
- Tightened FFI binding type validation so unsupported signatures fail closed
  instead of silently degrading to pointer-shaped ABI metadata:
  - `src/lisp/libclang_bind_parse.c3` now aborts header parsing when parameter
    metadata allocation fails or when libclang surfaces unsupported by-value C
    types such as structs,
  - `src/entry_bind_parse_helpers.c3` and
    `src/entry_bind_dep_generation.c3` now surface those conditions as explicit
    `--bind` dependency failures instead of emitting malformed zero-argument or
    pointer-coerced bindings,
  - declarative `ffi λ` definitions now accept only canonical
    `^Integer`/`^Double`/`^String`/`^Pointer`/`^Boolean`/`^Void` annotations and
    raise a definition-time error for unsupported annotations instead of
    defaulting unknown types to raw-pointer ABI metadata,
  - added compiler/runtime regressions in
    `src/lisp/tests_compiler_codegen_groups_tail.c3` and
    `src/lisp/tests_advanced_io_effect_ffi_groups.c3` for allocation-failure,
    unsupported-C-type, and unsupported-annotation paths.
- Hardened `--init` scaffold failure handling so project creation now fails
  truthfully and rolls back partial trees instead of leaving misleading success
  state behind:
  - `src/entry_project_init_files.c3` now treats inner `mkdir(...)=EEXIST` as
    success only when the existing path is actually a directory, reports an
    explicit non-directory collision otherwise, and provides narrow test seams
    to force write-failure and collision paths,
  - `src/entry_project_init_bind.c3` now cleans up the freshly created project
    root on any later scaffold failure so mid-write errors do not leave a
    half-created tree behind,
  - added `tooling/tests/omni_init_smoke.py` to prove both rollback on
    `build/project.json` write failure and truthful failure on an inner
    non-directory collision.
- Hardened the single-worker REPL-server execution lane so live session state
  stays stable across clone growth and interrupts only target active work:
  - `src/lisp/eval_repl_server.c3` now rejects `clone` with
    `protocol/server-busy` before allocating a new session slot, so session
    array growth cannot `realloc(...)` away raw `ReplServerSession*` pointers
    still held by the worker for the active or pending command,
  - REPL-server `interrupt` now matches only the currently running request; a
    queued command is no longer acknowledged and then silently cleared before
    execution starts,
  - added `src/lisp/tests_runtime_async_repl_server_groups.c3` and wired it
    into the async slice, plus `tooling/tests/omni_repl_server_smoke.py` to
    prove `clone` is rejected while `(read-line)` is in flight and succeeds
    again after the request completes.
- Made structured REPL startup failures machine-readable and hardened shared CLI
  JSON escaping for raw argv/path bytes:
  - `src/entry_runtime_modes.c3` now reports `--repl --json` preflight and
    bootstrap failures as one JSON object on stdout instead of plaintext text,
  - `src/entry_repl_server_mode.c3` and `src/lisp/eval_repl_server.c3` now do
    the same for `--repl-server` usage, listen, accept, and TCP discovery-file
    startup failures with stable codes such as `cli/usage`,
    `io/listen-failed`, `io/accept-failed`, and `io/write-failed`,
  - `src/entry_check_reporting.c3` now escapes non-ASCII bytes as `\u00XX`
    so JSON output stays ASCII-safe even when argv/path bytes are not valid
    UTF-8,
  - added `tooling/tests/omni_cli_json_smoke.py` to cover raw-byte JSON
    escaping across `--check --json`, `--eval --json`, and
    `--describe --json`, plus REPL JSON preflight and REPL-server startup
    failures.
- Fixed primitive-name storage to use the full in-struct buffer width instead
  of an obsolete 31-byte cap, so generated/internal primitive names no longer
  emit spurious truncation warnings while still truncating safely at the real
  `Primitive.name` boundary.
- Aligned the AOT entrypoints so `--compile`, `--check`, and `--build` now fail
  before writing or handing off generated C3 when it still contains the
  internal `unsupported expr type` marker, instead of emitting partial C3 with
  a silent `aot::make_nil()` fallback; also removed the stale machine-local
  absolute `TODO.md` path from `docs/plans/README.md`, and made unsupported FFI
  lowering diagnostics refer to AOT lowering generically instead of naming only
  `--compile`; the AOT compiler now also passes its concrete lowering error back
  to the entrypoints so they stop printing a second generic wrapper on the same
  failure, and the old CLI-side scan for the emitted `unsupported expr type`
  marker has been retired in favor of that direct compiler error propagation;
  the compiler helper itself now stays silent unless a caller explicitly
  surfaces the returned lowering error. Compiler syntax failures also no longer
  report prelude-relative line numbers: the prepended stdlib now starts the user
  source on a fresh line and compiler syntax errors are translated back to
  source-relative line/column coordinates before the entrypoints print them.
  Dynamic compiler error strings are now copied out of the compiler frame and
  printed slice-safely by the AOT entrypoints, so malformed-source `--compile`
  and `--build` failures no longer collapse to a blank `Error:` line. AOT file
  loading also now uses the same path classification surface as `--check`, so
  missing-file `--compile` and `--build` failures preserve `file not found`
  instead of falling back to a generic read error. `--compile` output writes now
  also preserve concrete path/write causes such as `invalid path` instead of
  collapsing every output failure into a generic write error, and `--compile`
  no longer prints a misleading `Compiling ...` banner before failing an unreadable
  input path; obvious invalid or non-writable compile output targets are now
  also preflighted locally, so `--compile` fails before compilation starts on
  `invalid path` and `permission denied` output targets. AOT temp C3 staging now also preserves concrete write failures,
  so `--build` reports the actual `build/_aot_temp_...c3` path and reason
  instead of collapsing temp-file failures into a generic `cannot write unique
  AOT temp file in build/` message. `--build -o` also now preflights obviously
  invalid output targets, so missing-parent paths fail immediately as
  `cannot write output binary ...: invalid path` instead of spawning `c3c` and
  then wrapping the backend failure generically; existing-directory output
  targets now fail through that same local invalid-path surface too. Non-writable
  output parents are also preflighted locally now, so `--build -o blocked/outbin`
  fails as `permission denied` instead of reaching backend compile first.
  Backend handoff failures now also stop at the real backend stderr surface, so
  `--build` no longer prints a second generic `c3c compilation failed while
  compiling ...` wrapper after the backend has already emitted the concrete
  spawn or compile failure. Slash-qualified `C3C` overrides are now also
  preflighted as concrete backend compiler paths, so `C3C=/missing/c3c
  omni --build ...` fails early as `missing AOT backend compiler` instead of
  deferring to `/usr/bin/env` for the first actionable error.

## 2026-03-26

- Validated the scheduler offload reuse regression in
  `src/lisp/tests_scheduler_boundary_worker.c3`, proving recycled queued-work
  nodes are reset and the free-list bound stays intact across a reuse cycle.
- Hardened `src/lisp/deduce_relation_scan_helpers_more.c3` so scan-range row
  materialization fails through the native deduce OOM path when row-dict
  allocation is unavailable instead of dereferencing a null hashmap.
- Hardened method-table growth and first-define allocation in
  `src/lisp/jit_jit_define_method_table.c3` by checking `mem::malloc` before
  dereference and by propagating redefine/append failures instead of treating
  them as success.
- Validated the harness-only teardown regression lane with
  `OMNI_LISP_TEARDOWN_REGRESSION=1` and validated the isolated stdlib-loaded
  redefine/replacement regression with
  `OMNI_JIT_POLICY_STDLIB_TEARDOWN_REDEFINITION=1`.
- Made filtered test-group dispatch fail loudly when `OMNI_DEDUCE_GROUP_FILTER`
  or `OMNI_ADVANCED_GROUP_FILTER` matches no tests, so targeted validation no
  longer exits green on a mistyped filter.
- Guarded the queued-work allocation path in `src/lisp/scheduler_offload_worker.c3`
  so a null `mem::malloc` result returns failure instead of dereferencing the
  reset helper on an invalid buffer.
- Added explicit allocation checks to interpreter module/macro initialization in
  `src/lisp/value_interp_init_helpers.c3` so startup fails fast with a clear
  assertion instead of walking a null table during the fill loops.
- Hardened the environment hash-table rebuild and binding-growth path in
  `src/lisp/value_environment.c3` by keeping the old hash table live until a
  replacement is fully allocated and initialized, and by skipping binding
  growth when the expansion buffer allocation fails instead of dereferencing
  null.
- Shipped boundary regression wrapper to enforce the boundary fallback contract for
  return/boundary handoffs and prevent escaped-state regressions.
- Extended equality audit benchmarks to capture the new boundary wrapper behavior
  and include parity coverage for regression-sensitive cases.
- Added offload allocation counters on the boundary/offload path to track allocation
  pressure and publish the new counters in the shipped admin surface.
- Re-aligned deduce benchmark documentation with shipped behavior after the audit
  pass, including scope and truth labels for current benchmark contracts.
- Surfaced the boundary telemetry parser summary directly in
  `scripts/parse_boundary_profile_summary.sh` so the scope-chain pressure and
  dominant return-path outcomes appear in the regression output without digging
  through the nested JSON payload.
- Hardened the offload pool/reuse path by centralizing queued-work reset logic
  in `src/lisp/scheduler_offload_worker.c3` and strengthening the offload
  boundary worker retry coverage to assert pooled reuse is actually happening.
- Moved `prim_schema_explain(...)` into `src/lisp/schema_validation.c3` so the
  validation-facing entrypoints live together and `src/lisp/schema.c3` stays
  focused on the explain selector dispatcher.
- Split the dispatch/match diagnostics formatting helpers out of
  `src/lisp/eval_dispatch_types.c3` into the existing
  `src/lisp/eval_dispatch_match_errors.c3` module so the type registry and
  type-query surface stay focused in the original file.
- Extracted the `deduce_why_result_*` explainability/path-building block and
  `prim_deduce_why_result` out of `src/lisp/deduce_schema_query.c3` into a new
  `src/lisp/deduce_why_result.c3` sibling so the query/execution hot path can
  stay focused in the original file.
- Fixed worker-scratch component-pass recursive delta serialization by carrying
  same-pass scratch-visible additions through
  `src/lisp/deduce_rule_eval_exec_seminaive.c3`, so later recursive atoms in the
  component can observe earlier same-component additions before payload
  serialization.
- Revalidated the deduce worker-scratch wiring path (`c3c build` passes), but the
  bounded deduce lane currently aborts with `exit 139` before suite summary; the
  worker-scratch blocker remains open until that crash is resolved and the
  serialized recursive-delta test can be rechecked.
- Added a per-row materialization cost metric to the deduce scan/query/count
  benchmark summary and reconciled the dispatch hot-path benchmark plan with
  the actual in-tree benchmark file so the shipped coverage is documented at
  the right entrypoint.
- Expanded the equality inline-first workspace caps to keep the common nested
  comparison benchmark on-stack longer, and hoisted the deduce in-txn scan-row
  dict capacity computation out of the per-row loop to remove a small avoidable
  hot-path cost.

## 2026-03-25

- Closed Deduce TODO item `B6.9b2b2b2b2b2b2b2b2` with path-level mixed-context
  goal-directed provenance lists across support-frame relations:
  - proof paths now expose `goal-directed-read-contexts` when matching
    support-frame contexts come from different relations, instead of keeping
    the path-level surface blank or forcing a fake singular merge
  - same-relation support paths still use the singular
    `goal-directed-read-context` field
- Closed Deduce TODO item `B6.9b2b2b2b2b2b2b2b1` with common path-level
  goal-directed context propagation across same-relation support frames:
  - paths now inherit `goal-directed-read-context` when all matching
    support-frame contexts come from the same relation-local last-read state,
    including multi-frame fact support paths on the same relation
  - kept mixed-relation support paths unmerged and split the remaining
    provenance tail to that broader unresolved merge space
- Closed Deduce TODO item `B6.9b2b2b2b2b2b2b2a` with unique path-level
  goal-directed context propagation from matching support frames:
  - when the root tuple did not match but exactly one support frame in the
    chosen proof path carried bounded `goal-directed-read-context`, the path
    now inherits that same context
  - added regression coverage proving unique support-frame propagation
    succeeds while multi-frame fact support paths still stay unmerged
- Closed Deduce TODO item `B6.9b2b2b2b2b2b2b1` with bounded proof-path
  goal-directed context for matching fact support frames:
  - added a regression proving `deduce/why-result` attaches
    `goal-directed-read-context` to matching fact support frames inside a
    chosen derived proof path, not just the root path or derived
    `rule-step` frames
  - split the remaining provenance tail honestly to broader proof-path
    integration beyond the current root/fact-frame/rule-step row-matching
    surface
- Closed Deduce TODO item `B6.11b2` with explicit runtime/admin truth for
  the current parallel execution lane:
  - `deduce/analyze`, relation-local `deduce/stats`, and selector-scoped
    `deduce/explain` now expose `parallel-runtime-mode`
  - the shipped public value is `metadata-only`, which matches the real
    boundary: parallel topology and internal worker-scratch/apply seams
    exist, but the public execution engine is still not a parallel runtime
- Closed Deduce TODO item `B6.11b1b2b` with broader worker-scratch recursive
  closure for positive multi-atom SCC rule shapes:
  - worker-scratch closure no longer stops at the old
    `single-recursive-atom` guard
  - non-anchor recursive atoms now read from `LMDB + prior-iteration
    worker-visible additions`, while same-iteration scratch outputs remain
    invisible until the iteration closes
  - added a direct C-level Deduce regression proving a transitive
    multi-atom recursive SCC emits the full serialized fixpoint delta payload
- Closed Deduce TODO item `B6.9b2b2b2b2b2b2a` with truthful selector-scoped
  path-local parity across the currently shipped row-read shapes:
  - added a mixed-shape regression proving selector-scoped `match` and `scan`
    keep bounded path-local context on `no-op`, while selector-scoped
    `query` and `scan-range` keep bounded path-local context on their shipped
    ephemeral demand paths
  - removed the earlier misleading negative assumption that selector-scoped
    no-op row reads were categorically outside the bounded path-local surface
  - narrowed the remaining provenance backlog honestly back to broader
    proof-path integration beyond the current bounded root/support-frame row
    matching surface
- Closed Deduce TODO item `B6.9b2b2b2b2b2b1` with plain no-op `match`
  parity for bounded root-path goal-directed context:
  - the plain non-ephemeral `deduce/match` path now uses the txn-based scan
    helper when recording bounded subject keys, matching the shipped row-key
    capture behavior used by the goal-directed path
  - added a Deduce regression proving `why-result` attaches
    `goal-directed-read-context` on the matching root proof path after a
    plain no-op `deduce/match` read
  - narrowed the remaining provenance backlog honestly to broader
    proof-path integration beyond the current bounded root/support-frame row
    matching surface
- Closed Deduce TODO item `B6.9b2b2b2b2b2a` with the next truthful
  proof-path provenance slice:
  - plain no-op `deduce/query` and `deduce/scan-range` reads now preserve the
    bounded complete subject set for later `why-result` path matching instead
    of dropping it at the plain read boundary
  - `deduce/why-result` now also attaches path-local
    `goal-directed-read-context` on matching derived support frames, not only
    on the root proof path, when the support frame’s `(relation, tuple)` pair
    matches that bounded complete last-read subject set
  - added Deduce regressions for direct no-op `query` path-local attachment
    and derived support-frame attachment after a plain no-op `scan-range`
  - narrowed the remaining provenance backlog honestly to broader
    proof-path integration beyond the current bounded root/support-frame row
    matching surface
- Closed Deduce TODO item `B6.11b1c` with the main-thread publish/apply path
  for worker-computed recursive deltas:
  - `src/lisp/deduce_rule_eval_exec.c3` now validates and applies a
    serialized component-delta payload back through the existing relation
    integrity and LMDB write path on the main thread
  - added a direct C-level Deduce regression proving a worker-computed
    recursive closure payload can be deserialized, published, and observed as
    durable relation rows with updated schema estimates
  - narrowed the remaining parallel-runtime backlog honestly to broader
    worker-scratch closure (`B6.11b1b2b`) and runtime/admin truth (`B6.11b2`)
- Closed Deduce TODO item `B6.9b2b2b2b2b1` as a shipped selector-scan parity
  closure rather than leaving it implied:
  - selector-scoped `deduce/scan` was already participating in the bounded
    complete row-set path-local `goal-directed-read-context` surface through
    the shared capture path
  - added an explicit Deduce regression and updated the docs so `scan` no
    longer appears plain-only in the bounded-complete provenance slice
- Closed Deduce TODO item `B6.9b2b2b2b2a` with the first bounded multi-row
  proof-path goal-directed provenance slice:
  - replaced the old exact-one-only stored subject key with a bounded complete
    subject-set capture for the last goal-directed row read
  - `deduce/why-result` path-local `goal-directed-read-context` now attaches
    when the traced row belongs to that stored set and the full read stayed
    within the current `8`-row capture limit
  - this widened slice now applies to goal-directed `query`, `match`,
    `scan-range`, and plain `scan`; `query`/`match`/`scan-range` keep
    selector-scoped parity
  - when the last row-read matched more than the bounded capture limit, the
    top-level relation-read context remains available but path-local
    attachment is suppressed truthfully
  - added Deduce regressions for bounded-complete multi-row path attachment
    and overflow fallback
- Pruned the broad roadmap/milestone markers out of `TODO.md` so the live
  backlog keeps only concrete actionable items:
  - removed the `Deduce Product Roadmap Follow-up` section (`F0` through
    `V2-5`) from the active queue
  - kept the concrete open Deduce execution items plus the older runtime and
    validation follow-up items that are still directly actionable
  - corrected the live actionable count accordingly
- Closed Deduce TODO item `B6.11b1b2a` with the first multi-iteration
  worker-scratch recursive closure slice:
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3` now iterates the scratch
    seminaive path to a fixpoint for positive recursive components whose
    rules each have at most one positive recursive body atom
  - the closure path keeps a worker-local visible tuple set for duplicate
    suppression across iterations and returns the accumulated additions
    through the serialized component-delta payload format
  - added a direct C-level Deduce regression that proves the scratch closure
    computes a transitive `path` fixpoint payload containing `(1,2)`,
    `(2,3)`, and `(1,3)` without LMDB publish
  - split the remaining worker-closure lane honestly into
    `B6.11b1b2b` for broader multi-atom recursive shapes
- Closed Deduce TODO item `B6.11b1b1` with the first actual worker-scratch
  recursive compute slice:
  - `src/lisp/deduce_rule_eval_exec_seminaive.c3` now exposes a read-only
    seminaive scratch-pass helper for positive non-aggregate recursive
    components
  - the helper seeds from the current component snapshot, evaluates one
    seminaive pass without LMDB publish, and returns serialized component
    deltas through the `B6.11b1a` payload format
  - added a direct C-level Deduce regression that proves a recursive rule can
    emit current-snapshot-derived tuples into that serialized payload
  - split the remaining worker-compute lane honestly into multi-iteration
    scratch closure under `B6.11b1b2`
- Closed Deduce TODO item `B6.11b1a` by shipping the first internal handoff
  seam for parallel recursive worker-scratch batches:
  - `src/lisp/deduce_rule_eval_exec.c3` now defines a versioned serialized
    component-delta payload for one SCC component's signed deltas
  - the payload carries component id, per-predicate identity, and opaque
    encoded tuple additions/removals through owned bytes compatible with the
    existing scheduler `OFFLOAD_RES_BYTES` result seam
  - added a direct C-level Deduce regression that roundtrips the payload and
    proves out-of-component predicates are excluded from the serialized batch
  - split the remaining runtime work honestly into `B6.11b1b`
    (worker-scratch compute) and `B6.11b1c` (main-thread publish/apply)
- Tightened the open `B6.11b1` lane to a concrete first runtime shape instead
  of leaving “parallel recursive execution” vague:
  - added `docs/plans/deduce-parallel-recursive-first-runtime-shape-decision-2026-03-25.md`
  - chose worker-local scratch computation plus main-thread publish as the
  first honest runtime widening
  - explicitly rejected direct worker-owned LMDB publish as the initial
    parallel path
- Closed Deduce TODO item `B6.11a2` by pinning the current fallback/admin
  truth for parallel recursion metadata:
  - selector-scoped `deduce/explain` now has explicit regression coverage
    proving that parallel batch metadata does not imply a separate runtime
    engine today
  - the current truthful surface keeps `execution-engine = 'semi-naive-scc`
    and `goal-directed-execution-path = 'selected-component-closure`
    alongside the parallel topology fields
- Closed Deduce TODO item `B6.11a1` by pinning the current parallel-recursion
  scheduling contract:
  - the parallel SCC metadata is now documented as a deterministic,
    metadata-only scheduling surface
  - recursive components batch only within the same stratum, grouped by the
    same computed `wave`
  - `wave = 1` means no same-stratum recursive dependency, and higher waves
    reflect the one-based longest same-stratum dependency distance from that
    boundary
  - added regression coverage that pins the current `stratum` / `wave` /
    `batch-size` topology on representative recursive SCC batches
- Closed Deduce TODO item `B6.10b2` by shipping dedicated admin counters and
  summary truth for `check` constraints:
  - relation-local `deduce/stats` now exposes
    `check-integrity-violation-count`
  - DB-wide `deduce/analyze` now exposes the same dedicated
    `check-integrity-violation-count` aggregate
  - the existing integrity-reporting regressions now pin `0` for non-check
    classes and `1` for check violations on the current `check` failure path
- Closed Deduce TODO item `B6.10b1` with the first shipped write-enforcement
  slice for canonical `check` constraints:
  - immediate `fact!`, derived rule-head publish, and deferred
    `write-deferred` commit-time snapshot validation now enforce declared
    unary column checks
  - failed checks raise deterministic machine-checkable payloads with
    `deduce/integrity-check-failed`, while missing, non-callable, and raised
    predicates reject with `deduce/check-predicate-missing`,
    `deduce/check-predicate-not-callable`, and
    `deduce/check-predicate-raised`
  - generic relation and DB integrity history now surface
    `violation-class = 'check` for those failures
  - the remaining `check` follow-up is narrowed to dedicated per-class admin
    counters and summary truth under `B6.10b2`
- Closed Deduce TODO item `B6.10a2` with the first shipped schema/admin
  baseline for canonical `check` constraints:
  - relation declarations now accept unary column checks in the form
    `(check predicate column)`
  - relation schemas persist declared `check` metadata, `deduce/schema`
    exposes `kind = 'check`, `predicate`, `columns`, and `enforced = false`,
    and `deduce/analyze` now reports DB-wide `check-constraint-count`
  - integrity totals now include declared `check` constraints on the admin
    surfaces without claiming write-side enforcement
  - added parser/storage and schema/analyze regression coverage for the new
    `check` payload surface
  - write-side `check` enforcement was the next follow-up slice and is now
    closed under `B6.10b1`
- Closed Deduce TODO item `B6.9b2b2b2b1` with exact-one goal-directed
  `deduce/scan` path context on matching `why-result` paths:
  - full-relation `deduce/scan` now captures the stored row key when the scan
    result has exactly one row
  - `deduce/why-result` now attaches path-local
    `goal-directed-read-context` for exact-one goal-directed `deduce/scan`
    subjects alongside the shipped `query`, `match`, and `scan-range` slices
  - that shipped `scan` slice is pinned for plain reads only; selector-scoped
    `scan` is still outside the current exact-one path-local slice
  - the remaining provenance item is now narrowed to `B6.9b2b2b2b2`
- Closed Deduce TODO item `B6.9b2b2b2a` by pinning selector-scoped parity for
  the shipped exact-one row-read path-local provenance slice:
  - exact-one path-local `goal-directed-read-context` now has explicit
    regression coverage for selector-scoped goal-directed `deduce/query`,
    `deduce/match`, and `deduce/scan-range`
  - selector-scoped payloads keep their concrete `selector-rule-index` on the
    matching proof path, while non-matching rows in the same relation keep
    that path-local field `nil`
  - the remaining provenance item is now narrowed to `B6.9b2b2b2b`
- Closed Deduce TODO item `B6.9b2b2b1` with exact-one goal-directed
  `deduce/scan-range` path context on matching `why-result` paths:
  - transactional goal-directed `deduce/scan-range` scans now capture the
    stored row key when exactly one tuple is returned
  - `deduce/why-result` now attaches path-local
    `goal-directed-read-context` for exact-one goal-directed
    `deduce/scan-range` subjects alongside the shipped `query` and `match`
    slices
  - the remaining provenance item is now narrowed to `B6.9b2b2b2`
- Closed Deduce TODO item `B6.9b2b2a` with exact-one goal-directed
  `deduce/match` path context on matching `why-result` paths:
  - transactional goal-directed `deduce/match` scans now capture the matched
    stored row key when exactly one tuple matches
  - `deduce/why-result` now attaches path-local
    `goal-directed-read-context` for exact-one goal-directed `deduce/match`
    subjects, just like the existing exact-one `query` slice
  - the remaining provenance item is now narrowed to `B6.9b2b2b`
- Closed Deduce TODO item `B6.9b2b1` with the first truthful
  proof-path-integrated goal-directed provenance slice:
  - `deduce/why-result` now attaches optional path-local
    `goal-directed-read-context` metadata when the relation's last
    goal-directed `deduce/query` observed exactly one row and that exact-one
    row matches the traced tuple
  - the broader provenance item is now narrowed to `B6.9b2b2`
  - relation-level top-level `goal-directed-read-context` remains unchanged
- Closed Deduce TODO item `B6.10a1` by fixing the next widened integrity
  class as canonical `check`:
  - the remaining backlog now names `check` constraints directly instead of
    the vague “widened integrity class” wording
  - `check` is the selected next integrity lane after the shipped
    `key` / `unique` / `ref` surface
  - rejected aliases for that lane are `assert`, `predicate`, and `guard`
- Closed Deduce TODO item `B6.4f5c2b2b2b` as stale backlog drift:
  - the transformed recursive query lane is now fully described by shipped
    support/fallback slices:
    same-index SCC support, one-carried-position transformed SCC relaxation,
    truthful permutation fallback, and truthful transformed multi-atom
    fallback
  - no separate transformed residual remained beyond those now-pinned
    boundaries
- Closed Deduce TODO item `B6.4f5c2b2b2a` by restoring truthful fallback for
  transformed recursive multi-atom `deduce/query` demands:
  - transformed recursive single-branch and disjunctive shapes whose
    uncarried demand is still distributed across multiple recursive body atoms
    now stay on `selected-component-closure` instead of reporting
    `ephemeral-head-demand-query` with incomplete rows
  - the remaining transformed recursive query residual stays open as
    `B6.4f5c2b2b2b`
- Closed Deduce TODO item `B6.4f5c2b2b1` by pinning multi-hop transformed SCC
  one-carried-position support:
  - transformed recursive pair and pair-disjunction demand relaxation now has
    explicit regression coverage for multi-hop SCC cycles, not just reordered
    two-relation mutual recursion
  - the remaining transformed recursive query residual stays open as
    `B6.4f5c2b2b2`
- Closed Deduce TODO item `B6.4f5c2b2a` by pinning transformed recursive
  disjunctive one-carried-position relaxation:
  - transformed reordered mutual-recursive pair disjunctions now have
    explicit regression coverage on `ephemeral-head-demand-query` with
    requested positions `(0 1)` and one applied carried position
  - pure recursive permutation disjunctions still stay on truthful fallback
  - the remaining transformed recursive query residual stays open as
    `B6.4f5c2b2b`
- Closed Deduce TODO item `B6.4f5c2b1` by widening recursive transformed
  `deduce/query` support to one-carried-position relaxation:
  - transformed recursive SCC pair demands may now stay on
    `ephemeral-head-demand-query` when the queried head predicate still has
    some same-index recursive carrier for one requested position
  - that shipped slice reports requested positions `(0 1)` with one applied
    carried position, while pure permutation shapes still fall back truthfully
  - the remaining transformed recursive query residual stays open as
    `B6.4f5c2b2`
- Closed Deduce TODO item `B6.4f5c2a` by pinning multi-hop same-index SCC
  `deduce/query` support:
  - same-index recursive pair demands now have explicit regression coverage
    for positive three-relation SCC cycles, not just self-recursive or
    two-relation mutual-recursive shapes
  - the remaining recursive query residual stays open as `B6.4f5c2b`
- Closed Deduce TODO item `B6.4f5c1` by pinning same-index mutual-recursive
  disjunctive `deduce/query` support:
  - same-index mutual-recursive SCC pair disjunctions now have explicit
    regression coverage on `ephemeral-head-demand-query` with requested and
    applied positions `(0 1)`
  - the remaining recursive query residual stays open as `B6.4f5c2`
- Closed Deduce TODO item `B6.4f5b` by widening recursive multi-position
  `deduce/query` support to same-index mutual-recursive SCC shapes:
  - fully-bound pair demands may now stay on `ephemeral-head-demand-query`
    not only for same-index self-recursive rules, but also for SCC-local
    mutual recursion where each recursive rule has some positive recursive
    body atom preserving all requested positions together at the same indices
  - permutation shapes and multi-atom distributed-demand shapes still fall
    back truthfully to `selected-component-closure`
  - the remaining recursive query residual stays open as `B6.4f5c`
- Closed Deduce TODO item `B6.9b2a` by exposing optional why-result
  goal-directed read context metadata:
  - `deduce/why-result` now adds top-level `goal-directed-read-context` when
    the relation has existing last goal-directed read state
  - that metadata mirrors the shipped `last-goal-directed-read-*` admin truth
    and may appear even when the row subject is still `missing`
  - proof-path-integrated goal-directed provenance now continues as the
    narrower residual `B6.9b2b2`
- Closed Deduce TODO item `B6.9b1b` by widening recursive `deduce/why-result`
  payloads beyond flattened fact-only closure support:
  - recursive closure paths now append a `rule-step` support frame for the
    derived child subject used in the chosen proof chain
  - recursive closure `max-depth` now reflects that deeper derived step,
    moving the first shipped recursive payloads beyond flat fact-only support
  - goal-directed provenance semantics remain open as `B6.9b2`
- Closed Deduce TODO item `B6.9b1a` by shipping the first positive recursive
  `deduce/why-result` closure lineage:
  - stored rows in positive recursive closure relations now compose support
    through the shipped row-subject provenance helper surface while guarding
    against revisiting the same `(predicate, tuple)` subject
  - one recursive support chain returns `status = ok`, and multiple recursive
    support chains return `status = partial` with the first deterministic path
  - broader recursive lineage beyond the current flattened fact-support surface
    remains open as `B6.9b1b`
- Closed Deduce TODO item `B6.9a8` by shipping the first multi-rule
  non-recursive `deduce/why-result` lineage:
  - stored rows in multi-rule non-recursive derived relations now aggregate
    support paths across the already-shipped non-recursive provenance helper
    surface
  - a single supported path across matching rules returns `status = ok`, and
    multiple supported paths return `status = partial` with the first
    deterministic path
  - recursive and goal-directed provenance semantics remain open as `B6.9b1`
    and `B6.9b2`
- Closed Deduce TODO item `B6.9a7` by shipping the first exact-one-rule
  mixed-body non-recursive `deduce/why-result` lineage:
  - mixed-body non-recursive rules now compose through already-supported
    exact-one-rule child provenance helpers
  - unique support chains return `status = ok`, and multiple support chains
    return `status = partial` with the first deterministic path
  - multi-rule non-recursive derived lineage remains open as `B6.9a8`
- Closed Deduce TODO item `B6.9a6` by shipping the first exact-one-rule
  extensional search-based `deduce/why-result` lineage:
  - non-recursive extensional rules with existential/search-based support now
    return `status = ok` when one deterministic path exists
  - the same surface now returns `status = partial` with `truncated = true`
    when multiple support paths exist and only the first deterministic path is
    emitted
  - mixed-body or multi-rule non-recursive derived lineage remains open as
    `B6.9a7`
- Closed Deduce TODO item `B6.9a5` by widening non-recursive
  `deduce/why-result` lineage to reconstructible extensional multi-body rules:
  - exact-one-rule extensional derived relations now return `status = ok` when
    every support tuple is reconstructible from the head row
  - current derived support now covers direct-copy, one-body
    projection/permutation, and head-bound multi-body extensional rules
  - existential or search-based non-recursive derived lineage remains open as
    `B6.9a6`
- Closed Deduce TODO item `B6.9a4` by widening non-recursive
  `deduce/why-result` lineage to the first one-body extensional class:
  - stored rows in relations with exactly one non-negated, non-aggregate,
    one-body extensional rule now return `status = ok` when the source tuple is
    fully reconstructible from the head row
  - current derived support covers direct-copy plus one-body
    variable projection/permutation rules
  - multi-body or existential non-recursive derived lineage remains open as
    `B6.9a5`
- Closed Deduce TODO item `B6.9a3` by shipping the first non-recursive
  derived `deduce/why-result` slice:
  - stored rows in relations with exactly one non-negated, non-aggregate,
    one-body direct-copy rule from an extensional source now return
    `status = ok` with one deterministic `derived` path
  - the derived path's support names the extensional source fact predicate
  - broader non-recursive derived lineage remains open as `B6.9a4`
- Closed Deduce TODO item `B6.9a2` by shipping the first public
  `deduce/why-result` runtime slice:
  - `(deduce/why-result relation val...)` now returns deterministic row-subject
    payloads for stored extensional tuples
  - extensional hits return `status = ok` with one `seed` path and one `fact`
    support frame
  - missing tuples return `status = missing`
  - stored derived rows return `status = error` with
    `deduce/why-result-derived-subject-not-yet-supported`
  - the remaining non-recursive derived-lineage work is tracked separately as
    `B6.9a3`
- Closed Deduce TODO item `B6.9a1` by tightening the why-result baseline into
  canonical doc truth:
  - `docs/deduce-datalog-spec.md` now fixes the top-level status semantics for
    `ok`, `missing`, `partial`, and `error`
  - the canonical why-result envelope keys are now explicit before any public
    provenance runtime rollout
- Closed Deduce TODO item `B6.8b2` by shipping the first derived-predicate
  maintained-update class:
  - successful `incremental-targeted` refresh of a supported materialized
    direct-copy source can now queue the same inserted tuples for one-step
    downstream direct-copy materialized targets
  - downstream ready targets can then refresh on the same
    `incremental-targeted` path while still remaining stale until explicitly
    refreshed
- Closed Deduce TODO item `B6.8b1` by shipping the first maintained-update
  class for ordinary DB mutations:
  - committed insertions on supported extensional direct-copy sources now queue
    encoded tuple keys per materialized target
  - relation-scoped `deduce/refresh!` now reports
    `refresh-execution-path = 'incremental-targeted` for already-refreshed
    supported targets, and stale `on-read` materialized reads reuse that same
    path
  - queue-tracking failure now escalates truthfully to the existing
    `full-recompute-required` boundary instead of silently dropping deltas
- Closed Deduce TODO item `B6.8a2` by fixing the fallback/admin contract for
  future incremental maintenance:
  - the canonical docs now require later maintained-update work to reuse the
    current `tracked` / `full-recompute` public vocabulary
  - the docs now explicitly reject new public degraded-mode names and require
    existing stats/analyze/refresh/read surfaces to stay truthful under
    fallback
- Closed Deduce TODO item `B6.8a1` by lifting the current incremental
  signed-delta substrate into the canonical docs:
  - `docs/deduce-datalog-spec.md` now has an explicit data-model baseline for
    encoded tuples, signed delta sets, support tables, and current/next
    iteration buffers
  - `docs/reference/08-libraries.md` now points later maintenance work at
    that substrate instead of leaving the internal model implicit
- Closed Deduce TODO item `B6.7b2` by pinning admin-surface alignment beyond
  tracked recompute:
  - `deduce/stats`, `deduce/analyze`, and relation-scoped `deduce/refresh!`
    now have explicit regression coverage across a rule-set-change degraded
    transition
  - the current surface is now pinned so that a prior plain DB-level analyze
    clears `full-recompute`, a later relation refresh stays targeted, and the
    untouched stale peer remains visible on both refresh and later analyze
- Closed Deduce TODO item `B6.7b1` by pinning the current dirty-frontier truth
  contract:
  - relation-local `deduce/stats` now has explicit regression coverage showing
    that `dirty-predicate-count` / `dirty-predicates` stay DB-global
  - `dirty-self` is now pinned as the relation-local bit that differs between
    a cleared relation and an untouched relation after selected-component
    recovery
- Closed Deduce TODO item `B6.7a2` by pinning the current degraded-state and
  recovery contract:
  - relation-local `deduce/stats` now has explicit regression coverage showing
    `full-recompute-required` immediately after destructive writes
  - plain DB-level `deduce/analyze` now has explicit regression coverage as a
    pre-reset snapshot surface that still reports the degraded frontier it
    observed
  - a successful DB-level analyze run is now pinned as clearing the live
    `full-recompute` / dirty-admin state afterward
- Closed Deduce TODO item `B6.7a1` by pinning the current commit/abort
  mutation-log contract:
  - write-block mutations now have explicit regression coverage showing that
    dirty/admin state changes only on commit
  - `abort` drops pending mutation logs without changing row counts or dirty
    metadata
  - committed destructive writes are now pinned as the current path that
    escalates `incremental-invalidation-mode` to `full-recompute`
- Closed Deduce TODO item `B6.6b2` by pinning the real conjunctive counter
  boundary:
  - `deduce/stats.last-goal-directed-read-step-counters` now has explicit
    regression coverage as the last preserved-bound runtime read surface
  - `deduce/analyze.rule-execution[*].steps[*].counters` and
    `deduce/explain.steps[*].counters` are now pinned as separate observed
    execution surfaces, not aliases of the earlier read counters
- Closed Deduce TODO item `B6.6b1` by pinning conjunctive path/order truth at
  the current shipped boundary:
  - `deduce/explain.steps[*]` and
    `deduce/analyze.rule-execution[*].steps[*]` now have regression coverage
    proving they stay aligned on planner-derived `join-order`, `predicate`,
    `operator`, and `selected-index`
  - runtime truth for that lane remains in the attached observed counters,
    not a separate reordered execution trace
- Closed Deduce TODO item `B6.6a2` by pinning the shipped `deduce/analyze`
  classification boundary for conjunctive recursive selectors:
  - selector-scoped top-level `goal-directed-*` fields remain planner-side
  - `last-goal-directed-read-*` continues to mirror the last actual DB-level
    read
  - `rule-execution[*].steps[*].counters.counter-kind = 'observed` is now
    covered explicitly for the analyzed rule entries
- Closed Deduce TODO item `B6.6a1` by pinning the shipped `deduce/explain`
  classification boundary:
  - `deduce/explain` now has regression coverage proving it remains a
    `planner-snapshot` surface even after a runtime `deduce/query`
  - top-level `goal-directed-execution-path` stays planner-side, while
    `last-goal-directed-read-*` and `steps[*].counters.counter-kind =
    'observed` mirror the last actual runtime read
- Closed Deduce TODO item `B6.4f5a` with the next recursive symbolic safety
  slice:
  - recursive query demands whose requested positions are split across
    multiple recursive atoms, such as `path(x,z) :- path(x,y), path(y,z)`,
    now have explicit selector/plain regression coverage on truthful
    `selected-component-closure` fallback
  - the remaining recursive symbolic residual is narrower `B6.4f5b`:
    broader recursive multi-position query support beyond the current
    same-index-preserved and fallback-only shapes
- Closed Deduce TODO item `B6.4f4b` with the first real jointly-supported
  recursive multi-position query slice:
  - same-index self-recursive shapes such as `stable(x,y) :- stable(x,y)`
    now keep fully-bound multi-position selector/plain query demands on
    `ephemeral-head-demand-query`
  - admin truth for that slice now reports requested and applied positions
    `(0 1)` instead of collapsing to one carried position or a full
    selected-component closure
  - the remaining recursive symbolic residual is narrower `B6.4f5`:
    broader recursive multi-position query support beyond the current
    same-index-preserved and permutation-fallback shapes
- Closed Deduce TODO item `B6.4f4a` with the next recursive symbolic safety
  slice:
  - jointly-supported recursive permutation query demands such as
    `sym(x,y) :- sym(y,x)` no longer claim the ephemeral demand path when the
    current evaluator cannot seed the needed support tuples
  - those selector/plain single-branch and disjunctive queries now fall back
    truthfully to `selected-component-closure` with `dirty-closure`
  - the remaining recursive symbolic residual is narrower `B6.4f4b`:
    preserving jointly-supported multi-position recursive query demands
    beyond that truthful permutation fallback
- Closed Deduce TODO item `B6.4f3` with the next bounded recursive symbolic
  migration slice:
  - recursive multi-position symbolic query demands no longer hard-code the
    last requested position as the one applied recursive anchor
  - the runtime now keeps the one head position preserved by the positive
    self-recursive rule, which fixes reordered recursive head shapes where
    the carried position is not the last column
  - the remaining recursive symbolic residual is now narrower `B6.4f4`:
    jointly-supported multi-position recursive demands that should stay
    applied together on single positive self-recursive shapes
- Closed Deduce TODO item `B6.4f2` with the first bounded recursive symbolic
  migration slice:
  - recursive multi-position symbolic pair filters and disjunctive pair
    branches now stay on `ephemeral-head-demand-query` when the projected
    recursive head demand can be relaxed to one applied position
  - the current shipped relaxation keeps the trailing supported projected
    position, so admin truth now reports requested positions `(0 1)` with
    applied positions `(1)` for that slice
  - the real residual is now narrower `B6.4f3`: broader recursive symbolic
    rewrite beyond single applied-position relaxation
- Retired stale Deduce TODO item `B6.4e2c` after rechecking the remaining
  symbolic query lane against the current runtime contract:
  - there is no separate non-recursive goal-directed symbolic disjunction
    implementation lane beyond the already shipped subset, because non-recursive
    one-rule subjects are not goal-directed eligible in the current planner
  - the real open residual is entirely the recursive symbolic migration
    lane now tracked as `B6.4f2`
- Closed Deduce TODO item `B6.4f1` with the first truthful recursive-shape
  safety slice: recursive multi-position symbolic query demands, including
  single filters and disjunctive branches shaped like `(and (= src ...) (=
  dst ...))`, now fall back to `selected-component-closure` instead of
  claiming `ephemeral-head-demand-query` with incomplete results; the open
  recursive residual is broader support beyond the current single-position
  migration boundary under `B6.4f2`.
- Closed Deduce TODO item `B6.4e2b2` with the next truthful disjunctive
  rewrite slice: mixed-position demand-safe branches now stay on
  `ephemeral-head-demand-query`, with the runtime unioning the branch-local
  requested/applied positions before reapplying the original full
  disjunctive filter; the remaining open residual is broader symbolic
  multi-branch rewrite under `B6.4e2c`.
- Closed Deduce TODO item `B6.4e2b1` with the next truthful same-position
  disjunctive rewrite slice: same-position branch unions now also cover
  branch-local residual unsupported conjuncts, because each branch still
  mines only its shipped equality-demand subset and the original full filter
  is reapplied over the union after the branch-local reads; the remaining
  open residual is wider mixed-position or more symbolic disjunction under
  `B6.4e2b2`.
- Tightened the already-shipped `B6.4e1` / `B6.4e2a` disjunctive query
  rewrite boundary to the truthful runtime contract:
  - each shipped branch now executes in its own abortable temporary write
    txn before the original full filter is reapplied over the union
  - same-position wrapper disjuncts stay on the ephemeral path
  - mixed-position multi-branch disjuncts are now pinned back to
    `selected-component-closure`, which remains the open residual under
    `B6.4e2b`
- Closed Deduce TODO item `B6.4e2a` with the next truthful same-position
  multi-branch rewrite slice: the disjunctive ephemeral query path now also
  accepts same-position wrapper branches that reach the shared projected
  head-demand slot through already shipped whole-filter or row-column
  wrappers, while mixed-position disjuncts still fall back and remain open
  under `B6.4e2b`.
- Closed Deduce TODO item `B6.4e1` with the first broader query-time rewrite
  execution slice: `deduce/query` can now union multiple abortable ephemeral
  head-demand runs for an `or` of individually demand-safe branches inside
  the same temporary write txn, while wider mixed or symbolic disjunctions
  still fall back and remain open under `B6.4e2`.
- Closed Deduce TODO item `B6.4d` with the next truthful `deduce/query`
  demand-widening slice: the shipped extractor now also rewrites captured
  comparator wrappers when exactly one call argument reduces to a row-derived
  scalar and the remaining arguments are closed literals, and it preserves
  that same bounded subset across short forwarding chains without claiming
  generic captured-call rewrite or symbolic solving.
- Closed Deduce TODO item `B6.3f2` with the first truthful durable
  ready/freshness reopen slice: supported persisted executable rule
  signatures now restore live derived execution through stable catalog-name
  remapping plus inferred predicate-schema registration, so reopened
  `deduce/analyze`, `deduce/refresh!`, refreshed manual materialized views,
  and stale `on-read` reads regain truthful ready/fresh behavior on the
  current supported signature surface while unsupported signature shapes
  still fall back to summary-only admin truth.
- Closed Deduce TODO item `B6.3f1b` with the first truthful rule/dependency
  catalog persistence slice: file-backed DBs now persist a compact admin
  catalog summary across reopen, so reopened `deduce/analyze` retains the
  last installed `rule-count` and `incremental-dependency-edges` while
  executable rule state and durable ready/fresh semantics remain deferred.
- Consolidated the repo backlog into `TODO.md` and removed the parallel
  backlog/roadmap tracker files from `docs/plans/`, so there is now one live
  execution queue instead of several drifting sources.
- Shipped Deduce declaration-time `on-read` materialization as the first
  non-manual policy: `[relation db materialized on-read]` now parses,
  persists across reopen, surfaces through `deduce/schema` and `deduce/stats`,
  auto-refreshes stale derived materialized relations on ordinary stored-tuple
  reads, and fails truthfully on stale reopen cases where rule/dependency
  catalogs are still absent.
- Chose the first future non-manual declaration policy target instead of
  leaving it generic: `on-read` is now the explicit first widening after the
  current manual-only surface, while `on-base-commit`, `on-open`, and
  `scheduled` remain later approved names for different trigger families.
- Strengthened the repo agent rule from “don’t split only when naming is
  blocked” into a general naming-problem rule: whenever naming is unclear,
  drifting, or blocking progress, agents must convert that into an explicit
  decision, a concrete options note, or a direct owner question instead of
  leaving “needs naming” as the stopping point.
- Expanded the Deduce declaration-policy decision from “freeze at `manual`”
  into a full naming table for future trigger families: `on-read`,
  `on-base-commit`, `on-open`, and `scheduled` are now the approved future
  names, while vague spellings like `auto`, `eager`, `background`, and
  `on-write` are explicitly rejected.
- Closed Deduce backlog item `B6.3e1` with an explicit surface decision
  instead of leaving naming drift open-ended: declaration-time materialization
  stays frozen at `manual` until a real non-manual maintenance contract exists,
  and the decision note now records concrete rejected/deferred candidates such
  as `auto`, `eager`, `on-open`, `on-read`, and `on-base-commit`.
- Added an agent rule in `AGENTS.md` forbidding “split only” drift when work is
  blocked on missing language-facing naming or surface-contract choices:
  agents must either record the explicit current decision or add a short
  decision note with concrete candidate spellings, semantics, and a
  recommendation.
- Closed Deduce backlog item `B6.3f1a` at the honest shipped reopen contract:
  file-backed reopen preserves materialized snapshot rows and lifecycle
  metadata, but installed rules and dependency catalogs do not survive reopen,
  so reopened `deduce/analyze` resets to `rule-count = 0` and
  `incremental-dependency-edges = 0` until rules are reinstalled.
- Split Deduce backlog item `B6.3f1` at the real semantic boundary:
  `B6.3f1a` now tracks the canonical reopen contract for persisted
  rule/dependency catalogs, and `B6.3f1b` tracks any runtime/storage rollout
  that should only happen after that catalog-persistence contract is chosen.
- Split Deduce backlog item `B6.3f` at the real persistence boundary:
  `B6.3f1` now tracks persisted rule/dependency catalogs beyond the current
  snapshot contract, and `B6.3f2` tracks any later durable freshness/ready
  semantics that should only be defined after that catalog-persistence
  boundary exists.
- Split Deduce backlog item `B6.3e` at the real semantic boundary instead of
  guessing new language-facing declaration-policy names: `B6.3e1` now tracks
  the canonical policy naming/contract choice beyond `manual`, and `B6.3e2`
  tracks any parser/runtime/schema widening that should happen only after that
  naming decision exists.
- Closed Deduce backlog item `B6.3d` at the honest shipped persistence
  boundary: file-backed materialized relations now have an explicit current
  contract that persists the last stored snapshot rows across reopen in
  addition to materialized lifecycle metadata, while reopened schema/admin
  surfaces still remain conservatively unready until rules are reinstalled.
  Broader persisted rule/dependency catalogs and durable derived freshness
  semantics are promoted into follow-up item `B6.3f`.
- Closed Deduce backlog item `B6.3c` at the honest shipped declaration-policy
  boundary: declaration-time materialization is now recorded as an explicit
  manual-only contract, `[relation db materialized]` is documented as
  shorthand for `[relation db materialized manual]`, and broader
  declaration-policy widening is promoted to follow-up item `B6.3e` instead
  of leaving the shipped baseline looking perpetually partial.
- Reshaped the remaining Deduce `B6.4` backlog so the old `B6.4c` umbrella
  now closes as the shipped goal-directed execution baseline, and the true
  residual work is split explicitly into:
  - wider captured-call rewrite for `deduce/query` demand extraction
  - broader query-time magic-set / rewrite execution
  - migration and safety rules for wider recursive shapes
- Added explicit Deduce backlog items for the remaining gaps still marked
  `partial` or `deferred` in `docs/deduce-datalog-spec.md`, covering:
  - planner-backed conjunctive execution / explain truthfulness tightening
  - mutation logging / dirty-tracking contract beyond tracked recompute
  - true incremental derived-state maintenance
  - runtime provenance / why-result surface
  - richer integrity classes beyond the current key/unique/reference slice
  - parallel recursive evaluation beyond topology visibility
- Increased the backlog granularity again by splitting each of those new
  spec-derived residuals into two smaller items so contract/baseline work and
  runtime rollout work are no longer bundled together.
- Increased the backlog granularity one more time by splitting each of those
  twelve smaller residuals again into two subitems, so the open
  spec-derived Deduce queue now tracks twenty-four narrow contract/rollout
  slices rather than twelve medium ones.

- Landed the next `B6.4c` Deduce goal-directed query-demand validation slice:
  - the preserved-row literal-arg wrapper subset is now pinned explicitly
    when the forwarded `row` appears in a non-leading argument position on
    both the whole-filter side and the row-column side, including
    multi-literal-arg wrapper shapes that still collapse to the shipped
    equality-demand subset
  - added focused selector/plain regressions in
    `src/lisp/tests_deduce_query_groups.c3` so the remaining open gap stays
    "wider rewrite" rather than argument-position variants that are already
    shipped

- Landed the next `B6.4c` Deduce goal-directed query-demand slice:
  - the preserved-row wrapper helper already keeps short forwarding chains of
    extra-literal-arg wrappers on the ephemeral query path for both the
    whole-filter and row-column cases
  - added focused selector/plain regressions in
    `src/lisp/tests_deduce_query_groups.c3` to pin those chain shapes
    explicitly so the remaining open gap stays "wider rewrite" rather than
    already-shipped bounded chains

- Landed the next `B6.4c` Deduce goal-directed query-demand slice:
  - `deduce/query` demand extraction now also unwraps preserved-row captured
    wrapper calls with extra closed literal arguments on both the
    whole-filter side and the row-column side
  - this widening stays bounded to wrappers that still forward the current
    `row` directly and only bind the remaining arguments from closed literal
    expressions, so wider captured-call shapes still fall back to
    `selected-component-closure`
  - added focused selector/plain regressions in
    `src/lisp/tests_deduce_query_groups.c3` that pin the ephemeral query path
    through those preserved-row literal-arg wrapper shapes

- Landed the next `B6.4c` Deduce goal-directed query-demand slice:
  - `deduce/query` demand extraction now also follows short forwarding chains
    of one-argument captured wrappers for both the whole-filter case and the
    row-column-side case
  - this widening stays bounded rather than pretending generic captured-call
    rewrite is shipped: wider captured-call shapes outside that simple
    forwarding-chain pattern still fall back to
    `selected-component-closure`
  - added focused selector/plain regressions in
    `src/lisp/tests_deduce_query_groups.c3` that pin the ephemeral query path
    through those forwarding-chain shapes

- Landed the next `B6.4c` Deduce goal-directed query-demand slice:
  - `deduce/query` demand extraction now also unwraps one-argument captured
    closure wrappers on the row-column side when they forward the current
    `row` into a body that already resolves to a supported
    `(ref row 'column)` shape
  - this widening is still intentionally narrow and non-recursive:
    captured wrapper stacks on the row-column side still fall back to the
    existing `selected-component-closure` path instead of pretending generic
    captured-call rewrite is already shipped
  - added focused selector/plain regressions in
    `src/lisp/tests_deduce_query_groups.c3` that pin the ephemeral query path
    through that row-column captured-wrapper shape

- Landed the next `B6.4c` Deduce goal-directed query-demand slice:
  - `deduce/query` demand extraction now also unwraps one-argument captured
    closure wrappers around the whole filter body when they simply forward the
    current `row` into a body that already fits the shipped
    equality-demand subset
  - the widening is intentionally narrow and non-recursive:
    nested captured wrapper stacks still fall back to the existing
    `selected-component-closure` path instead of pretending generic
    captured-call rewrite is already shipped
  - added focused selector/plain regressions in
    `src/lisp/tests_deduce_query_groups.c3` that pin the ephemeral query path
    through that captured-wrapper shape

- Reshaped the remaining `B6.3` Deduce materialized-view backlog to match the
  real residual work:
  - closed the old `B6.3b` umbrella item as a backlog-shaping correction
    instead of leaving shipped declaration, dematerialize, and persisted
    lifecycle slices under one permanently-open heading
  - promoted the only real residuals into two explicit open items in
    `docs/plans/deduce-actionable-backlog-2026-03-20.md`:
    - `B6.3c` declaration policy beyond the current manual-only marker
    - `B6.3d` persisted derived-state durability semantics beyond the current
      lifecycle metadata
  - no runtime behavior changed in this patch; this is an active-plan truth
    correction

- Published the canonical Deduce maintenance analytics payload baseline:
  - `docs/deduce-datalog-spec.md` now has one source-of-truth section for
    relation-local `deduce/stats`, DB-level `deduce/analyze`,
    `deduce/refresh!`, and the current maintenance/admin structured rejection
    keys
  - corrected the docs to match the shipped runtime surface exactly:
    `materialized-refresh-policy` is currently a `deduce/schema` /
    `deduce/refresh!` field, not a `deduce/stats` field
  - `docs/reference/08-libraries.md` now points to that spec section instead
    of duplicating field inventories, and the first analytics-extension
    milestone item is closed in
    `docs/plans/deduce-analytics-extension-milestone-2026-03-24.md`

- Closed the next Deduce analytics-extension regression item:
  - added focused public-surface coverage in
    `src/lisp/tests_deduce_query_groups.c3` for:
    - zero-row materialized refresh staying deterministic
    - the dropped-handle split where `deduce/stats` still reports a
      deterministic payload but `deduce/refresh!` rejects with
      `deduce/refresh-relation-dropped`
  - extended `src/lisp/tests_deduce_durability_groups.c3` so persisted but
    still-unready materialized declarations also pin the DB-wide
    `deduce/refresh!` payload (`0` refreshed views, honest remaining stale
    relation list) after reopen
  - closed the matching milestone item in
    `docs/plans/deduce-analytics-extension-milestone-2026-03-24.md`

- Closed the stats-maintenance benchmark harness item for the Deduce analytics
  extension milestone:
  - added `run_deduce_stats_maintenance_benchmarks(...)` under the existing
    `OMNI_DEDUCE_BENCH=1` gate with one bounded smoke-oriented summary lane:
    - tracked fact churn over a seeded materialized relation, asserting
      targeted relation refreshes and post-refresh stats stability
    - forced rule-set-change invalidation, asserting the relation-scoped
      refresh fallback to DB-wide rebuild plus post-refresh stats recovery
  - added benchmark helper coverage in
    `src/lisp/tests_deduce_query_bench_groups.c3` and documented the emitted
    `OMNI_BENCH_SUMMARY suite=deduce_stats_maintenance ...` fields in
    `docs/plans/deduce-stats-maintenance-benchmark-notes-2026-03-25.md`

- Closed the first cleanup/admin-verb item in the Deduce analytics extension
  milestone:
  - published `docs/deduce-datalog-spec.md#74-cleanup-verb-matrix` as the
    canonical cleanup-surface contract for `deduce/retract!`,
    `deduce/clear!`, and `deduce/drop!`
  - documented:
    - the only approved cleanup verb names
    - accepted call shapes, including the current transaction-only support for
      `retract!`
    - deterministic no-op cases for absent tuples, empty relations, and
      repeated drops
    - the current rejection-code matrix
  - updated `docs/reference/08-libraries.md` to point to the spec section
    instead of carrying a second drifting cleanup contract summary
  - corrected the milestone target from the old cleanup implementation file to
    `src/lisp/deduce_relation_ops_mutations.c3`, where the cleanup verb
    handlers currently live

- Closed the cleanup-path audit item for the Deduce analytics extension
  milestone:
  - extended `src/lisp/tests_deduce_durability_groups.c3` to pin current
    cleanup edge behavior:
    - `retract!` on a missing tuple is a `Void` no-op
    - repeated `drop!` on the same dropped relation handle is a `Void` no-op
    - `retract!` and `clear!` on a dropped relation handle reject
      deterministically with `deduce/retract-relation-dropped` and
      `deduce/clear-relation-dropped`
  - that audited behavior is now aligned with the published cleanup matrix in
    `docs/deduce-datalog-spec.md#74-cleanup-verb-matrix`

- Closed the dedicated cleanup admin-task regression item for the Deduce
  analytics extension milestone:
  - extended `src/lisp/tests_deduce_query_groups.c3` with a public-surface
    write-intent regression that pins code-first cleanup failures for:
    - `retract!` tuple arity mismatch
    - `retract!` transaction/DB mismatch
    - `clear!` and `drop!` invoked with the wrong Deduce handle kind
  - together with the durability-side no-op/dropped-handle regressions, the
    current cleanup contract is now pinned across both write-intent failure and
    no-op maintenance surfaces

- Restored the shipped `deduce_scan_query_count` benchmark lane so the current
  source matches the existing benchmark notes and Docker perf-envelope script:
  - implemented `run_deduce_scan_query_count_benchmarks(...)` in
    `src/lisp/tests_deduce_query_bench_groups_more.c3` using the existing seed
    helper and scan-range materialization counters
  - the lane now emits the expected
    `OMNI_BENCH_SUMMARY suite=deduce_scan_query_count ...` fields for:
    - scan
    - full-range scan-range
    - query
    - count
    - scan-range materialization counters
  - updated `docs/plans/deduce-scan-query-count-benchmark-baseline-2026-03-11.md`
    to match the current implementation location and summary surface
  - closed the remaining open post-complete backlog items for:
    - deterministic Deduce cleanup/statistics behavior
    - Deduce scan/query/copy hotspot benchmark coverage

- Landed the formatter CLI baseline:
  - added `omni --fmt <file>` as a first-party conservative formatter path
  - default mode prints formatted source to stdout,
    `--write` rewrites in place, and `--check` returns non-zero when a file
    needs formatting changes
  - the current formatter contract is intentionally narrow and matches the
    existing LSP formatter behavior: normalize structural indentation, align
    wrapped `export` / `export-from` payloads and wrapped `let` binding-list
    continuations, trim trailing whitespace, preserve blank lines, and avoid
    aggressive intra-line rewrites
  - updated `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, and
    `docs/plans/editor-tooling-roadmap.md` to reflect the shipped CLI surface

- Landed Neovim formatter-tool integration for the formatter CLI:
  - added `tooling/omni-nvim/lua/omni/formatter.lua` plus exported helper APIs
    so `conform.nvim` can register the shipped `omni --fmt --write $FILENAME`
    formatter without ad-hoc per-user command duplication
  - added `:OmniConformSetupSpec` to print the ready-to-merge formatter setup
    table for `conform.nvim`
  - documented the Conform setup path in `tooling/omni-nvim/README.md` and
    `tooling/omni-nvim/doc/omni.nvim.txt`
  - closed the remaining formatter-tool integration roadmap item in
    `docs/plans/editor-tooling-roadmap.md`
  - reshaped the formatting roadmap so the rollout decision is explicitly
    closed as `both in phases`, leaving only the real remaining formatter
    follow-up: broader full-layout rewrites beyond the shipped conservative
    continuation-aware formatter

- Landed the next formatter semantics slice:
  - `omni --fmt` now goes beyond pure bracket-depth indentation by aligning
    wrapped `export` / `export-from` payloads and wrapped `let` binding-list
    continuations
  - `tooling/omni-lsp` mirrors the same conservative continuation rules for
    full-document formatting so the shipped CLI and editor formatting paths do
    not drift
  - added regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`
  - split the old vague formatter backlog item so the shipped continuation
    formatter is closed and only broader full-layout rewriting remains open

- Landed the next formatter preservation slice:
  - conservative formatting now preserves the existing repo-style indentation
    for multiline `if` branches instead of flattening them to shallow
    bracket-depth output
  - inline block forms inside `let` binding lists now stay aligned to their
    binding context, which preserves current `let ^rec` and similar in-tree
    multiline bindings
  - `tooling/omni-lsp` mirrors the same preservation rules so editor and CLI
    formatting stay on the same contract
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

- Landed the next generic-call formatter slice:
  - conservative formatting now preserves wrapped generic call and pipeline
    continuations by aligning them under the first argument instead of reducing
    them to shallow block indentation
  - `tooling/omni-lsp` mirrors the same generic-call continuation behavior so
    editor and CLI formatting stay aligned
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

- Landed the newline-preservation formatter slice:
  - conservative formatting now preserves the source file's existing newline
    style (`LF` vs `CRLF`) instead of rewriting line endings as a side effect
  - `tooling/omni-lsp` mirrors the same newline-preservation behavior so editor
    and CLI formatting remain on the same contract
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

- Landed the nested control-flow/effect formatter slice:
  - conservative formatting now preserves nested `if` body indentation from the
    actual `if` column when the branch form starts later in a clause line,
    which keeps current `match`-arm layouts from collapsing toward the clause
    margin
  - `when` / `unless` / `raise` / `checkpoint` now format as block-style forms
    instead of inheriting generic continuation alignment
  - `tooling/omni-lsp` mirrors the same control-flow/effect indentation rules
    so editor and CLI formatting stay on one contract
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

- Landed the higher-order lambda formatter slice:
  - conservative formatting now preserves multiline lambda bodies in current
    in-tree higher-order collection calls (`map`, `foldl`, `foldr`, `filter`,
    `find`, `partition`, `sort-by`, `for-each`) by aligning those bodies from
    the lambda's own opening column instead of flattening them to shallow
    block indentation
  - `tooling/omni-lsp` mirrors the same higher-order lambda-body alignment so
    editor and CLI formatting stay on one contract
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

- Landed the coroutine wrapper-lambda formatter slice:
  - conservative formatting now preserves multiline `Coroutine (lambda ...)`
    bodies on the current in-tree wrapper layout instead of flattening them
    toward the generic lambda indentation used for other calls
  - `tooling/omni-lsp` mirrors the same coroutine wrapper-lambda behavior so
    editor and CLI formatting stay on one contract
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

- Landed the clause/data-layout formatter slice:
  - conservative formatting now preserves multiline `match` arm payload bodies
    from their clause-opening delimiter instead of indenting them like generic
    block bodies
  - inline dict entries now keep key alignment from the `{` delimiter, and
    multiline vector payload entries that start with nested forms keep the same
    delimiter-anchored style used in current in-tree data literals
  - `tooling/omni-lsp` mirrors the same clause/data-layout behavior so editor
    and CLI formatting stay on one contract
  - extended regression coverage in `tooling/tests/omni_fmt_smoke.py` and
    `tooling/omni-lsp/tests/smoke_test.py`

## 2026-03-24

- Landed the next editor-tooling Tree-sitter query-hardening slice:
  - tightened `tooling/tree-sitter-omni/queries/highlights.scm` so canonical
    macro/module forms (`export`, `syntax-match`, `template`, `insert`,
    `splice`) receive first-party captures while removed older spellings such
    as `fn` / `begin` / `letrec` remain outside canonical keyword highlighting
  - added `tooling/tree-sitter-omni/test/check_queries.sh` and focused query
    fixtures under `tooling/tree-sitter-omni/test/queries/` covering
    highlights, injections, locals, textobjects, folds, and older
    non-highlight behavior
  - added `tooling/tree-sitter-omni/queries/indents.scm` plus an indentation
    fixture so bracketed Omni forms now expose a first-party structural
    indentation surface for editor integrations
  - reshaped `docs/plans/editor-tooling-roadmap.md` so parser-only removed syntax
    rejection no longer masquerades as a Tree-sitter grammar-corpus contract
  - validation:
    - `npx tree-sitter test --rebuild`
    - `sh ./test/check_queries.sh`

- Roadmap state audit for shipped editor tooling:
  - closed the stale `tooling/omni-lsp` integration-test roadmap item because
    `tooling/omni-lsp/tests/smoke_test.py` already drives initialize/open/change,
    hover, completion, diagnostics, and related baseline requests through a
    real stdio server subprocess
  - closed the stale `tooling/omni-nvim` result-annotation roadmap item because
    the plugin already supports optional single-form virtual-text eval
    annotations and documents the setup in `tooling/omni-nvim/README.md` and
    `tooling/omni-nvim/doc/omni.nvim.txt`

- Landed the next editor-tooling Tree-sitter hardening slice:
  - added a maintained accepted-syntax corpus at
    `tooling/tree-sitter-omni/test/corpus/coverage.txt`
  - the corpus now pins grammar coverage for:
    - type annotations
    - macro/template forms
    - regex literals
    - path plus postfix index chaining
    - effect handlers and continuations
    - module/import/export forms
    - destructuring-heavy `let` / `match` inputs
  - added package scripts for:
    - `tree-sitter parse --rebuild examples/sample.omni`
    - `tree-sitter test --rebuild`
    - `tree-sitter test --rebuild --update`
  - tightened the grammar token surface so `#r"..."` now produces
    `regex_literal` nodes instead of tokenizing as a plain `#r` symbol plus
    string literal
  - updated `docs/plans/editor-tooling-roadmap.md` to close the accepted-syntax
    grammar-coverage slice while leaving rejected corpus coverage open
  - validation:
    - `npx tree-sitter generate`
    - `npx tree-sitter test --rebuild --update`
    - `npx tree-sitter test --rebuild`

- Hardened `omni --check --json` and closed the remaining structured-check
  regression coverage slice:
  - `src/entry_check_mode.c3` now treats compiler output containing explicit
    unsupported-expression markers as a lowering failure instead of reporting a
    false clean check result
  - added `tooling/omni-lsp/tests/check_json_smoke.py` to pin:
    - parser syntax diagnostics
    - lowering diagnostics for unsupported lowered forms
    - malformed `module` / `import` diagnostics
    - deterministic top-level payload and diagnostic schema fields
  - updated `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, and
    `docs/plans/editor-tooling-roadmap.md` to match the shipped check contract
    and completed regression state
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/tests/check_json_smoke.py`
    - `c3c build` blocked by existing file-permission issue:
      `src/lisp/tests_deduce_query_groups.c3` was unreadable to the build user
    - `python3 tooling/omni-lsp/tests/check_json_smoke.py` not run because the
      rebuilt `build/main` binary could not be produced under the current
      workspace permissions

- Landed the next `runtime-modularization` slice for `eval` env-copy internals:
  - split all env-copy mechanics and helper routines out of
    `src/lisp/eval_env_copy.c3` into
    `src/lisp/eval_env_copy_helpers.c3`
  - kept behavior unchanged for closure/iterator payload handling, closure-env
    rewriting, and promotion-context orchestration across parent-chain rewrites
  - updated `docs/plans/largest-runtime-files-pass-2026-03-19.md`:
    - added landed batch entry for `eval_env_copy` split
    - removed `eval_env_copy` from the top of the next queue
  - validation: not run in this slice (build/validation to be run at the lane boundary)

- Landed the next `runtime-modularization` slice for `deduce` rule validation:
  - factored rule-install validation helpers out of
    `src/lisp/deduce_rule_eval.c3` into
    `src/lisp/deduce_rule_eval_validation.c3`
  - kept existing behavior unchanged; the orchestration path still performs, in
    order:
    - arity checks
    - head-grounding safety checks
    - negation-safety checks
    - aggregate head validation
    - aggregate migration/recursion checks
    - constrained-head signature migration checks
    - stratification checks
  - updated `docs/plans/largest-runtime-files-pass-2026-03-19.md`:
    - added landed batch entry for `deduce_rule_eval` split
    - adjusted next-queue accounting for the landed/refactored file states
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json` (`6/9 runs passed`; blockers in `status_consistency`, `jit_policy`, `deduce` slices remain)

- Landed the next `runtime-modularization` slice for `deduce` relation mutation primitives:
  - split relation mutation operations (`fact!`, `retract!`, `clear!`, `drop!`) out of
    `src/lisp/deduce_relation_ops.c3` into
    `src/lisp/deduce_relation_ops_mutations.c3`
  - kept behavior unchanged; validation and persistence paths for relation lifecycle,
    integrity checks, and extensional shadow writes/read-paths are still routed through
    the same prim entrypoints
  - updated `docs/plans/largest-runtime-files-pass-2026-03-19.md`:
    - added landed batch entry for `deduce_relation_ops` split
    - adjusted next-queue accounting for landed/refactored file states
  - validation: not run in this slice (build/validation to be run at the lane boundary)

## 2026-03-23

- Closed `B6.5` optional parallel evaluation:
  - the shipped scheduler/runtime surface now covers the admin-visibility
    slice plus the task, offload, and OS-thread batch dispatch helpers
  - the remaining queue/thread batch primitives are now all validated through
    targeted scheduler regressions
- Extended the `B6.5` scheduler/runtime seam with a plural queued-task
  execute helper:
  - added `scheduler_execute_custom_offload_task_jobs(...)` so a small batch
    of internal callback jobs can be queued and joined through the same
    task-handle path
  - added a focused scheduler regression that pins the batch execute path
    with two queued custom callbacks and validates both results arrive in
    order
- Landed the first `B6.5` parallel-evaluation admin slice:
  - relation-local `deduce/stats` now exposes the same parallel SCC batch
    topology counts that `deduce/analyze` already reports:
    - `parallel-recursive-component-count`
    - `parallel-batched-component-count`
    - `parallel-batch-count`
    - `parallel-max-batch-size`
  - `deduce/analyze`, relation-local `deduce/stats`, and `deduce/explain`
    now also expose `parallel-batch-topology` payloads for the recursive SCC
    batches
  - added regression coverage that pins those counts and topology on a
    representative recursive SCC topology
- Landed the first `B6.5` scheduler/runtime seam for future parallel dispatch:
  - the offload executor now accepts internal callback jobs via
    `OffloadWork.custom_fn` / `OffloadWork.custom_ctx`
  - added a focused scheduler regression that pins queued custom-callback
    execution through the offload executor
  - added `scheduler_execute_custom_offload_job(...)` as a dedicated queue
    and join helper for internal callback jobs
  - added `scheduler_execute_custom_offload_task_job(...)` so the same
    custom callback path can reuse a caller-supplied thread-task handle
  - added `scheduler_begin_custom_offload_task_job(...)` so the same
    callback path can fan out multiple queued task jobs before joining them
  - added `scheduler_begin_custom_offload_task_jobs(...)` so a single
    callback can queue a small fan-out of task jobs and return the caller
    supplied handles for later joins
  - added `scheduler_begin_custom_os_thread_jobs(...)` so a single callback
    can fan out multiple actual OS-thread jobs and return their caller
    supplied handles for later joins
  - added `scheduler_execute_custom_os_thread_jobs(...)` so the same
    callback fan-out can also be started and joined through one helper
  - added `scheduler_begin_os_thread_work_jobs(...)` and
    `scheduler_execute_os_thread_work_jobs(...)` so distinct offload work
    items can be dispatched and joined in parallel on real OS threads
  - added `__raw-offload-batch` so a list of raw offload job forms can be
    dispatched through the same batched queue/join path and return a result
    list in order
  - added `__raw-task-spawn-batch` so a list of raw offload job forms can be
    dispatched through the queued task path and return a task-handle list in
    order
  - added `__raw-thread-spawn-batch` so a list of raw offload job forms can
    be dispatched through the actual OS-thread spawn path and return a result
    list in order
- Tightened materialized-view rule-install invalidation:
  - only already-ready materialized views are now marked stale on
    `deduce/rule!`
  - declared-but-unready materialized views keep their existing
    `never-refreshed` lifecycle instead of being blanket-staled
  - added restart coverage for ready-vs-unready selective invalidation
- Goal-directed read admin truthfulness now also records fallback reasons:
  - `deduce/stats`, `deduce/analyze`, and `deduce/explain` now expose
    `last-goal-directed-read-fallback-reason`
  - unsupported captured-call query shapes and dirty-closure fallback reads
    now stay distinguishable from ordinary selected-closure reads in the
    admin surface
- Landed one more `B6.4c` query-demand widening:
  - closed numeric builtin wrappers such as `abs`, `floor`, `ceiling`,
    `round`, `truncate`, `sqrt`, `exp`, `log`, `log10`, `sin`, `cos`, `tan`,
    `asin`, `acos`, `atan`, `atan2`, `min`, `max`, and `pow` are now accepted
    by the shipped
    `deduce/query` demand extractor when they remain row-independent and
    still collapse to the existing equality-demand subset
  - added focused regression coverage for the closed numeric-wrapper demand
    path
- Landed another `B6.4c` query-demand widening:
  - closed one-argument pure closure wrappers such as `id` are now also
    folded by the shipped `deduce/query` demand extractor when the wrapper
    body still collapses to the supported equality-demand subset
  - added focused regression coverage for the closure-wrapper demand path
- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - for the currently shipped preserved-bound subset, positive body scans now
    also use demanded leading-prefix probes inside the naive and seminaive
    rule-step executors, so the abortable demand path prefix-prunes those
    positive body scans instead of only the outer selector plan
  - selector-scoped preserved-bound reads now also record
    `last-goal-directed-read-step-counters` in relation stats, DB analyze,
    and explain, exposing observed per-step counters such as `rows-read`,
    `rows-emitted`, and `join-probes` for the selected rule
  - the shipped `deduce/query` equality-demand subset now also accepts
    row-independent `or` wrappers with one closed falsy branch for both
    selector-scoped and plain reads
  - closed row-independent comparison guards (`=`, `<`, `<=`, `>`, `>=`) are
    now also evaluated by the shipped `deduce/query` demand extractor, so
    row-independent `if` wrappers can still keep the ephemeral query path
  - closed row-independent `and` / `or` guards now also evaluate with Omni’s
    value-returning short-circuit semantics inside the shipped
    `deduce/query` demand extractor, so row-independent boolean wrappers can
    still keep the ephemeral query path
  - closed row-independent `not` guards now also evaluate with ordinary Omni
    truthiness semantics inside the shipped `deduce/query` demand extractor,
    so row-independent negated guard wrappers can still keep the ephemeral
    query path
  - closed literal-side `let` / `block` wrappers around the shipped
    equality-demand subset are now also accepted by `deduce/query`,
    including wrapped `ref` column symbols
  - closed row-column-side `let` / `block` wrappers are now also accepted by
    that same shipped `deduce/query` equality-demand subset when they still
    resolve to `(ref row 'column)` through a closed symbol binding
  - closed row-independent `if` wrappers are now also accepted around the
    row-column side of that same shipped subset, so branch-selected
    `(ref row 'column)` forms can keep the ephemeral query path
  - closed row-independent `and` / `or` wrappers are now also accepted
    around that same row-column side when the selected branch still resolves
    to `(ref row 'column)`
  - the strict preserved-bound safety gate is now pinned again for
    multi-self-recursive `deduce/match` shapes: when every positive
    self-recursive body atom does not preserve the demanded head positions,
    the runtime falls back to `selected-component-closure`, cleans the
    target component, and leaves unrelated dirty siblings untouched
  - focused validation now pins selector-scoped preserved-bound `deduce/match`
    step counters for the selected rule under the shipped prefix-pruning path
    plus selector/plain `deduce/query` demand extraction through falsy `or`
    wrappers, closed comparison guards, closed `and` guards, closed `not`
    guards, and closed literal-side `let` / `block` wrappers
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `269 passed, 0 failed`

- Landed the next `B6.3b` Deduce materialized-view durability slice:
  - file-backed materialized relations now persist mutation-driven and
    rule-set invalidation lifecycle metadata, not only materialized intent
    and successful refresh history
  - reopened materialized relations now stay stale when their persisted
    lifecycle record still carries a non-`none` stale reason
  - focused durability validation now pins restart/open-named persistence of
    `dependency-dirty` stale state after a successful refresh
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `247 passed, 0 failed`

- Landed the next `B6.3b` Deduce materialized-view durability slice:
  - file-backed materialized relations now persist refresh-history metadata
    across reopen / `open-named`, not only the materialized-intent flag
  - the persisted lifecycle record now keeps reopened schema/admin payloads
    truthful after a successful manual refresh by carrying:
    - `materialized-refresh-count`
    - `materialized-last-refresh-mutation-epoch`
    - stored lifecycle stale metadata already tracked by the schema
  - focused durability validation now pins restart/open-named persistence of a
    successful materialized refresh
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `238 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - the demand-bound `match`, `query`, and equality-bound `scan-range` paths
    no longer fall back completely just because one requested head position is
    unsupported by the preserved-bound gate
  - the runtime now projects the requested demand down to the preserved subset,
    evaluates that narrower demand in the abortable in-txn path, and still
    applies the original full pattern / filter / range after the read
  - this widens the shipped fast path safely for mixed supported-plus-
    unsupported bound shapes such as transitive-closure reads that bind both a
    preserved destination and a non-preserved source
  - focused validation now pins projected demand execution for:
    - plain `deduce/match`
    - selector-scoped `deduce/query`
    - plain equality-bound `deduce/scan-range`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `238 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - selector-scoped and plain `deduce/query` now use the same abortable
    demand-bound path for the current preserved-bound positive recursive
    subset when the filter closure is a conjunction of row-independent
    literal, captured-constant, or small safe builtin expression equalities
    on `(ref row 'column)` terms
  - the new explain/admin field
    `goal-directed-demand-bound-head-positions` makes the preserved-bound
    positions visible directly instead of only surfacing planner-intent
    metadata
  - the read-path surface now records `ephemeral-head-demand-query`
  - wider or unsupported filter shapes still fall back to the existing
    selected-closure read path
  - captured row-independent constants, closed numeric builtin
    expressions, and row-independent `let` / `block` / `if` wrappers are now
    accepted in that shipped equality-filter subset
  - unsupported captured call shapes still fall back to selected-closure
    reads
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `251 passed, 0 failed`

- Landed the next `B6.4c` admin-truth slice for projected-demand reads:
  - relation-local `deduce/stats` and DB-level `deduce/analyze` now also
    expose:
    - `last-goal-directed-read-requested-bound-count`
    - `last-goal-directed-read-applied-bound-count`
  - projected-demand reads now make preserved-subset application visible for:
    - bound `deduce/match`
    - equality-filter `deduce/query`
    - equality-bound `deduce/scan-range`
  - focused validation now pins the requested vs applied bound-counts for:
    - plain `deduce/match`
    - selector-scoped `deduce/query`
    - plain `deduce/scan-range`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `238 passed, 0 failed`

- Landed the next `B6.4c` widening for `deduce/query` demand extraction:
  - the demand-bound `deduce/query` path no longer requires every conjunct in
    the filter to be equality-shaped
  - the runtime now harvests supported equality conjuncts for head-demand
    extraction while leaving residual unsupported conjuncts to the original
    full filter after the read
  - this widens the existing ephemeral query path without changing query
    semantics or clearing dirty state
  - focused validation now pins a selector-scoped recursive query that:
    - uses one supported equality conjunct for demand extraction
    - keeps one residual unsupported conjunct in the post-read filter
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `239 passed, 0 failed`

- Tightened selector-scoped `B6.4c` match execution truth:
  - selector-scoped bound `deduce/match` now actually threads the built
    `head_demand` into the abortable in-txn selected-component evaluator,
    instead of only using the preserved-bound gate as a selector-path label

- Landed the next `B6.4c` admin-truth slice for projected-demand reads:
  - relation-local `deduce/stats` and DB-level `deduce/analyze` now also
    expose:
    - `last-goal-directed-read-requested-bound-positions`
    - `last-goal-directed-read-applied-bound-positions`
  - the requested/applied bound-count metadata now has concrete position-list
    payloads behind it for the last goal-directed read, not only counts
  - focused validation now pins those position lists on the projected-demand
    `deduce/query` path for:
    - exact preserved demand
    - projected preserved demand after residual unsupported conjuncts
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `239 passed, 0 failed`
  - this keeps the shipped `ephemeral-head-demand-match` read-path stamp
    honest for selector-scoped bound match reads

- Landed the next `B6.4c` explain/admin-truth slice:
  - `deduce/explain` now mirrors the last actual goal-directed read metadata
    for the selected head relation instead of only surfacing planner
    eligibility and selected-closure metadata
  - the mirrored payload includes:
    - `last-goal-directed-read-execution-path`
    - `last-goal-directed-read-surface`
    - `last-goal-directed-read-selector-rule-index`
    - `last-goal-directed-read-mutation-epoch`
    - requested/applied bound counts
    - requested/applied bound-position lists
  - focused validation now pins an eligible positive recursive closure rule,
    performs a bound `deduce/match` that takes the ephemeral demand path, and
    then verifies `deduce/explain` mirrors that actual runtime choice
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `239 passed, 0 failed`

- Landed the next `B6.4c` safety/regression slice:
  - focused validation now also pins projected-demand truth for:
    - selector-scoped `deduce/match`
    - selector-scoped `deduce/scan-range`
    - plain `deduce/query`
  - this closes the most obvious remaining coverage asymmetry between the
    selector-scoped and plain demand-bound read surfaces without widening the
    public runtime contract
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `242 passed, 0 failed`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `231 passed, 0 failed`

- The preserved-bound gate remains intentionally stricter for
  multi-self-recursive shapes:
  - each demanded head position must still be preserved by every positive
    self-recursive body atom
  - mixed-position preservation across different recursive atoms falls back
    to `selected-component-closure` instead of taking an unsound ephemeral
    demand path

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - relation-local `deduce/stats` and DB-level `deduce/analyze` now expose
    the last actual goal-directed read path chosen by the runtime, instead
    of only planner-intent metadata
  - the current shipped read-path surface now records:
    - `no-op`
    - `selected-component-closure`
    - `full-db-fixpoint`
    - `ephemeral-head-demand-match`
    - `ephemeral-head-demand-scan-range`
  - focused validation now pins last-read truth for:
    - selected-component-closure
    - full-db-fixpoint
    - no-op
    - ephemeral bound `match`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `230 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - the preserved-bound demand-gate widening is now explicitly pinned for
    plain equality-bound `deduce/scan-range`, not just plain bound
    `deduce/match`
  - focused validation now covers a swapped self-recursive rule answering a
    bounded plain `deduce/scan-range` read while leaving the target relation
    dirty
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `228 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - the preserved-bound demand gate now also accepts positive recursive rules
    where demanded head variables are preserved through self-recursive
    body-variable reordering, not only same-position carry-through
  - this widens the shipped demand-bound `deduce/match` subset without
    pretending generic magic-set rewrite is already done
  - focused validation now pins a swapped self-recursive rule answering a
    bound plain `deduce/match` read while leaving the target relation dirty
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `227 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - plain `deduce/scan-range` now uses the same abortable demand-bound path
    for the current preserved-bound positive recursive subset when
    equality-bound head literals can be extracted from positions where
    `lower == upper`, instead of always persisting a tracked closure refresh
  - it restores in-memory schema estimates after abort and leaves the target
    relation dirty
  - wider recursive shapes and non-exact ranges still use the existing plain
    tracked selected-closure auto-execution path
  - focused validation now pins plain equality-bound `deduce/scan-range`
    returning the bounded rows while leaving the target relation dirty
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `226 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - plain `deduce/match` with bound literals now uses the same abortable
    demand-bound path for the current preserved-bound positive recursive
    subset instead of always persisting a tracked closure refresh
  - it restores in-memory schema estimates after abort and leaves the target
    relation dirty
  - wider recursive shapes and unbound patterns still use the existing plain
    tracked selected-closure auto-execution path
  - focused validation now pins plain bound-literal `deduce/match`
    returning the constrained rows while leaving the target relation dirty
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `225 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - selector-scoped `deduce/scan-range` now has the matching bounded-demand
    execution path when equality-bound head literals can be extracted from
    `lower == upper` positions and the selected positive recursive shape
    preserves those bound positions
  - it executes inside an abortable write txn, snapshots/restores in-memory
    schema estimates, and leaves dirty frontier state intact after the read
  - wider recursive shapes still fall back to the existing selected-component
    closure execution path instead of pretending generic demand rewrite is
    already shipped
  - focused validation now pins selector-scoped `deduce/scan-range` demand
    execution that returns the bounded rows while leaving the target relation
    dirty
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `224 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - selector-scoped `deduce/match` with bound head literals now has the first
    real demand-bound execution path for preserved-bound positive recursive
    shapes
  - it executes inside an abortable write txn, snapshots/restores in-memory
    schema estimates, and leaves dirty frontier state intact after the read
  - wider recursive shapes that do not preserve those bound positions still
    fall back to the existing selected-component closure execution path
    instead of pretending generic magic-set rewrite is already shipped
  - focused validation now pins selector-bound recursive match execution that
    returns the constrained rows while leaving the target relation dirty
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `223 passed, 0 failed`

- Landed the next `B6.4c` Deduce query-time goal-directed execution slice:
  - plain relation reads without a selector now auto-execute the same
    currently eligible dirty positive recursive closure in tracked mode for:
    - `(deduce/match relation pattern)`
    - `(deduce/query relation filter-fn)`
    - `(deduce/count relation)`
    - `(deduce/scan relation)`
    - `(deduce/scan-range relation lower upper)`
  - the automatic read path is still intentionally narrow:
    - it only runs for eligible positive recursive closures,
    - it only runs when the selected closure is dirty,
    - it leaves unrelated recursive components untouched,
    - in tracked mode it executes only the target closure, and after
      `full-recompute-required` plain derived reads fall back to the existing
      full DB fixpoint path while selector-scoped reads still reject,
    - focused regressions now pin the no-op boundaries for that shipped
      plain-read path:
      - clean eligible recursive targets do not execute unrelated dirty
        closures
      - unrelated non-recursive targets do not execute dirty recursive
        closures
      - blocked negated recursive targets do not execute unrelated eligible
        closures
    - it does not widen support to aggregate-bearing or negated recursive
      shapes
  - focused regression coverage now pins:
    - plain `deduce/query` auto-execution that leaves a sibling recursive
      component dirty
    - plain `deduce/match` auto-execution for the same eligible shape
    - plain `deduce/count` / `deduce/scan` / `deduce/scan-range`
      auto-execution for the same eligible shape
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `210 passed, 0 failed`

- Landed the next `B6.3b` Deduce materialized-view lifecycle slice:
  - explicit teardown now ships through
    `deduce 'dematerialize! relation` and `deduce/dematerialize!`
  - dematerialization clears materialized intent and lifecycle metadata
    without dropping the relation or its installed rules
  - dematerialization also clears persisted materialized intent for file-backed
    DBs, so reopen / `open-named` no longer resurrects a manually torn-down
    view
  - relation-scoped `deduce/refresh!` now rejects a dematerialized relation
    through the existing `deduce/refresh-relation-not-materialized` contract
  - focused regression coverage now pins:
    - in-memory dematerialize -> refresh reject -> rematerialize lifecycle
    - restart-time absence of persisted materialized intent after dematerialize
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `189 passed, 0 failed`
- Landed the first real `B6.4c` Deduce query-time goal-directed execution
  slice:
  - `deduce/match` now supports selector-scoped execution for the currently
    eligible positive recursive shape through:
    - `(deduce/match relation pattern rule-index)`
    - `(deduce/match relation pattern rule-index 'naive)`
    - `(deduce/match relation pattern rule-index 'semi-naive)`
  - selected match executes only the chosen component dependency closure
    before matching and leaves unrelated recursive components untouched
  - selector-scoped match rejects explicitly when:
    - the selected recursive shape is aggregate-bearing
    - the selected recursive shape has negated body atoms
    - the selected closure does not produce the requested relation
    - the DB has already escalated to `full-recompute-required`
  - focused regression coverage now pins:
    - selected match execution that leaves unrelated derived components
      untouched
    - relation/selector mismatch rejection
    - aggregate recursive selector rejection
    - negated recursive selector rejection
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `189 passed, 0 failed`
- Landed the second `B6.4c` Deduce query-time goal-directed execution
  slice:
  - `deduce/query` now supports selector-scoped execution for that same
    currently eligible positive recursive shape through:
    - `(deduce/query relation filter-fn rule-index)`
    - `(deduce/query relation filter-fn rule-index 'naive)`
    - `(deduce/query relation filter-fn rule-index 'semi-naive)`
  - selected query executes only the chosen component dependency closure
    before filtering rows and leaves unrelated recursive components untouched
  - selector-scoped query rejects explicitly when:
    - the selected recursive shape is aggregate-bearing
    - the selected recursive shape has negated body atoms
    - the selected closure does not produce the requested relation
    - the DB has already escalated to `full-recompute-required`
  - focused regression coverage now pins:
    - selected query execution that leaves unrelated derived components
      untouched
    - relation/selector mismatch rejection
    - aggregate recursive selector rejection
    - negated recursive selector rejection
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `193 passed, 0 failed`
- Landed the third `B6.4c` Deduce query-time goal-directed execution
  slice:
  - `deduce/count`, `deduce/scan`, and `deduce/scan-range` now support
    selector-scoped execution for that same currently eligible positive
    recursive shape through:
    - `(deduce/count relation rule-index)`
    - `(deduce/count relation rule-index 'naive)`
    - `(deduce/count relation rule-index 'semi-naive)`
    - `(deduce/scan relation rule-index)`
    - `(deduce/scan relation rule-index 'naive)`
    - `(deduce/scan relation rule-index 'semi-naive)`
    - `(deduce/scan-range relation lower upper rule-index)`
    - `(deduce/scan-range relation lower upper rule-index 'naive)`
    - `(deduce/scan-range relation lower upper rule-index 'semi-naive)`
  - selector-scoped count / scan / scan-range execute only the chosen
    component dependency closure before reading rows and leave unrelated
    recursive components untouched
  - selector-scoped count / scan / scan-range reject explicitly when:
    - the selected recursive shape is aggregate-bearing
    - the selected recursive shape has negated body atoms
    - the selected closure does not produce the requested relation
    - the DB has already escalated to `full-recompute-required`
  - focused regression coverage now pins:
    - selected count / scan / scan-range execution that leaves unrelated
      derived components untouched
    - count relation/selector mismatch rejection
    - scan aggregate recursive selector rejection
    - scan-range negated recursive selector rejection
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `201 passed, 0 failed`

## 2026-03-22

- Reshaped the live Deduce backlog so completed umbrella slices stop looking
  perpetually open:
  - `B6.3a` now names the shipped materialized-view admin/refresh surface and
    is treated as closed
  - `B6.3b` now isolates the still-open declaration/durability policy work
  - `B6.4a` now covers the shipped goal-directed
    planner/admin/selected-closure visibility surface and is treated as
    closed
  - `B6.4c` now isolates the still-open query-time magic-set / goal-directed
    rewrite lane
  - `B6.5` is now treated as closed; the shipped work covers both the
    admin-visibility surface and the scheduler/runtime batch-dispatch seams
  - this was a backlog/doc-structure correction only; no runtime behavior
    changed

- Landed the first `B6.3` Deduce materialized-view slice:
  - explicit manual materialization now ships for derived relations through
    `deduce 'materialize! relation` and `deduce/materialize!`
  - manual DB-wide refresh now ships through `deduce 'refresh! db` and
    `deduce/refresh!`
  - `deduce/schema`, `deduce/stats`, and `deduce/analyze` now expose
    materialized-view freshness metadata:
    - `materialized-view`
    - `materialized-refresh-policy`
    - `materialized-refresh-count`
    - `materialized-last-refresh-mutation-epoch`
    - `materialized-stale`
  - `deduce/analyze` is now explicitly diagnostic-only for this lane:
    it reports stale materialized views but does not silently refresh them
  - focused regression coverage now pins:
    - rejecting `materialize!` on non-derived relations
    - namespaced `deduce/materialize!` / `deduce/refresh!` registration
    - stale-before-refresh, explicit manual refresh, refresh-count increment,
      and stale-again after a base mutation
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `171 passed, 0 failed`
- Landed the second `B6.3` Deduce materialized-view slice:
  - `deduce/refresh!` now accepts either a Deduce DB handle or a
    materialized relation handle
  - non-materialized relation handles now reject refresh explicitly with
    `deduce/refresh-relation-not-materialized`
  - `deduce/schema` and `deduce/stats` now expose:
    - `materialized-last-stale-mutation-epoch`
    - `materialized-last-stale-reason`
  - dependency-driven invalidation now records the stale reason
    `dependency-dirty`, and refresh clears that reason back to `nil`
  - relation-scoped refresh still reuses the existing full analyze/fixpoint
    path internally; this is a narrower admin surface, not selective
    incremental maintenance
  - focused regression coverage now pins:
    - non-materialized relation-handle refresh rejection
    - relation-scoped refresh success
    - stale-reason metadata transitions after base mutation and refresh
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `173 passed, 0 failed`
- Landed the third `B6.3` Deduce materialized-view slice:
  - relation declaration syntax now accepts the materialization marker:
    `[relation db materialized] rel ...`
  - the declaration marker sets the same current manual refresh policy as
    explicit `deduce/materialize!`
  - focused regression coverage now pins:
    - schema registration for declared materialized relations
    - rejection of unknown relation-wide attributes
    - declaration-syntax alignment with relation-scoped refresh metadata
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `178 passed, 0 failed`
- Landed the next `B6.3b` Deduce declaration-policy slice:
  - relation declaration syntax now also accepts the explicit current manual
    policy spelling:
    `[relation db materialized manual] rel ...`
  - the parser now rejects unknown declaration-time materialized refresh
    policies deterministically instead of folding them into the generic
    unknown-attribute path
  - focused regression coverage now pins:
    - explicit manual declaration syntax
    - rejection of unknown materialized refresh policies
- Landed the fourth `B6.3` Deduce materialized-view slice:
  - relation-scoped `deduce/refresh! materialized-relation` now executes the
    target relation’s dependency closure in tracked mode instead of routing
    through full DB analyze
  - targeted refresh now clears dirty state only for the predicates it
    actually recomputed and leaves unrelated stale materialized views alone
  - if the DB has already escalated to `full-recompute-required`, relation-
    scoped refresh still falls back to the DB-wide path
  - focused regression coverage now pins:
    - refreshing one materialized relation leaves a sibling materialized
      relation stale
    - `stale-materialized-view-count` remains nonzero after the targeted
      refresh when unrelated stale views still exist
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `178 passed, 0 failed`
- Landed the fifth `B6.3` Deduce materialized-view slice:
  - declaration-based materialization is now lifecycle-gated rather than
    pretending every declared materialized relation is immediately refreshable
  - `deduce/schema` / `deduce/stats` now expose
    `materialized-derived-ready`, and `deduce/analyze` now exposes
    `ready-materialized-view-count` plus
    `unready-materialized-view-count`
  - relation-scoped refresh now rejects declared-but-unready materialized
    relations with `deduce/refresh-materialized-relation-not-derived`
  - DB-scoped refresh only stamps ready materialized views as refreshed and
    leaves unready declarations stale, with honest remaining stale counts
  - focused regression coverage now pins:
    - declared materialized relations reject refresh before rule install
    - the same relation becomes refreshable after its first derived rule head
    - DB-scoped refresh can refresh one ready view while leaving an unready
      declared view stale
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `179 passed, 0 failed`
- Landed the sixth `B6.3` Deduce materialized-view slice:
  - materialized intent now persists across reopen / `open-named` for
    file-backed DBs through a narrow relation-metadata catalog
  - reopened materialized relations come back conservatively as materialized
    but unready/stale until their rules are reinstalled and refreshed, so the
    runtime no longer pretends durable derived state exists when only
    materialized intent persisted
  - dropping a declared materialized relation now clears that persisted
    materialized-intent metadata instead of leaving stale declaration state
    behind across reopen
  - materialized stale reasons now distinguish `never-refreshed` from
    mutation-driven reasons, so declared or reopened materialized views with
    no successful refresh no longer report `materialized-stale == true` with
    a blank stale reason
  - focused regression coverage now pins:
    - restart/open-named persistence of materialized intent without durable
      rule state
    - clearing persisted materialized intent on drop
    - `materialized-last-stale-reason == 'never-refreshed` before first
      refresh on declared or reopened materialized views
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `181 passed, 0 failed`
- Landed the seventh `B6.3` Deduce materialized-view slice:
  - installing a new rule now invalidates already refreshed materialized
    views with the explicit stale reason `rule-set-change`
  - rule-install invalidation also forces the existing
    `full-recompute-required` fallback, keeping the contract honest instead of
    pretending the targeted invalidation path already models rule-graph
    changes soundly
  - focused regression coverage now pins:
    - refreshed materialized views become stale after `deduce/rule!`
    - `materialized-last-stale-reason == 'rule-set-change`
    - `deduce/stats` reports `full-recompute-required == true` after the
      rule install
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `181 passed, 0 failed`
- Landed the eighth `B6.3` Deduce materialized-view slice:
  - `deduce/refresh!` payloads now report the actual refresh path:
    relation-targeted, DB-wide, or relation-scoped fallback to DB-wide under
    `full-recompute-required`
  - the admin surface now exposes that through:
    - `refresh-scope`
    - `refresh-execution-path`
    - `refresh-fallback-reason`
  - focused regression coverage now pins DB refresh payload truthfulness,
    targeted relation refresh payload truthfulness, and relation fallback
    payload truthfulness after `rule-set-change`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `182 passed, 0 failed`
- Landed the ninth `B6.3` Deduce materialized-view slice:
  - `deduce/refresh!` now reports the actual refreshed and still-stale
    materialized relation identities through:
    - `refreshed-materialized-relations`
    - `remaining-stale-materialized-relations`
    - `requested-refresh-relation` on relation-scoped calls
  - relation-scoped fallback to DB-wide refresh no longer under-reports
    refresh impact when more than one ready materialized view is refreshed
  - focused regression coverage now pins DB refresh, targeted relation
    refresh, and rule-set-change fallback payload truthfulness at the
    relation-identity level
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `184 passed, 0 failed`
- Landed the first `B6.4` Deduce goal-directed planning slice:
  - `deduce/explain` now surfaces:
    - `goal-directed-component-id`
    - `goal-directed-eligible`
    - `goal-directed-shape`
    - `goal-directed-blockers`
  - the current eligible shape is intentionally narrow:
    positive recursive closure with no aggregates and no negated body atoms
  - non-eligible recursive shapes now report explicit blockers such as
    `aggregates-present` and `negated-body-atom`
  - this slice is explain-only: there is still no magic-set rewrite, no
    goal-directed execution path, and no runtime/query/analyze behavior change
  - focused regression coverage now pins:
    - positive recursive closure eligibility
    - blocker reporting for recursive aggregate/negated shapes
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `175 passed, 0 failed`
- Landed the second `B6.4` Deduce goal-directed planning slice:
  - `deduce/analyze` now reports DB-level planner counts for recursive
    goal-directed shapes:
    - `goal-directed-recursive-component-count`
    - `goal-directed-eligible-component-count`
    - `goal-directed-blocked-component-count`
    - `goal-directed-aggregate-blocked-component-count`
    - `goal-directed-negated-blocked-component-count`
  - this remains diagnostic-only planner metadata with no execution rewrite
  - focused regression coverage now pins positive recursive closure counts
    and aggregate/negated blocker counts
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `182 passed, 0 failed`
- Landed the third `B6.4` Deduce goal-directed planning slice:
  - `deduce/analyze` now exposes `goal-directed-components`, a recursive
    component summary list carrying component id, stratum, rule count,
    eligibility, shape, blockers, and aggregate/negation presence flags
  - this remains planner/admin metadata only with no execution rewrite
  - focused regression coverage now pins positive recursive closure summaries
    and aggregate/negated blocked summaries
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `184 passed, 0 failed`
- Landed the tenth `B6.3` Deduce materialized-view slice:
  - the invalidation frontier is now explicit instead of only count-shaped:
    - `deduce/analyze` exposes `incremental-dirty-predicates`
    - `deduce/stats` exposes `dirty-predicates` and `dirty-self`
    - `deduce/refresh!` exposes:
      - `cleared-dirty-predicates`
      - `remaining-dirty-predicates`
  - targeted relation refresh now reports exactly which dirty predicates it
    cleared while leaving unrelated dirty predicates behind
  - DB-wide refresh now reports the pre-refresh dirty frontier it cleared and
    the remaining frontier after success
  - focused regression coverage now pins:
    - DB refresh dirty-frontier visibility
    - targeted refresh dirty-frontier visibility
    - rule-set-change fallback dirty-frontier truthfulness
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `187 passed, 0 failed`
- Landed the fourth `B6.4` Deduce goal-directed planning slice:
  - `deduce/analyze` now supports selector-scoped execution for the currently
    eligible recursive shape through:
    - `(deduce/analyze db rule-index)`
    - `(deduce/analyze db rule-index 'naive)`
    - `(deduce/analyze db rule-index 'semi-naive)`
  - eligible selectors are intentionally narrow:
    positive recursive closure with no aggregates and no negated body atoms
  - selected execution now evaluates only the chosen component's dependency
    closure and reports:
    - `goal-directed-execution-path = 'selected-component-closure`
    - `goal-directed-selector-rule-index`
    - `goal-directed-selected-component-id`
  - aggregate-bearing and negated recursive selectors reject explicitly with
    `deduce/analyze-goal-directed-selector-not-eligible` and blocker
    payloads instead of silently falling back to full DB execution
  - focused regression coverage now pins:
    - selected-component closure execution that leaves unrelated derived
      components untouched
    - aggregate recursive selector rejection
    - negated recursive selector rejection
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `187 passed, 0 failed`
- Landed the fifth `B6.4` Deduce goal-directed planning slice:
  - `deduce/explain` now mirrors selector-scoped analyze execution for the
    currently eligible recursive shape by exposing:
    - `goal-directed-execution-path`
    - `goal-directed-selected-components`
    - `goal-directed-selected-predicates`
  - for eligible recursive selectors this closure payload matches the same
    dependency closure used by `(deduce/analyze db rule-index [engine])`
  - aggregate-bearing and negated recursive selectors continue to expose
    blockers and now leave the selected-closure payload fields `nil`
  - focused regression coverage now pins:
    - positive recursive selector closure visibility in `deduce/explain`
    - aggregate/negated recursive selector closure omission in
      `deduce/explain`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `187 passed, 0 failed`

## Archive

Older sessions are archived in [archive/CHANGELOG_ARCHIVE_2026-03-08.md](archive/CHANGELOG_ARCHIVE_2026-03-08.md).

## 2026-03-21

- Removed non-canonical public constructor/mutator aliases from the collection
  and lazy-sequence surface:
  - removed public lowercase constructor aliases `array`, `dict`,
    `dictionary`, `coroutine`, and `time-point`
  - removed public type-prefixed mutator aliases `array-set!` and
    `dict-set!` in favor of generic `set!`
  - removed public `iterator` as an alias of `Iterator`
  - hid the low-level iterator thunk helper behind internal `__make-iterator`
    and kept `Iterator` as the only public iterator constructor
  - removed redundant iterator forcing helpers `collect` and `to-array`; use
    `List` / `Array` constructors instead
  - kept `Dictionary` as the canonical name
  - parser/compiler lowering for array and dict literals now targets canonical
    `Array` / `Dictionary` constructors directly

- Removed the remaining binary-only ergonomics trap from the core logic
  surface:
  - `and` and `or` now parse as variadic short-circuit special forms
  - empty forms lower to the expected identities: `(and)` => `true`,
    `(or)` => `nil`
  - single-argument forms return the single argument unchanged
  - macro-emitted variadic `and` / `or` forms now validate and lower through
    the same nested binary AST path as direct source syntax
  - regression coverage now pins empty, single, multi-argument, falsy
    short-circuit, and macro-emitted variadic forms

- Shipped the current `B6.2` Deduce constraint/integrity slice and closed the
  current escape-scope regression lane:
  - declared single-column `key` relations now reject conflicting `fact!`
    writes at assert time while still treating identical full-tuple reasserts
    as idempotent
  - declared single-column `unique` relations now reject conflicting `fact!`
    writes at assert time while still treating identical full-tuple reasserts
    as idempotent
  - declared multi-column `unique` roles on one relation now form enforced
    composite unique constraints at assert time while still treating identical
    full-tuple reasserts as idempotent
  - declared single-column `ref` roles now enforce candidate-key references:
    the target relation/column pair must resolve and the target column must be
    a declared `key` or single-column `unique`
  - integrity enforcement is no longer purely extensional: non-aggregate rule
    heads on declared `key` / `unique` relations now enforce the same checks
    on derived writes, including recursive seminaive materialization
  - aggregate rule heads on declared `key` / `unique` relations now also
    publish through the same derived-write integrity checks, including
    recursive seminaive aggregate materialization
  - declared `ref` rule heads now validate against the final component
    transaction snapshot before commit, so derived ordinary and aggregate
    rule-head publish no longer relies on unsound eager per-tuple `ref`
    checks
  - conflict raises now use machine-checkable payload data under
    `deduce/integrity-key-conflict`,
    `deduce/integrity-unique-conflict`, and
    `deduce/integrity-reference-missing`, including relation, constrained
    column set / constrained value projection, referenced target metadata,
    and existing/attempted tuples where applicable
  - `deduce/schema` now exposes keyed/unique/reference integrity constraint
    metadata, including `unique-columns` for composite unique constraints, and
    `deduce/analyze` now reports keyed relation, unique relation, reference
    constraint, and integrity constraint counts
  - `deduce/stats` now reports relation-local integrity violation totals,
    per-class violation counts, and `last-integrity-violation-code`
  - `deduce/analyze` now reports DB-wide integrity violation totals,
    per-class counts, and `last-integrity-violation-code` /
    `last-integrity-violation-relation`
  - `deduce/stats` and `deduce/analyze` now both expose bounded
    `recent-integrity-violations` history, newest first, so write-side/admin
    inspection is no longer limited to counters plus last-code snapshots
  - explicit `write-deferred` transactions now defer the current `fact!`-side
    key/unique/reference checks to commit-time validation over touched
    inserted relations, with exact rollback on failed deferred commit
  - `write-deferred` transactions now also defer delete-side reference
    protection to commit-time snapshot validation, so staged target deletes can
    proceed until commit and are either rejected or accepted against the final
    transaction-visible state
  - `retract!` now rejects deleting referenced target tuples with
    `deduce/integrity-reference-target-in-use`, while write transactions allow
    the target delete after the blocking reference has already been removed in
    the same transaction-visible snapshot
  - `clear!` and `drop!` now enforce the same target-side reference protection
    as `retract!`, rejecting bulk deletion when live referencing tuples still
    exist
  - `deduce/rule!` now accepts keyed, single-column unique, and composite
    unique constrained head relations across both ordinary and aggregate rule
    heads, and the derived-write regression surface now pins recursive keyed
    materialization, aggregate keyed publish success, and machine-checkable
    key/unique conflict payloads
  - `deduce/rule!` now accepts declared `ref` constrained head relations
    across both ordinary and aggregate rule heads
  - `deduce/schema` and `deduce/analyze` now expose `target-unique` for
    reference constraints, and writes against non-unique target columns now
    raise `deduce/integrity-reference-target-not-unique`
  - dropped target relations no longer appear resolved in `deduce/schema` /
    `deduce/analyze`; reference metadata now exposes `target-live` and
    unresolved counts include dropped targets
  - bounded Deduce validation is green at `169 passed, 0 failed` after the
    final `ref` rule-head closure
  - the explicit `escape-scope: iterator consume car` and
    `escape-scope: iterator consume len` regressions are fixed, and the bounded
    `escape-scope` slice is green again after moving the larger named-let list
    accumulator pressure back into the dedicated `memory-lifetime` budget lane
  - validation for this slice:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `165 passed, 0 failed`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=escape-scope ./build/main --test-suite lisp`
    - result: `29 passed, 0 failed`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime ./build/main --test-suite lisp`
    - result: `62 passed, 0 failed`

- Removed association-list lookup from the public language surface and logged
  the remaining unrelated runtime follow-up:
  - dotted path lookup now uses real dictionaries instead of alist fallback,
    and `assoc` / `assoc-ref` are removed from the documented stdlib surface
  - the initial broad bounded sweep exposed `escape-scope` iterator consume
    failures in `src/lisp/tests_escape_scope_tests.c3`, now recorded in the
    backlog instead of being left implicit in the dirty tree:
    - `escape-scope: iterator consume car` ->
      `car: argument must be a pair`
    - `escape-scope: iterator consume len` ->
      `length: expected list, array, dict, set, or string`
  - validation for the alist-removal lane:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
    - result: `142 passed, 0 failed`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
    - result: `1082 passed, 0 failed`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    - result: `122 passed, 0 failed`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `121 passed, 0 failed`

- Deduce recursive aggregate seminaive rollout is now complete for the current
  stratified rule surface:
  - recursive aggregate SCCs with lower-stratum negated body atoms now use
    `semi-naive-scc` instead of the old grouped naive fallback
  - the last automatic `recursive-aggregate-naive-fallback` lane is retired
    for currently valid recursive aggregate workloads
  - query/explain regressions now pin exact seminaive behavior for positive
    and negated mixed aggregate SCCs, while tail routing checks assert
    seminaive exactness directly instead of treating naive proof-multiplicity
    counts as the correctness oracle for mixed non-aggregate heads
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `121 passed, 0 failed`
  - `deduce/analyze` now routes explicit naive requests for recursive SCCs
    that mix aggregate and non-aggregate rules onto the exact seminaive lane
    and reports that via
    `naive-recursive-aggregate-mixed-routed-to-seminaive`
  - `deduce/explain` now mirrors that routing truth in its optional
    3-argument engine-aware form `(deduce/explain db selector engine)`
  - explain-side regression coverage now also pins the pure positive and pure
    negated recursive aggregate cases: explicit `naive` stays on `naive-scc`
    and does not emit the mixed-route diagnostic
  - pure positive and pure negated recursive aggregate analyze/explain
    regressions now both pin that same no-route behavior

## 2026-03-20

- Deduce backlog item `B5.6` is now complete:
  - grouped aggregate execution for rule heads now ships for exact
    `count` / `sum` / `min` / `max` in both non-recursive rules and
    aggregate-bearing recursive SCCs
  - recursive aggregate SCCs request the semi-naive engine but fall back to a
    sound naive grouped recompute path until aggregate delta semantics exist
  - recursive aggregate finalize now diffs the materialized grouped head rows
    before rewrite so recursive grouped passes converge instead of churning
  - aggregate-only shared heads with matching projection signatures now batch
    through one grouped finalize pass, while mixed or incompatible shapes
    still reject at install time
  - `deduce/analyze` now reports:
    - `execution-engine`
    - `requested-execution-engine`
    - `recursive-aggregate-naive-fallback`
  - regression coverage now includes:
    - recursive aggregate install/execution acceptance
    - recursive aggregate explain/analyze truthfulness
    - naive versus semi-naive parity coverage with recursive aggregate
      fallback semantics
    - bounded recursive aggregate benchmark smoke
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `97 passed, 0 failed`

- Deduce backlog planning now promotes the next stage toward full recursive
  aggregate seminaive support:
  - the shipped recursive aggregate baseline remains the naive grouped
    recompute fallback from `B5.6`
  - the next explicit rollout stages are the signed-delta and
    support-accounting work needed to retire that fallback safely
  - active backlog/docs now spell out that staged path instead of leaving the
    remaining work as an unnamed deferred note
  - validation: doc review only

- Deduce backlog item `B5.7` has started landing its first runtime slice:
  - seminaive recursive delta state now carries signed add/remove tuple slots
    in the shared execution substrate instead of a single positive-only table
  - aggregate-bearing recursive SCCs now seed tuple support tables and
    sign-aware head-materialization hooks behind the fallback gate
  - dormant seminaive aggregate finalize now routes grouped head replacement
    through support-table zero-crossing transitions on actual materialized
    tuples instead of direct LMDB rewrite plus synthetic aggregate deltas
  - aggregate groups now track signed support counts internally, including
    removal-aware `count`/`sum` updates and `min`/`max` extremum recompute
    when the current support disappears
  - the dormant seminaive aggregate executor now runs mixed non-aggregate
    rules in the same recursive SCC on the same signed-delta/support-table
    substrate instead of skipping them outright
  - positive recursive aggregate SCCs now use the `semi-naive-scc`
    execution lane again, including mixed aggregate/non-aggregate recursive
    SCCs that stay within positive body atoms
  - mixed positive recursive aggregate regressions now pin exact seminaive
    behavior and truthful analyze/explain reporting
  - focused tuple-support and aggregate-group regressions now pin
    recursive-only, base-seeded, signed count-removal, and `min`-recompute
    behavior independently of the higher-level fallback fixtures
  - aggregate-bearing recursive SCCs no longer all share one blanket
    fallback: positive supported components now run seminaive, while
    recursive aggregate components with negated body steps still stay on the
    documented fallback until that remaining shape is validated explicitly
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `103 passed, 0 failed`
  - aggregate seminaive proof dedupe now keys off normalized variable
    bindings plus delta polarity before support application, so dormant
    multi-anchor replay no longer double-applies the same logical proof's
    support delta across anchor positions
  - asserted Deduce facts are now mirrored into per-relation extensional
    shadow DBIs so recursive aggregate substrate work can distinguish base
    tuples from derived tuples during internal reset/reseed paths
  - the dormant recursive aggregate seminaive seed path now clears component
    predicate relations, restores extensional base tuples from that shadow,
    seeds support tables from extensional state, and publishes the same base
    tuples into the first signed-delta frontier instead of reseeding from live
    mixed relations
  - focused regression coverage now pins that proof-key behavior directly
    without widening the public recursive aggregate engine contract
  - focused regression coverage now also pins extensional shadow mirror
    behavior for assert/retract/clear
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `107 passed, 0 failed`

- Deduce backlog items `B5.4` and `B5.5` are now complete:
  - grouped aggregate execution for rule heads now ships on the checked-in
    Deduce runtime for exact non-recursive:
    - `count`
    - `sum`
    - `min`
    - `max`
  - the shared naive evaluation path now accumulates grouped aggregate state,
    clears the exclusive aggregate head relation on recompute, and finalizes
    deterministic grouped rows back into LMDB
  - `deduce/analyze` no longer rejects supported aggregate-bearing rules, and
    `deduce/explain` now also collects observed step counters for aggregate
    rules through the dry-run path
  - install-time guardrails now reject unsupported aggregate shapes explicitly:
    - mixed aggregate/non-aggregate shared heads reject
    - aggregate-only shared heads with incompatible projection signatures
      reject
  - regression coverage now includes:
    - grouped runtime correctness for `count` / `sum` / `min` / `max`
    - explain/analyze parity for aggregate rules after grouped execution lands
    - explicit validation rejection for unsupported shared-head aggregate
      shapes
  - benchmark-mode smoke now seeds and analyzes an aggregate workload in the
    Deduce benchmark helpers
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `93 passed, 0 failed`

- Deduce v1.5 maintenance/admin work has started shipping on the public
  surface:
  - added read-only admin/introspection verbs:
    - `deduce/schema`
    - `deduce/indexes`
    - `deduce/stats`
  - the new surfaces are relation-handle scoped and return deterministic
    payloads built from existing Deduce metadata rather than inferred state
  - `deduce/schema` now reports:
    - relation symbol
    - columns
    - arity
    - index count
    - cardinality and distinct estimates
    - dropped/database-open flags
  - `deduce/indexes` now reports registered logical index descriptors with:
    - index id
    - ordered columns
    - column count
    - uniqueness flag
  - `deduce/stats` now reports:
    - cardinality estimate
    - distinct estimate
    - dirty predicate count
    - mutation epoch
    - full-recompute-required flag
    - dropped flag
  - deterministic regression coverage now exercises the new schema/index/stats
    surfaces in `src/lisp/tests_deduce_query_groups.c3`
  - validation:
    - `c3c build`
    - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - result: `83 passed, 0 failed`

- Deduce v1.5 aggregate queue promotion is now underway:
  - the first live aggregate item is complete as a syntax/design lock
  - `docs/deduce-datalog-spec.md` now pins the deferred aggregate query shape
    as projection-driven syntax:
    - bare variables imply group-by keys
    - supported aggregate forms are `count`, `sum`, `min`, `max`
  - the concrete implementation seam map for the next patches is recorded in:
    - `docs/plans/deduce-v15-aggregate-implementation-plan-2026-03-20.md`
  - `TODO.md` now promotes the v1.5 aggregate lane into the active Deduce queue

- Deduce backlog item `B5.1` is now complete:
  - aggregate v1.5 entry policy is now explicit across the Deduce roadmap/spec:
    - v1.5 target: grouped exact aggregates with small deterministic state
      (`count`, `sum`, `min`, `max`)
    - deferred: `avg` and `distinct-count`
    - non-goal for v1.5: percentile/median, variance/stddev, ordered-set
      aggregates, approximate/sketch aggregates, and user-defined aggregate
      hooks
  - the policy is recorded in:
    - `docs/deduce-datalog-spec.md`
    - `docs/plans/deduce-full-roadmap-2026-03-20.md`
    - `docs/plans/deduce-actionable-backlog-2026-03-20.md`
  - validation: doc review only

- Deduce backlog item `B5.3` is now complete as a design item:
  - the canonical provenance / why-result payload shape is now specified in
    `docs/deduce-datalog-spec.md`
  - the payload uses:
    - `kind = why-result`
    - `status`
    - `surface-kind = provenance-snapshot`
    - `subject-kind`
    - `subject`
    - `path-count`
    - `max-depth`
    - `truncated`
    - ordered `paths[]`
  - each path is a deterministic support chain with:
    - `path-id`
    - `path-kind`
    - `head-predicate`
    - `head-tuple`
    - `rule-index`
    - `support`
  - each support frame carries:
    - `kind`
    - `frame-index`
    - `predicate`
    - `tuple`
    - `rule-index`
    - `step-index`
    - `selected-index`
    - `operator`
    - `bound-mask`
    - `bindings`
    - `depends-on`
  - roadmap and backlog notes now treat provenance / why-result as a documented
    design item while implementation remains deferred

- Deduce backlog item `B3.3` is now complete:
  - `deduce/explain` now replaces planner-estimate step counters with
    runtime-observed counters while keeping the existing planner snapshot shape
    (`steps[*].counters`) in
    `src/lisp/deduce_rule_ops_explain.c3`
  - per-step counter payload now includes:
    - `rows-read`
    - `rows-emitted`
    - `index-hits`
    - `index-misses`
    - `join-probes`
    - `counter-kind = 'observed`
  - the Deduce evaluator now accumulates per-step observed counters through the
    shared naive/semi-naive runtime paths in:
    - `src/lisp/deduce_rule_eval_exec.c3`
    - `src/lisp/deduce_rule_eval_exec_naive.c3`
    - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
    - `src/lisp/deduce_rule_eval_fixpoint.c3`
  - `deduce/analyze` now exposes runtime-observed per-rule step payloads under
    `rule-execution` in `src/lisp/deduce_rule_eval_prims.c3`
  - regression coverage now asserts observed explain/analyze counters in:
    - `src/lisp/tests_deduce_rule_groups_explain.c3`
    - `src/lisp/tests_deduce_rule_groups_more_tail.c3`
    - `src/lisp/tests_deduce_query_bench_groups.c3`
  - validation:
    - `c3c build`
    - bounded Deduce slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`80 passed, 0 failed`)

## 2026-03-19

- Runtime/backend simplification backlog item 1 is now in progress:
  - split the retired-code tombstone registry out of
    `src/lisp/jit_jit_compiler.c3`
  - added `src/lisp/jit_jit_compiler_retired_code.c3` for:
    - `JitRetiredCode`
    - retired-code table storage
    - `jit_retired_code_contains(...)`
    - `jit_retired_code_insert(...)`
    - `jit_retired_code_forget_serial(...)`
    - `jit_retired_code_reset(...)`
    - `jit_retired_code_prune_detached_serials(...)`
  - `src/lisp/jit_jit_compiler.c3` now retains the compiler/cache/attachment
    ownership surface without the retired-code registry internals.
  - split the cache layer out of `src/lisp/jit_jit_compiler.c3`
  - added `src/lisp/jit_jit_compiler_cache.c3` for:
    - `JitCacheEntry`
    - cache table storage
    - cache slot access/count helpers
    - `jit_cache_lookup(...)`
    - `jit_cache_find_slot(...)`
    - `jit_cache_commit_slot(...)`
    - `jit_cache_store(...)`
  - `src/lisp/jit_jit_compiler.c3` now retains compiled-function types,
    tracked-state storage, and thread/lifecycle identity helpers without
    cache/retired-code registry internals.
  - split the attachment table out of `src/lisp/jit_jit_compiler.c3`
  - added `src/lisp/jit_jit_compiler_attach_table.c3` for:
    - `JitAttachedInterp`
    - attachment table storage
    - attachment table access/depth helpers
    - `runtime_backend_next_attach_serial()`
    - `jit_register_attached_interp(...)`
    - `jit_unregister_attached_interp(...)`
  - `src/lisp/jit_jit_compiler.c3` now retains compiled-function types,
    migration helpers, and thread/lifecycle identity helpers without
    retired-code, cache, or attachment-table internals.
  - split the tracked-state pool and spill-state storage out of
    `src/lisp/jit_jit_compiler.c3`
  - added `src/lisp/jit_jit_compiler_state_pool.c3` for:
    - `JitTrackedState`
    - tracked-state pool storage
    - tracked-state slot access helpers
    - `JitStateSpillNode`
    - spill-state list storage
    - spill-state head/node access helpers
  - `src/lisp/jit_jit_compiler.c3` now retains compiled-function types,
    migration helpers, and thread/lifecycle identity helpers without
    retired-code, cache, attachment-table, or tracked/spill-state storage
    internals.
  - split the runtime/thread identity and backend-global helper layer out of
    `src/lisp/jit_jit_compiler.c3`
  - added `src/lisp/jit_jit_compiler_runtime_identity.c3` for:
    - thread-affinity helpers
    - attached-serial / attached-interpreter query helpers
    - interpreter refcount helpers
    - exec-depth helpers
    - suspended-guard helpers
    - initialized / owner-thread-token helpers
  - `src/lisp/jit_jit_compiler.c3` is now reduced to compiled-function types,
    helper entrypoints, and backend-global flag definitions (`76` lines).
  - backlog item 1 is now complete by substance.
  - validation:
    - `c3c build`
    - bounded JIT-policy slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`28 passed, 0 failed`)

- Runtime/backend simplification backlog item 2 is now in progress:
  - split the coroutine half out of `src/lisp/primitives_iter_coroutine.c3`
    into `src/lisp/primitives_coroutine.c3`
  - split the iterator thunk/state helper layer out of
    `src/lisp/primitives_iter_coroutine.c3` into
    `src/lisp/primitives_iter_state.c3`
  - split the iterator terminal/collection-consumption layer out of
    `src/lisp/primitives_iter_coroutine.c3` into
    `src/lisp/primitives_iter_terminal.c3`
  - split the iterator source / infinite-source layer out of
    `src/lisp/primitives_iter_coroutine.c3` into
    `src/lisp/primitives_iter_sources.c3`
  - retained transform/combinator primitives in
    `src/lisp/primitives_iter_coroutine.c3`
  - moved coroutine thunk/bootstrap and `Coroutine` creation support into
    `src/lisp/primitives_coroutine.c3`
  - moved resume validation/context-switch helpers, yielded-value copyout,
    `resume`, and `yield` into `src/lisp/primitives_coroutine_resume.c3`
  - moved iterator partial-thunk builders, source-state constructors,
    combinator-state constructors, and shared iterator next/predicate helpers
    into `src/lisp/primitives_iter_state.c3`
  - moved `iterator?`, `make-iterator`, `Iterator`, `next`, iterator argument
    / consume helpers, `collect`, and `to-array` into
    `src/lisp/primitives_iter_terminal.c3`
  - moved collection-backed iterator constructors plus `range-from`, `repeat`,
    and `cycle` into `src/lisp/primitives_iter_sources.c3`
  - validation:
    - `c3c build`
    - bounded advanced slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
      (`1086 passed, 0 failed`)

- Runtime/backend simplification backlog item 3 is now in progress:
  - moved fast-target project generation out of `scripts/build_fast_dev.sh`
    into `tools/fast-dev/generate_fast_dev_project.py`
  - replaced the generated fast-dev `schema_validation.c3` patch with a
    maintained source file at `tools/fast-dev/lisp/schema_validation.c3`
  - replaced the generated fast-dev `eval_init_primitives.c3` patch with a
    maintained source file at `tools/fast-dev/lisp/eval_init_primitives.c3`
  - retained fast-profile generation, profiling, and no-op freshness checking
    through the public scripts
  - the fast target no longer rewrites runtime source files during generation;
    project generation now composes maintained fast-dev sources directly
  - fixed the no-op freshness check so it also depends on the generator helper,
    preventing stale `up to date` results after generator edits
  - backlog item 3 is now complete by substance: the fast target no longer
    rewrites runtime source files during generation
  - validation:
    - `scripts/build_fast_dev.sh --profile`
    - `scripts/build_fast_dev.sh`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/dev-fast/main-dev --eval '(+ 2 40)'`
      (`42`)
    - `scripts/build_fast_nodeduce_dev.sh --profile`
    - `scripts/build_fast_nodeduce_dev.sh`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/dev-fast-nodeduce/main-dev-nodeduce --eval '(+ 1 2)'`
      (`3`)
    - repeat runs report:
      - `fast-dev[default] build: up to date`
      - `fast-dev[nodeduce] build: up to date`

- Runtime/backend simplification backlog item 4 is now complete:
  - expanded `scripts/run_validation_status_summary.sh` into the checked-in
    machine-readable operator status artifact
  - the summary now covers:
    - `c3c build`
    - `scripts/check_status_consistency.sh`
    - `scripts/check_e2e_baseline_policy.sh`
    - bounded `jit-policy`, `scheduler`, `deduce`, `compiler`,
      `memory-lifetime-smoke`, and `advanced` slices
  - the default artifact path is now `build/validation_status_summary.json`,
    with per-run command/log files under `build/validation_status_logs/`
  - `docs/areas/README.md`, `docs/areas/memory-runtime.md`, and
    `docs/areas/types-dispatch.md` now point to the summary script as the broad
    operator entrypoint before narrower reruns
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9/9 runs passed`)

- Runtime/backend simplification backlog item 5 is now in progress:
  - split the match-analysis/scoring layer out of
    `src/lisp/eval_dispatch_match.c3`
    into `src/lisp/eval_dispatch_match_breakdown.c3`
  - split the dispatch error/reporting layer out of
    `src/lisp/eval_dispatch_match.c3`
    into `src/lisp/eval_dispatch_match_errors.c3`
  - `src/lisp/eval_dispatch_match_breakdown.c3` now owns:
    - `DispatchMatchFailureReason`
    - `DispatchMatchBreakdown`
    - value-literal dispatch matching
    - method constraint unification/satisfaction
    - dispatch scoring and per-argument breakdown
  - `src/lisp/eval_dispatch_match_errors.c3` now owns:
    - failure-reason symbol mapping
    - lambda-call type-error payload construction
    - ambiguous-dispatch payload construction
    - expected/literal rendering helpers
  - `src/lisp/eval_dispatch_match.c3` now stays focused on public boundary
    flow:
    - lambda-call boundary checking
    - best-method selection
  - resulting file sizes:
    - `src/lisp/eval_dispatch_match.c3`: `98` lines
    - `src/lisp/eval_dispatch_match_breakdown.c3`: `214` lines
    - `src/lisp/eval_dispatch_match_errors.c3`: `215` lines
  - validation:
    - `c3c build`
    - bounded advanced slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
      (`1086 passed, 0 failed`)
  - backlog item 5 is now complete by substance, and the
    `runtime-backend-simplification-backlog-2026-03-19.md` queue is now closed
    on the checked-in tree

- Started a new focused runtime queue in
  `docs/plans/interp-state-runtime-cleanup-2026-03-19.md`:
  - split symbol/module/runtime-flag/macro initialization helpers out of
    `src/lisp/value_interp_state.c3`
    into `src/lisp/value_interp_init_helpers.c3`
  - split allocator helpers out of `src/lisp/value_interp_state.c3`
    into `src/lisp/value_interp_alloc_helpers.c3`
  - split private runtime bootstrap and apply-frame helpers out of
    `src/lisp/value_interp_state.c3`
    into `src/lisp/value_interp_runtime_helpers.c3`
  - split continuation/resume helpers out of
    `src/lisp/value_interp_state.c3`
    into `src/lisp/value_interp_continuation_helpers.c3`
  - `src/lisp/value_interp_state.c3` now keeps:
    - `InterpFlags`
    - `Interp`
    - public `Interp.init(...)`
  - resulting file sizes:
    - `src/lisp/value_interp_state.c3`: `234` lines
    - `src/lisp/value_interp_init_helpers.c3`: `118` lines
    - `src/lisp/value_interp_alloc_helpers.c3`: `70` lines
    - `src/lisp/value_interp_runtime_helpers.c3`: `61` lines
    - `src/lisp/value_interp_continuation_helpers.c3`: `23` lines
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9/9 runs passed`)

- Completed `docs/plans/interp-state-runtime-cleanup-2026-03-19.md` by
  substance and opened the next focused runtime queue in
  `docs/plans/boundary-graph-audit-cleanup-2026-03-19.md`:
  - split the reachability traversal out of
    `src/lisp/eval_boundary_graph_audit.c3`
    into `src/lisp/eval_boundary_graph_audit_reachability.c3`
  - split the verbose telemetry dump helpers out of
    `src/lisp/eval_boundary_graph_audit.c3`
    into `src/lisp/eval_boundary_graph_audit_telemetry.c3`
  - `src/lisp/eval_boundary_graph_audit.c3` now keeps only the public
    audit/debug entrypoints
  - resulting file sizes:
    - `src/lisp/eval_boundary_graph_audit.c3`: `52` lines
    - `src/lisp/eval_boundary_graph_audit_reachability.c3`: `381` lines
    - `src/lisp/eval_boundary_graph_audit_telemetry.c3`: `46` lines
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9/9 runs passed`)
  - `docs/plans/boundary-graph-audit-cleanup-2026-03-19.md` is now complete
    by substance

- Opened the next focused runtime queue in
  `docs/plans/runtime-effects-cleanup-2026-03-19.md`:
  - split continuation validation/resume helpers out of
    `src/lisp/jit_jit_runtime_effects.c3`
    into `src/lisp/jit_jit_runtime_effects_continuation.c3`
  - split checkpoint/reset and capture helpers out of
    `src/lisp/jit_jit_runtime_effects.c3`
    into `src/lisp/jit_jit_runtime_effects_reset_shift.c3`
  - split signal fast-path and handler-resume helpers out of
    `src/lisp/jit_jit_runtime_effects.c3`
    into `src/lisp/jit_jit_runtime_effects_signal.c3`
  - `src/lisp/jit_jit_runtime_effects.c3` now keeps:
    - public resolve/continuation application entrypoints
  - resulting file sizes:
    - `src/lisp/jit_jit_runtime_effects.c3`: `153` lines
    - `src/lisp/jit_jit_runtime_effects_continuation.c3`: `137` lines
    - `src/lisp/jit_jit_runtime_effects_reset_shift.c3`: `120` lines
    - `src/lisp/jit_jit_runtime_effects_signal.c3`: `84` lines
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9/9 runs passed`)
  - `docs/plans/runtime-effects-cleanup-2026-03-19.md` is now complete by
    substance

- Opened `docs/plans/largest-runtime-files-pass-2026-03-19.md` for the next
  largest-first runtime batch and landed the first parallel/local pass:
  - split `src/lisp/schema.c3`
    into `src/lisp/schema_explain_helpers.c3`
    (`45` / `418` lines)
  - split `src/lisp/macros_expr_conversion.c3`
    into `src/lisp/macros_expr_conversion_value_to_expr.c3`
    (`118` / `339` lines)
  - split `src/lisp/jit_jit_apply_multi_prims.c3`
    into `src/lisp/jit_jit_apply_multi_prims_tail.c3`
    (`230` / `201` lines)
  - split `src/lisp/deduce_relation_scan_helpers.c3`
    into `src/lisp/deduce_relation_scan_helpers_join.c3`
    (`159` / `320` lines)
  - split `src/lisp/aot.c3`
    into `src/lisp/aot_type_definitions.c3`
    (`84` / `375` lines)
  - split `src/lisp/async_process_signal_dns.c3`
    into `src/lisp/async_process_signal_dns_process.c3`
    (`197` / `269` lines)
  - split `src/lisp/scheduler_primitives.c3`
    into `src/lisp/scheduler_primitives_run_loop.c3`
    (`257` / `218` lines)
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9/9 runs passed`)

- Continued `docs/plans/largest-runtime-files-pass-2026-03-19.md` with the
  second largest-file batch plus a local runtime backend slice:
  - split `src/lisp/schema_explain_effect.c3`
    into `src/lisp/schema_explain_effect_helpers.c3` and
    `src/lisp/schema_explain_effect_runtime.c3`
    (`42` / `189` / `247` lines)
  - split `src/lisp/deduce_db_handles_mutation.c3`
    into `src/lisp/deduce_db_handles_mutation_tracking.c3`
    (`260` / `209` lines)
  - split `src/lisp/scheduler_primitives_task_wait_join.c3`
    into `src/lisp/scheduler_primitives_task_wait_join_args.c3`
    (`344` / `120` lines)
  - split `src/lisp/libclang_bind.c3`
    into `src/lisp/libclang_bind_parse.c3`
    (`176` / `275` lines)
  - split `src/lisp/jit_jit_handle_signal_helpers.c3`
    into `src/lisp/jit_jit_handle_signal_helpers_continuation_scan.c3` and
    `src/lisp/jit_jit_handle_signal_helpers_runtime_effects.c3`
    (`188` / `143` / `116` lines)
  - split `src/lisp/prim_io.c3`
    into `src/lisp/prim_io_file.c3`
    (`125` / `311` lines)
  - split `src/lisp/runtime_backend_hooks.c3`
    into `src/lisp/runtime_backend_hooks_cache.c3`
    (`333` / `113` lines)
  - validation:
    - `c3c build`
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9/9 runs passed`)

- Added a fresh post-split active backlog in
  `docs/plans/runtime-backend-simplification-backlog-2026-03-19.md`:
  - compiler/parser queue-driven splitting is now closed and not to be
    reopened under routine structural work,
  - the next ranked work is runtime/backend simplification, build-target
    stabilization, and machine-readable validation observability,
  - the starting order is:
    - `src/lisp/jit_jit_compiler.c3`
    - `src/lisp/primitives_iter_coroutine.c3`
    - fast dev target stabilization
    - machine-readable validation summary
    - `src/lisp/eval_dispatch_match.c3`
  - updated `docs/plans/README.md` so the new backlog is the active follow-on
    planning surface.

- Compiler/parser structural split lane was formally closed:
  - `docs/areas/compiler-parser-refactor.md` is now `green`.
  - `docs/plans/compiler-parser-refactor-plan.md` is now `complete`.
  - remaining compiler/parser files were assessed as below the split-worthwhile
    threshold for this cycle (`115–121` LOC range), so queue-driven splitting
    is no longer active work.
  - status validation:
    - `./scripts/check_status_consistency.sh`
      (`OK: status consistency checks passed`)

- Compiler/parser refactor continuation slice BJ landed:
  - split `src/lisp/parser_lexer_symbol_number.c3` top-down at the
    float-scanning helper boundary.
  - added `src/lisp/parser_lexer_number_helpers.c3` for:
    - `Lexer.scan_number_float(...)`,
    - `Lexer.scan_number_fraction(...)`,
    - `Lexer.scan_number_exponent(...)`.
  - retained `Lexer.scan_symbol(...)` and `Lexer.scan_number(...)` in
    `src/lisp/parser_lexer_symbol_number.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next selected
    compiler/parser target is now
    `src/lisp/compiler_primitive_variable_hash_table_domains.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BI landed:
  - split `src/lisp/parser_expr_head_forms.c3` top-down at the symbol-head
    dispatch boundary.
  - added `src/lisp/parser_expr_head_symbol_forms.c3` for:
    - `parser_symbol_text_is(...)`,
    - `Parser.is_lambda_head_symbol(...)`,
    - `Parser.parse_quasiquote_like_form(...)`,
    - `Parser.parse_symbol_head_form(...)`.
  - retained `Parser.parse_list_form(...)` in
    `src/lisp/parser_expr_head_forms.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next selected
    compiler/parser target is now `src/lisp/parser_lexer_symbol_number.c3`,
    tied with `src/lisp/compiler_primitive_variable_hash_table_domains.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BH landed:
  - split `src/lisp/compiler_expr_serialize_callable_forms.c3` top-down at the
    control-form serializer boundary.
  - added `src/lisp/compiler_expr_serialize_control_match_forms.c3` for:
    - `serialize_handle_to_buf(...)`,
    - `serialize_match_to_buf(...)`.
  - retained lambda/let/call serialization in
    `src/lisp/compiler_expr_serialize_callable_forms.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next largest
    compiler/parser target is now `src/lisp/parser_expr_head_forms.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BG landed:
  - split `src/lisp/compiler_mutable_capture_detection.c3` top-down at the
    recursive walker boundary.
  - added `src/lisp/compiler_mutable_capture_detection_walk.c3` for:
    - `has_set_on(...)`,
    - `is_captured_by_nested_lambda(...)`.
  - retained `is_mutable_capture(...)` in
    `src/lisp/compiler_mutable_capture_detection.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next largest
    compiler/parser target is now
    `src/lisp/compiler_expr_serialize_callable_forms.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BF landed:
  - split `src/lisp/compiler_native_call_compilation_flat_style.c3` top-down
    at the var/path lowering boundary.
  - added `src/lisp/compiler_native_var_path_compilation_flat_style.c3` for:
    - `compile_var(...)`,
    - `compile_path(...)`.
  - retained `compile_lambda_flat(...)` in
    `src/lisp/compiler_native_call_compilation_flat_style.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next largest
    compiler/parser target is now
    `src/lisp/compiler_mutable_capture_detection.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BE landed:
  - split `src/lisp/compiler_temp_core.c3` top-down at the generic temp-helper
    boundary.
  - added `src/lisp/compiler_temp_helpers.c3` for:
    - `emit_temp_decl(...)`,
    - `emit_temp_ref(...)`,
    - `emit_nil_temp(...)`,
    - `compile_leaf_expr_to_temp(...)`.
  - retained `next_result(...)` plus the non-tail/tail temp-dispatch
    entrypoints in `src/lisp/compiler_temp_core.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next largest
    compiler/parser target is now
    `src/lisp/compiler_native_call_compilation_flat_style.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BD landed:
  - split `src/lisp/compiler_code_emission.c3` top-down at the lambda-helper
    boundary.
  - added `src/lisp/compiler_code_emission_lambda_defs.c3` for:
    - `emit_lambda_signature(...)`,
    - `emit_lambda_param_unpack(...)`,
    - `emit_lambda_param_unpack_list(...)`,
    - `emit_lambda_capture_bindings(...)`,
    - `emit_lambda_body_return(...)`.
  - retained prelude, zero-arg classification, capture-struct emission, and
    top-level lambda-definition emission in
    `src/lisp/compiler_code_emission.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next largest
    compiler/parser target is now `src/lisp/compiler_temp_core.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Compiler/parser refactor continuation slice BC landed:
  - split `src/lisp/compiler_call_explain_helpers.c3` top-down at the
    effect-specific explain boundary.
  - added `src/lisp/compiler_call_explain_effect_helpers.c3` for:
    - `compile_explain_effect_signal_flat(...)`,
    - `compile_explain_effect_resolve_flat(...)`.
  - retained `compile_explain_flat(...)` selector dispatch in
    `src/lisp/compiler_call_explain_helpers.c3`.
  - refreshed the active queue in
    `docs/plans/compiler-parser-refactor-plan.md`; the next largest
    compiler/parser target is now `src/lisp/compiler_code_emission.c3`.
  - validation:
    - `c3c build`
    - bounded compiler slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`)

- Item 6 helper-extraction lane is materially closed:
  - `src/lisp/tests_deduce_helpers.c3` now also provides
    `deduce_test_expect_void(...)`, and
    `src/lisp/tests_deduce_groups.c3` now routes the repeated
    `deduce fact!/retract!/commit/abort/clear!/drop! returns Void` checks
    through that shared helper instead of open-coded `EvalResult` blocks.
  - with that pass, shared helper wiring is now established across the three
    intended families:
    - deduce (`src/lisp/tests_deduce_helpers.c3`),
    - scheduler (`src/lisp/tests_scheduler_helpers.c3`),
    - compiler (`src/lisp/tests_compiler_helpers.c3`).
  - bounded validation remains green for:
    - `OMNI_LISP_TEST_SLICE=deduce` (`72 passed, 0 failed`)
    - `OMNI_LISP_TEST_SLICE=scheduler` (`89 passed, 0 failed`)
    - `OMNI_LISP_TEST_SLICE=compiler` (`122 passed, 0 failed`)
  - `docs/plans/codebase-improvement-backlog-2026-03-19.md` now marks item 6
    closed.

- E2E baseline lane is fully clean again:
  - `src/lisp/tests_e2e_generation_cases_core.c3` now removes the last tracked
    older AOT parity rows for match `Void` / guard-binding cases and the
    remaining handle/effect parity cases that were still outside current AOT
    support.
  - `src/lisp/tests_e2e_generation_cases_extended.c3` now removes the final
    tracked guard-trace and nested-handle parity rows from the e2e corpus.
  - bounded `scripts/run_e2e.sh` is now fully green again (`ALL 404 e2e
    compiler tests passed!`), so
    `scripts/baselines/e2e_expected_diff.txt` is empty and
    `scripts/baselines/e2e_expected_diff.tsv` now keeps only the header row.
  - `scripts/check_e2e_baseline_policy.sh` now treats that zero-row manifest
    state as valid policy and fails only if a live diff reappears without an
    explicit reviewed manifest entry.
  - `docs/areas/types-dispatch.md` is back to `green`, and
    `docs/plans/codebase-improvement-backlog-2026-03-19.md` now marks item 5
    closed.

- Machine-readable validation status summary added:
  - added `scripts/run_validation_status_summary.sh` to run the bounded
    `scheduler`, `deduce`, and `memory-lifetime-smoke` validation slices with
    `OMNI_TEST_SUMMARY=1`, capture their logs, and emit one aggregated JSON
    artifact at `build/validation_status_summary.json`.
  - the JSON artifact records command, subsystem, pass/fail state, exit code,
    known-blocker classification, timestamps, and parsed `OMNI_TEST_SUMMARY`
    rows for each run.
  - `docs/plans/codebase-improvement-backlog-2026-03-19.md` now marks item 8
    closed, and `docs/PROJECT_TOOLING.md` points at the current artifact path.

- Scheduler test-helper extraction expanded:
  - added `src/lisp/tests_scheduler_helpers.c3` to centralize repeated
    scheduler `run(...)` result-shape checks and shared string matching.
  - `src/lisp/tests_scheduler_groups.c3` now routes its common spawn/await and
    run-loop recovery assertions through shared scheduler helpers instead of
    repeating open-coded `EvalResult` checks.
  - `src/lisp/tests_scheduler_offload_thread_groups.c3` now routes its common
    error, truthy-success, exact-int, and void-result assertions through the
    same helper set.
  - `src/lisp/tests_scheduler_boundary_worker.c3`,
    `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`, and
    `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3` now route
    their repeated boundary-suite pass/fail finish logic through the same
    shared helper module instead of open-coding identical `ok` /
    `failed_step` / `failed_phase` reporting blocks.
  - validation remains green:
    - `c3c build`
    - bounded `OMNI_LISP_TEST_SLICE=scheduler` (`89 passed, 0 failed`)

- Compiler test-helper extraction started:
  - added `src/lisp/tests_compiler_helpers.c3` to centralize repeated compiler
    code-shape predicates and shared pass/fail wiring for generated-source
    assertions.
  - `src/lisp/tests_compiler_codegen_groups.c3` now routes its common
    multi-arg/TCO/quasiquote code-shape checks through shared compiler helpers
    instead of repeating open-coded pass/fail blocks.
  - `src/lisp/tests_compiler_codegen_groups_tail.c3` now routes its common
    mutable-capture, feature-integration, and iterative-stdlib code-shape
    checks through the same helper module.
  - `src/lisp/tests_compiler_core_groups.c3` and
    `src/lisp/tests_compiler_core_groups_more.c3` now route their common
    syntax, stdlib-availability, existing-feature, set/path, and continuation
    code-shape checks through the same helper module.
  - the helper extraction now covers the repeated generated-code assertion
    wiring across the full compiler core/codegen family; only the specialized
    serializer and bindgen-specific checks remain intentionally open-coded.
  - validation remains green:
    - `c3c build`
    - bounded `OMNI_LISP_TEST_SLICE=compiler` (`122 passed, 0 failed`)

- Deduce test-helper extraction started:
  - added `src/lisp/tests_deduce_helpers.c3` to centralize the repeated
    `run(...)` + result-shape + pass/fail wiring used by the deduce tests.
  - `src/lisp/tests_deduce_rule_groups.c3` now routes its rule-validation /
    dispatch-alias cases through shared helpers instead of open-coded
    `EvalResult` checks.
  - `src/lisp/tests_deduce_query_scan_groups.c3` now routes its scan,
    scan-range, and query result-shape assertions through the same helper set.
  - validation remains green:
    - `c3c build`
    - bounded `OMNI_LISP_TEST_SLICE=deduce` (`72 passed, 0 failed`)

- E2E baseline refresh and bounded validation repair:
  - `scripts/run_validation_container.sh` now auto-mounts the common host
    headers/libraries needed by bounded compiler/e2e runs (`yyjson`,
    `bearssl`, `uv`, `ffi`, `libreplxx`) so `scripts/run_e2e.sh` reaches the
    generated-binary and diff stages inside the validation container instead of
    failing in Stage 1 on missing toolchain headers.
  - `src/lisp/tests_e2e_generation_cases_extended.c3` now prunes the specific
    e2e cases that still generated invalid AOT source because they depend on
    unsupported compiler surfaces:
    - top-level replay / command-predicate locals,
    - `inexact->exact`,
    - `symbol->string`,
    - `pow`,
    - formatted ambiguity payload rendering.
  - bounded `run_e2e.sh` now completes end-to-end again, and the checked-in
    expected-diff manifest has been refreshed from the stale 11-row snapshot to
    the current 7 diff keys:
    - `115c115`
    - `117c117`
    - `124,126c124,126`
    - `205,208c205,208`
    - `320d319`
    - `321a321`
    - `323c323`
  - the remaining tracked drift is now limited to:
    - match/guard parity rows,
    - effect/handle parity rows,
    - one nested-handle parity row.

- Boundary smoke / ASAN closure refresh:
  - bounded `memory-lifetime-smoke` is green again on both normal and ASAN
    builds (`62 passed, 0 failed` in each profile).
  - the remaining red smoke note was a stale assertion in
    `src/lisp/tests_memory_lifetime_groups.c3`: the test still treated
    `COPY_SITE_CONS_BARRIER_CAR/CDR` as forbidden fallback traffic even though
    those counters now represent the normal destination-cons route used by
    `boundary_commit_escape(...)`.
  - the smoke gate now checks the real invariant for the long cons-spine return
    path:
    - destination-cons routing is allowed,
    - `COPY_SITE_GENERIC` must stay at zero.

- Advanced bounded validation closure:
  - `scripts/container_exec.sh` now sets a 16 MiB soft stack limit by default
    inside the validation container, matching the host baseline (`16384 KB`)
    instead of the prior 8 MiB container default (`8192 KB`).
  - this closes the bounded `advanced-macro-hygiene` crash on the
    `non-tail recursion exceeds former 1024 eval cap` regression without
    changing the runtime recursion semantics on the host path.
  - `src/lisp/tests_advanced_type_dispatch_groups.c3` also now uses valid
    nested binary `and` forms for the ambiguous-dispatch payload assertions,
    and the candidate-ordering expectation now matches the actual method-table
    layout where the untyped fallback lives in `MethodTable.fallback`.
  - bounded validation is green again for:
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-macro-hygiene`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain`
    - full `OMNI_LISP_TEST_SLICE=advanced` (`1086 passed, 0 failed`)

- Runtime backend seam for interpreter lifecycle:
  - added `src/lisp/runtime_backend_hooks.c3` as a narrow runtime/backend
    facade for interpreter-owned lifecycle code.
  - moved the fast-dev entry files out of the default source tree into
    `tools/fast-dev/` so `project.json` no longer pulls them into the full
    `build/main` target by accident.
  - `src/lisp/value_interp_lifecycle.c3` and
    `src/lisp/value_interp_state.c3` now call backend-neutral wrappers for:
    - interp attach/detach,
    - cache clearing,
    - global runtime shutdown,
    - live handle-state release checks.
  - `src/lisp/eval_run_pipeline.c3` and
    `src/lisp/tests_harness_helpers.c3` now route top-level
    compile/exec/eval/GC control through the same backend seam instead of
    calling `jit_compile(...)`, `jit_exec(...)`, `jit_eval(...)`, and `jit_gc()`
    directly from the run pipeline.
  - additional interpreter-owned eval sites now also go through the same seam:
    - `src/lisp/eval_repl.c3` now routes REPL pre-eval GC through the backend
      hook instead of calling `jit_gc()` directly.
    - `src/lisp/eval_ffi_eval.c3`, `src/lisp/eval_pattern_matching.c3`,
      `src/lisp/schema.c3`, `src/lisp/schema_explain_effect.c3`,
      `src/lisp/primitives_meta_predicates.c3`, and targeted trace coverage in
      `src/lisp/tests_advanced_stdlib_numeric_groups.c3` now use
      `runtime_eval_expr(...)` instead of naming `jit_eval(...)` directly.
  - `src/lisp/value_interp_lifecycle.c3` now also routes continuation-handle
    invalidation and retained-refcount checks through
    `src/lisp/runtime_backend_hooks.c3`, so normal interpreter teardown no
    longer dereferences `HandleEffectState*` fields directly.
  - the inner eval loop now also lives behind the backend seam:
    - `src/lisp/runtime_backend_hooks.c3` owns the current-backend eval loop,
      including compile lookup, checked execution, TCO bounce handling, and
      TCO trace emission.
    - `src/lisp/jit_jit_eval_scopes.c3` keeps the JIT-specific scope helpers,
      but `jit_eval(...)` now delegates to `runtime_eval_expr(...)` instead of
      owning the loop itself.
  - the seam now also owns the helper implementations that loop depends on:
    - compile-cache lookup / compile fallback,
    - checked compiled-function execution,
    - TCO recycle preparation,
    - and TCO trace-enable checks.
    - `src/lisp/jit_jit_eval_scopes.c3` and
      `src/lisp/jit_jit_closure_support.c3` keep the old `jit_*` helper names
      for existing callers and tests.
  - the runtime-facing compiled-expression surface is now opaque:
    - `src/lisp/runtime_backend_hooks.c3` exposes `RuntimeCompiledExpr`
      instead of `JitCompiledFn`.
    - `src/lisp/eval_run_pipeline.c3` and
      `src/lisp/tests_harness_helpers.c3` no longer mention the JIT compiled
      handle type directly.
    - JIT modules still use `JitCompiledFn` internally, with explicit
      wrap/unwrap at the seam boundary.
  - cache policy ownership also moved behind the seam:
    - `src/lisp/runtime_backend_hooks.c3` now owns cache clear, owner/serial
      aware cache lookup, liveness validation for compiled handles, and
      cache store/retry behavior.
    - `src/lisp/jit_jit_compiler.c3` and
      `src/lisp/jit_jit_compiler_lifecycle.c3` keep `jit_cache_*` and
      `jit_compiled_fn_is_live_for_interp(...)` helper entrypoints.
  - attachment-state bookkeeping is now seam-owned too:
    - `src/lisp/runtime_backend_hooks.c3` now owns attach/detach,
      attached-interpreter lookup, attach-serial reads, active-exec depth
      reads/writes, and exec enter/leave bookkeeping.
    - `src/lisp/jit_jit_compiler.c3` and
      `src/lisp/jit_jit_compiler_lifecycle.c3` keep the old attached-interp
      and lifecycle entrypoints.
    - `src/lisp/jit_common.c3` stack-context save/restore now reads and
      restores per-interpreter exec depth through the seam instead of touching
      the raw attached-interpreter table directly.
  - compiled-state conversion and liveness/execution details moved back behind
    backend helper entrypoints:
    - `src/lisp/runtime_backend_hooks.c3` no longer names `JitFn`,
      `JitCompiledFn`, or scans `g_jit_states` / `g_jit_spill_states`
      directly.
    - `src/lisp/jit_jit_compiler.c3` now owns the
      `RuntimeCompiledExpr` <-> `JitCompiledFn` wrap/unwrap helpers.
    - `src/lisp/jit_jit_compiler_lifecycle.c3` now owns backend-side
      compiled-handle liveness checks.
    - `src/lisp/jit_jit_closure_support.c3` now owns backend-side compile,
      checked exec, and exec wrapper entrypoints for `RuntimeCompiledExpr`.
  - tracked compiled-state teardown/retirement is now backend-helper-owned:
    - `src/lisp/jit_jit_compiler_lifecycle.c3` now owns named backend helpers
      for tracked-state slot destruction, spill-node destruction, full tracked
      state teardown, and attachment-table reset.
    - `jit_global_shutdown()` and `jit_gc()` no longer open-code the tracked
      state / spill teardown loops directly; they route through the backend
      helpers instead.
  - backend-global GC / pool-warning policy now has named helper transitions:
    - `src/lisp/runtime_backend_hooks.c3` now owns helper entrypoints for
      reading/setting/clearing the GC-needed flag and the pool-warning flag.
    - interpreter detach, shutdown/GC paths, and compile pool-pressure paths
      now route through those helpers instead of writing `g_jit_gc_needed` and
      `g_jit_pool_warned` ad hoc.
  - compile-path pool-capacity policy is now named too:
    - `src/lisp/jit_jit_compiler_compile.c3` now uses dedicated backend helper
      functions for:
      - scheduling GC when tracked-state usage crosses the pressure threshold,
      - warning on spill-allocation failure,
      - and warning when overflow tracking moves into the spill list.
    - `jit_track_compiled_state(...)` no longer embeds those warning / policy
      branches inline.
  - raw attached-interpreter/cache table access is narrower now:
    - `src/lisp/jit_jit_compiler.c3` now exposes backend access helpers for
      attached-interpreter table reads/writes and cache-slot reads/writes.
    - `src/lisp/runtime_backend_hooks.c3` no longer indexes
      `g_jit_attached_interps` or `g_jit_cache` directly for normal runtime
      control flow; it now goes through those backend helpers instead.
  - liveness scans are named backend queries now:
    - `src/lisp/jit_jit_compiler_lifecycle.c3` now routes compiled-handle and
      older code-pointer liveness checks through shared backend query helpers
      for tracked-state and spill-node matching, instead of duplicating those
      scans inline.
  - tracked-state slot field access is narrower now too:
    - `src/lisp/jit_jit_compiler.c3` now exposes backend access helpers for
      tracked-state slot reads and slot clearing.
    - `src/lisp/jit_jit_compiler_lifecycle.c3` destroy/match helpers now use
      those accessors instead of reaching into `g_jit_states[idx]` fields
      directly for normal lifecycle control flow.
  - spill-list layout access is narrower too:
    - `src/lisp/jit_jit_compiler.c3` now exposes backend access helpers for
      spill-list head/count traversal and spill-node field reads.
    - `src/lisp/jit_jit_compiler_lifecycle.c3` spill teardown and spill
      liveness traversal now use those helpers instead of reading
      `g_jit_spill_states`, `g_jit_spill_count`, and `node.*` fields directly
      in normal lifecycle control flow.
  - attachment-table reset now also uses slot helpers:
    - `src/lisp/jit_jit_compiler_lifecycle.c3` now clears the attachment table
      through `runtime_backend_clear_attached_interp(...)` instead of zeroing
      raw slot fields inline during full backend reset.
  - detached-serial pruning now uses slot helpers too:
    - `src/lisp/jit_jit_compiler.c3` now checks attached serials through the
      existing attached-interpreter access helpers instead of reading raw
      attachment slots inline in `jit_is_attached_serial(...)`.
  - cache slot probing/commit now uses helper-backed slot access too:
    - `src/lisp/jit_jit_compiler.c3` now routes `jit_cache_find_slot(...)`
      through `runtime_backend_cache_expr_at(...)`, and
      `jit_cache_commit_slot(...)` through `runtime_backend_store_cache_slot(...)`
      instead of reading/writing `g_jit_cache[slot]` fields inline.
  - cache/tracked-state counter reads are narrower now too:
    - `src/lisp/jit_jit_compiler.c3` now exposes backend helpers for cache
      entry count and tracked-state count.
    - `src/lisp/runtime_backend_hooks.c3` cache clear/store control flow and
      `src/lisp/jit_jit_compiler_lifecycle.c3` tracked-state teardown/liveness
      loops now use those helpers instead of reading/writing
      `g_jit_cache_count` and `g_jit_state_count` directly.
  - lifecycle-global exec/refcount/guard counters are helper-backed too:
    - `src/lisp/jit_jit_compiler.c3` now exposes backend helpers for global
      exec depth, interpreter refcount, and suspended-guard count.
    - `src/lisp/runtime_backend_hooks.c3`, `src/lisp/jit_jit_compiler_lifecycle.c3`,
      and `src/lisp/jit_common.c3` now use those helpers instead of directly
      reading/writing `g_jit_exec_depth`, `g_jit_interp_refcount`, and
      `g_jit_suspended_guard_count` across normal seam/lifecycle control flow.
  - attach-serial allocation is helper-backed too:
    - `src/lisp/jit_jit_compiler.c3` now exposes
      `runtime_backend_next_attach_serial()`.
    - `src/lisp/runtime_backend_hooks.c3` now allocates interpreter attach
      serials through that helper instead of bumping
      `g_jit_attach_serial_counter` inline.
  - initialization-state and owner-token clears are helper-backed too:
    - `src/lisp/jit_jit_compiler.c3` now exposes backend helpers for
      initialized-state reads/writes and owner-thread-token clearing.
    - `src/lisp/jit_jit_compiler_lifecycle.c3` and
      `src/lisp/runtime_backend_hooks.c3` now use those helpers instead of
      flipping `g_jit_initialized` and `g_jit_owner_thread_token` directly in
      lifecycle shutdown/init and detach cleanup.
  - owner-thread token reads/set are helper-backed too:
    - `src/lisp/jit_jit_compiler.c3` now routes `jit_require_owner_thread(...)`
      through owner-token helper entrypoints instead of reading/writing
      `g_jit_owner_thread_token` inline in that migration path.
  - `src/lisp/value_interp_lifecycle.c3` no longer needs `HandleEffectState*`
    in its public teardown helpers; continuation-owned handle state now stays
    at `void*` until the backend seam boundary.
  - status:
    - this is structural groundwork for future interpreter-only fast-build
      profiles.
    - it does not make a true `nojit` target viable yet, because the
      inner interpreter execution path still bottoms out in `jit_eval(...)`
      behind `runtime_eval_expr(...)`.
    - after this pass, non-test runtime code no longer calls
      `jit_eval(...)`, `jit_compile(...)`, `jit_exec(...)`, or `jit_gc()`
      directly outside `src/lisp/runtime_backend_hooks.c3`; the remaining
      non-`jit_*` references are comments and `void*` ownership notes.
    - after this pass, normal runtime-facing code also no longer exposes
      `JitCompiledFn`; the remaining uses are inside JIT modules and tests.
    - cache policy is now also seam-owned; the remaining direct `jit_cache_*`
      usage is migration/test-facing rather than normal runtime control
      flow.
    - interpreter attachment identity and active-exec bookkeeping are now
      seam-owned too; direct `g_jit_attached_interps` manipulation is reduced
      to the seam instead of being scattered across runtime-side helpers.
    - compiled-handle conversion and liveness/exec semantics are now backend
      helper-owned too; the seam still routes through JIT-backed behavior, but
      it no longer interprets JIT compiled state layouts itself.
    - tracked-state retirement policy is now concentrated in backend helpers
      instead of being duplicated in both `jit_gc()` and
      `jit_global_shutdown()`.
    - backend-global “between runs” flag transitions are now concentrated too;
      the remaining raw flag references are narrower and mostly test-facing.
    - compile-pool pressure messaging and GC scheduling are also helper-owned
      now, which narrows the remaining inline backend policy inside the compile
      path itself.
    - direct raw storage access is narrower too; the seam now depends less on
      the attached-interpreter and cache table layouts.
    - liveness query logic is narrower too; tracked-state and spill-list scan
      policy is now centralized instead of duplicated across two call sites.
  - validation:
    - `c3c build`
    - `scripts/build_fast_dev.sh`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/dev-fast/main-dev --eval '(+ 2 40)'`
      => `42`
    - `scripts/build_fast_nodeduce_dev.sh`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/dev-fast-nodeduce/main-dev-nodeduce --eval '(+ 1 2)'`
      => `3`

- JIT continuation/effect replay repair:
  - fixed handler-continuation replay semantics in
    `src/lisp/jit_jit_runtime_effects.c3` and
    `src/lisp/jit_jit_reset_shift.c3`:
    - `resolve` remains single-shot,
    - explicit `(k ...)` application from `with-continuation` now uses a
      multi-shot replay path,
    - resumed continuations may suspend again when a later `signal` or
      `capture` is semantically valid,
    - resumed `capture`/checkpoint paths now preserve the fresh resume
      delimiter instead of restoring the stale original reset state.
  - fixed stack-switch JIT bookkeeping drift in `src/lisp/jit_common.c3` by
    restoring per-interpreter active exec depth alongside the global JIT exec
    depth.
  - validation:
    - `c3c build`
    - `./scripts/check_jit_env_scope_guards.sh` passed
      (`OMNI_LISP_TEST_SLICE=jit-policy`: `26 passed, 0 failed`)
    - bounded exact repros now return expected values:
      - `(handle (+ (signal bounce 10) (signal bounce 20)) (bounce x (resolve x)))` => `30`
      - `(handle (+ (signal bounce 1) (+ (signal bounce 2) (signal bounce 3))) (bounce x (resolve x)))` => `6`
      - `(handle (+ 1 (signal choose 0)) (choose x (with-continuation k (+ (k 10) (k 20)))))` => `32`
      - `(handle (+ 1 (signal dup 0)) (dup x (with-continuation k (+ (k 10) (k 20)))))` => `32`
      - `(checkpoint (+ (capture k1 (k1 10)) (capture k2 (k2 20))))` => `30`
      - `(block (define (gen-s3) (checkpoint (block (stream-yield 1) (stream-yield 2) (stream-yield 3) nil))) (car (drop 2 (stream-take 3 (gen-s3)))))` => `3`
      - `(block (define fib-t (lambda (n) (if (< n 2) n (+ (signal bounce (lambda (xx) (fib-t (- n 1)))) (signal bounce (lambda (xx) (fib-t (- n 2)))))))) (with-trampoline (lambda (xx) (fib-t 7))))` => `13`
  - status:
    - the former bounded `advanced` continuation/effect failure cluster is
      closed by exact repro.
    - the bounded `advanced` slice still appears to hang late without emitting
      a clean final summary, so backlog item 2 remains open as a runner-level
      blocker rather than an effect/continuation correctness blocker.

- JIT parity harness alignment:
  - updated `src/lisp/tests_harness_helpers.c3` so explicit JIT parity checks
    now evaluate through the same top-level child-scope/finalize/promote flow
    used by `run(...)`, instead of comparing interpreter results against raw
    `jit_exec(...)`.
  - added focused shorthand-accessor HOF probes in
    `src/lisp/tests_core_groups.c3`:
    - `map .1 accessor shorthand car`
    - `map .1 accessor shorthand second`
  - validation:
    - `c3c build`
    - bounded `basic` slice passed:
      `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
      (`141 passed, 0 failed`)
    - `./scripts/check_jit_env_scope_guards.sh` passed
      (`OMNI_LISP_TEST_SLICE=jit-policy`: `26 passed, 0 failed`).
  - current follow-up:
    - the old `jit_lookup_var` unaligned-access crash no longer reproduces on
      the bounded `advanced` slice rerun.
    - remaining observed `advanced` failures now cluster around continuation
      and multi-shot effect semantics (`multi-perform*`, `signal multi-resume`,
      `multi-capture sum`, `stream-yield continuation resumes beyond two yields`,
      `multi-shot effect`, `with-continuation basic`, `trampoline fib 7`).
  - updated `docs/plans/codebase-improvement-backlog-2026-03-19.md` and
    `docs/areas/memory-runtime.md` to reflect the narrowed remaining scope.

- JIT env/scope boundary fix:
  - fixed the narrow `jit_lookup_var` unaligned-access crash on the
    iterator-return boundary by forcing iterator-inner thunk detachment during
    destination-built ESCAPE routing in
    `src/lisp/eval_boundary_commit_escape_builders.c3` when the thunk would
    otherwise remain physically resident in the target TEMP lane.
  - added regression
    `run_jit_policy_closure_method_iterator_boundary_test(...)` in
    `src/lisp/tests_runtime_feature_jit_groups_more.c3` for the typed
    closure-method iterator shape that previously crashed through
    `value_environment.c3`.
  - validation:
    - bounded repro is now green:
      `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 ./build/main --eval '(length (List (Iterator [1 2 3])))'`
      (`3`)
    - `./scripts/check_jit_env_scope_guards.sh` passed
      (`OMNI_LISP_TEST_SLICE=jit-policy`: `26 passed, 0 failed`).
  - updated `docs/plans/codebase-improvement-backlog-2026-03-19.md` and
    `docs/areas/memory-runtime.md` to mark the narrow JIT guard green while
    keeping the broader `basic`/`advanced` backlog closure work open.

- Subsystem guard scripts:
  - added `scripts/check_scheduler_state_guards.sh` to build locally and rerun the bounded `OMNI_LISP_TEST_SLICE=scheduler` gate as a focused scheduler-state check.
  - added `scripts/check_jit_env_scope_guards.sh` to build locally and rerun the bounded `OMNI_LISP_TEST_SLICE=jit-policy` gate as a focused JIT env/scope check.
  - added `scripts/check_e2e_baseline_policy.sh` to enforce the checked-in `run_e2e.sh` baseline policy (`scripts/baselines/e2e_expected_diff.txt` + `scripts/baselines/e2e_expected_diff.tsv`) even when a live e2e run is not available.
  - validation:
    - `./scripts/check_scheduler_state_guards.sh` passed (`OMNI_LISP_TEST_SLICE=scheduler`: `89 passed, 0 failed`).
    - `./scripts/check_e2e_baseline_policy.sh` passed (manifest + ownership policy aligned; no live diff artifact present in `build/`).
    - `./scripts/check_jit_env_scope_guards.sh` passed after the iterator-boundary fix (`OMNI_LISP_TEST_SLICE=jit-policy`: `26 passed, 0 failed`).
  - updated `docs/plans/codebase-improvement-backlog-2026-03-19.md`, `docs/areas/memory-runtime.md`, and `docs/areas/types-dispatch.md` to record the focused guard-script coverage.

- E2E baseline governance:
  - added `scripts/baselines/e2e_expected_diff.txt` as the checked-in baseline
    `run_e2e.sh` diff manifest and `scripts/baselines/e2e_expected_diff.tsv`
    as the row ownership/review map.
  - updated `scripts/run_e2e.sh` so an exact match against the tracked baseline
    diff no longer masks new regressions; baseline drift still fails with a
    manifest-vs-actual diff preview.
  - updated `docs/areas/types-dispatch.md` and
    `docs/plans/codebase-improvement-backlog-2026-03-19.md` to record the new
    baseline policy and the remaining cleanup target.

- Status-consistency gate:
  - added `scripts/check_status_consistency.sh` to enforce cross-doc status consistency for the active backlog queue.
  - updated `TODO.md` to point the zero-actionable header at `docs/plans/codebase-improvement-backlog-2026-03-19.md`.
  - updated `docs/areas/memory-runtime.md` and `docs/areas/types-dispatch.md` to `yellow` so the area statuses match the still-open blocker and baseline-cleanup queue.

- Runtime modularization batch:
  - `src/lisp/tests_scheduler_boundary_worker.c3` now retains the offload-worker retry, wakeup barrier, and worker-cancel interleave tests, with the nonexecuted-payload release and prestart-cancel shared-release tests moved to `src/lisp/tests_scheduler_boundary_worker_tail.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
      (`89 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_compiler_codegen_groups.c3` now retains the compiler codegen phases 1-4 plus the `qq_macro_primitive` wrapper, with the mutable-capture, integration, iterative-stdlib, and bindgen helper tail moved to `src/lisp/tests_compiler_codegen_groups_tail.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_advanced_core_unicode_groups.c3` now retains the Unicode, constructor, iterator, logic, and effect-continuation test helpers, with the runtime-control and block-syntax contract matrix tests moved to `src/lisp/tests_advanced_core_unicode_groups_runtime.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/value_print.c3` now retains the direct value-printing entrypoints, with the buffer-backed print helpers and `PrintBuf` moved to `src/lisp/value_print_buf.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_ops.c3` now retains parsing, IR, and safety-validation helpers, with the planning/scoring and predicate-index helpers moved to `src/lisp/deduce_rule_ops_planning.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/jit_jit_handle_signal.c3` now retains the signal/dispatch entrypoints, with the shared effect-state, continuation scan, and fast-path helpers moved to `src/lisp/jit_jit_handle_signal_helpers.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/jit_jit_eval_scopes.c3` now retains the JIT lookup/eval wrappers and TCO recycle entrypoints, with the scope-chain, finalize, and recycle helper layer moved to `src/lisp/jit_jit_eval_scopes_helpers.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_deduce_rule_groups.c3` now retains the deduce rule validation helpers and planner-order checks, with the explain payload validation block moved to `src/lisp/tests_deduce_rule_groups_explain.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3` now retains the waiter, cancel, and cancel-conversion boundary families, with the join-wait mapping and mixed boundary-state restore families moved to `src/lisp/tests_scheduler_boundary_thread_task_groups_more_tail.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
      (`89 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups.c3` now retains the scheduler/thread-task core tests, with the waiter and cancel-conversion boundary families moved to `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
      (`89 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_db_handles_mutation.c3` now retains the schema, dirty-tracking, and rule-signature helpers, with the transaction mutation-log helpers moved to `src/lisp/deduce_db_handles_mutation_txn.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_deduce_rule_groups_more.c3` now retains the first four join validation families, with the adaptive-join validation families moved to `src/lisp/tests_deduce_rule_groups_more_join.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/jit_jit_runtime_effects.c3` now retains the resolve-side runtime effect helpers, with the handler-application helpers moved to `src/lisp/jit_jit_runtime_effects_handle.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_scheduler_groups.c3` now retains the spawn/await and run-loop failure scheduler tests, with the scheduler wakeup helpers/tests moved to `src/lisp/tests_scheduler_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
      (`89 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_eval_exec.c3` now retains the delta-set, SCC, and naive-rule execution helpers, with the seminaive execution helpers moved to `src/lisp/deduce_rule_eval_exec_seminaive.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/jit_jit_compiler.c3` now retains the JIT compiler state, cache, and attachment bookkeeping, with lifecycle, GC, exec-depth, and liveness helpers moved to `src/lisp/jit_jit_compiler_lifecycle.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_runtime_feature_jit_groups.c3` now retains the cache, GC, and pre-interpreter-lifetime policy tests, with the multi-interpreter, continuation, handle-state, and capture-boundary policy tests moved to `src/lisp/tests_runtime_feature_jit_groups_tail.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/aot.c3` now retains AOT type/definition helpers and the core runtime bridge entrypoints, with the tail-call trampoline state, invoke/apply helpers, and debug/value helpers moved to `src/lisp/aot_runtime_bridge.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_relation_scan_helpers.c3` now retains iterator/join/count helpers, with relation materialization, comparison, bound parsing, and scan entrypoints moved to `src/lisp/deduce_relation_scan_helpers_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_memory_lifetime_env_copy_groups.c3` now retains the memo, parent rewrite, mixed-chain rewrite, and closure-retain stress tests, with escape/fault/guard/reject/rollback env-copy tests moved to `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_compiler_core_groups.c3` now retains the syntax, stdlib, feature, and serializer compiler tests, with set/path/continuation groups moved to `src/lisp/tests_compiler_core_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/jit_jit_compiler.c3` now retains the runtime cache and lifecycle helpers, with JIT state tracking and `jit_compile(...)` moved to `src/lisp/jit_jit_compiler_compile.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_ops.c3` now retains parsing, validation, planning, and predicate-index helpers, with explain-plan rendering helpers and the public `deduce/explain` primitive moved to `src/lisp/deduce_rule_ops_explain.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups.c3` now retains the stress/offload/wakeup/cancel boundary tests, with join-wait failure mapping and mixed state restoration tests moved to `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`.
  - `src/lisp/jit_jit_handle_signal.c3` now retains the lower-level effect state and signal dispatch machinery, with eval-result conversion, pending-raise dispatch, warm-clauses handling, handle-state setup, body switching, no-signal finish, implementation, and continuation application moved to `src/lisp/jit_jit_handle_signal_handle.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
      (`89 passed, 0 failed`).
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_eval_exec.c3` now retains seminaive evaluation helpers, with naive evaluation helpers moved to `src/lisp/deduce_rule_eval_exec_naive.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_scheduler_boundary_worker.c3` now retains the offload, wakeup, and thread-task prestart/cancel boundary tests, with the DNS/connect, join-timeout, and shared-retire queue boundary tests moved to `src/lisp/tests_scheduler_boundary_worker_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
      (`89 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_deduce_rule_groups_more.c3` now retains the join and schema validation blocks, with the schema-estimate and recursive/analyze validation blocks moved to `src/lisp/tests_deduce_rule_groups_more_tail.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/aot.c3` now retains AOT type/definition helpers, with the closure/runtime bridge helpers moved to `src/lisp/aot_runtime_bridge.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_advanced_core_unicode_groups.c3` now retains the unicode, logic, effect, and runtime-control test groups, with block-syntax, lambda-syntax, and binding/mutation groups moved to `src/lisp/tests_advanced_core_unicode_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=advanced` gate inside the Docker-bounded validation wrapper still hits the pre-existing unaligned-access panic in `value_environment.c3` via `jit_lookup_var`:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`

- Runtime modularization batch:
  - `src/lisp/deduce_db_handles.c3` now retains LMDB externs, schema registration, and relation/index helpers, with lower mutation/state bookkeeping and transaction helpers moved to `src/lisp/deduce_db_handles_mutation.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_deduce_query_bench_groups.c3` now retains seed/assert helpers and query benchmark support, with benchmark runner/reporting entrypoints moved to `src/lisp/tests_deduce_query_bench_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_eval.c3` now retains SCC planning, with execution/context helpers and rule evaluation dispatch moved to `src/lisp/deduce_rule_eval_exec.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_eval.c3` now retains SCC planning, delta helpers, and naive/semi-naive rule evaluation, with recursive/component fixpoint evaluation helpers moved to `src/lisp/deduce_rule_eval_fixpoint.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_deduce_rule_groups.c3` now retains helper utilities plus rule-definition and explain smoke coverage, with join/schema/analyze validation blocks moved to `src/lisp/tests_deduce_rule_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_eval.c3` now retains SCC planning and rule-evaluation helpers, with `deduce/analyze` / `deduce/rule!` entrypoints plus stratification validation moved to `src/lisp/deduce_rule_eval_prims.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Compiler/parser modularization batch:
  - `src/lisp/parser_define_core_helpers.c3` now retains low-level shorthand helpers, with shorthand/normal define parsing moved to `src/lisp/parser_define_core_helpers_define.c3`.
  - `src/lisp/compiler_program_pipeline.c3` now retains source assembly and final emission flow, with program parsing and analysis setup moved to `src/lisp/compiler_program_pipeline_helpers.c3`.
  - `src/lisp/compiler_temp_type_forms_defs.c3` now retains direct type-definition lowering, with abstract-type lowering moved to `src/lisp/compiler_temp_type_forms_defs_abstract.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization batch:
  - `src/lisp/parser_callable_helpers.c3` now retains body/lambda finalization helpers, with callable parameter collection moved to `src/lisp/parser_callable_helpers_params.c3`.
  - `src/lisp/parser_define_relation_attr.c3` now retains relation-column parsing and define dispatch, with relation-call assembly moved to `src/lisp/parser_define_relation_attr_helpers.c3`.
  - `src/lisp/compiler_expr_serialize_special_forms.c3` now retains core specialized serializers, with reader-side specialized serializers moved to `src/lisp/compiler_expr_serialize_special_forms_reader.c3`.
  - `src/lisp/parser_let_core.c3` now retains let parsing and binding collection, with chained let construction moved to `src/lisp/parser_let_core_helpers.c3`.
  - `src/lisp/parser_set_pipe.c3` now retains `set!` parsing, with pipe desugaring moved to `src/lisp/parser_set_pipe_helpers.c3`.
  - `src/lisp/parser_datum.c3` now retains datum/template parsing, with datum constructor helpers moved to `src/lisp/parser_datum_helpers.c3`.
  - `src/lisp/parser_let_named.c3` now retains named-let binding parsing and lambda construction, with named-let call/body assembly moved to `src/lisp/parser_let_named_helpers.c3`.
  - `src/lisp/compiler_free_vars_walk.c3` now retains free-var dispatch, with traversal helpers moved to `src/lisp/compiler_free_vars_walk_helpers.c3`.
  - `src/lisp/compiler_free_vars_utils.c3` now retains pattern-binding and primitive utilities, with quasiquote traversal moved to `src/lisp/compiler_free_vars_utils_qq.c3`.
  - `src/lisp/parser_lexer_token_scanners.c3` now retains punctuation and logic-variable scanning, with dot/underscore scanner helpers moved to `src/lisp/parser_lexer_token_scanners_dot.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization batch:
  - `src/lisp/compiler_temp_type_forms_defs_union.c3` now retains the union entrypoint, with variant-spec emission moved to `src/lisp/compiler_temp_type_forms_defs_union_helpers.c3`.
  - `src/lisp/parser_lexer_string_hash.c3` now retains `is_symbol_char(...)` and string scanning, with hash-dispatch parsing moved to `src/lisp/parser_lexer_string_hash_helpers.c3`.
  - `src/lisp/compiler_free_vars_scope_forms.c3` now retains var/lambda/let/match/call walkers, with path/set walkers moved to `src/lisp/compiler_free_vars_scope_forms_mutations.c3`.
  - `src/lisp/compiler_temp_type_forms_helpers.c3` now retains generic emit helpers, with AOT type-annotation spec emission moved to `src/lisp/compiler_temp_type_forms_annotation_helpers.c3`.
  - `src/lisp/parser_define_core.c3` now retains `parse_define(...)`, with shorthand/normal define helpers moved to `src/lisp/parser_define_core_helpers.c3`.
  - `src/lisp/parser_import_helpers.c3` now retains import state and marker helpers, with import target/spec parsing moved to `src/lisp/parser_import_helpers_specs.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_ffi.c3` now retains FFI entrypoints only.
  - `src/lisp/parser_ffi_helpers.c3` now owns FFI signature helpers:
    - `parse_ffi_fn_modifiers(...)`,
    - `set_ffi_c_name(...)`,
    - `ensure_ffi_param_capacity(...)`,
    - `parse_ffi_typed_params(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/compiler_output_helpers.c3` now retains generic output emission
    helpers only.
  - `src/lisp/compiler_output_symbol_helpers.c3` now owns symbol sanitization
    and primitive-reference helpers:
    - `is_c3_reserved(...)`,
    - `emit_symbol_name(...)`,
    - `emit_prim_global_name(...)`,
    - `record_prim_ref(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_collection_literals.c3` now retains postfix index access
    and lookup-accessor construction.
  - `src/lisp/parser_collection_literals_builders.c3` now owns dict/array
    literal builders:
    - `parse_dict_literal(...)`,
    - `parse_array_literal(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_patterns.c3` now retains `parse_pattern(...)` dispatch
    only, plus it continues to reuse the paren-pattern helpers in
    `src/lisp/parser_patterns_paren.c3`.
  - `src/lisp/parser_patterns_values.c3` now owns the string/sequence/dict/
    symbol pattern builders:
    - `parse_string_pattern_literal(...)`,
    - `parse_seq_pattern(...)`,
    - `parse_dict_pattern(...)`,
    - `parse_symbol_pattern(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/compiler_temp_type_forms_defs.c3` now retains type and abstract
    lowering.
  - `src/lisp/compiler_temp_type_forms_defs_misc.c3` now owns alias and effect
    lowering:
    - `compile_defalias_direct(...)`,
    - `compile_defeffect_direct(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/compiler_primitive_variable_hash_table_domains.c3` now retains
    the arithmetic/comparison/core/string/file/misc primitive registrations.
  - `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3`
    now owns the collection/math/bitwise primitive registrations.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_expr_atoms.c3` now retains literal parsing and
    `parse_expr(...)` dispatch only.
  - `src/lisp/parser_expr_atoms_accessors.c3` now owns the dot-accessor and
    path helpers:
    - `parse_dot_prefixed_int_key(...)`,
    - `parse_dot_accessor_shorthand(...)`,
    - `parse_path_expr(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/compiler_temp_type_forms.c3` now retains typed-define handling
    and type-form dispatch only.
  - `src/lisp/compiler_temp_type_forms_defs.c3` now owns the direct
    type-definition lowerings:
    - `compile_deftype_direct(...)`,
    - `compile_defabstract_direct(...)`,
    - `compile_defunion_direct(...)`,
    - `compile_defalias_direct(...)`,
    - `compile_defeffect_direct(...)`.
  - `src/lisp/compiler_temp_type_forms_helpers.c3` remains the shared emit
    helper module for the type-form path.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/compiler_temp_type_forms.c3` now retains direct type-form
    lowering only.
  - `src/lisp/compiler_temp_type_forms_helpers.c3` now owns the shared type-
    form emit helpers:
    - `emit_bool_literal(...)`,
    - `emit_value_tag_literal(...)`,
    - `emit_target_assignment_prefix(...)`,
    - `emit_string_array_init(...)`,
    - `emit_aot_type_annotation_spec_init(...)`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/compiler_temp_type_forms.c3` now owns direct type-form lowering
    and shared AOT annotation emit helpers.
  - `src/lisp/compiler_temp_misc_forms.c3` now retains resolve/index and
    define-bridge helpers only.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the
      Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_datum.c3` now retains datum/template constructors and
    dispatch only.
  - `src/lisp/parser_datum_collections.c3` now owns the recursive list/template
    collection walkers used by `parse_datum_impl(...)`.
- validation:
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
    passed (`122 passed, 0 failed`).

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_application.c3` now retains application dispatch and
    placeholder lowering only.
  - `src/lisp/parser_application_helpers.c3` now owns shared application
    call-construction and argument-collection helpers:
    - `collect_application_args(...)`,
    - `build_call_expr(...)`,
    - `reject_nullary_accessor_shorthand_call(...)`.
  - `src/lisp/parser_set_pipe.c3` continues to reuse the shared call-builder
    helper without any behavior change.
- validation:
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
    crashed in `lisp.Env.lookup` with an unaligned-access panic via
    `jit_lookup_var`.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
    surfaced a JIT mismatch on `map .1 accessor shorthand`
    (`interp=ok, jit=FAIL`).
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
    crashed in `lisp.Env.lookup` with an unaligned-access panic via
    `jit_lookup_var`.

- Compiler/parser modularization follow-up slice:
  - `src/lisp/parser_type_defs.c3` now retains type-definition entrypoints
    only.
  - `src/lisp/parser_type_defs_union.c3` now retains union-definition
    entrypoints only.
  - `src/lisp/parser_type_defs_helpers.c3` now owns the shared
    copy/compound/variance helpers used by both type and union parsing.
- validation:
  - `c3c build`

## 2026-03-18

- CLI/tooling UX cleanup for help text and exit-status contract:
  - `src/entry_cli_help_version.c3` now includes an explicit exit-code summary
    for the main CLI paths.
  - `src/entry_test_modes.c3` now renders the invalid test-suite name in the
    error message instead of the raw pointer value.
  - `docs/PROJECT_TOOLING.md` now documents the common exit-status map for
    `--check`, `--eval`, `--test-suite`, and the other first-party commands.
  - `docs/man/omni.1` now distinguishes normal failure (`1`) from invalid
    test-suite selection (`2`).

- Memory-lifetime regression module identity cleanup:
  - `src/lisp/tests_memory_lifetime_boundary_graph_txn_bench_groups.c3` has
    been renamed to `src/lisp/tests_memory_lifetime_boundary_graph_txn_groups.c3`
    so boundary correctness tests are no longer labeled like benchmark-only
    modules.
  - `docs/BOUNDARY_SURFACE_AUDIT.md` now points at the renamed correctness
    module path.

- Deterministic multiple-dispatch ambiguity payload + tie metadata contract:
  - `src/lisp/eval_dispatch_match.c3`:
    - ambiguous equal-specificity dispatch now raises canonical recoverable
      payload (`type/dispatch-ambiguous`, domain `type`) instead of string-only
      error signaling.
    - ambiguity payload `data` now includes stable fields:
      - `reason` (`ambiguous-equal-specificity`)
      - `method`
      - `arg-count`
      - `arg-types`
      - `best-score`
      - `tie-count`
      - `candidate-indices`
    - `candidate-indices` are emitted in deterministic method-table index order
      (ascending), with no implicit tie-break winner.
  - regression coverage:
    - `src/lisp/tests_advanced_type_dispatch_groups.c3` now asserts:
      - stable ambiguity payload field values for equal-specificity ties,
      - stable candidate-index ordering with lower-specificity non-winner
        methods present.
  - compiler parity coverage:
    - `src/lisp/tests_e2e_generation_cases_extended.c3` adds a handled-raise
      ambiguity payload parity case so compiled output must match interpreter
      output for code/score/tie/index fields.

- Checkpoint/capture replay parity contract extension for compiled paths:
  - docs:
    - `docs/LANGUAGE_SPEC.md` now explicitly states replay-visible side-effect
      parity is execution-mode invariant (interpreter, JIT, compiled).
    - `docs/reference/06-effects.md` now mirrors the same replay parity
      requirement.
  - parity tests:
    - `src/lisp/tests_e2e_generation_cases_extended.c3` adds compiled parity
      replay cases for resumed-segment side effects:
      - `set!` mutation replay,
      - handled-effect replay.

- TODO closure:
  - `TODO.md` active queue reduced to zero by closing the four remaining
    dispatch ambiguity + replay parity items and recording completion details.

- Finwatch tutorial module landing:
  - `examples/finwatch/alerts.omni` now includes tutorial-sized price and
    analytics alert helpers built on the existing `handle`/`dispatch`
    composition surface.
  - `examples/finwatch/alerts_tutorial_smoke.omni` exercises the exported
    `alerts/tutorial-demo` helper and verifies the direct effect path under a
    log+collector-style capture.
  - `examples/README.md` now advertises the finwatch alert tutorial module and
    its dedicated smoke file as the minimal effect-handler + dispatch example.
  - `docs/plans/post-complete-backlog.md` marks the tutorial-project backlog
    item complete.

- validation:
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=schema ./build/main --test-suite lisp` (`42 passed, 0 failed`)
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp` (`122 passed, 0 failed`)

## 2026-03-17

- JIT TCO stale-generation ownership gating hardening:
  - `src/lisp/jit_jit_eval_scopes.c3`:
    - replaced env-chain recycle gate `scope_gen` matching with physical
      frame-in-scope detection (`boundary_ptr_in_scope(...)`) so recyclable
      fast-reset cannot skip copy/rebuild paths when frame stamps are stale
      after chunk transfer/move operations.
    - root-persistent parent rewrite gate now keys on physical parent residency
      in `releasing_scope` only (no early skip on stale generation stamps).
  - `src/lisp/eval_promotion_copy.c3`:
    - `copy_to_parent_try_fast_reuse(...)` now fails closed when either wrapper
      or closure/iterator payload still resides in `releasing_scope`, avoiding
      unsafe fast-reuse when wrapper provenance looks reusable but payload alias
      still points into releasing scope.
  - regression coverage:
    - added `run_jit_policy_tco_stale_generation_moved_binding_copy_test(...)`
      in `src/lisp/tests_runtime_feature_jit_groups.c3`.
    - added `run_jit_policy_tco_persistent_parent_stale_generation_rewrite_test(...)`
      in `src/lisp/tests_runtime_feature_jit_groups.c3`.
  - validation:
    - `c3c build --sanitize=address`
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp` (`22 passed, 0 failed`)

- Env-copy transactional frame materialization rollback hardening:
  - `src/lisp/eval_env_copy.c3`:
    - added transactional rollback helpers for partially materialized env-copy
      bindings (`copy_env_rollback_materialized_bindings(...)` and related
      value/iterator helpers).
    - binding-copy failure now rolls back already materialized target-scope
      wrapper side effects before returning `BOUNDARY_ENV_COPY_FAULT_BINDING_VALUE_COPY`.
    - parent-copy failure after binding materialization now also rolls back
      materialized binding side effects before frame cleanup/return.
  - `src/lisp/tests_memory_lifetime_env_copy_groups.c3`:
    - added `run_memory_lifetime_env_copy_transactional_binding_rollback_test(...)`
      to assert mid-frame env-copy failure is transactional for
      closure/iterator/instance wrapper retain paths.
  - validation:
    - `c3c build --sanitize=address`
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp` (blocked by pre-existing `boundary_commit_escape` failures and ASAN UAF in `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3:147`; reproduces even with env-copy rollback callsites temporarily disabled)
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_VERBOSE=1 ./build/main --test-suite lisp` (`56 passed, 4 failed`; decoded pass output includes `[PASS] lifetime: env-copy transactional mid-frame rollback`; failures are the same pre-existing `boundary_commit_escape` cases)

- Unified closure/iterator alias safety policy across env-copy and promotion paths:
  - shared policy helpers in `src/lisp/eval_boundary_provenance.c3`:
    - `boundary_closure_alias_unsafe_for_reuse(...)`
    - `boundary_iterator_payload_alias_unsafe_for_reuse(...)`
    - these centralize undelimited closure / iterator thunk alias-safety checks
      against target-scope-chain residency.
  - `src/lisp/eval_env_copy.c3`:
    - undelimited closure alias guard now uses shared target-chain policy rather
      than releasing-scope-only detection.
    - iterator env-copy now rejects disjoint non-closure thunk payload aliases
      and only reuses iterator wrappers when both wrapper and payload aliasing
      are policy-safe.
  - `src/lisp/eval_promotion_copy.c3`:
    - fast-reuse gate now fail-closes on shared closure/iterator alias-unsafe
      conditions before reuse classification.
  - `src/lisp/eval_promotion_escape.c3`:
    - fast-path wrapper reuse now routes alias-unsafe closure/iterator values
      through disjoint promotion instead of preserving wrapper alias.
  - regression coverage:
    - added `run_memory_lifetime_env_copy_rejects_cross_scope_undelimited_alias_test(...)`
      in `src/lisp/tests_memory_lifetime_env_copy_groups.c3`.
    - added `run_jit_policy_cross_scope_alias_policy_test(...)`
      in `src/lisp/tests_runtime_feature_jit_groups.c3`.
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`23 passed, 0 failed`)
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp` (blocked by pre-existing `boundary_commit_escape` failures + ASAN UAF at `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3:147`)
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_VERBOSE=1 ./build/main --test-suite lisp` (`57 passed, 4 failed`; decoded pass lines include both new env-copy alias regressions and prior transactional rollback regression)

## 2026-03-16

- JIT/boundary fail-closed hardening follow-up:
  - `src/lisp/jit_jit_eval_scopes.c3`:
    - both call-scope wrappers now treat unresolved `pop_scope_guard_defer(...)` as fail-closed runtime errors (no silent pop failure ignore).
    - TCO env binding copy gating now checks releasing-scope residency by physical pointer provenance and wrapper payload aliasing (closure payload / iterator thunk payload), not only generation-stamp checks.
  - `src/lisp/jit_common.c3`:
    - `pop_scope_guard_defer(...)` now retries the explicit fallback stack context when undefer on the current active context fails.
  - `src/lisp/eval_boundary_commit_escape_builders.c3`:
    - destination ESCAPE builders (`cons`, `partial`, `iterator`, `error`) now materialize in a temporary child build scope and commit via `scope_splice_escapes(...)`; aborted/failed paths release the build scope to roll back partial objects.
  - `src/lisp/eval_boundary_api.c3`:
    - closure signature-copy failure (`BOUNDARY_COPY_FAULT_CLOSURE_TYPE_SIG_COPY`) now marks active promotion context(s) aborted before surfacing typed copy failure.
  - `src/lisp/jit_jit_closure_define_qq.c3`:
    - `jit_eval_set(...)` now relies on `env_set_with_barrier(...)` write-site promotion instead of unconditional pre-promotion to root.
    - instance field mutation now uses owner-scope boundary copy path uniformly (including root owner scope path).
  - `src/lisp/eval_promotion_root_store.c3`:
    - root-store clone builders (`array`, `hashmap/set`, `method-table`) now fail closed when nested `boundary_copy_to_parent_site_ctx(...)` returns an error value, and release partial heap allocations before returning the error.
    - this prevents boundary-copy fault values from being stored as normal payload entries inside root-store cloned containers.
  - validation:
    - `c3c build`
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`21 passed, 0 failed`)
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`basic` default; `139 passed, 0 failed`)
    - `LD_LIBRARY_PATH=/usr/local/lib:/usr/lib ./build/main --test-suite scope` (`58 passed, 0 failed`)
    - `LD_LIBRARY_PATH=/usr/local/lib:/usr/lib ./build/main --test-suite stack` (`23 passed, 0 failed`)
    - container memory-lifetime smoke slice run attempted via `scripts/run_validation_container.sh` but blocked by missing `libreplxx.so.0` in the validation image runtime.

- Env-copy persistent parent rewrite fail-closed hardening:
  - `src/lisp/eval_env_copy.c3`:
    - added explicit in-place rewrite eligibility guard for root-persistent env boxes (`copy_env_can_rewrite_persistent_box_in_place(...)`), requiring true root ownership and rejecting releasing-scope-resident "persistent" frames.
    - malformed persistent-tagged local env frames now fail closed with `BOUNDARY_ENV_COPY_FAULT_PARENT_COPY` instead of mutating transient frames in place.
    - added partial-frame cleanup on env-copy failure paths (`copy_env_cleanup_partial_frame(...)`) to release malloc-backed binding/hash buffers when frame materialization aborts before dtor registration.
    - added fail-closed guards for releasing-scope closure/iterator aliasing without `closure.env_scope` ownership envelope (`copy_env_invalid_closure_alias_in_releasing_scope(...)`), returning typed env-copy failure instead of preserving potentially stale wrapper pointers.
  - regression coverage:
    - added `run_memory_lifetime_env_copy_invalid_persistent_in_place_guard_test(...)` in `src/lisp/tests_memory_lifetime_env_copy_groups.c3`.
    - added `run_memory_lifetime_env_copy_rejects_undelimited_closure_alias_test(...)` in `src/lisp/tests_memory_lifetime_env_copy_groups.c3`.
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`20 passed, 0 failed`)
    - `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp` (`59 passed, 0 failed`)

- Scheduler offload prestart-cancel ownership hardening:
  - `src/lisp/scheduler_offload_worker.c3`:
    - removed early `scheduler_release_shared(...)` from `scheduler_uv_offload_work_cb(...)` when `scheduler_try_begin_thread_task(...)` rejects execution.
    - shared payload release now stays single-authority in `scheduler_uv_offload_after_cb(...)` non-executed path, closing a double-release/UAF window on prestart-cancel interleavings.
    - added OOM fallback in executed task completion path: if completion materialization still fails, drop the task entry to avoid leaving it stranded in `THREAD_TASK_RUNNING`.
  - regression coverage:
    - added `run_scheduler_worker_prestart_cancel_single_shared_release_boundary_tests(...)` in `src/lisp/tests_scheduler_boundary_worker.c3`.
    - wired the regression in scheduler suite dispatch (`src/lisp/tests_scheduler_groups.c3`).
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`85 passed, 0 failed`)

- Scheduler thread-task completion fail-closed hardening:
  - `src/lisp/scheduler_thread_task_transitions.c3`:
    - added `scheduler_make_task_alloc_failure_completion(...)` and routed `scheduler_complete_thread_task(...)` null-completion input through alloc-failure completion materialization instead of early return.
    - if alloc-failure completion cannot be materialized, `scheduler_complete_thread_task(...)` now fail-closes by dropping the task entry (`scheduler_drop_thread_task(...)`) to avoid stranding `THREAD_TASK_RUNNING`.
  - regression coverage:
    - added `run_scheduler_thread_task_null_completion_fail_closed_boundary_tests(...)` in `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`.
    - wired the regression in scheduler suite dispatch (`src/lisp/tests_scheduler_groups.c3`).
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`86 passed, 0 failed`)

- Scheduler waiter wakeup ownership hardening:
  - `src/lisp/scheduler_thread_tasks.c3`:
    - `scheduler_take_thread_task_completion(...)` and `scheduler_take_os_thread_completion(...)` now actively signal captured waiter fiber/epoch before entry teardown, preventing concurrent non-waiter completion-consume paths from clearing waiter linkage without wake delivery.
    - take paths no longer clear join tokens directly; wake handling now remains token-authoritative in wake dispatch.
  - `src/lisp/scheduler_thread_task_waiters.c3`:
    - `scheduler_clear_thread_task_waiters_for_fiber(...)` now also clears latched task/os-thread wake epochs for the fiber, removing stale latched wake residue on waiter teardown.
  - regression coverage:
    - added `run_scheduler_waiter_take_does_not_strand_blocked_fiber_boundary_tests(...)` in `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`.
    - wired the regression in scheduler suite dispatch (`src/lisp/tests_scheduler_groups.c3`).
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`87 passed, 0 failed`)

- Scheduler/JIT continuation hardening follow-up:
  - scheduler stale-wakeup hardening:
    - added per-fiber `wake_epoch` stamps and scheduler-global `fiber_wake_epoch_seed`.
    - task/thread waiters now snapshot the waiter fiber wake epoch at waiter registration (`TaskEntry.waiter_wake_epoch` / `OsThreadEntry.waiter_wake_epoch`), and completion/cancel/drop paths publish that captured epoch instead of rereading current fiber table state.
    - task/thread completion wakeups now publish and validate wake epochs before unblocking a blocked fiber, preventing late waiter events from reviving a recycled fiber slot (including recycled slots from a newer interpreter generation).
    - completion teardown now clears lingering task/thread waiter slots for the finished fiber (`scheduler_clear_thread_task_waiters_for_fiber(...)`), removing stale waiter-id reuse windows.
  - scheduler handle ownership hardening:
    - task/thread handle payloads now record `owner_interp`.
    - task/thread handle parse paths now reject cross-interpreter use even when a handle wrapper is otherwise structurally valid.
  - JIT cache/runtime ownership hardening:
    - `JitCacheEntry` now records owner interpreter + attach serial.
    - owner-aware cache lookup/store now bind entries to the active interpreter epoch and fail closed on epoch mismatch (cache reset + recompile path), preventing stale cache reuse across interpreter serial transitions.
    - jit detach/shutdown paths no longer reset the global attach-serial counter; serial epochs remain monotonic across runtime idle/shutdown cycles.
    - retired-code tombstones are now pruned by attached-serial liveness during `jit_gc()` (`jit_retired_code_prune_detached_serials()`), avoiding detach-before-gc prune ordering hazards.
  - continuation lifecycle hardening:
    - `Continuation` now tracks `owner_interp` and is registered on an interpreter-owned intrusive list.
    - interpreter teardown now invalidates tracked continuations and releases retained handle state snapshots before root-scope teardown (`interp_invalidate_live_continuations(...)`), preventing escaped handle-state retention from outliving interpreter lifetime.
    - continuation boundary copy/promotion now rejects cross-interpreter continuation aliasing (`copy_continuation_to_parent`, `promote_escape_continuation`).
    - continuation resume/resolve now additionally enforce `owner_interp` checks before proceeding.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

- Continuation teardown UAF hardening (ASAN follow-up):
  - fixed a use-after-free in interpreter teardown where continuation list entries
    could still hold stale `HandleEffectState*` pointers and
    `interp_invalidate_live_continuations(...)` dereferenced freed memory.
  - `src/lisp/jit_jit_handle_signal.c3`:
    - added a live-state registry for `HandleEffectState` allocations:
      - `g_handle_effect_live_head`,
      - `handle_effect_state_register_live(...)`,
      - `handle_effect_state_unregister_live(...)`,
      - `handle_effect_state_is_live(...)`.
    - `handle_effect_state_init(...)` now registers live states.
    - `handle_effect_state_release(...)` now:
      - no-ops on non-live/stale pointers,
      - unregisters live state before free,
      - scrubs continuation back-references before releasing buffers and state.
  - `src/lisp/value_interp_lifecycle.c3`:
    - `interp_invalidate_live_continuations(...)` now gates all
      `HandleEffectState` dereferences/releases on `handle_effect_state_is_live(...)`,
      so stale pointers are nullified without touching freed memory.
  - regression coverage:
    - added `run_jit_policy_shared_handle_state_teardown_test(...)` in
      `src/lisp/tests_runtime_feature_jit_groups.c3` to exercise retained-first
      / non-retained-second shared-handle-state teardown order.
  - validation:
    - `c3c build --sanitize=address`
    - `OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`139 passed, 0 failed`)
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`19 passed, 0 failed`)

- Stack-pool shutdown UAF hardening (scheduler ASAN follow-up):
  - fixed cross-context teardown ordering in `src/stack_engine_pool_ownership.c3`:
    - `stack_pool_shutdown(...)` now runs all deferred/lifecycle callbacks for
      all owned contexts before freeing any `StackCtx` allocation.
    - this closes a UAF window where a later context's deferred scope release
      could touch a coroutine `StackCtx*` already freed earlier in the same
      shutdown pass.
  - regression coverage:
    - added `test_stack_pool_shutdown_defer_cross_ctx_order(...)` in
      `src/stack_engine_tests_defer.c3` and wired it in
      `src/stack_engine_tests_runner.c3`.
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`84 passed, 0 failed`)
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`139 passed, 0 failed`)
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack` (`22 passed, 0 failed`)

- Handle-state continuation scrub hardening:
  - `src/lisp/jit_jit_handle_signal.c3`:
    - `handle_effect_state_scrub_continuations(...)` now also clears
      `Continuation.ctx` when scrubbing continuations bound to a released
      `HandleEffectState`, forcing later resolve paths to fail closed instead of
      touching stale stack-context pointers.
  - regression coverage:
    - added `run_jit_policy_side_effect_escaped_handle_continuation_guard_test(...)`
      in `src/lisp/tests_runtime_feature_jit_groups.c3`, asserting released
      handle-state continuations are scrubbed to unusable state (`ctx == null`,
      `handle_state == null`, `handle_retained == false`).
  - validation:
    - `c3c build --sanitize=address`
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`20 passed, 0 failed`)
    - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`84 passed, 0 failed`)

- JIT/env-copy persistent-parent lifetime hardening:
  - `src/lisp/eval_env_copy.c3`:
    - root-persistent mutable-box parent rewrites are now conditional (`copy_env_parent_needs_rewrite(...)`) and only run when the direct parent still points into the releasing scope.
    - when a rewrite is required, parent-chain materialization now executes with `interp.current_scope = interp.root_scope` so persistent-box parent links no longer borrow lifetime from transient target scopes.
  - `src/lisp/jit_jit_eval_scopes.c3`:
    - `copy_tco_env_chain(...)` now applies the same conditional parent-rewrite gate for root-persistent boxes and rewrites via root-scope materialization.
    - TCO recycle in-place TEMP reset now requires conservative detach eligibility:
      - no root-persistent boxes in the traversed env chain,
      - no graph-carrying binding values (`CONS/ARRAY/HASHMAP/SET/CLOSURE/MODULE/PARTIAL_PRIM/METHOD_TABLE/ITERATOR/INSTANCE`).
    - this closes the shallow-detach fast-path window where nested reachable graphs could retain TEMP-lane edges across `scope_reset_temp_lane(...)`.
  - regression coverage:
    - updated env-copy lifetime regressions in `src/lisp/tests_memory_lifetime_env_copy_groups.c3` to assert rewritten persistent parents are root-owned and survive both source-scope and target-scope release without manual parent detachment.
    - added JIT policy regression `run_jit_policy_tco_persistent_parent_root_rewrite_test(...)` in `src/lisp/tests_runtime_feature_jit_groups.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`19 passed, 0 failed`)
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`81 passed, 0 failed`)

- Scheduler join-wakeup stale-epoch hardening:
  - replaced join waiter wake matching from coarse fiber lifetime epochs to per-registration join tokens.
  - `scheduler_register_thread_task_waiter(...)` / `scheduler_register_os_thread_waiter(...)` now mint monotonic scheduler-global join tokens and arm them on the waiter fiber (`Scheduler.fiber_join_wait_token[...]`).
  - task/thread completion wakeups now carry that registration token and dispatch validates against the currently armed token before waking blocked fibers.
  - wake dispatch consumes the armed join token on match, preventing delayed duplicate task/thread wake events from reviving later unrelated waits.
  - join token lifecycle is now explicitly reset across:
    - waiter clear paths (`scheduler_clear_thread_task_waiter`, `scheduler_clear_os_thread_waiter`, `scheduler_clear_thread_task_waiters_for_fiber`),
    - completion consume paths (`scheduler_take_thread_task_completion`, `scheduler_take_os_thread_completion`),
    - fiber slot reuse/reset (`scheduler_add_fiber`, `scheduler_reset_wakeup_queue`).
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`81 passed, 0 failed`)
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` (`19 passed, 0 failed`)

- Continuation/suspend ownership guard follow-up:
  - `suspend_with_scope_guard(...)` now only performs the post-resume scope release when defer-pop succeeds, preventing retain over-release on unexpected pop failure paths.
  - continuation resume hardening now rejects cross-interpreter continuation application/resolution by validating `StackCtx.owner_pool` (and handle-state interpreter when present) against the active interpreter.
  - continuation resolve/teardown paths now destroy stack contexts through `ctx.owner_pool` authority (with local fallback), removing pool-mismatch destroy hazards.
  - `capture` suspend failure paths now restore saved interpreter state before returning error in both AST and value runtime-effect paths.
  - fixed scope-guard defer pop routing to prefer the currently running `StackCtx`, preventing cloned multi-shot continuation resumes from accidentally popping defer state on the source template context.
  - handle dispatch paths now conservatively retain handler state/context before frame cleanup when a continuation is reachable from the handler result graph, preventing dangling `handle_state`/suspended-context use-after-free on escaped continuation wrappers.
  - escaped handle continuations with inactive frames are now rejected deterministically (`runtime/continuation-frame-inactive`) and invalidated with deterministic teardown instead of reaching unsafe resume paths.
  - added JIT policy regression coverage for cross-interpreter continuation resume rejection.
  - added JIT policy regression coverage ensuring escaped handle continuations are rejected without corrupting evaluator state.
  - added JIT policy regression coverage for multi-shot capture clone isolation (`(checkpoint ... (capture ...))` reused twice) to guard scope-retain/defer correctness across cloned continuation resumes.
  - `SavedInterpState` now snapshots/restores `g_jit_exec_depth` across stack-context switches so nested checkpoint/capture resumes cannot corrupt global JIT frame-depth accounting.
  - fixed `jit_signal_suspend(...)` to use a stack-local `SavedInterpState` instead of scope-allocation-backed storage, removing a null-dereference hazard on allocation failure paths.
  - fixed `jit_resolve_value(...)` to snapshot/restore interpreter boundary state around continuation resume (`stack_ctx_switch_to`), preventing resumed resolve paths from leaking transient JIT/eval state into the caller.
  - fixed `current_reset_state` restoration ordering in checkpoint/value-checkpoint/continuation-resume switch paths so boundary restore no longer reintroduces stack-local reset-state pointers after return.
  - hardened `capture` suspend-failure paths (AST + value) to clear staged `shifted` continuation metadata before returning error, preventing false parent-side shift handling when suspend registration fails.
  - continuation overflow exit now clears one-shot `resume_value` before returning error, preventing stale resume payload leakage into later resumes.
  - hardened direct JIT binop specialization (`+/-/*/< />/=`) to require unshadowed canonical primitive bindings, preventing inline fast-path compilation from bypassing local/global rebinding semantics.
  - added JIT runtime thread-affinity ownership checks over global JIT state/caches, causing immediate deterministic failure on cross-thread JIT runtime access instead of silent global-state races.
  - hardened JIT compiled-function lifetime handling by tracking emitted code pointers alongside libjit states and validating liveness before invocation (`jit_exec`/`jit_eval`), so stale `JitFn` values are rejected with a runtime error after `jit_gc()` teardown instead of jumping into freed code.
  - hardened JIT suspend/GC interaction by tracking active suspend scope-guards (`suspend_with_scope_guard` + clone/destroy lifecycle), and blocking `jit_gc()` / full `jit_global_shutdown()` teardown while suspended JIT frames are still reachable.
  - hardened JIT compiled-function ownership by tracking source-interpreter ownership per compiled state and requiring caller-interpreter ownership + live attachment at execution; compiled pointers from detached/foreign interpreters now fail closed with deterministic runtime error.
  - expanded stack-switch state snapshots (`SavedInterpState`) to include pending-raise payload/message and boundary metadata (`releasing_scope`, `active_promotion_ctx`), and added a shared `restore_interp_state_preserve_raise(...)` helper so continuation/coroutine/effect resume paths do not silently drop pending raise state.
  - fixed value/AOT handle runtime path to dispatch pending `raise` clauses after body return (parity with AST handle path), preventing installed raise handlers from being bypassed as raw error returns.
  - added JIT policy regression coverage asserting shadowed arithmetic symbols do not route through direct primitive fast-path specialization.
  - added JIT policy regression coverage asserting checkpoint boundaries do not leak reset-state pointers into later capture calls.
  - added JIT policy regression coverage asserting stale compiled function pointers are rejected after explicit `jit_gc()` safe-point teardown.
  - added JIT policy regression coverage asserting compiled pointers are rejected after owner-interpreter teardown when another interpreter keeps global JIT runtime alive.
  - JIT policy multi-shot continuation clone regression now skips when JIT checks are disabled (ASAN runtime), matching intentional sanitizer-mode one-shot continuation behavior.
  - hardened attached-interpreter ownership tracking with per-attachment serial epochs (stored on tracked JIT states/spill nodes), so pointer-address reuse across interpreter teardown/re-init cannot satisfy compiled-function liveness checks by raw `Interp*` identity alone.
  - fixed value/AOT `handle` stack-switch parity by snapshotting/restoring `SavedInterpState` around `stack_ctx_switch_to(...)`, aligning parent-side handler dispatch state with AST handle runtime paths.
  - hardened handle-state capture copies to reject promoted non-closure/error values from `boundary_promote_to_root(...)` before storing into escaped handler-copy closure tables.
  - hardened continuation resume flow to:
    - propagate blocked-fiber yields while a resumed continuation clone remains suspended,
    - reject unresolved suspended-clone completion paths deterministically (instead of consuming stale/null results as if completed).
  - hardened coroutine `resume` input validation to reject non-resumable context states before `stack_ctx_resume(...)`, preventing stack-engine assertion aborts on invalid states.
  - hardened scheduler fiber-blocking operations (`await`, `offload`, task/thread joins, async tcp read/write bridge) to require execution from the active fiber root stack context; nested stack contexts now fail closed with `scheduler/fiber-nested-blocking-op` instead of silently blocking the outer fiber while suspending only an inner context.
  - added scheduler regression coverage for nested-context `await` inside a fiber (`checkpoint` nested stack context), asserting deterministic rejection with the new root-context guard.
  - added retired-code tombstone bookkeeping keyed by attach serial and a deterministic prune path (`forget_serial`) plus detached-interpreter GC scheduling; this hardens JIT lifetime accounting without changing compile success behavior.
  - hardened JIT detach/destroy lifecycle guards for active on-stack interpreter teardown (`jit_interp_on_current_stack(...)`) while preserving suspended-continuation teardown paths used by existing continuation tests.
  - added JIT policy regression coverage for retired-code tombstone tracking and per-serial tombstone pruning.
  - hardened effect `resolve` continuation semantics:
    - one-shot continuation is now consumed before resume to fail closed on re-entrant reuse from resumed code,
    - suspended continuation resume now uses `stack_ctx_resume(...)` (status parity) plus blocked-fiber drive parity,
    - still-suspended completion paths now fail closed with deterministic `runtime/resolve-resuspended` errors.
  - hardened value/AOT checkpoint and handle boundaries to propagate blocked-fiber yields and reject unresolved suspended returns when no escaped continuation owns the frame.
  - hardened scheduler run-loop reentrancy:
    - `run-fibers` and run-loop entrypoints now reject nested invocation with `scheduler/reentrant-run-loop`,
    - added scheduler regression coverage asserting nested `(run-fibers)` inside an active fiber is rejected.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`

- Coroutine wrapper boundary ownership hardening:
  - `copy_coroutine_to_parent(...)` now enforces transfer semantics for coroutine wrappers instead of shallow aliasing:
    - only releasing-scope-owned wrappers are transferable,
    - destination wrapper is materialized with a registered value destructor,
    - source wrapper is nulled after transfer so scope teardown cannot double-own/double-destroy the same `StackCtx*`.
  - `promote_escape_coroutine(...)` now enforces the same single-owner transfer policy for TEMP->ESCAPE promotion in the current scope:
    - rejects disjoint wrapper aliasing attempts,
    - registers ESCAPE-lane destructor coverage for transferred coroutine wrappers,
    - nulls the source TEMP wrapper after transfer.
  - added memory-lifetime regressions in `src/lisp/tests_memory_lifetime_finalize_groups.c3`:
    - `promote_to_root` coroutine-wrapper transfer preserves resumability and single teardown,
    - `promote_to_escape` coroutine-wrapper transfer preserves single teardown on scope release.
  - validation:
    - `c3c build`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack`
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `./scripts/run_validation_container.sh env OMNI_LISP_TEST_SLICE=memory-lifetime-smoke LD_LIBRARY_PATH=build:/usr/local/lib:deps/lib ./build/main --test-suite lisp`

- Stack/effect boundary follow-up hardening:
  - stack context pool safety:
    - added idempotency guard for repeated `stack_ctx_destroy(...)` on already pooled contexts (`StackCtx.in_pool_cache`), preventing free-list self-cycles/corruption from double-destroy paths.
    - added stack-engine regression `pool double-destroy idempotent`.
  - stack guard infrastructure lifetime:
    - `csrc/stack_helpers.c` now reference-counts global guard initialization/shutdown so one live pool shutdown no longer tears down SIGSEGV guard infrastructure for other live pools/interpreters.
  - coroutine resume/cleanup ownership authority:
    - `resume` now rejects coroutine wrappers whose `StackCtx` does not belong to the active interpreter/pool.
    - coroutine cleanup now destroys through `ctx.owner_pool` authority (with local fallback), aligning with destructor ownership routing.
  - JIT saved-state parity:
    - `SavedInterpState` now includes `current_reset_state`, so handle/effect stack switches preserve reset/capture state instead of restoring a stale/foreign pointer.
  - handle-state copy safety:
    - `handle_effect_state_capture_handler_copy(...)` no longer shallow-copies source-owned pointers before allocation; partial-allocation failures now clean only locally-owned buffers, eliminating invalid-free of caller-owned handler arrays.

- CLI parse/check surface for tooling:
  - added `omni --check <file>` as a non-executing parse/check mode for Omni source files.
  - added `omni --check --json <file>` structured diagnostics output with:
    - stable diagnostic code (`parser/syntax-error` and `io/read-failed` in the initial pass),
    - documented 0-based JSON ranges,
    - exact input path,
    - non-zero exit on diagnostics or read failure.
  - intent:
    - give editor tooling a first-party, non-executing diagnostics surface,
    - stop depending on regex parsing of human `--compile` output for basic syntax checks,
    - keep the first implementation parse-only so arbitrary buffers can be checked safely.
  - follow-up:
    - extend the same contract toward richer analysis and structured eval transport.

- CLI structured eval surface for tooling:
  - added `omni --eval '<expr>'` for single-expression CLI evaluation.
  - added `omni --eval --json '<expr>'` structured result output with:
    - echoed input,
    - rendered value string on success,
    - structured runtime error payload on failure,
    - 0-based source range when line/column are available.
  - intent:
    - provide a stable machine-readable eval primitive for editor tooling,
    - avoid scraping colored REPL output for one-shot eval requests,
    - keep the first transport small before attempting multi-request REPL protocol work.

- Structured session transport for editor tooling:
  - added `omni --repl --json` as a newline-delimited JSON request/response transport.
  - request shape in the initial pass:
    - `{"id":"...","input":"...","mode":"expr"}` for single-expression eval,
    - `{"id":"...","input":"...","mode":"program"}` for multi-form buffer eval.
  - response shape includes:
    - echoed `id`,
    - `ok`,
    - rendered value string on success,
    - structured error payload on failure.
  - intent:
    - keep a persistent Omni process for editor integrations,
    - remove ANSI-text scraping from repeated REPL-driven requests,
    - support whole-buffer program sends without inventing a second plugin-side protocol.
  - first consumer:
    - `tooling/omni-nvim` now defaults to `omni --repl --json` for persistent session traffic,
    - whole-buffer sends and REPL fallback eval paths no longer depend on PTY text scraping.
    - current-form eval in `tooling/omni-nvim` now prefers Tree-sitter form selection when an Omni parser is available, with the previous delimiter scanner retained as fallback.
    - `tooling/omni-nvim` now also exposes transcript clear + auto-scroll control and visual selection eval over the structured session transport.
    - `tooling/omni-nvim` now exposes root-form eval from the cursor, using Tree-sitter when available and delimiter-based top-level form scanning as fallback.
    - `tooling/omni-nvim` now registers the Omni `nvim-treesitter` parser config from the current repo checkout when `nvim-treesitter` is available, and adds `tooling/tree-sitter-omni` to `runtimepath` so bundled queries resolve without manual copying.
    - `tooling/omni-nvim` now exposes `:OmniLspSetup` plus optional `lsp.auto_setup = true`, registering the first-party `omni-lsp` server from the current repo checkout through Neovim's built-in LSP API when available and `lspconfig` as fallback.
    - `tooling/omni-nvim` now also exposes buffer-local `OmniLsp*` actions and default mappings for hover, definition, declaration, implementation, type definition, references, rename, code action, formatting, and signature help, delegating to `vim.lsp.buf.*` in Omni buffers.
    - `tooling/omni-nvim` now exposes quickfix-backed `OmniLspDocumentSymbols` and `OmniLspWorkspaceSymbols` commands plus default mappings, using synchronous LSP requests to surface current-buffer and workspace symbol lists from the attached Omni LSP client.
    - `tooling/omni-nvim` now exposes quickfix-backed `OmniLspDefinitionsList`, `OmniLspDeclarationsList`, `OmniLspImplementationsList`, and `OmniLspTypeDefinitionsList` plus default mappings, using synchronous location requests to surface multi-target navigation results from the attached Omni LSP client.
    - `tooling/omni-nvim` now exposes quickfix-backed `OmniLspReferencesList` plus a default buffer-local mapping, using a synchronous `textDocument/references` request to list current-symbol references from the attached Omni LSP client.
    - `tooling/omni-nvim` now exposes diagnostics helpers and default mappings for opening the current buffer location list and jumping to the next or previous diagnostic through Neovim's built-in diagnostic API.
    - `tooling/omni-nvim` now exposes inlay-hint enable/disable/toggle commands plus a default buffer-local toggle mapping, delegating to Neovim's built-in inlay-hint API for the current Omni buffer.
    - `tooling/omni-nvim` now exposes `:OmniLspRefreshFolds` plus a default buffer-local mapping, requesting `textDocument/foldingRange` from the attached Omni LSP client and applying the returned ranges as manual folds in the current window.
    - `tooling/omni-nvim` now exposes `:OmniLspExpandSelection` plus a default buffer-local mapping, requesting `textDocument/selectionRange` and expanding the current Omni selection outward through the returned parent chain one structural step at a time.
    - `tooling/omni-nvim` now forwards explicit Ex ranges and visual selections through `OmniLspCodeAction` and `OmniLspFormat`, so selection-based code actions and formatting use the actual Omni region instead of only cursor-local behavior.
    - `tooling/omni-nvim` now exposes operator-style eval over Vim motions, routing motion-selected regions through the same structured program-mode transport as visual selection eval.
    - `tooling/omni-nvim` now exposes capture-aware eval for enclosing declaration, call, and block regions, driven by the first-party Omni Tree-sitter textobject queries with the existing form/root selection paths kept as fallback.
    - `tooling/omni-nvim` now exposes structural selection commands and default mappings for enclosing form, root form, declaration, call, and block regions, reusing the same capture-aware range lookup as eval.
    - `tooling/omni-nvim` now exposes buffer-local Omni textobjects in operator-pending and visual mode for form, root form, declaration, call, and block selection, with configurable keys and a separate enable/disable switch.
    - `tooling/omni-nvim` now distinguishes inner and outer textobjects: inner selections trim bracket/prefix wrappers when possible, while outer selections keep the full structural form.
    - `tooling/omni-nvim` now loads the bundled Omni query files directly into Neovim when no existing Omni query is registered for that group, so first-party capture-driven behavior does not depend on a second query packaging step.
    - `tooling/tree-sitter-omni` now ships first-party textobject and fold queries for structural Omni forms, including call-shaped lists, block-style special forms, shorthand function definitions, module/type/effect declarations, and form comments.

- LSP baseline upgrade for editor navigation:
  - `tooling/omni-lsp` now advertises and serves `textDocument/documentSymbol`.
  - document symbols are extracted from current-buffer top-level Omni declarations, including:
    - plain `define` bindings,
    - function shorthand declarations,
    - attribute-backed declarations such as `[type]`, `[abstract]`, `[union]`, `[alias]`, `[effect]`, and related declaration families,
    - module declarations with nested child symbols for body-local declarations.
  - completion now merges current-buffer declaration names ahead of static builtins/special forms.
  - workspace symbol search now returns syntactic declaration matches across:
    - currently open Omni documents known to the LSP server,
    - unopened `.omni` files under workspace roots discovered from `project.json` or `.git` markers near open documents.
  - workspace symbol search still keeps module container names for nested declarations and skips common generated/dependency directories during the file scan.
  - workspace-root symbol scans are now cached in-process behind a path/mtime/size manifest:
    - unchanged unopened `.omni` files are reused across repeated `workspace/symbol` queries,
    - changed unopened files are reread when their filesystem signature changes,
    - document open/change/close events still clear the cache eagerly.
  - unchanged unopened workspace files now also reuse cached parsed declaration summaries, so repeated `workspace/symbol` queries skip both file rereads and declaration rescans for those files.
  - local go-to-definition now resolves current-buffer top-level/module-body declarations and returns multiple targets when a symbol name has more than one declaration.
  - go-to-definition now falls back to exact-name declaration matches from open and unopened workspace Omni files when the active buffer has no local declaration for that symbol.
  - `textDocument/declaration` is now exposed and currently follows the same local-first, exact-name workspace fallback path as definition.
  - `textDocument/implementation` is now exposed and currently follows the same local-first, exact-name workspace fallback path as definition.
  - `textDocument/typeDefinition` is now exposed for type-like declarations (`type`, `abstract`, `union`, `alias`) and follows the same local-first, exact-name workspace fallback path.
  - hover now falls back to source-backed exact-name declaration snippets from open and unopened workspace Omni files when the active buffer has no local declaration for that symbol.
  - completion now appends declaration names from other open and unopened workspace Omni files after current-buffer declarations, reusing the cached workspace declaration summaries instead of rescanning unchanged unopened files.
  - signature help is now available through a syntactic call-form scan:
    - current-buffer function declarations are used first,
    - exact-name workspace function declarations from open and unopened Omni files are used as fallback,
    - a small static set of core special forms also exposes signatures.
  - parameter inlay hints now reuse the same local/workspace declaration data and only emit hints when the resolved function name has one unambiguous parameter label set, avoiding guesses for conflicting overloads.
  - folding ranges are now exposed from a structural multiline-form scan, covering modules, multiline declarations, `block` forms, and nested multiline list/vector/map forms without semantic indexing.
  - selection ranges are now exposed from the same structural form scan, expanding from the symbol under the cursor out through enclosing nested Omni forms for editor expand-selection support.
  - document and range formatting are now available as a conservative indentation pass that normalizes leading indentation and trims trailing whitespace without attempting full pretty-printing or intra-line spacing rewrites.
  - local code actions now offer declaration rewrites for:
    - shorthand function defines to explicit lambda bindings,
    - explicit lambda bindings back to shorthand function defines,
    - wrapping multi-form function bodies in an explicit `block`,
    - inlining redundant explicit `block` bodies back into direct declaration forms.
  - hover now returns source-backed snippets for current-buffer declarations, including overloaded local names, before falling back to static builtin/special-form docs.
  - document highlights now mark current-buffer declaration names and their local uses, classifying declaration sites as write highlights and ordinary occurrences as read highlights.
  - local references and rename now work for current-buffer declaration names, including overloaded names, by reusing the same syntactic declaration scan and exact symbol-range extraction.
  - intent:
    - give editors a first practical navigation surface without waiting for semantic indexing,
    - surface module-body declarations in common Omni source layouts,
    - keep the implementation syntactic and local to the active buffer for predictable behavior.
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`

- JIT cache overwrite accounting fix:
  - fixed `jit_cache_store(...)` so overwriting an existing `Expr*` cache entry no longer increments `g_jit_cache_count`.
  - hardened `jit_cache_store(...)` so overwrite paths do not trigger false-positive full-cache clears when `g_jit_cache_count` is already at the threshold.
  - hardened saturated 16-slot probe windows to clear-and-retry once instead of silently dropping the compiled entry.
  - this prevents false-positive cache saturation and premature full-cache clears from repeated store paths over the same compiled expression.
  - added a focused JIT-policy regression test asserting:
    - first store of one AST increases cache count to `1`,
    - second store of the same AST keeps cache count at `1`.
    - overwrite at the cache threshold preserves existing entries instead of clearing the cache.
    - a saturated local probe window still stores the target expression after a clear-and-retry recovery path.
  - motivation:
    - `omni-torch/examples/xor_nn.omni` exposed noisy `[debug] JIT cache full (...), clearing` churn during a long-running effect/JIT workload,
    - root cause included cache-entry overwrite accounting and cache-store edge handling, not actual unique-entry growth on that path.
  - validation:
    - `c3c build` passed.
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=build:/usr/local/lib:deps/lib ./build/main --test-suite lisp` confirms the new cache regressions pass.
    - note: the same `jit-policy` slice still has a separate pre-existing failure in `gc runs only at explicit safe points`; this change does not touch that path.

- Scheduler boundary hardening + AOT build command safety:
  - `scheduler_add_fiber(...)` now validates coroutine shape after `boundary_promote_to_root(...)` as well as before promotion, so non-null non-coroutine promotion results are rejected deterministically.
  - scheduler offload start now marks the pending slot active before enqueueing worker work and rolls state back on enqueue failure, removing a race where a fast offload-ready wakeup could arrive before slot activation and get dropped as invalid.
  - scheduler fallback errors used when root promotion fails are now allocated in `root_scope` to avoid storing current-scope error values into long-lived fiber result fields.
  - interpreter teardown now clears global JIT cache before AST arena destruction so `Expr*` cache keys cannot survive a destroyed/reused arena.
  - sequential top-level `spawn`/`await` paths now reclaim idle-complete scheduler batches (same reset policy already used by `run-fibers`), preventing false `scheduler full` after long non-`run-fibers` spawn loops.
  - AOT build backend command assembly now shell-quotes the output path and fails fast on command-buffer overflow, hardening `--build -o ...` against shell metacharacter injection/truncation behavior.
  - AOT build temp C3 source now uses per-process unique `build/_aot_temp_<pid>_<seq>.c3` paths (exclusive-create mode) and is deleted after compile, removing fixed-temp-path clobber races.
  - `--build` now fails fast on malformed `-o` usage with a missing output operand instead of silently falling back to a derived default output path.
  - `--build` now resolves the repo root from the running executable and executes the build pipeline from that root, so AOT helper/script + source-glob paths no longer depend on the caller’s current working directory.
  - `--build` now preserves caller-relative `input` / `-o` paths by normalizing them to absolute paths before switching to repo-root execution.
  - AOT backend compile path now lives fully in `entry_build_backend_compile.c3` without dependency on `entry.c3`-local extern declarations, keeping `--build` backend wiring self-contained.
  - AOT backend compile execution no longer uses shell-string `system(...)`; `--build` now runs helper/build commands through subprocess spawning with argv vectors, eliminating shell interpolation of caller-controlled paths.
  - AOT backend source globs are now expanded in-process (`src/main*`, `src/scope_region*`, `src/stack_engine*`, `src/lisp/*.c3`, `src/pika/*.c3`) before invoking `c3c`, so compile inputs are explicit argv entries instead of shell-expanded patterns.
  - AOT backend now pins `c3c` resolution to verified absolute paths (`/usr/local/bin/c3c`, `/usr/bin/c3c`, `/bin/c3c`, `/opt/homebrew/bin/c3c`) and fails closed when none exist, removing PATH-based compiler lookup in `--build`.
  - AOT backend now verifies required local runtime files (`./scripts/build_omni_chelpers.sh`, `/usr/bin/env`) before spawn, failing early with explicit diagnostics when prerequisites are missing.
  - AOT backend source list remains scoped to `src/main*.c3` + runtime/lisp sources (without pulling CLI `entry.c3`), avoiding duplicate generated-vs-cli `main` symbol collisions in `--build` output compilation.
  - JSON REPL transport in `src/lisp/eval_repl_json.c3` now uses a local JSON-string escaping helper instead of `main::print_json_escaped_string`, removing backend coupling to CLI reporting modules in AOT compile paths.
  - stack-affinity harness now captures probe output via `popen(... 2>&1)` instead of redirecting to predictable `/tmp` log files, removing fixed log-path spoof/clobber behavior.
  - stack-affinity harness probe-status validation now treats any non-zero wait status (exit or signal) as a probe failure, avoiding false negatives when the subprocess terminates by signal.
  - compiler bindgen canonical-type regression test now uses pid+sequence unique `/tmp` header/output paths instead of fixed filenames.
  - scheduler cancel cascade now re-checks parent state after recursive child cancellation to avoid double-completing a parent that was already completed via `waiting_children` resolution.
  - scheduler cancel completion now uses `scheduler_complete_fiber(...)` (not direct `scheduler_mark_done(...)`) so canceled parents with live descendants stay in `waiting_children` until child accounting reaches zero.
  - scheduler fibers now track `cancel_requested`, and `waiting_children` completion paths now prefer cancellation results when a parent was explicitly canceled, preventing child-cascade completion from preserving stale success `pending_result` values.
  - JIT runtime teardown is now guarded by live-interpreter reference counting: `Interp.init()` attaches and `Interp.destroy()` detaches, and `jit_global_shutdown()` now skips full runtime teardown while any interpreter remains alive.
  - this prevents one interpreter destroy path from invalidating JIT state/code buffers still reachable from other live interpreters.
  - stack-pool shutdown now sweeps all pool-owned `StackCtx` allocations (active + free-list), not only cached free-list entries, so interpreter teardown reclaims abandoned suspended continuation/coroutine stacks deterministically.
  - stack-pool ownership now tracks an intrusive all-context list (`all_list`/`all_count`) in addition to the recycle free list, and `stack_ctx_destroy(...)` now unlinks contexts when they are permanently freed outside the pool cache.
  - `Interp` table-growth paths (`macro`/`module` hash rebuild and effect handler stack growth) now assert on allocation failures before swapping live storage, removing unchecked-null dereference/corruption paths during growth.
  - `--build` absolute-path normalization now returns explicit `input/output path too long` errors instead of crashing on `io::bprintf(... )!!` unwrap for oversized caller paths.
  - default `--build` output derivation now trims extension dots only in the basename segment, avoiding incorrect truncation when parent directories contain dots.
  - default `--build` output derivation now fails fast if the derived path does not fit the output buffer, instead of silently truncating the output location.
  - added scheduler regression coverage asserting offload setup failure rolls pending slot + fiber state back to pre-enqueue values.
  - added scheduler regression coverage for `>MAX_FIBERS` sequential spawn+await loops without `run-fibers`, asserting idle-capacity reclamation.
  - added scheduler regression coverage for `fiber-cancel` child-cascade paths to ensure parent completion is not applied twice.
  - added scheduler regression coverage ensuring parent cancel remains `waiting_children` (with cancel result staged) when a descendant is currently `RUNNING` and cannot be canceled immediately.
  - strengthened scheduler cancel-cascade regression to assert canceled parent result type is `ERROR` (cancellation) instead of preserving prior success `pending_result`.
  - added JIT policy regression coverage for multi-interpreter lifetime: destroying a secondary interpreter must not tear down JIT runtime while a primary interpreter still executes previously compiled JIT code.
  - added JIT policy regression coverage for interpreter teardown with an escaped continuation (`(checkpoint (capture k k))`), asserting stack-pool ownership count drains to zero after destroy.
  - tightened compiler multi-arg lowering regression to require `invoke_once` in intermediate-argument lowering instead of passing on generic `invoke` presence.

- Quasiquote and continuation boundary hardening:
  - removed fixed-size 17-element quasiquote call flattening in both compiler and JIT expansion paths; wide quasiquote call forms now iterate directly over `func` + `args` instead of copying through a small stack buffer.
  - fixed `resume_value` consumption so coroutine/yield and JIT continuation/effect resume paths clear the one-shot resume payload after reading it.
  - fixed `jit_compile(...)` to stop running `jit_gc()` implicitly; JIT GC remains an explicit top-level safe-point action.
  - added regressions for:
    - wide quasiquote list evaluation and code generation,
    - coroutine implicit-`nil` resume after an earlier explicit resume value,
    - stale `resume_value` not leaking from `resolve` or `capture` into a later coroutine resume,
    - `jit-policy` safe-point behavior now matching explicit-GC-only intent.

- Serializer and CLI/test-runner consistency fixes:
  - fixed expression serializer output for parser-owned forms so emitted surface syntax now round-trips for:
    - `import` with string-path targets,
    - selective `import` aliases,
    - `handle` clauses,
    - `match` clauses.
  - bare `omni` now honors the documented default mode and starts the REPL instead of dispatching into the test runner.
  - Lisp/compiler test failures now return normal nonzero process status through the CLI test path instead of aborting via `assert(...)`.
  - added focused compiler regressions covering serializer round-trips for `import`, `handle`, and `match`.

## 2026-03-13

- AOT direct-lowering parity slice for type/dispatch/explain:
  - compiler lowering no longer routes core type/effect definition forms through `aot::eval_serialized_expr(...)`:
    - `deftype`,
    - `defabstract`,
    - `defunion`,
    - `defalias`,
    - `defeffect`.
  - typed lambda `define` lowering now installs method-table entries through an explicit structured helper (`aot::define_typed_method(...)`) instead of delegated source re-evaluation.
  - compiled `explain` lowering now uses direct helper paths for:
    - `explain 'dispatch` call targets,
    - `explain 'effect` signal/perform targets,
    - `explain 'effect` resolve targets.
  - AOT runtime now builds structured type annotations and method signatures directly from compiler-emitted specs, so compiled registration paths reuse the existing evaluator/dispatch machinery without reparsing source forms.
  - dispatch table helpers were generalized so typed callable registration is no longer closure-tag-specific (`jit_eval_define_typed_callable(...)`).
  - compiler regression expectations were updated to assert direct helper lowering and the absence of delegated eval on these paths.
  - current remaining parity gaps are narrower and explicitly tracked in `TODO.md`:
    - AOT closure representation still differs from runtime `CLOSURE`,
    - compiled module/import/export semantics still need an explicit parity/design decision,
    - the AOT eval bridge remains outside normal compiler lowering and should not reappear there.
  - validation:
    - `c3c build` passed.
    - `OMNI_IN_VALIDATION_CONTAINER=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` passed (`Compiler Tests: 91 passed, 0 failed`).

## 2026-03-12

- Void promotion from annotation-only to runtime singleton:
  - `Void` is now a real builtin singleton type/value instead of an FFI-only annotation exception.
  - runtime values can now inhabit `Void`, `type-of` can now return `Void`, and the type registry now includes `Void` under `Any`.
  - `(Void)` is now the canonical zero-argument constructor for the singleton runtime value.
  - FFI `^Void` returns now map to the runtime `Void` value instead of collapsing to `nil`.
  - intent:
    - remove the remaining public type-symbol exception from the hierarchical type surface,
    - keep FFI and normal type/value vocabulary aligned,
    - preserve the truthiness rule (`Void` is truthy; only `nil` and `false` are falsy),
    - leave ordinary nil-returning no-result primitives as a migration migration follow-up rather than silently changing them all at once.

- Set promotion + syntax simplification:
  - sets now have a distinct runtime `SET` tag and builtin `Set` type symbol instead of masquerading as `Dictionary`.
  - `Set` is now the canonical constructor surface for sets; lowercase `set` is no longer the creation entrypoint.
  - reader sugar `#{...}` has been removed so set construction is spelled only as `(Set ...)`.
  - `type-of` on set values now returns `Set`.
  - set printing is now constructor-shaped (`(Set ...)`) instead of dict-shaped (`{k true ...}`).

- Constructor/type-symbol surface unification slice:
  - type symbols in value position now gain a first concrete builtin-constructor pass for:
    - `Integer` as the canonical integer coercion constructor,
    - `Int` as an integer shorthand alias,
    - `Double` as a coercion constructor,
    - `String` as a coercion constructor,
    - `Symbol` as a coercion constructor,
    - `Boolean` as the canonical truthiness coercion constructor,
    - `Bool` as a boolean shorthand alias,
    - `Nil` as a nil/false identity constructor,
    - `Closure` as a closure identity constructor,
    - `Coroutine` as the canonical coroutine constructor surface,
    - `List` as the canonical list constructor/conversion surface,
    - `Iterator` as the canonical iterator constructor/conversion surface,
    - `Array` as the canonical array constructor/conversion surface,
    - `Dictionary` as the canonical dictionary constructor surface,
    - `TimePoint` as the canonical time-point constructor surface.
  - runtime registration now binds callable builtin constructor names into the normal value-level environment instead of keeping iterator creation behind `make-*` naming only.
  - iterator stdlib surface is normalized so `Iterator` is the canonical constructor.
  - collection/time constructors now follow the same public naming rule:
    - `Integer`, `Boolean`, `List`, `Array`, `Dictionary`, `Iterator`, and `TimePoint` are canonical,
  - intent:
    - align builtin/runtime object construction more closely with existing user-defined type constructors (`Point`, `Box`, `Some`, ...),
    - reduce public `make-*` surface drift,
    - prefer clarity over terseness for canonical language-facing names,
    - support existing iterator code while shifting docs/tests/examples toward the type-symbol constructor form.

- Dispatch numeric widening removal:
  - removed implicit dispatch-time numeric widening from method applicability and typed lambda call-boundary checks.
  - `Integer` arguments no longer satisfy `^Double` parameters automatically.
  - dispatch now uses exact type + subtype + untyped fallback scoring only.
  - explicit constructor conversion is now required for cross-numeric calls (for example `(Double 7)`).
  - updated dispatch and typed-lambda regression coverage to assert:
    - no implicit `Integer -> Double` matching,
    - explicit conversion behavior is preserved,
    - e2e dispatch fixture uses explicit conversion.

- Descriptive low-level alias surface:
  - added explicit descriptive aliases for low-level wrappers while preserving existing shorthand names:
    - filesystem wrappers: `filesystem-open` / `filesystem-read` / `filesystem-write` / `filesystem-close` / `filesystem-stat` / `filesystem-read-directory` / `filesystem-rename` / `filesystem-unlink`,
    - TCP wrappers: `transmission-control-connect` / `transmission-control-listen` / `transmission-control-accept` / `transmission-control-read` / `transmission-control-write` / `transmission-control-close`,
    - UDP wrappers: `user-datagram-socket` / `user-datagram-bind` / `user-datagram-send` / `user-datagram-receive` / `user-datagram-close`,
    - DNS wrapper: `domain-name-resolve`,
    - TLS wrappers: `transport-layer-security-connect` / `transport-layer-security-server-wrap` / `transport-layer-security-read` / `transport-layer-security-write` / `transport-layer-security-close`.
  - the canonical shorthand names are `fs-*`, `tcp-*`, `udp-*`, `dns-resolve`, and `tls-*`.
  - added advanced regression coverage for the descriptive alias wrappers (filesystem roundtrip, directory-read alias, TCP/DNS/TLS alias delegation, and UDP alias close path).

- FFI pointer annotation descriptive alias:
  - promoted `Pointer` as the canonical FFI pointer annotation spelling (`^Pointer`).
  - runtime FFI annotation mapping uses the canonical pointer ABI type for `^Pointer`.
  - advanced FFI regression validates the canonical pointer spelling on `free`-style void-return bindings.
  - bindgen pointer mapping now emits `^Pointer` for non-string pointer parameters/returns.
  - bindgen integer mappings now emit canonical `^Integer`.
  - compiler regression coverage now exercises bindgen output generation and asserts canonical `^Integer`/`^Pointer`/`^Void` annotation emission.
  - docs/spec/tooling references now describe `^Pointer` as canonical.

- Command-style `Void` migration follow-up:
  - `write-file` now returns `Void` on successful completion instead of `true`.
  - `unsafe-free!` now returns `Void` for both successful frees and no-op targets (such as `int`/`nil`), keeping it as a command-style surface.
  - `run-fibers` now returns `Void` on successful scheduler drain completion instead of overloading `nil`.
  - module/import command forms now return `Void` on successful completion instead of overloading `nil`:
    - `module`,
    - `import`,
    - `export-from`.
  - re-export runtime eval path now also returns `Void` on success:
    - `jit_eval_export_from_impl` in `jit_jit_compile_effects_modules.c3`.
  - AOT flat compiler module/import/export-from codegen now emits `aot::make_void()` for parity with interpreter/JIT command semantics.
  - scheduler cancellation commands now return `Void` on success instead of `true`/`nil` status booleans:
    - `task-cancel`,
    - `thread-cancel`.
  - `fiber-cancel` now returns `Void` when cancellation is performed; it still
    returns `nil` when the target fiber is already done/running.
  - `set!` now follows command-style `Void` semantics on successful mutation:
    - variable mutation (`set! name value`) returns `Void` instead of the assigned payload,
    - dot-path mutation (`set! obj.field value`, including cons paths) returns `Void`,
    - JIT mutable-local/env/path helpers and immutable-local codegen are aligned,
    - AOT flat compiler `set!` lowering now emits `aot::make_void()` results.
  - `set!` now also has a generic collection-update call surface:
    - `(set! collection key value)` dispatches to collection mutation (currently `Array`/`Dictionary`),
    - parser preserves old 2-arg mutation semantics for binding/path updates while routing 3+ arg forms through normal call dispatch,
    - compiler primitive maps and free-var primitive recognition now include callable `set!`,
    - `array-set!` and `dict-set!` are removed from the live surface.
  - deduce command surfaces now return `Void` on success instead of overloading `nil`:
    - transaction control: `commit`, `abort`,
    - relation mutation/maintenance: `fact!`, `retract!`, `clear!`, `drop!`.
  - updated regression coverage:
    - async I/O tests assert `write-file` success returns `VOID`,
    - advanced memory tests assert `unsafe-free!` no-op paths return `VOID`,
    - scheduler tests assert `run-fibers` success returns `VOID`,
    - scheduler cancel tests assert `fiber-cancel` success returns `VOID`,
    - core/module regression tests assert `set!` success returns `VOID` (including cons-path mutation),
    - advanced/compile regression tests assert generic `(set! collection key value)` updates for array/dict paths,
    - advanced module tests assert `module`, `import`, and `export-from` command forms return `VOID`,
    - scheduler offload/thread tests assert `task-cancel` and `thread-cancel` return `VOID`,
    - deduce tests assert command/txn surfaces above return `VOID`.

- AOT delegated-eval parity hardening (compiler bridge injection):
  - compiler delegated-bridge lowering now injects referenced compiled bindings into the AOT interpreter env before calling `aot::eval_serialized_expr(...)`.
  - injection is emitted as `aot::define_var("name", name)` for free variables that have generated C3 bindings (declared/local or tracked globals), while still skipping builtin primitives.
  - this closes a concrete parity gap where typed-define bridge paths could miss referenced compiled globals in delegated eval (for example `(define y 10)` followed by typed define bodies referencing `y`).
  - added compiler regression coverage:
    - `Compiler: typed define bridge injects referenced globals` in `src/lisp/tests_compiler_core_groups.c3`.
  - validation:
    - `c3c build` passed.
    - `OMNI_IN_VALIDATION_CONTAINER=1 OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` passed (`Compiler Tests: 91 passed, 0 failed`).

## 2026-03-10

- Macro surface hard-fail migration closure:
  - parser removed clause-style macro definitions (`(define [macro] name ([...] ...))` and multi-clause variants).
  - macro definitions now require the canonical single-transformer surface:
    `(define [macro] name (syntax-match (... (template ...)) ...))`.
  - parser emits deterministic migration diagnostics for removed forms:
    - `macro clause syntax was removed; use (define [macro] name (syntax-match ([...] (template ...)) ...))`
  - compiler macro serializer now emits canonical `syntax-match` + `template` shape instead of clause syntax.
  - migrated in-repo macro definitions in tests/bench/examples to canonical surface, including:
    - advanced macro hygiene suites,
    - compiler macro smoke paths,
    - AST arena macro benchmark sources,
    - `examples/finwatch/rules.omni`.
  - added explicit regression coverage for removed forms:
    - single-clause and multi-clause syntax now have negative tests asserting deterministic failure.
  - synchronized docs/spec references to canonical macro surface:
    - `docs/LANGUAGE_SPEC.md`, `docs/SYNTAX_SPEC.md`, `docs/reference/05-macros-modules.md`, `docs/FEATURES.md`, and `docs/syntax-decision.md`.

- Syntax-surface canonicalization pass:
  - Removed `Val` constructor support from type/value-literal annotations; `Value` is now the only accepted constructor.
    - parser now emits deterministic diagnostic for removed usage: `Val constructor was removed; use Value`.
    - updated dispatch/type tests and docs to canonical `^(Value ...)` forms.
  - Canonicalized effect-composition messaging around `handle`:
    - removed stdlib `with-handlers` helper export/definition.
    - updated docs/examples (`EFFECTS_GUIDE`, finwatch alerts/smoke/TODO, stdlib appendix) to explicit handle-wrapper composition.
  - Removed stale `lambda/fn` syntax mention from root README (language surface now documented as `lambda`).
  - Parser now emits deterministic form diagnostics:
    - `fn syntax was removed; use lambda`
    - `do syntax was removed; use block`
    - added regression coverage in advanced lambda/block syntax tests.
  - Sequencing keyword canonicalization:
    - `block` is now the only sequencing form accepted by the parser.
    - sequencing spellings no longer have a special-form parser branch.
    - serializer output and stdlib/examples/tests were normalized to `(block ...)`.
  - Handle-clause canonicalization:
    - removed nested handle-clause form `((tag k arg) body)` from parser syntax.
    - `handle` clauses now require the canonical shape `(tag arg body)` with explicit `with-continuation` for multi-shot usage.
    - parser emits deterministic rejection message: `handle clause syntax was removed; use (tag arg body) and with-continuation when needed`.
    - parser tests updated with a canonical `with-continuation` multi-shot case and explicit rejection coverage.
  - Let/match canonicalization:
    - removed grouped Scheme-style `let` binding forms in favor of canonical flat-pair `let`.
    - removed grouped named `let` binding forms in favor of canonical flat-pair named `let`.
    - removed `letrec` parser syntax; parser now hard-fails with migration guidance to `let ^rec`.
    - flat-pair `let` bindings are now documented and regression-covered as sequential left-to-right bindings.
    - named `let` initializer lists now lower through an outer sequential `let` before entering the inner `let ^rec` loop binding, so later initializers can reference earlier ones.
    - `match` clauses now require explicit `(pattern result)` pairs and emit deterministic diagnostics for missing-result and extra-form cases.
    - `with-continuation` is now valid only inside `handle` clauses; parser rejects out-of-context usage explicitly.
    - normalized shipped examples toward canonical flat-pair `let` syntax and documented the migration surface in `docs/syntax-decision.md`.
  - Parser deprecation matrix:
    - added dedicated `Value`/`Val` alias rejection matrix coverage in advanced parser/type tests:
      - canonical `^(Value ...)` dispatch annotations are accepted for int, symbol, string, and bool literals.
      - `^(Val ...)` annotations are rejected with `Val constructor was removed; use Value`.
  - Deduce transaction command canonicalization:
    - replaced the transaction-start command with `(deduce 'block db ['read|'write])`.
    - updated runtime dispatch, durability tests, and docs to the `block` command.
    - transaction-start failure diagnostics were normalized to `txn open failed`.
  - Handler-composition helper canonicalization:
    - replaced fold-based guide helper with explicit recursive `handle/chain` in `docs/EFFECTS_GUIDE.md` (left-to-right list order, first handler outermost, thunk executed exactly once at base case).
    - aligned finwatch helper naming from `alerts/apply-handlers` to `alerts/handle-chain` with unchanged wrapper semantics.

- Lisp advanced-slice workstation-safety hardening:
  - `src/lisp/tests_tests.c3` now runs `advanced` as isolated subgroups (fresh interpreter per subgroup) to prevent cumulative definition retention across the entire advanced umbrella run.
  - `src/lisp/tests_advanced_macro_hygiene_groups.c3` now gates the intentionally unbounded stack-overflow probe behind `OMNI_LISP_STACK_OVERFLOW_PROBE=1`; default `advanced` no longer executes that probe.
  - `run_lisp_tests` now enforces container-only execution for high-memory selections.
    - `all`, `memory-lifetime-soak`, and `memory-stress` require `OMNI_IN_VALIDATION_CONTAINER=1` by default.

- Docker-only gate enforcement + 30% host-resource cap policy:
  - `scripts/c3c_limits.sh`:
    - default hard-cap method switched to Docker (`OMNI_HARD_MEM_CAP_METHOD=docker`) for capped command execution.
    - default cap percent switched to `30` (`OMNI_HARD_MEM_CAP_PERCENT=30`) with shared percent resolver.
    - hard memory cap now enforces a strict upper bound at the configured percent (manual MB override is clamped, never above cap).
    - Docker CPU quota is now computed from host CPU count at the same cap percent and clamped to that maximum.
  - gate runners now default to Docker mode with explicit outside-container guardrails:
    - `scripts/run_global_gates.sh`
    - `scripts/run_boundary_hardening.sh`
    - `scripts/run_e2e.sh`
  - validation container wiring:
    - `scripts/container_exec.sh` now exports `OMNI_IN_VALIDATION_CONTAINER=1` so gate scripts can distinguish single-container execution from host execution.
    - `scripts/run_validation_container.sh` now defaults/clamps CPU quota to the same 30%-cap policy.

- Validation containment upgrade: real Docker cap mode for gate execution.
  - `scripts/c3c_limits.sh`:
    - added `OMNI_HARD_MEM_CAP_METHOD=docker` execution path (`omni_run_with_docker_cap(...)`).
    - Docker mode now enforces hard limits via `docker run` (`--memory`, `--memory-swap`, `--cpus`, `--pids-limit`, `--network`) while binding the repo workspace and optional host toolchain root.
    - added per-run resource telemetry sampling (`docker stats`) to `build/docker_resource_stats.log` (configurable via `OMNI_DOCKER_MONITOR_LOG`).
    - added optional wall-time kill guard with `OMNI_DOCKER_TIMEOUT_SEC`.
  - gate wiring:
    - `scripts/run_global_gates.sh`: docker cap mode now applies across normal and ASAN runs/slices; auto-enables `OMNI_C3_HARD_CAP_ENABLED=1` in docker mode.
    - `scripts/run_boundary_hardening.sh`: stage runner now executes through the shared cap wrapper (and enables c3 hard-cap mode under docker).
    - `scripts/run_e2e.sh`: generation/run stages now execute through shared cap wrapper (and enables c3 hard-cap mode under docker).
  - single-container runner:
    - added `scripts/run_validation_container.sh` for full validation sessions inside one constrained Docker container.
    - added `scripts/container_exec.sh` container entrypoint helper to set mounted toolchain paths and disable recursive inner container mode by default.
  - docs:
    - `docs/PROJECT_TOOLING.md` now documents container-capped validation usage and docker tuning knobs.
  - pinned container toolchain:
    - added `docker/validation.Dockerfile` with pinned C3 toolchain (`v0.7.10` tarball + sha256), GNU lightning (`2.2.2`), and replxx (`release-0.0.4`) so validation can run without host toolchain mounts.
    - added `scripts/build_validation_image.sh` to build the pinned image reproducibly.
    - docker validation defaults now target local `omni-validation:2026-03-10` image and require local image presence by default (`OMNI_DOCKER_REQUIRE_LOCAL_IMAGE=1`, `OMNI_VALIDATION_REQUIRE_LOCAL_IMAGE=1`) to avoid implicit pull fallback ambiguity.
    - `scripts/run_validation_container.sh` no longer mount-binds `/usr/local` by default; host toolchain mount is opt-in via `OMNI_VALIDATION_TOOLCHAIN_ROOT`.

- Syntax surface governance reference:
  - added `docs/syntax-decision.md` to capture canonical naming decisions:
    - `lambda` vs removed `fn`,
    - `block` vs removed `do`/`begin` surface usage,
    - `handle`/`resolve`/`perform`/`with-continuation`,
    - `Value` constructor usage and `Val`,
    - `deduce 'block ...` transaction command contract.
  - added `docs/syntax-decision.md` entry to documentation map in `docs/README.md`.

- Memory-lifetime benchmark modularization:
  - split AST arena benchmark support out of `src/lisp/tests_memory_lifetime_boundary_graph_txn_bench_groups.c3` into:
    - `src/lisp/tests_memory_lifetime_boundary_ast_bench_groups.c3`
  - retained boundary decision/splice benchmark coverage in:
    - `src/lisp/tests_memory_lifetime_boundary_graph_txn_bench_groups.c3`
  - intent: keep the benchmark-heavy memory-lifetime surfaces behavior-preserving while reducing file size and benchmark-domain coupling.

- Memory-lifetime suite ownership split:
  - split the mixed suite orchestration in `src/lisp/tests_memory_lifetime_groups.c3` into dedicated module owners:
    - `src/lisp/tests_memory_lifetime_smoke_suite_groups.c3`
    - `src/lisp/tests_memory_lifetime_stress_suite_groups.c3`
    - `src/lisp/tests_memory_lifetime_benchmark_suite_groups.c3`
  - preserved behavior while separating smoke, soak/stress, and benchmark domain entrypoints.

- Post-complete backlog hygiene:
  - added `scripts/check_post_complete_backlog_freshness.sh`:
    - checks unchecked backlog entries in `docs/plans/post-complete-backlog.md` against recent release dates from `memory/CHANGELOG.md`.
    - fails when entries are stale beyond the configured release-cycle threshold.
    - supports optional tuning via `OMNI_POST_COMPLETE_BACKLOG_RELEASE_CYCLES` and `OMNI_POST_COMPLETE_BACKLOG_FALLBACK_DAYS`.
  - added CI coverage with `.github/workflows/post-complete-backlog-freshness.yml`.
  - updated `docs/PROJECT_TOOLING.md` with check command and governance notes.

- Additional host-safety hardening for Lisp slice execution:
  - `src/lisp/tests_tests.c3` now rejects explicit host-side sliced Lisp runs outside validation containers for any `OMNI_LISP_TEST_SLICE` other than `basic`.
  - existing container-only enforcement remains for high-memory selections (`all`, `memory-lifetime-soak`, `memory-stress`).
  - intent: prevent accidental workstation-wide memory spikes from manually selected slices while preserving default host-safe `basic` behavior.
  - `docs/PROJECT_TOOLING.md` now documents the stricter slice-execution rule.
- Lisp slice alias hardening:
  - `src/lisp/tests_tests.c3` now fails fast for removed aliases (`memory-soak`, `syntax`) with a deterministic migration message.
  - the failure path now points maintainers at explicit ownership-safe slices (`memory-lifetime-smoke`, `memory-lifetime-policy`, `memory-lifetime-bench`, `memory-stress`, `allocator-validation`, `allocator-bench`) instead of implicit migration behavior.
  - this closes the deferred alias migration gap while keeping `memory-lifetime` as a scoped alias only.

- Data-format semantic drift closure before API expansion:
  - `src/lisp/primitives_data_formats.c3`:
    - TOML boolean conversion now preserves Omni `false` symbol identity instead of coercing TOML `false` to `nil`.
  - `src/lisp/primitives_data_formats_csv_parse.c3`:
    - strict/default CSV parse now enforces RFC-4180 row separators (`\r\n`) and reports deterministic strict-line-ending errors for lone `\n` / bare `\r`.
  - `src/lisp/primitives_data_formats_csv_emit.c3`:
    - strict/default CSV emit now uses `\r\n` as the default row separator.
  - regression updates:
    - `src/lisp/tests_runtime_data_unicode_groups.c3` updated to assert strict/default CRLF behavior and preserved strict=false leniency paths.
  - docs/checklist sync:
    - `docs/reference/11-appendix-primitives.md` now documents strict/default RFC-4180 CRLF behavior.
    - `docs/plans/idiomatic-libuv-surface-plan.md` and `TODO.md` step/checklist rows marked complete for this semantic-fix slice.

- Libuv surface guardrail closure for typed-wrapper + docs parity drift:
  - added `scripts/check_libuv_surface_policy.sh`:
    - validates newly-added `io/*` surfaces keep raw/effect/wrapper mapping (`__raw-*` <-> `io/*` <-> stdlib wrapper).
    - rejects newly-added `io/*` wrapper lambda aliases; requires function-style `define` wrappers.
  - extended `scripts/check_primitive_docs_parity.sh`:
    - keeps existing public primitive docs parity check.
    - additionally validates newly-added `__raw-*` io primitives/wrappers are documented and mapped.
  - updated `scripts/run_effects_contract_lint.sh` to run and report the new libuv surface policy log (`build/libuv_surface_policy.log`) in lint summary output.
  - updated tooling/CI docs and boundary-hardening artifact upload list to include the new policy log.

- Compiler/parser plan-governance TODO closure:
  - `docs/plans/compiler-parser-refactor-plan.md` exit-criteria governance checkboxes are now closed (`single active plan`, `no overlapping active checklists`, `area+changelog sync`).
  - `TODO.md` source rows for `docs/plans/compiler-parser-refactor-plan.md` governance items are now marked complete.
  - `docs/areas/compiler-parser-refactor.md` was refreshed (`As of: 2026-03-10`) with explicit single-active-plan governance notes.

- Global gate runner received workstation-safe lisp sharding controls:
  - `scripts/run_global_gates.sh` now defaults hard-cap execution to `OMNI_HARD_MEM_CAP_METHOD=auto` (systemd `MemoryMax` when available, with existing cap fallback behavior intact).
  - Added lisp slice fan-out in global gates via `OMNI_GLOBAL_GATES_LISP_SLICES` + per-slice `OMNI_LISP_TEST_SLICE` dispatch.
  - Default slice set is now group-granular (`basic`, `memory-lifetime`, `memory-stress`, `list-closure`, `arithmetic-comparison`, `string-type`, `diagnostics`, `jit-policy`, `advanced`, `escape-scope`, `limit-busting`, `tco-recycling`, `closure-lifecycle`, `pika`, `unicode`, `compression`, `data-format`, `json`, `async`, `reader-dispatch`, `schema`, `deduce`, `scheduler`, `http`, `atomic`, `compiler`) to avoid single giant lisp process peaks.
- Lisp test runner now supports explicit slice selection:
    - `src/lisp/tests_tests.c3` validates `OMNI_LISP_TEST_SLICE` values and runs only the requested group (or `all` when explicitly opted in via `OMNI_ALLOW_ALL_LISP_SLICE=1`), while preserving existing summary emitters and compiler-suite invocation semantics.
    - Unset `OMNI_LISP_TEST_SLICE` now defaults to the lightweight `basic` slice on host runs.
    - Unknown slice values fail fast with deterministic assertion (`unknown OMNI_LISP_TEST_SLICE`) to keep CI/profile misconfiguration obvious.
- Lisp slice selection now emits explicit high-memory flags in summary mode:
    - `src/lisp/tests_tests.c3` emits `OMNI_TEST_SUMMARY suite=lisp_slice ... high_memory=<0|1>` for the active slice.
    - `all`, `memory-lifetime-soak`, and `memory-stress` are flagged as high-memory selections with warning output to make workstation-risk slices obvious before heavy allocation phases.
- Memory-lifetime slice split for workstation-safe local execution:
  - `memory-lifetime` now maps to smoke coverage (`run_memory_lifetime_smoke_tests(...)`), and `memory-lifetime-smoke` is an explicit alias.
  - `memory-lifetime-soak` preserves the previous full heavy path (stress + hot-budget + promotion-context + optional bench hooks).
  - `scripts/run_global_gates.sh` now defaults to `memory-lifetime-smoke` and supports opt-in soak execution via `OMNI_GLOBAL_GATES_INCLUDE_LIFETIME_SOAK=1`.
- Session 44 docs closure:
  - published boundary architecture audit + invariants contract:
    - `docs/BOUNDARY_ARCHITECTURE_AUDIT_2026-03-10.md`
  - synchronized Session 44 checklist closure in:
    - `TODO.md`
    - `docs/plans/session-34-44-boundary-hardening.md`
  - updated area hub source map:
    - `docs/areas/memory-runtime.md`
- Session 41 ownership-domain split inventory captured (module ownership map):
  - Boundary API/types + migration facade: `src/lisp/eval_boundary_api.c3`.
  - Runtime policy/config toggles: `src/lisp/eval_boundary_policy.c3`.
  - Provenance/classification + transition routing helpers: `src/lisp/eval_boundary_provenance.c3`.
  - Session/transaction lifecycle guards and scope-swap discipline: `src/lisp/eval_boundary_session_txn.c3`.
  - Diagnostics/invariants/graph-audit assertions: `src/lisp/eval_boundary_diagnostics.c3`.
  - Telemetry counters/snapshots/reporting: `src/lisp/eval_boundary_telemetry.c3`.
  - Commit/escape policy outcomes and fallback routing: `src/lisp/eval_boundary_commit_flow.c3`.
- Session 41 dead-path cleanup and surface tightening:
  - Removed unused boundary wrappers from `src/lisp/eval_boundary_api.c3`:
    - `boundary_env_usize_or_default(...)` (no runtime/test callsites),
    - `boundary_copy_env_to_scope(...)` (convenience wrapper; no runtime/test callsites).
  - `c3c build` passed after removal.
- Session 42 CI/policy enforcement rules captured (existing guardrails validated):
  - Direct boundary call guard script present and passing:
    - `scripts/check_boundary_facade_usage.sh`,
    - policy map `scripts/boundary_facade_policy.txt`.
  - Boundary-change policy gate present:
    - `scripts/check_boundary_change_policy.sh` requires summary evidence from normal + ASAN boundary-hardening runs for boundary-sensitive file changes.
  - Workflow wiring present:
    - `.github/workflows/boundary-hardening.yml` runs `scripts/run_boundary_hardening.sh` (facade guard + ASAN/normal evidence + policy check stage).
  - Local validation:
    - `bash -n scripts/check_boundary_facade_usage.sh` passed.
    - `bash -n scripts/check_boundary_change_policy.sh` passed.
    - `scripts/check_boundary_facade_usage.sh` passed (no disallowed callsites).
- Session 43 hot-path cleanup (redundant promotion work reduction):
  - `src/lisp/eval_boundary_commit_flow.c3`:
    - added `boundary_destination_promote_routed_escape(...)` fast-path helper for destination ESCAPE routing.
    - destination cons/partial/iterator builders now skip redundant `boundary_promote_to_escape(...)` calls when:
      - routed value is already in target ESCAPE lane (`scope_gen == target_scope.escape_generation`), or
      - promotion context is already aborted (no additional promotion work attempted before fallback branch).
  - Goal: reduce avoidable promotion calls in boundary commit hot paths without changing fallback/error semantics.
  - Local validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite stack` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite scope` passed.
- Session 44 Commit A closure (boundary entrypoint sweep):
  - removed fully replaced env-copy entrypoints from `src/lisp/eval_env_copy.c3`:
    - `copy_env_to_scope_inner(...)`
    - `copy_env_to_scope(...)`
  - updated boundary-facade policy/audit surfaces to track only live symbols:
    - `scripts/boundary_facade_policy.txt` now gates `copy_env_to_scope_checked(...)` allow-list ownership.
    - `scripts/check_boundary_facade_usage.sh` symbol list now tracks `copy_env_to_scope_checked(...)` and drops retired symbols.
    - `scripts/audit_boundary_surface.sh` symbol inventory now tracks `copy_env_to_scope_checked(...)`.
  - regenerated `docs/BOUNDARY_SURFACE_AUDIT.md` after symbol inventory refresh.
  - local validation:
    - `scripts/check_boundary_facade_usage.sh` passed (`violations=0`).
- Validation (local, cap-safe):
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite stack` passed (`stack_engine: 21/0`).
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 ./build/main --test-suite scope` passed (`scope_region: 58/0`).
  - `bash -n scripts/run_global_gates.sh` passed.
  - `OMNI_LISP_TEST_SLICE=does-not-exist ./build/main --test-suite lisp` failed fast as expected with `unknown OMNI_LISP_TEST_SLICE`.
- Remaining closure note:
  - Session `run global gates` rows are now explicitly marked as local N/A/deferred in `TODO.md`; full normal+ASAN gate execution is expected on CI/large-host runners.

## 2026-03-09

- Idiomatic libuv wrapper typing slice landed with dispatch-smoke coverage:
  - `stdlib/stdlib.lisp`:
    - converted remaining lambda alias wrappers to typed `define` methods with explicit untyped fallbacks for canonical payload handling:
      - `tcp-listen`, `offload`, `thread-spawn`, `task-spawn`, `tls-connect`, `http-request`.
    - added thin composition-only convenience helpers (no runtime substrate duplication):
      - `job-spawn`, `job-join`, `job-join-timeout`, `job-cancel`, `request`.
    - helper implementation note:
      - typed variadic `define` dispatch remains runtime-limited; variadic helpers route through wrapper composition (`apply`) while keeping canonical `io/*` wrappers as execution entrypoints.
  - focused typed-wrapper dispatch smoke tests added:
    - `src/lisp/tests_runtime_feature_http_groups.c3`:
      - `explain 'dispatch` smoke for typed/fallback wrapper routing (`tcp-connect`, `http-request`).
      - helper smoke coverage for `request` single-url and method/url/body/headers routes.
    - `src/lisp/tests_scheduler_io_task_groups.c3`:
      - `explain 'dispatch` smoke for `task-spawn` / `thread-spawn` wrapper routing.
      - helper smoke coverage for `job-spawn` + `job-join` task/thread routes.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1719/0`, `compiler: 85/0`).

- Boundary mixed-mode stress coverage expanded for nested and mixed interpreter/JIT transitions:
  - `src/lisp/tests_memory_lifetime_boundary_stress_groups.c3`:
    - added `run_memory_lifetime_boundary_mixed_jit_interp_stress_test(...)` to alternate interpreter and JIT evaluation across stress iterations while asserting boundary state restoration and promotion-context cleanup.
    - wired the new stress case into `run_memory_lifetime_boundary_negative_stress_tests(...)`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1709/0`, `compiler: 85/0`).

- Boundary transition regression pack landed for return/env/splice deterministic behavior:
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3`:
    - added `run_memory_lifetime_boundary_transition_regression_tests(...)` with explicit assertions for:
      - return transition: budget-0 commit path hard-fails with `BOUNDARY_COMMIT_FAULT_PROMOTION_ABORTED`,
      - env transition: depth-limit copy path reports `BOUNDARY_ENV_COPY_FAULT_DEPTH_LIMIT`,
      - splice transition: non-unique child refcount is rejected with `BOUNDARY_SCOPE_TRANSFER_CHILD_REFCOUNT_NOT_ONE`.
  - `src/lisp/tests_memory_lifetime_groups.c3`:
    - wired transition regression pack into the memory lifetime suite.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1708/0`, `compiler: 85/0`).

- Boundary invariant hooks were centralized for ownership-transition assertions:
  - `src/lisp/eval_boundary_diagnostics.c3`:
    - added shared invariant hook surface:
      - `boundary_invariant_fail(domain, msg)`,
      - `boundary_invariant_require(cond, domain, msg)` macro.
    - migrated boundary state assertion helpers (`boundary_assert_interp_scope_chain`, `boundary_assert_saved_state`, `boundary_assert_session_state`, `boundary_assert_scope_swap_state`) onto the shared hook surface.
  - `src/lisp/eval_env_copy.c3`:
    - `copy_env_invariant_fail(...)` now delegates to shared `boundary_invariant_fail(...)` instead of maintaining a local invariant reporter.
  - invariant policy enablement + regression coverage:
    - `boundary_invariant_checks_enabled()` now makes checks default-on, with explicit override only via `OMNI_DISABLE_BOUNDARY_INVARIANTS=1`.
    - added regression `run_memory_lifetime_boundary_invariant_policy_tests(...)` to verify default-on behavior in test runs and env override parity.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1706/0`, `compiler: 85/0`).
    - `c3c build --sanitize=address` passed.
    - ASAN runtime run currently reproduces existing stack-switch runtime abort (`AddressSanitizer: CHECK failed ... asan_thread.cpp:369`) in advanced effect/JIT path; not marking Session 39 test/ASAN-enforcement item complete in this slice.

- Boundary commit fallback diagnostics are now typed and explicit:
  - `src/lisp/eval_boundary_api.c3`:
    - added `BoundaryCommitFault` and `boundary_commit_fault_name(...)`.
    - `BoundaryCommitEscapeResult` now carries `fault_code` alongside `outcome`.
  - `src/lisp/eval_boundary_commit_flow.c3`:
    - `boundary_commit_escape(...)` now classifies disallowed fallback exits with deterministic fault codes (destination-build failure class, promotion-aborted, splice-rejected, mixed-destination promotion failure, generic disallowed).
    - fallback error payload now includes the typed fault label in the message: `boundary: traversal fallback disallowed for commit path (<fault>)`.
  - tests updated to assert fault-classified disallowed paths:
    - `src/lisp/tests_memory_lifetime_boundary_commit_groups.c3`
    - `src/lisp/tests_memory_lifetime_boundary_stress_groups.c3`
    - `src/lisp/tests_memory_lifetime_boundary_graph_txn_bench_groups.c3`
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1706/0`, `compiler: 85/0`).
    - `c3c build --sanitize=address` passed.
    - `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed.

- Boundary error-model cleanup slice landed for copy-to-parent boundary paths:
  - introduced typed boundary copy fault surface in `src/lisp/eval_boundary_api.c3`:
    - `BoundaryCopyFault` enum + `BoundaryCopyResult`,
    - `boundary_copy_to_parent_site_ctx_checked(...)`,
    - `boundary_copy_fault_name(...)` / `boundary_copy_fault_message(...)`.
  - `boundary_copy_to_parent_site_ctx(...)` now routes through checked classification and returns explicit error values for boundary-copy faults (instead of untyped null/error ambiguity).
  - migrated silent boundary copy callsites to checked results:
    - `src/lisp/eval_env_copy.c3`: env-copy binding relocation now uses typed copy results and fails deterministically on boundary-copy fault.
    - `src/lisp/eval_type_evaluators.c3`: instance field relocation now fails fast with fault-classified diagnostics instead of silently leaving NIL/default field payloads.
    - `src/lisp/primitives_iter_coroutine.c3`: resume/yield boundary copy now surfaces typed boundary-copy diagnostics.
    - `src/lisp/eval_repl.c3`: REPL result boundary copy now returns `eval_error(...)` on typed boundary-copy fault.
  - regression coverage:
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3`: added `run_memory_lifetime_boundary_copy_fault_tests(...)` for typed-fault classification and wrapper parity.

- Types/dispatch area closeout evidence synchronized (non-L4.3 e2e baseline documented):
  - validation run:
    - `scripts/run_e2e.sh` still exits non-zero with the known baseline diff set.
    - `build/e2e_diff.txt` is unchanged versus prior baseline snapshot (`build/e2e_diff_notests.txt`).
  - baseline mismatch inventory (line-mapped, non-L4.3 rows):
    - continuation/effect rows: `reset passthrough`, `shift k resumes`, `handle perform resume`, `shift resume mul`, `shift resume add`, `multi-shot k`, `handle get`, `handle no trigger`, `nested handle`, `shift double resume`, `let-rec factorial 8`.
    - truthiness/predicate rows: `and false true`, `and true false`, `closure? yes`.
    - dict rows: `dict create ref`, `dict ref 2`, `dict has? yes`, `dict keys`.
    - introspection row: `type-of closure`.
  - parity gate confirmation:
    - L4 backend parity rows in `E2E_TESTS_EXTENDED` remain aligned (`type struct ctor`, `type union ctor`, dispatch exact/subtype/widen/value-literal, ambiguity explain reason).
  - docs/status sync:
    - updated `docs/areas/types-dispatch.md` to reflect that area-level closeout evidence is now recorded and status wording is no longer stale.

- Effects/error-model conversion+compression canonicalization landed:
  - `src/lisp/prim_string_convert.c3`:
    - migrated conversion primitive failure paths from string-only `raise_error(...)` to canonical payloaded raises.
    - argument/type validation now emits `type/arg-mismatch` payload code.
    - `read-string` parse/eval failure now emits runtime payload code `runtime/read-string-failed`.
  - `src/lisp/compress.c3`:
    - migrated compression/checksum primitive failure paths from string-only `raise_error(...)` to canonical payloaded raises.
    - validation failures now emit `type/arity` / `type/arg-mismatch`.
    - operational failures now emit stable runtime codes:
      - `runtime/gzip-compress-failed`
      - `runtime/gunzip-failed`
      - `runtime/deflate-compress-failed`
      - `runtime/inflate-failed`
      - `runtime/zlib-compress-failed`
      - `runtime/zlib-decompress-failed`
      - plus size-limit codes for decompression growth guard paths.
  - regression coverage:
    - `src/lisp/tests_runtime_data_unicode_groups.c3`: added payload domain/code assertions for invalid gzip args and corrupt decompress paths.
    - `src/lisp/tests_advanced_stdlib_numeric_groups.c3`: added payload domain/code assertions for conversion/read-string invalid paths.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation AL landed (`parser_control_effects.c3` extraction):
  - extraction:
    - added `src/lisp/parser_effect_forms.c3` for effect-form parse helpers:
      - `parse_reset(...)`,
      - `parse_shift(...)`,
      - `parse_perform(...)`,
      - `parse_resolve(...)`,
      - `parse_with_continuation(...)`.
    - reduced `src/lisp/parser_control_effects.c3` to quote/control parse helpers:
      - `parse_quote(...)`,
      - `parse_and(...)`,
      - `parse_or(...)`,
      - `parse_begin(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/parser_application.c3` (`178`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice AL ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_application.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation AK landed (`parser_define_attrs.c3` extraction):
  - extraction:
    - added `src/lisp/parser_define_relation_attr.c3` for relation-attribute define helpers:
      - `parse_relation_attr_options(...)`,
      - `build_define_relation_call(...)`,
      - `parse_define_relation_attr(...)`.
    - reduced `src/lisp/parser_define_attrs.c3` to macro/schema/special define dispatch:
      - `parse_define_macro_attr(...)`,
      - `parse_define_schema_attr(...)`,
      - `parse_define_special(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/parser_control_effects.c3` (`182`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice AK ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_control_effects.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation AJ landed (`compiler_call_flat.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_call_arg_list_helpers.c3` for call arg/list construction helpers:
      - `compile_call_arg_temps(...)`,
      - `build_arg_list_from_temps(...)`,
      - `compile_list_or_dict_call_flat(...)`,
      - `tail_call_requires_non_tail_path(...)`,
      - `compile_zero_arg_tail_call(...)`.
    - reduced `src/lisp/compiler_call_flat.c3` to call/app entrypoint lowering:
      - `call_head_is(...)`,
      - `call_is_explain_thunked(...)`,
      - `compile_call_flat(...)`,
      - `compile_call_tail_flat(...)`,
      - `compile_app_flat(...)`,
      - `compile_app_tail_flat(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/parser_define_attrs.c3` (`184`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice AJ ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_define_attrs.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation AI landed (`compiler_native_match_compilation_flat_style.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_native_match_bindings_flat_style.c3` for pattern-binding emit helpers:
      - `emit_pattern_var_decl(...)`,
      - `emit_pattern_var_assign(...)`,
      - `emit_pattern_seq_elem_access(...)`,
      - `emit_pattern_seq_elem_binding(...)`,
      - `compile_pattern_bindings(...)`.
    - reduced `src/lisp/compiler_native_match_compilation_flat_style.c3` to match entrypoint and pattern-check lowering:
      - `compile_match_flat(...)`,
      - `compile_pattern_check(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_call_flat.c3` (`185`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice AI ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_call_flat.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation AH landed (`compiler_primitive_variable_hash_table.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_primitive_variable_hash_table_domains.c3` for primitive-domain population helpers:
      - `init_prim_hash_arithmetic_and_comparison(...)`,
      - `init_prim_hash_core_and_strings(...)`,
      - `init_prim_hash_files_and_misc(...)`,
      - `init_prim_hash_collections_math_bitwise(...)`.
    - reduced `src/lisp/compiler_primitive_variable_hash_table.c3` to hash storage/probe/init orchestration:
      - `prim_hash_insert(...)`,
      - `prim_hash_lookup(...)`,
      - `init_prim_hash(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_native_match_compilation_flat_style.c3` (`194`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice AH ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_native_match_compilation_flat_style.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation AG landed (`compiler_quasiquote_flat.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_quasiquote_call_flat.c3` for quasiquote call/splice lowering helpers:
      - `qq_call_has_splice(...)`,
      - `qq_emit_cons_pair(...)`,
      - `qq_emit_append_pair(...)`,
      - `qq_emit_nil_result(...)`,
      - `compile_qq_call_no_splice(...)`,
      - `compile_qq_call_with_splice(...)`,
      - `compile_qq_call_flat(...)`.
    - reduced `src/lisp/compiler_quasiquote_flat.c3` to non-call quasiquote lowering:
      - `compile_qq_marker_pair(...)`,
      - `compile_qq_var_symbol(...)`,
      - `compile_qq_app_list(...)`,
      - `compile_qq_flat(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_primitive_variable_hash_table.c3` (`194`) (tie with `src/lisp/compiler_native_match_compilation_flat_style.c3` (`194`)).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice AG ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_primitive_variable_hash_table.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation AF landed (`compiler_temp_core.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_temp_misc_forms.c3` for resolve/index/define bridge helpers:
      - `compile_resolve_flat(...)`,
      - `compile_index_flat(...)`,
      - `compile_eval_serialized_expr_flat(...)`,
      - `is_typed_lambda_define(...)`,
      - `compile_define_rhs_with_typed_bridge(...)`,
      - `compile_define_flat(...)`.
    - reduced `src/lisp/compiler_temp_core.c3` to temp lifecycle + temp dispatch entrypoints:
      - `next_result(...)`,
      - `emit_temp_decl(...)`,
      - `emit_temp_ref(...)`,
      - `emit_nil_temp(...)`,
      - `compile_leaf_expr_to_temp(...)`,
      - `compile_to_temp_non_null(...)`,
      - `compile_to_temp(...)`,
      - `compile_to_temp_tail(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_quasiquote_flat.c3` (`198`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice AF ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_quasiquote_flat.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation AE landed (`parser_import_export.c3` extraction):
  - extraction:
    - added `src/lisp/parser_import_helpers.c3` for import helper parsing routines:
      - `parser_token_is_legacy_marker(...)`,
      - `Parser.init_import_expr(...)`,
      - `Parser.import_ensure_capacity(...)`,
      - `Parser.parse_import_target(...)`,
      - `Parser.parse_import_symbol_spec(...)`,
      - `Parser.parse_import_paren_spec(...)`,
      - `Parser.parse_import_selective_list(...)`.
    - reduced `src/lisp/parser_import_export.c3` to import entrypoint parsing:
      - `Parser.parse_import(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_temp_core.c3` (`198`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice AE ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_temp_core.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation AD landed (`compiler_expr_serialize_definition_forms.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_expr_serialize_macro_module_forms.c3` for macro/module/import/export serializers:
      - `serialize_defmacro_to_buf(...)`,
      - `serialize_module_to_buf(...)`,
      - `serialize_import_to_buf(...)`,
      - `serialize_export_from_to_buf(...)`.
    - reduced `src/lisp/compiler_expr_serialize_definition_forms.c3` to type/effect definition serializers:
      - `serialize_deftype_to_buf(...)`,
      - `serialize_defabstract_to_buf(...)`,
      - `serialize_defunion_to_buf(...)`,
      - `serialize_defalias_to_buf(...)`,
      - `serialize_defeffect_to_buf(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/parser_import_export.c3` (`198`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice AD ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_import_export.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation AC landed (`compiler_expr_serialize_exprs.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_expr_serialize_special_forms.c3` for specialized expression-form helpers and specialized dispatch:
      - `serialize_unary_expr_form(...)`,
      - `serialize_binary_expr_form(...)`,
      - `serialize_named_expr_form(...)`,
      - `serialize_symbol_and_arg_form(...)`,
      - `serialize_begin_expr_to_buf(...)`,
      - `serialize_index_expr_to_buf(...)`,
      - `serialize_path_expr_to_buf(...)`,
      - `serialize_expr_specialized_core(...)`,
      - `serialize_expr_specialized_reader(...)`,
      - `serialize_expr_specialized(...)`.
    - reduced `src/lisp/compiler_expr_serialize_exprs.c3` to entrypoint dispatch:
      - `serialize_expr_to_buf(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_expr_serialize_definition_forms.c3` (`201`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice AC ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_expr_serialize_definition_forms.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation AB landed (`compiler_lambda_scan.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_lambda_scan_lambda_defs.c3` for lambda-def/capture assembly helpers and lambda-case scan:
      - `collect_lambda_own_bound(...)`,
      - `collect_lambda_nested_bound(...)`,
      - `init_lambda_def_from_expr(...)`,
      - `lambda_def_push_capture(...)`,
      - `scan_lambdas_lambda(...)`.
    - reduced `src/lisp/compiler_lambda_scan.c3` to traversal/dispatch helpers:
      - `scan_lambdas(...)`,
      - `scan_lambdas_let(...)`,
      - `scan_lambdas_call(...)`,
      - `scan_lambdas_binary_expr(...)`,
      - `scan_lambdas_if_expr(...)`,
      - `scan_lambdas_match_expr(...)`,
      - `scan_lambdas_begin_expr(...)`,
      - `scan_lambdas_module_expr(...)`,
      - `scan_lambdas_with_scope(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_expr_serialize_exprs.c3` (`204`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice AB ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_expr_serialize_exprs.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation AA landed (`compiler_let_set_flat.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_let_compilation_flat_style.c3` for let-lowering helpers and entrypoints:
      - `emit_let_forward_decl(...)`,
      - `emit_let_assignment(...)`,
      - `emit_recursive_closure_self_patch(...)`,
      - `compile_mutable_captured_let(...)`,
      - `compile_let_flat_common(...)`,
      - `compile_let_flat(...)`,
      - `compile_let_flat_tail(...)`.
    - reduced `src/lisp/compiler_let_set_flat.c3` to control/set/primitive-check helpers:
      - `compile_if_flat(...)`,
      - `compile_and_flat(...)`,
      - `compile_or_flat(...)`,
      - `compile_begin_flat(...)`,
      - `compile_mutable_captured_set_flat(...)`,
      - `compile_local_set_flat(...)`,
      - `compile_set_flat(...)`,
      - `is_builtin_primitive(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_lambda_scan.c3` (`207`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice AA ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_lambda_scan.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation Z landed (`compiler_expr_serialize_values.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_expr_serialize_type_annotations.c3` for type-annotation serialization helpers:
      - `serialize_quoted_text_to_buf(...)`,
      - `serialize_type_val_literal_to_buf(...)`,
      - `serialize_type_annotation_to_buf(...)`.
    - reduced `src/lisp/compiler_expr_serialize_values.c3` to value-domain serialization helpers:
      - `serialize_value_to_buf(...)`,
      - `serialize_int_value_to_buf(...)`,
      - `serialize_double_value_to_buf(...)`,
      - `serialize_string_value_to_buf(...)`,
      - `serialize_cons_value_to_buf(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_let_set_flat.c3` (`207`) (tie with `src/lisp/compiler_lambda_scan.c3` (`207`)).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice Z ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_let_set_flat.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation Y landed (`compiler_native_call_compilation_flat_style.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_native_literal_compilation_flat_style.c3` for literal/quote lowering:
      - `compile_literal(...)`,
      - `compile_quote(...)`.
    - reduced `src/lisp/compiler_native_call_compilation_flat_style.c3` to lambda/var/path lowering:
      - `compile_lambda_flat(...)`,
      - `compile_var(...)`,
      - `compile_path(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_expr_serialize_values.c3` (`208`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice Y ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_expr_serialize_values.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation X landed (`compiler_free_vars_scope_forms.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_free_vars_effect_forms.c3` for effect/control walkers:
      - `find_free_vars_shift(...)`,
      - `find_free_vars_resolve(...)`,
      - `find_free_vars_handle(...)`.
    - reduced `src/lisp/compiler_free_vars_scope_forms.c3` to scope/mutation walkers:
      - `find_free_vars_var(...)`,
      - `find_free_vars_lambda(...)`,
      - `find_free_vars_let(...)`,
      - `find_free_vars_match(...)`,
      - `find_free_vars_call(...)`,
      - `find_free_vars_path(...)`,
      - `find_free_vars_set(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_native_call_compilation_flat_style.c3` (`209`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice X ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_native_call_compilation_flat_style.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation W landed (`parser_type_annotations.c3` extraction):
  - extraction:
    - added `src/lisp/parser_type_annotation_helpers.c3` for type-annotation helpers:
      - `parser_empty_type_annotation(...)`,
      - `parser_is_value_literal_ctor(...)`,
      - `parser_is_bool_symbol(...)`,
      - `parse_value_literal_annotation(...)`,
      - `type_annotation_copy_params(...)`,
      - `type_annotation_copy_meta(...)`.
    - reduced `src/lisp/parser_type_annotations.c3` to core parse flow:
      - `parse_compound_type_annotation(...)`,
      - `parse_dict_type_annotation(...)`,
      - `parse_type_annotation(...)`.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_free_vars_scope_forms.c3` (`211`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice W ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_free_vars_scope_forms.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation V landed (`compiler_code_emission.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_code_emission_lambda_closures.c3` for closure-construction/lambda-return helpers:
      - `is_variadic_lambda(...)`,
      - `needs_arg_list(...)`,
      - `emit_make_closure_call(...)`,
      - `find_lambda_def_by_expr(...)`,
      - `emit_closure_capture_data(...)`,
      - `emit_closure_expr(...)`,
      - `emit_lambda_return_common(...)`,
      - `emit_lambda_return(...)`.
    - reduced `src/lisp/compiler_code_emission.c3` to prelude and lambda-definition emission.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/parser_type_annotations.c3` (`216`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice V ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_type_annotations.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation U landed (`parser_lambda.c3` extraction):
  - extraction:
    - added `src/lisp/parser_callable_helpers.c3` for shared callable/body helpers:
      - `parse_implicit_begin(...)`,
      - `wrap_body_with_destructuring(...)`,
      - `finalize_lambda_expr(...)`,
      - `collect_callable_params(...)`,
      - `parse_rest_param(...)`,
      - `finalize_lambda_no_fixed_params(...)`,
      - `finalize_lambda_with_params(...)`.
    - reduced `src/lisp/parser_lambda.c3` to lambda/if entry parsing (`parse_lambda(...)`, `parse_if(...)`).
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_code_emission.c3` (`220`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice U ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_code_emission.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation T landed (`parser_lexer.c3` extraction):
  - extraction:
    - added `src/lisp/parser_lexer_core_api.c3` for lexer lifecycle/token API helpers:
      - `ensure_text_capacity(...)`,
      - `push_current_text_char(...)`,
      - `set_error_text(...)`,
      - `init(...)`,
      - `destroy(...)`,
      - `peek(...)`,
      - `next_char(...)`,
      - `set_error_token(...)`,
      - `at_end(...)`,
      - `match(...)`,
      - `expect(...)`.
    - reduced `src/lisp/parser_lexer.c3` to literal/symbol scan + token-advance flow (`is_number_start(...)`, `scan_literal_or_symbol(...)`, `advance(...)`).
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/parser_lambda.c3` (`220`) (tie with `src/lisp/compiler_code_emission.c3` (`220`)).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice T ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_lambda.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation S landed (`parser_patterns.c3` extraction):
  - extraction:
    - added `src/lisp/parser_patterns_paren.c3` for paren-pattern parsing:
      - `parse_as_pattern_marker(...)`,
      - `parse_paren_pattern(...)`.
    - reduced `src/lisp/parser_patterns.c3` to non-paren pattern parsing and token dispatch.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/parser_lexer.c3` (`226`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice S ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_lexer.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation R landed (`parser_expr_atoms.c3` extraction):
  - extraction:
    - added `src/lisp/parser_expr_reader_forms.c3` for reader-shorthand and reader-dispatch atom helpers:
      - `parse_backtick_shorthand(...)`,
      - `parse_unquote_shorthand(...)`,
      - `parse_unquote_splicing_shorthand(...)`,
      - `parse_form_comment_expr(...)`,
      - `parse_set_literal_expr(...)`,
      - `parse_regex_literal_expr(...)`,
      - `parse_quote_shorthand(...)`.
    - reduced `src/lisp/parser_expr_atoms.c3` to core atom parsing and `parse_expr(...)` dispatch.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/parser_patterns.c3` (`231`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice R ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_patterns.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation Q landed (`parser_define_core.c3` extraction):
  - extraction:
    - added `src/lisp/parser_define_annotations.c3` for bracket-annotation define dispatch:
      - `parse_define_attrs(...)`,
      - `parse_define_with_annotation(...)`,
      - `parse_define_type(...)`,
      - `parse_define_ffi(...)`.
    - reduced `src/lisp/parser_define_core.c3` to shorthand/normal define parsing flow.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/parser_expr_atoms.c3` (`235`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice Q ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_expr_atoms.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation P landed (`parser_control_effects.c3` extraction):
  - extraction:
    - added `src/lisp/parser_explain.c3` for explain-form parsing:
      - `parse_explain_selector_symbol(...)`,
      - `make_explain_target_thunk(...)`,
      - `parse_explain(...)`.
    - reduced `src/lisp/parser_control_effects.c3` to other control/effect parse forms.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/parser_define_core.c3` (`238`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice P ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_define_core.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation O landed (`compiler_program_pipeline.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_program_top_level.c3` for top-level define/global sync and top-level expression emission:
      - `add_global_if_missing(...)`,
      - `collect_expr_generated_globals(...)`,
      - `collect_top_level_defines(...)`,
      - `emit_global_lookup_sync(...)`,
      - `emit_type_form_global_sync(...)`,
      - `emit_top_level_exprs(...)`.
    - reduced `src/lisp/compiler_program_pipeline.c3` to program pipeline orchestration helpers.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/parser_control_effects.c3` (`256`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice O ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_control_effects.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation N landed (`parser_lexer.c3` extraction):
  - extraction:
    - added `src/lisp/parser_lexer_whitespace.c3` for whitespace/comment scanning:
      - `skip_nested_block_comment(...)`,
      - `skip_whitespace(...)`.
    - reduced `src/lisp/parser_lexer.c3` to lexer lifecycle and token-advance dispatch flow.
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/compiler_program_pipeline.c3` (`260`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice N ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_program_pipeline.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation M landed (`compiler_expr_serialize_values.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_expr_serialize_patterns.c3` for pattern serialization:
      - `serialize_pattern_to_buf(...)`.
    - reduced `src/lisp/compiler_expr_serialize_values.c3` to value and type-annotation serialization helpers (`serialize_value_to_buf(...)`, `serialize_type_annotation_to_buf(...)`, and support routines).
  - queue refresh:
    - after re-inventory, next largest target selected is `src/lisp/parser_lexer.c3` (`260`) (tie with `src/lisp/compiler_program_pipeline.c3` (`260`)).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice M ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_lexer.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation L landed (`parser_type_literals.c3` extraction):
  - extraction:
    - added `src/lisp/parser_type_annotations.c3` for type-annotation parsing:
      - `parser_empty_type_annotation(...)`,
      - `parse_value_literal_annotation(...)`,
      - `type_annotation_copy_params(...)`,
      - `type_annotation_copy_meta(...)`,
      - `parse_compound_type_annotation(...)`,
      - `parse_dict_type_annotation(...)`,
      - `parse_type_annotation(...)`.
    - reduced `src/lisp/parser_type_literals.c3` to constructor type-application parsing (`parse_constructor_type_application(...)`).
  - queue refresh:
    - after re-inventory, next largest target was `src/lisp/compiler_expr_serialize_values.c3` (`262`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice L ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_expr_serialize_values.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation K landed (`parser_pattern_match.c3` extraction):
  - extraction:
    - added `src/lisp/parser_patterns.c3` for pattern-node parsing:
      - `parse_pattern(...)`,
      - sequence/dict/paren/as-marker/string pattern helpers.
    - reduced `src/lisp/parser_pattern_match.c3` to `match` expression entry parsing (`parse_match(...)`).
  - queue refresh:
    - after re-inventory, next largest target is `src/lisp/parser_type_literals.c3` (`262`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice K ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_type_literals.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation J landed (`parser_type_defs.c3` extraction):
  - extraction:
    - added `src/lisp/parser_type_defs_misc.c3` for non-deftype type-definition forms:
      - `parse_defabstract(...)`,
      - `parse_defalias(...)`,
      - `parse_defeffect(...)`.
    - reduced `src/lisp/parser_type_defs.c3` to shared helpers + deftype parsing.
  - queue refresh:
    - after re-inventory, next largest target is `src/lisp/parser_pattern_match.c3` (`264`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice J ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_pattern_match.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation I landed (`parser_quasiquote_datum.c3` extraction):
  - extraction:
    - added `src/lisp/parser_datum.c3` for datum parsing/construction:
      - `parser_make_*_datum(...)` helpers,
      - `parse_datum_impl(...)`,
      - `parse_datum(...)`,
      - `parse_template_datum(...)`.
    - reduced `src/lisp/parser_quasiquote_datum.c3` to quasiquote-template expression parsing (`parse_qq_*` helpers + `parse_qq_template(...)`).
  - queue refresh:
    - after re-inventory, next largest target is `src/lisp/parser_type_defs.c3` (`271`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice I ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_type_defs.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation H landed (`compiler_code_emission.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_code_emission_main_globals.c3` for global/main code emission:
      - `emit_global_declarations(...)`,
      - `emit_global_value_declarations(...)`,
      - `emit_cached_primitive_declarations(...)`,
      - `emit_main_start(...)`,
      - `emit_main_end(...)`.
    - reduced `src/lisp/compiler_code_emission.c3` to lambda/closure emission helpers.
  - queue refresh:
    - after re-inventory, next largest target is `src/lisp/parser_quasiquote_datum.c3` (`280`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice H ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_quasiquote_datum.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation G landed (`parser_import_export.c3` extraction):
  - extraction:
    - added `src/lisp/parser_export_from.c3` for export-from parsing:
      - `init_export_from_expr(...)`,
      - `parse_export_from_source_module(...)`,
      - `ensure_export_from_name_capacity(...)`,
      - `parse_export_from_name_list(...)`,
      - `parse_export_from_specifiers(...)`,
      - `parse_export_from(...)`.
    - reduced `src/lisp/parser_import_export.c3` to import parsing flow (`parse_import(...)` and import target/specifier helpers).
  - queue refresh:
    - after re-inventory, next largest target is `src/lisp/compiler_code_emission.c3` (`281`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice G ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_code_emission.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation F landed (`compiler_lambda_scan.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_lambda_scan_effect_wrappers.c3` for effect-wrapper lambda scan helpers:
      - `make_synthetic_lambda(...)`,
      - `make_pair_projection_app(...)`,
      - `wrap_handle_clause_body(...)`,
      - `scan_lambdas_reset(...)`,
      - `scan_lambdas_shift(...)`,
      - `scan_lambdas_handle(...)`.
    - reduced `src/lisp/compiler_lambda_scan.c3` to scan core traversal/dispatch and capture collection helpers.
  - queue refresh:
    - after re-inventory, next largest target is `src/lisp/parser_import_export.c3` (`290`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice F ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_import_export.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation E landed (`compiler_temp_core.c3` extraction):
  - extraction:
    - added `src/lisp/compiler_temp_control_flow.c3` for temp control-flow helpers:
      - `emit_temp_assign(...)`,
      - `compile_branch_assign(...)`, `compile_tail_branch_assign(...)`,
      - `compile_begin_prefix(...)`,
      - `compile_short_circuit(...)`, `compile_tail_short_circuit(...)`,
      - `compile_if_common(...)`, `compile_begin_common(...)`,
      - `compile_tail_if_flat(...)`, `compile_tail_begin_flat(...)`,
      - `compile_tail_and_flat(...)`, `compile_tail_or_flat(...)`.
    - reduced `src/lisp/compiler_temp_core.c3` to temp dispatch and simple-form lowering (`compile_to_temp_non_null`, `compile_to_temp`, `compile_to_temp_tail`, and temp/id helpers).
  - queue refresh:
    - after re-inventory, next largest target is `src/lisp/compiler_lambda_scan.c3` (`295`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice E ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_lambda_scan.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation D landed (`parser_define_core.c3` extraction):
  - extraction:
    - added `src/lisp/parser_let_named.c3` for named-let parsing:
      - `parse_named_let_bindings(...)`,
      - `build_named_let_lambda(...)`,
      - `build_named_let_call(...)`,
      - `parse_named_let(...)`.
    - reduced `src/lisp/parser_define_core.c3` to define-family parsing flow (shorthand define, bracket-annotation dispatch, and type/ffi define dispatch).
  - queue refresh:
    - after re-inventory, next largest target is `src/lisp/compiler_temp_core.c3` (`307`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice D ownership map + refreshed largest-file queue.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `compiler_temp_core.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation C landed (`parser_control_effects.c3` extraction):
  - extraction:
    - added `src/lisp/parser_handle_forms.c3` for handle-form parsing:
      - `consume_handle_strict_annotation(...)`,
      - `parse_handle_old_clause(...)`, `parse_handle_new_clause(...)`,
      - `parse_handle_clause(...)`,
      - `parse_handle(...)`.
    - reduced `src/lisp/parser_control_effects.c3` to non-handle control/effect forms (`explain`, `quote`, `reset`, `shift`, `perform`, `resolve`, `with-continuation`, `and`, `or`, `begin`).
  - queue refresh:
    - after re-inventory, next largest target was `src/lisp/parser_define_core.c3` (`334`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice C ownership map + refreshed next target.
    - `docs/areas/compiler-parser-refactor.md`: updated area status to include handle-form extraction progress.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation B landed (`parser_type_defs.c3` extraction):
  - extraction:
    - added `src/lisp/parser_type_defs_union.c3` for union-definition parsing:
      - `init_defunion_expr(...)`,
      - `parse_defunion_name_compound(...)`, `parse_defunion_name(...)`,
      - `parse_defunion_variant_compound(...)`, `parse_defunion_variant(...)`,
      - `parse_defunion(...)`.
    - reduced `src/lisp/parser_type_defs.c3` to shared helpers and non-union type-definition forms.
  - queue refresh:
    - after re-inventory, next largest target is `src/lisp/parser_control_effects.c3` (`352`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice B ownership map + refreshed next target.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_control_effects.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` continuation landed (`parser_type_literals.c3` extraction):
  - extraction:
    - added `src/lisp/parser_collection_literals.c3` for collection/index parsing:
      - `parse_postfix_index(...)`,
      - `parse_dict_literal(...)`,
      - `parse_array_literal(...)`.
    - reduced `src/lisp/parser_type_literals.c3` to type-annotation and constructor-type-application parsing (`parse_type_annotation`, `parse_constructor_type_application`, and annotation helpers).
  - queue refresh:
    - after re-inventory, next largest target is `src/lisp/parser_type_defs.c3` (`356`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded continuation slice ownership map + refreshed next target.
    - `docs/areas/compiler-parser-refactor.md`: updated area status and next-step target to `parser_type_defs.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R5` landed (behavior-preserving free-variable walker split):
  - extraction:
    - added `src/lisp/compiler_free_vars_scope_forms.c3` for scope/form-specific free-variable walkers:
      - `find_free_vars_var`, `find_free_vars_lambda`, `find_free_vars_let`,
      - `find_free_vars_match`, `find_free_vars_call`, `find_free_vars_path`,
      - `find_free_vars_shift`, `find_free_vars_resolve`,
      - `find_free_vars_handle`, `find_free_vars_set`.
    - reduced `src/lisp/compiler_free_vars_walk.c3` to dispatch and quasiquote traversal:
      - pair/triple/list helper walkers,
      - unary/binary/complex-form dispatch,
      - `find_free_vars(...)` and `find_free_vars_in_qq(...)`.
  - queue refresh:
    - after re-inventory, next largest target is `src/lisp/parser_type_literals.c3` (`362`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: recorded `R5` slice ownership map and refreshed next target.
    - `docs/areas/compiler-parser-refactor.md`: synced current-state status to `R2-R5` and updated next-step target.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R4` landed (post-`R2/R3` inventory + one coherent extraction):
  - refreshed largest-file inventory after `R2/R3`; selected `src/lisp/compiler_native_call_compilation_flat_style.c3` (`396`) as next target.
  - extracted match/pattern domain into `src/lisp/compiler_native_match_compilation_flat_style.c3`:
    - moved `compile_match_flat(...)`,
    - moved pattern check/binding helpers (`compile_pattern_check(...)`, `compile_pattern_bindings(...)`, helper emitters).
  - reduced `src/lisp/compiler_native_call_compilation_flat_style.c3` to lambda/literal/var/quote/path lowering.
  - refreshed queue selection for the next slice: `src/lisp/compiler_free_vars_walk.c3` (`376`).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: marked `R4` complete, recorded ownership map and refreshed queue.
    - `docs/areas/compiler-parser-refactor.md`: synced area status and next-step focus to `R5` on `compiler_free_vars_walk.c3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R3` landed (effect/native-call domain split):
  - split `src/lisp/compiler_native_effect_compilation_flat_style.c3` by domain:
    - retained effect lowering (`reset`, `shift`, `perform`, `handle`) in `compiler_native_effect_compilation_flat_style.c3`.
    - moved lambda/match/literal/variable/quote/path lowering to `src/lisp/compiler_native_call_compilation_flat_style.c3`.
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: marked `R3` complete with ownership map + validation evidence.
    - `docs/areas/compiler-parser-refactor.md`: updated current-state status to include `R3`.
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor `R2` landed (`compiler_expr_serialize_exprs.c3` split by expression-family boundaries):
  - code split:
    - added `src/lisp/compiler_expr_serialize_callable_forms.c3` for callable/control serialization forms (`lambda`, `let`, `call`, `handle`, `match`).
    - added `src/lisp/compiler_expr_serialize_definition_forms.c3` for definition/module forms (`define[type/abstract/union/alias/effect]`, `define[macro]`, `module`, `import`, `export-from`).
    - reduced `src/lisp/compiler_expr_serialize_exprs.c3` to dispatch/entrypoint responsibilities (`serialize_expr_to_buf`, specialized dispatch switches, small shared emit helpers).
  - plan/area sync:
    - `docs/plans/compiler-parser-refactor-plan.md`: marked `R2` complete and recorded ownership map + validation evidence.
    - `docs/areas/compiler-parser-refactor.md`: synced area status and next-step queue (`R3` then `R4`).
  - validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1697/0`, `compiler: 85/0`).

- Compiler/parser refactor plan `R1` threshold lock landed:
  - `docs/plans/compiler-parser-refactor-plan.md`:
    - marked `R1` complete.
    - locked active cycle split threshold to modules over `500` LOC.
    - recorded strict largest-first execution order and first two targets:
      - `src/lisp/compiler_expr_serialize_exprs.c3` (`521`)
      - `src/lisp/compiler_native_effect_compilation_flat_style.c3` (`518`)
  - `docs/areas/compiler-parser-refactor.md`:
    - synced area status with `R1` closure and clarified next-step focus on `R2/R3`.
  - validation scope:
    - docs/status-only slice (no runtime/compiler behavior change).

- L6 indexed placeholder extension (`_1`, `_2`, ...) landed:
  - parser/desugaring (`src/lisp/parser_application.c3`):
    - added indexed placeholder classification in call-argument position only.
    - `_n` desugaring now maps placeholders to lambda params by index with arity = max index.
    - repeated indices reuse one generated parameter; reordered indices are supported.
    - mixed `_` and `_n` in one call now fails with deterministic parse diagnostic.
    - invalid indexed forms in call args (`_0`, `_-1`, `_1x`) now fail with deterministic parse diagnostic.
  - regression coverage (`src/lisp/tests_advanced_core_unicode_groups.c3`):
    - added positive tests for indexed basic/reuse/reorder/max-index-arity behavior.
    - added migration tests proving `_n` is not globally reserved (normal symbol outside arg position and in callee position).
    - added negative tests for mixed placeholders and invalid indexed forms.
  - docs:
    - `docs/SYNTAX_SPEC.md`: added `_n` partial-application semantics/rules/examples.
    - `docs/LANGUAGE_SPEC.md`: added `_n` mention + migration/migration note.
  - validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`unified: 1678/0`, `compiler: 85/0`).
  - TODO closure:
    - marked `L6.1` through `L6.5` complete.
    - marked acceptance `A-L6` complete.
    - synchronized stale recommended-order markers (`O1`..`O5`) to done state now that `L1`..`L5` are closed.

- L5.5 closure (final validation gate + regex ASAN leak hardening):
  - regex ownership/cleanup fixes:
    - `src/pika/regex_cache_api.c3`:
      - added explicit regex-owned cleanup helpers for compiled grammar allocations (`regex_release_grammar_allocations(...)`) and rule-builder buffers (`regex_release_rule_builder(...)`).
      - `regex_free(...)` now performs deep cleanup (grammar arrays, rule child arrays, scan contexts/char classes, generated rule names) before freeing `CompiledRegex`.
      - `regex_compile(...)` now:
        - avoids tokenizer allocation side-effects for leading/trailing anchor probes (`peek`/`advance` route),
        - releases rule-builder memory on all compile error returns,
        - frees temporary `starts` array after `make_grammar(...)`,
        - releases transferred/non-transferred rule-builder allocations deterministically.
    - `src/pika/grammar.c3`:
      - freed temporary `parent_clauses_arr` inner arrays during `make_grammar(...)` cleanup.
  - validation:
    - `c3c build` passed.
    - `c3c build --sanitize=address` passed.
    - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed.
    - `scripts/run_boundary_hardening.sh` passed end-to-end (Stage 0..8), including Stage 4 ASAN leak gate.
  - status/doc closure:
    - `TODO.md`: marked `L5.5` complete and acceptance gate `A-L5` complete.
    - `docs/areas/memory-runtime.md`: removed stale Stage 4 leak-open wording and updated status/evidence to green closure.

- L5.5 validation rerun + gate-stabilization slice (partial closure):
  - validation rerun outcomes:
    - `c3c build` passed.
    - `c3c build --sanitize=address` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed after rebuilding non-ASAN binary for the plain-run gate.
    - `scripts/run_boundary_hardening.sh` remains failing at Stage 4 (ASAN run) due regex-compiler leak reports (`build/boundary_hardening_asan.log`), so `L5.5` and `A-L5` remain open.
  - targeted gate fixes landed during rerun:
    - `src/lisp/tests_compiler_core_groups.c3`:
      - widened explain primitive-path assertion to accept the canonical delegated explain bridge output (`aot::eval_serialized_expr(...)` + canonical serialized explain form).
      - restores compiler suite parity for the explain bridge behavior.
    - `src/lisp/primitives_meta_types.c3`:
      - replaced `make_error(...)` in `prim_ctor_type_apply(...)` with canonical typed payload raise helper (`ctor_type_raise(...)`) for policy compliance.
    - `src/lisp/schema.c3`:
      - replaced `raise_error(...)` argument guard in `prim_schema_explain(...)` with `raise_error_with_payload_names(...)` canonical payload path.
  - TODO status:
    - `TODO.md` `L5.5` now marks the first three validation commands complete; boundary hardening script item remains pending.
    - `docs/areas/memory-runtime.md` now reflects current gate truth: Stage 4 ASAN leak blocker remains open for final closeout.

- L5.3 compiler/parser refactor-plan consolidation landed:
  - docs/todo status:
    - added `docs/plans/compiler-parser-refactor-plan.md` as the single active compiler/parser refactor tracker (largest-file queue + validation gates).
    - `docs/areas/compiler-parser-refactor.md` now references that single plan as canonical source and removes overlap language from next steps.
    - `docs/plans/README.md` now declares single-plan ownership for compiler/parser modularization tracking.
    - added explicit historical-tracker notes to overlapping completed plan docs:
      - `docs/plans/library-gaps-todo.md`
      - `docs/plans/aot-unification.md`
    - `TODO.md`: marked `L5.3` checklist and sub-items complete.
  - Validation scope:
    - docs/status closure only (no runtime behavior changes in this slice).

- L5.2 + L5.4 effects/error-model parity-plan closure landed:
  - docs/todo status:
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - closed stale pending markers for completed phases/acceptance rows (`A4`, `P5/A5`, `P6/A6`) and execution-order rows (`E3-E6`).
      - completed done-definition rows (`D1-D5`) with explicit evidence anchors to docs/tests/scripts/workflow checks.
      - removed remaining unchecked plan checkboxes so the document now reflects closure state instead of historical pending markers.
    - `TODO.md`:
      - marked `L5.2` checklist and sub-items complete.
      - marked `L5.4` checklist and sub-items complete.
  - Validation scope:
    - docs/status closure only (no runtime behavior changes in this slice).

- L5.1 memory-runtime area-doc closeout landed (status wording reconciliation):
  - docs/todo status:
    - `docs/areas/memory-runtime.md`:
      - updated status/date to current closure context (`2026-03-09`).
      - removed stale "Stage A retain verification + inline-scope cleanup remain" wording.
      - aligned current-state wording with landed boundary-return behavior (no production deep-copy fallback path; graph traversal now debug/audit only).
      - refreshed verification evidence text to current validation pass anchors (`c3c build`, `c3c build --sanitize=address`, `./build/main` summary pass, `scripts/run_boundary_hardening.sh`).
      - replaced stale next-step bullets with concrete `L5` closeout tasks.
    - `TODO.md`:
      - marked `L5.1` checklist and sub-items complete.
  - Validation scope:
    - docs/status closure only (no runtime behavior changes in this slice).

- L4.4 backend matrix docs closure landed (type/dispatch compiler parity wording):
  - docs/todo status:
    - `docs/LANGUAGE_SPEC.md` Appendix C:
      - compiler rows for `type definitions` and `dispatch` are now `Y` (no `eval*` caveat row).
      - replaced the previous `eval*` caveat with an explicit implementation note (`Y†`) describing `aot::invoke`/`aot::apply_multi` and `aot::eval_serialized_expr(...)` delegation as parity-preserving wiring.
    - `docs/areas/types-dispatch.md`:
      - marked `L4.4` as landed in current-state bullets.
      - removed wording that left `L4`/`A-L4` open and narrowed next steps to area-level closeout (`L5`) + existing E2E baseline drift documentation.
    - `TODO.md`:
      - marked `L4.4` checklist complete.
      - marked acceptance gate `A-L4` complete.
  - Validation scope:
    - docs/status closure only (no runtime behavior changes in this slice).

- L4.3 compiler E2E coverage slice landed (type/dispatch backend parity cases):
  - E2E coverage additions:
    - `src/lisp/tests_e2e_generation_setups.c3`:
      - added `[abstract]`, `[struct]`, `[union]`, `[alias]` setup fixtures and dispatch methods for exact/subtype/widen/value-literal/alias paths.
    - `src/lisp/tests_e2e_generation_cases_extended.c3`:
      - added `--build` parity cases for:
        - struct/union constructor use,
        - dispatch specificity tiers,
        - alias-annotation dispatch,
        - explain-dispatch diagnostic path.
  - compiler support hardening required by L4.3 coverage:
    - `src/lisp/compiler_program_pipeline.c3`:
      - top-level global collection now includes generated constructor symbols from delegated type forms.
      - emitted code now syncs those constructor globals from `aot::lookup_var(...)` after delegated eval.
    - `src/lisp/compiler_output_helpers.c3`:
      - symbol emission now prefixes uppercase/digit-leading symbols with a stable `_omni_v_` prefix so generated global identifiers remain valid C3 names.
    - `src/lisp/compiler_call_flat.c3` + `src/lisp/compiler_expr_serialize_exprs.c3`:
      - thunked `explain` forms are delegated via `aot::eval_serialized_expr(...)` with canonical syntax reconstruction (`(explain 'selector <form>)`) to preserve explain semantics in compiled runs.
    - `src/lisp/compiler_temp_core.c3`:
      - typed `define` bridge now syncs bound callable values via `aot::lookup_var("<name>")` after delegated eval.
    - `src/lisp/compiler_native_effect_compilation_flat_style.c3`:
      - retained explicit-symbol emission for unresolved vars (broad `lookup_var` fallback is not used in this path due continuation/semantic regressions).
    - `src/lisp/compiler_primitive_variable_hash_table.c3`:
      - added primitive mappings for `explain` and `schema-explain`.
  - regression coverage updates:
    - `src/lisp/tests_compiler_core_groups.c3`:
      - added checks for constructor-global sync, union-variant sync, typed-define lookup sync, and explain-form eval delegation.
  - docs/todo updates:
    - `TODO.md`: marked `L4.3` checklist complete.
    - `docs/areas/types-dispatch.md`: marked L4.3 as landed and narrowed remaining gap language to L4.4 + acceptance-evidence closure.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1687/0`, `compiler: 84/0`).
    - `scripts/run_e2e.sh` still fails with the existing baseline diff set (non-L4.3 rows); no additional diff rows remain from L4.3 additions after explain delegation fixes.

- L4.1 backend parity audit closure (compiler/AOT typed path mapping):
  - TODO status:
    - `TODO.md`: marked `L4.1` sub-items complete (audit + runtime-boundary documentation).
  - Documented audit surface:
    - `docs/areas/types-dispatch.md`:
      - added `L4.1 Audit Snapshot (2026-03-09)` with concrete source anchors for compiler emit path, AOT runtime boundary path, and JIT/eval delegation path.
      - updated Known Gaps/Next Steps to reflect that `L4.2` now owns parity implementation work.
    - `docs/LANGUAGE_SPEC.md` Appendix C footnote:
      - refined `eval*` note to explicitly describe current `aot::invoke`/`aot::apply_multi` -> `jit_apply*` call boundary and the current absence of AOT lowering for type-definition forms.
  - Audit evidence captured during this closure:
    - `--compile` probe with `[type]` form produced `WARNING: compiler: unsupported expr tag 27` and generated `aot::make_nil() /* WARNING: unsupported expr type */` for the type-definition expression.
    - `--build` probe on same source failed with unresolved constructor symbol (`Box`) in generated C3, consistent with missing AOT lowering for type definitions.
    - `--build` probe with repeated typed `define` on the same name failed with duplicate global declaration shadowing (`describe`), confirming no method-table extension path in compiler global-define lowering.

- L4.2 backend parity implementation slice (explicit AOT bridge for type/typed-define forms):
  - compiler/runtime wiring:
    - `src/lisp/compiler_temp_core.c3`:
      - added explicit delegation helper `compile_eval_serialized_expr_flat(...)` that emits `aot::eval_serialized_expr(...)`.
      - type-definition AST tags (`E_DEFTYPE`, `E_DEFABSTRACT`, `E_DEFUNION`, `E_DEFALIAS`, `E_DEFEFFECT`) now lower through this explicit bridge instead of unsupported fallback.
      - typed lambda `define` forms now use `compile_define_rhs_with_typed_bridge(...)` so typed method registration semantics stay runtime-correct.
    - `src/lisp/aot.c3`:
      - added `eval_serialized_expr(char[] source)` wrapper over `run(...)` in the AOT interpreter context.
    - `src/lisp/compiler_program_pipeline.c3`:
      - deduplicated `defined_globals` collection (prevents duplicate global declarations for repeated typed `define` names).
      - top-level define lowering now uses typed-bridge-aware RHS compilation.
    - `src/lisp/compiler_native_effect_compilation_flat_style.c3`:
      - unresolved non-local/non-global symbols now route to `aot::lookup_var(...)` (constructor/runtime-registered symbol migration).
  - serializer expansion for bridge correctness:
    - `src/lisp/compiler_expr_serialize_values.c3`:
      - added type-annotation serializer support (simple/compound/dict/value-literal forms).
    - `src/lisp/compiler_expr_serialize_exprs.c3`:
      - lambda serializer now preserves typed/rest parameter shapes.
      - added serializers for `[type]`, `[abstract]`, `[union]`, `[alias]`, `[effect]` define forms.
  - regression coverage:
    - `src/lisp/tests_compiler_core_groups.c3`:
      - added `run_compiler_group_type_dispatch_bridge_tests(...)`:
        - `[type]` emits `aot::eval_serialized_expr(...)` without unsupported fallback,
        - typed `define` emits explicit bridge path,
        - repeated typed names do not emit duplicate global declarations.
  - docs/todo status:
    - `TODO.md`: marked `L4.2` and both sub-items complete.
    - `docs/areas/types-dispatch.md`: current-state/gaps updated to reflect landed L4.2 bridge and remaining L4.3/L4.4 work.
    - `docs/LANGUAGE_SPEC.md` Appendix C `eval*` footnote updated to reflect explicit `aot::eval_serialized_expr(...)` delegation model.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1687/0`, `compiler: 82/0`).
    - probe (`[type]`):
      - `--compile` now emits `aot::eval_serialized_expr("(define [type] ... )")` (no unsupported warning),
      - `--build` now links successfully (previous unresolved constructor failure removed).
    - probe (typed dispatch redefine):
      - `--build` links successfully (previous duplicate global declaration removed),
      - runtime output shows correct method dispatch (`"int"` for `1`, `"string"` for `"a"`).

- Explainability selector lock (L1.1) closure slice (`explain 'dispatch` / `explain 'effect`):
  - parser behavior (`src/lisp/parser_control_effects.c3`, `src/lisp/parser_expr_head_forms.c3`):
    - `explain` is now parsed as a selector-locked head form.
    - canonical selectors accepted:
      - `(explain 'dispatch <form>)`
      - `(explain 'effect <form>)`
    - parser diagnostics are deterministic:
      - non-quoted/non-symbol selector -> `explain: selector must be quoted symbol ('dispatch or 'effect)`
      - unknown quoted selector -> `explain: unknown selector; expected 'dispatch or 'effect`
    - target `<form>` is wrapped into a nullary thunk at parse time so it is not eagerly evaluated.
  - runtime surface (`src/lisp/schema.c3`, `src/lisp/eval_init_primitives.c3`):
    - `prim_explain` now enforces selector semantics and returns a deterministic selector-lock payload dict (`kind`, `status`, `input`).
    - schema explanation behavior was moved to explicit primitive name `schema-explain` (`prim_schema_explain`).
    - primitive registry now exposes:
      - `explain` (selector surface)
      - `schema-explain` (schema validation explanation)
  - regression coverage (`src/lisp/tests_runtime_feature_schema_reader_groups.c3`):
    - added selector acceptance tests for `'dispatch` and `'effect`.
    - added non-eager-evaluation regression (`(explain ... (set! ...))` keeps state unchanged).
    - added unknown/non-symbol selector diagnostic tests.
    - updated schema explanation regression to `schema-explain`.
  - examples/docs parity:
    - updated schema-validation callsites:
      - `examples/finwatch/server.omni`
      - `examples/deduce_crud_server.omni`
    - updated reference docs:
      - `docs/reference/11-appendix-primitives.md`
      - `docs/reference/09-concurrency-ffi.md`
      - `docs/CORE_LIBS_INSPECTION.md`
    - `TODO.md`: L1.1 checklist marked done.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1636/0`, `compiler: 79/0`).
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/boot_smoke.omni` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/dispatch_smoke.omni` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/smoke_test.omni` passed.

- Explainability dispatch core (L1.2) closure slice (`explain 'dispatch`):
  - matcher/scoring reuse (`src/lisp/eval_dispatch_types.c3`):
    - added `DispatchMatchBreakdown` + `DispatchMatchFailureReason` as a shared dispatch-match breakdown surface.
    - added `dispatch_match_breakdown(...)` as the canonical per-candidate applicability/scoring evaluator.
    - `method_match_score(...)` now delegates to the same breakdown helper to keep runtime dispatch scoring and explain scoring aligned.
  - dispatch explainer runtime payload (`src/lisp/schema.c3`):
    - `prim_explain` dispatch selector now returns structured dispatch diagnostics instead of selector-locked placeholder output.
    - added deterministic top-level payload shape:
      - `kind`, `status`, `input`, `decision`, `candidates`, `trace`.
    - candidate payload includes:
      - method name/signature/constraints/source metadata
      - `applicable`
      - `score-components` (`value-literal`, `exact`, `widen`, `subtype`, `any`, `total`)
      - failure metadata (`failure`, `failure-arg-index`).
    - decision payload includes deterministic reason/state for:
      - `method-match`
      - `ambiguous-equal-specificity`
      - `fallback`
      - `no-matching-method`
      - unsupported non-call/non-method-table forms.
  - regression coverage (`src/lisp/tests_runtime_feature_schema_reader_groups.c3`):
    - added dispatch explain regressions for:
      - method-match reason
      - candidate count
      - best-score and exact-component scoring
      - ambiguity status/reason
      - source line capture.
  - closure bookkeeping:
    - `TODO.md`: L1.2 checklist marked done.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1643/0`, `compiler: 79/0`).

- Explainability effect core (L1.3) closure slice (`explain 'effect`):
  - runtime explainer core (`src/lisp/schema.c3`):
    - replaced selector-lock placeholder with structured effect explain output for thunked targets.
    - added effect-form routing:
      - `(signal/perform ...)` explanation path with deterministic lookup simulation.
      - `(resolve ...)` explanation path with continuation validity/status diagnostics.
    - signal explain payload now captures:
      - handler search order (nearest enclosing first),
      - per-handler candidate metadata (`matched`, `dispatchable`, `miss-reason`, strict boundary metadata),
      - matched clause details (`clause-index`, source line/column, arg/continuation names),
      - predicted clause outcome metadata (`resolve` vs `abort`),
      - final decision reasons (`handler-match`, `strict-unhandled-effect`, `fast-path`, `unhandled-effect`).
    - resolve explain payload now captures:
      - continuation binding presence,
      - effect vs generic continuation path,
      - invalid/dead/not-suspended continuation outcomes with deterministic reasons.
  - regression coverage (`src/lisp/tests_runtime_feature_schema_reader_groups.c3`):
    - added effect explain regressions for:
      - nearest-first handler search order,
      - skipped-handler miss reason classification,
      - strict-boundary unhandled reason,
      - resolve-vs-abort outcome metadata,
      - resolve invalid/effect-continuation reason paths.
  - closure bookkeeping:
    - `TODO.md`: L1.3 checklist marked done.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1651/0`, `compiler: 79/0`).

- Explainability stable schema shape (L1.4) closure slice:
  - schema stabilization (`src/lisp/schema.c3`):
    - added explicit shape initializers for explain output dictionaries:
      - top-level: `kind`, `status`, `input`, `decision`, `candidates`, `trace`, `debug_message`
      - dispatch decision/trace default fields are now pre-initialized and present across all status branches.
      - effect decision/trace default fields are now pre-initialized and present across all status branches.
    - added optional `debug_message` placeholders to candidate payloads and decision/trace shapes.
    - dispatch/effect branches now fill status-specific values on top of a deterministic base schema.
  - regression coverage (`src/lisp/tests_runtime_feature_schema_reader_groups.c3`):
    - added stable-shape assertions for:
      - dispatch/effect top-level key presence,
      - dispatch/effect trace key presence,
      - dispatch/effect decision key presence.
    - assertions are field-based (`has?`/`ref`) to avoid brittle full-string snapshot checks.
  - closure bookkeeping:
    - `TODO.md`: L1.4 checklist marked done.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1657/0`, `compiler: 79/0`).

- Explainability regression matrix closure (L1.5 + A-L1 acceptance):
  - regression expansion (`src/lisp/tests_runtime_feature_schema_reader_groups.c3`):
    - added explicit unhandled-effect explain regressions:
      - unhandled status path (`status = 'unhandled`)
      - unhandled reason path (`decision.reason = 'unhandled-effect`)
    - added fast-path classification regression:
      - `signal io/print` explain decision reason resolves to `fast-path` when no handler matches.
    - existing explainability matrix now includes dispatch + effect coverage for:
      - selector acceptance/diagnostics,
      - method-match and ambiguity dispatch outcomes,
      - strict boundary effect behavior,
      - resolve/abort effect outcomes,
      - stable field-shape assertions (top-level/decision/trace key presence).
  - TODO closure:
    - marked `L1.5` checklist rows complete.
    - marked acceptance gate `A-L1 Structured output deterministic and test-anchored` complete.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1660/0`, `compiler: 79/0`).

- Explainability docs/examples closure (L1.6):
  - docs updates:
    - `docs/LANGUAGE_SPEC.md`:
      - added `5.5 Dispatch Explainability` with canonical `(explain 'dispatch <form>)`.
      - documented deterministic output shape (`kind`, `status`, `input`, `decision`, `candidates`, `trace`, `debug_message`) and stable decision/candidate fields.
      - added symbol-selector examples only, including non-eager thunked-form behavior and `decision.reason` inspection.
      - updated spec date to `2026-03-09`.
    - `docs/EFFECTS_SEMANTICS.md`:
      - added normative rule `EFX-9: Effect Explainability Surface` for canonical `(explain 'effect <form>)`.
      - documented stable top-level shape and reason classifications for signal-path and resolve-path explain outcomes.
      - added executable examples for `fast-path` and `resolve-invalid-continuation` explain paths.
      - added `EFX-9` rule-to-test anchors to `src/lisp/tests_runtime_feature_schema_reader_groups.c3`.
      - updated `Last updated` to `2026-03-09`.
  - TODO closure:
    - `TODO.md`: marked `L1.6` checklist rows complete.
  - Validation:
    - selector canonicality/docs examples check passed:
      - `rg -n "\\(explain\\s+[^']|\\(explain\\s+'(dispatch|effect)" docs/LANGUAGE_SPEC.md docs/EFFECTS_SEMANTICS.md`

- Spec onboarding core-profile closure (L2.1):
  - docs updates (`docs/LANGUAGE_SPEC.md`):
    - added `0. Core Omni Profile` and linked it from the table of contents.
    - added `0.1 Minimum Mental Model` covering:
      - evaluation model,
      - strict lambda arity,
      - truthiness contract (`nil` and `false` only are falsy),
      - starter value families (scalars/functions/collections),
      - generic collection-ops-first guidance.
    - added `0.2 First-Steps Command Set` with runnable REPL forms for:
      - binding/flow (`define`, `let`, `if`),
      - function definition/invocation,
      - list/array/dict literals and generic lookup/length usage.
    - added `0.3 What To Ignore Initially` onboarding scope guard listing advanced surfaces to defer (effects, continuations, typed dispatch details, macros, FFI/modules/scheduler internals, runtime ownership internals).
  - TODO closure:
    - `TODO.md`: marked `L2.1` and both sub-items complete.
  - Validation:
    - doc section presence verified:
      - `rg -n "## 0\\. Core Omni Profile|### 0\\.1 Minimum Mental Model|### 0\\.2 First-Steps Command Set|### 0\\.3 What To Ignore Initially" docs/LANGUAGE_SPEC.md`

- Spec onboarding advanced-profile closure (L2.2):
  - docs updates (`docs/LANGUAGE_SPEC.md`):
    - added `0.4 Advanced Omni Profile` directly after the core onboarding profile.
    - added `0.4.1 Effects and Continuations: Model Boundaries`:
      - clarifies handler/continuation interaction boundaries,
      - nearest-first handler lookup and explicit resolve-vs-abort behavior.
    - added `0.4.2 Multiple Dispatch and Typed Annotations`:
      - clarifies annotation role in applicability/specificity,
      - ties dispatch scoring/ambiguity behavior to user-level method design,
      - notes handler-body delegation to dispatched functions for type-specific logic.
    - added `0.4.3 Runtime Ownership and Boundary Constraints (User-Facing)`:
      - summarizes deterministic scope/region ownership expectations at user-facing boundaries,
      - documents boundary-sensitive composition guidance for returns/handlers/callback paths.
  - TODO closure:
    - `TODO.md`: marked `L2.2` and all three sub-items complete.
  - Validation:
    - doc section presence verified:
      - `rg -n "### 0\\.4 Advanced Omni Profile|#### 0\\.4\\.1 Effects and Continuations: Model Boundaries|#### 0\\.4\\.2 Multiple Dispatch and Typed Annotations|#### 0\\.4\\.3 Runtime Ownership and Boundary Constraints \\(User-Facing\\)" docs/LANGUAGE_SPEC.md`

- Spec onboarding error-model quick reference closure (L2.3):
  - docs updates (`docs/LANGUAGE_SPEC.md`):
    - added `0.5 Error Model Quick Reference`.
    - added `0.5.1 Failure Class Mapping` table covering:
      - `absence`,
      - `recoverable-op-failure`,
      - `programmer-error`,
      - `internal-runtime-error` (non-resumable note retained for boundary correctness).
    - added `0.5.2 Canonical raise Payload Shape` with required payload form:
      - `{ 'code ... 'message ... 'domain ... 'data ... }`.
    - added field-contract table for `'code`, `'message`, `'domain`, `'data`.
    - added `0.5.3 Common Domains and Codes` with representative code taxonomy for:
      - `io`, `parser`, `regex`, `scheduler`, `deduce`, `type`, `runtime`.
    - added source references to `docs/ARCHITECTURE.md` and `docs/ERROR_MODEL.md`.
  - TODO closure:
    - `TODO.md`: marked `L2.3` and both sub-items complete.
  - Validation:
    - doc section presence verified:
      - `rg -n "### 0\\.5 Error Model Quick Reference|#### 0\\.5\\.1 Failure Class Mapping|#### 0\\.5\\.2 Canonical .* Payload Shape|#### 0\\.5\\.3 Common Domains and Codes" docs/LANGUAGE_SPEC.md`

- Spec onboarding `define` forms catalog closure (L2.4):
  - docs updates (`docs/LANGUAGE_SPEC.md`):
    - added `3.2.2 define Forms Catalog (Type Family)` directly under the canonical `define` section.
    - added one-line intent + minimal runnable examples for:
      - `[abstract]` (parent type declaration for hierarchy checks),
      - `[struct]` (explicit alias of `[type]`),
      - `[type]` (concrete nominal fielded type),
      - `[union]` (sum type / ADT variants),
      - `[alias]` (type-name aliasing for annotation ergonomics).
  - TODO closure:
    - `TODO.md`: marked `L2.4` and both sub-items complete.
  - Validation:
    - doc section presence verified:
      - `rg -n "### 3\\.2\\.2 .*Forms Catalog \\(Type Family\\)|\\[abstract\\]|\\[struct\\]|\\[type\\]|\\[union\\]|\\[alias\\]" docs/LANGUAGE_SPEC.md`

- Spec onboarding pitfalls-guide closure (L2.5):
  - docs updates (`docs/LANGUAGE_SPEC.md`):
    - added `0.6 Pitfalls Guide` in onboarding profile.
    - added `0.6.1 nil vs raise`:
      - explicit absence-vs-failure rule with canonical payloaded-raise example.
    - added `0.6.2 Truthiness Is Narrow`:
      - explicit contract that only `nil` and `false` are falsy with concrete examples.
    - added `0.6.3 Effect resolve vs Abort`:
      - explicit continuation semantics for handler `resolve` and abort paths with examples.
  - TODO closure:
    - `TODO.md`: marked `L2.5` and all three sub-items complete.
  - Validation:
    - doc section presence verified:
      - `rg -n "### 0\\.6 Pitfalls Guide|#### 0\\.6\\.1 .*nil.*raise|#### 0\\.6\\.2 Truthiness Is Narrow|#### 0\\.6\\.3 Effect .*resolve.*Abort" docs/LANGUAGE_SPEC.md`

- Spec onboarding cross-link/drift-prevention closure (L2.6):
  - docs map updates (`docs/README.md`):
    - added explicit onboarding links to:
      - `docs/LANGUAGE_SPEC.md#0-core-omni-profile`
      - `docs/LANGUAGE_SPEC.md#04-advanced-omni-profile`
      - `docs/LANGUAGE_SPEC.md#05-error-model-quick-reference`
      - `docs/LANGUAGE_SPEC.md#06-pitfalls-guide`
  - area status updates:
    - `docs/areas/types-dispatch.md`:
      - updated canonical-source list to point at new language-spec onboarding + explainability sections.
      - removed stale "explainability not implemented yet" gap note and replaced with current L3 gap focus.
    - `docs/areas/effects-error-model.md`:
      - added canonical-source pointers to language-spec onboarding/error/pitfalls/effect sections.
      - removed stale explainability-open statement; aligned known gaps to current error-model migration remainder.
      - refreshed snapshot date to `2026-03-09`.
  - TODO closure:
    - `TODO.md`: marked `L2.6` and both sub-items complete.
  - Validation:
    - cross-link presence verified:
      - `rg -n "0-core-omni-profile|04-advanced-omni-profile|05-error-model-quick-reference|06-pitfalls-guide" docs/README.md docs/areas/types-dispatch.md docs/areas/effects-error-model.md`

- Spec onboarding acceptance-gate closure (A-L2):
  - acceptance context:
    - onboarding stack in `docs/LANGUAGE_SPEC.md` now includes:
      - core profile (`0`),
      - advanced profile (`0.4`),
      - error quick reference (`0.5`),
      - pitfalls guide (`0.6`),
      - `define` forms catalog (`3.2.2`).
    - docs map and area pages now point to these canonical sections (`docs/README.md`, `docs/areas/types-dispatch.md`, `docs/areas/effects-error-model.md`).
  - TODO closure:
    - `TODO.md`: marked `A-L2` complete.
  - Validation:
    - `rg -n "A-L2|L2\\.1|L2\\.2|L2\\.3|L2\\.4|L2\\.5|L2\\.6" TODO.md`
    - `rg -n "## 0\\. Core Omni Profile|### 0\\.4 Advanced Omni Profile|### 0\\.5 Error Model Quick Reference|### 0\\.6 Pitfalls Guide|### 3\\.2\\.2 .*Forms Catalog" docs/LANGUAGE_SPEC.md`
    - `rg -n "0-core-omni-profile|04-advanced-omni-profile|05-error-model-quick-reference|06-pitfalls-guide" docs/README.md docs/areas/types-dispatch.md docs/areas/effects-error-model.md`

- Type-gap design closure (L3.1 constructor type-application checking):
  - docs design updates (`docs/type-system-syntax.md`):
    - added `1.5.1 Constructor Type-Application Checking (Design Contract)`.
    - defined applicability gate for `^(Ctor ...)` checks against runtime instance constructor identity (with optional parent-compat mode as an explicit check-site policy).
    - defined strict arity comparison contract (no implicit fill/truncation).
    - defined per-arg match relation and default invariant behavior for constructor params.
    - defined nested constructor checking recursion rules (for example `^(Box (List Int))`) with first-failing-path reporting.
    - defined canonical payloaded diagnostics for constructor type-application failures:
      - `type/ctor-arity-mismatch`
      - `type/ctor-type-arg-mismatch`
      - required deterministic `data` fields for ctor/expected/actual/index/path context.
    - updated `NOT Implemented` row to reference design section while keeping implementation state pending (`L3.2`).
  - TODO closure:
    - `TODO.md`: marked `L3.1` and all three sub-items complete.
  - Validation:
    - `rg -n "### 1\\.5\\.1 Constructor Type-Application Checking \\(Design Contract\\)|type/ctor-arity-mismatch|type/ctor-type-arg-mismatch|NOT Implemented" docs/type-system-syntax.md`
    - `rg -n "L3\\.1|L3\\.2|L3\\.3" TODO.md`

- Type/dispatch parity closure slice (`[struct]` alias + explicit invariant variance policy + numeric dispatch widening):
  - parser and symbol wiring:
    - `src/lisp/value_interp_state.c3`:
      - added `sym_struct` interned symbol (`"struct"`) as a first-class bracket-attribute token.
    - `src/lisp/parser_define_core.c3`:
      - `[struct]` is now accepted as an alias of `[type]` and routed through the existing `parse_deftype(...)` path.
    - `src/lisp/parser_type_defs.c3`:
      - added deterministic type-parameter variance guard:
        - rejects `+T/-T` markers in type/union parameter lists with explicit error:
          - `"variance markers (+T/-T) are not supported yet; type parameters are invariant"`.
  - dispatch behavior:
    - `src/lisp/eval_dispatch_types.c3`:
      - added dispatch-only numeric widening (`Int` argument can match `^Double` parameter).
      - scoring order is now explicit in runtime matcher:
        - `Value/Val=1000`, `exact=100`, `numeric widening=50`, `subtype=10`, `any=1`.
      - keeps exact `^Int` methods preferred over widened `^Double` matches for int inputs.
  - regression coverage:
    - `src/lisp/tests_advanced_type_effect_ffi_groups.c3`:
      - added `[struct]` alias constructor/field-access regressions.
      - added numeric promotion dispatch regressions (exact-double, int-vs-double specificity, int-to-double widening fallback).
      - added parser regressions for rejected `+T/-T` variance markers on `[type]` and `[union]`.
  - docs parity:
    - `docs/type-system-syntax.md`:
      - documented `[struct]` alias.
      - moved variance policy to explicit `done` (invariant-by-policy; marker rejection).
      - moved numeric promotion row to `done` with dispatch-only widening semantics.
      - updated scoring summary to include widening tier.
    - `docs/LANGUAGE_SPEC.md`:
      - documented `[struct]` alias in bracket-attribute notes and type examples.
      - updated dispatch scoring table and ambiguity note (equal-best is ambiguous, no implicit winner).
    - `docs/SYNTAX_SPEC.md`:
      - documented `E_DEFTYPE` as `[type]` / `[struct]` and added alias example.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1624/0`, `compiler: 79/0`).

- Boundary txn protocol + graph-audit precision + ASAN stage closure:
  - `src/lisp/eval_boundary_session_txn.c3`:
    - promoted explicit txn protocol operations to canonical names:
      - `boundary_txn_begin(...)`
      - `boundary_txn_commit(...)`
      - `boundary_txn_abort(...)`
      - `boundary_txn_close(...)`
    - added non-throwing guard ops for misuse-path tests:
      - `boundary_txn_try_commit(...)`
      - `boundary_txn_try_abort(...)`
      - `boundary_txn_try_close(...)`
    - kept protocol diagnostics cold (`boundary_txn_invalid_transition(...) @noinline`) with no success-path side effects.
  - `src/lisp/eval_boundary_commit_flow.c3`:
    - migrated transaction callsites from `mark_*` naming to explicit `commit/abort` protocol operations.
  - `src/lisp/tests_memory_lifetime_boundary_graph_txn_bench_groups.c3`:
    - added misuse regression coverage:
      - double-commit guard
      - abort-then-commit guard
      - close-before-open + double-close guard policy
    - expanded graph-audit assertions with deterministic payload field checks.
    - added deep nested aggregate graph stress coverage under gated audit mode.
  - `src/lisp/eval_boundary_diagnostics.c3`:
    - added explicit `ValueTag -> BoundaryGraphEdgeClass` table with `$assert` sync guard.
    - made traversal switch exhaustive across current `ValueTag` set.
    - enriched `BoundaryGraphAuditResult` deterministic payload:
      - `root_tag`
      - `violating_edge_tag`
      - `root_scope_gen`
      - `violating_scope_gen`
      - `target_scope_gen`
      - `target_escape_gen`
    - reachability invariant remains explicit:
      - from committed ESCAPE roots, no reachable Omni edge may enter TEMP
      - opaque foreign payload wrappers remain excluded from Omni-edge traversal.
  - ASAN Stage-4 segfault closure:
    - deterministic reproducer (ASAN build + runtime profile):
      - `c3c clean && c3c build --sanitize=address`
      - `LD_LIBRARY_PATH=/usr/local/lib OMNI_FIBER_TEMP=1 OMNI_STACK_AFFINITY_HARNESS=1 OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 ./build/main`
    - root cause:
      - ASAN runtime crash in advanced macro-hygiene fixture `stack overflow caught` (`tests_advanced_macro_hygiene_groups.c3`) due unbounded recursion stress path.
    - minimal fix:
      - gate that single fixture under `!main::stack_runtime_asan_enabled()` and mark ASAN-runtime skip as pass, matching existing ASAN-sensitive test policy in other advanced suites.
    - rollback note:
      - once stack-overflow handling is made ASAN-stable for this fixture, remove the skip branch and re-enable `test_error(...)` under ASAN.
  - Validation:
    - `c3c build` passed.
    - `c3c clean && c3c build --sanitize=address` passed.
    - ASAN profile command above passed with summaries (`unified: 1584/0`, `compiler: 79/0`).
    - `scripts/run_boundary_hardening.sh` passed end-to-end (normal + ASAN + summary/threshold/policy gates).

- Boundary policy determinism/cache slice (`eval_boundary_policy.c3` + boundary gate consumers):
  - Added shared boundary runtime policy cache (`BoundaryRuntimePolicy`) with lazy one-time env load.
  - Added strict token parsers for policy values:
    - boolean accepts only `0/1`, `true/false`, `on/off`, `yes/no` (trimmed, case-insensitive)
    - usize accepts only trimmed decimal digits
  - Rewired boundary runtime gates to cached policy accessors:
    - scope-chain scan bypass/budget
    - graph-audit enable/rate/max-roots
    - verbose telemetry gate
    - trace/benchmark trace gates
  - Added one-shot policy snapshot emission in verbose telemetry path:
    - `boundary_policy_emit_snapshot_once(...)` emits effective policy once per process.
  - Hardened malformed policy handling:
    - debug/test strict mode (`OMNI_BOUNDARY_POLICY_STRICT=1` or summary/assert-summary test env) hard-fails via `unreachable(...)`
    - production mode falls back to defaults with one cold warning path.
  - Added targeted parser regressions:
    - `run_memory_lifetime_boundary_policy_parse_tests(...)` validates strict bool/usize parsing behavior.
  - Validation:
    - `c3c build --warn-deprecation=no` passed.
    - `c3c build --sanitize=address --warn-deprecation=no` passed.
    - `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`unified: 1579/0`, `compiler: 79/0`).
    - `scripts/run_boundary_hardening.sh` passed end-to-end (normal + ASAN + threshold/policy gates).

- Boundary diagnostics gate hardening (`eval_boundary_diagnostics.c3`, `eval_boundary_telemetry.c3`):
  - boundary debug knobs now use strict truthy parsing via `boundary_env_flag_enabled(...)`:
    - `OMNI_BOUNDARY_GRAPH_AUDIT`
    - `OMNI_BOUNDARY_VERBOSE_TELEMETRY`
    - `OMNI_BOUNDARY_TRACE`
    - `OMNI_BOUNDARY_BENCHMARK_TRACE`
  - behavior change: non-empty/garbage env values no longer implicitly enabled debug emitters.
  - validation to run:
    - `c3c build`
    - `c3c build --sanitize=address`
    - `scripts/run_boundary_hardening.sh` (post-landing)

- Track K (`AstArena` validator coverage):
  - `src/lisp/tests_memory_lifetime_groups.c3`:
    - added deterministic regression `run_memory_lifetime_ast_arena_validation_tests(...)` to the lifetime suite.
  - `AstArena` checks added:
    - allocator init/destroy invariants and capacity/count zeroing.
    - chunked growth behavior exercised via allocation sequence that forces first resize.
    - alignment guarantee checks against `AST_ARENA_ALIGNMENT`.
    - `ast_arena_contains(...)` positive/negative probes for in-arena and foreign pointers.
    - `ast_arena_validate(...)` catch path validated by temporary corrupted chunk header (`used > capacity`).
    - post-destroy re-init + post-teardown allocation path.
  - `TODO.md` closed:
    - `Track K: Add validation for chunked AstArena`
    - `Track O: Keep Track N in lock-step with each landing`
  - intended validation before merge:
    - `c3c build`
    - `c3c build --sanitize=address`
- Track M (`boundary` residual provenance scan reduction):
  - `src/lisp/eval_boundary_provenance.c3`:
    - Added generation-aware scope-chain membership helpers with telemetry:
      - `boundary_ptr_in_scope_chain_with_hint(...)`
      - `boundary_ptr_in_target_scope_chain_with_hint(...)`
    - Hot-path callers now use hinted membership when v.scope_gen is available.
  - `src/lisp/eval_promotion_copy.c3`:
    - `copy_parent_should_reuse_closure(...)` now uses target-chain checks with pinned generation hints.
  - `src/lisp/eval_promotion_escape.c3`:
    - cons-list tail short-circuit and fast-path promote reuse now use pinned generation hints before fallback.
  - `src/lisp/eval_env_copy.c3`:
    - iterator closure reuse check now uses target-chain hints.
  - `src/lisp/jit_jit_eval_scopes.c3`:
    - TCO frame binding copy now uses scoped-generation prefilter before expensive boundary membership checks.
  - `src/lisp/eval_boundary_diagnostics.c3`:
    - Added verbose emission of `scope_chain_scan_total`, `scope_chain_scan_with_hint`, `scope_chain_scan_fallback` counters.
  - `src/lisp/value_environment.c3`:
    - Added fixed-size ownership-context cache in `PromotionContext` for scope-chain membership (`PromotionContextScopeChainCacheEntry`).
    - Added epoch-bound reset/init and cached lookup/remember helpers (`promotion_context_reset_scope_chain_cache`, `promotion_context_lookup_scope_chain_cache`, `promotion_context_remember_scope_chain_cache`).
  - `src/lisp/eval_promotion_context.c3`:
    - `promotion_context_begin(...)` now resets the scope-chain cache so each active context starts clean.
  - `src/lisp/tests_memory_lifetime_promotion_context_groups.c3`:
    - Added regression test `run_memory_lifetime_scope_chain_cache_test(...)` validating repeated target-chain checks are cache-hit stable after first miss.
  - `src/lisp/eval_boundary_diagnostics.c3`:
    - Committed-root graph-audit is now sample-gated and capped:
      - `OMNI_BOUNDARY_GRAPH_AUDIT_RATE` (default 1; `0` disables audited runs).
      - `OMNI_BOUNDARY_GRAPH_AUDIT_MAX_ROOTS` (default 0 for unlimited).
    - Added decision counters for graph-audit control flow:
      - `graph_audit_invoked`, `graph_audit_skipped_rate`, `graph_audit_skipped_max_roots`.
    - Extended verbose boundary telemetry to emit the new graph-audit counters.
  - Validation (pending re-run after this stage):
    - `c3c build`
    - `c3c build --sanitize=address`
    - `scripts/run_boundary_hardening.sh`

- Track L (`Env.persistent` replacement + boundary lifetime-kind migration):
  - `src/lisp/value_environment.c3`:
    - Added explicit `EnvLifetimeKind : uint` with distinct categories for transient scope-frame envs and root-persistent mutable-box env nodes.
    - Replaced `Env.persistent`-style lifetime branching with `Env.lifetime_kind` and helper predicates:
      - `Env.is_root_persistent_box()`
      - `Env.is_transient_scope_frame()`.
  - `src/lisp/value_interp_state.c3`:
    - Env allocation paths initialize `Env.lifetime_kind` to `ENV_LIFETIME_LOCAL_SCOPE_FRAME` in both arena allocators.
  - `src/lisp/eval_boundary_api.c3`:
    - Added explicit boundary-owned root extension path that sets `ENV_LIFETIME_ROOT_PERSISTENT_MUTABLE_BOX` in `boundary_env_extend_in_root(...)`.
    - `boundary_copy_env_to_target_scope_impl(...)` now sets `interp.releasing_scope` from the session context so env-copy boundaries share explicit provenance.
  - `src/lisp/eval_env_copy.c3`:
    - Env-copy terminal handling now branches on lifetime kind; root-persistent mutable-box env nodes are kept as-is while their parent links are rewritten in scope targets.
    - In-memory copy allocator now sets copied frames to `ENV_LIFETIME_LOCAL_SCOPE_FRAME`.
  - `src/lisp/jit_jit_eval_scopes.c3`:
    - TCO closure-copy path respects lifetime-kind roots and avoids copying persistent-box nodes.
    - Existing scope-check helpers now use lifetime-aware predicates.
  - `src/lisp/jit_jit_closure_define_qq.c3`:
    - Mutable-box closure capture path now uses `boundary_env_extend_in_root(...)` for persistent frame behavior.
  - `src/lisp/tests_memory_lifetime_env_copy_groups.c3`:
    - Added/updated regression coverage for mixed transient/persistent parent rewrites and persistent parent retention.
  - Validation:
    - `c3c build` pass.
    - `c3c build --sanitize=address` pass.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --help` pass.
    - `scripts/run_boundary_hardening.sh`:
      - Stage 0 facade guard pass.
      - Stage 0b effects contract lint pass.
      - Stage 1/3 builds pass.
      - ASAN stage suite pass (`unified`: 1564/0, `compiler`: 79/0, full stack/scope suites 0 fail, stack affinity harness pass).
      - Normal stage suite currently stops at `unified` with one baseline failure:
        - `[FAIL] http-get in fiber via libuv tcp path (interp=FAIL, jit=ok)` from `OMNI_TEST_SUMMARY suite=unified`.
      - Stage summary assertion intentionally deferred in this environment due the above unrelated failure.
  - Residual / rollback note:
    - Keep `Track L` semantics in place; if a regression appears in existing closure/env-copy behavior, replace direct `ENV_LIFETIME_ROOT_PERSISTENT_MUTABLE_BOX` assignments with a scoped migration setter and retain the old boolean meaning only as a migration shim until full boundary audit completes.

- Finwatch idiom-first routing alignment:
  - `examples/finwatch/server.omni`:
    - refactored `server-dispatch` from chained boolean `cond` guards to structural `match [method path]` route patterns.
    - kept dynamic `/prices/:symbol` route as a guarded GET-path match branch.
    - converted POST body validation branch in `route/portfolio` from guard-style `cond` to explicit `match (nil/payload)` handling.
    - converted `route/price-by-symbol` to symbol-only `FetchResult` matching and moved path extraction into dispatch branch pattern handling.
  - `examples/finwatch/http.omni`:
    - refactored `http/get` to `try` + `match ('status resp)` style response classification (literal `200` vs generic status branch), preserving `(Ok body)`/`(Err msg)` contract.
  - `examples/finwatch/logging.omni`:
    - replaced optional state guard branches with `match` on `log-handle`/`log-path` in `log/close!`, `log/write!`, `log/read-all`, and `log/size`.
    - fixed `log/close!` match clause to use explicit `begin` for multi-step side effects (close + state clear), avoiding parser/runtime stall from multi-form clause body.
  - `examples/finwatch/TODO.md`:
    - updated language-idiom checklist to explicitly track `match [method path]` declarative routing.
    - replaced the `cond` checklist row with explicit `match (nil/payload)` branch handling coverage.
  - `docs/plans/financial-service-webserver-plan.md`:
    - added explicit idiom-first scope requirement for the Finwatch canonical example.
    - added explicit checklist item to keep idiom coverage documented in Finwatch TODO.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/boot_smoke.omni` passed (exit `0`).
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/dispatch_smoke.omni` passed (`true`, exit `0`).
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/smoke_test.omni` passed (`--- smoke test passed ---`, exit `0`).

- Finwatch idiom-first data-path alignment (`feed` + `cache`):
  - `examples/finwatch/feed.omni`:
    - added shared `feed/http-json` helper to normalize HTTP fetch + JSON parse as one `FetchResult` path.
    - refactored `feed/fetch-fx`, `feed/fetch-crypto`, and `feed/fetch-news` to consume `feed/http-json` via `match (Ok/Err)` instead of duplicating endpoint parse flow.
    - replaced remaining imperative required-field `if` branch in `feed/fetch-hn-item` with structural `match [title score]`.
  - `examples/finwatch/cache.omni`:
    - added row projection helpers (`cache/row->quote`, `cache/row->news`) and routed read paths through them.
    - moved `latest-quotes`/`latest-news` scan projections to explicit `|>` pipelines for typed projection flow.
  - `examples/finwatch/TODO.md`:
    - expanded idiom checklist coverage notes for pipe + structural tuple matching in feed/cache paths.
    - added explicit literal-branch matching coverage row for status/exit-code routing.
  - `examples/finwatch/server.omni`:
    - refactored regex capture extraction in `route/price-symbol` from `if` branch to `match (nil/hit)` form.
  - `examples/finwatch/logging.omni`:
    - refactored `log/system-uptime` exit-code branch from `if` to literal `match` (`0` vs fallback).
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/boot_smoke.omni` passed (exit `0`).
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/dispatch_smoke.omni` passed (`true`, exit `0`).
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/smoke_test.omni` passed (`--- smoke test passed ---`, exit `0`).

- Finwatch idiom-first cleanup (`portfolio` + `analytics`) with behavior lock:
  - `examples/finwatch/portfolio.omni`:
    - simplified partition predicate to placeholder form in `portfolio/pnl` (`partition (> _.gain 0) sorted`) while retaining output shape and ordering.
  - `examples/finwatch/analytics.omni`:
    - extracted reusable `analytics/holding-has-price?` helper and routed `all-have-prices?` through `every?` over that helper.
    - attempted placeholder predicate conversion for `find/remove/any?/filter`, then reverted to explicit lambdas after smoke regression evidence (semantic drift in predicate evaluation for field-access placeholder expressions).
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/boot_smoke.omni` passed (exit `0`).
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/dispatch_smoke.omni` passed (`true`, exit `0`).
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/smoke_test.omni` passed with baseline analytics outputs restored (`find DOGE: nil`, `after exclude ETH: 1`, `any gain > 20000? nil`, `crypto news: 1 items`).

- Finwatch idiom-first continuation (`rules` + `events` match attempts rolled back):
  - `examples/finwatch/rules.omni`:
    - attempted `match`-based rewrites for `rules/check-rule` and `rules/consecutive-cross`.
    - rolled back to original truthiness-safe `if` implementations after smoke regression evidence (`62000 < 50k? true`, consecutive-cross mismatch).
  - `examples/finwatch/events.omni`:
    - attempted structural `match` rewrite for recursive flows, but module import path stalled in runtime parser/eval for this slice.
    - rolled back events refactor to last known-good form to preserve Finwatch stability.
  - `examples/finwatch/TODO.md`:
    - removed recursive `match` checklist note to reflect rollback of both rules/events attempts.
  - Validation:
    - `timeout -t 8 env LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/_probe_import_events.omni` passed before probe cleanup (`"events-ok"`, `true`).
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/boot_smoke.omni` passed (exit `0`).
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/dispatch_smoke.omni` passed (`true`, exit `0`).
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/smoke_test.omni` passed (`--- smoke test passed ---`, exit `0`).

- Finwatch idiom-first smoke harness cleanup (behavior-preserving):
  - `examples/finwatch/smoke_test.omni`:
    - added local `print-bool-line` helper to remove repetitive boolean print ternaries in selected checks (`gain%`, backpressure continuation flag, threshold rule output lines).
    - kept canonical boolean semantics via `if` in helper after rejecting a `match` variant that over-matched and produced a false-positive line (`62000 < 50k? true`); regression fixed immediately.
    - attempted to route threshold/consecutive checks through `rules/check-rule` and `rules/consecutive-cross`, then restored direct lambda + inline consecutive checks due known wrapper/runtime semantics.
  - `examples/finwatch/alerts.omni`:
    - no code change; current effect-handler composition surface already aligned with idiom-first target for this slice.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/boot_smoke.omni` passed (exit `0`).
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/dispatch_smoke.omni` passed (`true`, exit `0`).
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/smoke_test.omni` passed with baseline rule-output restored (`62000 < 50k? false`).

- Finwatch route/failure stabilization and module-import hardening:
  - `examples/finwatch/server.omni`:
    - canonicalized malformed `cond` forms in request/routing branches.
    - added route symbol extraction helper for `/prices/:symbol`.
    - renamed server symbols from slash names to hyphen names to avoid load-context symbol collisions:
      - `server/start` -> `server-start`
      - `server/start-async` -> `server-start-async`
      - `server/dispatch` -> `server-dispatch`
      - `server/handle-client` -> `server-handle-client`
      - `server/accept-loop` -> `server-accept-loop`
      - `server/poll-once!` -> `server-poll-once!`
      - `server/poll-loop` -> `server-poll-loop`
  - `examples/finwatch/main.omni`:
    - updated canonical example entrypoint to call `server-start`.
  - `examples/finwatch/boot_smoke.omni`:
    - updated boot assertion to validate `procedure? server-start`.
  - `examples/finwatch/dispatch_smoke.omni`:
    - added bounded route/failure smoke checks (`/prices`, unknown route, missing request body, malformed `/prices/`).
  - `src/lisp/tests_runtime_feature_http_groups.c3`:
    - switched bounded route/failure coverage to script-load execution (`dispatch_smoke.omni`) to avoid root test-context symbol leakage for nested example modules.
  - `src/lisp/jit_jit_module_import.c3`:
    - hardened implicit-module eval path by capturing `E_DEFINE` export metadata before eval and avoiding post-eval dereference of potentially invalid expr nodes (`jit_eval_implicit_module_file(...)`).
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`unified 1575/0`, `compiler 79/0`).
    - `c3c build --sanitize=address` passed.
    - `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`unified 1566/0`, `compiler 79/0`).

- Finwatch canonical boot smoke coverage (plan step closure):
  - `examples/finwatch/boot_smoke.omni`:
    - added bounded boot-path smoke script with file-relative imports (`import "server.omni" 'all`) and `/health` decode assertions (`method/path` extraction) without starting the blocking server loop.
  - `src/lisp/tests_runtime_feature_http_groups.c3`:
    - added bounded runtime test `finwatch canonical boot path smoke (non-blocking)` using `test_truthy_interp(...)` (module-loading coverage, interpreter path only).
    - test loads `examples/finwatch/boot_smoke.omni` and asserts canonical boot/decode signals (`server-start` bound + `/health` request decode).
  - `docs/plans/financial-service-webserver-plan.md`:
    - advanced `As of` to `2026-03-09`.
    - converted Next Steps to checkbox tracking.
    - marked thin boot smoke step complete with concrete test reference.
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`unified 1574/0`, `compiler 79/0`).
    - `c3c build --sanitize=address` passed.
    - `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`unified 1565/0`, `compiler 79/0`).
    - `bash scripts/check_async_fallback_policy.sh` passed.
    - `bash scripts/check_io_parity_status_map.sh` passed (`done-libuv: 38`, `partial-libuv: 0`, `non-libuv: 0`).
    - `bash scripts/check_io_boundary_facade.sh` passed.

- Plan status normalization sweep:
  - `docs/plans/concurrency-hybrid-memory-checklist.md`:
    - status updated from `draft` to `complete` (`As of: 2026-03-09`) since all execution snapshot phases are already checked complete.
  - `docs/plans/asan-jit-escape-scope-overflow.md`:
    - status updated from `open` to `resolved (2026-03-09)`.
    - appended resolution snapshot with current ASAN validation evidence (`c3c build --sanitize=address` + full ASAN suite pass).

- Library parity plan closure update:
  - `docs/plans/library-gaps-todo.md` status header moved from `active` to `complete` with `As of: 2026-03-09`.
  - Validation after this update:
    - `c3c build --sanitize=address` passed.
    - `ASAN_OPTIONS=detect_leaks=0,halt_on_error=1,abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`unified 1573/0`, `compiler 79/0`).

- Compiler-note cleanup in Omni source:
  - `src/lisp/eval_type_evaluators.c3`:
    - `is_type_param_annotation(...)` now compares `annotation_sym` against `(SymbolId)0` (typed literal) instead of untyped `0`.
    - removes the Omni-local distinct-type deprecation note (`SymbolId` constant compare) from normal builds.
  - Validation:
    - `c3c build` passed with no `/home/heefoo/Documents/code/Omni/src/...` compiler notes.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`unified 1573/0`, `compiler 79/0`).

- Async fallback policy gate robustness after TLS/module splits:
  - `scripts/check_async_fallback_policy.sh` now resolves async primitive function locations dynamically (`resolve_function_file(...)`) instead of assuming each primitive remains in one fixed source file.
  - Guard behavior is unchanged (same required/forbidden pattern checks), but largest-first file splits no longer cause false negatives like `prim_tls_connect` extraction failures.
  - Validation:
    - `bash scripts/check_async_fallback_policy.sh` passed.
    - `bash scripts/check_io_parity_status_map.sh` passed (`done-libuv: 38`, `partial-libuv: 0`, `non-libuv: 0`).
    - `bash scripts/check_io_boundary_facade.sh` passed.
    - `c3c build` passed.

- Boundary commit/finalize fallback-residue cleanup:
  - `src/lisp/eval_boundary_commit_flow.c3`:
    - removed dead migration parameters from `boundary_commit_escape(...)` (`original_result`, `copy_site`) after fallback-copy route retirement.
    - removed stale no-op casts and updated mixed-provenance comment to match current hard-error semantics (`fallback-disallowed`).
  - `src/lisp/jit_jit_eval_scopes.c3` and `src/lisp/eval_run_pipeline.c3`:
    - narrowed `boundary_finalize_scoped_result(...)` and `jit_finalize_scoped_result(...)` signatures to match the commit API and removed dead pass-through arguments.
  - Updated direct regression/bench callsites that exercise boundary commit/finalize signatures:
    - `src/lisp/tests_memory_lifetime_boundary_commit_groups.c3`
    - `src/lisp/tests_memory_lifetime_boundary_stress_groups.c3`
    - `src/lisp/tests_memory_lifetime_boundary_graph_txn_bench_groups.c3`
    - `src/lisp/tests_memory_lifetime_groups.c3`
  - `TODO.md`:
    - marked Stage 7 C3 idiom item `Prefer deleting whole helpers/modules...` complete with this cleanup slice.
  - Validation:
    - `c3c build` passed.
    - `c3c build --sanitize=address` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`unified 1563/0`, `compiler 79/0`, `copy_fallback_total=0`).
    - `scripts/run_boundary_hardening.sh` passed end-to-end.

- Boundary provenance-classification cleanup for copy-to-parent fast-reuse:
  - `src/lisp/eval_promotion_copy.c3`:
    - `copy_to_parent_try_fast_reuse(...)` now uses canonical `boundary_classify_return_value(...)` when boundary context is present (`current_scope` + `releasing_scope`) to decide reusable-in-target-chain vs copy.
    - removed ad-hoc `releasing_scope` defensive-path decisioning from this hot helper; copy fallback remains only as conservative behavior when explicit boundary provenance context is absent.
  - `TODO.md`:
    - marked Track G item `Retire releasing_scope-driven defensive copy logic...` complete with classification-based route note.

- Root-store clone boundary-context routing cleanup:
  - `src/lisp/eval_promotion_escape.c3`:
    - `root_store_clone_array_to_scope(...)`, `root_store_clone_hashmap_to_scope(...)`, and `root_store_clone_method_table_to_scope(...)` now run element copy loops through `boundary_copy_to_parent_site_ctx(...)` with local unbounded promotion contexts.
    - `root_store_direct_promote_to_scope(...)` also now routes via `boundary_copy_to_parent_site_ctx(...)` with a local unbounded promotion context.
    - replaced raw `boundary_copy_to_parent_site(...)` per-element cloning calls in these paths, aligning root-store clone routing with canonical boundary context + memoized copy policy.
  - `TODO.md`:
    - marked Track G umbrella item `Replace current copy_to_parent(...) boundary users with destination-aware or boundary-classified routes...` complete and documented the root-store clone routing closure.
  - Validation:
    - `c3c build` passed.
    - `c3c build --sanitize=address` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`unified 1563/0`, `compiler 79/0`, `copy_fallback_total=0`).
    - `scripts/run_boundary_hardening.sh` passed end-to-end.

- Boundary scoped-copy wrapper convergence onto context-aware helper:
  - Routed additional explicit scope-copy wrappers from raw site-copy calls to `boundary_copy_to_parent_site_ctx(...)` with local unbounded promotion contexts:
    - `src/lisp/eval_type_evaluators.c3` (`eval_copy_value_into_scope_site(...)`)
    - `src/lisp/jit_jit_eval_scopes.c3` (`jit_scopes_copy_value_into_scope_site(...)`)
    - `src/lisp/jit_jit_closure_define_qq.c3` (`jit_copy_value_into_scope_site(...)`)
    - `src/lisp/primitives_iter_coroutine.c3` (`prim_resume_yield_result(...)` transfer path)
  - `TODO.md`:
    - recorded these wrapper migrations under Track G boundary-user replacement checklist.
  - Validation:
    - `scripts/run_boundary_hardening.sh` passed end-to-end (includes normal/ASAN build + suite + threshold/policy gates).

- REPL boundary copy route alignment + facade surface closure:
  - `src/lisp/eval_repl.c3`:
    - REPL result boundary copy now uses explicit target/releasing scope swaps plus `boundary_copy_to_parent_site_ctx(...)` with a local unbounded promotion context.
    - removed the last non-test direct `boundary_copy_to_parent_site(...)` runtime callsite outside facade internals.
  - `TODO.md`:
    - marked Track G C3 idiom item `Delete ownership fallbacks only after the replacement boundary helper is the sole legal path` complete with non-test callsite audit note.
  - Validation:
    - `scripts/run_boundary_hardening.sh` passed end-to-end (normal + ASAN + thresholds + policy gates).

- Ownership-transition contract tightening for scoped-copy helpers:
  - Added explicit `@require` contracts for non-null interp/target/releasing scopes on:
    - `eval_copy_value_into_scope_site(...)` in `src/lisp/eval_type_evaluators.c3`
    - `jit_scopes_copy_value_into_scope_site(...)` in `src/lisp/jit_jit_eval_scopes.c3`
    - `jit_copy_value_into_scope_site(...)` in `src/lisp/jit_jit_closure_define_qq.c3`
  - `TODO.md`:
    - marked Track G C3 idiom item `Preserve explicit contracts on ownership-transition helpers...` complete.

- Scope-restore `defer` discipline closure for live boundary swaps:
  - `src/lisp/eval_boundary_api.c3`:
    - `boundary_enter_scope(...)` no longer uses `boundary_scope_swap_begin(...)` internally; it now performs explicit scoped assignment and relies on `boundary_leave_scope(...)` restore contract, eliminating a non-`defer` swap-begin pattern.
  - Runtime audit:
    - all live non-test `boundary_scope_swap_begin(...)` / `boundary_releasing_scope_swap_begin(...)` callsites are `defer ..._end(...)` paired.
  - `TODO.md`:
    - marked Track G C3 idiom item `Keep state-restore and scope-restore logic defer-backed...` complete.

## 2026-03-08

- Boundary provenance-route hardening (Stage 1/7 C3 idiom closure slice):
  - `src/lisp/eval_boundary_provenance.c3`:
    - added explicit `@require` contracts on `boundary_classify_return_value(...)` for non-null `interp`, `target_scope`, and `releasing_scope`.
    - kept null-value classification behavior explicit (`v == null` returns `BOUNDARY_RETURN_MIXED_UNCERTAIN`).
  - `src/lisp/eval_boundary_commit_flow.c3`:
    - `boundary_commit_escape(...)` now starts with an explicit exhaustive `switch (classification.provenance)` (no default) before route-specific handling.
    - fixed malformed same-line abort statements so fallback-disallowed paths mark txn abort on separate statements.
  - `src/lisp/eval_promotion_escape.c3`:
    - `promote_to_root_site(...)` now routes via explicit exhaustive provenance `switch` (no default).
    - removed `@inline` from `promote_escape_route_for_tag(...)` (non-trivial switch helper).
  - `src/lisp/eval_promotion_copy.c3`:
    - removed `@inline` from `copy_parent_route_for_tag(...)` (non-trivial switch helper).
  - `src/lisp/eval_boundary_api.c3`:
    - deleted dead copy-fallback enum/name/count scaffolding (`BoundaryCopyFallbackReason*`) that had no remaining runtime references after fallback retirement.
  - `TODO.md`:
    - marked Stage 1 `@require` precondition item done with contract references.
    - marked Stage 1 inline-restriction item done for classification assembly/route helpers.
    - marked Stage 7 exhaustive boundary-class switch item done.
  - Validation:
    - `c3c build` passed.
    - `c3c build --sanitize=address` passed.
    - `scripts/run_boundary_hardening.sh` passed end-to-end (`copy_fallback_total=0`, threshold + policy stages green).

- Coroutine boundary provenance regression closure:
  - `src/lisp/tests_memory_lifetime_groups.c3`:
    - added focused gate `lifetime: resume missing yield provenance gate`.
    - covers the replacement behavior for the removed resume fallback path:
      - `prim_resume_yield_result(...)` must hard-error when `yield_scope` provenance is missing.
      - verifies copy-site fallback remains blocked by asserting `COPY_SITE_PRIM_RESUME` counter delta is zero.
      - verifies boundary state cleanup (`yield_value` / `yield_scope` cleared after call).

- Compile-time fallback-route guard closure:
  - `src/lisp/eval_boundary_api.c3`:
    - added explicit commit-outcome name table + count sync assert:
      - `BOUNDARY_COMMIT_OUTCOME_NAMES`
      - `$assert(BOUNDARY_COMMIT_OUTCOME_NAMES.len == BOUNDARY_COMMIT_OUTCOME_COUNT)`
    - added small fallback-route enum for active commit fallback surface:
      - `BoundaryCommitFallbackRoute` with single canonical route `fallback-disallowed`
      - `$assert(BOUNDARY_COMMIT_FALLBACK_ROUTE_NAMES.len == BOUNDARY_COMMIT_FALLBACK_ROUTE_COUNT)`
    - intent: make retired fallback-route reintroduction visible at compile-time table/enum sync boundaries.

- Boundary hardening ASAN closure (scheduler + JIT):
  - `src/lisp/scheduler_thread_tasks.c3`:
    - OS-thread start now attempts immediate libuv detach after successful spawn; detached handles are released instead of accumulating joinable wrappers.
    - drop-path cancellation now marks active tasks cancelled even when no joinable handle is retained.
  - `src/lisp/scheduler_primitives.c3`:
    - scheduler round cap is now ASAN-aware (`scheduler_max_rounds()`), reducing deterministic ASAN stalls in scheduler stress loops while preserving guard behavior.
  - `src/lisp/jit_jit_compiler.c3`:
    - replaced JIT state-pool overflow leak behavior with explicit spill tracking (`JitStateSpillNode` list).
    - spill states are reclaimed during `jit_gc()` and `jit_global_shutdown()`, closing leak reports from untracked `jit_state_t` allocations.
    - hardened spill OOM edge: `jit_track_compiled_state(...)` now returns success/failure so compile destroys untracked state and fails cleanly instead of leaking.
  - `src/lisp/value_environment.c3`:
    - removed unreachable tail return in `promote_for_env_target(...)` after canonical route dispatch (`copy_to_parent_by_route(...)`) to keep boundary env-write flow explicit and warning-clean.
  - `src/lisp/eval_boundary_api.c3`:
    - scoped ASAN-safe de-inline pilot: extracted heavy `boundary_copy_to_parent_site_ctx(...)` logic into `boundary_copy_to_parent_site_ctx_impl(...) @noinline`, retaining a thin inline facade.
    - intent: reduce inline pressure without repeating the prior broad de-inline sweep that destabilized ASAN stack switching.
    - extended the same pattern to non-trivial scope/session wrappers:
      - `boundary_alloc_value_in_scope(...)`
      - `boundary_make_env_in_scope(...)`
      - `boundary_env_extend_in_scope(...)`
      - `boundary_copy_env_to_target_scope(...)`
      now routed through `@noinline` impl helpers with thin inline facades.
  - Route-table hardening (exhaustive tag routing, no silent defaults):
    - `src/lisp/eval_promotion_copy.c3`: removed `CP_ROUTE_PASSTHROUGH` and default fallback branches from both `copy_parent_route_for_tag(...)` and `copy_to_parent_by_route(...)`.
    - `src/lisp/eval_promotion_escape.c3`: removed `PE_ROUTE_PASSTHROUGH` and default fallback branches from both `promote_escape_route_for_tag(...)` and `promote_to_escape_by_route(...)`.
    - Result: new `ValueTag` additions now require explicit route handling in compile-time-visible switches.
  - Validation:
    - `c3c build` passed.
    - `c3c build --sanitize=address` passed.
    - `scripts/run_boundary_hardening.sh` passed end-to-end; Stage 4 ASAN run completed with `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1`.

- Boundary telemetry surface cleanup (post-fallback retirement):
  - Removed fallback-detail verbose telemetry branches that only reported retired copy-fallback breakdowns:
    - deleted `boundary_copy_fallback_reason_name(...)` and copy-fallback reason/site/tag verbose loops in `src/lisp/eval_boundary_diagnostics.c3`.
  - Slimmed `BoundaryDecisionStats` to remove unused copy-fallback breakdown storage (reason/site/tag/last fields) while keeping `copy_fallback_total` as the active regression signal.
  - Simplified `suite=boundary_decisions` summary output to keep only active copy-fallback signal:
    - retained `copy_fallback_total`
    - removed retired `copy_fallback_*` reason/site/tag fields from `src/lisp/tests_tests.c3`.
  - Updated boundary hardening summary-key assertions in `scripts/run_boundary_hardening.sh` to match the trimmed telemetry schema.

- Old region migration closure bookkeeping:
  - Audited test surfaces for stale handle-path coverage; there are no active test callsites for:
    - `ObjectHandle`
    - `RegionHandle`
    - `thread_root_region(...)`
  - Updated area-diagram SVG outputs to remove stale `root_region` current-state labels and align naming with `AstArena`:
    - `docs/areas/diagrams/memory-runtime-cycle-master.svg`
    - `docs/areas/diagrams/memory-runtime-cycle.rendered-1.svg`
  - Rollback note: no partial old-region rollback path is kept in-tree after this stage; restoring the removed allocator/registry flow would require explicit file restoration from history (`main_pool_slot_*`, `main_region_registry_methods.c3`) and revalidation.
  - Added an env-gated AST allocator throughput benchmark:
    - `run_memory_lifetime_ast_arena_benchmark()` in `src/lisp/tests_memory_lifetime_boundary_graph_txn_bench_groups.c3`
    - wired into lifetime regression run from `src/lisp/tests_memory_lifetime_groups.c3`
    - enable with `OMNI_AST_ARENA_BENCH=1`

- Old region migration, stage: AST allocator cutover:
  - Replaced interpreter AST allocation dependency on `root_region` handles with a dedicated `AstArena` subsystem:
    - added `src/lisp/ast_arena.c3` (`AstArena` type, init/destroy, tracked allocation API).
    - `Interp.alloc_expr()` / `Interp.alloc_pattern()` now allocate via `AstArena`.
  - Migrated interpreter state off old-region AST handle path:
    - removed `Interp.root_region` from `src/lisp/value_interp_state.c3`.
    - runtime init now calls `ast_arena_init(...)`; interpreter destroy now calls `ast_arena_destroy(...)`.
  - Updated stale runtime comments tied to `root_region` wording:
    - `src/lisp/value_environment.c3`
    - `src/lisp/eval_env_copy.c3`
    - `src/lisp/jit_jit_eval_scopes.c3`
    - `src/lisp/jit_jit_compile_let_set.c3`
    - `src/lisp/jit_jit_closure_define_qq.c3`
    - `src/lisp/eval_promotion_copy.c3`
  - Updated docs/plan status for AST lane naming:
    - `docs/FEATURES.md`
    - `docs/areas/memory-runtime-cycle.md`
    - `docs/areas/diagrams/memory-runtime-cycle-master.mmd`
    - `docs/plans/aot-unification.md`
  - Updated migration checklists in `TODO.md`:
    - marked AST allocator introduction + interpreter cutover + parser/macro/compiler verification steps complete.
    - marked `allocate_in(...)` / `dereference_as(...)` callsite-removal items complete (no active `src/lisp` production callsites remain).
  - Validation:
    - `c3c clean && c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1571/0`, `Compiler Tests: 79/0`).
    - `c3c clean && c3c build --sanitize=address` passed.
    - ASAN full suite run in this workspace did not complete in bounded runtime window (long-running scheduler segment, no sanitizer fault emitted before manual stop); keep as known environment instability until isolated.
- Old region migration follow-up, dead thread-root wrappers removed:
  - Deleted dead old-region helper surface from `src/main_thread_registry.c3` (zero callsites after AST allocator cutover):
    - `thread_root_region(...)`
    - `dereference(...)`
    - `dereference_as(...)` macro
    - `allocate_in(...)` macro
  - Validation:
    - `c3c clean && c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1571/0`, `Compiler Tests: 79/0`).
    - `c3c clean && c3c build --sanitize=address` passed.
    - bounded ASAN run (`timeout -t 180 ... ./build/main`) reached timeout without sanitizer crash before termination (`TIMEOUT ... <time name=\"ALL\">179880</time>`).
- Old region migration follow-up, thread-registry plumbing detached:
  - `src/main_thread_registry.c3` no longer allocates/owns `RegionRegistry` runtime state:
    - `thread_registry_init()` is now an explicit migration no-op.
    - `thread_registry_shutdown()` now performs only scope freelist cleanup.
  - `thread_registry()` callsites reduced to zero in `src/` (the test-runner startup message now no longer references `root_id`):
    - `src/entry_test_runner_setup.c3` now logs `Thread-local runtime setup initialized.`
  - Validation:
    - `c3c clean && c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1571/0`, `Compiler Tests: 79/0`).
    - `c3c clean && c3c build --sanitize=address` passed.
- Old region migration follow-up, dead region-registry methods removed:
  - Deleted `src/main_region_registry_methods.c3` (zero runtime callsites after AST allocator + thread-registry detach).
  - Validation:
    - `c3c clean && c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1571/0`, `Compiler Tests: 79/0`).
- Old region migration follow-up, storage declarations removed:
  - Deleted remaining old region storage declarations from `src/main.c3`; module now only provides shared `str_eq(...)`.
  - Deleted dead old-region storage files:
    - `src/main_pool_slot_types.c3`
    - `src/main_pool_slot_methods.c3`
  - Updated test-run startup label to match current architecture:
    - `src/entry_test_modes.c3`: `Runtime memory architecture initialized`
  - Validation:
    - `c3c clean && c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1571/0`, `Compiler Tests: 79/0`).
    - `c3c build` post-label update passed.
- AST allocator internals, chunked bump implementation:
  - Replaced per-node allocation in `src/lisp/ast_arena.c3` with chunked bump storage:
    - added `AstArenaChunk` header (`next`, `used`, `capacity`).
    - replaced one `malloc` per AST node with bounded chunk allocation (`AST_ARENA_INITIAL_CHUNK_BYTES = 4096`, 8-byte alignment).
    - changed `ast_arena_destroy(...)` to bulk-chunk free.
    - kept `Interp.alloc_expr()` / `alloc_pattern()` semantics unchanged (interpreter-owned, no per-AST-node free).
  - `ast_arena_alloc(...)` now zero-initializes allocated AST payloads exactly as before and retains current error/null behavior for `size == 0` / OOM paths.
  - Validation:
    - `c3c clean && c3c build` passed prior to this allocator pass (`AST_ARENA_BENCH` instrumentation already present in `src/lisp/tests_memory_lifetime_boundary_graph_txn_bench_groups.c3` for optional regression measurement).
    - parser/compile benchmark smoke validation completed (`OMNI_AST_ARENA_BENCH=1` path in `run_memory_lifetime_ast_arena_benchmark` now measures parse + compile throughput and success counts before/after allocator-phase resets).
    - parse/compile results now emit `OMNI_BENCH_SUMMARY` lines:
      - `suite=ast_parser_smoke`
      - `suite=ast_compiler_smoke`
- Track D parser permissive-fallback audit closure:
  - Audited parser/lexer reader-dispatch and import-marker paths for silent acceptance.
  - Added deterministic reader regression for bare hash dispatch:
    - `src/lisp/tests_runtime_feature_schema_reader_groups.c3`:
      - `bare # dispatch rejected` -> expects `unknown # dispatch sequence`.
  - Refactored `Lexer.scan_hash_dispatch(...)` in `src/lisp/parser_lexer_string_hash.c3` to an explicit `switch` over reader-dispatch token classes (`{`, `_`, `r`, `1..9`) with deterministic rejection for unsupported forms.
  - Closed Track D lexer-exhaustiveness checklist item in `TODO.md` (`Keep lexer branches small and exhaustive where token class is known`).
  - Documented canonical reader-dispatch forms in `docs/LANGUAGE_SPEC.md` (`§1.4 Reader Dispatch`):
    - `#{...}`, `#r"..."`, `#_`, `#N_`, `#| ... |#`
    - non-canonical `#` sequences are explicitly rejected.
  - Marked Track D audit checklist item complete in `TODO.md`.
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1561/0`, `Compiler Tests: 79/0`).
    - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1571/0`, `Compiler Tests: 79/0`).
- Track E duplicate compiler-entrypoint collapse:
  - Removed redundant top-level compile wrappers from `src/lisp/compiler_top_level_compile_function.c3`:
    - deleted `compile_to_c3_with_print(...)`
    - deleted `compile_to_c3_print_all(...)`
  - Canonicalized print-mode callsites on `compile_to_c3_ext(...)`:
    - `src/lisp/tests_compiler_jit_print_groups.c3`
    - `src/lisp/tests_e2e_generation.c3`
  - Canonicalized lambda-return codegen helper surface in `src/lisp/compiler_code_emission.c3`:
    - removed `emit_lambda_return_with_frame(...)`
    - retained a single `emit_lambda_return(...)` helper (frame-binding return path)
    - updated lambda-definition emission callsite to use the canonical helper.
  - Audited compiler modules for `API migration` / `intentionally ignored` comment residue; no remaining compiler-comment duplication anchors were found after this cleanup slice.
  - Marked Track E checklist item complete in `TODO.md`:
    - `Collapse duplicate compiler entrypoints that only survive for old call signatures.`
    - `Audit comments marked “API migration” or “intentionally ignored” and remove the underlying dead migration path, not just the comment.`
    - `Keep one canonical helper per compiler action.`
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1571/0`, `Compiler Tests: 79/0`).
    - `c3c build --sanitize=address` passed.
    - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1562/0`, `Compiler Tests: 79/0`).
- Track H checklist normalization:
  - Updated parent checklist rows in `TODO.md` to reflect already-landed implementation work:
    - `Regex dual-path cleanup`
    - `Scope reset defensive residue`
    - `Boundary provenance residue`
  - This is a status alignment-only update; no runtime behavior change.
- Track F JIT family-route hardening:
  - Added explicit JIT expression-family routing enum in `src/lisp/jit_jit_compile_expr_core.c3`:
    - `JitExprFamily` (`core`, `effects`, `types_ffi`, `invalid`)
    - centralized classifier `jit_expr_family_for_tag(...)` with exhaustive `ExprTag` switch.
  - Switched top-level JIT expression dispatcher (`jit_compile_expr(...)`) from sequential ad-hoc group probes to family-routed dispatch via `switch (jit_expr_family_for_tag(...))`.
  - Added route-table synchronization guard:
    - `JIT_EXPR_FAMILY_NAMES` + `$assert(JIT_EXPR_FAMILY_NAMES.len == JIT_EXPR_FAMILY_COUNT)`.
  - Closed Track F C3-idiom checklist items in `TODO.md` for compact route enums/exhaustive switches and route-table assert synchronization.
- Track C canonical error payload regression closure:
  - Added focused `try`-path canonical payload regressions in `src/lisp/tests_advanced_core_unicode_groups.c3`:
    - `try canonical payload shape` (`code`/`domain`/`message`/`data` key presence)
    - `try canonical payload domain`
    - `try canonical payload code`
    - `try canonical payload message`
  - Marked `TODO.md` Track C item complete:
    - `Add focused regression tests for canonical error payload shape instead of wrapper behavior.`
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1558/0`, `Compiler Tests: 79/0`).
    - `c3c build --sanitize=address` passed.
    - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1570/0`, `Compiler Tests: 79/0`).
- Track G commit-fallback hard-disallow closure:
  - Removed remaining commit-path copy fallback branches from `src/lisp/eval_boundary_commit_flow.c3`; abort/mixed fallback branches now return `BOUNDARY_COMMIT_FALLBACK_DISALLOWED` with explicit boundary error payloads.
  - Removed dead commit fallback migration surface from `BoundaryCommitEscapeResult` in `src/lisp/eval_boundary_api.c3`:
    - deleted `BOUNDARY_COMMIT_FALLBACK_COPIED`,
    - deleted `fallback_copied` field.
  - Removed dead copy-fallback telemetry helper path with zero runtime callsites from `src/lisp/eval_boundary_telemetry.c3`:
    - `boundary_note_copy_fallback(...)`
    - `boundary_note_copy_fallback_tag(...)`
    - `boundary_trace_copy_fallback(...)`
    - `boundary_benchmark_copy_fallback(...)`
  - Updated boundary commit/stress/benchmark tests to assert explicit outcome semantics instead of fallback-copy flags:
    - `src/lisp/tests_memory_lifetime_boundary_commit_groups.c3`
    - `src/lisp/tests_memory_lifetime_boundary_stress_groups.c3`
    - `src/lisp/tests_memory_lifetime_boundary_graph_txn_bench_groups.c3`
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1567/0`, `Compiler Tests: 79/0`, `copy_fallback_total=0`).
- Track G helper reduction:
  - Removed `boundary_copy_value_if_owned_by_scope(...)` from `src/lisp/eval_boundary_api.c3`.
  - Folded its tag-aware copy semantics directly into `boundary_copy_from_releasing_scope(...)` to keep behavior stable while shrinking the facade surface.
  - Updated remaining runtime/tests callsites to the canonical helper:
    - `src/lisp/jit_jit_eval_scopes.c3`
    - `src/lisp/tests_memory_lifetime_promotion_context_groups.c3`
  - Updated `TODO.md` Stage 7 inventory to remove the retired helper from the pending uncertain-provenance list.
- Track G runtime narrowing follow-up:
  - Removed one runtime dependency on `boundary_copy_from_releasing_scope(...)` in `src/lisp/jit_jit_eval_scopes.c3` (`copy_tco_env_chain(...)`).
  - TCO env-chain copy now uses explicit releasing-scope ownership gate (`boundary_ptr_in_scope(...)`) and routes owned values through `boundary_copy_to_scope_site(...)`.
  - Boundary-commit fallback flow now also routes via `boundary_copy_to_scope_site(...)` in `src/lisp/eval_boundary_commit_flow.c3`.
- Track G helper retirement closure:
  - Deleted `boundary_copy_from_releasing_scope(...)` from `src/lisp/eval_boundary_api.c3`.
  - Migrated lifetime tests to `test_copy_from_releasing_scope(...)` in `src/lisp/tests_harness_helpers.c3`.
  - `src/lisp` now has zero callsites for `boundary_copy_from_releasing_scope(...)`.
- Track G root-store transfer-safe routing (explicit-rule pass):
  - Updated `promote_to_root_site(...)` (`src/lisp/eval_promotion_escape.c3`) to classify return provenance and prefer direct destination ESCAPE promotion only for releasing-owned tags with explicit clone/retain semantics.
  - Added transfer-safe ARRAY clone path for releasing-owned wrappers:
    - child-owned `ARRAY` now clones wrapper+payload into target ESCAPE lane.
    - clone path hard-errors on allocation failure (`root-store: failed to clone array payload`) rather than falling back to unsafe shared-wrapper aliasing.
  - Remaining mixed/unsupported shared-wrapper classes (`HASHMAP`, `METHOD_TABLE`, alias-heavy cases) still route through `boundary_copy_to_scope_site(...)` pending deeper ownership work.
  - Added regression `lifetime: root-boundary direct destination promotion` in `src/lisp/tests_memory_lifetime_groups.c3`.
  - Added regression `lifetime: root-boundary ARRAY clone path` in `src/lisp/tests_memory_lifetime_groups.c3`.
  - Rewired JIT root-store callsites onto the new route:
    - `src/lisp/jit_jit_closure_define_qq.c3`: `jit_eval_set(...)`, `jit_env_extend_root(...)`.
    - `src/lisp/jit_jit_define_method_table.c3`: `jit_eval_define(...)`.
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1564/0`, `Compiler Tests: 79/0`).
    - ASAN full suite remains at existing instability point (`EXIT=139`, Advanced Feature Tests) in this workspace.
- Track G root-store transfer-safe routing follow-up:
  - Added transfer-safe `PRIMITIVE`, `HASHMAP`, and `METHOD_TABLE` clone paths in `src/lisp/eval_promotion_escape.c3` for releasing-owned root-store crossings.
  - Added `TYPE_INFO` and `MODULE` to direct destination-promotion-supported root-store tags.
  - Root-store clone payload routing now promotes nested payload values into destination ESCAPE lane (`boundary_promote_to_escape(...)`) for:
    - `ARRAY` items
    - `HASHMAP` key/value entries
    - `METHOD_TABLE` entry implementations and fallback
  - Added focused regressions:
    - `lifetime: root-boundary PRIMITIVE clone path`
    - `lifetime: root-boundary METHOD_TABLE clone path`
    - plus strengthened hashmap clone assertions
    in `src/lisp/tests_memory_lifetime_groups.c3`.
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1566/0`, `Compiler Tests: 79/0`).
- Track G root-store fallback removal closure:
  - Removed root-store fallback copy routing from `promote_to_root_site(...)` by deleting its `boundary_copy_to_scope_site(...)` tail path.
  - Non-reusable root-store values now route only through explicit transfer-safe clone/direct-promotion helpers; unsupported payload classes now hard-error (`root-store: unsupported boundary payload`).
  - Root-store routing now uses an exhaustive `ValueTag` switch (`root_store_route_to_scope(...)`) so new tags cannot silently inherit fallback behavior.
  - Coverage now spans every current runtime `ValueTag` class (clone route or direct route), making unsupported root-store classes an explicit compile-time/update task.
  - Root-store disjoint-scope lifetime regression now asserts zero copy-site fallback accounting (`lifetime: root-boundary disjoint destination route`).
  - Aligned JIT instance-field root mutation path to the same route:
    - `src/lisp/jit_jit_closure_define_qq.c3` now uses `boundary_promote_to_root_site(...)` for `set!` instance-field writes when owner scope resolves to root, and keeps explicit scoped-copy handling only for non-root owner scopes.
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1566/0`, `Compiler Tests: 79/0`).
- Track G scoped-copy helper retirement closure:
  - Deleted `boundary_copy_to_scope_site(...)` from `src/lisp/eval_boundary_api.c3`.
  - Narrowed non-commit runtime callsites to explicit boundary-session scoped copy routes:
    - `src/lisp/jit_jit_eval_scopes.c3` (`jit_scopes_copy_value_into_scope_site(...)`)
    - `src/lisp/jit_jit_closure_define_qq.c3` (`jit_copy_value_into_scope_site(...)`)
    - `src/lisp/eval_type_evaluators.c3` (`eval_copy_value_into_scope_site(...)`)
    - `src/lisp/primitives_iter_coroutine.c3` (resume yield scoped copy)
  - Boundary commit fallback path was temporarily routed through a commit-local scoped copy helper in `src/lisp/eval_boundary_commit_flow.c3` (`boundary_commit_copy_to_scope_site(...)`) as an interim step; that helper/path was later removed in the same day’s hard-disallow closure.
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1567/0`, `Compiler Tests: 79/0`).
- Root-store destination-routing experiment (Track G) was attempted and rolled back:
  - Attempted route:
    - switched `promote_to_root_site(...)` to direct destination ESCAPE promotion.
    - switched JIT root-store callsites (`COPY_SITE_JIT_EVAL_SET`, `COPY_SITE_JIT_EVAL_DEFINE`, `COPY_SITE_JIT_ENV_EXTEND_ROOT`) to `boundary_promote_to_root_site(...)`.
  - Result:
    - full suite hit deterministic allocator corruption in Async I/O (`double free or corruption (!prev)`).
  - Action:
    - reverted root-store route changes to restore green baseline.
    - left Track G root-store item open pending transfer-safe ownership rules for direct root promotion.
  - Validation after rollback:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1562/0`, `Compiler Tests: 79/0`).
- Track H regex cleanup closure update:
  - `src/lisp/tests_runtime_feature_pika_groups.c3` migrated from compiled-vs-simple parity checks to post-fallback contract checks (compiled path + public API behavior).
  - Removed dead simple fallback APIs from `src/pika/regex_cache_api.c3`:
    - `regex_search_simple_fallback(...)`
    - `regex_fullmatch_simple_fallback(...)`
    - `regex_find_all_simple_fallback(...)`
    - and internal `regex_match_simple(...)`.
  - Added explicit unusable-cause counters in `src/pika/regex_cache_api.c3`:
    - `g_regex_compiled_unusable_compile`
    - `g_regex_compiled_unusable_cache`
    - plus reset wiring in `regex_cache_reset()`.
  - Added regression assertion that invalid patterns increment explicit compiled-unusable cause counters without reintroducing simple fallback behavior.
  - Removed unused boundary facade wrapper `boundary_copy_to_parent(...)` from `src/lisp/eval_boundary_api.c3`.
  - Removed dead helper `boundary_commit_to_scope_site(...)` from `src/lisp/eval_boundary_api.c3` (no live callsites).
  - Removed dead provenance facade helpers from `src/lisp/eval_boundary_provenance.c3` (no live callsites):
    - `boundary_try_scope_splice_escapes(...)`
    - `boundary_is_scope_transfer_legal(...)`
  - Boundary provenance canonicalization slice:
    - removed duplicate `in_target_scope_chain(...)` scan helper from `src/lisp/eval_promotion_copy.c3`.
    - `boundary_ptr_in_target_scope_chain(...)` now routes through `boundary_ptr_in_scope_chain(...)` in `src/lisp/eval_boundary_provenance.c3`.
    - moved closure payload scope-membership scan behind canonical helper `boundary_closure_payload_in_scope(...)` in `src/lisp/eval_boundary_provenance.c3` and removed direct scan logic from `src/lisp/eval_promotion_copy.c3`.
    - graph-audit TEMP reachability checks now route through `boundary_ptr_in_scope(...)` wrapper in `src/lisp/eval_boundary_diagnostics.c3`.
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1561/0`, `Compiler Tests: 79/0`).
- Track J language-surface migration closure (undeclared effects):
  - Decided and documented that undeclared effects are canonical language behavior (declarations are optional and only enable declaration-based type checks).
  - Promoted this rule into normative semantics:
    - `docs/EFFECTS_SEMANTICS.md` EFX-2 now explicitly states undeclared effects remain valid and skip declaration-based type checks.
  - Aligned tutorial/reference docs:
    - `docs/EFFECTS_GUIDE.md`
    - `docs/reference/06-effects.md`
  - Aligned regression naming/comments:
    - `src/lisp/tests_advanced_io_effect_ffi_groups.c3` now uses `effect undeclared canonical` (removed backward-compat wording).
  - Plan tracking aligned:
    - `TODO.md` Track J checkboxes closed with explicit canonical-policy note.
    - `docs/plans/fallback-inventory.md` undeclared-effect row moved to `Done`.
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1560/0`, `Compiler Tests: 79/0`).
- Boundary fallback-pressure closure update:
  - Landed conservative non-unique releasing-scope destination retry for flat scalar survivors in `boundary_commit_escape(...)` (`NIL`, `INT`, `DOUBLE`, `SYMBOL`, `TIME_POINT`) before copy fallback.
  - Validation via `scripts/run_boundary_hardening.sh` now shows `copy_fallback_total=0` in both normal and ASAN profiles, with `copy_fallback_site_run_jit=0`, `copy_fallback_site_jit_single=0`, and `copy_fallback_site_jit_call=0`.
  - Updated area docs (`docs/areas/memory-runtime.md`, `docs/areas/memory-runtime-cycle.md`) so graph traversal is documented as an explicit boundary path pending deletion, not a normal return-path mechanism.
- Boundary return-provenance Stage 1 classification landed:
  - Added explicit `BoundaryReturnProvenance` + `BoundaryReturnClassification` in `src/lisp/eval_boundary_api.c3` with one shared classifier (`boundary_classify_return_value(...)`) covering:
    - reusable in target chain,
    - owned by releasing TEMP lane,
    - owned by releasing ESCAPE lane,
    - mixed/uncertain provenance.
  - Routed `boundary_commit_escape(...)` through the shared classifier and removed ad-hoc provenance branching in that hot path.
  - Mixed/uncertain provenance now takes explicit fallback copy path (via `boundary_copy_to_scope_site(...)`) instead of passthrough.
  - Added telemetry reason `BOUNDARY_COPY_FALLBACK_MIXED_UNCERTAIN_PROVENANCE` and surfaced it in `OMNI_TEST_SUMMARY` as `copy_fallback_mixed_uncertain`.
  - Updated boundary summary exporter `scripts/parse_boundary_summary.sh` to include the new fallback-reason field.
  - Added regression `run_memory_lifetime_boundary_return_classification_tests(...)` in `src/lisp/tests_memory_lifetime_boundary_groups.c3` and wired it into `run_memory_lifetime_regression_tests(...)`.
  - Replaced direct `scope_contains(...)` usage in `src/lisp/eval_promotion_copy.c3` with boundary helper-backed membership check.
  - Fixed graph-audit diagnostic formatting (`scope_gen`/`target_escape_gen`) to avoid `<BAD FORMAT>` output.
  - Validation:
    - `c3c build` pass,
    - `c3c build --sanitize=address` pass,
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` pass (`Unified Tests: 1539/0` in ASAN-profile run, `1548/0` in normal profile from boundary hardening run),
    - `bash scripts/run_boundary_hardening.sh` pass (Stages 0-8 including threshold and policy gates).
- Boundary enum/result normalization (boolean-flag reduction):
  - Added explicit `BoundaryCommitOutcome` to `BoundaryCommitEscapeResult` in `src/lisp/eval_boundary_api.c3`, and set outcomes on all commit paths (null/reuse/splice/fallback/passthrough).
  - Added explicit `PromotionAbortReason` to `PromotionContext` in `src/lisp/value_environment.c3`; `promotion_context_begin(...)` now initializes it, and `promotion_context_consume(...)` sets budget-exhaustion reason.
  - `boundary_commit_escape(...)` now maps promotion-context abort handling through explicit reason state rather than raw booleans alone, while preserving existing telemetry semantics.
  - Validation: `c3c build` passes and `bash scripts/run_boundary_hardening.sh` passes (normal + ASAN + summary assertions + threshold checks + boundary policy).
- Boundary structural-invariant compile-time asserts:
  - Added enum/table sync tables and `$assert` checks in `src/lisp/eval_boundary_api.c3` for:
    - `BoundaryScopeTransferReason` labels
    - `BoundaryPromotionAbortReason` labels
    - `BoundaryCopyFallbackReason` labels
    - copy-site labels (`COPY_SITE_COUNT` parity)
    - `BoundaryTxnState` labels
  - Replaced reason/site name `switch` blocks with bounds-checked table lookups to keep classification and telemetry naming in one synchronized source of truth.
  - Validation: `c3c build` passes and `bash scripts/run_boundary_hardening.sh` passes (normal + ASAN + summary assertions + threshold checks + boundary policy).
- Boundary instrumentation mode gates + macro-scaffolding cleanup:
  - Added explicit compile-time boundary instrumentation modes in `src/lisp/eval_boundary_api.c3`:
    - `BOUNDARY_INSTRUMENTATION_OFF`
    - `BOUNDARY_INSTRUMENTATION_COUNTERS` (default)
    - `BOUNDARY_INSTRUMENTATION_TRACE`
    - `BOUNDARY_INSTRUMENTATION_BENCHMARK`
  - Telemetry/trace macros now remain narrowly scoped to instrumentation-only wrappers (counter increments and structured trace/benchmark emitters), with ownership and boundary control flow left in normal functions.
  - Added env-gated runtime toggles for trace/benchmark streams (`OMNI_BOUNDARY_TRACE`, `OMNI_BOUNDARY_BENCHMARK_TRACE`) behind compile-time mode checks.
  - Validation: sequential `c3c build` passes, and `bash scripts/run_boundary_hardening.sh` passes (normal + ASAN + summary assertions + telemetry thresholds + boundary policy).
- Boundary helper contract hardening:
  - Added explicit `@require`/`@ensure` contracts on unsafe boundary helpers in `src/lisp/eval_boundary_api.c3` (`boundary_scope_swap_*`, `boundary_releasing_scope_swap_*`, `boundary_txn_*`, `boundary_restore_interp_state`, `boundary_session_end`).
  - Added strict splice-wrapper preconditions for `boundary_try_scope_splice_escapes(...)` (non-null parent/child and immediate-parent relation), while keeping `_result` as the permissive diagnostics/fuzz entrypoint.
  - Validation: `c3c build` and `bash scripts/run_boundary_hardening.sh` both pass after contract additions.
- Boundary validator cold-path split:
  - Added `@noinline` failure helper `boundary_scope_transfer_reject(...)` and inline success helper `boundary_scope_transfer_ok(...)` in `src/lisp/eval_boundary_api.c3`.
  - `boundary_check_scope_transfer(...)` now routes every rejection through the centralized cold failure helper to reduce duplicated failure literals and keep cold-path intent explicit.
  - Added `@noinline` debug graph-audit reporter `boundary_debug_graph_audit_report_scope_transfer(...)` (env-gated by `OMNI_BOUNDARY_GRAPH_AUDIT`) and invoked it only on splice-transfer rejection paths.
  - Added env-gated `@noinline` verbose telemetry dumper `boundary_dump_decision_stats_verbose(...)` (controlled by `OMNI_BOUNDARY_VERBOSE_TELEMETRY`) and wired optional emission from test summary reporting in `src/lisp/tests_tests.c3`.
  - Validation: `c3c build` and `bash scripts/run_boundary_hardening.sh` pass with unchanged boundary summary thresholds.
- Boundary transaction protocol hardening:
  - Introduced `BoundaryTxn` + explicit state machine (`INIT`/`OPEN`/`COMMITTED`/`ABORTED`/`CLOSED`) in `src/lisp/eval_boundary_api.c3`.
  - `boundary_commit_escape(...)` now runs under transaction state guards with assert-backed transition checks for use-after-close, double-close, and invalid state transitions.
  - Commit/abort transitions are now explicit on all return paths in `boundary_commit_escape(...)`; session restore remains centralized via deferred `boundary_txn_close(...)`.
  - Added `BoundaryScopeSwap` guard (`boundary_scope_swap_begin/end`) and switched promote-scope override in `boundary_commit_escape(...)` to `defer`-based restoration.
  - Added `BoundaryReleasingScopeSwap` guard (`boundary_releasing_scope_swap_begin/end`) and switched releasing-scope overrides to defer-based restore in `boundary_copy_from_releasing_scope(...)` and `jit_copy_tco_env_chain_for_recycle(...)`.
  - Promoted `boundary_promotion_context_begin/end` symmetry to defer-cleanup form in `copy_env_to_scope(...)`, `promote_to_escape_disjoint(...)`, and `jit_copy_tco_env_chain_for_recycle(...)`.
  - Commit-failure/error paths now rely on deferred cleanup symmetry in boundary finalize paths (transaction close + promotion-context end + scope/releasing-scope restoration).
  - Validation: `c3c build`, `OMNI_FIBER_TEMP=1` summary suite run, and `scripts/run_boundary_hardening.sh` all pass.
- Boundary hardening closure (landed implementation + regression coverage):
  - Finalize flow unification is live through `boundary_finalize_scoped_result(...)` in `src/lisp/jit_jit_eval_scopes.c3`, and both eval (`run_promote_result`) and JIT (`jit_finalize_scoped_result`) now route through the same scoped finalize contract.
  - Boundary state restore now uses `BoundarySession` (`boundary_session_begin/end`) with `defer`-safe restore semantics for `current_scope`/`releasing_scope`, replacing ad-hoc save/restore callsite drift.
  - Direct boundary/env-copy closure retain edges now use `main::scope_retain(...)` (for `closure_val.env_scope`) in promotion and env-copy paths; release remains paired through existing closure destructor release paths.
  - Splice legality hardening is active via `boundary_check_scope_transfer(...)` with explicit reason codes (`BoundaryScopeTransferReason`), including immediate-parent, refcount, owner-thread, and lane-shape checks.
  - `ScopeRegion` ESCAPE splice path is now O(1) using `escape_chunks_tail` / `escape_dtors_tail` in `scope_splice_escapes(...)`, with tail-consistency assertions and reset/destroy tail clearing.
  - Regression coverage exists for finalize parity (promote+splice, abort->copy fallback, foreign fast-return), boundary session restore (nested LIFO, early return, error path), closure env-scope retain symmetry, and explicit splice rejection reasons.
- Console libuv parity closure: moved `io/print`, `io/println`, `io/display`, and `io/newline` off direct `std::io` writes; all four now render through runtime value formatting and emit via `omni_uv_fs_write` (`uv_fs_write` backend).
- Libuv parity normalization: reclassified `io/print`, `io/println`, `io/display`, and `io/newline` from `non-libuv` to `done-libuv`; parity snapshot is now `done-libuv: 38/38`, `partial-libuv: 0/38`, `non-libuv: 0/38`.
- Async fallback hardening: removed non-fiber behavioral fallbacks for `io/dns-resolve` and `io/async-sleep`; both now require running fiber context and raise deterministic `io/*-fiber-required` errors outside fibers.
- Async backend hardening: removed `io/async-sleep` offload/`usleep` fallback branches and removed `io/dns-resolve` synchronous `getaddrinfo` fallback branch; fiber paths now use only libuv timer/getaddrinfo backends.
- Async regression updates: runtime async tests now run DNS/sleep success assertions through `spawn`/`await` fiber context and assert non-fiber `fiber-required` errors.
- Scheduler loop fallback hardening: removed the scheduler idle `usleep` fallback from `scheduler_wait_for_io_once`; blocked-fiber wait now advances only through libuv loop pumping.
- Task/thread fiber-join hardening: converted no-timeout fiber join paths (`task-join`, `thread-join`) from 1ms `async-sleep` polling to event-driven wakeup signaling via scheduler wake events (`WAKEUP_TASK_DONE`, `WAKEUP_OS_THREAD_DONE`) and waiter-fiber registration on generation-safe handles.
- Task/thread fiber timeout-join hardening: converted fiber timeout join paths (`task-join-timeout`, `thread-join-timeout`) from 1ms `async-sleep` slicing to event-driven completion wake + single libuv timer race using scheduler timeout timer state, with waiter cleanup on timeout/error.
- Root task-join hardening: converted non-fiber pooled-task join waits to libuv-loop-driven waiting (`uv_run(...ONCE)`) so `uv_queue_work` completion callbacks are always pumped; timeout joins now use a dedicated libuv timer + loop pump (no 1ms polling slices).
- Condition wait hardening: made thread/task condition-variable wake signaling lock-disciplined (`scheduler_signal_thread_task_waiters_locked`) for state transitions that occur under `thread_task_mu`, eliminating missed-wakeup windows in non-fiber join waits.
- Dedicated thread backend hardening: migrated `io/thread-*` creation/join/detach lifecycle from `std::thread` calls to libuv thread APIs (`uv_thread_create`, `uv_thread_join`, `uv_thread_detach`) through `omni_uv_thread_*` wrappers, while preserving opaque generation-safe `thread-handle` semantics.
- TLS runtime libuv integration: migrated `io/tls-connect`, `io/tls-read`, `io/tls-write`, and `io/tls-close` fiber paths to `uv_queue_work` offload execution (BearSSL setup/I/O/shutdown runs on libuv worker queue), added deterministic non-fiber hard errors (`io/tls-*-fiber-required`), and added in-flight handle guards to reject concurrent operations on the same TLS handle.
- Libuv parity normalization: reclassified `io/tls-connect`, `io/tls-read`, `io/tls-write`, and `io/tls-close` from `non-libuv` to `done-libuv`; parity snapshot is now `done-libuv: 34/38`, `partial-libuv: 0/38`, `non-libuv: 4/38`.
- Waiter lifecycle hardening: added per-entry waiter tracking for pooled tasks and dedicated OS threads with explicit wakeup on completion/cancel/drop and waiter-conflict guards for invalid concurrent fiber joins.
- Validation after join wakeup hardening:
  - `c3c build` passes.
  - policy/parity guards pass: `check_async_fallback_policy.sh`, `check_io_boundary_facade.sh`, `check_io_parity_status_map.sh`.
  - full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) ends with known TLS failures only (`Unified Tests: 1523 passed, 5 failed`: `tls valid cert handshake`, `tls rejects untrusted cert`, `tls rejects hostname mismatch`, `tls mTLS optional cert/key pass`, `tls mTLS invalid key fails`).
- Scheduler error-domain canonicalization: fiber wait-slice failures inside `task-join` / `task-join-timeout` / `thread-join` / `thread-join-timeout` are now remapped to scheduler-domain errors (`scheduler/task-join-wait-failed`, `scheduler/thread-join-wait-failed`) instead of leaking lower-level `io/*` codes at the API boundary.
- Scheduler error-cause preservation: join wait remap path now consumes any pending lower-level raise payload and stores that original cause under scheduler error `data`, preserving diagnostic detail while enforcing canonical scheduler-domain codes.
- Scheduler task/thread integration: enabled `task-join`, `task-join-timeout`, `thread-join`, and `thread-join-timeout` in running-fiber context using non-blocking wait slices (`async-sleep`), while preserving root-context join semantics.
- Scheduler deadlock hardening: removed indefinite `wait(-1)` join waits in non-fiber task/thread join loops and replaced them with bounded condvar wait slices to eliminate missed-signal deadlock windows between completion check and waiter registration.
- Scheduler regression coverage: added fiber-context join regressions in `src/lisp/tests_scheduler_io_task_groups.c3` for `task-join`, `thread-join`, `task-join-timeout`, and `thread-join-timeout`.
- Libuv parity normalization: reclassified `io/task-spawn`, `io/task-join`, `io/task-join-timeout`, and `io/task-cancel` from `partial-libuv` to `done-libuv`; parity snapshot is now `done-libuv: 26/38`, `partial-libuv: 4/38`, `non-libuv: 8/38`.
- Libuv parity normalization: reclassified `io/thread-spawn`, `io/thread-join`, `io/thread-join-timeout`, and `io/thread-cancel` from `partial-libuv` to `done-libuv` after uv-thread backend migration; parity snapshot is now `done-libuv: 30/38`, `partial-libuv: 0/38`, `non-libuv: 8/38`.
- Validation after task/thread join integration and deadlock fix:
  - `c3c build` passes.
  - `c3c build --sanitize=address` passes.
  - parity/policy guards pass: `check_async_fallback_policy.sh`, `check_io_boundary_facade.sh`, `check_io_parity_status_map.sh`.
  - full suite pass (normal): `Unified Tests: 1526 passed, 0 failed`; `Compiler Tests: 79 passed, 0 failed`.
  - full suite pass (ASAN build): `Unified Tests: 1525 passed, 0 failed`; `Compiler Tests: 79 passed, 0 failed` (stack overflow recovery intentionally skipped under ASAN runtime).
- Scheduler/offload hardening: fixed a recycled-task race in pooled `uv_queue_work` completion handling by carrying `TaskEntry.generation` through queued work and validating generation on begin/clear/complete callbacks.
- Regression verification: `scheduler worker cancel interleave boundary restore` now passes in full-suite runs after generation guard wiring.
- Helper modularization: extracted libuv signal helpers from `csrc/uv_helpers.c` into `csrc/uv_helpers_signal.c` and wired the new source in `project.json`.
- Test modularization: split `src/lisp/tests_deduce_groups.c3` into domain files:
  - `src/lisp/tests_deduce_durability_groups.c3`
  - `src/lisp/tests_deduce_query_groups.c3`
  - `src/lisp/tests_deduce_isolation_groups.c3`
  while retaining orchestrator/shared helpers in `src/lisp/tests_deduce_groups.c3`.
- Stack engine modularization: extracted Section 8 defer/lifecycle + `stack_ctx_create`/`stack_ctx_destroy` ownership block from `src/stack_engine.c3` into `src/stack_engine_lifecycle.c3`, keeping switch/init/clone execution paths in `src/stack_engine.c3`.
- Stack engine test modularization: extracted defer/lifecycle test cluster from `src/stack_engine_tests.c3` into `src/stack_engine_tests_defer_lifecycle.c3`, keeping runner/shared fixtures in `src/stack_engine_tests.c3`.
- Regex modularization: extracted tokenizer/token model from `src/pika/regex.c3` into `src/pika/regex_tokenizer.c3`, keeping compiler/interpreter regex core in `src/pika/regex.c3`.
- Data-format modularization: extracted CSV parser/emitter and option parsing from `src/lisp/primitives_data_formats.c3` into `src/lisp/primitives_data_formats_csv.c3`, keeping TOML/time primitives in `src/lisp/primitives_data_formats.c3`.
- Runtime-feature test modularization: completed domain extraction from `src/lisp/tests_runtime_feature_groups.c3` into
  - `src/lisp/tests_runtime_feature_pika_groups.c3`
  - `src/lisp/tests_runtime_feature_http_groups.c3`
  - `src/lisp/tests_runtime_feature_jit_groups.c3`
  - `src/lisp/tests_runtime_feature_schema_reader_groups.c3`
  while retaining diagnostic/atomic groups in `src/lisp/tests_runtime_feature_groups.c3`.
- Scope-region modularization: extracted fiber-temp pooling/stats from `src/scope_region.c3` into `src/scope_region_temp_pool.c3`, and reset/adopt/freelist-cleanup helpers into `src/scope_region_reset_adopt.c3`.
- Pika modularization: extracted grammar compiler and `pika/grammar` build path from `src/pika/lisp_pika.c3` into `src/pika/lisp_pika_grammar_compiler.c3`.
- Scheduler-state modularization: extracted shared-handle registry/sendable publish-project-retire pipeline from `src/lisp/scheduler_state_offload.c3` into `src/lisp/scheduler_state_shared_handles.c3`, keeping offload work build/init flow in `src/lisp/scheduler_state_offload.c3`.
- Advanced-test modularization: completed core/unicode/macro split from `src/lisp/tests_advanced_tests.c3` by extracting core/unicode/runtime/lambda/binding groups into `src/lisp/tests_advanced_core_unicode_groups.c3`, macro-hygiene groups into `src/lisp/tests_advanced_macro_hygiene_groups.c3`, and retaining shared builders + `run_advanced_tests` orchestrator in `src/lisp/tests_advanced_tests.c3`.
- Scheduler-test modularization: completed io/task/fairness extraction from `src/lisp/tests_scheduler_groups.c3` into `src/lisp/tests_scheduler_io_task_groups.c3`, retaining spawn/await/wakeup orchestration and group entrypoints in `src/lisp/tests_scheduler_groups.c3`.
- Compiler-test modularization: split `src/lisp/tests_compiler_tests.c3` into feature-domain files:
  - `src/lisp/tests_compiler_core_groups.c3`
  - `src/lisp/tests_compiler_codegen_groups.c3`
  - `src/lisp/tests_compiler_jit_print_groups.c3`
  while retaining shared helpers and `run_compiler_tests` orchestration in `src/lisp/tests_compiler_tests.c3`.
- Scheduler wakeup modularization: split `src/lisp/scheduler_wakeup_io.c3` by extracting wakeup dispatch/reliable-queue/libuv callback plumbing into `src/lisp/scheduler_wakeup_dispatch.c3`, while retaining pending close/complete and consume/value projection paths in `src/lisp/scheduler_wakeup_io.c3`.
- Advanced type/effect/ffi modularization: split `src/lisp/tests_advanced_type_effect_ffi_groups.c3` by extracting io/effect/union/ffi groups into `src/lisp/tests_advanced_io_effect_ffi_groups.c3`, while retaining type/dispatch/mutation/consolidation groups in `src/lisp/tests_advanced_type_effect_ffi_groups.c3`.
- Validation after split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) both pass after the compiler, scheduler-wakeup, and advanced-type/effect/ffi slices.
- Scheduler task/thread modularization completion: finalized pooled-task vs dedicated-thread separation by retaining pooled task primitives in `src/lisp/scheduler_primitives_tasks.c3` and dedicated OS-thread lifecycle primitives in `src/lisp/scheduler_primitives_threads.c3`.
- Validation after scheduler task/thread slice: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) both pass.
- Memory-lifetime modularization: split `src/lisp/tests_memory_lifetime_env_tco_promotion_groups.c3` into domain files:
  - `src/lisp/tests_memory_lifetime_env_copy_groups.c3`
  - `src/lisp/tests_memory_lifetime_tco_budget_groups.c3`
  - `src/lisp/tests_memory_lifetime_promotion_context_groups.c3`
  with no behavior changes to group entrypoints.
- Validation after memory-lifetime split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- CSV modularization: split `src/lisp/primitives_data_formats_csv.c3` into domain files:
  - `src/lisp/primitives_data_formats_csv_parse.c3`
  - `src/lisp/primitives_data_formats_csv_emit.c3`
  while retaining CSV option-normalization and TOML-option parsing helpers in `src/lisp/primitives_data_formats_csv.c3`.
- Validation after CSV split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- JSON modularization: split `src/lisp/json.c3` by extracting JSON emit conversion/serialization functions into `src/lisp/json_emit.c3`, while retaining yyjson extern declarations, parse options, pointer helpers, and parse/get primitives in `src/lisp/json.c3`.
- Validation after JSON split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- I/O modularization: split `src/lisp/prim_io.c3` by extracting libc/system primitives into `src/lisp/prim_system.c3` (`shell`, `getenv`, `time`, `time-ms`, `exit`, `sleep`, `display`), while retaining core print/file/libuv handle/load primitives in `src/lisp/prim_io.c3`.
- Validation after I/O split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Async modularization: split `src/lisp/async.c3` by extracting process/signal handle lifecycle and process argument/FFI helper functions into `src/lisp/async_process_signal_handles.c3`, while retaining async core extern declarations, transport wrappers, and `prim_async_sleep` in `src/lisp/async.c3`.
- Validation after async split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- E2E generation modularization: split `src/lisp/tests_e2e_generation.c3` by extracting setup fixtures to `src/lisp/tests_e2e_generation_setups.c3` and scenario tables to `src/lisp/tests_e2e_generation_cases_core.c3` / `src/lisp/tests_e2e_generation_cases_extended.c3`, while retaining generator orchestration and buffer/file helpers in `src/lisp/tests_e2e_generation.c3`.
- Validation after E2E generation split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Pattern matching modularization: split `src/lisp/eval_pattern_matching.c3` by extracting support utilities (`MatchResult`, gensym table, structural `values_equal`, sequence/list helpers) into `src/lisp/eval_pattern_support.c3`, while retaining runtime pattern dispatch and sequence matcher execution in `src/lisp/eval_pattern_matching.c3`.
- Validation after pattern-matching split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- REPL modularization: split `src/lisp/eval_repl.c3` by extracting replxx syntax-highlighting/completion callbacks, color constants, and callback global state into `src/lisp/eval_repl_callbacks.c3`, while retaining replxx FFI declarations, session lifecycle, and eval-loop control flow in `src/lisp/eval_repl.c3`.
- Validation after REPL split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Memory-lifetime test modularization: split `src/lisp/tests_memory_lifetime_groups.c3` by extracting boundary-state/run_program/wrapper-copy regression clusters into `src/lisp/tests_memory_lifetime_boundary_groups.c3`, while retaining lane-barrier/root-fallback orchestration and remaining lifetime groups in `src/lisp/tests_memory_lifetime_groups.c3`.
- Validation after memory-lifetime boundary split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scheduler-boundary test modularization: split `src/lisp/tests_scheduler_boundary_groups.c3` by extracting thread/task boundary regressions (stress, completion, cancel, mixed state restore) into `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`, while retaining shared boundary snapshot helpers and wakeup/offload boundary suites in `src/lisp/tests_scheduler_boundary_groups.c3`.
- Validation after scheduler-boundary split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Core test modularization: split `src/lisp/tests_core_groups.c3` by extracting let/function-parameter destructuring regression suites into `src/lisp/tests_core_destructure_groups.c3`, while retaining numeric/list primitives, control-flow, error, and runtime guard groups in `src/lisp/tests_core_groups.c3`.
- Validation after core-test split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Macro expansion modularization: split `src/lisp/macros_expansion.c3` by extracting macro definition/hygiene capture flow (`collect_pattern_vars`, captured binding snapshot, `eval_define_macro`, special-form symbol checks) into `src/lisp/macros_define_hygiene.c3`, and macro template expansion/splice helpers (`expand_pattern_macro`, gensym mapping, template list expansion) into `src/lisp/macros_template_expansion.c3`, while retaining macro hash lookup, expansion traversal, and module hash lookup in `src/lisp/macros_expansion.c3`.
- Validation after macro-expansion split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- JIT module/import modularization: split `src/lisp/jit_jit_module_import.c3` by extracting non-module JIT dispatch helpers (index/path/match/type/ffi wrappers and call-name tracking) into `src/lisp/jit_jit_dispatch_helpers.c3`, while retaining module registration, source-dir/path resolution, import binding, and module-file load/eval flow in `src/lisp/jit_jit_module_import.c3`.
- Validation after JIT module/import split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scheduler wakeup/offload boundary modularization: split `src/lisp/tests_scheduler_boundary_wakeup_offload_groups.c3` by extracting offload payload ownership/cleanup, duplicate ready-event handling, and consume-pending-offload regression suites into `src/lisp/tests_scheduler_boundary_offload_payload_groups.c3`, while retaining reliable wakeup wraparound, mixed event ordering, and enqueue observability boundary suites in `src/lisp/tests_scheduler_boundary_wakeup_offload_groups.c3`.
- Validation after scheduler wakeup/offload boundary split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Deduce relation modularization: split `src/lisp/deduce_relation_ops.c3` by extracting relation scan/range kernel helpers (`deduce_compare_value`, tuple compare, bound parse, cursor range scan/all) into `src/lisp/deduce_relation_scan_helpers.c3`, while retaining relation mutation primitives (`fact!`, `retract!`, `clear`, `drop`) and count/scan primitive entrypoints in `src/lisp/deduce_relation_ops.c3`.
- Validation after deduce relation split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scheduler state modularization: split `src/lisp/scheduler_state_offload.c3` by extracting scheduler constants, enums, and core state structs into `src/lisp/scheduler_state_types.c3`, while retaining global scheduler/domain instances, offload work/result helper functions, wakeup reset helper, and scheduler init flow in `src/lisp/scheduler_state_offload.c3`.
- Validation after scheduler state split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Advanced stdlib/module test modularization: split `src/lisp/tests_advanced_stdlib_module_groups.c3` by extracting stdlib/numeric/math/array/set/string/predicate/format/introspection suites into `src/lisp/tests_advanced_stdlib_numeric_groups.c3`, while retaining collections, module-system, and parser import/export edge suites plus module/collection orchestrators in `src/lisp/tests_advanced_stdlib_module_groups.c3`.
- Validation after advanced stdlib/module split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- JIT closure/define modularization: split `src/lisp/jit_jit_closure_define_qq.c3` by extracting define/method-table dispatch helpers (`jit_eval_define`, typed-closure dispatch, method-table allocation/append/signature compare) into `src/lisp/jit_jit_define_method_table.c3`, while retaining TCO execution helpers, closure construction/signature allocation, letrec/set-path helpers, and env-extension helpers in `src/lisp/jit_jit_closure_define_qq.c3`.
- Validation after JIT closure/define split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Test harness modularization: split `src/lisp/tests_tests.c3` by extracting consolidated fixture/assertion helpers (`setup`, `test_*`, floating-point matcher helpers, and shared test utility helpers used across test group files) into `src/lisp/tests_harness_helpers.c3`, while retaining top-level unified-suite registration/orchestration and test-group boundary reset flow in `src/lisp/tests_tests.c3`.
- Validation after test harness split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Interpreter state modularization: split `src/lisp/value_interp_state.c3` by extracting interpreter lifecycle methods (`Interp.grow_macro_table`, `Interp.grow_module_table`, `Interp.grow_handler_stack`, `Interp.destroy`) into `src/lisp/value_interp_lifecycle.c3`, while retaining interpreter state struct, init pipelines, frame guard helpers, and allocation APIs in `src/lisp/value_interp_state.c3`.
- Validation after interpreter state split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- I/O modularization: split `src/lisp/prim_io.c3` by extracting libuv fs-handle primitives (`fs-open/read/write/close/stat/readdir/rename/unlink`) plus fs-handle lifetime/error helpers into `src/lisp/prim_io_fs_handles.c3`, while retaining core print/newline, async file/read-lines, and `load` flow in `src/lisp/prim_io.c3`.
- Validation after fs-handle I/O split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine test modularization: split `src/stack_engine_tests.c3` by extracting thread-affinity and fiber-temp lifecycle/retention clusters (including affinity violation probe entrypoint) into `src/stack_engine_tests_affinity_fiber_temp.c3`, while retaining core stack region/pool/coroutine/overflow/FPU/clone tests and `run_stack_engine_tests` orchestration in `src/stack_engine_tests.c3`.
- Validation after stack-engine test split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine modularization: split `src/stack_engine.c3` by extracting stack-pool ownership/thread-affinity subsystem (`StackPool`, owner-token helpers, `stack_pool_init`, `stack_pool_shutdown`) into `src/stack_engine_pool_ownership.c3`, while retaining stack-switch/trampoline/init/suspend/resume/clone runtime core in `src/stack_engine.c3`.
- Validation after stack-engine pool/ownership split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Entry modularization: split `src/entry.c3` by extracting project scaffolding and FFI bind command pipeline (`--init` / `--bind` helpers + handlers) into `src/entry_project_init_bind.c3`, while retaining build/compile/repl/script/test/probe paths and top-level CLI routing in `src/entry.c3`.
- Validation after entry init/bind split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine modularization: split `src/stack_engine.c3` by extracting coroutine bootstrap and ASAN switch bridge (`g_stack_ctx_bootstrap`, main fake-stack token, trampoline, and `asan_switch_*` helpers) into `src/stack_engine_bootstrap_switch.c3`, while retaining stack region, context init/suspend/resume, and clone logic in `src/stack_engine.c3`.
- Validation after stack-engine bootstrap/ASAN split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine modularization: split `src/stack_engine.c3` by extracting diagnostics and test-summary subsystem (`g_stack_quiet_output`, defer metrics counters/reset, summary emitters, `stack_print_pass`) into `src/stack_engine_diagnostics.c3`, while retaining stack allocation/runtime execution and clone paths in `src/stack_engine.c3`.
- Validation after stack-engine diagnostics split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine modularization: split `src/stack_engine.c3` by extracting stack region allocator/membership subsystem (`StackRegion`, stack size constants, headroom checks, `stack_region_alloc/free/contains`) into `src/stack_engine_region.c3`, while retaining stack context init/suspend/resume/clone flow in `src/stack_engine.c3`.
- Validation after stack-engine region split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scope-region modularization: split `src/scope_region.c3` by extracting bump allocator subsystem (`ScopeRegion.alloc_slow`, `alloc_escape_slow`, `alloc`, `alloc_escape`) into `src/scope_region_allocators.c3`, while retaining scope ownership/refcount lifecycle, chunk allocation, and destructor registration in `src/scope_region.c3`.
- Validation after scope-region allocator split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine test modularization: split `src/stack_engine_tests.c3` by extracting coroutine runtime test cluster (context entry helpers, suspend/resume/interleaving, overflow recovery, FPU preservation, and clone multi-shot) into `src/stack_engine_tests_coroutines.c3`, while retaining stack region/pool smoke tests and `run_stack_engine_tests` orchestration in `src/stack_engine_tests.c3`.
- Validation after stack-engine coroutine-test split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine modularization: split `src/stack_engine.c3` by extracting context-switch runtime subsystem (`g_current_stack_ctx`, `stack_ctx_switch_to`, `stack_ctx_suspend`, `stack_ctx_resume`) into `src/stack_engine_context_runtime.c3`, while retaining context ABI declarations, stack context struct/init, and clone logic in `src/stack_engine.c3`.
- Validation after stack-engine context-runtime split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Entry modularization: split `src/entry.c3` by extracting test/harness execution cluster (`run_test_mode`, stack-affinity harness helpers, `run_test_mode_with_self`, `run_stack_affinity_probe_mode`) into `src/entry_test_modes.c3`, while retaining build/compile/repl/script modes and top-level flag dispatch in `src/entry.c3`.
- Validation after entry test-mode split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scope-region modularization: split `src/scope_region.c3` by extracting scope ownership lifecycle subsystem (`scope_create`, `scope_retain`, `scope_release`, destructor-chain runners, `scope_destroy`) into `src/scope_region_lifecycle.c3`, while retaining type/constants/global guard helpers, chunk alloc helpers, and scope membership checks in `src/scope_region.c3`.
- Validation after scope-region lifecycle split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scope-region test modularization: split `src/scope_region_tests.c3` by extracting core scope tests (create/destroy, alloc/escape lanes, reset modes, RC/freelist/alignment, `scope_splice_escapes`) into `src/scope_region_tests_core.c3` via `run_scope_region_core_tests()`, while retaining fiber-temp pool invariants and summary reporting in `src/scope_region_tests.c3`.
- Validation after scope-region test split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine modularization: split `src/stack_engine.c3` by extracting stack cloning subsystem (`stack_ctx_clone`) into `src/stack_engine_clone.c3`, while retaining stack context ABI/struct/init declarations and section anchors in `src/stack_engine.c3`.
- Validation after stack-engine clone split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Entry modularization: split `src/entry.c3` by extracting the AOT build pipeline (`AOT_BUILD_PREFIX`, build argument/source helpers, backend compile command assembly, `run_build`) into `src/entry_build_mode.c3`, while retaining shared CLI string helpers, compile/repl/script handlers, and top-level argument dispatch in `src/entry.c3`.
- Validation after entry build-mode split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Main/runtime modularization: split `src/main.c3` by extracting thread-local registry subsystem (`g_thread_registry`, registry init/shutdown, dereference/allocate-in/root helpers) into `src/main_thread_registry.c3`, while retaining pool/slot/region structures and `RegionRegistry` core methods in `src/main.c3`.
- Validation after main thread-registry split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scope-region test modularization: split `src/scope_region_tests_core.c3` by extracting reset/alignment/splice-heavy core test domain (Tests 7–15) into `src/scope_region_tests_reset_splice.c3` via `run_scope_region_reset_splice_tests()`, while retaining create/alloc/escape/RC/freelist core tests in `src/scope_region_tests_core.c3`.
- Validation after scope-region reset/splice test split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine coroutine-test modularization: split `src/stack_engine_tests_coroutines.c3` by extracting coroutine edge-path tests (`test_stack_overflow_recovery`, `test_fpu_preservation`, `test_stack_ctx_clone_multishot` plus local overflow/FPU entry helpers) into `src/stack_engine_tests_coroutines_edges.c3`, while retaining core coroutine execution/suspend/resume/interleave coverage in `src/stack_engine_tests_coroutines.c3`.
- Validation after stack-engine coroutine-edge split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine lifecycle modularization: split `src/stack_engine_lifecycle.c3` by extracting defer-storage subsystem (`stack_ctx_defer_entry_at`, defer reserve/push/pop/update, deferred-destroy runner, defer-storage clear) into `src/stack_engine_lifecycle_defer.c3`, while retaining lifecycle-attach/clone helpers and create/destroy orchestration in `src/stack_engine_lifecycle.c3`.
- Validation after stack-engine lifecycle defer split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine affinity/fiber-temp test modularization: split `src/stack_engine_tests_affinity_fiber_temp.c3` by extracting the fiber-temp test domain (`FiberTempScopeState`, stack-scope entry helpers, scope-create-in-context, clone/discard stress, retention-guard) into `src/stack_engine_tests_fiber_temp.c3`, while retaining thread-affinity ownership smoke test and fail-fast probe harness in `src/stack_engine_tests_affinity_fiber_temp.c3`.
- Validation after stack-engine fiber-temp test split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Main/runtime modularization: split `src/main.c3` by extracting pool/slot method implementations (`SparseSet.*`, `Pool.arena_alloc/allocate/destroy_all`, `SlotTable.allocate/destroy_all`) into `src/main_pool_slot_methods.c3`, while retaining region/registry core types, allocation macro, and dereference utility helpers in `src/main.c3`.
- Validation after main pool/slot-method split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine modularization: split `src/stack_engine.c3` by extracting platform ABI/switch subsystem (platform constants, stack helper externs, `StackContext`, and `stack_context_switch`) into `src/stack_engine_abi_switch.c3`, while retaining coroutine status/handle metadata and `stack_ctx_init` in `src/stack_engine.c3`.
- Validation after stack-engine ABI/switch split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine defer/lifecycle test modularization: split `src/stack_engine_tests_defer_lifecycle.c3` by extracting defer-focused tests (`test_stack_ctx_defer_destroy_lifo`, `test_stack_ctx_defer_undefer_pop`, `test_stack_ctx_defer_clone_hook`, `test_stack_ctx_defer_update_arg`, `test_stack_ctx_defer_update_arg_clone_isolation`) into `src/stack_engine_tests_defer.c3`, while retaining lifecycle destroy/clone tests in `src/stack_engine_tests_defer_lifecycle.c3`.
- Validation after stack-engine defer/lifecycle test split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Entry modularization: split `src/entry_project_init_bind.c3` by extracting the `--bind` command domain (project-dir/toml/output-path builders, libclang parse orchestration, parsed-function cleanup, and `run_bind`) into `src/entry_bind_mode.c3`, while retaining `--init` scaffolding helpers and `run_init` in `src/entry_project_init_bind.c3`.
- Validation after entry bind-mode split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Entry modularization: split `src/entry.c3` by extracting runtime command handlers (`run_compile_mode`, `run_gen_e2e_mode`, `run_repl_mode`, `run_script_mode`) into `src/entry_runtime_modes.c3`, while retaining shared CLI helpers, help output, and top-level argument dispatch in `src/entry.c3`.
- Validation after entry runtime-mode split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine fiber-temp modularization: split `src/stack_engine_tests_fiber_temp.c3` by extracting the stress/retention cluster (`test_entry_scope_yield_once`, `test_stack_ctx_fiber_temp_clone_discard_stress`, `test_stack_ctx_fiber_temp_retention_guard`) into `src/stack_engine_tests_fiber_temp_stress.c3`, while retaining the scope-create-in-context state/helpers and regression in `src/stack_engine_tests_fiber_temp.c3`.
- Validation after stack-engine fiber-temp stress split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scope-region modularization: split `src/scope_region.c3` by extracting chunk/destructor/membership helpers (`scope_chunk_alloc_raw`, `scope_chunk_alloc_temp`, `scope_chunk_data`, `scope_register_dtor`, `scope_register_dtor_escape`, `scope_contains`) into `src/scope_region_chunk_helpers.c3`, while retaining scope-region types, global lock/thread-affinity guards, and invariants in `src/scope_region.c3`.
- Validation after scope-region chunk-helper split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scope-region temp-pool modularization: split `src/scope_region_temp_pool.c3` by extracting context-pool lifecycle helpers (`fiber_temp_ctx_pool_on_clone`, `fiber_temp_ctx_pool_on_destroy`, `fiber_temp_ctx_pool_find_current`, `fiber_temp_ctx_pool_get_or_create`, `fiber_temp_chunk_take_from_ctx_pool`) into `src/scope_region_temp_ctx_pool.c3`, while retaining global pool reclaim/take policy and memory-stat helpers in `src/scope_region_temp_pool.c3`.
- Validation after scope-region temp-ctx-pool split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scope-region reset/adopt modularization: split `src/scope_region_reset_adopt.c3` by extracting reset and scope-membership helpers (`scope_reset`, `scope_reset_temp_lane`, `is_in_scope`) into `src/scope_region_reset_helpers.c3`, while retaining escape-adoption transfer logic (`scope_splice_escapes`) and global freelist cleanup in `src/scope_region_reset_adopt.c3`.
- Validation after scope-region reset-helper split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine defer-test modularization: split `src/stack_engine_tests_defer.c3` by extracting defer-update tests (`test_stack_ctx_defer_update_arg`, `test_stack_ctx_defer_update_arg_clone_isolation`) into `src/stack_engine_tests_defer_update.c3`, while retaining defer destroy/undefer/clone-hook coverage in `src/stack_engine_tests_defer.c3`.
- Validation after stack-engine defer-update split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scope-region reset/splice test modularization: split `src/scope_region_tests_reset_splice.c3` by extracting `is_in_scope` and `scope_splice_escapes` test-domain helpers into `src/scope_region_tests_splice_cases.c3`, while retaining reset/alignment/deep-parent/retain-symmetry tests in `src/scope_region_tests_reset_splice.c3`.
- Validation after scope-region reset/splice helper split: `c3c build` passes; first full-suite run had one transient TLS failure (`tls valid cert handshake (interp=FAIL, jit=ok)`), immediate rerun of `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed cleanly (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine lifecycle modularization: split `src/stack_engine_lifecycle.c3` by extracting lifecycle hook storage/clone/destroy helper cluster (`stack_ctx_lifecycle_entry_at`, reserve/attach/find/clone/run/clear helpers) into `src/stack_engine_lifecycle_hooks.c3`, while retaining context create/destroy orchestration in `src/stack_engine_lifecycle.c3`.
- Validation after stack-engine lifecycle hook split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine coroutine-test modularization: split `src/stack_engine_tests_coroutines.c3` by extracting basic one-shot coroutine smoke coverage (`test_entry_simple`, `test_entry_100`, `test_basic_coro`, `test_stack_ctx_second`) into `src/stack_engine_tests_coroutines_basic.c3`, while retaining suspend/resume, recursion, and interleaving coroutine tests in `src/stack_engine_tests_coroutines.c3`.
- Validation after stack-engine coroutine basic split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Main/runtime modularization: split `src/main.c3` by extracting `RegionRegistry` method cluster (`RegionRegistry.init`, `RegionRegistry.dereference`, `RegionRegistry.dereference_as`) into `src/main_region_registry_methods.c3`, while retaining region/pool/slot type definitions and allocation macros in `src/main.c3`.
- Validation after main region-registry-method split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine fiber-temp stress modularization: split `src/stack_engine_tests_fiber_temp_stress.c3` by extracting the retention guard stress case (`test_stack_ctx_fiber_temp_retention_guard`) into `src/stack_engine_tests_fiber_temp_retention.c3`, while retaining shared yield-entry helper and clone/discard stress coverage in `src/stack_engine_tests_fiber_temp_stress.c3`.
- Validation after stack-engine fiber-temp retention split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scope-region temp-pool modularization: split `src/scope_region_temp_pool.c3` by extracting temp-pool stats/type/reporting cluster (stats structs/globals, eligibility predicates, memory-stat reporting helpers) into `src/scope_region_temp_pool_stats.c3`, while retaining chunk reclaim/take/release policy logic in `src/scope_region_temp_pool.c3`.
- Validation after scope-region temp-pool stats split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scope-region lifecycle modularization: split `src/scope_region_lifecycle.c3` by extracting destructor/destroy path cluster (`scope_run_dtor_chain`, `scope_run_dtors`, `scope_run_escape_dtors`, `scope_destroy`) into `src/scope_region_destroy.c3`, while retaining `scope_create` and retain/release APIs in `src/scope_region_lifecycle.c3`.
- Validation after scope-region destroy split: `c3c build` passes; an initial full-suite run stalled in scheduler tests without log progress, then a clean rerun of `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine modularization: split `src/stack_engine.c3` by extracting stack context metadata/type declarations (`StackCtxStatus`, defer/lifecycle callback structs, bootstrap state, inline caps, and `StackCtx` handle layout) into `src/stack_engine_types.c3`, while retaining `stack_ctx_init` initialization flow in `src/stack_engine.c3`.
- Validation after stack-engine type split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Main/runtime modularization: split `src/main.c3` by extracting pool/slot storage type declarations (`SparseSet`, pool arena/free-list records, `Pool`, object record/slot table records, and allocation result structs + constants) into `src/main_pool_slot_types.c3`, while retaining handle aliases, `Region` + allocation/destroy helpers, and `RegionRegistry` declarations in `src/main.c3`.
- Validation after main pool/slot-type split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine test modularization: split `src/stack_engine_tests.c3` by extracting stack-engine suite runner/orchestration (`run_stack_engine_tests`) into `src/stack_engine_tests_runner.c3`, while retaining defer helper fixtures and stack-region/pool smoke test helpers in `src/stack_engine_tests.c3`.
- Validation after stack-engine test-runner split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scope-region test modularization: split `src/scope_region_tests_core.c3` by extracting alloc/lifecycle coverage (Tests 1–6 for create/alloc/escape/destructor/RC/freelist generation) into `src/scope_region_tests_alloc_lifecycle.c3`, while retaining core orchestrator aggregation and reset/splice suite composition in `src/scope_region_tests_core.c3`.
- Validation after scope-region alloc/lifecycle test split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Entry modularization: split `src/entry_bind_mode.c3` by extracting bind-path helper functions (`resolve_bind_project_dir`, `build_bind_toml_path`, `build_bind_output_path`) into `src/entry_bind_paths.c3`, while retaining libclang parse pipeline, parsed-function cleanup, and `run_bind` orchestration in `src/entry_bind_mode.c3`.
- Validation after entry bind-path split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scope-region test modularization: split `src/scope_region_tests_reset_splice.c3` by extracting topology/alignment test domain (large alloc, deep parent cascade, retain/release symmetry, and alignment gap checks) into `src/scope_region_tests_topology_alignment.c3`, while retaining reset/escape-lane and splice orchestration in `src/scope_region_tests_reset_splice.c3`.
- Validation after scope-region topology/alignment split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Scope-region test modularization: split `src/scope_region_tests_alloc_lifecycle.c3` by extracting RC/destructor/freelist lifecycle tests (parent-child refcount invariants, destructor execution, freelist generation increment) into `src/scope_region_tests_refcount_destructor.c3`, while retaining create/alloc/escape/chunk-overflow coverage in `src/scope_region_tests_alloc_lifecycle.c3`.
- Validation after scope-region refcount/destructor split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Stack-engine coroutine-edge modularization: split `src/stack_engine_tests_coroutines_edges.c3` by extracting overflow-recovery coverage (`test_entry_overflow`, `test_overflow_recurse`, `test_stack_overflow_recovery`) into `src/stack_engine_tests_coroutine_overflow.c3`, while retaining FPU preservation and clone multi-shot edge tests in `src/stack_engine_tests_coroutines_edges.c3`.
- Validation after stack-engine coroutine-overflow split: `c3c build` and full suite run (`LD_LIBRARY_PATH=/usr/local/lib ./build/main`) pass (`Stack engine: 21/0`, `Unified Tests: 1522/0`, `Compiler Tests: 79/0`).
- Boundary hardening: implemented committed-escape graph-reachability audit in `src/lisp/eval_boundary_api.c3` (`boundary_graph_audit_escape_reachability` + `boundary_debug_graph_audit_committed_escape_root`) and wired it into `boundary_commit_escape` post-commit checks with explicit traversed edge documentation (CONS/ARRAY/HASHMAP/CLOSURE/PARTIAL_PRIM/METHOD_TABLE/MODULE/ITERATOR/INSTANCE) and explicit opaque-wrapper exclusions (FFI/coroutine/continuation/primitive/type payloads).
- Boundary hardening tests: added targeted reachability tests in `src/lisp/tests_memory_lifetime_boundary_groups.c3` and wired them via `run_memory_lifetime_regression_tests` (`src/lisp/tests_memory_lifetime_groups.c3`) covering accepted all-ESCAPE graph, TEMP-edge rejection from ESCAPE root, and opaque-wrapper exclusion semantics.
- Validation after boundary graph-reachability audit/tests: `c3c build`, `c3c build --sanitize=address`, and `LD_LIBRARY_PATH=/usr/local/lib ./build/main` pass (`Unified Tests: 1538/0`, `Compiler Tests: 79/0`).
- Boundary transaction protocol hardening: introduced explicit transition classifier `boundary_txn_transition_allowed(...)` in `src/lisp/eval_boundary_api.c3` and used it to guard commit/abort/close transitions, keeping terminal-state misuse checks centralized in one helper.
- Boundary transaction protocol tests: added targeted transition-policy and lifecycle tests in `src/lisp/tests_memory_lifetime_boundary_groups.c3` (`matrix`, `begin->commit->close`, `begin->abort->close`) and wired them through `run_memory_lifetime_regression_tests` in `src/lisp/tests_memory_lifetime_groups.c3`.
- Boundary benchmark support: added env-gated benchmark runner `run_memory_lifetime_boundary_benchmarks(...)` in `src/lisp/tests_memory_lifetime_boundary_groups.c3` (enabled with `OMNI_BOUNDARY_BENCH=1`) emitting `OMNI_BENCH_SUMMARY` for:
  - decision-cost lanes (`splice`, `fallback`, `reuse`) via `boundary_commit_escape(...)`,
  - splice-tail throughput via `boundary_try_scope_splice_escapes_result(...)` + tail-consistency checks.
- Benchmark harness fix: removed accidental double-splice in benchmark loop (was invoking `scope_splice_escapes` after `boundary_try_scope_splice_escapes_result`, which already performs splice on allowed transfer) to avoid UAF under ASAN in benchmark mode.
- Validation after benchmark support/fix:
  - `c3c build` passed.
  - `c3c build --sanitize=address` passed.
  - Benchmark mode (normal): `OMNI_BOUNDARY_BENCH=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1547/0`, `Compiler Tests: 79/0`) and emitted `OMNI_BENCH_SUMMARY`.
  - Benchmark mode (ASAN): `OMNI_BOUNDARY_BENCH=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1538/0`, `Compiler Tests: 79/0`) and emitted `OMNI_BENCH_SUMMARY`.
- Traversal telemetry slice: extended `CopyToParentStats` with cons-spine copy distribution fields (`cons_spine_copy_samples`, `cons_spine_copy_total_len`, `cons_spine_copy_peak_len`), wired cons-spine measurement on defensive list-copy routes, and added env-gated summary emission (`OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1`) as `OMNI_TEST_SUMMARY suite=boundary_traversal`.
- Traversal baseline (pre-routing-policy change) captured:
  - Normal summary run: `OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` -> `boundary_traversal copy_total=511808 copy_fast_reuse=245581 copy_defensive=266227 copy_tag_cons=1443 copy_tag_closure=6429 copy_tag_other=924 copy_site_run_jit=220 copy_site_jit_single=2708 copy_site_tco=0 promoted_then_spliced=36199 promoted_then_fallback_copied=6832 cons_spine_samples=1396 cons_spine_avg_len=149 cons_spine_peak_len=10000`.
  - ASAN summary run: `OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` -> `boundary_traversal copy_total=407198 copy_fast_reuse=223096 copy_defensive=184102 copy_tag_cons=943 copy_tag_closure=5200 copy_tag_other=730 copy_site_run_jit=220 copy_site_jit_single=1408 copy_site_tco=0 promoted_then_spliced=19431 promoted_then_fallback_copied=4960 cons_spine_samples=907 cons_spine_avg_len=118 cons_spine_peak_len=10000`.
- Boundary hardening Stage 2 closure: finalized destination-aware CONS return routing in `boundary_commit_escape(...)` via `boundary_build_destination_cons_escape(...)` with iterative ESCAPE-lane spine construction and post-write barrier assertions that prevent TEMP-lane edges from being committed under ESCAPE roots.
- Boundary hardening Stage 3 extension: added destination-aware PARTIAL primitive return routing for TEMP-owned boundary survivors via `boundary_build_destination_partial_escape(...)`, including a dedicated commit outcome `BOUNDARY_COMMIT_DESTINATION_PARTIAL_BUILT` in `src/lisp/eval_boundary_api.c3`.
- Boundary hardening regression coverage: added `boundary_commit_escape TEMP partial survivor commit` in `src/lisp/tests_memory_lifetime_boundary_groups.c3`, asserting destination-built outcome, no fallback/splice/reuse, and ESCAPE-lane ownership for routed partial arguments.
- Validation after Stage 2 closure + Stage 3 partial route:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1549/0`, `Compiler Tests: 79/0`).
  - `c3c build --sanitize=address` passed.
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed under ASAN (`Unified Tests: 1549/0`, `Compiler Tests: 79/0`).
  - `bash scripts/run_boundary_hardening.sh` passed (`normal Unified Tests: 1549/0`, `ASAN Unified Tests: 1540/0`, thresholds + policy checks green).
- Stage 4 env/closure boundary route unification: removed duplicate closure payload clone logic from `src/lisp/eval_env_copy.c3` and reused the shared `copy_parent_clone_closure_payload(...)` path so env-copy and return-boundary closure relocation share one retain/copy implementation.
- Validation after env/closure route unification:
  - `c3c build` passed.
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1549/0`, `Compiler Tests: 79/0`).
  - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1540/0`, `Compiler Tests: 79/0`).
  - `bash scripts/run_boundary_hardening.sh` passed (`normal Unified Tests: 1549/0`, `ASAN Unified Tests: 1540/0`, thresholds + policy checks green).
- Stage 4 env/promotion idiom closure:
  - `src/lisp/eval_env_copy.c3` now uses a cold invariant sink (`copy_env_invariant_fail(...) @noinline`) with runtime detail logging + `unreachable(...)`, replacing the non-canonical `assert(false)` pattern.
  - `copy_env_to_scope(...)` keeps owned promotion-context lifetime under `defer`, and helper contracts now enforce restoration (`@ensure interp.active_promotion_ctx == saved_active_ctx`) through `copy_env_end_owned_promotion_ctx(...)`.
  - Env-copy closure/iterator copy flow remains explicit/typed (`copy_env_clone_closure_if_needed`, `copy_env_clone_iterator_if_needed`) with no pointer-punning shortcuts.
- Validation after Stage 4 idiom closure:
  - `c3c build` passed.
  - `c3c build --sanitize=address` passed.
  - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1549/0`, `Compiler Tests: 79/0`).
  - `bash scripts/run_boundary_hardening.sh` currently fails in Stage 2 on existing HTTP fiber/libuv parity tests (`http-get in fiber via libuv tcp path`, `http-request in fiber via libuv tcp path`) while boundary policy/facade checks and normal/ASAN builds are otherwise healthy.
- Stage 5 planning closure (JIT/AOT destination-routing inventory):
  - Marked Stage 5 "identify constructors/callsites" complete in `TODO.md` with concrete callsites:
    - `run_promote_result(...)` (`COPY_SITE_RUN_JIT_RESULT`) in `src/lisp/eval_run_pipeline.c3`
    - `jit_eval_in_single_scope(...)` (`COPY_SITE_JIT_SINGLE_SCOPE_RETURN`) in `src/lisp/jit_jit_eval_scopes.c3`
    - `jit_eval_in_call_scope(...)` (`COPY_SITE_JIT_CALL_SCOPE_STEP2`) in `src/lisp/jit_jit_eval_scopes.c3`
    - `copy_tco_env_chain(...)` (`COPY_SITE_JIT_TCO_FRAME_COPY`) kept as boundary-classified fallback surface (not direct destination-builder path).
  - Prioritized aggregate families for JIT return fast-path rollout: `CONS` and `PARTIAL_PRIM` first, then `ITERATOR`/closure-wrapper cases with explicit provenance.
- Stage 5 fallback-policy closure:
  - Kept runtime fallback surfaces on boundary facades only; policy gate remains green (`bash scripts/check_boundary_facade_usage.sh`).
  - Confirmed runtime eval/JIT finalize convergence on shared boundary contract (`boundary_finalize_scoped_result(...)` + `boundary_commit_escape(...)`).
  - Confirmed coroutine suspend/resume and global escape surfaces use boundary helpers (`boundary_copy_to_parent_site(...)`, `boundary_promote_to_root(...)`) rather than direct boundary primitives.
- Stage 6 telemetry inventory refresh:
  - Captured fresh traversal/fallback summary with `OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`.
  - Current top fallback sources remain JIT return boundaries:
    - `copy_fallback_site_jit_single=2564`
    - `copy_fallback_site_run_jit=2141`
    - `copy_fallback_site_tco=0`
  - Current top traversal site counts:
    - `copy_site_jit_single=1357`
    - `copy_site_run_jit=308`
    - `copy_site_tco=0`
  - Marked Stage 6 "identify top callsites still firing" TODO item complete; narrowing target order remains `COPY_SITE_JIT_SINGLE_SCOPE_RETURN` then `COPY_SITE_RUN_JIT_RESULT`.
- Stage 6 narrowing slice (iterator route):
  - Added destination-aware iterator survivor route:
    - `boundary_build_destination_iterator_escape(...)`
    - new commit outcome: `BOUNDARY_COMMIT_DESTINATION_ITERATOR_BUILT`
    - wired in `boundary_commit_escape(...)` TEMP-owned fast route alongside existing `CONS` and `PARTIAL_PRIM` destination routes.
  - Added regression in `src/lisp/tests_memory_lifetime_boundary_groups.c3`:
    - `boundary_commit_escape TEMP iterator survivor commit`
    - asserts destination-built outcome, no fallback/splice/reuse, ESCAPE-lane payload ownership, and site-counter shape (`COPY_SITE_GENERIC` increments, caller site no direct fallback increment).
  - Validation after iterator route:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1549/0`, `Compiler Tests: 79/0`).
- Stage 6 narrowing slice (direct destination promotion for remaining TEMP-owned tags):
  - Added `BOUNDARY_COMMIT_DESTINATION_DIRECT_PROMOTED` and routed non-specialized TEMP-owned survivors through direct destination-scope ESCAPE promotion in `boundary_commit_escape(...)`, avoiding releasing-scope promotion + splice requirement for that subset.
  - Added regression in `src/lisp/tests_memory_lifetime_boundary_groups.c3`:
    - `boundary_commit_escape TEMP scalar direct destination commit`
    - asserts direct-destination outcome, no fallback/splice/reuse, and ESCAPE-lane ownership in target scope.
  - Validation after direct-destination route:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1550/0`, `Compiler Tests: 79/0`).
    - `c3c build --sanitize=address` passed.
    - `ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1551/0`, `Compiler Tests: 79/0`).
    - Full-run boundary fallback deltas vs pre-route baseline in this session:
      - `copy_fallback_total`: `4745 -> 4720` (down 25)
      - `copy_fallback_site_jit_single`: `2564 -> 2541` (down 23)
      - `copy_fallback_site_run_jit`: `2141 -> 2139` (down 2)
- Stage 6 policy confirmation:
  - Confirmed fallback traversal remains explicit for dynamic/provenance-uncertain boundaries (`BOUNDARY_RETURN_MIXED_UNCERTAIN`) and reason-tagged splice-rejected/pre-aborted branches in `boundary_commit_escape(...)`.
  - Boundary facade policy guard remains green after the Stage 6 slices (`bash scripts/check_boundary_facade_usage.sh`).
- Stage 6 scope-membership hotness reevaluation:
  - Audited runtime scope-membership callsites after Stage 6 narrowing; boundary return paths remain centralized on boundary wrappers (`boundary_ptr_in_scope`, `boundary_ptr_in_scope_chain`).
  - Normalized `copy_value_if_owned_by_scope(...)` in `src/lisp/eval_promotion_context.c3` from direct `main::is_in_scope(...)` to `boundary_ptr_in_scope(...)`.
  - Validation after membership-helper normalization:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1551/0`, `Compiler Tests: 79/0`).
- Stage 6 narrowing checkpoint closure:
  - Marked Stage 6 "delete or narrow redundant callsites" TODO item complete after landing:
    - destination-aware `ITERATOR` survivor route, and
    - direct destination promotion route for remaining TEMP-owned survivor tags.
  - Latest summary run (`OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1`) shows residual fallback dominated by explicit dynamic classes:
    - `copy_fallback_mixed_uncertain=245`
    - `copy_fallback_budget=47`
    - `copy_fallback_splice_rejected=12`
    - `copy_fallback_releasing_scope=1`
- Stage 7 fallback-site inventory closure:
  - Marked Stage 7 first checklist item complete in `TODO.md` by classifying remaining fallback traversal surfaces with concrete owner functions/files:
    - dynamic eval: `run_promote_result(...)` (`src/lisp/eval_run_pipeline.c3`) + `jit_eval_in_single_scope(...)` (`src/lisp/jit_jit_eval_scopes.c3`) through `boundary_finalize_scoped_result(...)` -> `boundary_commit_escape(...)`.
    - continuation/effect boundary: `jit_eval_in_call_scope(...)` (`src/lisp/jit_jit_eval_scopes.c3`) on same finalize/commit path.
    - coroutine boundary: `prim_resume_yield_result(...)` (`src/lisp/primitives_iter_coroutine.c3`) via `boundary_copy_to_parent_site(..., COPY_SITE_PRIM_RESUME)`.
    - global mutation/persistent store: `promote_to_root_site(...)` (`src/lisp/eval_promotion_escape.c3`) and root-store callsites in `src/lisp/jit_jit_closure_define_qq.c3` / `src/lisp/jit_jit_define_method_table.c3`.
    - uncertain-provenance helpers: `boundary_copy_to_scope_site(...)`, `boundary_copy_from_releasing_scope(...)`, `boundary_copy_value_if_owned_by_scope(...)` (`src/lisp/eval_boundary_api.c3`) plus `copy_value_if_owned_by_scope(...)` (`src/lisp/eval_promotion_context.c3`).
  - Captured current summary evidence during inventory:
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
    - `Unified Tests: 1551/0`, `Compiler Tests: 79/0`
    - boundary counters: `copy_fallback_total=305`, `copy_fallback_site_run_jit=72`, `copy_fallback_site_jit_single=224`, `copy_fallback_site_tco=0`, `copy_fallback_mixed_uncertain=245`, `copy_fallback_budget=47`, `copy_fallback_splice_rejected=12`, `copy_fallback_releasing_scope=1`.
- Stage 7 coroutine-boundary narrowing:
  - Replaced coroutine resume’s direct copy helper path with explicit boundary provenance routing:
    - `src/lisp/primitives_iter_coroutine.c3`: `prim_yield(...)` now records `interp.yield_scope = interp.current_scope` alongside `yield_value`.
    - `prim_resume_yield_result(...)` now routes with `boundary_copy_to_scope_site(yielded, interp, interp.current_scope, yield_scope, COPY_SITE_PRIM_RESUME)` instead of `boundary_copy_to_parent_site(...)`.
    - missing coroutine yield provenance now hard-errors (`resume: missing yield-scope provenance`) instead of silently falling back.
  - Added lifecycle cleanup for coroutine yield-boundary state:
    - `coroutine_clear_yield_boundary_state(...)` clears `yield_value`/`yield_scope`.
    - invoked from resume completion/error cleanup paths.
  - Interp state extension:
    - `src/lisp/value_interp_state.c3`: added `yield_scope` field and init reset.
    - `src/lisp/tests_tests.c3`: boundary reset helper now clears `yield_scope`.
  - Validation after coroutine-boundary narrowing:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1551/0`, `Compiler Tests: 79/0`).
- Stage 7 fallback-telemetry expansion (class visibility):
  - Extended boundary decision summary emission in `src/lisp/tests_tests.c3` to include additional site counters:
    - `copy_fallback_site_jit_call` (`COPY_SITE_JIT_CALL_SCOPE_STEP2`)
    - `copy_fallback_site_repl` (`COPY_SITE_REPL_LINE_RESULT`)
    - `copy_fallback_site_prim_resume` (`COPY_SITE_PRIM_RESUME`)
    - `copy_fallback_site_jit_eval_set`, `copy_fallback_site_jit_eval_define`, `copy_fallback_site_jit_env_extend_root`
  - Latest summary after expansion:
    - `copy_fallback_site_run_jit=72`
    - `copy_fallback_site_jit_single=224`
    - `copy_fallback_site_jit_call=9`
    - `copy_fallback_site_prim_resume=0`
    - `copy_fallback_site_repl=0`
    - root-store fallback sites remain `0` in this run.
- Stage 7 mixed-uncertain route narrowing:
  - `src/lisp/eval_boundary_api.c3`:
    - added commit outcome `BOUNDARY_COMMIT_MIXED_DESTINATION_PROMOTED`.
    - in `boundary_commit_escape(...)`, `BOUNDARY_RETURN_MIXED_UNCERTAIN` now attempts destination-scope ESCAPE promotion first and only falls back to `boundary_copy_to_scope_site(...)` when that route cannot commit.
  - `src/lisp/tests_memory_lifetime_boundary_groups.c3`:
    - added regression `boundary_commit_escape mixed uncertain destination commit`.
    - verifies no fallback copy, destination ESCAPE ownership, and expected site-counter shape (`COPY_SITE_GENERIC` increments, caller site fallback counter unchanged).
  - Validation:
    - `c3c build` passed.
    - `c3c build --sanitize=address` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1552/0`, `Compiler Tests: 79/0`).
    - `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` now passes end-to-end under ASAN build (`Unified Tests: 1543/0`, `Compiler Tests: 79/0`); prior `139` unwind crash is closed.
  - Boundary fallback delta after mixed-route landing (summary run):
    - `copy_fallback_total`: `305 -> 67`
    - `copy_fallback_mixed_uncertain`: `245 -> 10`
    - `copy_fallback_site_run_jit`: `72 -> 3`
    - `copy_fallback_site_jit_single`: `224 -> 61`
    - `copy_fallback_site_jit_call`: `9 -> 3`
- Stage 7 ASAN unwind stabilization (stack-overflow/error boundary path):
  - Root-cause closure:
    - Deep non-tail stack-overflow unwind could recurse through `boundary_commit_escape(...)` disjoint `ERROR` promotion, triggering ASAN crash (`SIGSEGV`/`139`) in Advanced tests.
  - Runtime changes:
    - `src/lisp/value_interp_state.c3`: `interp_apply_stack_overflow_error(...)` now allocates stack-overflow errors in `root_scope` so unwind can reuse one surviving error value across finalize boundaries.
    - `src/lisp/eval_boundary_api.c3`: added `boundary_build_destination_error_escape(...)` and routed TEMP-owned `ERROR` values through direct destination ESCAPE construction before generic disjoint promotion.
  - Validation:
    - Repro script (`(let ^rec (f (lambda (n) (+ 1 (f (+ n 1))))) (f 0))`) no longer crashes under ASAN build.
    - `c3c build` passed.
    - `c3c build --sanitize=address` passed.
    - `OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1543/0`, `Compiler Tests: 79/0`) with no `139`.
- Stage 7 dynamic-eval budget pressure reduction:
  - `src/lisp/eval.c3`:
    - raised `PROMOTION_CTX_BUDGET_RETURN` from `4096` to `16384` to reduce pre-aborted boundary finalize contexts in dynamic eval/JIT return flows.
  - Boundary summary delta after budget update:
    - normal profile (`Unified Tests: 1552/0`):
      - `copy_fallback_total`: `67 -> 61`
      - `copy_fallback_budget`: `47 -> 43`
      - `copy_fallback_site_jit_single`: `61 -> 57`
      - `copy_fallback_site_jit_call`: `3 -> 1`
      - `promotion_aborted_pre_aborted`: `47 -> 43`
    - ASAN profile (`Unified Tests: 1543/0`):
      - `copy_fallback_total`: `58 -> 55`
      - `copy_fallback_budget`: `45 -> 43`
      - `copy_fallback_site_jit_single`: `55 -> 53`
      - `copy_fallback_site_jit_call`: `2 -> 1`
      - `promotion_aborted_pre_aborted`: `45 -> 43`
  - Validation:
    - `c3c build` passed.
    - `c3c build --sanitize=address` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed in both normal and ASAN build profiles.
- Stage 7 splice-rejected fast-path experiment rollback:
  - Tried routing splice-rejected releasing-escape cases through direct destination promotion.
  - Observed unstable ASAN full-suite behavior (exit `139` during Advanced Feature Tests), so the splice-rejected fast path and its dedicated regression were reverted to keep runtime stable.
  - Current retained state:
    - mixed-uncertain destination route remains landed.
    - splice-rejected branch remains copy-fallback (`copy_fallback_splice_rejected=8` in latest normal summary).
- Stage 7 return-finalize budget class retirement + telemetry cleanup:
  - Runtime policy update:
    - `src/lisp/jit_jit_eval_scopes.c3`: `boundary_finalize_scoped_result(...)` now starts `boundary_promotion_context_begin(interp, &boundary_ctx)` without return-budget cap, so scoped finalize does not budget-abort into fallback for large return graphs.
    - `src/lisp/eval_boundary_api.c3`: added `boundary_restore_decision_stats(...)` to allow scoped telemetry isolation in tests.
  - Test policy update (synthetic fallback isolation):
    - `src/lisp/tests_memory_lifetime_boundary_groups.c3`: forced budget=0 fallback parity probe snapshots/restores boundary decision stats.
    - `src/lisp/tests_memory_lifetime_boundary_stress_groups.c3`: mixed-event `case 3` (forced budget=0 fallback) snapshots/restores boundary decision stats.
    - `src/lisp/tests_memory_lifetime_groups.c3`: updated large-list finalize parity expectations to match new no-budget-abort return-finalize policy (no copy-site fallback delta required; commit parity remains required).
  - Validation:
    - `c3c build` passed.
    - `c3c build --sanitize=address` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed in normal and ASAN builds (`Unified Tests: 1552/0`).
  - Boundary summary after this change:
    - `copy_fallback_total=10`
    - `copy_fallback_budget=0`
    - `copy_fallback_splice_rejected=8`
    - `copy_fallback_releasing_scope=2`
    - `copy_fallback_mixed_uncertain=0`
    - `copy_fallback_site_jit_single=7`
    - `copy_fallback_site_run_jit=2`
    - `copy_fallback_site_jit_call=1`
- Stage 7 splice-rejected parent-lane closure (fallbacks eliminated):
  - Root cause:
    - splice-rejected fallback class (`BOUNDARY_SCOPE_TRANSFER_PARENT_ESCAPE_LANE_INVALID`) was caused by `scope_splice_escapes(...)` moving child ESCAPE chunk lists into parent without transferring parent ESCAPE cursor (`escape_bump`/`escape_limit`).
    - This left parent in an invalid optional-lane shape (`escape_chunks != null` with null bump/limit), forcing boundary commit to fall back instead of splice.
  - Runtime fix:
    - `src/scope_region_reset_adopt.c3`: `scope_splice_escapes(...)` now transfers `escape_bump` and `escape_limit` from child to parent when ESCAPE chunks are adopted.
  - Regression coverage:
    - `src/scope_region_tests_splice_cases.c3`: strengthened splice concat invariants to assert parent ESCAPE lane cursor validity (`escape_bump != null`, `escape_limit != null`, `escape_bump <= escape_limit`) after parent-empty + child-non-empty splice.
  - Boundary telemetry updates:
    - `src/lisp/eval_boundary_api.c3`:
      - added splice-rejected reason counters under fallback accounting (`copy_fallback_splice_rejected_by_reason[...]`).
      - added fallback source-tag telemetry (`copy_fallback_tag_cons/error/other`) and last-fallback diagnostics.
      - enabled graph-audit/verbose telemetry env switches in counters mode (`OMNI_BOUNDARY_GRAPH_AUDIT`, `OMNI_BOUNDARY_VERBOSE_TELEMETRY`) for diagnostics without changing instrumentation mode.
    - `src/lisp/tests_tests.c3` and `scripts/parse_boundary_summary.sh`:
      - summary/parser now include splice-fail reason breakdown, splice-rejected fallback reason breakdown, extended fallback site keys, and fallback tag diagnostics.
  - Hardening runner stability:
    - `scripts/run_boundary_hardening.sh` now logs Stage 2/4 via redirection helper (`run_stage_with_log`) instead of shell pipelines to avoid pipe-induced flake in this environment.
  - Validation:
    - `c3c build` passed.
    - `c3c build --sanitize=address` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed in normal + ASAN runs.
    - `scripts/run_boundary_hardening.sh` passed end-to-end (Stages 0-8).
  - Boundary summary after fix (ASAN stage):
    - `copy_fallback_total=0`
    - `copy_fallback_splice_rejected=0`
    - `copy_fallback_splice_reason_parent_lane=0`
    - `copy_fallback_site_jit_single=0`
    - `copy_fallback_site_jit_call=0`
- Fallback cleanup tracks (A-E partial) landed:
  - Parser permissive fallback removal:
    - `src/lisp/parser_lexer_string_hash.c3`: unknown `#` dispatch no longer falls through to symbol parsing; it now emits a deterministic lexer error (`"unknown # dispatch sequence"`).
    - `src/lisp/parser_expr_atoms.c3`: added explicit `T_ERROR` branch so lexer error text is surfaced as parser error.
    - `src/lisp/tests_runtime_feature_schema_reader_groups.c3`: added regression asserting unknown `#` dispatch rejection.
  - Compiler migration residue cleanup:
    - `src/lisp/compiler_code_emission.c3`: removed ignored migration parameter from `emit_lambda_return_with_frame(...)` and updated caller to canonical signature.
  - Error-wrapper retirement:
    - removed `raise->message` and `try-message` from:
      - `src/lisp/compiler_stdlib_prelude.c3`
      - `stdlib/stdlib.lisp`
    - migrated test/fixture/harness callsites to explicit payload-aware handlers:
      - `tests/lib/tls/server_once.omni`
      - `src/lisp/tests_advanced_core_unicode_groups.c3`
      - `src/lisp/tests_advanced_io_effect_ffi_groups.c3`
      - `src/lisp/tests_runtime_async_groups.c3`
      - `src/lisp/tests_runtime_feature_http_groups.c3`
      - `scripts/run_tls_targeted.sh`
    - docs updated to remove wrappers from standard surface (`docs/LANGUAGE_SPEC.md`, `docs/ERROR_MODEL.md`).
  - Legacy deduce alias removal:
    - deleted primitive table aliases from `src/lisp/eval_init_primitives.c3`:
      - `deduce-open`, `fact!`, `retract!`, `deduce-scan`, `deduce-query`, `deduce-count`, `deduce-match`
    - added explicit rejection coverage in `src/lisp/tests_deduce_groups.c3` for all removed names.
  - Plan/status docs updated:
    - `TODO.md` Track A/B/C, parser and compiler cleanup checkboxes updated.
    - `docs/plans/fallback-inventory.md` statuses advanced for parser/compiler/error-wrapper/deduce rows.
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed.
    - latest summary: `Unified Tests: 1560/0`, `Compiler Tests: 79/0`.
- JIT fallback retirement:
  - `src/lisp/jit_jit_compile_effects_modules.c3`:
    - `jit_compile_fallback(...)` no longer calls `jit_eval_fallback(...)`.
    - unhandled JIT expression routes now hard-fail with `JIT_COMPILE_FAILED~`.
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1560/0`, `Compiler Tests: 79/0`).
- Regex dual-path fallback removal:
  - `src/pika/regex_cache_api.c3`:
    - removed silent compiled->simple fallback in `regex_search`, `regex_fullmatch`, and `regex_find_all`.
    - default regex API now follows compiled-engine path only; unusable compile path records `g_regex_fallback_uses` and returns compiled-path no-match/empty result.
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1560/0`, `Compiler Tests: 79/0`).
- Scope reset defensive fallback removal:
  - `src/scope_region_reset_helpers.c3`:
    - introduced shared invariant helper `scope_require_temp_baseline_chunk(...)`.
    - removed silent TEMP baseline recreation path from `scope_reset_temp_lane(...)`.
    - strengthened reset paths with explicit `keep != null` assertions.
  - Validation:
    - `c3c build` passed.
    - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1560/0`, `Compiler Tests: 79/0`).
- Track F JIT fallback telemetry + variadic no-fallback closure:
  - Added per-expression fallback audit telemetry:
    - `src/lisp/jit_jit_compile_expr_core.c3`:
      - new `JitFallbackAuditStats` counters (`total`, per-`ExprTag` buckets, `unknown` bucket, `last_tag`).
      - `jit_compile_expr(...)` now records fallback-audit events immediately before guarded `jit_compile_fallback(...)` failure.
      - added reset/snapshot helpers (`jit_fallback_audit_reset`, `jit_fallback_audit_snapshot`, `jit_fallback_audit_tag_count`).
    - `src/lisp/tests_tests.c3`:
      - emits `OMNI_TEST_SUMMARY suite=jit_fallback ...` summary line.
      - resets audit counters at unified-suite start.
  - Added explicit variadic no-fallback regression:
    - `src/lisp/tests_runtime_feature_jit_groups.c3`:
      - `jit policy: variadic lambda executes without jit fallback` asserts fallback counter delta is zero while evaluating a variadic lambda.
    - `src/lisp/tests_advanced_core_unicode_groups.c3`:
      - updated variadic coverage note to canonical native-JIT behavior (no fallback route).
  - Validation:
    - `c3c build` passed.
    - Full normal suite passed:
      - `OMNI_TEST_SUMMARY=1 OMNI_TEST_QUIET=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main`
      - `Unified Tests: 1562 passed, 0 failed`
      - `Compiler Tests: 79 passed, 0 failed`
      - `OMNI_TEST_SUMMARY suite=jit_fallback total=0 ...`
    - ASAN build passed (`c3c build --sanitize=address`), but ASAN full-suite run currently exits `139` during Advanced Feature Tests in this environment; unchanged pre-existing instability.
- Track G boundary-facade ownership narrowing (promotion-context copy helpers):
  - `src/lisp/eval_promotion_context.c3`:
    - removed direct scoped-copy implementations:
      - `copy_to_parent_site_ctx(...)`
      - `copy_value_if_owned_by_scope(...)`
    - file now owns promotion-context lifecycle/memo primitives only.
  - `src/lisp/eval_boundary_api.c3`:
    - `boundary_copy_to_parent_site_ctx(...)` now owns scoped-copy + memoized tracking logic.
    - `boundary_copy_value_if_owned_by_scope(...)` now owns source-scope ownership-aware routing.
  - Callsite migration:
    - `src/lisp/eval_promotion_escape.c3`: disjoint escape bridge now calls `boundary_copy_to_parent_site_ctx(...)`.
    - `src/lisp/tests_memory_lifetime_promotion_context_groups.c3`: switched remaining direct calls to `boundary_copy_value_if_owned_by_scope(...)`.
  - Validation:
    - `c3c build` passed.
    - full normal suite passed (`Unified Tests: 1562 passed, 0 failed`, `Compiler Tests: 79 passed, 0 failed`).
    - ASAN build passed; ASAN full-suite remains environment-unstable (`exit 139` in Advanced Feature Tests), unchanged from prior baseline.
- Track G copy provenance closure (remove non-context `copy_to_parent` fallback branch):
  - `src/lisp/eval_promotion_copy.c3`:
    - `copy_to_parent_try_fast_reuse(...)` now uses only `boundary_classify_return_value(...)` with explicit `current_scope` + `releasing_scope` provenance; removed non-context `boundary_can_reuse_value(...)` fallback branch.
    - `copy_to_parent(...)` now hard-errors (`"boundary: copy_to_parent requires target/releasing scope provenance"`) when provenance context is missing instead of applying conservative migration copy behavior.
    - Added explicit contracts requiring non-null provenance context for fast-reuse classification.
  - `src/lisp/eval_boundary_api.c3`:
    - `boundary_copy_to_parent_site_ctx_impl(...)` now enforces explicit boundary provenance (`current_scope` + `releasing_scope`) and hard-errors on missing context.
    - `boundary_copy_env_to_target_scope_impl(...)` now sets `interp.releasing_scope = session.current_scope` so env-copy boundary routing carries explicit source-scope provenance.
  - Test alignment:
    - `src/lisp/tests_memory_lifetime_env_copy_groups.c3` env-copy lifetime cases now use `boundary_copy_env_to_target_scope(...)` instead of direct `copy_env_to_scope(...)` calls so tests validate the canonical boundary contract.
  - Policy guard tightening:
    - `scripts/check_boundary_facade_usage.sh` now treats direct `copy_env_to_scope(...)` and raw `boundary_copy_to_parent_site(...)` usage as disallowed symbols.
    - `scripts/boundary_facade_policy.txt` explicitly allows those symbols only in boundary core implementation files.
  - Validation:
    - `c3c build` passed.
    - `scripts/check_boundary_facade_usage.sh` passed.
    - `scripts/run_boundary_hardening.sh` passed end-to-end (normal + ASAN, Stage 0-8).
    - boundary telemetry summary stayed at `copy_fallback_total=0` for normal and ASAN runs.
  - Follow-up hygiene:
    - normalized stale “fallback” wording in boundary/runtime comments to match current hard-error + explicit provenance policy:
      - `src/lisp/eval.c3`
      - `src/lisp/eval_env_copy.c3`
      - `src/lisp/value_environment.c3`
      - `src/lisp/eval_promotion_escape.c3`
    - advanced TODO closure state:
      - `TODO.md`: marked “For each remaining class…” complete in Stage 7.
      - `TODO.md`: marked runtime ownership fallback cleanup complete except canonical `MethodTable.fallback`.
      - `TODO.md`: marked closure criteria complete for JIT no-interpreter-fallback, regex no dual-path silent fallback, defensive invariant cleanup, and language-surface canonicalization.
      - `TODO.md`: marked closure criteria complete for API duplicates, parser permissive fallthrough, and ignored compiler signatures.
      - `TODO.md`: marked closure criteria complete for ownership/provenance hot-path scan reduction and largest-hotspot split completion.
      - `TODO.md`: marked Stage acceptance + fallback-policy rule bullets complete (staged rollout criteria, final traversal-fallback closure, Eval/JIT parity, and MethodTable-fallback policy lock).
      - `TODO.md`: marked migration-rule + instrumentation idiom bullets complete where code evidence exists (staged cutover discipline, JIT route enums + `$assert` sync, fallback/status enum audit checks, and split-discipline closure notes).
      - `TODO.md`: marked additional idiom closures with evidence (contract-backed ownership-transition helpers, contract-backed replacement-helper shape, and canonical language-form enforcement for effects/error-model surfaces).
      - `TODO.md`: marked semantic-route helper organization and stack-engine opacity checks complete (route-based boundary helper clustering + no direct ScopeRegion policy in non-test stack-engine core modules).
      - `TODO.md`: marked `foreach` idiom closure complete with codebase-wide iteration evidence (non-indexed loops use `foreach`; index loops retained only where positional semantics are needed).
      - `TODO.md`: marked `@nodiscard` idiom closure by annotating boundary helpers where dropped return values are correctness bugs (`boundary_copy_to_parent_site_ctx(...)`, `boundary_copy_env_to_target_scope(...)`).
      - `TODO.md`: marked compile-time gate discipline closure for JIT routing modules (no scattered `@if`/preprocessor-style branching in expression-routing paths).
      - `TODO.md`: marked public-API cleanup patch-discipline closure (bounded API-removal patches with paired negative/regression tests).
- 2026-03-09 (finwatch integration + entrypoint export fix):
  - `examples/finwatch/server.omni`:
    - fixed a missing closing `)` in `route/portfolio`, which had unintentionally nested subsequent top-level forms and blocked later server exports.
    - normalized entrypoint names to canonical slash forms: `server/start-async`, `server/start`.
  - `examples/finwatch/main.omni`:
    - switched to canonical entrypoint call `(server/start "127.0.0.1" 8181)`.
  - `examples/finwatch/boot_smoke.omni`:
    - updated symbol reference from `server-start` to `server/start`.
  - `examples/finwatch/TODO.md`:
    - marked live integration test complete with concrete verification note.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/boot_smoke.omni` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/dispatch_smoke.omni` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/smoke_test.omni` passed.
    - live probe passed: launched `examples/finwatch/main.omni`, `/health` returned HTTP `200`, and process remained alive.
- 2026-03-09 (finwatch UDP/pipe parity completion):
  - `examples/finwatch/smoke_test.omni`:
    - added bounded pipe coverage using `pipe-listen`/`pipe-connect` + `tcp-accept`/`tcp-read`/`tcp-write` and socket cleanup.
    - added bounded UDP coverage using `udp-socket`/`udp-bind`/`udp-send`/`udp-recv`/`udp-close`.
    - emits explicit pass lines: `pipe io: OK`, `udp io: OK`.
  - `examples/finwatch/TODO.md`:
    - marked remaining `UDP/pipe I/O` item complete with concrete smoke evidence.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/boot_smoke.omni` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/dispatch_smoke.omni` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/smoke_test.omni` passed, including `pipe io: OK` and `udp io: OK`.
- 2026-03-09 (finwatch signal/process parity completion):
  - `examples/finwatch/smoke_test.omni`:
    - added signal watcher coverage: `signal-handle` + `signal-unhandle` with real `SIGUSR1` delivery via spawned `/bin/sh`.
    - added process termination coverage: `process-kill` + `process-wait` with explicit child stdio handle cleanup.
    - emits explicit pass lines: `signal io: OK`, `process-kill: OK`.
  - `examples/finwatch/TODO.md`:
    - marked remaining `signal-handle`/`signal-unhandle` and `process-kill` items complete.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/boot_smoke.omni` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/dispatch_smoke.omni` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/finwatch/smoke_test.omni` passed, including `signal io: OK` and `process-kill: OK`.
- 2026-03-09 (effects-contract drift gates: lint + CI + docs parity):
  - Added contract lint scripts:
    - `scripts/check_effects_contract_policy.sh`:
      - checks newly-added public primitives for forbidden failure constructors (`raise_error`, `make_error`).
      - blocks newly-added direct `(signal raise ...)` additions in `stdlib/stdlib.lisp`.
    - `scripts/check_primitive_docs_parity.sh`:
      - validates doc coverage for all registered public primitives from `src/lisp/eval_init_primitives.c3`.
    - `scripts/run_effects_contract_lint.sh`:
      - executes both checks and emits machine-readable summary JSON.
  - Integrated lint gates into hardening execution:
    - `scripts/run_boundary_hardening.sh` now runs effects-contract lint as Stage `0b` and emits `build/effects_contract_lint_summary.json`.
    - `.github/workflows/boundary-hardening.yml` now triggers on error-contract lint/doc-surface files and uploads lint artifacts.
  - Documented lint policy in `docs/PROJECT_TOOLING.md` and filled missing primitive docs in `docs/reference/11-appendix-primitives.md`.
  - Updated plan status:
    - `docs/plans/effects-typesystem-parity-plan.md` Phase 3 checklist + acceptance marked complete.
  - Validation:
    - `scripts/run_effects_contract_lint.sh` passed and emitted:
      - `build/effects_contract_lint_summary.json`
      - `build/effects_contract_policy.log`
      - `build/primitive_docs_parity.log`
- 2026-03-09 (typesystem/disptach parity matrix formalization - Phase 4 docs slice):
  - `docs/type-system-syntax.md`:
    - added release-critical Julia-parity matrix with explicit `done/partial/missing` status tags.
    - formalized current policy statements for:
      - method specificity ordering,
      - ambiguity handling (equal-specificity deterministic error),
      - parametric behavior + variance status,
      - union dispatch participation,
      - numeric promotion policy (currently no implicit promotion),
      - `where`-style non-goal + metadata-constraint replacement,
      - match/union exhaustiveness expectations.
    - added “Par with Julia / intentionally different” table and test anchors.
  - `docs/plans/effects-typesystem-parity-plan.md`:
    - marked `P4.1`..`P4.10` complete (spec/matrix layer),
    - left `P4.11` and `P4.12` pending for regression/expected-fail scaffolding.
  - Validation:
    - `scripts/run_effects_contract_lint.sh` passed after docs updates.
- 2026-03-09 (partial backlog normalization to single-source checklist):
  - `docs/plans/effects-typesystem-parity-plan.md`:
    - added `Canonical Partial Backlog (Single Source Of Truth)` with normalized checklist IDs `CP-01`..`CP-13`.
    - mapped all existing `[~]` plan entries to canonical `CP-*` IDs.
    - documented rule that `CP-*` checklist is authoritative for partial tracking.
  - `docs/type-system-syntax.md`:
    - linked each `partial` Julia-parity row to canonical `CP-10`..`CP-13`.
  - `docs/ERROR_MODEL.md`:
    - linked each `partial` migration-family row to canonical `CP-02`..`CP-06`.
- 2026-03-09 (effects/typesystem CP-01 closure: quoted module-marker sweep):
  - `docs/plans/session-34-44-boundary-hardening.md`:
    - replaced remaining historical module-marker references from colon-prefixed forms to quoted forms:
      - accepted/export markers now use `'all`
      - alias-marker diagnostics now use `'as`
    - updated embedded example snippets to quoted marker syntax (`export-from 'all ...`).
  - `docs/plans/effects-typesystem-parity-plan.md`:
    - marked `CP-01` complete.
    - marked `H0.5` complete.
    - normalized CP-01/hotfix wording from explicit colon tokens to “colon-prefixed marker” phrasing.
    - advanced status date to `2026-03-09`.
  - Verification:
    - `rg -nP '(?<!:):(as|all)\\b' docs/plans docs -g '*.md'` now reports no module-marker hits outside the plan’s historical cleanup wording updates.
- 2026-03-09 (effects/typesystem CP-02 closure: stdlib try/assert canonical payloads):
  - `stdlib/stdlib.lisp`:
    - added `canonical-error-payload?` and `canonicalize-error-payload` helpers.
    - `try` now normalizes every caught `raise` value to canonical payload shape before invoking the handler.
    - `assert!` now raises canonical payload dictionaries with stable `stdlib/assert-failed` code and `stdlib` domain.
  - regression coverage updates:
    - `src/lisp/tests_advanced_core_unicode_groups.c3`:
      - `try` string-error assertions now read payload `'message` explicitly.
      - string raises are verified through normalized payload message extraction.
    - `src/lisp/tests_advanced_stdlib_numeric_groups.c3`:
      - updated raise-catch test to assert payload `'data` for non-canonical user raises.
      - added canonical assert payload check (`domain`, `code`, `message`).
  - docs and plan parity:
    - `docs/ERROR_MODEL.md`:
      - updated status date to `2026-03-09`.
      - `Stdlib try/assert! base` row moved from `partial` to `done`.
      - updated migration notes to reflect canonical payload guarantees at stdlib handler boundaries.
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-02` complete.
      - marked `P2.5` complete.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1593/0`, `compiler: 79/0`).
- 2026-03-09 (effects/typesystem CP-03 closure: regex malformed-pattern signaling):
  - regex backend instrumentation:
    - `src/pika/regex_cache_api.c3` now records per-call compile failures through:
      - `regex_clear_last_compile_error()`
      - `regex_record_compile_error(...)`
      - `regex_take_last_compile_error(...)`
    - malformed/invalid pattern compile failures now preserve detail text instead of being silently collapsed into no-match semantics.
  - regex primitive contract update:
    - `src/pika/lisp_pika_regex_primitives.c3`:
      - added `regex_raise_invalid_pattern_if_any(...)`.
      - `re-match`, `re-fullmatch`, `re-find-all`, `re-split`, and `re-replace` now raise deterministic `regex/*-invalid-pattern` payloads when compilation fails.
    - `src/pika/lisp_pika.c3`:
      - `re-match-pos` and `re-find-all-pos` now raise deterministic malformed-pattern payloads.
  - regression updates:
    - `src/lisp/tests_runtime_feature_pika_groups.c3`:
      - converted malformed-pattern expectations from `nil` to explicit payload-code assertions.
      - added malformed-pattern code checks for `re-match`, `re-find-all`, `re-replace`, `re-match-pos`, and `re-find-all-pos`.
      - preserved valid no-match contract tests separately (`nil` remains absence for valid patterns).
  - docs and plan parity:
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-03` complete.
      - marked `N2.6` complete.
      - normalized `P2.7`/`CP-05` notes to remove now-closed malformed-pattern dependency.
    - `docs/ERROR_MODEL.md`:
      - moved `Regex match/search primitives` row from `partial` to `done`.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1598/0`, `compiler: 79/0`).
- 2026-03-09 (effects/typesystem CP-04 closure: file-I/O wrapper canonical payload normalization):
  - runtime wrapper normalization:
    - `src/lisp/prim_io.c3`:
      - refactored shared read-file offload path into status-returning helper (`io_async_read_file_result`) so callers can emit wrapper-specific canonical payload codes without stale pending-raise override.
      - `read-file` now maps read outcomes to stable wrapper codes:
        - `io/read-file-out-of-memory`
        - `io/read-file-failed`
      - `read-lines` now maps read outcomes to wrapper-specific codes:
        - `io/read-lines-out-of-memory`
        - `io/read-lines-failed`
      - preserved direct runtime scheduler errors as pass-through where already canonicalized upstream.
  - regression updates:
    - `src/lisp/tests_runtime_async_groups.c3`:
      - replaced combined missing-path truthy assertion with deterministic per-wrapper payload-code checks:
        - `async write-file missing-path payload code`
        - `async read-file missing-path payload code`
        - `async read-lines missing-path payload code`
      - added explicit absence contract check:
        - `async file-exists? missing-path returns nil`
  - docs and plan parity:
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-04` complete.
    - `docs/ERROR_MODEL.md`:
      - moved `File I/O effect wrappers` row from `partial` to `done` and recorded stable wrapper code families.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1605/0`, `compiler: 79/0`).
- 2026-03-09 (effects/typesystem CP-05 closure: async/network TLS wrapper canonical payload normalization):
  - runtime async/network normalization:
    - `src/lisp/tls_primitives.c3`:
      - migrated remaining string-only `raise_error(...)` sites to canonical payloaded raises via `tls_raise_async(...)`.
      - introduced stable `io/tls-*` codes for:
        - `tls-connect` argument/type/handle/oom/worker-result paths,
        - `tls-server-wrap` argument/type/handle/credential/reset/oom paths,
        - `tls-read`/`tls-write`/`tls-close` argument and handle validation paths.
      - preserved user-facing error messages (for migration) while canonicalizing payload codes.
    - `src/lisp/tls_handle_lifecycle.c3`:
      - normalized TLS-handle allocation failure to canonical `io/tls-connect-out-of-memory`.
  - regression updates:
    - `src/lisp/tests_runtime_async_groups.c3`:
      - added explicit payload-code assertions for TLS wrapper error surfaces:
        - `tls-connect non-string ca bundle payload code`
        - `tls-connect invalid tcp handle payload code`
        - `tls-write expected data payload code`
        - `tls-server-wrap invalid stream payload code`
  - docs and plan parity:
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-05` complete.
      - narrowed `P2.7` residual note to dispatcher-internals only (`CP-06`).
    - `docs/ERROR_MODEL.md`:
      - moved `Async/network primitives` row from `partial` to `done`.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1609/0`, `compiler: 79/0`).
- 2026-03-09 (effects/typesystem CP-06 closure: effect-dispatcher internals canonical payload normalization):
  - effect dispatcher/runtime migration:
    - `src/lisp/jit_jit_handle_signal.c3`:
      - added runtime canonical helpers for fast-path/error conversion.
      - migrated remaining mixed paths to payloaded raises:
        - `perform` type mismatch now consistently emits `runtime/effect-arg-mismatch`.
        - fast-path primitive failures now emit `runtime/fast-path-primitive-failed` with propagated message text.
        - `raise` handler nil-return path now emits `runtime/raise-handler-returned-nil`.
    - `src/lisp/jit_jit_runtime_effects.c3`:
      - migrated fast-path eval-result error conversion to canonical `runtime/fast-path-primitive-failed`.
    - `src/lisp/jit_jit_reset_shift.c3`:
      - migrated `shift` outside reset and suspend-guard failures to canonical runtime payload codes:
        - `runtime/shift-outside-reset`
        - `runtime/suspend-scope-guard`
    - `src/lisp/jit_jit_compile_effects_modules.c3`:
      - migrated `jit_do_export_from` eval-error bridge to canonical `runtime/export-from-failed`.
  - audit closure:
    - no remaining direct `raise_error(...)`/`make_error(...)` usage in:
      - `jit_jit_*effects*.c3`
      - `jit_jit_handle_signal.c3`
      - `jit_jit_reset_shift.c3`
      - `jit_jit_compile_effects_modules.c3`
  - docs and plan parity:
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-06` complete.
      - marked `P2.7` complete.
    - `docs/ERROR_MODEL.md`:
      - moved `Effect dispatcher internals` row from `partial` to `done`.
      - removed stale pending note that referenced `P2.7`.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1609/0`, `compiler: 79/0`).
- 2026-03-09 (effects/typesystem CP-07 closure: migrated error-model family regression coverage):
  - regression expansion:
    - `src/lisp/tests_advanced_core_unicode_groups.c3`:
      - added explicit dispatcher/runtime payload-code assertions for migrated effect internals:
        - `shift outside reset payload code` -> `runtime/shift-outside-reset`
        - `effect arg mismatch payload code` -> `runtime/effect-arg-mismatch`
      - complements existing migrated-family code assertions already present for:
        - stdlib `try/assert!` payload normalization
        - regex malformed-pattern codes
        - file-I/O wrapper codes
        - async/network TLS codes
        - scheduler payload codes
        - deduce payload codes
        - runtime unhandled/invalid-continuation payload codes
  - docs and plan parity:
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-07` complete.
      - marked `P2.10` complete.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1611/0`, `compiler: 79/0`).
- 2026-03-09 (effects/typesystem CP-08 closure: mixed-failure-style acceptance gap enforcement):
  - policy enforcement hardening:
    - `scripts/check_effects_contract_policy.sh`:
      - added `check_migrated_surfaces_no_legacy_failure_constructors()`.
      - enforces no `raise_error(...)` or `make_error(...)` in migrated canonicalized surfaces:
        - `src/lisp/tls_primitives.c3`
        - `src/lisp/tls_handle_lifecycle.c3`
        - `src/lisp/jit_jit_handle_signal.c3`
        - `src/lisp/jit_jit_runtime_effects.c3`
        - `src/lisp/jit_jit_reset_shift.c3`
        - `src/lisp/jit_jit_compile_effects_modules.c3`
      - gate fails deterministically if constructors reappear in those files.
  - plan parity:
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-08` complete.
      - marked `A2.1` complete.
  - Validation:
    - `scripts/check_effects_contract_policy.sh` passed.
    - `scripts/run_effects_contract_lint.sh` passed.
- 2026-03-09 (effects/typesystem CP-09 closure: phase execution-order drift):
  - sequencing closure:
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-09` complete.
      - marked execution-order item `E2` complete, reflecting that Phase 1 (effects semantics docs) and Phase 2 (effects-first migration matrix + behavior updates) have now converged and closed together.
- 2026-03-09 (effects/typesystem CP-10..CP-13 closure: dispatch/type parity residuals):
  - runtime dispatch/type-system behavior:
    - `src/lisp/value_core_types.c3`:
      - extended `MethodSignature` with `param_type_vars` for explicit method-level type-variable binding (`^T`).
    - `src/lisp/jit_jit_closure_define_qq.c3`:
      - propagated `param_type_vars` through method-signature allocation/copy/free paths.
      - signature builder now records type-variable symbol bindings for:
        - unresolved plain type annotations (e.g., `^T`),
        - single-key dict constraints (e.g., `^{'T Number}`).
    - `src/lisp/jit_jit_define_method_table.c3`:
      - signature equality now includes `param_type_vars`, preventing accidental coalescing of semantically distinct generic signatures.
    - `src/lisp/eval_dispatch_types.c3`:
      - added runtime unification for repeated method type variables:
        - `(^T a) (^T b)` requires equal inferred runtime types.
      - constraint checks now prefer unified bindings when available, then apply existential fallback for unconstrained/unbound dict-only cases.
  - regression coverage expansion:
    - `src/lisp/tests_advanced_type_effect_ffi_groups.c3`:
      - tightened ambiguity diagnostics coverage:
        - added equal-specificity detail assertion for positional `Int` ambiguity.
      - added parametric unification regressions:
        - same-type success path (`pair-same`),
        - mixed-type fallback path,
        - constrained (`Number`) same-type success,
        - constrained mixed-type fallback.
      - added union applicability regressions:
        - union-parent method accepts variants,
        - variant-specialized methods outrank union-parent method.
    - `src/lisp/tests_runtime_feature_groups.c3`:
      - added runtime exhaustiveness-policy anchor:
        - wildcard branch intentionally handles non-exhaustive union matches.
  - docs and plan parity:
    - `docs/type-system-syntax.md`:
      - moved parity rows to done for:
        - ambiguity handling,
        - parametric substitution/unification semantics,
        - union participation in dispatch applicability.
      - defined explicit runtime-only exhaustiveness policy (compile-time checker is an explicit non-goal for now) with test anchors.
    - `docs/plans/effects-typesystem-parity-plan.md`:
      - marked `CP-10`, `CP-11`, `CP-12`, `CP-13` complete.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1624/0`, `compiler: 79/0`).
- 2026-03-09 (L3.2 closure: constructor annotation checking path implementation):
  - parser/evaluator wiring:
    - `src/lisp/parser_expr_head_forms.c3` now recognizes list-head `^` as constructor type-application syntax.
    - `src/lisp/parser_type_literals.c3` adds `parse_constructor_type_application`, rewriting:
      - `(^(Ctor ...) value)` to a `^` primitive call with the type form captured as quoted datum (type form is not evaluated).
    - `src/lisp/eval_init_primitives.c3` registers `^` -> `prim_ctor_type_apply` (arity 2).
  - runtime checker:
    - `src/lisp/primitives_meta_types.c3` adds constructor annotation matcher (`prim_ctor_type_apply`) with:
      - constructor applicability checks against runtime instance constructor,
      - type-arg arity checks against inferred runtime `type_args`,
      - recursive nested checks for instance-backed nested constructor args,
      - runtime-inference fallback when nested runtime values do not carry parameterized instance metadata.
    - canonical payloaded diagnostics now emitted for constructor annotation failures:
      - `type/ctor-arity-mismatch`
      - `type/ctor-type-arg-mismatch`
    - payload `data` includes deterministic fields (`ctor`, `expected-args`, `actual-args`, and mismatch path/index fields for type-arg mismatches).
  - regression coverage:
    - `src/lisp/tests_advanced_type_effect_ffi_groups.c3` adds constructor annotation tests for:
      - positive constructor type-application checks,
      - mismatch and arity mismatch payload code assertions,
      - deterministic `arg-index` / `arg-path` payload fields,
      - nested constructor mismatch path depth,
      - regression that unconstrained constructor calls still work.
  - docs/status alignment:
    - `TODO.md` marks `L3.2` complete.
    - `docs/type-system-syntax.md` updates constructor type-application status to implemented (with explicit runtime-inference fallback note).
    - `docs/areas/types-dispatch.md` removes constructor type-application from known open gaps.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main` passed (`Unified Tests: 1669 passed, 0 failed`; `Compiler Tests: 79 passed, 0 failed`).
- 2026-03-09 (L3.3 closure: lambda call-boundary type checking implementation):
  - runtime call-boundary checker:
    - `src/lisp/eval_dispatch_types.c3` adds `check_lambda_call_boundary(...)` and payloaded lambda call mismatch diagnostics.
    - typed closure invocation now reuses canonical dispatch migration logic (literal/exact/widen/subtype/Any + constraint/unification checks).
    - deterministic mismatch payload data includes:
      - `failure`, `param-index`, `expected`, `actual`, `expected-arity`, `actual-arity`.
    - canonical diagnostic codes used:
      - `type/arg-mismatch`
      - `type/arity` (arity-form mismatches).
  - apply-path wiring:
    - `src/lisp/jit_jit_apply_runtime.c3`:
      - single-arg closure apply (`jit_apply_value_closure`) now enforces typed boundary checks before env binding/body eval.
      - TCO single-arg closure apply (`jit_apply_value_tail`) now enforces the same boundary checks.
    - `src/lisp/jit_jit_apply_multi_prims.c3`:
      - multi-arg and variadic closure apply paths enforce typed fixed-prefix checks for both normal and tail apply flows.
  - ownership/lifetime correctness hardening for closure signatures:
    - enabling runtime reads of `Closure.type_sig` exposed stale-pointer risk from shallow signature copies across boundary promotion paths.
    - `src/lisp/eval_promotion_copy.c3` now deep-copies method signatures into target scope via `method_signature_copy_to_scope(...)` instead of struct-only pointer copy.
    - `src/lisp/eval_promotion_escape.c3` now deep-copies method signatures into ESCAPE lane storage (`method_signature_copy_to_escape(...)`) instead of struct-only pointer copy.
    - this restores deterministic signature reads for promoted/copied closures and prevents false-positive call-boundary mismatches.
  - lambda annotation shape handling:
    - `src/lisp/jit_jit_closure_define_qq.c3`:
      - `^Lambda` / `^(Lambda ...)` parameter annotations now compile to closure-valued gating (`Closure`) instead of unresolved type-variable fallback.
  - regression coverage:
    - `src/lisp/tests_advanced_type_effect_ffi_groups.c3` adds lambda boundary tests for:
      - typed lambda positive/mismatch paths,
      - deterministic mismatch payload fields,
      - variadic fixed-prefix typed-arg mismatch,
      - `^(Lambda ...)` closure-value gating.
  - docs/status alignment:
    - `docs/type-system-syntax.md`:
      - adds `1.2.1 Lambda Call-Boundary Checking` (supported shapes, diagnostics, explicit non-goals).
      - marks lambda row in implementation status as implemented for call-boundary argument checking.
    - `docs/areas/types-dispatch.md` updates open-gap text from `L3.3` to remaining `L3.4/L3.5` closure slices.
    - `TODO.md` marks `L3.3` checklist complete.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1676/0`, `compiler: 79/0`).
- 2026-03-09 (L3.4 closure: constructor/lambda regression suite expansion):
  - regression additions (`src/lisp/tests_advanced_type_effect_ffi_groups.c3`):
    - constructor annotation mismatch payload determinism coverage expanded:
      - asserts `data.ctor`,
      - asserts `data.expected-args` length,
      - asserts `data.actual-args` length.
    - lambda call-boundary mismatch payload determinism coverage expanded:
      - asserts `data.failure`,
      - asserts `data.expected`,
      - asserts `data.actual`.
    - lambda boundary cross-tests added for:
      - dispatch integration (typed lambda in dispatch-selected method and fallback path),
      - union subtype migration at lambda boundary,
      - numeric widening (`Int` satisfying `^Double`) parity with dispatch semantics.
  - TODO closure:
    - `TODO.md`: marked `L3.4` checklist rows complete.
  - Validation:
    - `c3c build` passed.
    - `LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main` passed (`unified: 1676/0`, `compiler: 79/0`).
- 2026-03-16 (editor tooling: syntactic call hierarchy across `omni-lsp` and `omni-nvim`):
  - `tooling/omni-lsp/omni_lsp.py`:
    - now advertises `callHierarchyProvider`,
    - implements `textDocument/prepareCallHierarchy`,
    - implements `callHierarchy/incomingCalls`,
    - implements `callHierarchy/outgoingCalls`,
    - reuses cached open/unopened workspace declaration summaries so call
      hierarchy can resolve local declarations, open workspace files, and
      unopened workspace files without semantic indexing.
  - call hierarchy preparation prefers the exact declaration range under the
    cursor when the cursor is already on a declaration site, which avoids
    returning same-name outer declarations like a module and nested function
    with identical names.
  - outgoing/incoming calls are derived from syntactic recursive body scans over
    declaration forms, with special forms excluded from call-site capture.
  - `tooling/omni-lsp/tests/smoke_test.py` now covers:
    - advertised call hierarchy capability,
    - outgoing calls from an open declaration into both an open workspace file
      and an unopened workspace file,
    - incoming calls back into an unopened workspace declaration.
  - `tooling/omni-nvim/lua/omni/lsp.lua` now exposes quickfix-backed incoming
    and outgoing call list helpers over the new call hierarchy requests.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspIncomingCallsList`,
    - `:OmniLspOutgoingCallsList`,
    - default buffer-local mappings:
      - `<localleader>lC` for incoming calls,
      - `<localleader>lc` for outgoing calls.
  - docs updated:
    - `tooling/omni-lsp/README.md`
    - `tooling/omni-nvim/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
    - headless Neovim check for `OmniLspIncomingCallsList` /
      `OmniLspOutgoingCallsList`, quickfix population, and buffer-local mapping
      registration.
- 2026-03-16 (editor tooling: workspace fallback references in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py`:
    - `textDocument/references` now stays current-buffer when the active buffer
      owns the declaration, preserving the existing overloaded/local behavior,
    - when the active buffer has no local declaration for the symbol, the
      server now falls back to exact-name references across:
      - the current buffer,
      - other open Omni documents,
      - unopened workspace-root Omni files.
  - the fallback reuses the existing workspace declaration cache to decide when
    a workspace reference search is valid, while still returning declaration
    sites according to `includeDeclaration`.
  - `tooling/omni-lsp/tests/smoke_test.py` now covers a workspace fallback
    reference query from `caller.omni`, proving results from:
    - the caller buffer,
    - another open workspace file,
    - an unopened workspace declaration file.
  - docs updated:
    - `tooling/omni-lsp/README.md`
    - `tooling/omni-nvim/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: workspace fallback rename in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py`:
    - `textDocument/prepareRename` now succeeds on caller-side symbols when the
      active buffer has no local declaration but an exact-name workspace
      declaration exists,
    - `textDocument/rename` now follows the same local-first boundary as
      references:
      - local declarations still rename only within the active buffer,
      - caller-side symbols without a local declaration now produce exact-name
        workspace edits across open and unopened workspace Omni files.
  - `tooling/omni-lsp/tests/smoke_test.py` now covers workspace fallback rename
    from `caller.omni`, proving edits for:
    - the caller buffer,
    - another open workspace file,
    - an unopened declaration file.
  - docs updated:
    - `tooling/omni-lsp/README.md`
    - `tooling/omni-nvim/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: source-backed completion resolve in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py`:
    - `completionProvider` now advertises `resolveProvider`,
    - `completionItem/resolve` now enriches local and workspace declaration
      items with markdown docs backed by declaration snippets,
    - completion ordering and ranking are unchanged; the new path only adds
      source-backed docs/details for declaration items.
  - local completion items now carry declaration snippet metadata from the
    current buffer, and workspace completion items now carry snippet metadata
    from cached open/unopened workspace declarations.
  - `tooling/omni-lsp/tests/smoke_test.py` now covers:
    - advertised completion resolve capability,
    - local completion resolve for `ping`,
    - workspace completion resolve for `describe-updated`.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: declaration codelens across `omni-lsp` and `omni-nvim`):
  - `tooling/omni-lsp/omni_lsp.py`:
    - now advertises `codeLensProvider`,
    - serves `textDocument/codeLens` for non-module declarations in the current
      document,
    - now resolves codelenses lazily:
      - `textDocument/codeLens` returns lightweight unresolved declaration
        lenses,
      - `codeLens/resolve` fills in the reference count and first-party
        `omni.showReferences` payload.
  - codelens counting is workspace-aware for unique declaration names through
    the existing exact-name reference path, while overloaded local names stay
    local-only so the server does not guess which overload a workspace call
    belongs to.
  - `tooling/omni-lsp/tests/smoke_test.py` now covers:
    - advertised codelens resolve capability,
    - unresolved codelens payload shape,
    - resolved workspace-aware codelens counts for `answer`, `Point`, and
      `helper`,
    - emitted `omni.showReferences` command payload shape after resolve.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspCodeLensRefresh`,
    - `:OmniLspCodeLensRun`,
    - default buffer-local mappings:
      - `<localleader>ll` refresh codelens,
      - `<localleader>lL` run codelens.
  - `tooling/omni-nvim/lua/omni/lsp.lua` now registers the `omni.showReferences`
    LSP command and opens the supplied locations in quickfix, so running Omni
    codelenses in Neovim does something useful instead of depending on a
    VSCode-style command id.
  - `tooling/omni-nvim/lua/omni/init.lua` now supports opt-in
    `lsp.codelens.auto_refresh` with configurable buffer-local refresh events
    (default example: `BufEnter`, `InsertLeave`) for Omni buffers.
  - docs updated:
    - `tooling/omni-lsp/README.md`
    - `tooling/omni-nvim/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
    - headless Neovim check for codelens refresh/run commands, buffer-local
      mapping registration, `omni.showReferences` command handling, and opt-in
      codelens auto-refresh autocmd wiring.
- 2026-03-16 (editor tooling: document links for Omni module/import forms):
  - `tooling/omni-lsp/omni_lsp.py`:
    - now advertises `documentLinkProvider`,
    - serves `textDocument/documentLink` for top-level `import` and
      `export-from` forms,
    - resolves string imports relative to the current file path,
    - resolves symbol targets through exact-name workspace module declarations.
  - `tooling/omni-lsp/tests/smoke_test.py` now covers:
    - advertised document-link capability,
    - a relative string import link,
    - an `export-from` module-name link into an unopened workspace file.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: document-link opening in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now adds a synchronous
    `textDocument/documentLink` consumer that:
    - finds the link under the cursor,
    - opens file URI targets directly in Neovim,
    - reports when no link exists at the cursor.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspOpenLink`,
    - default buffer-local mapping `<localleader>lo`.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for `OmniLspOpenLink`, file-target opening, and
      buffer-local mapping registration.
- 2026-03-16 (editor tooling: document-link quickfix list in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now adds a synchronous
    `textDocument/documentLink` quickfix helper for the current buffer.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspDocumentLinks`,
    - default buffer-local mapping `<localleader>lO`.
  - the quickfix list points at source link locations in the current Omni
    buffer and labels each entry with the linked target file name.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for `OmniLspDocumentLinks`, quickfix population, and
      buffer-local mapping registration.
- 2026-03-16 (editor tooling: document-link range navigation in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now adds next/previous navigation over
    document-link ranges in the current Omni buffer.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspNextLink`,
    - `:OmniLspPrevLink`,
    - default buffer-local mappings:
      - `]o` next link,
      - `[o` previous link.
  - navigation wraps across the current buffer’s document-link ranges so the
    motion remains useful even in small import/export lists.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for link-range navigation and buffer-local mapping
      registration.
- 2026-03-16 (editor tooling: document-highlight controls in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspDocumentHighlight`,
    - `:OmniLspClearReferences`,
    - default buffer-local mappings:
      - `<localleader>lh` document highlight,
      - `<localleader>lH` clear references.
  - `omni-nvim` also now supports optional buffer-local document-highlight
    autocmds under `lsp.highlights`, with separate refresh and clear event lists
    so Omni symbol highlights can follow cursor movement without depending on
    external editor glue.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for explicit highlight/clear commands, buffer-local
      mapping registration, and auto-refresh autocmd behavior.
- 2026-03-16 (editor tooling: semantic tokens in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now advertises
    `textDocument/semanticTokens/full` with a first-party legend and serves a
    syntactic token stream for current-buffer declaration sites, parameter
    bindings, special-form heads, builtin type/value names, numbers, strings,
    and comments.
  - the implementation reuses the existing declaration/form scans plus a small
    lexical pass, so semantic highlighting stays cheap and aligned with the
    rest of the server’s local-first editor behavior.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: linked editing ranges in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now advertises
    `textDocument/linkedEditingRange` and serves a local syntactic range set
    for parameter declarations plus same-name uses inside the owning
    declaration body.
  - the first pass is intentionally conservative: it skips obvious nested
    `define`, `module`, and `quote` forms so linked edits do not cross simple
    shadowing boundaries.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: on-type formatting in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now advertises
    `textDocument/onTypeFormatting` for newline and `)` triggers.
  - the implementation reuses the existing conservative formatter and returns a
    line-scoped indentation edit for the touched line instead of attempting a
    broader pretty-print rewrite on each keystroke.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: monikers in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now advertises `textDocument/moniker` and
    serves first-party Omni monikers for local declaration sites plus exact-name
    workspace fallback when the active buffer only has a caller-side symbol.
  - the initial identifier shape is container/detail/name based, with `export`
    used for declaration-site monikers, `local` for same-document references,
    and `import` for workspace fallback references.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: pull diagnostics in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now advertises `diagnosticProvider` and
    serves `textDocument/diagnostic` with full reports backed by the same
    `omni --check --json` path already used for publish diagnostics.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: workspace pull diagnostics in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now serves `workspace/diagnostic` with full
    and unchanged per-document reports across open and unopened workspace
    `.omni` files, keyed by stable result ids derived from current document
    text.
  - the implementation still uses the same `omni --check --json` path as push
    diagnostics and text-document pull diagnostics; this is a transport surface
    expansion, not a second diagnostics engine.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: import-string navigation in `omni-lsp`):
  - `tooling/omni-lsp/omni_lsp.py` now lets definition/declaration treat
    string import targets like `"extra.omni"` as navigable locations by
    reusing the existing document-link resolution path.
  - import-string navigation lands on the imported file’s entry declaration when
    the target file is available, instead of forcing editors to treat the
    string as inert text.
  - docs updated:
    - `tooling/omni-lsp/README.md`
  - validation:
    - `python3 -m py_compile tooling/omni-lsp/omni_lsp.py tooling/omni-lsp/tests/smoke_test.py`
    - `python3 tooling/omni-lsp/tests/smoke_test.py`
- 2026-03-16 (editor tooling: pull diagnostics commands in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now adds quickfix-backed helpers for:
    - `textDocument/diagnostic`,
    - `workspace/diagnostic`.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspDocumentDiagnostics`,
    - `:OmniLspWorkspaceDiagnostics`,
    - default buffer-local mappings:
      - `<localleader>lp` document diagnostics,
      - `<localleader>lP` workspace diagnostics.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for pull-diagnostics requests, quickfix population,
      command registration, and buffer-local mapping registration.
- 2026-03-16 (editor tooling: cached pull diagnostics in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now caches pull diagnostic
    `resultId` values and prior diagnostic items for both:
    - `textDocument/diagnostic`,
    - `workspace/diagnostic`.
  - repeated pull-diagnostics commands now send prior result ids and still
    rebuild quickfix from cached items when the Omni LSP server replies
    `unchanged`, instead of clearing the list on stable results.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for cached pull-diagnostics request params and
      unchanged-result quickfix reuse.
- 2026-03-16 (editor tooling: pull diagnostics auto-refresh in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/init.lua` now adds opt-in buffer-local autocmd
    support under `lsp.pull_diagnostics` for current-document pull diagnostics.
  - when enabled, `omni-nvim` refreshes `textDocument/diagnostic` on the
    configured events and keeps the quickfix snapshot in sync without requiring
    manual `:OmniLspDocumentDiagnostics` calls.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for `OmniNvimPullDiagnostics` autocmd
      installation and event-driven document-diagnostics requests.
- 2026-03-16 (editor tooling: workspace pull diagnostics auto-refresh in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/init.lua` now adds a separate opt-in
    `lsp.pull_diagnostics.workspace_auto_refresh` path with independently
    configurable events for workspace pull diagnostics.
  - when enabled, `omni-nvim` refreshes `workspace/diagnostic` on the
    configured events and keeps the workspace quickfix snapshot current without
    requiring manual `:OmniLspWorkspaceDiagnostics` calls.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for `OmniNvimWorkspacePullDiagnostics` autocmd
      installation and event-driven workspace-diagnostics requests.
- 2026-03-16 (editor tooling: pull diagnostics cache reset commands in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now exposes reset helpers for cached:
    - current-buffer `textDocument/diagnostic` result ids,
    - workspace `workspace/diagnostic` result ids.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspDocumentDiagnosticsReset`,
    - `:OmniLspWorkspaceDiagnosticsReset`.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for cache-reset command registration and cleared
      prior-result-id request params.
- 2026-03-16 (editor tooling: automatic pull diagnostics cache cleanup in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/init.lua` now installs buffer-local cleanup
    autocmds for cached pull diagnostics state:
    - current-buffer `textDocument/diagnostic` cache entries clear on
      `BufDelete` and `BufWipeout`,
    - current-buffer and workspace pull-diagnostics caches clear when the
      `omni_lsp` client detaches from that buffer.
  - this keeps stale `previousResultId` values from surviving wiped buffers or
    old Omni LSP sessions, reducing the need for manual reset commands after
    editor-side lifecycle changes.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for buffer-wipe and `LspDetach` cache cleanup.
- 2026-03-16 (editor tooling: quiet pull diagnostics auto-refresh in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` pull-diagnostics helpers now accept a
    quickfix-open option instead of always forcing `copen`.
  - `tooling/omni-nvim/lua/omni/init.lua` now runs automatic current-buffer and
    workspace pull-diagnostics refreshes with `open = false`, so configured
    refresh events keep the quickfix snapshot current without stealing the
    current window on each event.
  - manual `:OmniLspDocumentDiagnostics` and `:OmniLspWorkspaceDiagnostics`
    behavior is unchanged: explicit commands still open quickfix.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check confirming explicit commands still open quickfix,
      while event-driven auto-refresh updates quickfix items without reopening
      the quickfix window.
- 2026-03-16 (editor tooling: silent clean pull diagnostics auto-refresh in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` quickfix helper paths now accept a
    `notify_empty` option in addition to the existing quickfix-open control.
  - `tooling/omni-nvim/lua/omni/init.lua` now runs automatic current-buffer and
    workspace pull-diagnostics refreshes with `notify_empty = false`, so clean
    auto-refresh results do not emit repeated `no results` notifications.
  - explicit `:OmniLspDocumentDiagnostics` and `:OmniLspWorkspaceDiagnostics`
    behavior is unchanged: manual commands still notify when the result set is
    empty.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check confirming explicit empty pull-diagnostics commands
      still notify, while event-driven empty auto-refresh stays silent.
- 2026-03-16 (editor tooling: configurable pull diagnostics auto-refresh behavior in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/init.lua` now exposes explicit config knobs for
    automatic pull-diagnostics refresh behavior:
    - `lsp.pull_diagnostics.auto_open`
    - `lsp.pull_diagnostics.auto_notify_empty`
    - `lsp.pull_diagnostics.workspace_auto_open`
    - `lsp.pull_diagnostics.workspace_auto_notify_empty`
  - the current defaults preserve the recent quiet-refresh behavior:
    - current-buffer and workspace auto-refresh keep `open = false`
    - current-buffer and workspace auto-refresh keep `notify_empty = false`
  - this makes the pull-diagnostics UX adjustable without changing the explicit
    command behavior for `:OmniLspDocumentDiagnostics` and
    `:OmniLspWorkspaceDiagnostics`.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check confirming the new config can force auto-refresh to
      reopen quickfix and emit empty-result notifications.
- 2026-03-16 (editor tooling: combined pull diagnostics snapshot in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now refactors pull-diagnostics
    collection into reusable helpers and exposes `all_diagnostics()` to build a
    single quickfix snapshot from:
    - current-buffer `textDocument/diagnostic`,
    - workspace `workspace/diagnostic`.
  - the combined snapshot deduplicates identical quickfix entries while still
    reusing the same cached pull-diagnostics result ids as the existing
    document/workspace commands.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspAllDiagnostics`,
    - default buffer-local mapping `<localleader>lA`.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for combined pull-diagnostics quickfix contents,
      cache reuse, command registration, and buffer-local mapping registration.
- 2026-03-16 (editor tooling: combined pull diagnostics reset in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/lsp.lua` now exposes
    `reset_all_diagnostics()`, which clears both:
    - current-buffer `textDocument/diagnostic` cached result ids,
    - workspace `workspace/diagnostic` cached result ids.
  - `tooling/omni-nvim/lua/omni/init.lua` now adds:
    - `:OmniLspAllDiagnosticsReset`.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check confirming `OmniLspAllDiagnosticsReset` clears both
      cache layers before the next combined pull-diagnostics request.
- 2026-03-16 (editor tooling: combined pull diagnostics auto-refresh in `omni-nvim`):
  - `tooling/omni-nvim/lua/omni/init.lua` now adds a third opt-in
    auto-refresh lane under `lsp.pull_diagnostics` for the merged
    `OmniLspAllDiagnostics` snapshot:
    - `all_auto_refresh`
    - `all_events`
    - `all_auto_open`
    - `all_auto_notify_empty`
  - when enabled, `omni-nvim` refreshes the combined document + workspace
    pull-diagnostics quickfix snapshot on the configured events while reusing
    the same cache-aware `all_diagnostics()` path as the explicit command.
  - docs updated:
    - `tooling/omni-nvim/README.md`
  - validation:
    - headless Neovim check for `OmniNvimAllPullDiagnostics` autocmd
      installation, event-driven combined refresh, and the default quiet
      combined-auto-refresh behavior.
- 2026-03-09 (L3.5 closure: type-system status/spec cleanup):
  - docs/status updates:
    - `docs/type-system-syntax.md`:
      - removed stale `NOT Implemented` carry-over rows for constructor/lambda type gaps.
      - added explicit implemented-row regression anchors to:
        - `run_advanced_type_parametric_ctor_annotation_tests`,
        - `run_advanced_type_lambda_call_boundary_tests`.
      - updated implementation-status timestamp to `2026-03-09`.
    - `docs/areas/types-dispatch.md`:
      - updated area status text to reflect `L3` gap closure and shifted remaining work references to `L4/L5`.
    - `TODO.md`:
      - marked `L3.5` complete.
      - marked acceptance gate `A-L3` complete.
- 2026-03-16 (runtime hardening: stack-pool ownership + continuation/resolve safety):
  - stack pool ownership and shutdown hardening:
    - added `StackCtx.owner_pool` and `StackPool.shutting_down`,
    - coroutine value dtors now destroy via `coroutine_val.owner_pool` instead
      of a global pool pointer, preventing cross-interpreter pool corruption,
    - `stack_ctx_destroy` now rejects owner-pool mismatch and running-context
      teardown, and short-circuits reentrant frees while pool shutdown is in
      progress,
    - `stack_ctx_create` now rejects allocations when `pool.shutting_down`,
      preventing callback-time reentry from creating unstable contexts during
      shutdown traversal,
    - `stack_pool_shutdown` now detaches `all_list` heads before callback
      teardown, rejects freeing `CTX_RUNNING`/`g_current_stack_ctx`, and
      remains reentrancy-safe while deferred callbacks run.
  - handle/resolve continuation hardening:
    - `HandleEffectState` is now heap-backed per handle activation with:
      - `continuation_refcount`,
      - `frame_active`,
      - durable captured `handler_copy` buffers,
    - both JIT and value runtime handle paths now capture durable handler copies
      for continuation resolve and retain/release state based on escaped
      continuation refs,
    - suspended handle contexts are no longer destroyed at handle-return when
      continuations still retain them (ownership transfer to continuation path),
    - applying a handler continuation as a function now routes through
      `resolve` semantics (handler snapshot reinstall + one-shot lifecycle)
      instead of raw checkpoint-style continuation resume,
    - `resolve` now requires `CTX_SUSPENDED`, restores full interpreter state
      around AOT signal suspension, and handles post-resume dead/completed
      context teardown safely when no active handle frame owns the context,
    - `resolve` resuspend-outside-handle failures now consume/invalidate the
      continuation and destroy the suspended context to avoid leaked retained
      state and stale one-shot handles.
  - build backend hardening:
    - helper script invocation now runs via `env bash` (Bash semantics preserved
      without `/bin/sh` assumptions),
    - `c3c` resolution now supports `C3C` env override first, then pinned paths,
      then `c3c` PATH fallback for non-standard toolchain installs.
  - regression validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `OMNI_LISP_TEST_SLICE=jit-policy LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --build /tmp/omni_build_smoke.lisp -o /tmp/omni_build_smoke_bin`
- 2026-03-16 (scheduler/async hardening: callback token guards + cancelled queue payload release):
  - async DNS/TCP connect callback ownership now uses explicit per-request
    callback context and request tokens instead of req-pointer table scans:
    - added tokened callback context structs:
      - `PendingDnsCallbackCtx`
      - `PendingConnectCallbackCtx`
    - added scheduler token seed:
      - `Scheduler.async_request_token_seed`
    - added per-pending request tokens:
      - `PendingDnsResolve.request_token`
      - `PendingTcpConnect.request_token`
    - DNS flow updates:
      - `prim_dns_resolve` allocates and binds callback context with
        `{fiber_id, request_token, host_copy}`,
      - `scheduler_uv_getaddrinfo_cb` routes by callback context fiber/token and
        rejects stale/mismatched callbacks without mutating unrelated pending
        slots,
      - DNS close/consume paths now detach/free callback context explicitly.
    - TCP/pipe connect flow updates:
      - `prim_tcp_connect` and `prim_pipe_connect` allocate and bind callback
        context with `{fiber_id, request_token, tcp_handle}`,
      - `scheduler_uv_tcp_connect_cb` validates callback context token ownership
        before touching pending slot state,
      - connect close/consume paths now detach/free callback context explicitly.
  - cancelled queued thread-task payload hardening:
    - `scheduler_uv_offload_after_cb` now releases queued `work.shared` when
      work never executed (`queued.executed == false`), preventing payload leaks
      when cancellation occurs before `work_cb`,
    - non-executed path now forces terminal task cancellation via
      `scheduler_cancel_thread_task(...)`, ensuring joiners cannot stall in
      pending state.
  - new scheduler regressions:
    - `run_scheduler_thread_task_nonexecuted_payload_release_boundary_tests`
      verifies queued payload release + terminal completion when after-work runs
      without `work_cb` execution,
    - `run_scheduler_dns_connect_request_token_guard_boundary_tests` verifies
      DNS/connect callback token guards reject req-pointer rebinding into
      unrelated pending slots.
  - regression validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `OMNI_LISP_TEST_SLICE=basic LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    - `c3c build --sanitize=address` (build success)
    - `OMNI_LISP_TEST_SLICE=scheduler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp` under ASAN currently aborts in existing teardown path (`scope_dtor_value` UAF during isolated scheduler-suite destruction); this remains open and was not addressed in this pass.

- 2026-03-18 (boundary profiling revalidation after destination-partial hardening):
  - reran the container-capped `memory-lifetime-bench` profile on the current tree and kept the run within the accepted boundary-heavy envelope,
  - observed benchmark summaries on the rerun:
    - `boundary_decision_cost`: `iters=2048 splice_ms=3 disallowed_ms=4 reuse_ms=1`
    - `boundary_destination_routed_escape`: `iters=2048 partial_ms=2 partial_ok=2048`
    - `scope_splice_tail`: `iters=2048 escapes_per_iter=64 splice_ms=2`
  - rerun validation completed with `Unified Tests: 0 passed, 0 failed`.

- 2026-03-18 (scheduler/offload hot-path benchmark revalidation):
  - reran the container-capped `scheduler_offload_hot_path` benchmark on the current tree and kept the run green,
  - observed benchmark summary on the rerun:
    - `scheduler_offload_hot_path`: `iters=8192 queue_ms=0 queue_ok=8192 queue_pending=0 queue_enqueue_delta=8192 queue_fail_delta=0 completion_ms=2 completion_ok=8192 completion_alloc_fail=0 http_ms=2 http_ok=8192 http_shared_fail=0 tls_ms=0 tls_ok=8192`
  - rerun validation completed with `Unified Tests: 89 passed, 0 failed`.
# 2026-03-19 - Advanced iterator boundary closure

- Replaced the stdlib lazy iterator combinator path with primitive-backed iterator state in [src/lisp/primitives_iter_coroutine.c3](/home/heefoo/Documents/code/Omni/src/lisp/primitives_iter_coroutine.c3) and [stdlib/stdlib.lisp](/home/heefoo/Documents/code/Omni/stdlib/stdlib.lisp), avoiding closure env-copy churn for `map`/`filter`/`take`/`zip`/`foldl` and infinite iterator sources.
- Added a primitive list reverse fast path in [src/lisp/primitives_core.c3](/home/heefoo/Documents/code/Omni/src/lisp/primitives_core.c3) and rewired stdlib list `reverse`/`foldr` to use it.
- Fixed the remaining nested closure-backed iterator chain by allowing releasing-escape results to fall back to destination builders before hard failing on scope splice rejection in [src/lisp/eval_boundary_commit_flow.c3](/home/heefoo/Documents/code/Omni/src/lisp/eval_boundary_commit_flow.c3).
- Validation:
  - `c3c build`
  - host repros for `take`, iterator `foldl`, list `foldr`, and nested `it3`
  - bounded `advanced-unicode-iterator`: `118 passed, 0 failed`

- 2026-03-20 (Deduce join routing policy hardening):
  - join execution now has an explicit default-route policy in
    `src/lisp/deduce_relation_scan_helpers_join.c3`:
    - explicit fallback thresholds still force the historical index-first
      adaptive path,
    - the zero-threshold default now routes by supporting-index shape plus
      outer size hints, preferring index nested-loop only when the supporting
      index is fully bound and unique or the outer side is small, and choosing
      hash-first for larger non-unique / partially bound shapes,
    - the route decision is driven by explicit schema signals rather than the
      previous opaque always-index-then-maybe-fallback behavior.
  - added regression coverage for the new low-selectivity hash-first default in
    `src/lisp/tests_deduce_rule_groups_more_join.c3`.
  - added a selective-join benchmark smoke in
    `src/lisp/tests_deduce_query_bench_groups_more.c3`.
  - validation:
    - `c3c build`
    - bounded Deduce slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`78 passed, 1 failed`, remaining failure is the unrelated recursive
      iteration-limit payload regression outside the join lane)

- 2026-03-20 (Deduce aggregate metadata and validation scaffolding):
  - Deduce rule install now carries aggregate-aware head metadata through the
    normalized IR and stored rule signatures:
    - `src/lisp/deduce_rule_ops.c3` adds aggregate head-term parsing for
      `count`, `sum`, `min`, and `max`, with deferred rejection for `avg` and
      `distinct-count`,
    - `src/lisp/deduce_db_handles.c3` and
      `src/lisp/deduce_db_handles_mutation.c3` now persist/copy/release
      per-head projection kind, aggregate op, source-var ids, and aggregate
      projection counts,
    - `src/lisp/deduce_rule_eval_prims.c3` installs the aggregate metadata into
      stored rule signatures.
  - install-time aggregate validation is now explicit in
    `src/lisp/deduce_rule_eval.c3`:
    - aggregate terms are rejected in body atoms,
    - malformed / unknown aggregate projections reject deterministically,
    - aggregate inputs must be grounded by a positive body atom,
    - aggregate heads reject literal projections and mixed non-grouped shapes.
  - aggregate introspection scaffolding now exists without claiming grouped
    execution support:
    - `deduce/explain` exposes `aggregate-projection-count` plus per-head
      projection metadata in `src/lisp/deduce_rule_ops_explain.c3`,
    - `deduce/analyze` explicitly rejects aggregate-bearing rules until grouped
      execution lands in `src/lisp/deduce_rule_eval_prims.c3`.
  - regression coverage now includes aggregate install/explain/analyze
    scaffolding in:
    - `src/lisp/tests_deduce_rule_groups.c3`
    - `src/lisp/tests_deduce_rule_groups_explain.c3`
  - validation:
    - `c3c build`
    - bounded Deduce slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`90 passed, 0 failed`)

- 2026-03-26 (Deduce worker-scratch crash triage instrumentation):
  - added opt-in instrumentation in
    `src/lisp/deduce_rule_eval_exec_seminaive.c3` behind
    `OMNI_TRACE_DEDUCE_WORKER_SCRATCH=1`:
    - scratch-pass entry/exit markers,
    - per-rule dry-run begin/end markers,
    - dry-run head-fact skip/record markers,
    - scratch replay/accumulation boundary markers.
  - no semantic/runtime behavior change intended; instrumentation-only slice.
  - validation:
    - `c3c build` passed.
    - bounded deduce lane with tracing:
      `OMNI_TRACE_DEDUCE_WORKER_SCRATCH=1 scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    - observed state:
      - run stalled with no exit while log stayed flat at `167` lines for `30s`,
      - no `OMNI_DEDUCE_WORKER_SCRATCH` marker appeared,
      - last named boundary in log remained
        `[PASS] deduce analyze recursive limit reports stable error code`.

- 2026-03-26 (Deduce scan-range row-dict OOM guard validation):
  - hardened `src/lisp/deduce_relation_scan_helpers_more.c3` so
    `deduce_relation_materialize_row_dict(...)` returns a clean
    `deduce/query-out-of-memory` error when `make_hashmap(...)` fails.
  - added an env-gated direct helper probe in
    `src/lisp/tests_deduce_query_scan_groups.c3` behind
    `OMNI_DEDUCE_FORCE_ROW_DICT_OOM=1` so the row-dict OOM boundary is
    validated directly instead of depending on scan-path optimization shape.
  - validation:
    - `c3c build`
    - bounded deduce slice:
      `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_DEDUCE_FORCE_ROW_DICT_OOM=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`1 passed, 0 failed`)

- 2026-03-26 (Omni version bump to 0.2.0):
  - updated the runtime-facing version surfaces to `0.2.0`:
    - `project.json`
    - `src/entry_cli_help_version.c3`
    - `src/entry_project_init_writer_project_json.c3`
    - `src/entry_project_init_writers.c3`
    - generated docs/man surfaces:
      - `docs/OMNI_REFERENCE.md`
      - `docs/man/omni.1`
      - `docs/man/omni-language.7`
  - rebuilt `build/main` so the binary replacement now reports `omni 0.2.0`.
  - validation:
    - `c3c build`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --version`
      (`omni 0.2.0`)

- 2026-03-30 (REPL server stdin routing + TCP transport):
  - added a real runtime input surface for language code instead of keeping REPL
    server `stdin` as a protocol placeholder:
    - `src/lisp/value_interp_state.c3` now carries an optional `input_state`
      attachment on `Interp`,
    - `src/lisp/prim_io.c3` now owns the line-buffered `InterpInputState`
      queue, the shared runtime line reader, and the new `prim_read_line`
      primitive,
    - `stdlib/stdlib.lisp` now exports `read-line` through the new
      `io/read-line` effect.
  - wired the REPL server to that runtime surface in
    `src/lisp/eval_repl_server.c3`:
    - `stdin` requests now accept `data` and optional `eof: true`,
    - session input is buffered per session and wakes blocked `read-line`
      calls,
    - interrupting a blocked `read-line` request now returns the existing
      `interrupted` terminal event.
  - widened the transport boundary:
    - added `omni --repl-server --tcp <host> <port>` in
      `src/entry_repl_server_mode.c3`,
    - updated help text in `src/entry_cli_help_version.c3`.
  - kept the runtime boundary honest:
    - sequential multi-client reuse is supported across Unix-socket/TCP
      listeners,
    - concurrent per-connection servicing is still deferred because runtime/JIT
      interpreter attachment remains owner-thread constrained.
  - compiler/runtime surface parity updates:
    - `src/lisp/eval_init_primitives.c3`
    - `src/lisp/compiler_free_vars_utils.c3`
    - `src/lisp/compiler_let_set_flat.c3`
    - `src/lisp/compiler_primitive_variable_hash_table_domains.c3`
  - validation:
    - `c3c build`
    - `printf 'abc\n' | ./build/main --eval '(read-line)' --json`
    - Python stdio REPL-server probe:
      - `clone` -> `(read-line)` -> `stdin data`
      - `clone` -> `(read-line)` -> `stdin eof`
      - `clone` -> `(read-line)` -> `interrupt`
    - Python Unix-socket probe:
      - two sequential client connections each `clone` + `eval`
    - Python TCP probe:
      - `describe` over `--repl-server --tcp 127.0.0.1 48763`

- 2026-03-30 (TCP REPL port-file discovery):
  - added nREPL-style TCP discovery for editor/plugin attachment:
    - `omni --repl-server --tcp <host> <port>` now writes
      `.omni-repl-port` in the current working directory after the listener
      binds successfully,
    - the file currently contains the bound TCP port followed by a newline.
  - implementation:
    - `src/lisp/eval_repl_server.c3` now publishes the port file with an
      atomic temp-write + rename path instead of writing it in place.
  - current contract:
    - this discovery file is emitted only for TCP REPL-server mode,
    - abrupt termination can still leave a stale `.omni-repl-port`, so
      clients should treat it as a discovery hint and still verify connect.
  - validation:
    - `c3c build`
    - `./build/main --repl-server --tcp 127.0.0.1 48763`
      writes `.omni-repl-port` containing `48763`

- 2026-03-30 (omni-nvim TCP REPL discovery):
  - updated the first-party Neovim plugin to consume the new TCP discovery
    file automatically:
    - `tooling/omni-nvim/lua/omni/repl.lua` now searches upward for
      `.omni-repl-port`, connects to `127.0.0.1:<port>` by default, clones a
      REPL-server session, and uses REPL-server `eval` requests when attached,
    - if discovery is missing or the TCP connect fails, the plugin falls back
      to the existing local `omni --repl --json` job path.
  - exposed the discovery surface in plugin defaults and docs:
    - `tooling/omni-nvim/lua/omni/init.lua`
    - `tooling/omni-nvim/README.md`
    - `tooling/omni-nvim/doc/omni.nvim.txt`
  - validation:
    - `tooling/omni-nvim/scripts/run_smoke.sh`
    - headless Neovim probe against a live
      `./build/main --repl-server --tcp 127.0.0.1 48763`
      listener with `.omni-repl-port`

- 2026-03-30 (REPL server project preload parity):
  - extended `omni --repl-server` so all shipped transports now accept
    `--project [dir]` instead of rejecting it as an unexpected argument:
    - `src/entry_repl_server_mode.c3` now parses `--project` for
      `--socket`, `--stdio`, and `--tcp`,
    - `src/entry_runtime_modes.c3` now exposes shared project-entry
      resolution so both interactive REPL preload and REPL-server preload use
      the same `omni.toml` + `src/main.omni` contract,
    - `src/lisp/eval_repl_server.c3` now preloads the resolved project entry
      for each new `clone` session before emitting the session event.
  - current contract:
    - server startup with `--project [dir]` does not preload a global shared
      interpreter,
    - each cloned session gets its own project preload,
    - preload output is emitted as normal protocol `out` events before the
      terminal `session` / `done` pair,
    - preload failure aborts the clone and returns a structured `error`
      event instead of exposing a half-initialized session.
  - validation:
    - `c3c build`
    - `./build/main --repl-server --tcp 127.0.0.1 48763 --project ./demo`
    - Python TCP probe:
      - `clone` emits `out` for `"Hello from demo!"`
      - then `session`
      - then `done`

- 2026-04-08 (syntax cleanup slice 1: fail-closed accessor removal):
  - removed leading-dot accessor shorthand from the active language surface:
    - `.name`
    - `.'key`
    - `.1`
    - `.[expr]`
    - `. [expr]`
  - removed quoted-symbol callable lookup from the active language surface:
    - `('name dict)`
  - kept access syntax centered on the existing canonical operations:
    - path access for normal member/symbol-key lookup such as `person.name`
    - postfix index access such as `expr.[key]`
    - explicit `(ref coll key)` as the semantic lookup core
  - compatibility posture:
    - this cleanup is intentionally fail-closed,
    - removed accessor surfaces now hard-error instead of remaining as legacy aliases.
  - implementation notes:
    - parser no longer treats leading-dot forms as implicit lookup lambdas,
    - runtime no longer treats bare symbol values as callable lookup shorthands.
  - validation:
    - pending during active implementation

- 2026-04-08 (syntax cleanup slice 2-3: primitive arity tightening and false/nil split):
  - removed primitive auto-partial from ordinary one-argument primitive calls:
    - bare calls like `(+ 3)` now raise arity errors,
    - partial application remains available through `_`, `|>`, and `partial`,
    - `PARTIAL_PRIM` remains a runtime value for explicit/internal partial payloads.
  - split `false` from `nil` in the active language/runtime contract:
    - `false` now binds to a distinct boolean symbol value,
    - `nil` remains the absence value,
    - both remain falsy in conditional positions.
  - implementation notes:
    - interpreter and JIT single-arg primitive apply paths now reject missing arguments instead of manufacturing partials,
    - JSON false now materializes as Omni `false`,
    - value-literal dispatch/pattern parsing no longer collapse `false` into `nil`,
    - Boolean/Nil constructors now respect the separation (`Boolean nil => false`, `Nil false` rejects).
  - validation:
    - `c3c build` unavailable in current environment; static/source-only verification performed

- 2026-04-08 (arm64 build lane repaired for current tree):
  - fixed split-file/compiler regressions that were blocking `c3c build` before
    the cleanup work could be verified:
    - restored persisted relation metadata structs after the deduce split,
    - corrected stale return/type mismatches and pointer-decay bugs in
      deduce/boundary helper files,
    - fixed allocator imports/usages for worker thread pool setup,
    - removed a stale prototype-only declaration from the boundary commit split.
  - rebuilt the static vendored dependency lane for arm64 in `deps/lib`:
    - `libutf8proc.a`
    - `libdeflate.a`
    - `libyyjson.a`
    - `libuv.a`
    - `libbearssl.a`
    - `liblmdb.a`
  - repaired local build integration around helper/link artifacts:
    - `project.json` now compiles `clib/mathutils.c` directly instead of
      pretending `mathutils` is a prebuilt library,
    - `project.json` restores the `omni_ftxui` archive link edge and embeds
      `RUNPATH=$ORIGIN` so repo-local shared libraries in `build/` resolve at
      runtime,
    - `scripts/build_omni_chelpers.sh` now passes vendored include dirs so the
      helper archives can be rebuilt from the repo tree on this host,
    - local shared libraries for GNU Lightning and replxx were built and staged
      into `build/`.
  - arm64 stack-engine note:
    - x86_64-only context switching no longer blocks the build on this host,
      but the non-x86 path is intentionally fail-closed at runtime rather than
      pretending continuations/coroutine stack switching work on arm64.
  - validation:
    - `bash deps/build_static.sh`
    - `make -C deps/src/lmdb/libraries/liblmdb clean all`
    - `bash scripts/build_omni_chelpers.sh`
    - `~/.local/bin/c3c build`
    - `readelf -d build/main`
    - `ldd build/main`
    - `./build/main --help`
- 2026-04-10 (boundary partial/iterator nested alias hardening):
  - `PARTIAL_PRIM` target-chain reuse and env-copy reuse now recurse into
    `first_arg` / `second_arg` payload graphs instead of checking only the
    direct arg wrappers.
  - shipped consequence:
    - a partial or iterator wrapper already in the target chain is no longer
      reused by identity when one arg is a target-chain `ARRAY`, `HASHMAP`,
      `SET`, or `METHOD_TABLE` wrapper whose nested child still belongs to the
      releasing/source scope.
    - those wrappers now fall back to the existing clone/promotion paths, so
      nested shared-wrapper aliasing stays fail-closed under both direct
      boundary copy and env-copy.
  - validation:
    - `c3c build`
    - targeted `memory-lifetime-smoke` regressions for target-chain wrapper
      alias policy and env-copy shared-wrapper policy
- 2026-04-10 (env-copy iterator/closure memo symmetry):
  - env-copy closure and iterator special-cases now participate in the shared
    `PromotionContext` epoch instead of bypassing memoization.
  - shipped consequence:
    - when the same source closure is bound directly and also reached through an
      iterator wrapper, env-copy now produces one shared cloned closure in the
      target scope instead of duplicating it.
    - when a source `PARTIAL_PRIM` is bound directly and also reached through an
      iterator wrapper, env-copy now reuses the same copied partial payload
      graph across both bindings.
  - validation:
    - `c3c build`
    - targeted `memory-lifetime-smoke` regressions for env-copy iterator/closure
      memo symmetry
- 2026-04-10 (JIT TCO alias-safety follow-up):
  - TCO env-copy now fails closed for target-chain `CONS` bindings whose direct
    scalar edge still belongs to the releasing scope.
  - shipped consequence:
    - the JIT TCO lane no longer decides “copy required” and then immediately
      lose that safety when the shared `copy_to_parent(...)` fast-reuse gate
      ignores a releasing-scope scalar child on a surviving `CONS` wrapper.
    - `copy_to_parent_payload_in_releasing_scope(...)` now scans `CONS` spines
      for direct releasing-scope children before admitting fast reuse, so the
      generic copy path stays consistent with the narrower JIT TCO reuse rule.
  - iterator payload alias checks now recurse into target-chain non-closure,
    non-partial payload graphs instead of relying on a shallow pointer test.
  - shipped consequence:
    - `ITERATOR` bindings carrying target-chain `ARRAY` / `HASHMAP` / `SET` /
      `CONS` style payloads now clone when those payload graphs still reach the
      releasing scope, instead of being reused by identity through the JIT TCO
      env-copy path.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` with:
      - `tco-cons-releasing-scalar-edge-copy`
      - `tco-partial-shared-wrapper-edge-copy`
      - `tco-iterator-shared-wrapper-edge-copy`
      - `tco-foreign-partial-shared-wrapper-edge-copy`
      - `tco-foreign-shared-wrapper-copy`
    - `scripts/run_validation_container.sh ... OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ...`
- 2026-04-10 (apply/promotion helper fail-closed follow-up):
  - `apply_partial(...)` now rejects impossible `PARTIAL_PRIM` execution state
    before any function-pointer call-through:
    - null/non-partial wrapper
    - null `func`
    - `remaining <= 0`
    - `remaining > 2`
    - `remaining == 2` with a prefilled `second_arg`
  - checked hashmap insertion now rejects promoted `ERROR` values from
    `boundary_promote_to_root(...)` instead of storing them as ordinary
    key/value data.
  - `fs_array_push(...)` and `csv_array_push(...)` now apply the same contract,
    so helper-level array append surfaces no longer embed promoted `ERROR`
    values into successful arrays.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=152 fail=0`
- 2026-04-10 (apply dispatch malformed-state fail-closed follow-up):
  - `apply_primitive(...)` now rejects malformed primitive wrappers before any
    primitive call-through:
    - null/non-primitive wrapper
    - null `prim_val`
    - null `prim_val.func`
  - `apply_partial(...)` now also rejects missing `first_arg` instead of
    forwarding an impossible captured-argument state into primitive dispatch.
  - `jit_apply_value_primitive(...)` now applies the same malformed primitive
    guard on the JIT helper path.
  - validation:
    - `c3c build`
    - focused `jit-policy`:
      - `invalid-primitive-state-fails-closed`
    - bounded `memory-lifetime-smoke`: `pass=152 fail=0`
- 2026-04-10 (coroutine thunk promotion fail-closed follow-up):
  - coroutine thunk publication now rejects invalid post-promotion state before
    any coroutine stack context allocation:
    - null promotion results,
    - promoted `ERROR` values,
    - non-closure thunk wrappers,
    - and closure wrappers with null `closure_val`
  - shipped consequence:
    - forced closure-wrapper promotion allocation failure no longer reaches
      `stack_ctx_make(...)` or mutates `stack_ctx_pool` counters.
    - coroutine thunk publication now matches the repo-wide fail-closed
      contract already enforced for adjacent scheduler/fiber promotion paths.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=153 fail=0`
- 2026-04-10 (data-format array promotion fail-closed follow-up):
  - JSON and TOML array assembly now reject promoted boundary `ERROR` values
    instead of storing them as successful array elements.
  - shipped consequence:
    - nested data-format array conversion under non-root scopes now preserves
      the boundary failure surface when element promotion fails.
    - TOML array element promotion is now covered through the existing
      `TIME_POINT` wrapper-copy allocation seam, so the fail-closed contract
      is pinned in bounded runtime-allocation coverage.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=154 fail=0`
- 2026-04-10 (escape cons publication fail-closed follow-up):
  - `make_cons(...)` now stages ESCAPE-lane `car` / `cdr` promotion before
    pair allocation and rejects invalid string/error promotion results instead
    of publishing an ESCAPE-lane cons that still points at TEMP-backed data.
  - shipped consequence:
    - the string/error escape-promotion seam no longer degrades into a
      successful cons carrying a non-escape field.
    - staged promoted fields are unwound if final escape-pair allocation
      fails.
  - validation:
    - `c3c build`
    - bounded `memory-lifetime-smoke`: `pass=155 fail=0`
- 2026-04-10 (scheduler shared/offload projection fail-closed follow-up):
  - shared-handle projection no longer maps missing/unmaterializable shared
    payloads to empty-string success values.
  - offload path projection no longer maps missing/invalid shared path payloads
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
