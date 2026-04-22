# Active TODO Index Part 05

Source: `TODO.md`

- [x] `LANG-FOREIGN-RUNTIME-CORE-107` define a minimal common foreign runtime core
  for C, Python, Julia, CUDA/cuBLAS, and optional C++/polyglot adapters
  while keeping safety and ownership model boundaries clear
  - design note: `docs/plans/foreign-runtime-core-plan-2026-04-11.md`
    locks a narrow contract for:
    - `ForeignHandle`-centric resource representation,
    - shared runtime/capability descriptors,
    - dictionary+array reflection payloads (`'parameters [..]` canonical in metadata),
    - common conversion/release/invocation shape, and
    - adapter separation from public runtime semantics.
  - shipped slice: `FOREIGN-CORE-001` common handle descriptors and
    `foreign-describe`:
    - `FfiHandle` carries runtime kind, handle kind, and capability bits.
    - C ABI library and pointer-return handles are tagged through the common
      descriptor path.
    - `(foreign-describe handle)` returns a dictionary with quoted-symbol keys;
      `'capabilities` is an array of capability symbols.
  - shipped slice: `FOREIGN-CORE-002A` explicit release:
    - `(foreign-release handle)` closes releasable `ForeignHandle` payloads
      through the same finalizer/free path used by scope teardown.
    - release is idempotent for already-closed handles and rejected for ordinary
      non-releasable library handles.
  - shipped slice: `FOREIGN-CORE-002B` C ABI callable reflection:
    - `(foreign-describe ffi-bound-function)` returns a dictionary with
      `'type 'ForeignCallable`, `'runtime 'c-abi`, `'kind 'function`,
      `'parameters [..]`, `'returns [..]`, and call/reflect capabilities.
    - Parameter/return descriptors expose user-facing `'type` plus ABI
      `'abi-type`; `ForeignHandle` descriptors preserve name, ownership,
      nullability, and finalizer metadata.
  - shipped slice: `FOREIGN-CORE-002C` bindgen descriptor comments:
    - raw bindgen modules now emit `bindgen-descriptor` comments using the
      shared `ForeignCallable` descriptor vocabulary, while `bindgen-policy`
      comments remain explicit review hints and are not a runtime reflection
      source.
  - shipped slice: `FOREIGN-CORE-002D` internal runtime adapter dispatch:
    - `foreign-describe` and `foreign-release` now route through a minimal
      `ForeignRuntimeAdapter` operation boundary.
    - C ABI callable reflection and bound-function calls are registered through
      the C ABI adapter, while generic handle describe/release remains
      available for current runtime kinds until dedicated Python, Julia,
      CUDA/cuBLAS, optional C++ tooling, and polyglot adapters exist.
  - shipped slice: `FOREIGN-CORE-002E` explicit load/resolve adapter hooks:
    - C ABI lazy `dlopen` and `dlsym` now route through explicit `load_bound`
      and `resolve_bound` adapter slots before the C ABI `call_bound` reaches
      libffi.
    - The adapter shape also reserves a `tensor_buffer` slot behind
      `FOREIGN_CAP_TENSOR_BUFFER`; no C ABI Tensor buffer marshalling is
      registered until the BLAS/cuBLAS backend has a concrete ownership and
      layout contract.
  - shipped slice: `FOREIGN-CORE-002F` capability-gated import/member hooks:
    - `FOREIGN_CAP_IMPORT` and `FOREIGN_CAP_MEMBER` guard the new internal
      adapter behavior.
    - `import_module` and `resolve_member` were added as adapter slots, with
      `foreign_runtime_import_module` and `foreign_runtime_resolve_member`
      handling the internal dispatch.
    - No new public user primitive was added, and Python/Julia/CUDA behavior is
      still not wired yet.
  - shipped slice: `FOREIGN-CORE-002G` C ABI string-return cleanup:
    - `^String` FFI returns now copy non-null C `char*` returns into Omni
      `String` values and keep null returns as `nil`.
    - This removes the remaining raw-address integer leak from the string
      return path while preserving the documented `^String` contract.
  - shipped slice: `FOREIGN-CORE-002H` opaque handle name reflection cleanup:
    - `(foreign-describe handle)` now preserves opaque foreign-resource handle
      families such as `File` as symbols under `'name`, matching
      `^{'name File ...}` metadata.
    - C ABI library handles keep their soname/path `'name` as a string because
      that field is the `dlopen` target, not a resource-family symbol.
  - shipped slice: `FOREIGN-CORE-002I` borrowed-handle release regression
    alignment:
    - the borrowed return-handle regression now binds the real C `fopen`
      symbol with borrowed `ForeignHandle` metadata, closes it through
      `fclose`, and verifies `foreign-release` still rejects the
      non-releasable borrowed wrapper.
  - shipped slice: `FOREIGN-CORE-002J` single-release foreign handle authority:
    - mixed internal foreign handles with both a native finalizer and
      `free_lib_handle` now normalize to finalizer-owned release authority at
      construction, preventing finalizer-plus-free double release.
  - shipped slice: `FOREIGN-CORE-002K` manual return ownership preservation:
    - `ForeignHandle` return policies now preserve explicit ownership in
      returned boxes (including `manual`) instead of re-deriving ownership from
      finalizer presence.
    - `(foreign-describe)` preserves manual ownership for returned foreign
      handles.
    - manual-return handles remain non-releasable and are rejected by
      `(foreign-release ...)`.
    - AOT declaration policy rejects manual ownership combined with a
      finalizer, matching the interpreter metadata parser.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=69 fail=0`
  - shipped slice: `FOREIGN-CORE-002L` AOT finalizer ownership parity:
    - `aot_ffi_policy_from_spec` now rejects return policies that include a
      finalizer unless ownership is `owned`.
    - This closes the gap where borrowed/default finalizer metadata was accepted
      but ignored by finalizer resolution.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `compiler` slice with libclang: `pass=263 fail=0`
    - `git diff --check`
  - shipped slice: `FOREIGN-CORE-002M` finalizer-owned foreign payload release:
    - finalizer-backed handle wrappers now free their own heap payloads after
      resource-specific teardown because finalizer presence makes the finalizer
      path the release authority.
    - covered wrappers include UV timer callback, process, signal, TCP, UDP,
      FS stream, TLS, deduce relation, deduce database, and deduce transaction
      handles.
    - deduce relation scan forced column-key-value OOM handling now fails before
      allocating the relation cache, avoiding the sanitizer-only forced-OOM leak.
  - validation:
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - ASAN targeted `deduce` slice: `pass=330 fail=0`
    - final `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - host targeted `deduce` slice: `pass=330 fail=0`
    - `scripts/check_status_consistency.sh`
    - `git diff --check`
  - shipped slice: `FOREIGN-CORE-002N` construction-failure ownership:
    - `make_ffi_handle_ex_with_descriptor` releases finalizer/free-backed
      non-library payloads on both box-allocation and value-allocation failures.
    - C ABI library handles remain non-releasable and stay under the existing
      `dlopen` caller cleanup path on constructor failure.
    - owned C FFI pointer returns rely on constructor cleanup when wrapping the
      returned pointer fails instead of running a second caller-side finalizer.
    - TLS client/server wrapper OOM paths use `tls_handle_finalizer` for
      connected initialized handles so graceful close/session logic runs before
      nested TLS storage is freed.
    - allocation-failure tests now pin exactly-once finalizer cleanup across
      FFI handle box-allocation and value-allocation failures.
  - shipped slice: `FOREIGN-CORE-002O` constructor-failure caller cleanup
    alignment:
    - redundant caller-side cleanup after `make_ffi_handle_ex` null returns were
      removed now that constructor failure owns finalizer/free-backed
      non-library payload release.
    - covered async TCP/UDP/process/signal/UV timer callback/FS stream handles,
      scheduler task/thread handles, atomic refs, TLS wrapper error paths,
      deduce database/transaction/relation handles, and boundary helper tests.
    - malformed C ABI library descriptors with `FOREIGN_CAP_RELEASE` now have
      direct advanced FFI coverage proving `foreign-release` still rejects them.
      Opaque malformed handles with a release bit but no finalizer/free release
      authority are also rejected, keep their payload pointer, and are not
      reflected as owned.
    - released handles no longer reflect as owned after payload cleanup clears
      the live pointer and release capability.
    - C ABI/AOT library registration failures close the raw library handle,
      clear the root-scoped value pointer, and release the constructed wrapper
      box so later teardown cannot see a freed box.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=74 fail=0`
    - host targeted `compiler` slice: `pass=263 fail=0`
    - host targeted `deduce` slice: `pass=330 fail=0`
    - `c3c build --sanitize=address --warn-deprecation=no`
    - ASAN targeted `advanced-ffi-system` group: `pass=69 fail=0`
    - ASAN targeted `deduce` slice: `pass=330 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=216 fail=0`
    - final `c3c build --warn-deprecation=no`
    - `scripts/check_status_consistency.sh`
    - `git diff --check`
  - shipped slice: `FOREIGN-CORE-002P` capability reflection authority
    alignment:
    - `foreign-describe` now omits `'release` from reflected capabilities
      unless the handle has actual release authority, matching the
      `foreign-release` gate for malformed opaque handles.
    - the malformed opaque-handle regression now checks both rejection and
      metadata reflection, so a stray release bit cannot leak into the
      describe surface.
  - shipped slice: `FOREIGN-CORE-002Q` C ABI library capability reflection
    tightening:
    - C ABI library handles now reflect `[load resolve reflect]`, not `'call`,
      because the library handle itself is not directly callable.
    - Bound C ABI functions keep reflecting `'call` through their
      `ForeignCallable` descriptors.
  - shipped slice: `FOREIGN-CORE-002R` process-spawn cleanup failure-injection
    coverage:
    - async process-spawn tests now force `stdin`, `stdout`, and `stderr`
      wrapper allocation failures after process creation.
    - each failure path proves the opened pipes and process state are cleaned
      through the shared foreign-handle cleanup authority.
  - validation:
    - `c3c build --warn-deprecation=no`
    - host targeted `advanced-ffi-system` group: `pass=75 fail=0`
    - bounded container `memory-lifetime-smoke` slice: `pass=225 fail=0`
  - closure note:
    - the common-core lane is closed; non-C runtime adapters remain split under
      `FOREIGN-CORE-004`, and TLS offload/in-flight lifecycle coverage remains
      a separate runtime validation follow-up.
  - follow-up risk: avoid overloading the core with polyglot host/guest API
    semantics before C ABI path, `ForeignHandle`, and reflection payload contract
    are stable.

- [x] `LANG-TENSOR-BACKEND-BOUNDARY-092` define the optional Tensor
  library/backend boundary
  - source plan: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`
  - closure evidence:
    - `TENSOR-080` is closed as a design/contract slice only; no backend
      runtime code landed.
    - keep BLAS/LAPACK/CUDA/cuBLAS as optional build/runtime acceleration
      behind the pure `Tensor` fallback.
    - use explicit `ForeignHandle` ownership and finalizer policy for native
      library/context handles.
    - do not add public `matmul`, backend-specific tensor types, or implicit
      CPU/GPU transfer semantics.

- [x] `LANG-TENSOR-NATIVE-BLAS-LAPACK-095` close optional native BLAS/LAPACK
  execution backend baseline behind `contract`/`realize`
  - source plan: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`
  - shipped slice: `TENSOR-090A` added optional runtime `cblas_dgemm`
    discovery and a private rank-2 `[1 0]` `Float64` fast path.
  - shipped slice: `TENSOR-090B` extended that private `dgemm` path with
    transpose flags for all contiguous rank-2 single-axis layouts:
    `[1 0]`, `[0 0]`, `[1 1]`, and `[0 1]`.
  - shipped slice: `TENSOR-090C` added optional runtime `cblas_dgemv`
    discovery and private rank-2/rank-1 plus rank-1/rank-2 `Float64` fast
    paths.
  - shipped slice: `TENSOR-090D` added optional runtime `cblas_ddot`
    discovery and a private rank-1/rank-1 `Float64` vector dot fast path.
  - shipped slice: `TENSOR-090E` added optional runtime `cblas_dger`
    discovery and a private rank-1/rank-1 `Float64` outer-product fast path.
  - closure note:
    - solver/decomposition naming and backend coverage were later resolved
      through the `matrix/*` surfaces and `TENSOR-090` follow-up slices;
    - any remaining native BLAS/LAPACK work should reopen as a concrete
      operation-specific item with focused pure-fallback regressions;
    - do not add public `matmul`, backend-specific Tensor types, or
      `ForeignHandle`-backed ordinary Tensor storage.
  - latest validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build main --output-dir build --build-dir build/obj2`
    - host targeted `advanced-collections-module` group: `pass=298 fail=0`
    - bounded container targeted `advanced-collections-module` group:
      `pass=298 fail=0`
    - direct transpose-backed smokes returned `84.0`, `68.0`, and `123.0`

- [x] `LANG-TENSOR-BROADCASTING-093` decide singleton-axis Tensor
  broadcasting semantics
  - source plan: `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`
  - closure evidence:
    - `TENSOR-070` implements right-aligned singleton-axis broadcasting for
      tensor-tensor `map`.
    - rank-0 tensors broadcast as tensor scalars, and incompatible shapes fail
      with `tensor/shape-mismatch`.
    - exact-shape tensor-tensor behavior remains the primary fast path.
    - validation:
      - `c3c build --warn-deprecation=no`
      - host targeted `advanced-collections-module` group: `pass=212 fail=0`

- [x] `LANG-TENSOR-LAZY-EDGE-ROLLBACK-094` add a deterministic regression
  for lazy tensor-expression child edge rollback
  - closure evidence:
    - `src/lisp/value_tensor.c3` now exposes deterministic tensor expression
      edge fail counters for copy-to-parent and escape-promotion paths.
    - `src/lisp/tests_tests.c3` resets those counters between isolated test
      groups.
    - memory-lifetime coverage now fails the third lazy `map` child-edge copy
      after a prior tensor child copy has committed, and verifies the partial
      copied tensor edge is cleaned from the destination scope.
    - root promotion of lazy Tensor expressions now promotes tensor operand
      edges into the destination ESCAPE lane and keeps shared dispatch method
      tables out of Tensor expression ownership graph-audit checks.
    - lazy closure-mapped Tensor promotion and copy-to-parent closure env-scope
      parent chains now detach from released child scopes.
    - commit-scoped graph audit now rejects releasing-child TEMP edges and stops
      at terminal/global env frames.
  - validation:
    - bounded container graph-audit repro for lazy Tensor return: `12.0` with no
      boundary violation output
    - bounded container `memory-lifetime-smoke` slice: `pass=222 fail=0`
    - bounded ASAN container `memory-lifetime-smoke` slice: `pass=222 fail=0`

- [x] `MEM-GRAPH-AUDIT-DIAGNOSTIC-20260422-001` eliminate or deliberately
  classify the current `memory-lifetime-smoke` boundary graph-audit diagnostic
  output.
  - surface/runtime classification: runtime memory/lifetime diagnostic,
    targeted until the emitting test and boundary path are isolated.
  - blocker or task: `scripts/run_validation_container.sh
    scripts/run_global_gates.sh` currently passes, but the
    `memory-lifetime-smoke` slice prints `reachable Omni edge enters TEMP` and
    `pre-splice escape root reachability violation` graph-audit diagnostics
    while reporting `fail=0`.
  - closed/classified 2026-04-22: the diagnostic is expected output from the
    passing negative regression `lifetime: root splice debug audit rejects
    releasing temp edge` in
    `src/lisp/tests_memory_lifetime_boundary_groups.c3`, not the older lazy
    Tensor return repro.
  - evidence: the test deliberately builds a child-scope ESCAPE `CONS` root
    whose `car` is a child-scope TEMP `INT`; `root_tag=4` is `CONS` and
    `violating_edge_tag=1` is `INT`, matching the emitted diagnostic and the
    intended pre-splice rejection path.
  - validation: `scripts/run_validation_container.sh bash -lc 'env
    LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_SUMMARY=1
    OMNI_TEST_VERBOSE=1 OMNI_SKIP_TLS_INTEGRATION=1
    OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp
    2>&1 | grep -n -E "\[boundary\]\[graph-audit\]|root splice debug audit
    rejects releasing temp edge|OMNI_TEST_SUMMARY suite"'` showed the two
    graph-audit lines immediately followed by the passing root-splice debug
    audit test and final `pass=237 fail=0`.
  - prerequisites: bounded-container execution path and current `build/main`;
    do not run host-heavy memory-lifetime gates directly.
  - negative-memory constraint: do not interpret current
    `memory-lifetime-smoke` graph-audit output as a Tensor/lazy-return
    regression. It is a deliberately emitted negative boundary-splice
    diagnostic unless the neighboring pass line or tag pattern changes.

- [x] `AUDIT-ERROR-PAYLOAD-PROCESS-WAIT-112` skip unconsumed structured error
  payloads and clean process-wait partial result dictionaries
  - closure evidence:
    - `src/lisp/value_constructors.c3` now builds structured raise payloads
      only when a matching `raise` handler can consume them, avoiding
      root-scoped payload dictionaries for ordinary unhandled error returns.
    - `src/lisp/async_process_signal_dns_process.c3` now cleans the
      partially built `process-wait` result hashmap if insertion fails.
    - `src/lisp/value_constructors.c3` now also cleans a partially built
      root-scoped raise payload dictionary if payload key/value construction
      fails mid-build.
    - regression coverage forces a `process-wait` insertion failure and
      verifies root hashmap destructor count returns to the pre-call value.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded container `memory-lifetime-smoke` slice: `pass=215 fail=0`

- [x] `TENSOR-060H` clean nested lazy Tensor materialization child temporaries
  - closure evidence:
    - `src/lisp/prim_tensor.c3` now tracks the concrete child values produced
      while resolving nested lazy Tensor `map`/`contract` operands and cleans
      them after evaluating the parent expression.
    - regression coverage materializes `(map + (map + tensor 1) 2)` and
      verifies only the top-level materialized Tensor remains live until
      caller cleanup.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded container `memory-lifetime-smoke` slice: `pass=212 fail=0`

- [x] `TENSOR-060I` clean failed lazy Tensor materialization result
  - closure evidence:
    - `src/lisp/prim_tensor.c3` now cleans fresh concrete Tensor
      materialization results through the generic fresh-value path rather than
      treating the lazy source graph as expression edges on the concrete
      result.
    - regression coverage verifies nested lazy `contract` materialization and
      failed lazy `map` materialization leave active tensor destructor counts
      unchanged after cleanup.
  - validation:
    - `c3c build --warn-deprecation=no`
    - bounded container `memory-lifetime-smoke` slice: `pass=215 fail=0`
