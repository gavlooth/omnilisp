  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `TENSOR_DTYPE_BIG_INTEGER`, dtype metadata, owned element cleanup,
    scalar-handle cleanup for lazy maps, concrete storage copy, and clone
    support.
  - Extended `Tensor` dtype parsing and constructors for
    `(Tensor BigInteger shape data-or-scalar)`, `(Tensor data BigInteger)`,
    and `(Tensor BigInteger data)`.
  - Added BigInteger Tensor `ref`, `(Array tensor)`, `(List tensor)`, scalar
    `realize` fill, concrete tensor copy, lazy `map`, and pure C3 `contract`
    paths.
  - Added focused advanced collection/module tests for BigInteger dtype/ref,
    inferred prefix/suffix construction, flat conversions, scalar fill, copy,
    map, broadcast, return/closure boundaries, contract, and mixed/inexact
    rejection.
  - Updated `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `memory/CHANGELOG.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct smokes for BigInteger dtype/ref, inferred prefix/suffix
    constructors, flat collection conversion, scalar fill, concrete copy,
    map, contract, and inexact-data rejection
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Host focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=295 fail=0`.
  - Bounded container focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=295 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - `git diff --check` passed.
  - ASAN build attempt failed before compile with the local C3 compiler
    sanitizer platform message.
  - BLAS fast paths remain `Double`-only; BigInteger contracts use the pure C3
    fallback and preserve exact integer results.
- Invalidated assumptions or failed approaches:
  - Treating precision Tensor work as BigFloat-only is now stale; BigInteger is
    a native Tensor dtype with storage, map, and contract support.
- Current best recommendation:
  - Use the pure C3 Tensor fallback as the semantic oracle for additional
    precision dtypes. Continue to BigComplex Tensor storage/kernels only when
    complex scientific tensor workflows become the active priority.
- Unresolved issues:
  - No BigInteger Tensor runtime blocker remains from this slice.
  - ASAN coverage remains unavailable through the local C3 compiler invocation.
- Next actions:
  - Commit and push this BigInteger Tensor slice.

Signature: GPT-5 Codex

## 2026-04-15 12:31 CEST - Native BigFloat Tensor Contract
- Objective attempted:
  - Complete the BigFloat Tensor arithmetic kernel lane by adding summed-axis
    contraction support after native storage and elementwise `map`.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended tensor-dispatched `contract` from `Double`-only evaluation to
    native `BigFloat` evaluation through the pure C3 contraction fallback.
  - Kept private BLAS `dgemm`/`dgemv` fast paths `Double`-only; BigFloat
    contracts use owned BigFloat sum/product handles and preserve Tensor dtype.
  - Preserved deterministic mixed tensor dtype rejection:
    `Double`/`BigFloat` tensor-tensor `contract` still raises
    `tensor/dtype-mismatch`.
  - Added focused advanced collections/module regressions for BigFloat vector
    dot, rank-2 matrix product, zero-size contracted-axis identity, explicit
    destination realization, return-boundary survival, closure-capture
    survival, and mixed-dtype rejection.
  - Updated `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `memory/CHANGELOG.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct smokes for BigFloat dot, matrix product, zero-size identity,
    destination realization, return-boundary survival, closure-capture
    survival, and mixed-dtype rejection
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - attempted `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - BigFloat vector dot returns `"55"` for `[1.5 2] . [10 20]`.
  - BigFloat rank-2 matrix product returns `"154"` for the existing Tensor
    matrix-product smoke shape.
  - BigFloat zero-size contracted-axis identity returns `"0"`.
  - Explicit destination `realize` works for BigFloat contract expressions.
  - Lazy BigFloat contract expressions survive function return and closure
    capture.
  - Focused advanced collections/module group passed on host and bounded
    container: `271 passed, 0 failed`.
  - Bounded `memory-lifetime-smoke` passed: `225 passed, 0 failed`.
  - ASAN validation could not run: the local C3 compiler rejected the
    sanitizer build before compilation with `Address sanitizer is only
    supported on Linux, FreeBSD, NetBSD, Darwin and Windows.`
- Invalidated assumptions or failed approaches worth preserving:
  - The previous checkpoint statement that BigFloat Tensor `contract` is
    unimplemented is now stale for pure C3 Tensor kernels. It remains true for
    accelerated BLAS-style BigFloat backends and for other unimplemented Tensor
    storage dtypes.
- Unresolved issues:
  - BigInteger and BigComplex Tensor storage dtypes remain unshipped.
  - BigFloat Tensor contracts are pure C3 and not BLAS-accelerated.
  - ASAN still cannot run in this environment because the C3 compiler rejects
    sanitizer builds before producing a binary.
- Current best recommendation:
  - Resume the optional LAPACK/LAPACKE solver/decomposition naming decision for
    `Double` Tensor convenience APIs, or start a separate dtype-storage slice
    for BigInteger/BigComplex Tensor support if high-precision non-real Tensor
    work is prioritized.
Signature: GPT-5 Codex

## 2026-04-15 12:25 CEST - Native BigFloat Tensor Map
- Objective attempted:
  - Move BigFloat Tensor support beyond storage/ref/conversion into the
    elementwise Tensor operation surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended tensor-dispatched `map` from `Double`-only evaluation to native
    `BigFloat` evaluation for unary, tensor-scalar, scalar-tensor,
    exact-shape tensor-tensor, and right-aligned singleton-axis broadcast
    cases.
  - Added owned BigFloat scalar handles to lazy map payloads and cloned them
    during Tensor payload clone/promotion paths so BigFloat scalar operands
    survive function-return and closure-capture boundaries.
  - Preserved deterministic mixed tensor dtype rejection:
    `Double`/`BigFloat` tensor-tensor `map` still raises
    `tensor/dtype-mismatch`.
  - Left `contract` `Double`-only; BigFloat contraction kernels remain a
    separate implementation slice.
  - Added focused advanced collections/module regressions for BigFloat unary
    map outside Double range, scalar-left/right map, broadcast map,
    destination realization, return-boundary survival, closure-capture
    survival, and mixed-dtype rejection.
  - Updated `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `memory/CHANGELOG.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct smokes for BigFloat unary preservation, scalar-left/right map,
    broadcast map, destination realization, return-boundary survival,
    closure-capture survival, and mixed-dtype rejection
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - attempted `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - `(String (ref (map (lambda (x) x) (Tensor [(BigFloat "1e309")] BigFloat)) [0]))`
    returns `"1e+309"`.
  - BigFloat tensor-scalar and scalar-tensor map cases return BigFloat values.
  - BigFloat tensor-tensor map supports right-aligned singleton-axis broadcast.
  - Explicit destination `realize` works for mapped BigFloat expressions.
  - Lazy BigFloat map expressions survive function return and closure capture.
  - Focused advanced collections/module group passed on host and bounded
    container: `264 passed, 0 failed`.
  - Bounded `memory-lifetime-smoke` passed: `225 passed, 0 failed`.
  - ASAN validation could not run: the local C3 compiler rejected the
    sanitizer build before compilation with `Address sanitizer is only
    supported on Linux, FreeBSD, NetBSD, Darwin and Windows.`
- Invalidated assumptions or failed approaches worth preserving:
  - The previous checkpoint statement that BigFloat Tensor `map` is unshipped
    is now stale. It remains true only for BigFloat `contract` kernels and for
    other unimplemented Tensor dtypes.
- Unresolved issues:
  - BigFloat Tensor `contract` kernels are not implemented.
  - BigInteger and BigComplex Tensor storage dtypes remain unshipped.
  - ASAN still cannot run in this environment because the C3 compiler rejects
    the sanitizer build before producing a binary.
- Current best recommendation:
  - Continue with BigFloat Tensor `contract` only if high-precision reductions
    are the next priority. Otherwise resume the optional LAPACK/LAPACKE naming
    decision for `Double` solver/decomposition conveniences.
Signature: GPT-5 Codex

## 2026-04-15 12:14 CEST - Native BigFloat Tensor Storage
- Objective attempted:
  - Move beyond Double-only Tensor ingestion by adding the first
    BigFloat-preserving concrete Tensor storage dtype.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `TENSOR_DTYPE_BIG_FLOAT` metadata, dtype printing/symbol lookup,
    owned BigFloat handle storage, element cleanup, deep clone, and concrete
    storage copy support.
  - Extended `Tensor` constructors to accept `BigFloat` dtype descriptors:
    `(Tensor BigFloat shape data-or-scalar)`, `(Tensor data BigFloat)`, and
    `(Tensor BigFloat data)`.
  - Added BigFloat Tensor support for `dtype`, `ref`, flat `(Array tensor)` /
    `(List tensor)` conversion, scalar `realize` fill, and concrete tensor copy
    realization.
  - Kept `map` and `contract` `Double`-only; BigFloat tensors reject those
    paths with `tensor/dtype-mismatch` until dedicated kernels land.
  - Added focused advanced collections/module regressions for dtype/ref,
    inferred prefix/suffix construction, flat collection conversion, scalar
    fill, concrete copy, and map rejection.
  - Updated the lifetime partial-constructor cleanup assertion for the more
    specific Double narrowing error text.
  - Updated `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/reference/04-type-system.md`,
    `docs/reference/11-appendix-primitives.md`,
    `docs/type-system-syntax.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `memory/CHANGELOG.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct smokes for BigFloat dtype/ref/inferred construction/Array/List
    conversion/scalar fill/concrete copy/map rejection
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - attempted `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - `(format "%s" (dtype (Tensor BigFloat [2] [(BigFloat "1.25") 2])))`
    returns `"BigFloat"`.
  - `(String (ref (Tensor [(BigFloat "1e309")] BigFloat) [0]))` returns
    `"1e+309"`, preserving values outside Double range.
  - Flat `(Array tensor)` and `(List tensor)` conversions return BigFloat
    values for BigFloat tensors.
  - Concrete `realize` scalar fill and tensor copy work for BigFloat tensors.
  - `(map + (Tensor BigFloat [1] [(BigFloat "1")]) 1)` fails closed with
    `map: tensor dtype mismatch`.
  - Focused advanced collections/module group passed on host and bounded
    container: `257 passed, 0 failed`.
  - Bounded `memory-lifetime-smoke` passed: `225 passed, 0 failed`.
  - Stage 3 source parity and whitespace checks passed.
- Invalidated assumptions or failed approaches worth preserving:
  - The earlier phrase "BigFloat/BigInteger-preserving Tensor storage remains
    unshipped" is now stale for BigFloat concrete storage. It remains true for
    BigInteger storage and for BigFloat tensor arithmetic kernels.
- Unresolved issues:
  - BigFloat Tensor `map` and `contract` kernels are not implemented.
  - BigInteger and BigComplex Tensor storage dtypes remain unshipped.
  - ASAN validation could not run: the C3 compiler rejected the sanitizer build
    immediately with `Address sanitizer is only supported on Linux, FreeBSD,
    NetBSD, Darwin and Windows.`
- Current best recommendation:
  - Continue with BigFloat Tensor `map` kernels next if the goal is scientific
    scalar precision through Tensor operations. Keep BLAS-backed Double kernels
    separate from BigFloat handle storage.
Signature: GPT-5 Codex

## 2026-04-15 11:57 CEST - Tensor Real Numeric Narrowing
- Objective attempted:
  - Close the Tensor constructor contract gap after inferred-shape
    construction: native `Double` tensors should accept real numeric inputs
    that can safely narrow to finite `Double`, not only fixed-width
    `Integer`/`Double` leaves.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Routed Tensor value-to-`Double` conversion through the shared
    `try_numeric_to_double` helper.
  - Native `Double` Tensor constructors now accept `Integer`, `Double`,
    `BigInteger`, and `BigFloat` inputs when representable as finite `Double`.
  - BigComplex values and out-of-`Double`-range BigFloat/BigInteger values fail
    closed.
  - Added focused advanced collections/module regressions for inferred
    BigInteger/BigFloat leaves, explicit BigFloat scalar fill, explicit
    BigInteger flat data, out-of-range BigFloat rejection, and BigComplex
    rejection.
  - Updated `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `memory/CHANGELOG.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct smokes for BigFloat Tensor data, BigInteger Tensor data,
    out-of-range BigFloat rejection, and BigComplex rejection
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results:
  - `(ref (Tensor [(BigFloat "1.25")]) [0])` returns `1.25`.
  - `(ref (Tensor [(BigInteger "9223372036854775808")]) [0])` returns a
    finite `Double` representation.
  - `(Tensor [(BigFloat "1e309")])` fails closed.
  - `(Tensor [(BigComplex 1 2)])` fails closed.
  - Focused advanced collections/module group passed on host:
    `248 passed, 0 failed`.
  - Bounded container rerun passed:
    `248 passed, 0 failed`.
  - Stage 3 source parity and whitespace checks passed.
- Current best recommendation:
  - Treat native `Double` Tensor numeric ingress as shipped for all real
    numeric values representable as finite `Double`. BigFloat/BigInteger
    preserving Tensor storage remains a separate dtype/storage project.
- Unresolved issues:
  - Tensor dtypes beyond native `Double` are still unshipped.
  - LAPACK/LAPACKE public qualifier is still undecided.
- Next actions:
  - Commit and push this narrowing slice, then continue with either explicit
    Tensor dtype work or the LAPACK/decomposition surface decision.
Signature: GPT-5 Codex

## 2026-04-15 11:52 CEST - Tensor Inferred Constructor Overloads
- Objective attempted:
  - Continue the owner-preferred constructor-dispatch Tensor surface by adding
    inferred-shape construction through `Tensor` itself.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added recursive Tensor shape inference for numeric scalars and rectangular
    nested arrays/proper lists.
  - Added flattening into native row-major `Double` tensor storage.
  - Preserved `(Tensor Double shape data-or-scalar)` and added
    `(Tensor data)`, `(Tensor data Double)`, and `(Tensor Double data)`.
  - Changed `Tensor` primitive registration from fixed arity 3 to variable
    arity, and allowed both `Double` and `'Double` as dtype markers.
  - Added focused advanced collections/module regressions for vector, matrix,
    scalar rank-0, dtype prefix/suffix, quoted dtype, empty vector, ragged
    rejection, and non-numeric rejection.
  - Updated `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/00-overview.md`, `docs/reference/03-collections.md`,
    `docs/reference/04-type-system.md`,
    `docs/reference/11-appendix-primitives.md`,
    `docs/type-system-syntax.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `memory/CHANGELOG.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct smokes for inferred vector, inferred matrix shape, dtype-prefix
    construction, and ragged rejection
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results:
  - `(ref (Tensor [1 2 3]) [2])` returns `3.0`.
  - `(format "%s" (shape (Tensor [[1 2] [3 4]])))` returns `"[2 2]"`.
  - `(ref (Tensor Double [1 2]) [1])` returns `2.0`.
  - `(Tensor [[1] [2 3]])` fails with `Tensor: inferred data must be rectangular`.
  - Focused advanced collections/module group passed on host:
    `242 passed, 0 failed`.
  - Bounded container rerun passed:
    `242 passed, 0 failed`.
  - Stage 3 source parity and whitespace checks passed.
- Current best recommendation:
  - Treat constructor-dispatch Tensor ingestion as shipped for native `Double`
    rectangular data. Continue next with backend naming policy, LAPACK solver
    surfaces, or additional dtype work, not a separate Tensor-only conversion
    helper.
- Unresolved issues:
  - Inferred constructors currently accept the first native `Double` storage
    path only; BigFloat/BigInteger-preserving Tensor dtypes remain unshipped.
  - Tensor-to-collection conversion remains flat row-major; nested
    reconstruction is still intentionally not part of the shipped contract.
  - LAPACK/LAPACKE public qualifier is still undecided.
- Next actions:
  - Commit and push this inferred-constructor slice, then choose the next
    Tensor/scientific lane explicitly.
Signature: GPT-5 Codex

## 2026-04-15 11:41 CEST - Tensor Collection Conversions
- Objective attempted:
  - Implement the owner-preferred constructor-dispatch Tensor conversion
    surface through `(Array tensor)` and `(List tensor)` rather than adding a
    Tensor-only materialization helper.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `tensor_to_flat_array_value` and `tensor_to_flat_list_value` helpers.
  - Extended `Array` and `List` constructors to accept a single Tensor input.
  - Tensor conversions force lazy Tensor expressions when needed and return
    flat row-major element values.
  - Shape/rank metadata remains explicit through `shape` and `rank`; collection
    conversion does not synthesize nested arrays/lists.
  - Added focused advanced collections/module regressions for concrete Tensor
    conversion, lazy `map` conversion, and lazy `contract` conversion.
  - Updated `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `memory/CHANGELOG.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct smokes for `(Array tensor)`, `(List tensor)`, lazy map conversion,
    and zero-size Tensor conversion
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results:
  - `(ref (Array (Tensor Double [2 2] [1 2 3 4])) 2)` returns `3.0`.
  - `(ref (List (Tensor Double [2 2] [1 2 3 4])) 2)` returns `3.0`.
  - `(ref (Array (map + (Tensor Double [2] [1 2]) 1)) 1)` returns `3.0`.
  - `(length (Array (Tensor Double [0] 0.0)))` returns `0`.
  - Focused advanced collections/module group passed on host:
    `232 passed, 0 failed`.
  - Bounded container rerun passed:
    `232 passed, 0 failed`.
  - Stage 3 source parity and whitespace checks passed.
- Current best recommendation:
  - Tensor now participates in the same constructor/conversion dispatch model
    as other collection-like values. Continue with LAPACK naming/surface
    decision or scalar precision policy; do not add Tensor-only conversion
    helper names for this already-shipped behavior.
- Unresolved issues:
  - Tensor-to-collection conversion is flat row-major only; nested shape
    reconstruction remains intentionally unshipped.
  - LAPACK/LAPACKE public qualifier is still undecided.
- Next actions:
  - Commit and push this conversion slice, then choose between LAPACK naming
    and scalar precision policy work.
Signature: GPT-5 Codex

## 2026-04-15 11:28 CEST - Tensor BLAS `dgemv` Contracts
- Objective attempted:
  - Continue `TENSOR-090` by expanding private BLAS acceleration from
    rank-2/rank-2 `dgemm` contracts to rank-2/rank-1 and rank-1/rank-2
    `dgemv` contracts without adding a public backend-specific surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/tensor_blas_helpers.c` to resolve `cblas_dgemv`, expose a
    `dgemv` availability probe and call counter, and call the BLAS routine with
    row-major transpose flags.
  - Added C3 externs for the new private Tensor BLAS `dgemv` backend functions.
  - Added Tensor evaluator eligibility for contiguous row-major matrix-vector,
    transposed matrix-vector, vector-matrix, and vector-transposed-matrix
    single-axis `Double` contractions.
  - Added path-sensitive advanced collections/module regressions that require
    `dgemv` call-count movement when the symbol is available and otherwise
    validate fallback semantics.
  - Updated `.agents/PLAN.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `memory/CHANGELOG.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct smokes for matrix-vector, transposed matrix-vector, vector-matrix,
    and vector-transposed-matrix contract results
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- Key results:
  - `(contract (Tensor Double [2 3] ...) (Tensor Double [3] ...) [1 0])`
    returns the expected matrix-vector result; direct smoke checked `320.0`.
  - Transposed matrix-vector direct smoke checked `150.0`.
  - Vector-matrix direct smoke checked `280.0`.
  - Vector-transposed-matrix direct smoke checked `320.0`.
  - Focused advanced collections/module group passed on host:
    `226 passed, 0 failed`.
  - Bounded container rerun passed:
    `226 passed, 0 failed`.
- Current best recommendation:
  - `TENSOR-090` now has private BLAS `dgemm` and `dgemv` coverage behind
    canonical `contract`/`realize`. The next backend step should be a
    LAPACK/LAPACKE solver/decomposition naming decision before exposing solver
    conveniences; do not add bare `solve`.
- Unresolved issues:
  - LAPACK/LAPACKE public qualifier is still undecided.
  - CUDA/cuBLAS remains a future explicit-device design slice.
- Next actions:
  - Decide the solver/decomposition namespace and first LAPACK primitive, or
    continue scalar precision policy work.
Signature: GPT-5 Codex

## 2026-04-15 11:21 CEST - BigComplex Component Access
- Objective attempted:
  - Continue the scientific scalar lane by making BigComplex analytically
    usable without string parsing or ad hoc destructuring.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added C++ helper exports for BigComplex `real-part`, `imag-part`, and
    `conjugate`.
  - Added C3 externs and value helpers that return BigFloat components for
    BigComplex inputs.
  - Added public numeric primitives `real-part`, `imag-part`, and `conjugate`,
    plus primitive-table registration and AOT lookup.
  - Real scalar inputs keep their existing value as the real part, use Integer
    `0` as the imaginary part, and are preserved by `conjugate`.
  - Added focused advanced numeric regressions and updated language/reference
    docs, `.agents/PLAN.md`, and `memory/CHANGELOG.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct smokes for BigComplex component access and real-scalar conjugation
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results:
  - `(String (real-part (BigComplex 1 2)))` returns `"1"`.
  - `(String (imag-part (BigComplex 1 2)))` returns `"2"`.
  - `(String (conjugate (BigComplex 1 2)))` returns `"1-2i"`.
  - `(String (conjugate (BigFloat "1.25")))` returns `"1.25"`.
  - Focused advanced numeric float-math group passed on host:
    `172 passed, 0 failed`.
  - Bounded container rerun passed:
    `172 passed, 0 failed`.
  - Stage 3 source parity and whitespace checks passed.
- Current best recommendation:
  - BigComplex now has arithmetic, elementary math, hyperbolic math, magnitude,
    and component access. The next scalar precision decision should be either a
    precision-control policy or a deliberate BigComplex special-function /
    distribution policy.
- Unresolved issues:
  - No precision-control API exists for BigFloat/BigComplex yet.
  - Broader complex special functions and distributions remain unimplemented.
- Next actions:
  - Pick and implement the next scalar precision policy slice, or switch back
    to Tensor backend acceleration once the scalar surface is sufficient.
Signature: GPT-5 Codex

## 2026-04-15 11:10 CEST - Hyperbolic Scalar Math
- Objective attempted:
  - Continue the scientific scalar math lane by adding standard hyperbolic
    functions across existing scalar numeric backends.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `sinh`, `cosh`, and `tanh` primitives.
  - Routed Double inputs through the C math library.
  - Added BigFloat helper op codes so hyperbolic functions preserve BigFloat
    results.
  - Added BigComplex helper op codes so hyperbolic functions preserve
    BigComplex results.
  - Added primitive registration, AOT lookup, focused regressions, and updated
    `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/11-appendix-primitives.md`, and `memory/CHANGELOG.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct smokes for Double, BigFloat, and BigComplex hyperbolic results
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results and observed behavior:
  - `(sinh 0.0)` returns `0.0`; `(cosh 0.0)` returns `1.0`.
  - `(String (sinh (BigFloat "0")))` returns `"0"`.
  - `(= (type-of (sinh (BigFloat "0"))) 'BigFloat)` returns `true`.
  - `(String (sinh (BigComplex 0 0)))` returns `"0+0i"`.
  - `(String (cosh (BigComplex 0 0)))` returns `"1+0i"`.
  - Focused advanced numeric float-math group passed on host and in the
    bounded container at `163 passed, 0 failed`.
  - Stage 3 source parity and whitespace checks passed.
- Invalidated assumptions or failed approaches worth preserving:
  - No failed implementation path required negative memory for this slice.
- Current best recommendation/checkpoint:
  - Hyperbolic math is implemented for Double, BigFloat, and BigComplex. Next
    scalar precision work should be BigFloat precision-control policy or a
    deliberate BigComplex special-function/distribution policy.
- Unresolved issues / blockers:
  - Full all-slice and ASAN validation were not run for this slice.
- Signature: Codex (GPT-5)

## 2026-04-17 09:59 CEST - TENSOR-100F1 Vulkan SVD-Backed Singular Values

- Objective attempted:
  - Continue from the latest Vulkan math-library session report using multiple
    agents, verify the in-progress Vulkan SVD-backed slice, and align the
    runtime contract with docs/plans.
- Workspace:
  - `/home/christos/Omni`
- Code/configuration changes made:
  - Preserved and verified the existing dense row-major Vulkan `Float64`
    `matrix/singular-values` implementation and spectral/nuclear
    `matrix/norm` routing through `omni_tensor_backend_vulkan_singular_values_f64`.
  - Fixed `matrix/svd`, `matrix/eigenvalues`, `matrix/eigenvectors`, and
    `matrix/eigenpairs` so non-CPU Tensor placement is rejected before generic
    realization can silently copy Vulkan input through CPU paths.
  - Updated current Tensor/Vulkan docs, TODO, plan, and changelog artifacts so
    `matrix/singular-values` plus `matrix/norm` `'spectral` / `'nuclear` are
    recorded as supported on Vulkan, while direct SVD/eigen surfaces remain
    CPU-only.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 csrc/tensor_vulkan_singular_values_f64.comp -o /tmp/omni_tensor_vulkan_singular_values_f64.spv`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_singular_values_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - Direct Vulkan smokes returned `3.0`, `5.0`, `vulkan`, and empty length `0`.
  - Initial focused `advanced-collections-module` run exposed stale/real
    unsupported-surface failures for direct `matrix/svd` and eigen surfaces.
  - After the fix,
    `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
    passed with `pass=866 fail=0`.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not treat direct `matrix/svd` or eigen surfaces as implicitly safe on
    Vulkan just because `matrix/singular-values` has a Vulkan helper. Those
    factor/eigensolver surfaces remain CPU-only until explicit Vulkan plans
    land.
  - Do not allow CPU-only matrix primitives to call generic realization before
    checking source device placement; that can hide unsupported Vulkan input
    behind CPU copyback.
- Current best recommendation/checkpoint:
  - Continue `TENSOR-100F` from the verified Vulkan singular-value helper.
    Next useful implementation should either refactor shared Vulkan helper
    plumbing without changing exported contracts, or add another explicit
    backend-neutral kernel with dedicated Vulkan semantics.
- Unresolved issues:
  - Bounded-container and full heavy gates were not run in this pass.
  - Direct `matrix/svd`, `matrix/eigenvalues`, `matrix/eigenvectors`, and
    `matrix/eigenpairs` still need dedicated Vulkan design/implementation
    before they can support Vulkan operands.
- Signature: Codex GPT-5

## 2026-04-17 10:37 CEST - TENSOR-100F1 Vulkan Audit Hardening

- Objective attempted:
  - Continue auditing and fixing the Tensor/Vulkan SVD-backed slice using
    multiple agents, then close the audit findings with focused validation.
- Workspace:
  - `/home/christos/Omni`
- Code/configuration changes made:
  - Continued GPT-5.4 high audit coverage for source/device semantics and
    tests/docs. The requested GPT-5.3-Codex-Spark implementation workers were
    quota-blocked by the environment, so implementation proceeded locally with
    fallback fast workers limited to no-conflict report/roadmap review.
  - Hardened `realize` semantics: one-argument `realize` preserves concrete
    Vulkan placement, while explicit destination `realize` rejects concrete or
    lazy Vulkan sources with `tensor/backend-unsupported`.
  - Hardened Vulkan singular-values execution: the helper now dispatches one
    work item for the single-invocation shader while preserving the `k + 2`
    payload, and shader non-convergence maps through
    `OMNI_TENSOR_VULKAN_NO_CONVERGENCE` to public `tensor/no-convergence`.
    The helper also rejects logical matrices whose element count exceeds the
    shader's 32-bit index space before dispatch.
  - Broadened tests for Vulkan `matrix/singular-values`, spectral/nuclear
    `matrix/norm`, CPU-only direct `matrix/svd`/eigen fail-closed behavior on
    lazy Vulkan inputs, explicit-destination `realize`, and CPU-only numeric
    Tensor helpers on Vulkan operands.
  - Added durable regressions for the `k == 64` singular-values boundary,
    rectangular `2x65`, empty `0x65`, status-payload non-convergence mapping,
    and the oversized-index validation guard.
  - Updated TODO/docs/plan/changelog wording for the `k <= 64` cap, direct
    SVD/eigen CPU-only behavior, stale `sqrt` roadmap wording, zero-axis
    `contract` support, destination `realize`, and latest validation details.
- Commands run and key results:
  - `glslangValidator -V --target-env vulkan1.0 -o /tmp/omni_tensor_vulkan_singular_values_f64.spv csrc/tensor_vulkan_singular_values_f64.comp`: passed.
  - `spirv-val --target-env vulkan1.0 /tmp/omni_tensor_vulkan_singular_values_f64.spv`: passed.
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=904 fail=0`.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=411 fail=0`.
  - Durable advanced regressions now cover `k == 64`, `k == 65` fail-closed,
    wide `2x65`, empty `0x65`, status-payload mapping, and oversized logical
    index validation before Vulkan dispatch.
