  - Routed `floor`, `ceiling`, `round`, and `truncate` through Tensor handling.
  - Added a shared Tensor rounding helper in `src/lisp/prim_tensor.c3`.
  - Real Tensor inputs return same-shape native `BigInteger` Tensor results.
  - `Double` Tensor inputs round through the C math operation and fail closed
    when the rounded result cannot narrow to Omni `Integer`.
  - `BigInteger` Tensor inputs clone exact integer values.
  - `BigFloat` Tensor inputs use the exact scalar BigFloat rounding path and
    preserve large integer results in BigInteger Tensor storage.
  - `BigComplex` Tensor inputs fail closed.
  - Lazy Tensor operands are realized before elementwise rounding.
  - Added advanced collections/module regressions for all four rounding
    primitives, result dtype, BigInteger clone behavior, large BigFloat
    promotion, lazy BigFloat realization, complex rejection, and Double
    out-of-range rejection.
  - Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Direct smokes confirmed Double Tensor rounding returns BigInteger dtype,
    BigFloat Tensor rounding preserves huge exact integer results, and
    BigComplex Tensor rounding fails closed.
  - Initial negative test used `1e20`, which the reader treated as non-numeric
    for Tensor construction; changed it to `1.0e20` so the Tensor constructs
    and the rounding range check is actually exercised.
  - Host focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=379 fail=0`.
  - Bounded container focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=379 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - `git diff --check` passed.
  - ASAN build attempt failed before compile with the local C3 compiler
    sanitizer platform message.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not use integer-looking exponent literals such as `1e20` when testing
    Tensor Double construction; use `1.0e20` to force the floating literal path.
- Current best recommendation:
  - Treat Tensor rounding as closed for real native Tensor dtypes.
- Unresolved issues:
  - Public LAPACK solver/decomposition naming remains unresolved.
  - ASAN coverage remains unavailable through the local C3 compiler invocation
    until proven otherwise.
- Next actions:
  - Commit and push this Tensor rounding slice.

Signature: GPT-5 Codex

## 2026-04-15 14:26 CEST - Tensor Atan2 Semantics
- Objective attempted:
  - Continue direct Tensor support for scalar scientific numeric primitives by
    adding real-plane binary `atan2`.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Routed `atan2` through Tensor handling when either argument is a Tensor.
  - Added Tensor `atan2` runtime handling for tensor-scalar, scalar-tensor, and
    broadcast tensor-tensor inputs.
  - Matched scalar `atan2` policy: complex operands fail closed.
  - `BigFloat` Tensor inputs preserve precision dtype; other real/exact inputs
    return `Double` tensors through the hardened finite-conversion path.
  - Lazy Tensor operands are realized before elementwise `atan2` evaluation.
  - Generalized the Tensor binary input dtype helper used by `pow`.
  - Added advanced collections/module regressions for Double tensor/scalar,
    scalar/tensor, BigInteger-to-Double, broadcast Tensor/Tensor, BigFloat,
    lazy BigFloat, BigComplex rejection, and huge BigInteger rejection.
  - Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Initial focused run failed three Double tests because `test_eq_double`
    compares raw binary equality and decimal source literals did not exactly
    match `atan2(1, 1)`. The tests now assert the printed Double value.
  - Host focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=369 fail=0`.
  - Bounded container focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=369 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - `git diff --check` passed.
  - ASAN build attempt failed before compile with the local C3 compiler
    sanitizer platform message.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not use `test_eq_double` with a decimal approximation for irrational
    math results such as `atan2(1, 1)`; use exact stable operations or a string
    or tolerance-based assertion.
- Current best recommendation:
  - Treat Tensor `atan2` as closed for real native Tensor dtypes and current
    broadcasting semantics.
- Unresolved issues:
  - Public LAPACK solver/decomposition naming remains unresolved.
  - ASAN coverage remains unavailable through the local C3 compiler invocation
    until proven otherwise.
- Next actions:
  - Commit and push this Tensor `atan2` slice.

Signature: GPT-5 Codex

## 2026-04-15 14:17 CEST - Tensor Pow Semantics
- Objective attempted:
  - Continue direct Tensor support for scalar scientific numeric primitives by
    adding the first binary scientific primitive beyond `map`.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Routed `pow` through Tensor handling when either argument is a Tensor.
  - Added Tensor `pow` runtime handling for tensor-scalar, scalar-tensor, and
    broadcast tensor-tensor inputs.
  - Result dtype policy is `BigComplex` if either input is complex,
    `BigFloat` if either input is BigFloat, otherwise `Double`.
  - `Double` and `BigInteger` Tensor inputs return `Double` tensors through
    the hardened finite-conversion path.
  - `BigFloat` and `BigComplex` Tensor inputs preserve precision dtype.
  - Lazy Tensor operands are realized before elementwise power evaluation.
  - Added advanced collections/module regressions for Double tensor/scalar,
    scalar/tensor, BigInteger-to-Double, broadcast Tensor/Tensor, BigFloat,
    BigComplex, lazy BigComplex, and huge BigInteger rejection.
  - Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Initial focused run showed BigComplex Tensor `pow` produces the same
    near-zero imaginary residual as scalar BigComplex `pow`; tests were changed
    to magnitude tolerance instead of exact string equality.
  - Host focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=361 fail=0`.
  - Bounded container focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=361 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - `git diff --check` passed.
  - ASAN build attempt failed before compile with the local C3 compiler
    sanitizer platform message.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not exact-string compare BigComplex Tensor `pow` results for analytic
    values that may carry tiny residual components; use magnitude tolerance.
- Current best recommendation:
  - Treat Tensor `pow` as closed for native numeric dtypes and current
    broadcasting semantics.
- Unresolved issues:
  - Public LAPACK solver/decomposition naming remains unresolved.
  - ASAN coverage remains unavailable through the local C3 compiler invocation
    until proven otherwise.
- Next actions:
  - Commit and push this Tensor `pow` slice.

Signature: GPT-5 Codex

## 2026-04-15 14:06 CEST - Tensor Unary Scientific Math
- Objective attempted:
  - Extend direct Tensor support from single scalar primitives to a broader
    unary scientific-math family.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added a shared Tensor unary-math helper in `src/lisp/prim_tensor.c3`.
  - Routed `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`,
    `tanh`, `exp`, `log`, and `log10` through the helper when their argument
    is a Tensor.
  - Folded Tensor `sqrt` onto the shared helper while preserving its result
    dtype policy.
  - `Double` Tensor inputs return same-shape `Double` tensors.
  - `BigInteger` Tensor inputs return same-shape `Double` tensors through the
    hardened finite-conversion path.
  - `BigFloat` Tensor inputs preserve `BigFloat` dtype and shape.
  - `BigComplex` Tensor inputs preserve `BigComplex` dtype and shape, including
    lazy source realization.
  - Added advanced collections/module regressions for every newly routed
    primitive family and a lazy BigComplex `log10` source.
  - Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Initial focused run caught a real omission: inverse trig routes were wired
    at the primitive level but missing from the Double helper switch. Added
    `asin`/`acos`/`atan` C bindings and switch cases.
  - Host focused advanced collections/module group passed after the fix:
    `OMNI_TEST_SUMMARY suite=unified pass=353 fail=0`.
  - Bounded container focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=353 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - `git diff --check` passed.
  - ASAN build attempt failed before compile with the local C3 compiler
    sanitizer platform message.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not assume routing a primitive through the shared Tensor helper is
    enough; every routed operation must have a Double switch case when real
    Tensor dtypes can produce `Double`.
- Current best recommendation:
  - Treat the Tensor unary scientific-math implementation as closed for the
    routed primitive set.
- Unresolved issues:
  - Public LAPACK solver/decomposition naming remains unresolved.
  - ASAN coverage remains unavailable through the local C3 compiler invocation
    until proven otherwise.
- Next actions:
  - Commit and push this Tensor unary scientific-math slice.

Signature: GPT-5 Codex

## 2026-04-15 13:56 CEST - Tensor Sqrt Semantics
- Objective attempted:
  - Continue direct Tensor support for scalar scientific numeric primitives.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended the existing `sqrt` primitive to accept Tensor inputs.
  - Added Tensor `sqrt` runtime handling for `Double`, `BigInteger`,
    `BigFloat`, and `BigComplex` tensors in `src/lisp/prim_tensor.c3`.
  - `Double` Tensor inputs return same-shape `Double` tensors.
  - `BigInteger` Tensor inputs return same-shape `Double` tensors, matching
    scalar `sqrt` conversion behavior for exact integers.
  - `BigFloat` Tensor inputs preserve `BigFloat` dtype and shape.
  - `BigComplex` Tensor inputs preserve `BigComplex` dtype and shape, including
    lazy source realization.
  - Hardened `csrc/big_integer_helpers.cpp` so `omni_big_integer_to_double`
    rejects enormous `cpp_int` values by bit length before attempting
    `convert_to<double>()`.
  - Added advanced collections/module regressions for Double, BigInteger,
    BigFloat, BigComplex, lazy BigComplex, and out-of-Double-range BigInteger
    Tensor `sqrt`.
  - Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct `--eval` smokes for huge BigInteger `Double` conversion rejection,
    huge BigInteger Tensor `sqrt` rejection, and BigComplex Tensor `sqrt`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Direct smokes now fail closed instead of aborting for huge BigInteger
    conversion and Tensor `sqrt`.
  - Direct BigComplex Tensor `sqrt` returned `"0+1i"`.
  - Host focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=341 fail=0`.
  - Bounded container focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=341 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - `git diff --check` passed.
  - ASAN build attempt failed before compile with the local C3 compiler
    sanitizer platform message.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not rely on `cpp_int::convert_to<double>()` alone as the range guard for
    enormous exact integers; pre-screen by bit length before conversion.
- Current best recommendation:
  - Treat Tensor `sqrt` as closed for native numeric dtypes.
- Unresolved issues:
  - Public LAPACK solver/decomposition naming remains unresolved.
  - ASAN coverage remains unavailable through the local C3 compiler invocation
    until proven otherwise.
- Next actions:
  - Commit and push this Tensor `sqrt` slice.

Signature: GPT-5 Codex

## 2026-04-15 13:47 CEST - Tensor Abs Semantics
- Objective attempted:
  - Continue the precision Tensor lane with a concrete scalar-to-Tensor
    numeric primitive extension.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended the existing `abs` primitive to accept Tensor inputs.
  - Added Tensor `abs` runtime handling for `Double`, `BigInteger`,
    `BigFloat`, and `BigComplex` tensors in `src/lisp/prim_tensor.c3`.
  - Real Tensor dtypes preserve dtype and shape under elementwise magnitude.
  - `BigComplex` Tensor inputs return same-shape `BigFloat` magnitude tensors,
    matching scalar `BigComplex` `abs`.
  - Lazy Tensor sources are realized before magnitude extraction and cleaned
    through the existing materialized-value boundary.
  - Added advanced collections/module regressions for Double, BigInteger,
    BigFloat, BigComplex, and lazy BigComplex Tensor `abs`.
  - Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct `--eval` smokes for BigComplex Tensor `abs`, BigInteger Tensor
    `abs`, and lazy BigComplex Tensor `abs`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Direct smokes returned `"5"`, `"9223372036854775808"`, and `"5"`.
  - Host focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=335 fail=0`.
  - Bounded container focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=335 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - `git diff --check` passed.
  - ASAN build attempt failed before compile with the local C3 compiler
    sanitizer platform message.
- Current best recommendation:
  - Treat Tensor `abs` as closed for native numeric dtypes. The next scientific
    slice should be another concrete kernel or an explicit naming decision for
    solver/decomposition conveniences.
- Unresolved issues:
  - Public LAPACK solver/decomposition naming remains unresolved.
  - ASAN coverage remains unavailable through the local C3 compiler invocation.
- Next actions:
  - Commit and push this Tensor `abs` slice.

Signature: GPT-5 Codex

## 2026-04-15 13:40 CEST - Real Tensor Component Semantics
- Objective attempted:
  - Close the real Tensor component-policy gap left by the BigComplex Tensor
    component slice.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended Tensor `real-part`, `imag-part`, and `conjugate` behavior from
    BigComplex-only Tensor handling to all native real Tensor dtypes.
  - `real-part` and `conjugate` now copy `Double`, `BigInteger`, and
    `BigFloat` tensors while preserving dtype and shape.
  - `imag-part` now returns same-shape zero tensors in the same real dtype.
  - Added advanced collections/module regressions for Double Tensor
    `real-part`, Double Tensor `imag-part`, BigInteger Tensor `conjugate`,
    and BigFloat Tensor `imag-part`.
  - Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - `c3c build`
  - direct `--eval` smokes for `imag-part` on Double Tensor and `conjugate`
    on BigInteger Tensor
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Direct smokes returned `0.0` and `"9223372036854775808"`.
  - Host focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=330 fail=0`.
  - Bounded container focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=330 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - `git diff --check` passed.
  - ASAN build attempt failed before compile with the local C3 compiler
    sanitizer platform message.
- Current best recommendation:
  - Treat Tensor component semantics as closed for current numeric dtypes. The
    next scientific slice should move to a concrete kernel or accepted naming
    decision rather than more component-policy cleanup.
- Unresolved issues:
  - Public LAPACK solver/decomposition naming remains unresolved.
  - ASAN coverage remains unavailable through the local C3 compiler invocation.
- Next actions:
  - Commit and push this real Tensor component semantics slice.

Signature: GPT-5 Codex

## 2026-04-15 13:29 CEST - BigComplex Tensor Component Kernels
- Objective attempted:
  - Continue the complex Tensor lane by making `real-part`, `imag-part`, and
    `conjugate` operate on native BigComplex Tensor inputs.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added BigComplex Tensor component helpers in `src/lisp/prim_tensor.c3`.
  - Routed `real-part`, `imag-part`, and `conjugate` through those helpers
    before scalar numeric handling when the argument is a Tensor.
  - `real-part` and `imag-part` realize lazy BigComplex Tensor sources and
    return native BigFloat Tensor results.
  - `conjugate` realizes lazy BigComplex Tensor sources and returns native
    BigComplex Tensor results.
  - Added focused advanced collections/module regressions for concrete
    component extraction, lazy-source component extraction, conjugation, and
    non-BigComplex Tensor fail-closed behavior.
  - Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct `--eval` smokes for BigComplex Tensor `real-part`, `imag-part`, and
    lazy-source `conjugate`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Direct smokes returned `"3"`, `"2"`, and `"4-6i"`.
  - Host focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=327 fail=0`.
  - Bounded container focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=327 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - `git diff --check` passed.
  - ASAN build attempt failed before compile with the local C3 compiler
    sanitizer platform message.
- Current best recommendation:
  - Treat BigComplex Tensor component access as shipped. The next scientific
    slice should either choose a public LAPACK/decomposition namespace or add a
    specific complex/scientific kernel with a locked result dtype.
- Unresolved issues:
  - Non-BigComplex Tensor inputs to component operations intentionally fail
    closed; real Tensor component dtype policy remains undecided.
  - Public LAPACK solver/decomposition naming remains unresolved.
  - ASAN coverage remains unavailable through the local C3 compiler invocation.
- Next actions:
  - Commit and push this BigComplex Tensor component slice.

Signature: GPT-5 Codex

## 2026-04-15 13:21 CEST - BigComplex Tensor Kernels
- Objective attempted:
  - Continue the scientific precision Tensor lane by adding native BigComplex
    Tensor storage and pure C3 kernels behind the existing `Tensor`, `map`,
    `contract`, and `realize` surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `TENSOR_DTYPE_BIG_COMPLEX`, dtype metadata, owned element cleanup,
    deep clone, concrete storage copy, and lazy scalar operand cloning.
  - Extended Tensor constructors to accept `BigComplex` dtype descriptors in
    explicit shape/data and inferred prefix/suffix forms. Real numeric leaves
    promote to zero-imaginary BigComplex elements.
  - Extended `ref`, flat `(Array tensor)` / `(List tensor)` conversion,
    concrete/scalar `realize`, tensor-dispatched `map`, and pure C3
    `contract` for BigComplex tensors.
  - Added advanced collections/module regressions for BigComplex Tensor dtype,
    ref, constructor forms, collection conversion, scalar fill/copy,
    lazy map boundaries, contraction boundaries, and mixed-dtype rejection.
  - Updated `memory/CHANGELOG.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/03-collections.md`, `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md`.
- Commands run:
  - `c3c build main --output-dir build --build-dir build/obj2`
  - `c3c build`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - direct `--eval` smokes for BigComplex Tensor `ref`, lazy `map`, and
    `contract`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Direct smokes returned `"1+2i"`, `"3+5i"`, and `"11+3i"`.
  - Host focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=321 fail=0`.
  - Bounded container focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=321 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - `git diff --check` passed.
  - ASAN build attempt failed before compile with the local C3 compiler
    sanitizer platform message.
- Current best recommendation:
  - Treat native precision Tensor coverage as shipped for BigInteger,
    BigFloat, and BigComplex. The next precision work should be a concrete
    policy or kernel slice, not another storage dtype placeholder.
- Unresolved issues:
  - BLAS/LAPACK acceleration remains `Double`-only.
  - Public LAPACK solver/decomposition naming remains unresolved.
  - ASAN coverage remains unavailable through the local C3 compiler invocation.
- Next actions:
  - Commit and push this BigComplex Tensor slice.

Signature: GPT-5 Codex

## 2026-04-15 13:08 CEST - Tensor BLAS DGER Fast Path
- Objective attempted:
  - Continue optional native BLAS backend coverage behind existing
    `contract`/`realize` by accelerating the already-supported zero-axis
    rank-1/rank-1 outer-product contraction.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/tensor_blas_helpers.c` to resolve `cblas_dger`, expose
    availability/call-count probes, and execute rank-1 outer products through
    the optional BLAS backend when available.
  - Added C3 extern declarations in `src/lisp/tensor_blas_backend.c3`.
  - Added `tensor_contract_try_blas_dger(...)` in `src/lisp/prim_tensor.c3`
    before the existing `ddot`/`dgemm`/`dgemv` fast paths. It only accepts
    contiguous rank-1/rank-1 `Double` zero-axis contractions into rank-2
    row-major output; unsupported cases fall back to the pure C3 kernel.
  - Extended the existing outer-product advanced collections/module regression
    to verify the private BLAS call counter when `cblas_dger` is available.
  - Updated `memory/CHANGELOG.md`, `TODO.md`,
    `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build main --output-dir build --build-dir build/obj2`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Host focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=298 fail=0`.
  - Bounded container focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=298 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - `git diff --check` passed.
  - ASAN build attempt failed before compile with the local C3 compiler
    sanitizer platform message.
- Current best recommendation:
  - Keep adding private BLAS kernels only where they are invisible behind the
    existing Tensor surface and have pure-fallback regressions. Do not unblock
    public LAPACK solver/decomposition work until the qualifier is accepted.
- Unresolved issues:
  - No `TENSOR-090E` runtime blocker remains from this slice.
  - ASAN coverage remains unavailable through the local C3 compiler invocation.
- Next actions:
  - Commit and push this BLAS dger slice.

Signature: GPT-5 Codex

## 2026-04-15 13:01 CEST - Tensor BLAS DDOT Fast Path
- Objective attempted:
  - Continue optional native BLAS backend coverage behind existing
    `contract`/`realize` without adding unresolved LAPACK solver names or new
    public Tensor surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/tensor_blas_helpers.c` to resolve `cblas_ddot`, expose
    availability/call-count probes, and execute rank-1 dot products through
    the optional BLAS backend when available.
  - Added C3 extern declarations in `src/lisp/tensor_blas_backend.c3`.
  - Added `tensor_contract_try_blas_ddot(...)` in `src/lisp/prim_tensor.c3`
    before the existing `dgemm`/`dgemv` fast paths. It only accepts
    contiguous rank-1/rank-1 `Double` single-axis contractions into rank-0
    output; unsupported cases fall back to the pure C3 kernel.
  - Added an advanced collections/module regression that verifies the vector
    dot result and, when `cblas_ddot` is available, the private BLAS call
    counter.
  - Updated `memory/CHANGELOG.md`, `TODO.md`,
    `docs/areas/tensor-scientific.md`,
    `docs/plans/tensor-scientific-computing-plan-2026-04-11.md`, and
    `.agents/PLAN.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build main --output-dir build --build-dir build/obj2`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
  - `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
- Key results:
  - Host focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=297 fail=0`.
  - Bounded container focused advanced collections/module group passed:
    `OMNI_TEST_SUMMARY suite=unified pass=297 fail=0`.
  - Bounded container `memory-lifetime-smoke` passed:
    `OMNI_TEST_SUMMARY suite=unified pass=225 fail=0`.
  - Stage 3 e2e source parity passed.
  - `git diff --check` passed.
  - ASAN build attempt failed before compile with the local C3 compiler
    sanitizer platform message.
- Current best recommendation:
  - Keep LAPACK solver/decomposition public names blocked until the owner
    accepts a qualifier. Additional private BLAS kernels can continue only
    where they stay invisible behind `contract`/`realize` and have pure
    fallback regressions.
- Unresolved issues:
  - No `TENSOR-090D` runtime blocker remains from this slice.
  - ASAN coverage remains unavailable through the local C3 compiler invocation.
- Next actions:
  - Commit and push this BLAS ddot slice.

Signature: GPT-5 Codex

## 2026-04-15 12:54 CEST - Native BigInteger Tensor Kernels
- Objective attempted:
  - Extend Omni Tensor precision support from `Double`/`BigFloat` to native
    exact integer Tensor storage and kernels.
- Workspace/target:
