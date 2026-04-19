  - Primitive docs parity, Stage 3 source parity, and targeted
    `git diff --check` passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not rely on shallow root `source.device` checks for lazy Tensor
    expressions. CPU-only helpers must inspect Tensor expression leaves before
    generic realization.
  - Do not treat the Vulkan singular-values shader status tail as a generic
    singular-matrix status. Its non-convergence status maps to
    `tensor/no-convergence`.
  - Do not rely only on per-axis `UINT32_MAX` checks in Vulkan helpers whose
    shaders index with 32-bit arithmetic; guard total logical element count
    before dispatch.
  - Do not treat destination `realize` as an implicit GPU-to-CPU copy boundary;
    explicit `to-device 'cpu` is the copy boundary for non-CPU sources.
- Current best recommendation/checkpoint:
  - The `TENSOR-100F1` Vulkan singular-values helper is verified for the
    current dense row-major `Float64`, `k <= 64` contract. Next useful work
    should be a separate Vulkan factor-output SVD/eigensolver design, or a
    shared Vulkan helper cleanup that preserves the public contracts.
- Unresolved issues:
  - Full heavy/container-only gates were not run in this pass.
  - Actual shader iteration-exhaustion is still not forced by a public numeric
    fixture; the status-payload mapping and C3 status-code mapping are now
    covered by deterministic probes.
  - Direct `matrix/svd`, `matrix/eigenvalues`, `matrix/eigenvectors`, and
    `matrix/eigenpairs` still need dedicated Vulkan design/implementation
    before they can support Vulkan operands.
- Signature: Codex GPT-5

## 2026-04-15 11:00 CEST - BigComplex Scalar Math
- Objective attempted:
  - Continue the BigComplex scientific scalar lane beyond arithmetic by adding
    complex-preserving elementary math.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/big_complex_helpers.cpp` with BigComplex unary math for
    `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `exp`, `log`, `log10`, and
    `sqrt`, plus binary `pow`.
  - Added C3 externs and value helper wrappers for BigComplex math.
  - Routed the corresponding math primitives through BigComplex before the
    BigFloat or Double paths.
  - Kept `atan2` as a real-plane helper and added an explicit complex-operand
    rejection.
  - Added focused advanced numeric regressions and updated
    `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/11-appendix-primitives.md`, and `memory/CHANGELOG.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct smokes for BigComplex `sqrt`, `exp`, `log`, `sin`, `cos`, `pow`,
    and `atan2` rejection
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results and observed behavior:
  - `(String (sqrt (BigComplex -1 0)))` returns `"0+1i"`.
  - `(String (exp (BigComplex 0 0)))` returns `"1+0i"`.
  - `(String (log (BigComplex 1 0)))` returns `"0+0i"`.
  - `(String (sin (BigComplex 0 0)))` returns `"0+0i"`.
  - `(String (cos (BigComplex 0 0)))` returns `"1+0i"`.
  - Complex `pow` is validated by magnitude tolerance because the underlying
    complex algorithm can leave tiny residual imaginary parts.
  - Focused advanced numeric float-math group passed on host and in the
    bounded container at `152 passed, 0 failed`.
  - Stage 3 source parity and whitespace checks passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Exact string equality is not the right regression contract for complex
    `pow`; use a magnitude tolerance for identities such as `i^2 = -1`.
- Current best recommendation/checkpoint:
  - BigComplex now supports elementary complex math. Next scalar precision
    work should be BigFloat precision-control policy or a deliberate
    BigComplex special-function/distribution policy.
- Unresolved issues / blockers:
  - Full all-slice and ASAN validation were not run for this slice.
- Signature: Codex (GPT-5)

## 2026-04-15 10:58 CEST - BigComplex Numeric Primitive
- Objective attempted:
  - Continue the scientific scalar precision lane by shipping the first
    BigComplex runtime slice.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/big_complex_helpers.cpp` and C3 extern wiring for decimal
    complex construction, cloning, destruction, rendering, arithmetic,
    equality, zero checks, and magnitude.
  - Added the `BIG_COMPLEX` value tag, payload field, runtime lifecycle hooks,
    scope copy/promotion paths, boundary audit classification, printing, hash
    and equality support.
  - Registered `BigComplex` as a callable constructor/type descriptor and a
    subtype of `Number`.
  - Routed arithmetic through BigComplex when either operand is complex:
    `+`, `-`, `*`, `/`, unary `-`, `=`, `zero?`, and `abs` are supported.
    `abs` returns `BigFloat`.
  - Ordered numeric operations fail closed for complex operands: `<`, `>`,
    `<=`, `>=`, `min`, `max`, `positive?`, and `negative?`.
  - Added focused advanced numeric regressions and updated
    `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/11-appendix-primitives.md`, and `memory/CHANGELOG.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct smokes for `(String (BigComplex 1 2))`, type identity, `Number`
    identity, addition, multiplication, division, `abs`, and ordering
    rejection
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results and observed behavior:
  - `(String (BigComplex 1 2))` returns `"1+2i"`.
  - `(= (type-of (BigComplex 1 2)) 'BigComplex)` and
    `(is? (BigComplex 1 2) 'Number)` return `true`.
  - `(String (+ (BigComplex 1 2) 3))` returns `"4+2i"`.
  - `(String (* (BigComplex 1 2) (BigComplex 3 4)))` returns `"-5+10i"`.
  - `(String (/ (BigComplex 1 2) (BigComplex 3 -4)))` returns
    `"-0.2+0.4i"`.
  - `(String (abs (BigComplex 3 4)))` returns `"5"`.
  - Focused advanced numeric float-math group passed on host and in the
    bounded container at `145 passed, 0 failed`.
  - Stage 3 source parity and whitespace checks passed.
- Invalidated assumptions or failed approaches worth preserving:
  - No failed implementation path required negative memory for this slice.
- Current best recommendation/checkpoint:
  - BigComplex scalar arithmetic is shipped. Next scalar precision work should
    be BigFloat precision-control policy or broader complex scientific math.
- Unresolved issues / blockers:
  - BigComplex transcendental functions are not implemented.
  - Full all-slice and ASAN validation were not run for this slice.
- Signature: Codex (GPT-5)

## 2026-04-15 10:20 CEST - Exact BigFloat Rounding
- Objective attempted:
  - Continue the scalar scientific numerics lane by closing the deferred exact
    BigFloat rounding-to-integer policy.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added a C++ BigFloat rounding helper that performs `floor`, `ceiling`,
    `round`, and `truncate` directly on `cpp_dec_float_50` and renders the
    rounded integer as fixed decimal text.
  - Added the C3 extern and runtime value helper to parse that decimal text
    through the BigInteger constructor, then narrow to `Integer` when the
    result fits `i64`.
  - Routed the rounding primitives through the exact BigFloat path before the
    existing Double-based path.
  - Added advanced numeric regressions for small narrowing, large BigInteger
    promotion, negative rounding semantics, and huge-result fail-closed
    behavior.
  - Updated `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/11-appendix-primitives.md`, and `memory/CHANGELOG.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build main --output-dir build --build-dir build/obj2`
  - direct smokes for large BigFloat floor promotion, result type, negative
    `round`, negative `truncate`, and over-cap failure
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results and observed behavior:
  - Large BigFloat floor now returns string `"9223372036854775808"` and type
    `BigInteger` instead of saturating or narrowing through `Double`.
  - `(round (BigFloat "-3.5"))` returns `-4`; `(truncate (BigFloat "-3.9"))`
    returns `-3`.
  - `(floor (BigFloat "1e400000"))` fails closed with
    `floor: BigFloat integer result out of supported range`.
  - Focused advanced numeric float-math group passed on host and in the bounded
    container at `134 passed, 0 failed`.
  - Stage 3 source parity and whitespace checks passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not convert rounded `cpp_dec_float_50` directly to `cpp_int` for this
    path. Local Boost conversion saturated near fixed-width limits; fixed
    decimal rendering plus BigInteger parsing is the validated path.
  - The earlier report statement that BigFloat rounding still used the
    Double-to-Integer path is superseded by this implementation.
- Current best recommendation/checkpoint:
  - Exact BigFloat integer rounding is shipped. Next scalar precision work is
    now precision-control policy or `BigComplex`.
- Unresolved issues / blockers:
  - BigFloat precision remains fixed at `cpp_dec_float_50`.
  - Full all-slice and ASAN validation were not run for this slice.
- Signature: Codex (GPT-5)

## 2026-04-15 08:48 CEST - BigFloat Scalar Math And Agent Rule
- Objective attempted:
  - Continue Omni scientific numerics non-conservatively by closing the
    BigFloat math gap instead of leaving the type at constructor/arithmetic
    support.
  - Persist the owner-requested hard anti-conservatism rule in `AGENTS.md`.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/big_float_helpers.cpp` with BigFloat-preserving wrappers for
    trig, inverse trig, exponential/logarithmic, power/root,
    `math/lgamma`, `math/erf`, `math/erfc`, `stats/normal-cdf`, and
    `stats/normal-quantile`.
  - Added C3 externs and value helper wrappers for BigFloat unary/binary math.
  - Updated math primitives so `BigFloat` operands return `BigFloat` for the
    supported scalar math/scientific helpers instead of narrowing through
    `Double`.
  - Added focused advanced numeric regressions for BigFloat math results,
    probability-domain failure, high-range `exp`, and high-precision
    `lgamma`.
  - Updated `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/11-appendix-primitives.md`, and `memory/CHANGELOG.md`.
  - Added `AGENTS.md` hard anti-conservatism rule with a mandatory
    conservative-choice tax.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `c3c build main --output-dir build --build-dir build/obj2`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - direct smokes for BigFloat `sqrt`, `pow`, `stats/normal-cdf`,
    `stats/normal-quantile`, high-range `exp`, and `math/lgamma` precision
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results and observed behavior:
  - Focused advanced numeric float-math group passed on host and in the bounded
    container at `127 passed, 0 failed`.
  - Direct smokes returned `"1024"` for `(pow (BigFloat "2") 10)`, `"0"` for
    `(stats/normal-quantile (BigFloat "0.5"))`, and `true` for
    `(= (type-of (exp (BigFloat "1000"))) 'BigFloat)`.
  - `math/lgamma` on `BigFloat "6"` matched the high-precision expected value
    within `1e-45`.
  - Stage 3 source parity and whitespace checks passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Treating BigFloat scientific functions as requiring Double-range narrowing
    is now superseded for the implemented scalar math set.
  - `c3c build` sometimes exited clean without recreating `build/main` after the
    executable had been removed; using explicit `--output-dir build --build-dir`
    restored a reliable validation artifact for this session.
- Current best recommendation/checkpoint:
  - BigFloat now has the core scalar scientific math surface. Next scalar work
    should choose precision-control policy, exact BigFloat rounding-to-integer
    behavior, or `BigComplex`.
- Unresolved issues / blockers:
  - BigFloat precision remains fixed at `cpp_dec_float_50`.
  - `floor`, `ceiling`, `round`, and `truncate` still use the existing
    Double-to-Integer path; exact BigFloat rounding-to-Integer/BigInteger needs
    a separate policy and implementation.
  - Full all-slice and ASAN validation were not run for this slice.
- Signature: Codex (GPT-5)

## 2026-04-15 07:52 CEST - BigFloat Numeric Promotion
- Objective attempted:
  - Continue the scientific numerics scalar lane with a non-conservative
    BigFloat implementation, not just a boxed constructor.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added Boost.Multiprecision `cpp_dec_float_50` helper plumbing in
    `csrc/big_float_helpers.cpp`, helper archive/project wiring, and C3 externs.
  - Added runtime `BIG_FLOAT` values with scope destruction, copy-to-parent,
    escape promotion, printing, `String`, `Double`, and `Integer` conversion.
  - Registered `BigFloat` as a callable constructor/type descriptor and
    `Number` subtype.
  - Added BigFloat support for `+`, `-`, `*`, `/`, numeric comparisons, `=`,
    `abs`, `min`, and `max`. Arithmetic returns `BigFloat` when a `BigFloat`
    operand participates.
  - Updated `parse-number` so valid floating inputs that overflow `Double`,
    such as `"1e309"`, promote to `BigFloat`.
  - Added focused advanced numeric regressions and updated language/reference
    docs plus `.agents/PLAN.md` and `memory/CHANGELOG.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (BigFloat "1.25"))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (+ (BigFloat "1.25") 2))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (/ (BigFloat "10") 4))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(= (type-of (parse-number "1e309")) '\''BigFloat)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (parse-number "1e309"))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(is? (BigFloat "1.25") '\''Number)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(/ (BigFloat "1") 0)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(sin (BigFloat "1e309"))'`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results and observed behavior:
  - Focused advanced numeric float-math group passed on host and in the bounded
    container at `115 passed, 0 failed`.
  - Direct smokes returned `"1.25"`, `"3.25"`, and `"2.5"` for constructor,
    addition, and division.
  - `parse-number "1e309"` now returns `BigFloat` and stringifies as
    `"1e+309"`.
  - `BigFloat` is recognized as a `Number`.
  - BigFloat division by zero preserves `/: division by zero`.
  - Double-returning transcendentals fail closed for out-of-Double-range
    BigFloat input instead of silently narrowing.
  - Stage 3 source parity and whitespace checks passed.
- Invalidated assumptions / failed approaches worth preserving:
  - A constructor-only BigFloat slice would be misleading because arithmetic
    gates would either reject or accidentally reinterpret the value. BigFloat
    now has the core numeric path wired with it.
  - Initial helper code assumed `cpp_dec_float_50` exposes `.isfinite()`; this
    was wrong for the local Boost version. Use `boost::math::isfinite(...)`.
- Current best recommendation/checkpoint:
  - Treat the first BigFloat numeric surface as active. Next scalar work should
    choose between precision-control APIs, broader BigFloat transcendental
    wrappers, or BigComplex.
- Unresolved issues / blockers:
  - BigFloat currently uses the fixed `cpp_dec_float_50` backend. User-visible
    precision configuration has not been designed.
  - Broader full-suite and ASAN validation were not run for this slice.
- Signature: Codex (GPT-5)

## 2026-04-15 07:35 CEST - StackCtx Boundary Copy Efficiency Narrowing
- Objective attempted:
  - Respond to the efficiency concern about the prior StackCtx boundary-copy
    safety fix.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Updated `src/lisp/eval_promotion_copy.c3` so StackCtx leaf/data-container
    values bypass the fast-reuse classifier only when current stack headroom is
    below `BOUNDARY_ALIAS_STACK_MIN_HEADROOM`.
  - Kept the original safety behavior for low-headroom effect-continuation
    returns, but restored normal scope-aware reuse classification for StackCtx
    payloads with enough stack headroom.
  - Updated `memory/CHANGELOG.md` with the validation result.
- Commands run:
  - `c3c build --obj-out obj`
  - direct nested effect payload predicate via `./build/main --eval`
  - `timeout 180s scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `timeout 180s scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 ./build/main --test-suite lisp`
  - `timeout 60s env LD_LIBRARY_PATH=/usr/local/lib:/home/christos/Omni/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-tco OMNI_TEST_SUMMARY=1 OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 ./build/main --test-suite lisp`
  - `timeout 60s env LD_LIBRARY_PATH=/usr/local/lib:/home/christos/Omni/build OMNI_LISP_TEST_SLICE=limit-busting OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `timeout 60s env LD_LIBRARY_PATH=/usr/local/lib:/home/christos/Omni/build OMNI_LISP_TEST_SLICE=tco-recycling OMNI_TEST_SUMMARY=1 OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 ./build/main --test-suite lisp`
  - `/usr/bin/time -f 'elapsed=%E exit=%x' timeout 120s env LD_LIBRARY_PATH=/usr/local/lib:/home/christos/Omni/build ./build/main --eval '(length (range 4000))'`
  - `/usr/bin/time -f 'elapsed=%E exit=%x' timeout 120s env LD_LIBRARY_PATH=/usr/local/lib:/home/christos/Omni/build ./build/main --eval '(length (range 16000))'`
- Key results and observed behavior:
  - The direct nested effect payload predicate still returns `true`.
  - Container `memory-lifetime-smoke` still passes at `225 passed, 0 failed`;
    the traversal-summary run reports `copy_fast_reuse=3` and
    `copy_defensive=89`.
  - Range/TCO checks stayed in the fixed regime: exact advanced TCO passed with
    `copy_tag_cons=0` and `copy_site_tco=0`; `limit-busting` passed `17/0`;
    `tco-recycling` passed `11/0`.
  - Direct `(length (range 4000))` took about 0.32s and
    `(length (range 16000))` about 4.18s.
- Invalidated assumptions / failed approaches worth preserving:
  - The broad "every StackCtx ordinary payload container must skip reuse
    classification" rule is too conservative now that the alias graph scan uses
    heap scratch storage. The correct safety boundary is low StackCtx headroom,
    not StackCtx presence by itself.
- Current best recommendation/checkpoint:
  - Keep reuse classification active for StackCtx returns when headroom allows
    it. Retain direct defensive copy as the low-headroom fallback for leaf values
    and ordinary data containers.
- Unresolved issues / blockers:
  - Full all-slice and ASAN validation were not run for this narrowing.
- Signature: Codex (GPT-5)

## 2026-04-15 06:51 CEST - Parse Number BigInteger Promotion
- Objective attempted:
  - Continue the scalar precision lane by implementing automatic BigInteger
    promotion for `parse-number` decimal integer overflow.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Updated `src/lisp/prim_string_convert.c3` so syntactically valid decimal
    integer overflow/underflow returns `BigInteger` instead of `nil`.
  - Preserved maybe-valued parse failure behavior for malformed strings by
    verifying the rest of the decimal input before calling the BigInteger
    constructor.
  - Updated `Double` string coercion so a `parse-number` BigInteger result can
    narrow to finite `Double`.
  - Added focused advanced numeric regressions in
    `src/lisp/tests_advanced_stdlib_numeric_groups.c3`.
  - Updated `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`,
    `docs/reference/04-type-system.md`,
    `docs/plans/number-parse-surface-decision-2026-04-11.md`, and
    `memory/CHANGELOG.md`.
- Commands run:
  - `c3c build --obj-out obj`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (parse-number "9223372036854775808"))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (parse-number "-9223372036854775809"))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(= (type-of (parse-number "9223372036854775808")) '\''BigInteger)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(= (type-of (parse-number "-9223372036854775808")) '\''Integer)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(parse-number "9223372036854775808x")'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(Double "9223372036854775808")'`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results and observed behavior:
  - `parse-number` now returns `BigInteger` for valid decimal strings outside
    fixed-width `Integer` range.
  - The exact `long.min` string still returns fixed-width `Integer`.
  - Malformed overflow-looking strings such as
    `"9223372036854775808x"` still return `nil`.
  - Focused advanced numeric float-math group passed on host and in the bounded
    container at `101 passed, 0 failed`.
  - Stage 3 source parity and whitespace checks passed.
- Invalidated assumptions / failed approaches worth preserving:
  - Do not keep treating `parse-number` integer overflow/underflow as `nil`;
    that contract is superseded for syntactically valid decimal integer input.
- Current best recommendation/checkpoint:
  - Treat the BigInteger scalar follow-up lane as closed through constructor,
    exact arithmetic, comparisons, bitwise operations, and `parse-number`
    decimal overflow promotion.
- Unresolved issues / blockers:
  - Source literals wider than fixed-width `Integer` still fail lexing; this
    slice only changes string parsing through `parse-number`.
  - BigFloat/BigComplex representation, precision policy, and lifetimes remain
    the next scalar precision design problem.
- Signature: Codex (GPT-5)

## 2026-04-15 06:42 CEST - BigInteger Bitwise Primitives
- Objective attempted:
  - Continue the scalar Boost.Multiprecision lane by closing the deferred
    BigInteger bitwise primitive surface.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/big_integer_helpers.cpp` with Boost.Multiprecision-backed
    bitwise binary ops, bitwise complement, and left/right shifts.
  - Added C3 extern declarations in `src/lisp/big_integer_backend.c3`.
  - Added BigInteger bitwise helper plumbing in
    `src/lisp/value_big_integer.c3`.
  - Updated `src/lisp/prim_math_core.c3` so `bitwise-and`, `bitwise-or`,
    `bitwise-xor`, `bitwise-not`, `lshift`, and `rshift` accept exact
    `Integer`/`BigInteger` operands.
  - Added focused advanced numeric bitwise regressions in
    `src/lisp/tests_advanced_stdlib_numeric_groups.c3`.
  - Updated `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`, and
    `memory/CHANGELOG.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-sort-bitwise-hof OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-sort-bitwise-hof OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (bitwise-and (BigInteger "18446744073709551615") 255))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (lshift 1 64))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (rshift (BigInteger "1267650600228229401496703205376") 100))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(lshift (BigInteger "1") -1)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(lshift 1 (BigInteger "9223372036854775808"))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(lshift 1 1048577)'`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- Key results and observed behavior:
  - Focused advanced numeric bitwise group passed on host and in the bounded
    container at `35 passed, 0 failed`.
  - Direct BigInteger bitwise-and smoke returned `"255"`.
  - `lshift 1 64` now returns `"18446744073709551616"` through overflow
    promotion instead of the old machine-width `0`.
  - BigInteger right shift smoke returned `"1"`.
  - Negative shift counts still return `0`.
  - Shift counts too large to narrow to `Integer`, or above the bounded
    exact-shift cap (`1048576` bits), fail closed with
    `shift count out of range`.
  - Stage 3 source parity passed.
- Invalidated assumptions / failed approaches worth preserving:
  - Do not treat the old `shift >= 64 -> 0` behavior as the exact-integer
    contract for `lshift`; it is superseded by promotion for non-negative exact
    integer shifts.
- Current best recommendation/checkpoint:
  - Treat BigInteger bitwise operations as shipped. Remaining scalar precision
    follow-ups are arbitrary-precision `parse-number` policy and the larger
    `BigFloat`/`BigComplex` representation work.
- Unresolved issues / blockers:
  - Shift counts are still bounded; unbounded BigInteger shift counts and
    representable-but-huge counts are intentionally rejected to avoid accidental
    huge allocations.
- Signature: Codex (GPT-5)

## 2026-04-15 06:31 CEST - Signed Long-Min Lexer Boundary
- Objective attempted:
  - Fix the raw source literal `-9223372036854775808` after the prior
    BigInteger primitive slice identified it as a lexer boundary failure.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Updated `src/lisp/parser_lexer_symbol_number.c3` so decimal integer
    scanning permits exactly the negative `long.min` magnitude while still
    rejecting positive overflow and negative underflow.
  - Updated `src/lisp/parser_lexer_number_helpers.c3` so float scanning uses a
    separate `double` integer-part accumulator. This avoids accidentally
    mis-signing `-9223372036854775808.0` while the integer scanner handles the
    `long.min` token boundary.
  - Added basic-suite regressions in `src/lisp/tests_core_groups.c3`.
  - Updated `docs/LANGUAGE_SPEC.md`, `docs/SYNTAX_SPEC.md`, and
    `memory/CHANGELOG.md`.
- Commands run:
  - `c3c build --obj-out obj`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '-9223372036854775808'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '-9223372036854775809'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '9223372036854775808'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '-9223372036854775808.0'`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results and observed behavior:
  - Raw `-9223372036854775808` now parses and evaluates to the fixed-width
    integer minimum.
  - Raw `-9223372036854775809` and `9223372036854775808` still fail with
    `integer literal overflow`.
  - Raw `-9223372036854775808.0` parses as a negative `Double`.
  - Host and bounded-container basic slices both passed at `144 passed,
    0 failed`.
  - Stage 3 source parity and whitespace checks passed.
- Invalidated assumptions / failed approaches worth preserving:
  - The prior operational warning that source literal `-9223372036854775808`
    cannot be used is superseded after this commit. It remains true only for
    older checkpoints before the lexer fix.
- Current best recommendation/checkpoint:
  - Treat fixed-width integer source literals as covering the full signed range
    `long.min..long.max`. Wider decimal integers should still use the
    `BigInteger` constructor until arbitrary-precision literal or
    `parse-number` policy is explicitly implemented.
- Unresolved issues / blockers:
  - This does not add BigInteger source literals; positive overflow and
    negative underflow still intentionally fail in the lexer.
- Signature: Codex (GPT-5)

## 2026-04-15 06:18 CEST - BigInteger Exact Number Primitives
- Objective attempted:
  - Explain and fix the observed BigInteger `gcd` failure by closing the next
    scalar exact-number primitive slice.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/big_integer_helpers.cpp` with Boost.Multiprecision-backed
    `gcd` and `lcm` op codes.
  - Added shared helper plumbing in `src/lisp/value_big_integer.c3` for exact
    integer predicates and optional narrowing of BigInteger helper results back
    to `Integer` when an overflow-boundary operation's exact result fits.
  - Updated `src/lisp/prim_math_core.c3` so `abs`, `min`, `max`, `gcd`, and
    `lcm` use the BigInteger-aware numeric path instead of rejecting
    `BigInteger` at the older fixed-width-only gates.
  - Updated advanced numeric tests so the old long-min overflow expectations
    match the current auto-promotion contract.
  - Updated `.agents/PLAN.md`, `docs/LANGUAGE_SPEC.md`, and
    `memory/CHANGELOG.md`.
- Commands run:
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '-9223372036854775808'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(- -9223372036854775807 1)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(gcd (BigInteger "9223372036854775808") 2)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (/ (BigInteger "18446744073709551616") 2))'`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-string-predicate-format OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp && env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-string-predicate-format OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (gcd (BigInteger "18446744073709551616") 24))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (lcm (Integer "-9223372036854775808") 2))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (abs (BigInteger "-5")))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (max (BigInteger "9223372036854775808") 1))'`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
- Key results and observed behavior:
  - The original failure was not a Boost helper issue: `gcd` still required
    `INT` operands and rejected `BIG_INTEGER` before dispatching to exact
    arithmetic.
  - The raw source literal `-9223372036854775808` still fails at parse time as
    `integer literal overflow`; use `(Integer "-9223372036854775808")` or build
    long-min from in-range literals for runtime arithmetic tests.
  - Helper archive rebuild and C3 build passed.
  - Focused advanced numeric float-math group passed at `98 passed, 0 failed`.
  - Focused advanced numeric string-predicate-format group passed at
    `61 passed, 0 failed`.
  - Bounded container rerun of those two advanced numeric group filters also
    passed at `98 passed, 0 failed` and `61 passed, 0 failed`.
  - Direct smokes returned `"8"` for BigInteger `gcd`, `"9223372036854775808"`
    for long-min `lcm`, `"5"` for BigInteger `abs`, and
    `"9223372036854775808"` for BigInteger `max`.
  - Stage 3 source parity passed.
- Invalidated assumptions / failed approaches worth preserving:
  - Do not treat `is_number`/`is_int` primitive gates as BigInteger-ready. New
    numeric primitives that should accept exact integers need `is_numeric_value`
    or `is_exact_integer_value`.
- Current best recommendation/checkpoint:
  - Treat BigInteger `abs`, `min`, `max`, `gcd`, and `lcm` as shipped. The next
    scalar precision follow-up should be BigInteger bitwise operations,
    arbitrary-precision `parse-number`, or the larger `BigFloat`/`BigComplex`
    representation work.
- Unresolved issues / blockers:
  - The parser still intentionally rejects the raw `-9223372036854775808`
    source token; changing literal policy is separate from runtime promotion.
- Signature: Codex (GPT-5)

## 2026-04-15 05:31 CEST - BigInteger Division Modulo And Ordering
- Objective attempted:
  - Continue the scalar precision lane by closing the deferred BigInteger
    `/`, `%`, and ordering-comparison surface before taking on BigFloat or
    larger parsing policy.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/big_integer_helpers.cpp` so the Boost.Multiprecision
    `cpp_int` helper supports division and modulo op codes.
  - Added shared BigInteger-aware numeric comparison helpers in
    `src/lisp/value_big_integer.c3`.
  - Updated `src/lisp/prim_math_arithmetic.c3` so `/` supports exact
    `Integer`/`BigInteger` quotient semantics when no `Double` participates,
    promotes the `long.min / -1` overflow boundary to `BigInteger`, and keeps
    mixed `Double` operations on the existing finite-narrowing path.
  - Updated `%` to accept `Integer` and `BigInteger` operands, with
    deterministic division-by-zero behavior and `long.min % -1` returning `0`.
  - Updated `src/lisp/primitives_core.c3` and
    `src/lisp/jit_jit_apply_multi_prims_tail.c3` so `<`, `>`, `<=`, and `>=`
    compare BigInteger exactly against Integer/BigInteger; comparisons involving
    `Double` require finite Double conversion.
  - Added focused regression coverage in
    `src/lisp/tests_advanced_stdlib_numeric_groups.c3`.
  - Updated `docs/LANGUAGE_SPEC.md`, `.agents/PLAN.md`, and
    `memory/CHANGELOG.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (/ (BigInteger "18446744073709551616") 2))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (% (BigInteger "9223372036854775810") 3))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(< (BigInteger "9223372036854775808") (BigInteger "9223372036854775809"))'`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results and observed behavior:
  - Helper archive rebuild and full C3 build passed.
  - Focused advanced numeric float-math group passed at `90 passed, 0 failed`;
    the group exercises interpreter and JIT paths for the added comparison
    tests.
  - Direct smokes returned `"9223372036854775808"` for BigInteger division,
    `"1"` for BigInteger modulo, and `true` for BigInteger less-than.
  - An initial test-only attempt used the literal
    `-9223372036854775808`, which fails at parse time before runtime overflow
    semantics are exercised. The regression now constructs `long.min` through
    `(- -9223372036854775807 1)`.
  - Stage 3 source parity and `git diff --check` passed.
- Invalidated assumptions / failed approaches worth preserving:
  - Do not use `-9223372036854775808` as a source literal when testing runtime
    `long.min` arithmetic; build it from in-range literals instead.
- Current best recommendation/checkpoint:
  - Treat BigInteger `/`, `%`, and ordering comparisons as shipped for exact
    integer operands. Remaining scalar precision work is now `BigFloat` /
    `BigComplex`, BigInteger bitwise operations, `gcd`/`lcm`, and
    arbitrary-precision `parse-number` policy.
- Unresolved issues / blockers:
  - Comparisons involving `Double` intentionally use finite Double narrowing;
    exact integer-vs-decimal comparison is a separate BigFloat/decimal policy
    question.
- Signature: Codex (GPT-5)

## 2026-04-14 22:27 CEST - Boost.Math Standard Normal Wrappers
- Objective attempted:
  - Continue the scalar scientific numerics plan by adding the first
    Boost.Math distribution helpers on top of the validated unary scalar
    wrapper pattern.
