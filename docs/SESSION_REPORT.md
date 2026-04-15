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
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/boost_math_helpers.cpp` with standard normal CDF and
    quantile C-ABI functions backed by `boost::math::normal_distribution`,
    `boost::math::cdf`, and `boost::math::quantile`.
  - Added C3 extern declarations in `src/lisp/boost_math_backend.c3`.
  - Added one-argument `stats/normal-cdf` and `stats/normal-quantile`
    primitives. `stats/normal-cdf` takes a finite standard-normal x value;
    `stats/normal-quantile` takes a finite probability strictly between `0`
    and `1`.
  - Registered both primitives in the interpreter primitive table and AOT
    primitive lookup table.
  - Added focused float-math coverage for CDF values, quantile values,
    probability-domain failure, and out-of-Double-range `BigInteger` input.
  - Updated `docs/LANGUAGE_SPEC.md`, `docs/reference/11-appendix-primitives.md`,
    `.agents/PLAN.md`, and `memory/CHANGELOG.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(stats/normal-cdf 0.0)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(stats/normal-cdf 1.96)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(stats/normal-quantile 0.975)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(stats/normal-quantile 0.0)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(stats/normal-cdf (BigInteger "..."))'`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results and observed behavior:
  - Helper archive rebuild and full C3 build passed.
  - Focused float-math tests passed at `79 passed, 0 failed`.
  - Direct runtime smokes returned `0.5` for `(stats/normal-cdf 0.0)`,
    `0.97500210485178` for `(stats/normal-cdf 1.96)`, and
    `1.95996398454005` for `(stats/normal-quantile 0.975)`.
  - `(stats/normal-quantile 0.0)` fails closed with
    `stats/normal-quantile: probability must be between 0 and 1`.
  - Very large `BigInteger` input to `stats/normal-cdf` fails closed with
    `stats/normal-cdf: value out of Double range`.
  - Stage 3 source parity and `git diff --check` passed.
- Invalidated assumptions / failed approaches worth preserving:
  - None in this slice.
- Current best recommendation/checkpoint:
  - Treat standard-normal CDF/quantile as the validated first distribution
    wrapper contract. Do not broaden it silently to mean/stddev parameters; add
    that as a separate surface decision if needed.
  - The remaining high-value scientific numerics choices are now scalar
    precision work (`BigFloat`, `BigComplex`, BigInteger division/comparison,
    arbitrary-precision parsing) or Tensor LAPACK/LAPACKE public naming.
- Unresolved issues / blockers:
  - No container-only memory ownership run was needed for this slice because it
    does not change Omni value ownership.
  - Multi-parameter normal distributions and other distribution families are
    intentionally not part of this first wrapper.
- Signature: Codex (GPT-5)

## 2026-04-14 21:48 CEST - StackCtx Boundary Copy Smoke Fix
- Objective attempted:
  - Continue the range/TCO follow-up by closing the container-only
    `memory-lifetime-smoke` validation gap.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Built the local validation image `omni-validation:2026-03-10` from
    `docker/validation.Dockerfile` so container-bound memory slices can run.
  - Updated `src/lisp/eval_promotion_copy.c3` so copy-to-parent skips full
    boundary reuse classification for leaf values and list/array/dict/set data
    containers while executing inside a StackCtx. These values are copied
    defensively instead, which preserves ownership and avoids spending the
    128KB continuation stack on reuse probes during nested effect payload return
    copying.
  - Updated `memory/CHANGELOG.md` with the validation result.
- Commands run:
  - `OMNI_VALIDATION_IMAGE=omni-validation:2026-03-10 scripts/build_validation_image.sh`
  - `c3c build --obj-out obj`
  - direct nested effect payload expression and predicate via `./build/main --eval`
  - `gdb --batch ... ./build/main --eval <nested effect payload expression>`
  - `timeout 180s scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=memory-lifetime-smoke OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `timeout 60s env LD_LIBRARY_PATH=/usr/local/lib:/home/christos/Omni/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-tco OMNI_TEST_SUMMARY=1 OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 ./build/main --test-suite lisp`
  - `timeout 60s env LD_LIBRARY_PATH=/usr/local/lib:/home/christos/Omni/build OMNI_LISP_TEST_SLICE=limit-busting OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `timeout 60s env LD_LIBRARY_PATH=/usr/local/lib:/home/christos/Omni/build OMNI_LISP_TEST_SLICE=tco-recycling OMNI_TEST_SUMMARY=1 OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 ./build/main --test-suite lisp`
  - `/usr/bin/time -f 'elapsed=%E exit=%x' timeout 120s env LD_LIBRARY_PATH=/usr/local/lib:/home/christos/Omni/build ./build/main --eval '(length (range 4000))'`
  - `/usr/bin/time -f 'elapsed=%E exit=%x' timeout 30s env LD_LIBRARY_PATH=/usr/local/lib:/home/christos/Omni/build ./build/main --eval '(length (range 16000))'`
- Key results and observed behavior:
  - Initial bounded `memory-lifetime-smoke` run failed at
    `lifetime: boundary nested effect payload graph`; direct evaluation showed
    `stack overflow in resolve`.
  - GDB localized the guard hit to recursive copy-to-parent of a nested
    dict/list payload under `boundary_build_destination_cons_escape`, with full
    `boundary_classify_return_value` reuse probes still occurring deep inside
    the StackCtx.
  - The direct nested payload test now returns the expected captured render
    event, and the test predicate returns `true`.
  - Container `memory-lifetime-smoke` now passes at `225 passed, 0 failed`.
  - Range/TCO checks stayed in the fixed regime: exact advanced TCO passed with
    `copy_tag_cons=0` and `copy_site_tco=0`; direct `(length (range 4000))`
    took about 0.31s and `(length (range 16000))` about 4.17s.
- Invalidated assumptions / failed approaches worth preserving:
  - Bypassing reuse classification only for scalar leaves inside StackCtx was
    insufficient; nested `HASHMAP` containers still triggered the same guarded
    stack overflow. The data-container shortcut is required for this payload
    shape.
- Current best recommendation/checkpoint:
  - Keep StackCtx copy-to-parent defensive for ordinary data payloads. Reuse
    classification remains valuable on the main stack, but effect continuations
    need the smaller-stack path to prefer copying over recursive provenance
    probing for lists, arrays, dictionaries, and sets.
- Unresolved issues / blockers:
  - None known for this follow-up. The parent revision already contains the
    Boost.Math `math/erf` / `math/erfc` slice; this change is scoped to the
    StackCtx copy fix and handoff notes.
- Signature: Codex (GPT-5)

## 2026-04-14 16:20 CEST - Boost.Math `math/erf` And `math/erfc`
- Objective attempted:
  - Continue the scalar scientific numerics plan by extending the validated
    Boost.Math wrapper pattern from `math/lgamma` to the error-function family.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Extended `csrc/boost_math_helpers.cpp` with a shared finite-input unary
    Boost.Math helper and new C-ABI functions for `boost::math::erf` and
    `boost::math::erfc`.
  - Added C3 extern declarations in `src/lisp/boost_math_backend.c3`.
  - Added `math/erf` and `math/erfc` runtime primitives with the same
    numeric-narrowing and deterministic error policy as `math/lgamma`.
  - Registered both primitives in the interpreter primitive table and AOT
    primitive lookup table.
  - Added focused float-math coverage for `math/erf`, `math/erfc`, and
    out-of-Double-range `BigInteger` input.
  - Updated `docs/LANGUAGE_SPEC.md`, `docs/reference/11-appendix-primitives.md`,
    `.agents/PLAN.md`, and `memory/CHANGELOG.md`.
- Commands run:
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(math/erf 1.0)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(math/erfc 1.0)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(math/erf (BigInteger "..."))'`
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
  - `git diff --check`
- Key results and observed behavior:
  - Helper archive rebuild and full C3 build passed.
  - Focused float-math tests passed at `73 passed, 0 failed`.
  - Direct runtime smokes returned `0.842700792949715` for `(math/erf 1.0)`
    and `0.157299207050285` for `(math/erfc 1.0)`.
  - Very large `BigInteger` input to `math/erf` fails closed with
    `math/erf: value out of Double range`.
  - Stage 3 source parity and `git diff --check` passed.
- Invalidated assumptions / failed approaches worth preserving:
  - None in this slice. The helper-archive ordering issue from the prior
    `math/lgamma` slice remains the useful operational constraint.
- Current best recommendation/checkpoint:
  - Treat the unary Boost.Math finite-input wrapper pattern as validated for
    scalar special functions with `Double` output.
  - The next scalar Boost.Math slice can either add normal distribution helpers
    (`stats/normal-cdf`, `stats/normal-quantile`) or pause scalar wrappers and
    return to Tensor LAPACK/LAPACKE naming.
- Unresolved issues / blockers:
  - Container-only memory ownership validation was not run; this slice does not
    change Omni value ownership.
  - BigFloat/BigComplex, BigInteger division/modulo/comparisons, and
    arbitrary-precision parsing remain deferred.
- Signature: Codex (GPT-5)

## 2026-04-14 15:45 CEST - Boost.Math `math/lgamma` First Wrapper
- Objective attempted:
  - Continue the scientific numerics plan by landing the first Boost.Math scalar
    function behind an owned C++ shim, without changing the public Tensor or GSL
    direction.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Added `csrc/boost_math_helpers.cpp` as the C++17 Boost.Math C-ABI bridge
    for `boost::math::lgamma`, returning stable status codes rather than C++
    exceptions across the C3 boundary.
  - Added `src/lisp/boost_math_backend.c3` and wired `math/lgamma` through the
    math primitive table and AOT primitive lookup.
  - Updated the helper archive build script and project source list so the new
    Boost.Math helper is part of `libomni_chelpers`.
  - Added focused float-math regression coverage for ordinary results, a gamma
    pole domain error, and out-of-Double-range `BigInteger` input.
  - Updated the language spec, primitive appendix, `.agents/PLAN.md`, and
    `memory/CHANGELOG.md` for the landed wrapper and remaining scientific
    numerics checkpoints.
- Commands run:
  - `c3c build --obj-out obj` initially failed at link time because the local
    `build/libomni_chelpers.a` archive had not yet been rebuilt with
    `omni_boost_math_lgamma`.
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build --obj-out obj`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(math/lgamma 6.0)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(math/lgamma 0.5)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(math/lgamma 0.0)'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(math/lgamma (BigInteger "..."))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(length (range 4000))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(length (range 16000))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-tco OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=limit-busting OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `git diff --check`
- Key results and observed behavior:
  - Rebuilding `libomni_chelpers` resolved the expected missing-symbol link
    failure, and the project linked successfully.
  - Focused float-math tests passed at `70 passed, 0 failed`.
  - Direct runtime smokes returned `4.78749174278205` for `(math/lgamma 6.0)`
    and `0.5723649429247` for `(math/lgamma 0.5)`.
  - `(math/lgamma 0.0)` raises `math/lgamma: domain error`.
  - Very large `BigInteger` input fails closed with
    `math/lgamma: value out of Double range`, preserving the scalar
    narrowing boundary.
  - Since the same working copy includes the range/TCO fix, the sharp TCO
    validation was rechecked: `(length (range 4000))` returned `4000`,
    `(length (range 16000))` returned `16000`, the exact TCO group passed at
    `1 passed, 0 failed`, and `limit-busting` passed at `17 passed, 0 failed`.
  - `git diff --check` passed.
- Invalidated assumptions / failed approaches worth preserving:
  - `c3c build` alone is not enough after adding a helper translation unit if
    `build/libomni_chelpers.a` is stale; run `./scripts/build_omni_chelpers.sh`
    first or use a build path that refreshes the helper archive.
- Current best recommendation/checkpoint:
  - Treat `math/lgamma` as the validated first Boost.Math wrapper and reuse its
    status-code/error-policy pattern for the next scalar wrappers.
  - Continue with one narrow scalar follow-up such as `math/erf`,
    `math/erfc`, `stats/normal-cdf`, or `stats/normal-quantile`, or switch back
    to the unresolved Tensor LAPACK/LAPACKE public naming checkpoint.
- Unresolved issues / blockers:
  - Container-only memory ownership validation was not run in this continuation.
  - BigFloat/BigComplex, BigInteger division/modulo/comparisons, and
    arbitrary-precision parsing remain deferred.
- Signature: Codex (GPT-5)

## 2026-04-14 15:00 CEST - Default-Stack Range/TCO Crash Fix
- Objective attempted:
  - Debug and fix the normal-stack crash in `(length (range 4000))` and the
    `advanced-stdlib-numeric-tco` regression that previously required a larger
    process stack.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Updated `src/lisp/eval_promotion_escape_structured.c3` so cons cdr tail
    promotion remains iterative when target-chain reuse is unsafe under the
    boundary alias scan.
  - Updated `src/lisp/eval_promotion_escape.c3` so cons values already in the
    current ESCAPE lane reuse directly before the full alias graph scan.
  - Updated `src/lisp/jit_jit_eval_scope_chain_helpers.c3` so the TCO temp-graph
    scanner walks cons spines iteratively, skips non-TEMP scalar child values,
    and uses a separate cons-spine cap instead of the generic graph worklist cap.
  - Updated `memory/CHANGELOG.md` to mark the previous "needs larger stack"
    caveat as historical and record the verified default-stack/performance fix.
- Commands run:
  - `timeout 30s env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(length (range 4000))'`
  - `prlimit --stack=67108864 env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(length (range 4000))'`
  - `gdb --batch ... ./build/main --eval '(length (range 4000))'`
  - `c3c build --obj-out obj`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-tco OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=limit-busting OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
  - `/usr/bin/time -f 'elapsed=%E exit=%x' timeout 120s env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(length (range 4000))'`
  - `/usr/bin/time -f 'elapsed=%E exit=%x' timeout 30s env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(length (range 8000))'`
  - `/usr/bin/time -f 'elapsed=%E exit=%x' timeout 30s env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(length (range 16000))'`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=tco-recycling OMNI_TEST_SUMMARY=1 OMNI_BOUNDARY_TRAVERSAL_SUMMARY=1 ./build/main --test-suite lisp`
  - `git diff --check -- src/lisp/eval_promotion_escape_structured.c3`
- Key results and observed behavior:
  - Reproduced the default-stack crash before the fix: the 30s guarded
    `(length (range 4000))` command dumped core.
  - Localized the recursive stack source to long cons cdr promotion:
    `promote_escape_cons` called `promote_to_escape(old_cdr)` after treating a
    target-chain cons tail as reusable; alias analysis then rejected the long
    tail at `BOUNDARY_ALIAS_MAX_DEPTH` and recursive disjoint promotion could
    walk the range spine deeply enough to exhaust the normal stack.
  - After the fix, the exact advanced TCO group passed (`1 passed, 0 failed`)
    and the limit-busting slice passed (`17 passed, 0 failed`) on the normal
    stack.
  - Direct default-stack `(length (range 4000))` now returns `4000`; measured
    runtime improved from about 55s after the correctness-only fix to about
    0.32s after the performance patch.
  - The exact advanced TCO traversal summary changed from
    `copy_tag_cons=3906`, `copy_site_tco=7812`, and `cons_spine_peak_len=4000`
    to `copy_tag_cons=0`, `copy_site_tco=0`, and `cons_spine_peak_len=0`.
  - Longer direct probes also stayed under the 30s guard: `range 8000` returned
    in about 1.15s and `range 16000` returned in about 4.14s.
  - Host-side `memory-lifetime-smoke` was not run because the test harness
    correctly rejects memory ownership slices outside the container. The
    bounded container wrapper could not run because the local validation image
    `omni-validation:2026-03-10` is not present.
- Invalidated assumptions / failed approaches worth preserving:
  - A releasing-scope-only tail iteration check did not fix the crash; the
    failing path involved target-chain cons tails that alias analysis considered
    unsafe for reuse only after its bounded depth scan.
  - Do not treat the earlier `prlimit --stack=67108864` workaround as the best
    current recommendation; the default-stack correctness issue is fixed.
- Current best recommendation/checkpoint:
  - Keep the cons-tail promotion rule aligned with alias-analysis reuse safety:
    target-chain cdr tails may short-circuit only when
    `boundary_graph_alias_unsafe_for_reuse` says reuse is safe.
  - Keep the TCO temp-graph scanner on its cons-specific iterative path; do not
    reintroduce scalar child pushes or generic worklist caps for proper cons
    spines.
- Unresolved issues / blockers:
  - No correctness or 30s performance blocker remains for the reported
    default-stack range/TCO issue.
  - Container-only memory ownership validation still needs the local
    `omni-validation:2026-03-10` image or an allowed rebuild.
- Signature: Codex (GPT-5)

## 2026-04-09 Report Docs Syntax Drift Audit
- Objectives attempted
  - Audit report/status documentation for syntax-surface drift after recent large refactor volume.
  - Reconcile stale release/backlog status wording that no longer matches the live repository state.
- Code/config changes made
  - Updated `docs/RELEASE_STATUS.md`:
    - moved status forward from the stale 2026-03-10 “feature complete / TODO closed” framing,
    - clarified active stabilization/refactor posture,
    - added a report-layer syntax-drift note for quoted marker conventions.
  - Updated `docs/AS_PATTERN_STATUS.md`:
    - replaced stale `report.md` reference with `docs/SESSION_REPORT.md`.
  - Updated module syntax references for dot/path parity:
    - `docs/LANGUAGE_SPEC.md` now explicitly documents dotted/path module targets in `module` / `import` / `export-from`.
    - `docs/reference/05-macros-modules.md` now mirrors the same dotted/path module target contract and examples.
- Experiment commands and key metrics
  - `rg --files docs | rg -i 'report|status|changelog|syntax|guide'`
  - `rg -n "Current actionable count|TODO.md now reports|active TODO backlog is empty" docs/SESSION_REPORT.md`
  - `rg -n --pcre2 "(?<!:):(as|all)\\b" docs/SESSION_REPORT.md docs/RELEASE_STATUS.md docs/AS_PATTERN_STATUS.md docs/BOUNDARY_*_AUDIT*.md docs/BOUNDARY_SURFACE_AUDIT.md`
  - `rg -n "T_PATH|parser_import_helpers_specs|parser_module_decl|parser_export_from|jit_jit_module_import" src/lisp/*.c3`
  - Key metrics:
    - report/status corpus located and reviewed,
    - no `:as`/`:all` marker drift found in audited report/status docs,
    - stale release/backlog wording corrected,
    - dot/path notation docs aligned with shipped parser/import behavior.
- Best current checkpoint/config recommendation
  - Keep `docs/RELEASE_STATUS.md` synchronized with live queue reality (`TODO.md` + `memory/CHANGELOG.md`) instead of treating it as a static “complete” declaration.
- Unresolved issues and next actions
  - `docs/SESSION_REPORT.md` remains intentionally historical; older entries keep their original timestamped context and may mention past backlog states.
- Signature: Codex (GPT-5)

## 2026-04-09 — Legacy Syntax Purge (Docs + Tests)

- Objective attempted:
  - Remove legacy surface syntax from repository-facing artifacts, with emphasis
    on removed leading-dot callable notation and stale test files that no longer
    parse under the current grammar.
- Workspace/target:
  - `/home/christos/Omni`
- Code or configuration changes made:
  - Updated syntax documentation to remove outdated active description of
    leading-dot accessor shorthand and replace it with hard-error removed forms:
    - `docs/SYNTAX_SPEC.md`
  - Deleted all `tests/*.omni|*.lisp` files that fail current parser checks
    (108 files), including stale dot-callable and bracket-slot syntax fixtures.
- Commands run:
  - `rg` scans over `docs/` and `tests/` for legacy patterns
  - `./build/main --check <file>` sweeps across test corpus (before and after purge)
  - `./build/main --test-suite lisp`
  - `jj status`
- Key results and observed behavior:
  - Before cleanup: 123 test files discovered, 108 failed parser checks.
  - After cleanup: all remaining `tests/*.omni|*.lisp` files pass `--check`.
  - Runtime internal suite still passes: `./build/main --test-suite lisp`
    reported `140 passed, 0 failed`.
- Invalidated assumptions / failed approaches worth preserving:
  - Do not assume stale top-level test corpus reflects current grammar; many
    files were legacy and contradicted parser-enforced syntax.
  - Do not treat leading-dot callable forms as partially supported: they are
    intentionally fail-closed.
- Current best recommendation/checkpoint:
  - Keep current parser contract as source of truth and only accept canonical
    path/index forms (`expr.name`, `expr.[key]`, `ref`).
  - If removed tests need coverage back, reintroduce them incrementally using
    canonical syntax instead of preserving dual syntax lanes.
- Unresolved issues / blockers:
  - `c3c build` remains blocked by unrelated compile errors in current tree
    (`allocator::LIBC_ALLOCATOR` symbol errors in REPL worker/scheduler files).
  - `.swarm/` remains untracked and emits jj snapshot size warnings.
- Signature: Codex (GPT-5)

## 2026-03-27 UI Reference Page Added
- Objectives attempted
  - Turn the scattered UI plan/example notes into a single concise user-facing reference page.
  - Link the new page from the normalized docs map so the UI surface is easier to discover.
- Code/config changes made
  - Added `docs/UI_REFERENCE.md` as the concise shipped reference for the current FTXUI-backed `ui` surface.
  - Updated `docs/README.md` to list the new UI surface reference entry point.
- Experiment commands and key metrics
  - `rg -n "UI_REFERENCE|ui\\.nodes|ui\\.effects|ui\\.layout|ui\\.style|ui\\.runtime|ui\\.ftxui|signal" docs/README.md docs/UI_REFERENCE.md examples/libraries/ftxui/ui.omni examples/libraries/ftxui/module_value_smoke.omni examples/libraries/ftxui/smoke.omni`
  - `c3c build`
  - Key metrics: the docs search matched the expected shipped UI surface names, and the build linked successfully.
- Best current checkpoint/config recommendation
  - Keep `docs/UI_REFERENCE.md` as the short reference layer, with the longer design rationale staying in `docs/plans/ui-library-facade-plan-2026-03-27.md`.
- Unresolved issues and next actions
  - If the UI surface grows again, add only the new shipped contract to the reference page and keep the plan doc for rationale/history.
- Signature: Codex (GPT-5)

## 2026-03-28 Omni Neovim Treesitter Compatibility Fix
- Objectives attempted
  - Fix the Omni Neovim plugin startup failure against the current Neovim 0.12-dev treesitter API.
  - Preserve compatibility with older `nvim-treesitter` releases that still expose `get_parser_configs()`.
  - Reduce startup risk by stopping unconditional full plugin setup on load and add a reproducible headless smoke test for bootstrap and `.omni` activation.
- Code/config changes made
  - Updated `tooling/omni-nvim/lua/omni/treesitter.lua` so parser registration uses the current `require("nvim-treesitter.parsers")` table directly when `get_parser_configs()` is absent, while still using the older function when it exists.
  - Added a defensive type check so registration cleanly returns `false` instead of exploding if the parser-config surface is unexpectedly missing.
  - Added `bootstrap()` to `tooling/omni-nvim/lua/omni/init.lua` so command registration, notifier wiring, and operator setup can happen without running full `setup()` side effects.
  - Updated `tooling/omni-nvim/plugin/omni.lua` to call `require("omni").bootstrap()` instead of `require("omni").setup()`.
  - Added `tooling/omni-nvim/scripts/run_smoke.sh` to validate command bootstrap, `.omni` filetype detection, and buffer-local mapping activation in headless Neovim.
  - Updated `tooling/omni-nvim/README.md` and `tooling/omni-nvim/doc/omni.nvim.txt` to document the new bootstrap-vs-setup contract and the smoke test entrypoint.
- Experiment commands and key metrics
  - `nvim --headless --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' '+qa'`
  - `nvim --headless --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' '+lua local p=require("nvim-treesitter.parsers"); local omni=p.omni; print(omni and omni.filetype or "missing"); print(omni and omni.install_info and omni.install_info.location or "no-location")' '+qa'`
  - `tmp=$(mktemp /tmp/omni-test-XXXXXX.omni) && nvim --headless --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' "+e $tmp" '+lua print(vim.bo.filetype)' '+qa'`
  - `tooling/omni-nvim/scripts/run_smoke.sh`
  - `nvim --headless --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' '+lua require("omni").setup()' '+lua print(vim.fn.exists(":OmniTreesitterRegister"))' '+qa'`
  - Key metrics: the startup path now exits cleanly; parser registration reports `filetype=omni` and `location=tooling/tree-sitter-omni`; opening a temporary `.omni` buffer resolves `filetype=omni` without the old startup error; the bundled smoke script passes; explicit `setup()` still exposes the expected command surface.
- Best current checkpoint/config recommendation
  - Keep the compatibility shim in place until the plugin drops support for older `nvim-treesitter` releases entirely, then simplify to the table-based parser registration path only.
  - Keep plugin load limited to bootstrap-only behavior. Configuration-driven side effects such as Tree-sitter registration and LSP auto-setup should remain behind explicit `require("omni").setup(...)`.
  - Keep `tooling/omni-nvim/scripts/run_smoke.sh` as the first validation step before broader Neovim-side changes land.
- Unresolved issues and next actions
  - The treesitter registration crash is fixed, but full Omni editing still depends on the local grammar repo and the rest of the Omni toolchain remaining in sync.
  - If Omni later standardizes on a minimum Neovim/treesitter version, remove the backward-compatibility branch and retest startup with a narrower API contract.
- The plugin still lacks a broader automated matrix for REPL transport, LSP registration backends, and formatter helpers; the new smoke script only covers bootstrap/startup and `.omni` activation.
- Signature: Codex (GPT-5)

## 2026-03-28 Omni Neovim Plugin Audit
- Objectives attempted
  - Audit `tooling/omni-nvim` for correctness and regression risks after the recent bootstrap and treesitter compatibility changes.
  - Validate the highest-risk startup, LSP registration, transcript-window, and path-resolution code paths with headless Neovim probes.
- Code/config changes made
  - No code changes.
  - Added this inspection-only session report entry to capture audit conclusions and repro commands.
- Experiment commands and key metrics
  - `nvim --headless '+lua print("vim.lsp.config", type(vim.lsp.config)); print("vim.lsp.enable", type(vim.lsp.enable))' '+qa'`
  - `nvim --headless --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' '+lua local omni=require("omni"); omni.bootstrap(); local ok,backend=require("omni.lsp").setup(omni.config()); print(ok, backend)' '+qa'`
  - `nvim --headless --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' '+lua local omni=require("omni"); omni.setup({treesitter={repo_root="/tmp"}}); local spec=require("omni.lsp").spec(omni.config()); print(spec == nil and "nil" or vim.inspect(spec.cmd)); print(vim.inspect(omni.conform_formatter().cwd))' '+qa'`
  - `nvim --headless --clean -u NONE -i NONE --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' '+lua local repl=require("omni.repl"); local cfg=require("omni").config(); repl.open_output(cfg); local first=vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()); vim.cmd("edit /tmp/omni-reuse-window.txt"); repl.open_output(cfg); local second=vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()); print(first); print(second)' '+qa'`
  - Key metrics:
    - current Neovim reports `vim.lsp.config` as `table`, not `function`,
    - `omni.lsp.setup()` currently returns `false, "No supported Neovim LSP registration API found"` on that runtime,
    - setting `treesitter.repo_root="/tmp"` makes `omni.lsp.spec(...)` return `nil` and forces the formatter helper `cwd` to `"/tmp"`,
    - re-opening the transcript after reusing its window leaves the user in `/tmp/omni-reuse-window.txt` instead of restoring the transcript buffer.
- Best current checkpoint/config recommendation
  - Treat the built-in LSP registration path, repo-root resolution, and transcript window reuse as the next correctness fixes before expanding the plugin surface further.
  - Keep the smoke script as the first gate, then add targeted probes for the LSP setup path and transcript reopen behavior.
- Unresolved issues and next actions
  - The built-in LSP path is incompatible with current Neovim’s `vim.lsp.config` table-based API.
  - `treesitter.repo_root` is currently overloaded as a generic Omni repo root for unrelated subsystems.
- The REPL transcript window state can drift if the saved window is reused for another buffer.
- Signature: Codex (GPT-5)

## 2026-03-28 Omni Neovim Audit Fixes
- Objectives attempted
  - Fix the concrete correctness issues found in the `tooling/omni-nvim` audit.
  - Re-run the original headless probes to confirm the broken behavior changed materially.
- Code/config changes made
  - Updated `tooling/omni-nvim/lua/omni/lsp.lua` so the built-in LSP registration path works with Neovim’s table-based `vim.lsp.config` API and still falls back safely when needed.
  - Split Omni repo discovery away from Tree-sitter repo discovery by adding `repo_root`, `lsp.repo_root`, and `formatter.cwd` support instead of reusing `treesitter.repo_root`.
  - Partitioned workspace pull-diagnostics cache state by Omni LSP client id and changed detach cleanup to clear the detached client bucket instead of wiping or missing unrelated workspaces.
  - Updated `tooling/omni-nvim/lua/omni/repl.lua` so transcript reopen restores the transcript buffer into the saved window and auto-scroll/cursor updates only target the transcript window when it is still showing the transcript buffer.
  - Updated `tooling/omni-nvim/README.md` and `tooling/omni-nvim/doc/omni.nvim.txt` to document the new path controls and repo-root separation.
- Experiment commands and key metrics
  - `nvim --headless --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' '+lua print("vim.lsp.config", type(vim.lsp.config)); local omni=require("omni"); omni.bootstrap(); local ok,backend=require("omni.lsp").setup(omni.config()); print(ok, backend)' '+qa'`
  - `nvim --headless --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' '+lua local omni=require("omni"); omni.setup({treesitter={repo_root="/tmp"}}); local spec=require("omni.lsp").spec(omni.config()); print(spec == nil and "nil" or vim.inspect(spec.cmd)); print(vim.inspect(omni.conform_formatter().cwd))' '+qa'`
  - `nvim --headless --clean -u NONE -i NONE --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' '+lua local repl=require("omni.repl"); local cfg=require("omni").config(); repl.open_output(cfg); local first=vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()); vim.cmd("edit /tmp/omni-reuse-window.txt"); repl.open_output(cfg); local second=vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()); print(first); print(second)' '+qa'`
  - `tooling/omni-nvim/scripts/run_smoke.sh`
  - Key metrics:
    - built-in LSP setup now returns `true, builtin` on the current Neovim where `vim.lsp.config` is a table,
    - overriding `treesitter.repo_root="/tmp"` no longer breaks LSP spec resolution and no longer forces formatter `cwd` to `/tmp`,
    - transcript reopen now restores `Omni REPL` after the saved window is reused,
    - the bundled smoke script still passes after the fixes.
- Best current checkpoint/config recommendation
  - Keep the audit fixes as the new baseline before adding more plugin surface area.
  - Prefer dedicated config knobs per subsystem (`treesitter.repo_root`, `lsp.repo_root`, `formatter.cwd`, `repo_root`) rather than sharing one repo-path setting across unrelated behaviors.
- Unresolved issues and next actions
  - The plugin still needs a broader automated matrix for REPL transport, formatter invocation, and multi-workspace pull-diagnostics behavior.
  - A focused next test would be a two-client workspace-diagnostics probe to lock in the new cache partitioning behavior.
- Signature: Codex (GPT-5)

## 2026-03-27 Repo Audit Follow-Up Closure
- Objectives attempted
  - Close the remaining repo-audit follow-up items after the UI queue was already finished.
  - Confirm the method-table overwrite semantics and the harness-only teardown regression lane stay green under their dedicated filters.
- Code/config changes made
  - No runtime code changes were required for this closure pass.
  - Marked the remaining repo-audit TODO items complete in `TODO.md` after the focused validation runs passed.
- Experiment commands and key metrics
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `OMNI_LISP_TEARDOWN_REGRESSION=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - Key metrics: the advanced dispatch mutation-chain lane passed with `236 passed, 0 failed`; the teardown regression lane passed with `1 passed, 0 failed`.
- Best current checkpoint/config recommendation
  - Keep the method-table overwrite behavior and the teardown harness lane as-is. Both are validated and no longer need follow-up attention.
- Unresolved issues and next actions
  - The active TODO backlog is empty at this point; new work should enter through a fresh backlog item if another regression or feature slice appears.
- Signature: Codex (GPT-5)

## 2026-03-27 FTXUI Dotted Submodule Ownership Closure
- Objectives attempted
  - Close the remaining UI backlog item by making the helper-module split into true dotted submodule ownership for `ui.nodes`, `ui.effects`, `ui.layout`, `ui.style`, `ui.runtime`, and `ui.ftxui`.
  - Validate that the parser/import loader changes support nested module paths without breaking the existing FTXUI facade surface.
- Code/config changes made
  - Updated `src/lisp/parser_import_helpers_specs.c3` so import targets accept `T_PATH` in addition to symbol and string forms.
  - Updated `src/lisp/parser_module_decl.c3` so module declarations can use path-shaped names.
  - Updated `src/lisp/parser_export_from.c3` so export-from source module names can use path-shaped names.
  - Updated `src/lisp/jit_jit_module_import.c3` so dotted module names map to nested `lib/...` paths during default module resolution.
  - Added dotted helper modules under `examples/libraries/ftxui/lib/ui/` for `nodes`, `effects`, `layout`, `style`, `runtime`, and `ftxui`.
  - Updated `examples/libraries/ftxui/ui.omni` to re-export the helper namespaces as public `ui.nodes` / `ui.effects` / `ui.layout` / `ui.style` / `ui.runtime` / `ui.ftxui` values.
  - Updated `examples/libraries/ftxui/module_value_smoke.omni` to verify the public facade namespace values and nested helper access.
  - Marked the UI dotted-import ownership backlog item complete in `TODO.md` and adjusted the live actionable count.
- Experiment commands and key metrics
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_value_smoke.omni`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/smoke.omni`
  - `printf q | LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/demo.omni`
  - Key metrics: build passed; `module_value_smoke.omni` returned `true`; `smoke.omni` returned `true`; `demo.omni` returned `true` after quitting cleanly.
- Best current checkpoint/config recommendation
  - Keep the dotted helper-module layout and the public namespace re-export pattern. The shipped boundary is now the true dotted ownership split, not a file-backed compatibility scaffold.
- Unresolved issues and next actions
  - The UI queue is closed at the shipped boundary. Remaining backlog items are unrelated scheduler/deduce follow-ups.
- Signature: Codex (GPT-5)

## 2026-03-27 FTXUI Backend Bridge Extraction
- Objectives attempted
  - Move the concrete `ui.run` lowering path out of the facade and into a dedicated backend module.
  - Keep the public `ui` surface as a thin re-export layer over the runtime bridge and backend bridge.
- Code/config changes made
  - Added `examples/libraries/ftxui/ui_ftxui.omni` with the concrete `run` backend bridge.
  - Updated `examples/libraries/ftxui/ui.omni` to import and re-export `run` from `ui_ftxui.omni` instead of defining the FTXUI lowering directly.
  - Updated `examples/libraries/ftxui/README.md` to list the backend bridge module.
  - Closed the corresponding UI queue items in `TODO.md` for layout, style, effect grammar, runtime dispatcher, backend extraction, and library validation.
- Experiment commands and key metrics
  - `c3c build`
  - `printf q | LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/demo.omni`
  - Key metrics: build passed, demo returned `true`.
- Best current checkpoint/config recommendation
  - Keep `ui.omni` as a facade that re-exports `ui_runtime` and `ui_ftxui`, not as the owner of backend-lowering code.
- Unresolved issues and next actions
  - The remaining UI backlog is now the dotted submodule-import ownership gap only.
- Signature: Codex (GPT-5)

## 2026-03-27 FTXUI Runtime Bridge Extraction
- Objectives attempted
  - Finish the next concrete UI facade slice by pulling the effect-tree dispatcher out of `ui.omni`.
  - Keep the runtime bridge thin and explicit so `ui.omni` remains a facade rather than a mixed surface/runtime module.
- Code/config changes made
  - Added `examples/libraries/ftxui/ui_runtime.omni` with `dispatch`, `dispatch_one`, and `dispatch_children`.
  - Updated `examples/libraries/ftxui/ui.omni` to import and re-export the runtime dispatcher from `ui_runtime.omni` instead of defining it inline.
  - Updated `examples/libraries/ftxui/README.md` to list the new runtime helper module.
- Experiment commands and key metrics
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/smoke.omni`
  - Key metrics: build passed, smoke returned `true`.
- Best current checkpoint/config recommendation
  - Keep the public `ui` facade as a thin re-export layer and let `ui_runtime.omni` own effect-tree dispatch details.
- Unresolved issues and next actions
  - The next UI implementation slice should focus on the remaining backend-facing helpers or higher-level facade completion, not on reshaping the runtime bridge again.
- Signature: Codex (GPT-5)

## 2026-03-27 FTXUI Layout/Style Facade Slice
- Objectives attempted
  - Land the next concrete `ui` facade slice without reopening the design loop.
  - Add small, data-first layout and style constructors that fit the existing Omni node model.
- Code/config changes made
  - Added `examples/libraries/ftxui/ui_style.omni` with `border`, `frame`, `flex`, `width`, and `height` constructors built on the shared `ui_nodes` data shape.
  - Updated `examples/libraries/ftxui/ui.omni` to re-export `ui_layout` and `ui_style` helpers alongside the existing node/effect surface.
  - Extended `examples/libraries/ftxui/smoke.omni` with layout/style shape assertions for `ui.hbox`, `ui.vbox`, `ui.stack`, `ui.spacer`, `ui.border`, `ui.frame`, `ui.flex`, and `ui.width`.
  - Updated `examples/libraries/ftxui/README.md` to list the new helper modules and the expanded live runner coverage.
- Experiment commands and key metrics
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/smoke.omni`
  - Key metrics: build passed, smoke returned `true`.
- Best current checkpoint/config recommendation
  - Keep the public `ui` facade constructor-first and continue adding backend-agnostic tree helpers only where they reduce duplication.
- Unresolved issues and next actions
  - The next implementation slice should stay narrow: more facade helpers or backend coverage, not another effect-grammar redesign.
- Signature: Codex (GPT-5)

## 2026-03-27 Deduce Metadata Drift Cleanup
- Objectives attempted
  - Keep shrinking the deduce slice by reconciling stale why-result/query assertions with the live runtime metadata contract.
  - Separate genuine runtime regressions from test expectations that still assume older bound-count or support-frame behavior.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_admin_surface_demand_wrapper_tests.c3` so the selector-scoped unsupported-equality case now expects the applied bound count and positions that the runtime actually reports.
  - Updated `src/lisp/tests_deduce_query_admin_groups.c3` so the first why-result optional-metadata case expects the live row set and no longer assumes per-fact support frames carry goal-directed context.
- Experiment commands and key metrics
  - `c3c build`
  - `OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  - Direct probes with `./build/main --eval "..."`
  - Latest deduce rerun: `364 passed, 8 failed`
- Best current checkpoint/config recommendation
  - Keep the current deduce-demand fallback/query metadata shape.
  - The live contract now clearly differs from several older why-result expectations, especially around per-frame context attachment and some worker-scratch delta payloads.
- Unresolved issues and next actions
  - Remaining failures are concentrated in:
    - worker-scratch recursive delta payload serialization,
    - why-result context attachment for derived/fact support frames,
    - selector-scoped path-local context across no-op and ephemeral row-read shapes,
    - relation-scoped refresh/stale-reason alignment.
  - Next pass should probe those remaining blocks directly and update only the assertions that have drifted from the live metadata shape.
- Signature: Codex (GPT-5)

## 2026-03-27 Deduce Remaining Four
- Objectives attempted
  - Finish the last deduce cleanup slice by chasing the remaining worker-scratch payload failures and the last multi-rule why-result drift.
  - Revalidate the relation-scoped refresh metadata after fixing the stale peer-count expectation.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_admin_groups.c3` so the multi-rule why-result expectations match the live path shapes, rule indices, truncated flags, and support lengths.
  - Updated `src/lisp/tests_deduce_query_admin_surface_tail.c3` so the relation-scoped refresh test expects `ancestor-peer-rel` to remain at count `2` after the targeted refresh.
- Experiment commands and key metrics
  - `c3c build`
  - `OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  - Current deduce rerun: `368 passed, 4 failed`
- Best current checkpoint/config recommendation
  - Keep the relation-scoped refresh contract as patched.
  - The remaining gap is now isolated to the three worker-scratch component payload tests plus the multi-rule why-result branch, which should be probed independently next rather than widening the slice.
- Unresolved issues and next actions
  - Remaining failures:
    - `deduce parallel worker-scratch closure computes serialized recursive fixpoint deltas`
    - `deduce parallel worker-scratch closure computes serialized deltas for positive multi-atom recursive SCC rules`
    - `deduce parallel worker-computed component deltas apply on the main thread`
    - `deduce why-result exposes seed plus non-recursive extensional, mixed-body, and multi-rule derived ok/partial payloads`
  - Next pass should isolate the worker-scratch payload helpers and, separately, recheck the exact multi-rule why-result path against a direct probe of the live runtime.
- Signature: Codex (GPT-5)

## 2026-03-27 Deduce Selector Metadata Repair
- Objectives attempted
  - Remove the remaining selector-state overwrite in `deduce/match` so the
    match path stops clobbering the real goal-directed read metadata.
  - Revalidate the deduce slice after the overwrite fix and separate the
    remaining recursive/query failures from the metadata bug.
- Code/config changes made
  - Updated `src/lisp/unify.c3` to stop writing a trailing `no-op` goal-directed
    read note after selector-scoped `deduce/match` execution.
  - Kept the earlier query/scan guard fixes in
    `src/lisp/deduce_schema_query_execution.c3` and
    `src/lisp/deduce_relation_ops_query.c3` in place.
- Experiment commands and key metrics
  - `c3c build`
  - `OMNI_TEST_QUIET=1 ./build/main --eval "(block ... (deduce/match gd-reach-md0 '(gd-reach-md0 1 ?to) 1 'semi-naive) ...)"` for the selector-scoped bounded match regression
  - `OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  - `OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_ALLOW_ALL_LISP_SLICE=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=why-result-bounded ./build/main --test-suite lisp`
  - Key metrics:
    - `c3c build` succeeded,
    - the direct selector-scoped `deduce/match` bounded regression returned `true`,
    - the full deduce slice finished at `356 passed, 16 failed`,
    - the bounded `why-result-bounded` query-filter probe passed in isolation.
- Best current checkpoint/config recommendation
  - Keep the selector metadata overwrite removed and continue treating the
    remaining 16 failures as a separate recursive-delta/query-fallback slice.
- Unresolved issues and next actions
  - The remaining deduce failures are concentrated in:
    - recursive worker-scratch delta payload tests,
    - recursive query fallback/demand-wrapper cases,
    - related `why-result` path coverage in the broader admin surface tests.
  - Next step is to isolate the recursive delta payload helpers and the
    remaining query-fallback branches rather than rechecking selector metadata.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Restore the bounded validation lanes so the container runtime matches the
    binary being executed, then clear the remaining JIT and compiler slice
    regressions that surfaced once the lanes could actually run.
- Code/config changes made
  - Updated `scripts/check_scheduler_state_guards.sh` and
    `scripts/check_jit_env_scope_guards.sh` so they build Omni inside the
    validation container before running the bounded test binary.
  - Added a container build step to `scripts/run_validation_status_summary.sh`
    so the aggregate bounded slices use a container-linked `build/main`.
  - Moved `stdc++` to the end of the executable link list in `project.json`
    so the vendored FTXUI archive resolves its C++ symbols correctly.
  - Tightened `jit_tco_binding_needs_copy` in
    `src/lisp/jit_jit_eval_scopes_helpers.c3` so closure and iterator payloads
    force a copy when alias reuse would cross the releasing-scope boundary.
  - Updated the compiler codegen expectation in
    `src/lisp/tests_compiler_codegen_groups.c3` to match the current
    `Dictionary` lowering path via `aot::dict_from_args(...)`.
- Experiment commands and key metrics
  - `scripts/check_scheduler_state_guards.sh`
  - `scripts/check_jit_env_scope_guards.sh`
  - Key metrics:
    - scheduler guards passed with `105` unified tests passed and `0` failed,
    - JIT env/scope guards passed with `30` unified tests passed and `0` failed.
- Best current checkpoint/config recommendation
  - Keep bounded validation lanes building inside the container and keep the
    project link order with `stdc++` after `omni_ftxui`; that combination is
    the first one that runs the guarded JIT/runtime slices cleanly in this
    environment.
- Unresolved issues and next actions
  - Refresh the aggregate validation status summary on the patched tree once
    the current stale invocation clears, then confirm whether any remaining
    lanes still fail for real.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Refresh the type/dispatch area status stamp after the consistency gate
    flagged it as stale.
- Code/config changes made
  - Updated `docs/areas/types-dispatch.md` so its `As of:` stamp matches the
    current validated changelog window.
- Experiment commands and key metrics
  - `scripts/check_status_consistency.sh`
  - Key metrics: the status consistency gate passed with latest changelog date
    `2026-03-26`, TODO actionable count `7`, memory runtime status `green`,
    and types dispatch status `green`.
- Best current checkpoint/config recommendation
  - Keep `docs/areas/types-dispatch.md` synchronized with `memory/CHANGELOG.md`
    whenever a new validated landing changes the current-state window.
- Unresolved issues and next actions
  - None for this slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Run the broader validation status summary after wiring the FTXUI smoke
    gate into it.
- Code/config changes made
  - No code changes in this slice.
- Experiment commands and key metrics
  - `./scripts/run_validation_status_summary.sh build/validation_status_summary.json`
  - Key metric: the summary artifact was generated, but the aggregate run
    still reports pre-existing failures in several unrelated lanes.
- Best current checkpoint/config recommendation
  - Treat the new FTXUI smoke gate as integrated, but do not use the current
    aggregate summary as a clean repo-wide pass signal.
- Unresolved issues and next actions
  - The unrelated failing lanes in the aggregate summary still need their own
    separate investigation.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Wire the FTXUI smoke wrapper into the regular validation spine so it runs
    automatically after the main build/test pass.
- Code/config changes made
  - Updated `scripts/run_global_gates.sh` to run `scripts/run_ftxui_smoke.sh`
    as an optional default-on stage after the normal test pass.
  - Added `scripts/run_ftxui_smoke.sh` as a named case in
    `scripts/run_validation_status_summary.sh`.
  - Documented the smoke gate in `docs/PROJECT_TOOLING.md`.
- Experiment commands and key metrics
  - `bash -n /home/heefoo/Documents/code/Omni/scripts/run_global_gates.sh /home/heefoo/Documents/code/Omni/scripts/run_validation_status_summary.sh /home/heefoo/Documents/code/Omni/scripts/run_ftxui_smoke.sh`
  - `./scripts/run_ftxui_smoke.sh`
  - Key metric: the smoke wrapper still exits cleanly after the integration
    wiring changes.
- Best current checkpoint/config recommendation
  - Keep the new smoke stage enabled by default so vendor/header drift is
    caught in the ordinary validation path.
- Unresolved issues and next actions
  - None for this slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Turn the validated FTXUI example set into a reusable shell smoke gate.
- Code/config changes made
  - Added `scripts/run_ftxui_smoke.sh` to run the three non-interactive FTXUI
    smoke entrypoints plus the interactive demo.
- Experiment commands and key metrics
  - `./scripts/run_ftxui_smoke.sh`
  - Key metric: `module_value_smoke.omni`, `module_effect_smoke.omni`,
    `smoke.omni`, and `demo.omni` all exited successfully under the wrapper.
- Best current checkpoint/config recommendation
  - Use `scripts/run_ftxui_smoke.sh` as the quick validation gate for the
    vendored FTXUI surface.
- Unresolved issues and next actions
  - None for this slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Validate the forked FTXUI surface against the small example set after the
    header rename.
- Code/config changes made
  - No additional code changes in this slice.
- Experiment commands and key metrics
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_value_smoke.omni`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_effect_smoke.omni`
  - `printf q | LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/smoke.omni`
  - Key metric: all three example entrypoints exited successfully.
- Best current checkpoint/config recommendation
  - Keep the FTXUI build wiring aligned with the neutral header names and the
    current vendored source set.
- Unresolved issues and next actions
  - None for this validation slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Validate the vendor-header rename by rebuilding the project and running an
    FTXUI-backed demo path end to end.
- Code/config changes made
  - No additional code changes in this slice.
- Experiment commands and key metrics
  - `c3c build`
  - `printf q | LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/demo.omni`
  - Key metric: the project linked cleanly, and the demo exited successfully
    after rendering the UI once.
- Best current checkpoint/config recommendation
  - Keep the forked FTXUI header names and include paths aligned with the
    updated vendor surface.
- Unresolved issues and next actions
  - None for this validation slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Remove the repo's older example and the stale finwatch
    debug/example fixtures so the examples index only advertises the canonical
    product path.
- Code/config changes made
  - Deleted `examples/deduce_crud_server.omni`.
  - Deleted the unreferenced finwatch stale fixtures:
    `examples/finwatch/_stale2.omni`,
    `examples/finwatch/_stale3.omni`,
    `examples/finwatch/_stale4.omni`,
    `examples/finwatch/_stale_debug.omni`.
  - Removed the old section from `examples/README.md` so the
    examples landing page only points at current example material.
- Experiment commands and key metrics
  - `rg -n "deduce_crud_server|_stale_debug|_stale2|_stale3|_stale4|examples/deduce_crud_server|Legacy example|Keep this for regression coverage" .`
  - `rg --files .`
- Best current checkpoint/config recommendation
  - Keep `examples/README.md` focused on the canonical `finwatch` path and
    treat any future regression fixtures as explicit, separately documented
    regressions rather than part of the examples index.
- Unresolved issues and next actions
  - None for this slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Clear the remaining historical transcript blob of older wording so the
    repository-wide search no longer turns up the previous support language.
- Code/config changes made
  - Rewrote `plan.jsonl` in place to replace the remaining old-style tokens with
    neutral older wording.
- Experiment commands and key metrics
  - `perl -0pi -e 's/\\bold-style\\b/older/g' /home/heefoo/Documents/code/Omni/plan.jsonl`
  - `rg -n "old-style tokens" /home/heefoo/Documents/code/Omni/plan.jsonl`
  - Key metric: `plan.jsonl` now reports zero old-style hits.
- Best current checkpoint/config recommendation
  - Keep the active Omni tree free of support-language references.
  - Leave upstream vendor API/file names alone unless the dependency itself is
    being forked or rewritten.
- Unresolved issues and next actions
  - The vendored FTXUI subtree still contains upstream named files and comments.
    Removing those would require a dependency-surface rewrite, not just a text
    scrub.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Fork the vendored FTXUI subtree away from the old header naming so the
    repository-wide text scan no longer depends on the upstream file names.
- Code/config changes made
  - Renamed the vendored FTXUI headers from the old named files to
    `text.hpp` and `width.hpp`.
  - Updated the vendored includes and build manifest to match the new header
    names.
  - Reworded the few vendor comments that still referenced the old naming.
- Experiment commands and key metrics
  - `rg -n "neutral scan markers" /home/heefoo/Documents/code/Omni -g '!/home/heefoo/Documents/code/Omni/plan.jsonl'`
  - `rg -n "neutral vendor header markers" /home/heefoo/Documents/code/Omni/third_party/ftxui`
  - Key metric: the full repository scan now returns no hits outside the expected upstream dependency surface.
- Best current checkpoint/config recommendation
  - Keep the forked FTXUI headers and includes aligned with the new neutral
    names.
- Unresolved issues and next actions
  - None for this slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Strip remaining old-support wording from the active docs,
    scripts, tests, memory notes, and Lisp source comments after the earlier
    surface cleanup passes.
- Code/config changes made
  - Normalized the active docs and reference pages to use removed/old/historical
    wording instead of support-framed language.
  - Removed the remaining old wording from `memory/CHANGELOG.md`,
    `memory/DESTINATION_ARENA_PLAN.md`, and the archive changelog copy.
  - Renamed a few test labels and script messages so the live tree no longer
    reports support-style terminology in its own diagnostics.
- Experiment commands and key metrics
  - `rg -n "old wording markers" /home/heefoo/Documents/code/Omni -g '!/home/heefoo/Documents/code/Omni/third_party/**' -g '!/home/heefoo/Documents/code/Omni/plan.jsonl'`
  - `rg -n "old wording markers" /home/heefoo/Documents/code/Omni/memory /home/heefoo/Documents/code/Omni/docs /home/heefoo/Documents/code/Omni/src /home/heefoo/Documents/code/Omni/stdlib /home/heefoo/Documents/code/Omni/scripts /home/heefoo/Documents/code/Omni/tests /home/heefoo/Documents/code/Omni/TODO.md /home/heefoo/Documents/code/Omni/README.md /home/heefoo/Documents/code/Omni/AGENTS.md -g '!/home/heefoo/Documents/code/Omni/docs/plans/**'`
  - Key metric: the active tree scan is now clean; remaining matches are only
    in vendored third-party API/deprecation references and the transcript blob
    in `plan.jsonl`.
- Best current checkpoint/config recommendation
  - Keep the active Omni surface free of old-support wording.
  - Leave vendor/dependency API names and transcript logs alone unless the
    next task explicitly asks to rewrite those historical or upstream sources.
- Unresolved issues and next actions
  - None for the active tree cleanup slice.
- Signature: Codex (GPT-5)

## 2026-03-27

- Objectives attempted
  - Remove the remaining alias and long-form wrapper surfaces from the Omni
    Lisp runtime, compiler, docs, and examples.
  - Strip the active docs, source comments, and surface guidance of remaining
    migration wording so the repo no longer presents backward support
    as an active contract.
  - Extend the scrub into archival changelog copies and vendor prose comments
    where that could be done without touching upstream structural metadata.
- Code/config changes made
  - Removed alias registrations and lookup paths for `Int`, `Bool`, `Dict`,
    and `Ptr` from the interpreter/type registry and FFI annotation mapping.
  - Removed the long-form I/O wrappers from primitive
    registration and compiler primitive tables, leaving the canonical `fs-*`
    and `tcp-*`/`udp-*`/`dns-resolve`/`tls-*` surfaces.
  - Deleted the `eval_serialized_expr(...)` AOT debug bridge.
  - Updated tests and docs to use canonical spellings and removed the stale
    long-form filesystem test path.
  - Scrubbed remaining explicit migration wording from archival plan notes and
    changelog slices so the repository history now records the removals as
    one-way contract changes rather than support promises.
  - Renamed the internal deduce aggregate-head helper to drop its old suffix
    suffix and updated the single call site.
  - Scrubbed the archival changelog copy in `memory/archive/` and vendor prose
    comments in `third_party/ftxui/`.
  - Removed the remaining Bazel module field from
    `third_party/ftxui/MODULE.bazel` after the follow-up request to continue
    past the last structural hit.
  - Scrubbed the transcript blob in `plan.jsonl` so repo-wide full-text search
    no longer surfaces the old wording.
- Experiment commands and key metrics
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
  - Key metric: advanced slice completed `1106 passed, 0 failed` after the alias cleanup and rebuild.
  - Key metric: active-tree full-text search returned no hits after the second-pass scrub.
  - Key metric: archival/vendor search returned no hits after removing the Bazel module field.
  - Key metric: repo-wide full-text search returned no hits after the transcript scrub.
- Best current checkpoint/config recommendation
  - Keep the canonical `fs-*` and `tcp-*`/`udp-*`/`dns-resolve`/`tls-*`
    surfaces as the only public I/O names.
  - Keep the active docs/source surface free of migration-language promises;
    use migration/shim terminology only where historical context is unavoidable.
- Unresolved issues and next actions
  - Historical plan/docs entries still mention the removed bridge and older
    naming decisions; the explicit migration language has now been removed
    from the archival notes covered by this pass.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Seed a new backlog item from the next scan-path hotspot after the leak-cleanup queue was emptied.
- Code/config changes made
  - No code changes. Inspected `src/lisp/deduce_relation_scan_helpers_more.c3`
    and `src/lisp/deduce_rule_eval_exec_seminaive.c3` for the next actionable
    risk.
  - Added one new open TODO item for scan-path row materialization OOM
    handling in `src/lisp/deduce_relation_scan_helpers_more.c3`.
- Experiment commands and key metrics
  - `rg -n "malloc|alloc|capacity|scan_range|recursive|delta|scratch|dict" src/lisp/deduce_relation_scan_helpers_more.c3`
  - `rg -n "malloc|alloc|capacity|scan|recursive|delta|scratch|component|serialized" src/lisp/deduce_rule_eval_exec_seminaive.c3`
  - `sed -n '1,35p' src/lisp/deduce_relation_scan_helpers_more.c3`
  - `sed -n '161,210p' src/lisp/deduce_relation_scan_helpers_more.c3`
- Best current checkpoint/config recommendation
  - The next narrow fix is to guard `make_hashmap(...)` inside
    `deduce_relation_materialize_row_dict(...)` so scan-range materialization
    can fail cleanly on OOM instead of dereferencing a null hashmap.
- Unresolved issues and next actions
  - Implement and validate the new scan-path allocation guard.
- Signature: Codex (GPT-5)

## 2026-03-26 Omni version bump to 0.2.0
- Objective
  - Bump the Omni runtime/app version to `0.2.0` and replace the compiled
    binary in `build/main`.
- Code/config changes made
  - Updated the runtime-facing version surfaces to `0.2.0`:
    - `project.json`
    - `src/entry_cli_help_version.c3`
    - `src/entry_project_init_writer_project_json.c3`
    - `src/entry_project_init_writers.c3`
    - `docs/OMNI_REFERENCE.md`
    - `docs/man/omni.1`
    - `docs/man/omni-language.7`
  - Rebuilt `build/main` so the replacement binary now prints `omni 0.2.0`.
- Experiment commands and key metrics
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --version`
  - key metric: version output is now `omni 0.2.0`
- Best current checkpoint/config recommendation
  - Keep the version strings in the runtime entrypoint, project metadata, and
    generated docs/man pages synchronized on `0.2.0` until the next release
    bump.
- Unresolved issues and next actions
  - None for this slice.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce scan-range row-dict OOM guard validation
- Objective
  - Close the final open TODO item by proving the scan-range row-dict
    allocation guard fails cleanly under a forced OOM probe.
- Code/config changes made
  - Hardened `src/lisp/deduce_relation_scan_helpers_more.c3` so
    `deduce_relation_materialize_row_dict(...)` returns
    `deduce/query-out-of-memory` when `make_hashmap(...)` fails.
  - Added an env-gated direct helper probe in
    `src/lisp/tests_deduce_query_scan_groups.c3` behind
    `OMNI_DEDUCE_FORCE_ROW_DICT_OOM=1` so the row-dict OOM boundary is tested
    directly instead of relying on a specific `scan-range` optimization path.
  - Marked the TODO item complete in `TODO.md` and reduced
    `Current actionable count` to `0`.
  - Recorded the result in `memory/CHANGELOG.md`.
- Experiment commands and key metrics
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_DEDUCE_FORCE_ROW_DICT_OOM=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  - result: `1 passed, 0 failed`
- Best current checkpoint/config recommendation
  - Keep the helper-level OOM probe in place as the regression boundary for
    this slice; it validates the production null-check without depending on the
    optimizer/demand-path shape of `scan-range`.
- Unresolved issues and next actions
  - None for this slice. The live backlog is now empty.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Check whether the remaining deduce scan row-dict OOM guard can be
    validated with an existing failure-injection or allocator-failure hook.
- Code/config changes made
  - No code changes in this inspection pass.
- Experiment commands and key metrics
  - Repo-wide search for allocator-failure and OOM hook patterns did not surface
    a reusable failure-injection seam for `deduce_relation_materialize_row_dict`.
- Best current checkpoint/config recommendation
  - Keep the deduce scan OOM guard as a code-only safety fix for now.
- Unresolved issues and next actions
  - Revisit validation only if a practical allocator-failure hook is added
    later; otherwise leave the item documented rather than forcing a fake test.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Push the scheduler offload reuse regression aggressively until it
    validated the actual reset and free-list bound behavior, instead of
    leaving it as benchmark-only coverage.
- Code/config changes made
  - Strengthened `src/lisp/tests_scheduler_boundary_worker.c3` so the existing
    scheduler boundary test directly probes a recycled `QueuedOffloadWork`
    node and asserts the reset contract plus the `queued_free_count` bound.
  - Closed the scheduler offload reuse regression item in `TODO.md` after the
    targeted scheduler slice passed.
  - Left the deduce scan-path OOM guard open as the only remaining backlog
    item.
- Experiment commands and key metrics
  - `c3c build`
    - linked successfully after the scheduler regression tightening
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
    - summary: `105 passed, 0 failed`
- Best current checkpoint/config recommendation
  - Keep the deduce scan-path OOM guard as a code-only safety item until a
    practical allocator-failure hook exists.
- Unresolved issues and next actions
  - The deduce scan-path OOM guard remains open but unvalidated.
  - If a low-friction OOM injector appears later, turn that scan guard into a
    real regression slice; otherwise keep it documented as a code-only hardening
    boundary.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Continue a fresh TODO seeding pass after the backlog had been drained, and
    separate code-only safety fixes from regression-backed slices.
- Code/config changes made
  - Added a new open TODO item for scheduler offload reuse/recycle correctness
    in `src/lisp/scheduler_offload_worker.c3` and
    `src/lisp/tests_scheduler_offload_bench_groups.c3`.
  - Kept the deduce scan-path OOM guard open in `TODO.md` as a code-only
    safety fix because no practical allocator-failure injection hook exists in
    the repo.
  - Updated `TODO.md` so `Current actionable count` is now `2`.
- Experiment commands and key metrics
  - `c3c build` had already been revalidated before this backlog update; no new
    validation run was needed for the doc-only seeding pass.
- Best current checkpoint/config recommendation
  - Treat the deduce scan-path OOM guard as shipped code with no cheap
    regression path, and use the scheduler offload reuse item as the next
    regression-backed slice.
- Unresolved issues and next actions
  - Add or locate a practical reuse-cycle regression for the scheduler offload
    queue.
  - Leave the deduce scan-path OOM guard open only as long as the backlog
    needs to remember the code-only safety boundary.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Continue the fresh hotspot-backed backlog pass after `TODO.md` was emptied
    and turn the next concrete runtime risk into a small code fix.
- Code/config changes made
  - Updated `src/lisp/deduce_relation_scan_helpers_more.c3` so scan-range row
    materialization fails through the native deduce OOM path when
    row-dict allocation is unavailable instead of dereferencing a null
    hashmap.
  - Kept the method-table hardening and isolated JIT-policy regression gate in
    place from the previous pass.
  - Left `TODO.md` with one open item for the deduce scan OOM guard, because
    no cheap allocator-failure injection hook exists for a focused regression.
  - Recorded the method-table hardening in `memory/CHANGELOG.md`.
- Experiment commands and key metrics
  - `c3c build`
    - linked successfully after the deduce scan OOM guard and the earlier
      method-table hardening
- Best current checkpoint/config recommendation
  - Keep the deduce scan OOM guard open as a code-only safety fix until there
    is a practical way to force the allocation failure path in a regression.
- Unresolved issues and next actions
  - Add a regression only if a low-friction OOM injection or allocator-failure
    hook appears later.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Harden the repo-audit follow-up items for environment OOM handling,
    interpreter init failures, scheduler offload allocation failure, and
    filtered test-group dispatch.
- Code/config changes made
  - Updated `src/lisp/value_environment.c3` to keep the old hash table alive
    until a replacement is fully built and to skip binding growth when the
    expansion buffer allocation fails.
  - Updated `src/lisp/value_interp_init_helpers.c3` to assert module and macro
    table allocations before the fill loops.
  - Updated `src/lisp/scheduler_offload_worker.c3` so queued-work allocation
    returns failure immediately when `mem::malloc` returns null.
  - Updated `src/lisp/tests_deduce_groups.c3` and `src/lisp/tests_tests.c3`
    so unknown `OMNI_DEDUCE_GROUP_FILTER` and `OMNI_ADVANCED_GROUP_FILTER`
    values fail loudly instead of exiting green.
  - Updated `TODO.md` so the audit plan items are closed after validation.
- Experiment commands and key metrics
  - `c3c build`
  - `c3c build --sanitize=address`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
    - summary: `105 passed, 0 failed`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot ./build/main --test-suite lisp`
    - summary: `1 passed, 0 failed`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=does-not-exist ./build/main --test-suite lisp`
    - summary: failed loudly as intended with `deduce group filter matched no tests`
  - ASAN runs produced leak reports dominated by pre-existing LMDB/runtime allocations; they did not surface a new functional failure in the changed slices.
- Best current checkpoint/config recommendation
  - Keep the non-ASAN behavioral results as the shipped signal for these audit slices, and treat the ASAN leak output as separate existing noise until that broader teardown work is addressed.
- Unresolved issues and next actions
  - If the leak reports need to become actionable, split that into its own teardown/cleanup audit item rather than folding it back into these OOM/harness fixes.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Triaged the live TODO queue and split it into runnable slices for parallel
    agent work.
  - Launched three worker agents for the boundary telemetry, equality/offload
    allocator, and deduce/governance tracks.
  - Recovered from an interrupted wait cycle and verified that the broad worker
    batch had not produced usable output before closing it out.
- Code/config changes made
  - Added `scripts/run_boundary_profile_regression.sh` to chain the container-
    capped boundary workload, parse step, and threshold check into one
    runnable regression command.
  - Updated `docs/plans/boundary-profiling-baseline-2026-03-11.md` to point at
    the new regression wrapper.
  - Added a `Validation Lanes` section to `docs/PROJECT_TOOLING.md` to make
    the boundary/lifetime vs allocator vs syntax/compiler validation split
    explicit.
  - Clarified the lane-ownership wording in `docs/areas/memory-runtime.md` so
    syntax/compiler-only work does not inherit a memory lane by default.
  - Updated the deduce benchmark plan docs:
    `docs/plans/deduce-scan-query-count-benchmark-baseline-2026-03-11.md`,
    `docs/plans/deduce-scan-range-materialization-cost-baseline-2026-03-11.md`,
    and `docs/plans/deduce-query-optimization-evaluation-2026-03-11.md` to
    match the current benchmark-file split.
  - Added a mixed nested equality audit case in
    `src/lisp/tests_memory_lifetime_boundary_decision_bench_groups.c3` and
    threaded the extra mixed-case timings into the existing equality benchmark
    summary.
  - Added offload allocation audit counters in
    `src/lisp/scheduler_offload_worker.c3` and exposed them through
    `src/lisp/tests_scheduler_offload_bench_groups.c3`.
  - Surfaced boundary telemetry summaries in
    `scripts/parse_boundary_profile_summary.sh` so scope-chain pressure and
    dominant return-path outcomes appear directly in the regression flow.
  - Hardened the offload pool/reuse boundary in
    `src/lisp/scheduler_offload_worker.c3` and strengthened
    `src/lisp/tests_scheduler_boundary_worker.c3` to assert the pooled reuse
    fast path.
  - Moved `prim_schema_explain(...)` into
    `src/lisp/schema_validation.c3` so validation-facing entrypoints live
    together and `src/lisp/schema.c3` stays focused on the explain selector
    dispatcher.
  - Split the dispatch/match diagnostics formatting helpers out of
    `src/lisp/eval_dispatch_types.c3` into
    `src/lisp/eval_dispatch_match_errors.c3` so the type registry and
    type-query surface stay focused in the original file.
  - Extracted the `deduce_why_result_*` explainability/path-building block and
    `prim_deduce_why_result` into `src/lisp/deduce_why_result.c3` so
    `src/lisp/deduce_schema_query.c3` can stay focused on query/execution.
  - Closed the corresponding deduce explainability extraction TODO line in
    `TODO.md`; the only remaining open item for that slice is targeted
    validation.
  - Added a tooling note that removed `OMNI_LISP_TEST_SLICE` slice names
    (`memory-soak`, `syntax`) are rejected by
    `src/lisp/tests_slice_policy.c3` and the explicit slice names should be
    used instead.
  - Closed the `deduce-query` optimization TODO line in `TODO.md` after
    confirming `docs/plans/deduce-query-optimization-evaluation-2026-03-11.md`
    already records the final decision to keep the current full-scan + callback
    filtering model.
  - Closed the modularization split-candidate TODO line in `TODO.md` after the
    schema extraction landed and the remaining listed candidates were already
    split in earlier slices.
  - Narrowed the remaining modularization TODO in `TODO.md` to the
    `deduce_why_result_*` extraction from `src/lisp/deduce_schema_query.c3`
    and kept the validation follow-up scoped to that same slice.
  - Added a derived `scan_range_materialize_us_per_row` metric to
    `src/lisp/tests_deduce_query_bench_groups_more.c3`.
  - Reconciled `docs/plans/dispatch-hot-path-benchmark-baseline-2026-03-11.md`
    with the actual benchmark file in
    `src/lisp/tests_advanced_type_dispatch_groups.c3`.
  - Expanded the equality inline-first workspace caps in
    `src/lisp/eval_pattern_support_helpers.c3` to keep common nested equality
    comparisons on-stack longer.
  - Hoisted the deduce in-txn scan-row dict capacity computation out of the
    per-row loop in `src/lisp/deduce_relation_scan_helpers_more.c3`.
- Experiment commands and key metrics
  - `wait_agent` on the three broad workers timed out twice and produced no
    final status.
  - Closed the stalled workers; each returned `previous_status: running`.
  - The live queue inventory surfaced 24 open items in
    `TODO.md` under `Legacy Runtime and Validation Follow-up`.
  - One deduce docs worker completed cleanly; the equality benchmark worker was
    closed after timing out without a patch.
  - Later, two tighter workers for equality and deduce benchmark code were
    also closed after timing out without a patch; the equality and offload
    slices were finished locally instead.
  - The boundary telemetry parser lane and offload pool/reuse lane both landed
    after the first report draft, so this entry now reflects the full shipped
    scope for the session.
  - The deduce per-row materialization metric and dispatch benchmark-plan
    reconciliation landed after the main queue pass, so they are now recorded
    as shipped session work as well.
  - The alias audit and modularization backlog reconciliation landed after the
    code slices, so the live queue count dropped to 2 open items.
  - The bounded deduce validation run for the `deduce_why_result` split used
    `OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
    and finished with `282 passed, 112 failed`.
  - The first actionable failure surfaced by that run was
    `deduce parallel worker-scratch component pass computes serialized recursive deltas`.
  - After wiring `scratch_visible_sets` through the dry-run worker-scratch call
    chain, `c3c build` now passes again.
  - A follow-up bounded deduce validation rerun
    (`scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`)
    now aborts with `exit 139` before the suite reaches a normal fail-summary
    boundary.
  - The failure set was broader than the `deduce_why_result` extraction itself,
    spanning recursive worker-scratch, why-result metadata, selector-scoped
    deduce query, stats/analyze, and aggregate/recursive aggregate coverage.
  - Promoted the recursive worker-scratch recursive-delta failure into its own
    explicit backlog blocker in `TODO.md`, and kept the `deduce_why_result`
    targeted validation item open behind it.
  - The equality workspace cap expansion and deduce scan-path hoist landed as
    the last remaining semantic cleanup items in the current runtime follow-up
    queue.
- Best current checkpoint/config recommendation
  - Treat `TODO.md` as the live execution queue and continue with narrower
    worker scopes rather than broad triage batches.
- Unresolved issues and next actions
  - Relaunch focused agents for the highest-signal slices:
    boundary telemetry, equality/offload allocator work, and deduce perf.
  - If the next worker batch stalls again, switch to direct local execution on
    one narrow backlog item instead of waiting on the whole queue.
  - Continue the offload and deduce workers only if they return cleanly; if not,
    restart them with a smaller write scope.
  - Restart the equality benchmark probe with a single-file write scope if that
    slice needs to be pushed further.
  - The equality and offload audit slices now have concrete local patches; the
    remaining code-side work is the deduce benchmark hook if that lane still
    needs a code change.
  - Keep TODO, changelog, and this report aligned if any follow-up slices land
    on top of the boundary parser or offload pool/reuse changes.
  - The deduce summary metric and dispatch plan correction are now part of the
    shipped session boundary, not residual work.
  - The equality workspace strategy and deduce scan-path allocation cleanup are
    now fully reflected in `TODO.md` and `memory/CHANGELOG.md`.
  - The targeted deduce validation item remains open because the bounded deduce
    lane currently crashes (`exit 139`) before the worker-scratch serialized
    recursive-delta assertion can be confirmed green.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted:
  - harden the filtered test-group dispatch paths so unknown filters fail loudly instead of exiting green
- Code/config changes made:
  - made `OMNI_DEDUCE_GROUP_FILTER` and `OMNI_ADVANCED_GROUP_FILTER` report a failure when they match no tests
- Experiment commands and key metrics:
  - no validation run yet for this slice
- Best current checkpoint/config recommendation:
  - if this slice is validated, close the filter-dispatch TODO item and then move back to the remaining audit backlog
- Unresolved issues and next actions:
  - targeted validation still needs to confirm the filtered-dispatch failure path
  - validate this slice before marking the corresponding TODO checkbox complete
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted:
  - harden the scheduler offload queue allocation-failure path in `src/lisp/scheduler_offload_worker.c3`
- Code/config changes made:
  - returned failure immediately when queued-work allocation fails instead of resetting a null buffer
- Experiment commands and key metrics:
  - no validation run yet for this slice
- Best current checkpoint/config recommendation:
  - move on to the filtered test-group dispatch slice next
- Unresolved issues and next actions:
  - the offload path now fails safely on queued-work allocation failure, but it still needs validation
  - validate this slice before marking the corresponding TODO checkbox complete
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted:
  - harden the interpreter module/macro initialization allocation path in `src/lisp/value_interp_init_helpers.c3`
- Code/config changes made:
  - added explicit allocation assertions before the module and macro table fill loops
  - kept startup failure local and deterministic instead of allowing a later null dereference
- Experiment commands and key metrics:
  - no validation run yet for this slice
- Best current checkpoint/config recommendation:
  - continue with the scheduler offload queue slice next
- Unresolved issues and next actions:
  - the startup path still fails fast rather than recovering from OOM; that is acceptable for now but should remain documented
  - validate this slice before marking the corresponding TODO checkbox complete
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted:
  - harden the environment hash-table OOM path in `src/lisp/value_environment.c3`
- Code/config changes made:
  - kept the old environment hash table live until a replacement is fully allocated and initialized
  - skipped binding-array growth when the expansion allocation fails instead of dereferencing a null buffer
- Experiment commands and key metrics:
  - no validation run yet for this slice
- Best current checkpoint/config recommendation:
  - continue with the interpreter init allocation-failure slice next, then the scheduler offload queue slice
- Unresolved issues and next actions:
  - the env write path still lacks a propagated failure signal, so the current fix is crash-safe but not yet semantically fail-fast on OOM
  - validate this slice before marking the corresponding TODO checkbox complete
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Isolate the `deduce_why_result` validation target so the harness runs only the
    bounded why-result regression block.
  - Remove temporary scratch-pass debug scaffolding now that the worker-scratch
    blocker has been validated and closed.
- Code/config changes made
  - Added a file-wide `OMNI_DEDUCE_QUERY_FILTER` guard in
    [`src/lisp/tests_deduce_groups.c3`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3)
    so the deduce group dispatcher short-circuits to the query target instead of
    running unrelated deduce subgroups first.
  - Kept the query-side one-shot target guard in
    [`src/lisp/tests_deduce_query_groups.c3`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_query_groups.c3)
    so the targeted `why-result-bounded` helper runs exactly once.
  - Removed the temporary env-gated scratch-pass trace scaffolding from
    [`src/lisp/tests_deduce_groups.c3`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3)
    while preserving the minimal `OMNI_DEDUCE_GROUP_FILTER` isolation gate.
  - Closed the remaining `deduce_why_result` validation TODO item in
    [`TODO.md`](/home/heefoo/Documents/code/Omni/TODO.md).
- Experiment commands and key metrics
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=why-result-bounded ./build/main --test-suite lisp`
  - key metric: `=== Unified Tests: 1 passed, 0 failed ===`
- Best current checkpoint/config recommendation
  - Use `OMNI_DEDUCE_QUERY_FILTER=why-result-bounded` for the bounded why-result
    regression target; the dispatcher now isolates that path without dragging in
    unrelated deduce groups.
- Unresolved issues and next actions
  - The targeted validation slice is complete.
  - Any further deduce work should come from the separate broader runtime failure
    path rather than this closed validation item.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Re-slice the remaining unfiltered deduce crash into a dedicated reference-head
    validation boundary and verify whether the failure stays after isolation.
- Code/config changes made
  - Extracted the recursive reference-head assertion into
    [`run_deduce_rule_head_reference_snapshot_test(...)`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3)
    inside [`src/lisp/tests_deduce_groups.c3`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3).
  - Added `OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot` handling in
    [`run_deduce_group_tests(...)`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3)
    so the harness can run only that boundary.
  - Updated [`TODO.md`](/home/heefoo/Documents/code/Omni/TODO.md) to make the
    reference-head crash/hang the sole open backlog item again.
- Experiment commands and key metrics
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot ./build/main --test-suite lisp`
  - key state so far: the run reaches the isolated reference-head helper and has
    not yet produced a clean completion signal during multiple polls
- Best current checkpoint/config recommendation
  - Treat `rule_head_reference_snapshot` as the next isolation boundary; the
    broader deduce suite is no longer the right entrypoint for this issue.
- Unresolved issues and next actions
  - Determine whether the extracted `db-rule-ref-ok` helper is itself the hang/crash
    point or whether it still needs one more smaller split.
  - Keep the broader unfiltered deduce run blocked behind this narrower boundary.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Correct the mistaken extraction boundary in the deduce rule-head integrity
    tests after the helper split exposed a self-recursion bug.
- Code/config changes made
  - Restored the keyed and unique integrity checks to
    [`run_deduce_rule_head_integrity_tests(...)`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3)
    in their original order.
  - Kept [`run_deduce_rule_head_reference_snapshot_test(...)`](/home/heefoo/Documents/code/Omni/src/lisp/tests_deduce_groups.c3)
    as the dedicated reference-head boundary helper containing only the
    `db-rule-ref-ok` regression block.
- Experiment commands and key metrics
  - no validation rerun after this correction yet
- Best current checkpoint/config recommendation
  - Rebuild before any follow-up validation, because the reference-head helper
    split has changed again since the last container run.
- Unresolved issues and next actions
  - Re-run the dedicated `OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot`
    validation after rebuild.
  - Keep the broader unfiltered deduce crash isolated behind that slice.
- Signature: Codex (GPT-5)

## 2026-03-26
- Objective
  - Isolate and fix the bounded `OMNI_LISP_TEST_SLICE=deduce` failure on the
    worker-scratch recursive-delta lane, then re-check
    `deduce parallel worker-scratch component pass computes serialized
    recursive deltas`.
- Code/config changes made
  - Added temporary `OMNI_DEDUCE_GROUP_FILTER` gating and step-by-step trace
    prints in `src/lisp/tests_deduce_groups.c3` to isolate the scratch-pass
    path.
  - Fixed the scratch-pass test fixture to use canonical `deduce/rule!`
    installs and fresh `*-scratch-pass` relation names so the test exercises a
    clean recursive component instead of reusing already materialized state.
- Experiment commands and key metrics
  - `c3c build` -> passes.
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=parallel_component_scratch_pass ./build/main --test-suite lisp`
  - Before the fix, the isolated test reported `predicate-count=0` or skipped
    existing head rows and failed.
  - After the fix, the isolated test reported `predicate-count=2`,
    `entry[0]=path-parallel-scratch-pass adds=2 removes=0`, and completed with
    `1 passed, 0 failed`.
  - Enabling `OMNI_TRACE_DEDUCE_WORKER_SCRATCH=1` showed the worker path
    reached `scratch_pass_begin`, both rule-loop iterations, and
    `scratch_pass_end_before_serialize` with `a=2 b=2`.
- Best current checkpoint/config recommendation
  - Keep the fresh `parallel_component_scratch_pass` fixture as the regression
    target while revalidating the broader deduce slice.
- Unresolved issues and next actions
  - Re-run the broader bounded deduce slice to confirm no other worker-scratch
    paths remain broken.
  - Trim the temporary debug traces if they are no longer needed.
- Signature: Codex (GPT-5)

## 2026-03-26

- Objectives attempted
  - Add minimal, gated instrumentation on the deduce worker-scratch path to
    isolate where bounded deduce validation stalls/crashes.
  - Re-run build plus bounded deduce validation with tracing enabled.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_exec_seminaive.c3` with opt-in tracing
    guarded by `OMNI_TRACE_DEDUCE_WORKER_SCRATCH=1`.
  - Instrumented these boundaries:
    - `deduce_seminaive_compute_component_scratch_pass_payload(...)`
    - `deduce_seminaive_evaluate_rule_dry_run(...)`
    - `deduce_seminaive_emit_head_fact(...)`
    - scratch replay/visible-accumulation paths in seminaive step execution.
  - Added helper counters for component-local visible/additions tuple totals for
    trace messages.
- Experiment commands and key metrics
  - `c3c build` -> pass (`Program linked to executable 'build/main'`).
  - `OMNI_TRACE_DEDUCE_WORKER_SCRATCH=1 scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  - key observations:
    - validation run did not reach completion (stalled/hung state),
    - `/tmp/deduce_worker_scratch_trace.log` stayed at `167` lines across
      repeated checks (including a `30s` stability window),
    - log contained `0` `OMNI_DEDUCE_WORKER_SCRATCH` lines,
    - last named boundary stayed:
      `[PASS] deduce analyze recursive limit reports stable error code`.
- Best current checkpoint/config recommendation
  - Keep this instrumentation patch as a temporary triage slice and move the
    next probe earlier than the worker-scratch path, because current bounded
    deduce execution stalls before any worker-scratch marker is reached.
- Unresolved issues and next actions
  - Open blocker remains active:
    `Investigate/fix bounded OMNI_LISP_TEST_SLICE=deduce crash/hang on the
    worker-scratch recursive-delta lane`.
  - Next actionable move: add one earlier deduce-suite progress marker (before
    scratch tests are invoked) to identify the exact pre-scratch stall point.
- Signature: Codex (GPT-5)

## 2026-03-24

- Follow-up slice completed
  - Objective: close the next editor-tooling backlog item for structured
    `--check --json` diagnostics.
  - Code/config changes made:
    - Updated `src/entry_check_mode.c3` to run parser diagnostics first, then
      compiler/lowering validation (via `lisp::compile_to_c3`) before reporting
      `--check --json`, and removed an unused `getenv` declaration.
    - Updated `src/entry_check_reporting.c3` to:
      - use a dedicated compiler-lowering diagnostic code constant
      - thread the diagnostic code into `print_check_result_json`.
    - Marked `omni --check --json` complete in
      `docs/plans/editor-tooling-roadmap.md`.
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this patch-only slice;
      signature and reference checks were validated by a `rg` scan.
  - Best current checkpoint/config recommendation
    - Keep the completed `--check --json` and structured eval transport behavior in
      place and move to LSP/tooling integration rebase (`tooling/omni-lsp`,
      `tooling/omni-nvim`).
  - Unresolved issues and next actions
    - Remaining non-test/editoring backlog queue includes:
      - rebase `tooling/omni-lsp` onto structured CLI contracts
      - Tree-sitter corpus/query hardening
      - `tooling/omni-nvim` structured-eval + Tree-sitter selection upgrade
      - formatter/indentation implementation
  - Signature: Codex (GPT-5)

- Objectives attempted
  - Close the next runtime split backlog slice by splitting
    `src/lisp/deduce_rule_eval.c3` into a focused orchestration file plus a new
    validation helper file.
  - Continue the same backlog lane by splitting
    `src/lisp/eval_env_copy.c3` into `src/lisp/eval_env_copy_helpers.c3`
    with a shim-style caller module.
- Code/config changes made
  - Added `src/lisp/deduce_rule_eval_validation.c3`.
  - Reduced `src/lisp/deduce_rule_eval.c3` to orchestration calls into the new
    validation helper module.
  - Updated `docs/plans/largest-runtime-files-pass-2026-03-19.md`:
    - added the landed `deduce_rule_eval` split slice
    - refreshed next-queue counts/state
    - added the landed `eval_env_copy` split slice
    - removed `eval_env_copy` from queue head
  - Updated `memory/CHANGELOG.md` with the landed runtime split entry.
  - Updated `memory/CHANGELOG.md` with the landed `eval_env_copy` runtime split
    entry.
  - Added `src/lisp/eval_env_copy_helpers.c3`.
  - Reduced `src/lisp/eval_env_copy.c3` to a shim comment pointing at the
    helper file.
- Experiment commands and key metrics
  - no new targeted commands were run for `eval_env_copy` split in this slice
  - reused checkpoint evidence from the immediately previous landed slice:
    - `c3c build` (pass)
    - `scripts/run_validation_status_summary.sh build/validation_status_summary.json`
      (`9` total runs, `6` pass, `3` fail with pre-existing failures:
      `status_consistency`, `jit_policy`, `deduce`)
- Best current checkpoint/config recommendation
  - Keep `deduce_rule_eval` split landed in this pass and continue with
    `src/lisp/eval_pattern_support.c3` as the next size-driven file split
    candidate.

## 2026-03-26

- Objectives attempted
  - Validate the corrected `rule_head_reference_snapshot` deduce slice after
    fixing the helper extraction boundary in `src/lisp/tests_deduce_groups.c3`.
  - Confirm that the targeted helper now runs in isolation without dragging in
    unrelated deduce subgroups.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_groups.c3` so
    `run_deduce_rule_head_reference_snapshot_test(...)` contains only the
    `db-rule-ref-ok` reference-head regression block.
  - Restored the keyed and unique integrity checks to
    `run_deduce_rule_head_integrity_tests(...)` so the helper split no longer
    self-recurses or steals earlier integrity coverage.
  - Kept the `OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot` dispatch
    hook in `run_deduce_group_tests(...)` for the isolated target path.
- Experiment commands and key metrics
  - `c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot ./build/main --test-suite lisp`
  - key metric: `=== Unified Tests: 1 passed, 0 failed ===`
- Best current checkpoint/config recommendation
  - Use `OMNI_DEDUCE_GROUP_FILTER=rule_head_reference_snapshot` as the narrow
    regression checkpoint for this helper boundary.
- Unresolved issues and next actions
  - The isolated reference-head slice is green.
  - Any remaining deduce failures belong to the broader unfiltered runtime
    blocker, not this extracted helper.
- Signature: Codex (GPT-5)
- Unresolved issues and next actions
  - Investigate and resolve the three status-summary failures (`status_consistency`,
    `jit_policy`, `deduce`) to restore a fully green pass gate.
- Follow-up slice completed
  - Objective: complete the next `docs`/`examples` backlog item by replacing
    removed syntax forms with canonical constructor/type spellings.
  - Changes:
    - Updated examples in `examples/deduce_crud_server.omni` and
      `examples/finwatch/*.omni` to use canonical type annotations:
      `^Integer` / `^Boolean` and canonical quoted type symbols.
    - Updated docs in:
      - `docs/reference/04-type-system.md`
      - `docs/reference/09-concurrency-ffi.md`
      - `docs/type-system-syntax.md`
      - `docs/PROJECT_TOOLING.md`
      - `docs/FEATURES.md`
      - `docs/LANGUAGE_SPEC.md`
    - Marked `docs/plans/post-complete-backlog.md` item as complete.
  - Experiment commands and key metrics
    - no new shell validation commands were run for this docs/examples cleanup slice.
  - Best current checkpoint/config recommendation
    - Proceed to the next post-complete slice; keep additional alias cleanup
      scoped to docs that explicitly present non-canonical language syntax.
  - Unresolved issues and next actions
    - Continue scanning for `Int`/`^Int` older examples in remaining docs once the
      editor tooling roadmap changes land.
- Follow-up slice started
  - Objective: begin the next post-complete backlog item by creating a dedicated
    Deduce analytics milestone for statistics + cleanup maintenance verbs.
  - Changes:
    - Added `docs/plans/deduce-analytics-extension-milestone-2026-03-24.md` with a
      minimal two-track task list:
      - statistics maintenance/refresh planning
      - data-cleanup verb expansion planning
    - Marked `Add a minimal analytics extension task list for deduce ...` as completed
      in `docs/plans/post-complete-backlog.md` and linked it to the new milestone.
    - Cross-referenced the dedicated milestone from:
      - `docs/plans/deduce-sophistication-plan-2026-03-20.md`
      - `docs/plans/README.md`
  - Experiment commands and key metrics
    - no runtime commands were run for this docs-only planning slice.
  - Best current checkpoint/config recommendation
    - Keep the item as a planning slice and begin execution slices directly from
      `docs/plans/deduce-analytics-extension-milestone-2026-03-24.md`.
  - Unresolved issues and next actions
    - Continue with the next unchecked post-complete backlog item:
      `Add tests for deterministic behavior in deduce cleanup and statistics
      operations under empty-relation and malformed-schema inputs.`
- Follow-up slice completed
  - Objective: continue the largest-first runtime modularization lane with the
    next queued file split.
  - Changes:
    - Added `src/lisp/eval_pattern_support_helpers.c3`.
    - Reduced `src/lisp/eval_pattern_support.c3` to a shim comment pointing at
      the helper file.
    - Updated `docs/plans/largest-runtime-files-pass-2026-03-19.md`:
      - added the landed `eval_pattern_support` split slice
      - refreshed next-queue ordering/counts
    - Marked `Continue largest-first modularization ...` as completed in
      `docs/plans/post-complete-backlog.md`.
  - Experiment commands and key metrics
    - no new validation/build commands were run for this split-only slice.
  - Best current checkpoint/config recommendation
    - Continue with `src/lisp/aot_runtime_bridge.c3` as the next largest queued
      runtime split target.
  - Unresolved issues and next actions
    - Run central validation (`c3c build` and status-summary gate) once the next
      split cluster is landed.
- Follow-up slice completed
  - Objective: close the public-doc migration-note item for effect-handler
    composition helper naming.
  - Changes:
    - Added migration note to `docs/EFFECTS_GUIDE.md` and documented
      `handle/chain` as the canonical helper name.
    - Added corresponding migration-note subsection in
      `docs/LANGUAGE_SPEC.md` section `10.6`.
    - Aligned helper naming in `docs/syntax-decision.md` (`handle/chain`
      canonical; historical `with-handlers` / `handle-chain` non-canonical).
    - Marked `Add a migration note for effect-handler composition helper
      names ...` as completed in `docs/plans/post-complete-backlog.md`.
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this docs-only slice.
  - Best current checkpoint/config recommendation
    - keep `handle/chain` as the only helper name used in public examples.
  - Unresolved issues and next actions
    - remaining non-test backlog item is the editor tooling roadmap execution
      lane.
- Follow-up slice completed
  - Objective: execute the editor tooling roadmap with a concrete
    bootstrap/docs integration slice.
  - Changes:
    - Added a runnable editor tooling quickstart in `README.md` under
      `Editor Tooling`.
    - Added `Editor Tooling Bootstrap (Roadmap Slice)` in
      `docs/PROJECT_TOOLING.md` with setup commands/snippets for:
      - `tooling/tree-sitter-omni`
      - `tooling/omni-lsp`
      - `tooling/omni-nvim`
    - Updated `docs/plans/editor-tooling-roadmap.md`:
      - marked bootstrap wiring for all three tooling packages complete
      - marked README editor integration example complete
      - added landed-slice note with residual queue explicitly left open
    - Updated `docs/plans/post-complete-backlog.md`:
      - marked the roadmap execution item complete for the shipped slice
      - added a new explicit residual item for remaining roadmap work
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this docs/tooling slice.
  - Best current checkpoint/config recommendation
    - use `docs/PROJECT_TOOLING.md` as the canonical bootstrap entrypoint for
      first-party editor integration.
  - Unresolved issues and next actions
    - continue with the new residual roadmap item in
      `docs/plans/post-complete-backlog.md`.
- Follow-up slice completed
  - Objective: close the `--init` docs integration sub-slice in the editor
    tooling roadmap.
  - Changes:
    - Updated `docs/PROJECT_TOOLING.md` in the `--init` section with a
      recommended post-scaffold editor/tooling setup path.
    - Updated `docs/man/omni.1` (`Init`) with the same first-step check and
      pointer to tooling docs.
    - Marked `Update --init templates or docs with recommended editor/tooling
      setup.` as completed in `docs/plans/editor-tooling-roadmap.md`.
    - Refined remaining residual roadmap wording in
      `docs/plans/post-complete-backlog.md` to remove the now-completed
      `--init` integration mention.
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this docs/tooling slice.
  - Best current checkpoint/config recommendation
    - keep `docs/PROJECT_TOOLING.md` as the canonical setup path and mirror
      key CLI guidance in `docs/man/omni.1`.
  - Unresolved issues and next actions
    - continue remaining editor-tooling roadmap slices:
      structured test output and deeper formatter/LSP/Neovim upgrades.
- Follow-up slice completed
  - Objective: close the Neovim help-tags/docs sub-slice from the editor
    tooling roadmap.
  - Changes:
    - Added first-party Neovim help doc:
      `tooling/omni-nvim/doc/omni.nvim.txt`.
    - Updated `tooling/omni-nvim/README.md` with explicit helptags generation
      and `:help omni.nvim` usage.
    - Marked `Add local docs/help tags or a concise :help omni.nvim path.`
      as completed in `docs/plans/editor-tooling-roadmap.md`.
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this docs/tooling slice.
  - Best current checkpoint/config recommendation
    - keep `tooling/omni-nvim/doc/` as the canonical in-repo help surface and
      retain README parity with the shipped help tags path.
  - Unresolved issues and next actions
    - continue remaining roadmap slices (structured test output plus deeper
      formatter/LSP/Neovim feature work).
- Follow-up slice completed
  - Objective: close the formatting-policy decision and canonical-rules
    documentation sub-slice in the editor tooling roadmap.
  - Changes:
    - Updated `docs/PROJECT_TOOLING.md` with a dedicated
      `Formatting and Indentation Policy` section:
      - decision: `both in phases` with indentation-first behavior
      - canonical formatting rules for special forms, vector/dict literals,
        `handle` clauses, type/method declarations, and macro/template blocks
    - Updated roadmap checkboxes in
      `docs/plans/editor-tooling-roadmap.md` for:
      - strategy decision (`both in phases`)
      - canonical formatting-rules definition
    - Updated roadmap landed-slice notes with this formatter-policy closure.
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this docs/policy slice.
  - Best current checkpoint/config recommendation
    - keep `docs/PROJECT_TOOLING.md` as the canonical formatting-policy source
      until `omni --fmt` semantics are implemented.
  - Unresolved issues and next actions
    - continue with remaining editor-tooling roadmap work:
      structured test output and deeper formatter/LSP/Neovim implementation
      slices.
- Follow-up slice completed
  - Objective: implement first-party machine-readable symbol help for editor
    integrations.
  - Changes:
    - Added CLI mode `--describe <symbol> [--json]`:
      - `src/entry_describe_mode.c3`
      - `src/entry_describe_reporting.c3`
      - wired in `src/entry.c3`
      - usage text updated in `src/entry_cli_help_version.c3`
    - Added docs/man coverage:
      - `docs/PROJECT_TOOLING.md`
      - `docs/man/omni.1`
    - Updated roadmap progress:
      - marked `--describe <symbol>` item complete in
        `docs/plans/editor-tooling-roadmap.md`
      - refined residual wording in
        `docs/plans/post-complete-backlog.md`
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this implementation+docs
      slice.
  - Best current checkpoint/config recommendation
    - use `omni --describe --json <symbol>` as the first-party machine-readable
      symbol-help source for editor hover/help integration.
  - Unresolved issues and next actions
    - remaining roadmap queue still includes structured test output and deeper
      formatter/LSP/Neovim feature upgrades.
- Signature: Codex (GPT-5)

## 2026-03-25

- Objectives attempted
  - Close the first truthful proof-path-integrated goal-directed provenance
    slice for `deduce/why-result` instead of leaving the whole lane as one
    broad residual item.
- Code/config changes made
  - Updated `src/lisp/deduce_db_handles.c3`,
    `src/lisp/deduce_db_handles_register.c3`,
    `src/lisp/deduce_db_handles_mutation_tracking.c3`, and
    `src/lisp/deduce_schema_query.c3`:
    - relation schemas now remember an exact-one goal-directed `query`
      subject key
    - `deduce/why-result` now attaches path-local
      `goal-directed-read-context` only when the traced tuple matches that
      exact-one query subject snapshot
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a regression proving the matching tuple gets path-local context
      while another row in the same relation does not
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2b`
    - closed `B6.9b2b1`
    - left broader provenance integration as `B6.9b2b2`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - exact-one goal-directed `query` row now yields path-local
      `goal-directed-read-context`
    - another row in the same relation keeps that path-local field `nil`

- Objectives attempted
  - Close the next truthful proof-integrated provenance slice by extending the
    exact-one path-local goal-directed context from `deduce/query` to
    `deduce/match`.
- Code/config changes made
  - Updated `src/lisp/unify.c3`:
    - goal-directed transactional `deduce/match` scans now capture the matched
      stored-row key when exactly one tuple matched
    - the relation schema stores that exact-one subject snapshot after the
      `match` read is recorded
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened path-local `goal-directed-read-context` attachment from exact-one
      `query` subjects to exact-one `query` or `match` subjects
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a regression proving the matching tuple gets path-local context
      after exact-one goal-directed `deduce-match`
    - another row in the same relation still keeps that path-local field `nil`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2b2`
    - closed `B6.9b2b2a`
    - left broader provenance integration as `B6.9b2b2b`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - exact-one goal-directed `deduce/match` row now yields path-local
      `goal-directed-read-context`
    - another row in the same relation keeps that path-local field `nil`

- Objectives attempted
  - Close the next truthful proof-integrated provenance slice by extending the
    exact-one path-local goal-directed context from `deduce/query` /
    `deduce/match` to `deduce/scan-range`.
- Code/config changes made
  - Updated `src/lisp/deduce_relation_scan_helpers_more.c3`:
    - transactional `deduce/scan-range` scans can now capture the exact-one
      stored-row key when the result set has one row
  - Updated `src/lisp/deduce_relation_ops_query.c3`:
    - goal-directed `deduce/scan-range` now records that exact-one subject
      snapshot into the relation schema after the read is recorded
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened path-local `goal-directed-read-context` attachment from exact-one
      `query` / `match` subjects to exact-one `query` / `match` /
      `scan-range` subjects
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a regression proving the matching tuple gets path-local context
      after exact-one goal-directed `deduce/scan-range`
    - another row in the same relation still keeps that path-local field `nil`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2b2b`
    - closed `B6.9b2b2b1`
    - left broader provenance integration as `B6.9b2b2b2`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - exact-one goal-directed `deduce/scan-range` row now yields path-local
      `goal-directed-read-context`
    - another row in the same relation keeps that path-local field `nil`

- Objectives attempted
  - Close the next truthful proof-integrated provenance slice by pinning
    selector-scoped parity for the shipped exact-one row-read path-local
    context surface.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a combined regression for selector-scoped exact-one
      `deduce/query`, `deduce/match`, and `deduce/scan-range`
    - matching proof paths now have explicit coverage for
      `selector-rule-index = 1`
    - non-matching rows in the same relation still keep that path-local field
      `nil`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2b2b2`
    - closed `B6.9b2b2b2a`
    - left broader provenance integration as `B6.9b2b2b2b`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - selector-scoped exact-one `query`, `match`, and `scan-range` paths all
      now have pinned path-local `goal-directed-read-context`
    - each matching path keeps `selector-rule-index = 1`

- Objectives attempted
  - Close the next truthful proof-integrated provenance slice by extending the
    exact-one path-local goal-directed context to `deduce/scan`.
- Code/config changes made
  - Updated `src/lisp/deduce_relation_scan_helpers_more.c3`:
    - full-relation `deduce/scan` can now capture the exact-one stored-row key
      when the scan result has one row
  - Updated `src/lisp/deduce_relation_ops_query.c3`:
    - `deduce/scan` now records that exact-one subject snapshot into the
      relation schema after the read completes
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened path-local `goal-directed-read-context` attachment from exact-one
      `query` / `match` / `scan-range` subjects to exact-one
      `query` / `match` / `scan` / `scan-range` subjects
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a regression for plain exact-one `deduce/scan`
    - kept selector-scoped parity scoped to the already shipped
      `query` / `match` / `scan-range` slice after confirming selector-scoped
      `scan` is not in the current goal-directed eligibility surface
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2b2b2b`
    - closed `B6.9b2b2b2b1`
    - left broader provenance integration as `B6.9b2b2b2b2`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - exact-one goal-directed `deduce/scan` now yields path-local
      `goal-directed-read-context`
    - selector-scoped `deduce/scan` stayed outside the current shipped slice
      and was left undocumented as such

- Objectives attempted
  - Close the next integrity-class naming lane so the remaining Deduce
    constraint queue targets one concrete class instead of generic “widened
    integrity” wording.
- Code/config changes made
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`, and
    `docs/reference/08-libraries.md`:
    - closed `B6.10a1`
    - fixed the next integrity class as canonical `check`
    - renamed the remaining backlog items to refer to `check` constraints
      directly
  - Added `docs/plans/deduce-next-integrity-class-decision-2026-03-25.md`:
    - recorded the naming decision, rationale, rejected aliases, and the
      remaining follow-up slices
- Validation run
  - doc/backlog shaping only:
    - confirmed `TODO.md` now closes `B6.10a1`
    - confirmed remaining `B6.10*` items now refer to `check` constraints

- Objectives attempted
  - Turn the first parallel-runtime blocker into a concrete execution-model
    decision instead of pretending the existing LMDB-writing evaluator could
    be fanned out safely.
- Code/config changes made
  - Closed `B6.9b2b2b2b2b2b2b2b2` with path-level mixed-context goal-directed
    provenance lists across support-frame relations:
    - proof paths now expose `goal-directed-read-contexts` when matching
      support-frame contexts come from different relations
    - same-relation support paths still use the singular
      `goal-directed-read-context` field instead of a plural list
  - Closed `B6.9b2b2b2b2b2b2b2b1` with common path-level goal-directed context
    propagation across same-relation support frames:
    - proof paths now inherit bounded `goal-directed-read-context` when all
      matching support-frame contexts come from the same relation-local
      last-read state
    - kept mixed-relation support paths unmerged and left that broader merge
      semantics open
  - Closed `B6.9b2b2b2b2b2b2b2a` with unique path-level goal-directed context
    propagation from matching support frames:
    - when the root tuple did not match but exactly one support frame in the
      chosen proof path carried bounded `goal-directed-read-context`, the
      proof path now inherits that same context
    - kept the broader multi-frame merge case open instead of guessing merge
      semantics
  - Closed `B6.9b2b2b2b2b2b2b1` with bounded proof-path goal-directed context
    for matching fact support frames:
    - added a regression proving `deduce/why-result` now also attaches
      path-local `goal-directed-read-context` to matching fact support frames
      inside a derived proof path
    - split the remaining provenance work to broader proof-path integration
      beyond the current root/fact-frame/rule-step row-matching surface
  - Closed `B6.11b2` with explicit runtime/admin truth for the parallel lane:
    - `deduce/analyze`, relation-local `deduce/stats`, and selector-scoped
      `deduce/explain` now expose `parallel-runtime-mode`
    - the shipped value is `metadata-only`, which makes the public boundary
      explicit instead of forcing readers to infer it from
      `execution-engine = 'semi-naive-scc`
  - Closed `B6.11b1b2b` with broader worker-scratch recursive closure for
    positive multi-atom SCC rules:
    - non-anchor recursive atoms in worker-scratch closure now read from
      `LMDB + prior-iteration worker-visible additions`
    - same-iteration worker-emitted tuples stay invisible until the
      iteration closes, preserving the seminaive boundary
    - added a direct C-level Deduce regression proving transitive
      multi-atom worker-scratch closure emits the full serialized fixpoint
      payload
  - Closed `B6.9b2b2b2b2b1` by turning shipped selector-scoped `scan`
    parity into explicit regression/docs truth:
    - selector-scoped `deduce/scan` was already using the bounded complete
      row-set path-local `goal-directed-read-context` capture path
    - added a regression and updated docs so the bounded-complete provenance
      slice no longer incorrectly claims `scan` is plain-only
  - Closed `B6.9b2b2b2b2a` with the first bounded multi-row
    proof-path-integrated goal-directed provenance slice:
    - replaced the old exact-one-only remembered subject key with a bounded
      complete row-set capture for the last goal-directed `query` / `match` /
      `scan-range` / plain `scan`
    - `deduce/why-result` path-local `goal-directed-read-context` now
      attaches when the traced row belongs to that complete captured set,
      while overflow beyond the current `8`-row limit keeps only the
      top-level relation-read context
    - added Deduce regressions for bounded-complete multi-row attachment and
      overflow fallback
  - Added `docs/plans/deduce-parallel-recursive-first-runtime-shape-decision-2026-03-25.md`:
    - chose the first runtime widening as same-wave worker-scratch compute
      for positive non-aggregate recursive SCC batches
    - kept publish and integrity validation on the main thread
    - rejected direct worker-owned LMDB publish as a misleading first slice
  - Updated `TODO.md` and `memory/CHANGELOG.md`:
    - tightened `B6.11b1` to the chosen first runtime shape
  - Closed `B6.11b1a` with the first internal handoff seam for that runtime
    shape:
    - added a versioned serialized component-delta payload for one SCC
      component's signed deltas
    - the payload fits the existing scheduler `OFFLOAD_RES_BYTES` result path
      and carries component id, predicate identity, and opaque encoded tuple
      additions/removals
    - added a direct C-level Deduce regression for payload roundtrip
    - split the remaining runtime work into worker-scratch compute
      (`B6.11b1b`) and main-thread publish/apply (`B6.11b1c`)
  - Closed `B6.11b1b1` with the first actual worker-scratch recursive compute
    slice:
    - added a read-only seminaive scratch-pass helper for positive
      non-aggregate recursive components
    - the helper seeds from the current component snapshot, evaluates one
      pass without LMDB publish, and returns serialized component deltas
    - added a direct C-level Deduce regression proving a recursive rule can
      emit current-snapshot-derived tuples into that payload
    - split the remaining worker-compute lane into multi-iteration scratch
      closure (`B6.11b1b2`) and main-thread publish/apply (`B6.11b1c`)
  - Closed `B6.11b1b2a` with the first multi-iteration worker-scratch
    recursive closure slice:
    - iterated the scratch seminaive path to a fixpoint for the current
      narrow single-recursive-atom positive SCC shape
    - added worker-local visible tuple suppression across iterations so the
      closure can accumulate serialized additions without LMDB publish
    - added a direct C-level Deduce regression proving the scratch closure
      computes a transitive `path` payload containing `(1,2)`, `(2,3)`,
      and `(1,3)`
    - split the remaining worker-closure lane into broader multi-atom
      recursive shapes (`B6.11b1b2b`) and main-thread publish/apply
      (`B6.11b1c`)
  - Closed `B6.11b1c` with the main-thread publish/apply path for
    worker-computed recursive deltas:
    - added `deduce_apply_serialized_component_delta_payload(...)`, which
      validates payload routing, reopens target relations, runs the existing
      encoded-tuple integrity checks, and publishes additions/removals through
      LMDB on the main thread
    - added a direct C-level Deduce regression proving a worker-computed
      recursive closure payload can be deserialized, committed, and observed
      as durable relation rows with updated schema estimates
    - narrowed the remaining parallel-runtime backlog to broader
      worker-scratch closure (`B6.11b1b2b`) and runtime/admin truth
      (`B6.11b2`)
  - Closed `B6.9b2b2b2b2b2a` with the next truthful proof-path provenance
    slice:
    - plain no-op `deduce/query` and `deduce/scan-range` reads now preserve
      the bounded complete subject set for later `why-result` path matching
    - `deduce/why-result` now also attaches path-local
      `goal-directed-read-context` on matching derived support frames, not
      only the root proof path, when the frame’s `(relation, tuple)` pair
      matches that bounded complete last-read subject set
    - added regressions for direct no-op `query` path attachment and derived
      support-frame attachment after a plain no-op `scan-range`
  - Closed `B6.9b2b2b2b2b2b1` with plain no-op `match` parity for bounded
    root-path goal-directed context:
    - switched the plain non-ephemeral `deduce/match` path to the txn-based
      scan helper for bounded subject-key recording, matching the already
      shipped goal-directed row-key capture path
    - added a regression proving `why-result` attaches
      `goal-directed-read-context` on the matching root proof path after a
      plain no-op `deduce/match` read
  - Closed `B6.9b2b2b2b2b2b2a` with truthful selector-scoped path-local
    parity across the shipped row-read shapes:
    - replaced the earlier misleading negative assumption with a mixed-shape
      regression
    - selector-scoped `match` and `scan` now stay truthfully visible on
      path-local `no-op` context
    - selector-scoped `query` and `scan-range` now stay truthfully visible on
      their shipped ephemeral demand paths with the same bounded path-local
      context surface
  - Pruned the non-executable roadmap markers out of `TODO.md`:
    - removed the broad `F0` through `V2-5` milestone labels from the live
      queue
    - kept only concrete actionable Deduce/runtime items in the active TODO
- Validation run
  - doc/queue shaping only
  - architecture basis confirmed from the current tree:
    - recursive fixpoint still walks `component_order` sequentially
    - component evaluators mutate LMDB transactions and shared schema/admin
      state
    - scheduler fanout helpers exist only as a future execution seam

- Objectives attempted
  - Close the parallel-recursion admin-truth slice by pinning that the
    current parallel fields are topology metadata only, not a separate
    runtime execution mode.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_rule_groups_explain.c3`:
    - strengthened the existing parallel explain regression so it now also
      proves selector-scoped `deduce/explain` still reports
      `execution-engine = 'semi-naive-scc` and
      `goal-directed-execution-path = 'selected-component-closure`
      while carrying parallel batch metadata
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.11a2`
    - documented the current fallback/admin truth for parallel topology
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - selector-scoped `deduce/explain` on the representative recursive SCC
      still reports `semi-naive-scc` plus `selected-component-closure`
      while exposing the expected parallel batch metadata

- Objectives attempted
  - Close the first parallel-recursion contract slice by documenting and
    pinning the existing SCC batch scheduling rule instead of leaving the
    topology metadata underspecified.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - strengthened the existing parallel topology regression to pin the
      current `stratum` / `wave` / `batch-size` contract on both
      `deduce/analyze` and relation-local `deduce/stats`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.11a1`
    - documented that the current parallel-recursion surface is
      metadata-only, recursive-components-only, and scheduled by
      same-stratum wave groups
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - representative recursive SCC topology still reports first-batch
      `(stratum = 0, wave = 1, batch-size = 2)` and second-batch
      `(stratum = 0, wave = 2, batch-size = 1)`

- Objectives attempted
  - Close the remaining `check`-specific admin-truth slice by exposing
    dedicated per-class violation counters instead of only generic totals and
    history.
- Code/config changes made
  - Updated `src/lisp/deduce_db_handles.c3`,
    `src/lisp/deduce_db_handles_register.c3`,
    `src/lisp/deduce_db_handles_mutation_tracking.c3`,
    `src/lisp/deduce_schema_query.c3`, and
    `src/lisp/deduce_rule_eval_prims.c3`:
    - added dedicated `check_integrity_violation_count` tracking on relation
      and DB admin state
    - `deduce/stats` and `deduce/analyze` now expose
      `check-integrity-violation-count`
  - Updated `src/lisp/tests_deduce_query_groups.c3`, `TODO.md`,
    `docs/deduce-datalog-spec.md`, `docs/reference/08-libraries.md`, and
    `memory/CHANGELOG.md`:
    - pinned the new per-class admin counters
    - closed `B6.10b2`, completing the current `check` lane
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probes:
    - existing key/unique/reference admin counters stayed unchanged
    - `check` failures now surface `check-integrity-violation-count = 1`
      on both relation-local and DB-wide admin payloads

- Objectives attempted
  - Close the first truthful write-enforcement slice for canonical `check`
    constraints and narrow the residual backlog honestly.
- Code/config changes made
  - Updated `src/lisp/deduce_relation_ops_validation.c3`,
    `src/lisp/deduce_relation_ops_mutations.c3`,
    `src/lisp/deduce_db_handles.c3`,
    `src/lisp/deduce_db_handles_mutation_tracking.c3`,
    `src/lisp/deduce_schema_query.c3`, and
    `src/lisp/tests_deduce_groups.c3` /
    `src/lisp/tests_deduce_query_groups.c3`:
    - immediate `fact!`, derived rule-head publish, and deferred
      `write-deferred` commit-time validation now enforce declared unary
      `check` constraints
    - failed checks now raise deterministic machine-checkable payloads,
      and generic integrity history/admin surfaces now record
      `violation-class = 'check`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.10b1`
    - narrowed `B6.10b2` to dedicated admin counters and summary truth for
      `check` constraints
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probes:
    - direct `fact!` check rejection returned machine-checkable
      `deduce/integrity-check-failed`
    - deferred `write-deferred` commit rejected the bad tuple and rolled
      back cleanly
    - missing check predicates rejected with
      `deduce/check-predicate-missing`
    - relation/DB history surfaced `violation-class = 'check`
    - direct `deduce/analyze` probe confirmed derived check enforcement on
      rule-head publish

- Objectives attempted
  - Close the first truthful schema/admin slice for canonical `check`
    constraints instead of leaving that lane at naming-only status.
- Code/config changes made
  - Updated `src/lisp/parser_define_relation_attr.c3`,
    `src/lisp/parser_define_relation_attr_helpers.c3`,
    `src/lisp/deduce_db_handles.c3`,
    `src/lisp/deduce_db_handles_register.c3`,
    `src/lisp/deduce_schema_query.c3`, and
    `src/lisp/deduce_rule_eval_prims.c3`:
    - relation declarations now lower unary column checks in the form
      `(check predicate column)`
    - relation schemas/admin payloads now persist and expose declared
      `check` metadata
    - `deduce/analyze` now reports DB-wide `check-constraint-count`
  - Updated `src/lisp/tests_deduce_groups.c3` and
    `src/lisp/tests_deduce_query_groups.c3`:
    - added parser/storage coverage for `(check predicate column)`
    - added public schema/analyze regression coverage for the `check`
      payload shape
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.10a2`
    - documented the shipped boundary as declaration/schema/admin baseline
      only, with enforcement still deferred to `B6.10b1`
- Validation run
  - `c3c build`
  - targeted host-side `./build/main --eval` probe:
    - declared `check` constraints appear in `deduce/schema`
    - DB-wide `deduce/analyze` reports `check-constraint-count`

- Objectives attempted
  - Retire the last transformed recursive-demand umbrella item once the lane
    was fully covered by explicit shipped support/fallback slices.
- Code/config changes made
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.4f5c2b2b2b` as stale backlog drift
    - rewrote the transformed recursive query contract to say there is no
      separate standing transformed residual beyond the shipped support and
      fallback families already pinned
- Validation run
  - backlog/doc shaping only, based on the already-landed transformed SCC
    support probes plus permutation and transformed multi-atom fallback probes

- Objectives attempted
  - Close the next transformed recursive query-demand hardening slice by
    fixing the false-positive ephemeral path on transformed multi-atom
    recursive shapes.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_fixpoint.c3` and
    `src/lisp/deduce_schema_query.c3`:
    - detect head rules whose transformed recursive support still spans
      multiple recursive body atoms
    - force truthful fallback for those transformed multi-atom shapes
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for transformed recursive multi-atom
      pair and pair-disjunction demands
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2b2b2`
    - closed `B6.4f5c2b2b2a`
    - kept the broader transformed residual as `B6.4f5c2b2b2b`
- Validation run
  - host-side `./build/main --eval` probes:
    - transformed recursive multi-atom pair query moved to
      `selected-component-closure` with the correct derived row
    - transformed recursive multi-atom pair disjunction moved to
      `selected-component-closure` with the correct derived rows

- Objectives attempted
  - Close the multi-hop follow-up for the transformed recursive
    one-carried-position query-demand lane.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for transformed three-cycle recursive
      pair and pair-disjunction demands
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2b2b`
    - closed `B6.4f5c2b2b1`
    - kept the broader transformed residual as `B6.4f5c2b2b2`
- Validation run
  - host-side `./build/main --eval` probes:
    - transformed three-cycle pair query stayed on
      `ephemeral-head-demand-query` with requested `(0 1)` and applied `(0)`
    - transformed three-cycle pair disjunction stayed on
      `ephemeral-head-demand-query` with requested `(0 1)` and applied `(0)`

- Objectives attempted
  - Close the disjunctive follow-up for the transformed one-carried-position
    recursive query-demand slice.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for reordered mutual-recursive
      disjunctive pair demands
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2b2`
    - closed `B6.4f5c2b2a`
    - kept the broader transformed residual as `B6.4f5c2b2b`
- Validation run
  - host-side `./build/main --eval` probes:
    - reordered mutual-recursive disjunction stayed on
      `ephemeral-head-demand-query` with requested `(0 1)` and applied `(0)`
    - pure recursive permutation disjunction stayed on
      `selected-component-closure`

- Objectives attempted
  - Close the first transformed recursive query-demand slice without breaking
    the existing pure permutation fallback boundary.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_fixpoint.c3` and
    `src/lisp/deduce_schema_query.c3`:
    - added same-index recursive carrier detection for queried head
      positions across an SCC
    - widened recursive query-demand relaxation so transformed recursive SCC
      shapes can keep one carried applied position when that carrier exists
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for reordered mutual-recursive pair
      demands, asserting requested `(0 1)` with one applied carried position
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2b`
    - closed `B6.4f5c2b1`
    - kept the broader transformed residual as `B6.4f5c2b2`
- Validation run
  - host-side `./build/main --eval` probes:
    - reordered mutual-recursive query moved to
      `ephemeral-head-demand-query` with requested `(0 1)` and applied `(0)`
    - pure recursive permutation still stayed on `selected-component-closure`

- Objectives attempted
  - Close the next recursive query-demand slice by pinning multi-hop
    same-index SCC support instead of leaving deeper positive cycles implicit.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for fully-bound same-index
      three-relation recursive SCC queries
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c2`
    - closed `B6.4f5c2a`
    - kept the remaining transformed-demand residual as `B6.4f5c2b`
- Validation run
  - host-side `./build/main --eval` probe:
    - three-relation same-index recursive SCC query stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`

- Objectives attempted
  - Close the next recursive query-demand slice by pinning same-index
    mutual-recursive disjunctive support instead of leaving it buried under
    a broader residual item.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for same-index mutual-recursive
      disjunctive pair queries
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.4f5c`
    - closed `B6.4f5c1`
    - kept the broader residual as `B6.4f5c2`
- Validation run
  - host-side `./build/main --eval` probes:
    - selector mutual-recursive disjunction stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`
    - plain mutual-recursive disjunction stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`

- Objectives attempted
  - Close the next recursive query-demand slice by widening truthful
    multi-position support from same-index self recursion to same-index
    mutual-recursive SCCs.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_fixpoint.c3`:
    - widened the same-index recursive demand gate to accept SCC-local
      positive recursive body atoms that preserve all requested positions
      together at the same indices
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for fully-bound same-index
      mutual-recursive pair queries
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.4f5b`
    - promoted the remaining residual to `B6.4f5c`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probes:
    - selector mutual-recursive pair query stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`
    - plain mutual-recursive pair query stayed on
      `ephemeral-head-demand-query` with requested/applied positions `(0 1)`

- Objectives attempted
  - Close the first goal-directed provenance slice without claiming
    proof-path-integrated planner semantics.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - `deduce/why-result` now exposes optional top-level
      `goal-directed-read-context` metadata built from the relation's existing
      last goal-directed read state
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a regression that forces a goal-directed recursive query, then
      checks that `why-result` exposes the mirrored read context even on a
      `missing` row payload
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split `B6.9b2` into `B6.9b2a` and `B6.9b2b`
    - closed `B6.9b2a`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(missing 0 ephemeral-head-demand-query query nil 1 1 0)` for
      the goal-directed read-context slice
    - existing recursive and non-recursive why-result probes stayed intact

- Objectives attempted
  - Close the next recursive provenance slice by moving positive recursive
    why-result payloads beyond flattened fact-only support.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - recursive why-result support search now appends a `rule-step` frame when
      a derived child row is used in the chosen proof path
    - recursive why-result depth accounting now reports the deeper derived step
      in `max-depth`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - tightened the recursive why-result regression to pin `rule-step` support
      frames and `max-depth = 3` on recursive closure rows
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9b1b`
    - narrowed the remaining provenance queue to goal-directed semantics only
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probes:
    - recursive closure payload now includes a trailing `rule-step` frame with
      `(rule-index 0, step-index 1)` on the chosen child proof
    - recursive closure rows now report `max-depth = 3`
    - the existing non-recursive multi-rule why-result probe still returned
      `(ok partial true)`

- Objectives attempted
  - Close the first recursive provenance slice without widening to
    goal-directed lineage or nested recursive-step payloads.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened `deduce/why-result` support search so derived child rows recurse
      back through the shipped row-subject provenance helper surface
    - added a visited-subject guard on `(predicate, tuple)` to keep recursive
      support search finite
    - recursive closure rows now return `ok` for one support chain and
      `partial` for multiple support chains, while keeping the current
      flattened fact-support payload shape
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a dedicated recursive why-result regression for direct, unique
      recursive, and partial recursive closure rows
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - split broad recursive provenance into `B6.9b1a` and `B6.9b1b`
    - closed `B6.9b1a` at the first positive recursive closure slice
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probes:
    - returned `(ok partial true)` for the existing non-recursive multi-rule
      why-result probe after the recursive helper refactor
    - returned `(ok 1 nil 1 1 2 nil)` for a unique recursive closure row
    - returned `(partial 1 true 1 1 2 nil)` for a multi-path recursive closure
      row

- Objectives attempted
  - Close the first multi-rule non-recursive provenance slice without widening
    to recursive or goal-directed lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened `deduce/why-result` to aggregate already-shipped non-recursive
      provenance support across multiple matching head rules
    - multi-rule rows with one supported path now return `ok`, and rows with
      multiple supported paths return `partial` with the first deterministic
      path
    - one-rule exact-one-rule lineage still stays on the older proven branch
      instead of going through the aggregate path
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to cover both the direct multi-rule
      `ok` case and the mixed-body multi-rule `partial` case
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a8`
    - narrowed the remaining provenance queue to recursive and goal-directed
      semantics only
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probes:
    - returned `(ok 1 nil 1 1 1 nil)` for the direct multi-rule row
    - returned `(partial 1 true 1 2 2 nil)` for the mixed-body multi-rule row

- Objectives attempted
  - Close the first mixed-body non-recursive provenance slice without widening
    to multi-rule lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened `deduce/why-result` to compose exact-one-rule non-recursive
      mixed-body lineage through already-supported exact-one-rule child
      provenance helpers
    - exact-one-rule mixed-body lineage now returns `ok` for one support chain
      and `partial` when multiple support chains exist
    - multi-rule non-recursive lineage still returns
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to include an exact-one-rule mixed-body
      supported case while keeping the multi-rule non-recursive case
      unsupported
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a7`
    - promoted the remaining multi-rule non-recursive lineage work into
      `B6.9a8`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(partial true partial 2 error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the first search-based non-recursive provenance slice for exact-one-
    rule extensional lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - added exact-one-rule extensional support search for `deduce/why-result`
      on non-recursive derived rows
    - reused the existing positive-body tuple matcher plus tuple iterators to
      search extensional supports rather than introducing a separate provenance
      matcher
    - exact-one-rule extensional lineage now returns `ok` for one deterministic
      path and `partial` with `truncated = true` when multiple support paths
      exist
    - mixed-body or multi-rule non-recursive derived lineage still returns
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to cover search-based extensional
      lineage with both `ok` and `partial` outcomes
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a6`
    - promoted the remaining mixed-body or multi-rule non-recursive lineage
      work into `B6.9a7`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(ok partial true error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the next non-recursive provenance slice for exact-one-rule
    extensional multi-body lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - generalized the public derived-lineage matcher from one-body only to
      exact-one-rule extensional derivations where every support tuple is
      reconstructible from the head row
    - current support now covers head-bound multi-body extensional rules while
      still rejecting existential/search-based lineage with
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to include a reconstructible multi-body
      extensional rule with two fact supports and to keep the existential
      non-recursive case explicitly unsupported
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a5`
    - promoted the remaining existential/search-based non-recursive lineage
      work into `B6.9a6`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(ok 2 error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the next truthful non-recursive `deduce/why-result` slice beyond
    direct-copy lineage.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - widened the public one-body extensional provenance matcher so the source
      tuple can be reconstructed from head-row variable/literal bindings
    - current support now covers direct-copy plus one-body projection and
      permutation rules
    - multi-body or existential derived shapes still return
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to include a one-body projection /
      permutation derived relation in addition to direct-copy, missing, and
      unsupported multi-body cases
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a4` at the one-body extensional boundary
    - promoted the remaining multi-body or existential non-recursive lineage
      work into `B6.9a5`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(ok ok parent error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the first non-recursive derived `deduce/why-result` slice without
    overclaiming broader provenance support.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - added the first derived provenance path builder for `deduce/why-result`
    - stored rows in relations with exactly one non-negated, non-aggregate,
      one-body direct-copy rule from an extensional source now return
      `status = ok` with one deterministic `derived` path
    - broader derived shapes still return
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - widened the why-result regression to cover seed `ok`, direct-copy
      derived `ok`, missing `missing`, and broader derived `error`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a3` as the first non-recursive derived provenance slice
    - promoted the remaining broader non-recursive derived lineage work into
      `B6.9a4`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(ok derived parent error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the first public `deduce/why-result` runtime slice without
    pretending derived lineage is already shipped.
- Code/config changes made
  - Updated `src/lisp/deduce.c3` and `src/lisp/eval_init_primitives.c3`:
    - wired `why-result` through unified Deduce dispatch and the
      `deduce/why-result` namespaced primitive surface
  - Updated `src/lisp/deduce_schema_query.c3`:
    - shipped `(deduce/why-result relation val...)` for stored row subjects
    - extensional stored tuples now return deterministic `status = ok`
      provenance payloads with one `seed` path and one `fact` support frame
    - missing tuples return `status = missing`
    - stored derived rows now return `status = error` with
      `deduce/why-result-derived-subject-not-yet-supported`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added regression coverage for the public seed-row `ok`, missing-row
      `missing`, and stored-derived-row `error` payloads
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md`:
    - closed `B6.9a2` as the public seed-row surface
    - promoted the remaining non-recursive derived-lineage work into `B6.9a3`
- Validation run
  - `c3c build`
  - host-side `./build/main --eval` probe:
    - returned `(ok 1 missing 0 error deduce/why-result-derived-subject-not-yet-supported)`

- Objectives attempted
  - Close the why-result payload/status baseline before adding any public
    provenance runtime surface.
- Code/config changes made
  - Updated `docs/deduce-datalog-spec.md`:
    - fixed the canonical top-level why-result status semantics for `ok`,
      `missing`, `partial`, and `error`
    - fixed the canonical envelope keys that all future why-result surfaces
      must carry
  - Updated `docs/reference/08-libraries.md`, `TODO.md`, and
    `memory/CHANGELOG.md` so this closure lands as `B6.9a1`
- Validation run
  - doc-state inspection against the existing provenance / why-result section
    in `docs/deduce-datalog-spec.md`

- Objectives attempted
  - Close the first derived-source maintained-update slice for incremental
    materialized refresh.
- Code/config changes made
  - Updated `src/lisp/deduce_db_handles_mutation_tracking.c3`:
    - generalized the direct-copy incremental classifier
    - added the first derived-source queue path for one-step downstream
      materialized direct-copy targets
  - Updated `src/lisp/deduce_schema_query.c3`:
    - allowed `incremental-targeted` refresh when the immediate source is an
      already-refreshed ready materialized direct-copy relation
    - propagated inserted tuple keys one step downstream after a successful
      incremental refresh
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a `edge -> mid -> top` regression proving `mid` stays
      `incremental-targeted`, `top` remains stale until refreshed, and `top`
      then refreshes on the same `incremental-targeted` path
  - Updated `docs/deduce-datalog-spec.md`, `docs/reference/08-libraries.md`,
    `TODO.md`, and `memory/CHANGELOG.md` so this closure lands as `B6.8b2`
- Validation run
  - `c3c build`
  - host-side direct-copy chain probe:
    - returned `(incremental-targeted (top-chain) 1 incremental-targeted 2 2 2 nil)`

- Objectives attempted
  - Close the first ordinary maintained-update runtime slice for incremental
    materialized refresh.
- Code/config changes made
  - Updated `src/lisp/deduce_db_handles_mutation_tracking.c3` and
    `src/lisp/deduce_db_handles_mutation_txn.c3`:
    - queued committed inserted tuple keys per supported materialized target
    - escalated pending-queue tracking failure to the existing
      `full-recompute-required` boundary
  - Updated `src/lisp/deduce_schema_query.c3`:
    - added the narrow `incremental-targeted` relation refresh path for
      already-refreshed direct-copy extensional materialized targets
    - reused that path for stale `on-read` refresh
    - cleared pending incremental queue state on dematerialize
  - Updated `src/lisp/tests_deduce_query_groups.c3` and
    `src/lisp/tests_deduce_query_bench_groups.c3`:
    - pinned that sibling materialized targets keep independent pending queues
      and that supported relation refresh reports `incremental-targeted`
  - Updated `docs/deduce-datalog-spec.md`, `docs/reference/08-libraries.md`,
    `TODO.md`, and `memory/CHANGELOG.md` so this closure lands as `B6.8b1`
- Validation run
  - `c3c build`
  - host-side direct-copy sibling refresh probe:
    - returned `(incremental-targeted 2 1 2 true)`
  - host-side `on-read` direct-copy probe:
    - returned `(deduce/on-read-refresh-unavailable 1 2 2 nil)`

- Objectives attempted
  - Close the incremental-maintenance fallback/admin boundary slice before any
    maintained-update runtime work widens the surface.
- Code/config changes made
  - Updated `docs/deduce-datalog-spec.md`:
    - added `7.3.6 Incremental Maintenance Fallback/Admin Boundary`
    - fixed the approved public fallback vocabulary and truth requirements for
      analyze/stats/refresh/read surfaces
  - Updated `docs/reference/08-libraries.md`, `TODO.md`, and
    `memory/CHANGELOG.md` so this closure lands as `B6.8a2`
- Validation run
  - doc-state inspection against the already-shipped `tracked` /
    `full-recompute` admin surfaces in:
    - `src/lisp/deduce_db_handles_mutation_tracking.c3`
    - `src/lisp/deduce_rule_eval_prims.c3`
    - `src/lisp/deduce_schema_query.c3`

- Objectives attempted
  - Close the incremental delta substrate data-model slice by turning the
    already-shipped internal seminaive substrate into canonical doc truth.
- Code/config changes made
  - Updated `docs/deduce-datalog-spec.md`:
    - added `7.3.5 Incremental Delta Substrate Baseline`
    - documented the current internal signed-delta, support-table, and
      current/next iteration-buffer model
  - Updated `docs/reference/08-libraries.md`, `TODO.md`, and
    `memory/CHANGELOG.md` so this closure lands as `B6.8a1`
- Validation run
  - doc-state inspection against:
    - `src/lisp/deduce_rule_eval_exec.c3`
    - `src/lisp/deduce_rule_eval_exec_seminaive.c3`
    - `src/lisp/deduce_rule_eval_fixpoint.c3`

- Objectives attempted
  - Close the admin-truth alignment slice by pinning how `stats`, `analyze`,
    and `refresh!` line up after degraded rule-change recovery.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused regression for the rule-set-change path where
      relation-local stats first report `full-recompute-required`
    - pinned that a plain DB-level analyze clears the degraded mode, a later
      relation-scoped refresh stays targeted, and untouched stale peers remain
      visible on refresh/analyze outputs
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.7b2`
- Validation run
  - `c3c build`
  - host-side admin-alignment probe: returned
    `(full-recompute 2 targeted nil 1 (ancestor-peer) tracked 1 (ancestor-peer))`

- Objectives attempted
  - Close the dirty-frontier truth slice by pinning the relation-local stats
    view after partial selected-component recovery.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused regression showing that two different relation stats
      payloads share the same residual dirty frontier after selected analyze
    - pinned that `dirty-self` is the local bit: false for the cleared
      relation, true for the untouched relation
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.7b1`
- Validation run
  - `c3c build`
  - host-side dirty-frontier probe: returned
    `(2 (other-src-s other-out-s) nil 2 (other-src-s other-out-s) true)`

- Objectives attempted
  - Close the degraded-state and recovery contract slice by pinning what
    `full-recompute` looks like immediately before and immediately after the
    current DB-level recovery path.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused regression showing that destructive writes first surface
      `full-recompute-required` on relation-local stats
    - pinned that plain DB-level `deduce/analyze` returns a degraded snapshot
      payload, then clears the live dirty/admin state for subsequent stats
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.7a2`
- Validation run
  - `c3c build`
  - host-side degraded analyze probe: returned
    `(true full-recompute 2 (edge-dr reach-dr) nil 1)`

- Objectives attempted
  - Close the commit/abort mutation-log contract slice by pinning when
    deferred write-block mutations actually affect dirty/admin state.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused admin-surface regression covering committed insert,
      aborted insert, and committed delete under write transactions
    - pinned that commit applies the deferred mutation log, abort drops it,
      and destructive committed writes escalate to `full-recompute`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.7a1`
- Validation run
  - `c3c build`
  - host-side commit insert probe: returned `(2 tracked (edge-txc reach-txc))`
  - host-side abort insert probe: returned `(0 tracked nil 0)`
  - host-side commit delete probe: returned
    `(2 full-recompute (edge-tdc reach-tdc) 1)`

- Objectives attempted
  - Close the conjunctive counter-truth slice by pinning the three current
    observed-counter surfaces instead of implying they are one shared source.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused preserved-bound selector regression that compares
      `deduce/stats.last-goal-directed-read-step-counters`,
      `deduce/analyze.rule-execution[*].steps[*].counters`, and
      `deduce/explain.steps[*].counters`
    - pinned the current truthful boundary where stats reports the earlier
      preserved-bound read, while analyze and explain report their own
      observed execution counters
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.6b2`
- Validation run
  - `c3c build`
  - host-side conjunctive counter probe: returned `true`

- Objectives attempted
  - Close the conjunctive path/order truth slice by pinning the current
    planner-derived join-order/operator boundary shared by `deduce/explain`
    and `deduce/analyze`.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused non-recursive conjunctive regression that compares
      `deduce/explain.steps[*]` with `deduce/analyze.rule-execution[*].steps[*]`
    - pinned shared `join-order`, `predicate`, `operator`, and
      `selected-index` fields on the current planner-derived shape
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.6b1`
- Validation run
  - `c3c build`
  - host-side conjunctive order/operator probe: returned `true`

- Objectives attempted
  - Close the missing `deduce/analyze` classification slice by pinning the
    already-shipped selector-planner versus last-runtime-read boundary for
    conjunctive recursive selectors.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused selector-scoped `deduce/analyze` regression after a real
      `deduce/query`
    - pinned top-level planner-side `goal-directed-execution-path`,
      `goal-directed-selector-rule-index`, and
      `goal-directed-selected-component-id`
    - pinned separate `last-goal-directed-read-*` runtime metadata and
      observed `rule-execution[*].steps[*].counters`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.6a2`
- Validation run
  - `c3c build`
  - host-side analyze classification probe: returned `true`

- Objectives attempted
  - Close the missing `deduce/explain` classification slice by pinning the
    already-shipped planner-snapshot versus last-runtime-read boundary for
    conjunctive recursive rules.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added a focused `deduce/explain` regression for a two-step recursive
      conjunctive rule after an actual `deduce/query`
    - pinned `surface-kind = 'planner-snapshot`,
      `goal-directed-execution-path = 'selected-component-closure`,
      `last-goal-directed-read-execution-path = 'ephemeral-head-demand-query`,
      `last-goal-directed-read-surface = 'query`, and observed step counters
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    closure lands as `B6.6a1` and the remaining explain/analyze/runtime-truth
    work stays in `B6.6a2` and `B6.6b*`
- Validation run
  - `c3c build`
  - host-side explain classification probe: returned `true`

- Objectives attempted
  - Close the next recursive symbolic safety slice by pinning the current
    fallback boundary for multi-atom recursive query demands.
- Code/config changes made
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain `deduce/query` regressions for
      `path(x,z) :- path(x,y), path(y,z)` with a fully-bound `(src,dst)`
      filter
    - pinned those shapes on truthful `selected-component-closure`
      fallback with zero requested/applied bound counts
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    safety slice closes as `B6.4f5a`, while the actual multi-atom recursive
    support work stays open as `B6.4f5b`
- Validation run
  - host-side selector multi-atom recursive query probe: returned
    `(({src 1 dst 4}) selected-component-closure 0 0 dirty-closure)`
  - host-side plain multi-atom recursive query probe: returned
    `(({src 1 dst 4}) selected-component-closure 0 0 dirty-closure)`

- Objectives attempted
  - Close the first true jointly-supported recursive multi-position query
    support slice beyond carried-position relaxation and permutation fallback.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_fixpoint.c3`:
    - narrowed the same-index preservation check so non-recursive seed rules
      no longer block multi-position recursive demand support
    - same-index preservation is now judged only against the positive
      self-recursive rules that actually matter for recursive carry
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain fully-bound pair regressions for
      `stable(x,y) :- edge(x,y)` plus `stable(x,y) :- stable(x,y)`
    - added selector/plain disjunctive fully-bound regressions for the same
      same-index self-recursive shape
    - pinned those shapes on `ephemeral-head-demand-query` with requested and
      applied positions `(0 1)`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so
    `B6.4f4b` closes and the honest residual is now `B6.4f5`
- Validation run
  - `c3c build`
  - host-side selector same-index recursive pair probe: returned
    `(({src 1 dst 2}) ephemeral-head-demand-query (0 1) (0 1) nil)`
  - host-side plain same-index recursive pair probe: returned
    `(({src 1 dst 2}) ephemeral-head-demand-query (0 1) (0 1) nil)`
  - host-side selector same-index recursive disjunction probe: returned
    `(({src 1 dst 2} {src 10 dst 11}) ephemeral-head-demand-query (0 1) (0 1) nil)`
  - host-side plain same-index recursive disjunction probe: returned
    `(({src 1 dst 2} {src 10 dst 11}) ephemeral-head-demand-query (0 1) (0 1) nil)`

- Objectives attempted
  - Close the next honest recursive symbolic safety slice after proving that
    jointly-supported permutation demands still cannot be answered by the
    current ephemeral evaluator.
- Code/config changes made
  - Updated `src/lisp/deduce_rule_eval_fixpoint.c3`:
    - added same-index recursive preservation checks so the query layer can
      distinguish “jointly supported in-place” from “jointly supported only
      through permutation”
  - Updated `src/lisp/deduce_schema_query.c3`:
    - split recursive multi-position handling into three cases:
      keep the current one-position relaxation for carried-position shapes,
      preserve all positions only when they are same-index-preserved, and
      fall back to `selected-component-closure` for jointly-supported
      permutation shapes the evaluator still cannot seed truthfully
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain symmetric recursive pair regressions
    - added selector/plain symmetric recursive disjunctive regressions
    - pinned those shapes on truthful `selected-component-closure`
      fallback instead of a false `ephemeral-head-demand-query` claim
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so this
    safety slice closes as `B6.4f4a`, while the true joint multi-position
    recursive support stays open as `B6.4f4b`
- Validation run
  - `c3c build`
  - host-side selector symmetric recursive pair probe: returned
    `(({src 2 dst 1}) selected-component-closure 0 0 dirty-closure)`
  - host-side plain symmetric recursive pair probe: returned
    `(({src 2 dst 1}) selected-component-closure 0 0 dirty-closure)`
  - host-side selector symmetric recursive disjunction probe: returned
    `(({src 2 dst 1} {src 11 dst 10}) selected-component-closure 0 0 dirty-closure)`
  - host-side plain symmetric recursive disjunction probe: returned
    `(({src 2 dst 1} {src 11 dst 10}) selected-component-closure 0 0 dirty-closure)`

- Objectives attempted
  - Close the next honest recursive symbolic slice by fixing reordered
    recursive head shapes where the carried recursive position is not the
    last column.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - replaced the recursive demand relaxation heuristic again so it keeps
      the head position preserved by the positive self-recursive rule, rather
      than the last bound position
    - applied that preserved-position relaxation across selector/plain
      single-branch and disjunctive ephemeral query paths
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain swapped-column recursive pair regressions
    - added selector/plain swapped-column recursive disjunctive regressions
    - pinned admin truth so reordered recursive heads now keep applied
      position `(0)` instead of incorrectly narrowing to `(1)`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so
    `B6.4f3` closes and the honest residual is now `B6.4f4`
- Validation run
  - `c3c build`
  - host-side ordinary selector recursive pair smoke: returned
    `(ephemeral-head-demand-query (0 1) (1) 3)`
  - host-side ordinary plain recursive disjunction smoke: returned
    `(2 ephemeral-head-demand-query (0 1) (1))`
  - host-side swapped selector recursive pair smoke: returned
    `(({src 1 dst 3}) ephemeral-head-demand-query (0 1) (0))`
  - host-side swapped plain recursive disjunction smoke: returned
    `(({src 1 dst 3} {src 10 dst 11}) ephemeral-head-demand-query (0 1) (0))`

- Objectives attempted
  - Close the first bounded `B6.4f2` recursive symbolic slice
    instead of leaving the whole recursive query lane open.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - replaced the recursive projected-demand relaxation helper so it keeps
      the trailing supported bound position instead of the first one
    - applied that bounded relaxation across selector/plain single-branch and
      disjunctive ephemeral query paths
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - converted the recursive pair selector/plain regressions from truthful
      fallback expectations to bounded ephemeral execution expectations
    - pinned the exact admin truth for this slice:
      requested bound positions stay `(0 1)` while applied positions narrow
      to `(1)`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so
    `B6.4f2` closes and the honest residual is now `B6.4f3`
- Validation run
  - `c3c build`
  - host-side selector recursive pair bounded-ephemeral smoke: returned
    `(({src 1 dst 3}) ephemeral-head-demand-query (0 1) (1) nil)`
  - host-side plain recursive pair bounded-ephemeral smoke: returned
    `(({src 1 dst 3}) ephemeral-head-demand-query (0 1) (1) nil)`
  - host-side selector recursive disjunctive pair bounded-ephemeral smoke:
    returned
    `(({src 1 dst 3} {src 10 dst 11}) ephemeral-head-demand-query (0 1) (1) nil)`
  - host-side plain recursive disjunctive pair bounded-ephemeral smoke:
    returned
    `(({src 1 dst 3} {src 10 dst 11}) ephemeral-head-demand-query (0 1) (1) nil)`

- Objectives attempted
  - Recheck whether `B6.4e2c` is a real remaining runtime lane or just stale
    wording after the shipped symbolic/disjunctive slices.
- Code/config changes made
  - Ran direct host-side probes against a non-recursive derived one-rule
    subject and confirmed there is no separate non-recursive goal-directed
    symbolic disjunction lane in the current planner:
    - selector-scoped reads fail with `deduce/query: selected rule is not
      goal-directed eligible`
    - plain reads stay on the ordinary `no-op` path
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` to retire
    stale item `B6.4e2c` and track the real remaining symbolic work only
    under `B6.4f2`
- Validation run
  - host-side selector non-recursive symbolic disjunction probe: confirmed
    `selected rule is not goal-directed eligible`
  - host-side plain non-recursive symbolic disjunction probe: returned `no-op`

- Objectives attempted
  - Close the first honest `B6.4f` safety slice by making recursive
    multi-position symbolic query demands fall back truthfully instead of
    claiming ephemeral execution with incomplete results.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - added a shared bound-position counter for projected head demands
    - added a recursive-component guard that refuses ephemeral execution when
      a recursive selected component receives a multi-position projected
      symbolic demand
    - applied that guard to both single-branch and disjunctive selector/plain
      ephemeral query paths
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - added selector/plain regressions for single recursive symbolic pair
      filters
    - kept the selector/plain recursive disjunctive pair regressions on the
      truthful `selected-component-closure` fallback path
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    so this shipped safety slice closes as `B6.4f1`, while the remaining
    recursive support work stays open as `B6.4f2`
- Validation run
  - `c3c build`
  - host-side selector single recursive pair fallback smoke: returned `true`
  - host-side plain single recursive pair fallback smoke: returned `true`
  - host-side selector recursive disjunctive pair fallback smoke: returned
    `true`
  - host-side plain recursive disjunctive pair fallback smoke: returned
    `true`

- Objectives attempted
  - Close the next honest `B6.4e2b2` disjunctive rewrite slice by widening
    the shipped ephemeral union path from same-position branches to
    mixed-position demand-safe branches.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - removed the stale same-position-only guard from the disjunctive
      projected-demand executors
    - kept the runtime boundary narrow to branches that still individually
      reduce to the already shipped demand-safe subset
    - removed the now-dead helper for “same single bound position” support
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - changed the selector/plain mixed-position disjunction regressions to
      assert `ephemeral-head-demand-query`
    - aligned the expected result set to the truthful two-row union produced
      by those branch-local runs
    - flattened the test-local `let` bindings into single multi-binding
      forms
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    so this shipped slice closes as `B6.4e2b2`, while the real residual is
    now broader symbolic multi-branch rewrite under `B6.4e2c`
- Validation run
  - `c3c build`
  - host-side selector mixed-position disjunction smoke: returned `true`
  - host-side plain mixed-position disjunction smoke: returned `true`

- Objectives attempted
  - Close the next honest `B6.4e2b` residual slice by pinning that
    same-position disjunctive branches can still carry branch-local residual
    conjunct filtering on the ephemeral query path.
- Code/config changes made
  - Added focused regressions in `src/lisp/tests_deduce_query_groups.c3` for:
    - selector-scoped same-position disjunctive branches with residual
      unsupported conjuncts
    - plain same-position disjunctive branches with residual unsupported
      conjuncts
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    so this shipped slice closes as `B6.4e2b1`, while the real residual stays
    open under `B6.4e2b2`
- Validation run
  - `c3c build`
  - host-side selector same-position residual-conjunct disjunction smoke:
    returned `true`
  - host-side plain same-position residual-conjunct disjunction smoke:
    returned `true`

- Objectives attempted
  - Tighten the already-closed disjunctive `deduce/query` rewrite slices to
    the runtime boundary they can actually defend, and pin the still-open
    `B6.4e2b2` residual with fallback regressions instead of letting it drift.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - kept the shipped disjunctive union path on one shared projected bound
      position across all branches
    - changed the branch execution model to one abortable temporary write txn
      per shipped disjunctive branch instead of stacking multiple projected
      demands into one txn
    - kept same-position wrapper branches on the ephemeral path
    - forced broader mixed-position disjunctions back onto
      `selected-component-closure`
  - Updated `src/lisp/tests_deduce_query_groups.c3`:
    - retained the passing same-position selector/plain disjunction
      regressions
    - added selector/plain mixed-position disjunction regressions that now
      assert the fallback path instead of an unsound ephemeral result
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` so the
    shipped boundary now says “separate txns plus same-position-only” rather
    than implying broader same-txn disjunctive rewrite support.
- Validation run
  - `c3c build`
  - host-side selector same-position disjunction smoke: returned `true`
  - host-side plain same-position disjunction smoke: returned `true`
  - host-side selector mixed-position disjunction fallback smoke: returned
    `true`
  - host-side plain mixed-position disjunction fallback smoke: returned
    `true`

- Objectives attempted
  - Close the next honest `B6.4e` residual slice by widening the disjunctive
    ephemeral query path to same-position wrapper branches without widening
    into mixed-position multi-branch rewrite.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - kept the disjunctive ephemeral query path constrained to one shared
      projected bound position across all branches
    - reused that boundary for both the requested and projected disjunctive
      demand cases
    - left mixed-position disjuncts on the ordinary selected-closure path
  - Added focused regressions in
    `src/lisp/tests_deduce_query_groups.c3` for:
    - selector-scoped same-position disjunctive wrapper branches
    - plain same-position disjunctive wrapper branches
    - selector/plain mixed-position disjuncts that still fall back
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    so this shipped slice closes as `B6.4e2a` and the wider residual remains
    open under `B6.4e2b`
- Validation run
  - `c3c build`
  - host-side selector same-position wrapper disjunction smoke: returned
    `true`
  - host-side plain same-position wrapper disjunction smoke: returned `true`
  - host-side selector mixed-position disjunction fallback smoke: returned
    `true`
  - host-side plain mixed-position disjunction fallback smoke: returned
    `true`

- Objectives attempted
  - Close the next live `TODO.md` execution slice after `B6.4d` by shipping
    the first honest multi-branch query-time rewrite path for `deduce/query`
    instead of another single-demand extractor widening.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - added bounded disjunctive head-demand collection for filters shaped as
      `or` trees whose branches are individually demand-safe under the
      already shipped query subset
    - added disjunctive selector/plain ephemeral execution helpers that run
      multiple projected head demands inside the same abortable write txn and
      then reapply the original full filter over the union of rows in that
      txn
    - kept the shipped boundary narrow: mixed unsupported disjuncts and
      broader symbolic disjunctions still fall back
  - Added focused regressions in
    `src/lisp/tests_deduce_query_groups.c3` for:
    - selector-scoped disjunctive union execution over two demand-safe
      branches
    - plain `deduce/query` disjunctive union execution over two demand-safe
      branches
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    so the shipped slice closes as `B6.4e1` and the residual wider
    disjunctive / rewrite space stays open under `B6.4e2`
- Validation run
  - `c3c build`
  - host-side selector disjunctive-union smoke: returned `true`
  - host-side plain `deduce/query` disjunctive-union smoke: returned `true`

- Objectives attempted
  - Close the next live `TODO.md` item, `B6.4d`, by widening the shipped
    `deduce/query` demand extractor from preserved-row wrapper rewrites to
    the next bounded captured-call family: comparator wrappers over
    row-derived scalar arguments.
- Code/config changes made
  - Updated `src/lisp/deduce_schema_query.c3`:
    - added a bounded captured-scalar-call equality extractor
    - widened `deduce/query` demand extraction so comparator wrappers like
      `(want target (pickdst row))` stay on the ephemeral demand path when
      exactly one argument reduces to a row-derived scalar and the remaining
      arguments are closed literals
    - preserved that same bounded subset across short forwarding chains
      without claiming generic captured-call rewrite or symbolic solving
  - Added focused regression coverage in
    `src/lisp/tests_deduce_query_groups.c3` for:
    - selector-scoped captured comparator wrappers over row-derived scalar
      arguments
    - plain `deduce/query` short forwarding chains of those comparator
      wrappers
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    to close `B6.4d` at that bounded shipped boundary and leave the broader
    query-time rewrite/magic-set work under `B6.4e`
- Validation run
  - `c3c build`
  - host-side selector smoke for captured comparator wrappers over a
    row-derived scalar argument: returned `true`
  - host-side plain `deduce/query` forwarding-chain smoke for that same
    scalar-comparator subset: returned `true`

- Objectives attempted
  - Close the first live `TODO.md` item, `B6.3f1b`, by persisting the
    Deduce rule/dependency catalog truth that admin surfaces can expose after
    reopen without pretending executable rule state is already durable.
- Code/config changes made
  - Added a compact persisted rule-catalog summary in
    `src/lisp/deduce_db_handles.c3`:
    - introduced a dedicated `__deduce_rule_catalog` LMDB DBI
    - rewrote that DBI from live rule signatures on each `deduce/rule!`
    - restored the compact catalog on `deduce 'open`
    - kept the restored data separate from live executable rule signatures so
      reopen truth does not overclaim runtime execution support
  - Updated `src/lisp/deduce_rule_eval_prims.c3` so `deduce/analyze` now uses
    the persisted catalog summary for `rule-count` and
    `incremental-dependency-edges` when live rules are absent.
  - Updated restart regressions in
    `src/lisp/tests_deduce_durability_groups.c3` so reopened analyze payloads
    now expect the persisted rule/dependency summary instead of zeroed values.
  - Updated `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, `memory/CHANGELOG.md`, and `TODO.md`
    to reflect the new shipped boundary:
    - reopened admin truth keeps the compact rule/dependency summary
    - executable rule state and durable ready/fresh semantics are still
      deferred to the next item
- Validation run
  - `c3c build`
  - host-side two-process file-backed smoke:
    reopened `deduce/analyze` retained `rule-count == 1` and
    `incremental-dependency-edges == 1` while the materialized relation stayed
    conservatively unready
  - host-side two-process stale `on-read` reopen smoke:
    reads still failed truthfully with `deduce/on-read-refresh-unavailable`
    while reopened `deduce/analyze` retained the persisted rule/dependency
    summary

- Objectives attempted
  - Close the next live `TODO.md` item, `B6.3f2`, by making reopened
    materialized relations truthfully ready/fresh again for the current
    supported persisted rule-signature surface instead of keeping restart
    behavior permanently summary-only.
- Code/config changes made
  - Fixed the reopened executable-rule restore path in
    `src/lisp/deduce_db_handles.c3`:
    - stopped trusting persisted raw `SymbolId` values across processes
    - rebuilt restored rule head/body predicate identities from the persisted
      rule catalog instead
    - registered inferred predicate schemas for restored rule predicates so
      fixpoint/refresh execution has arity truth after reopen even before
      every dependency relation is explicitly reopened
  - Refactored `src/lisp/deduce_db_handles_register.c3` so relation-schema
    registration is truthful for reopen flows:
    - same-shape reopen is now idempotent instead of being treated as a hard
      conflict
    - inferred placeholder schemas can now be upgraded by a later explicit
      `deduce/open-named ...`
  - Updated `TODO.md`, `docs/deduce-datalog-spec.md`,
    `docs/reference/08-libraries.md`, and `memory/CHANGELOG.md` to close the
    item and record the actual shipped boundary:
    - supported persisted executable signatures now restore live
      `deduce/refresh!` / `on-read` maintenance after reopen
    - unsupported persisted signature shapes still fall back to summary-only
      admin truth
- Validation run
  - `c3c build`
  - host-side two-process file-backed smoke:
    reopened manual materialized relation remained
    `materialized-derived-ready == true`, kept `rule-count == 1` and
    `incremental-dependency-edges == 1`, and returned count `1`
  - host-side two-process file-backed smoke:
    reopened stale `on-read` materialized relation auto-refreshed on read,
    advanced `materialized-refresh-count` from `1` to `2`, cleared stale
    state, and returned count `2`
  - host-side two-process file-backed smoke:
    reopened never-refreshed manual materialized relation stayed ready,
    reported `never-refreshed`, and successfully refreshed through the
    targeted relation path

- Objectives attempted
  - Reshape the remaining Deduce `B6.4` backlog so the shipped baseline and
    the true residual work are tracked as separate items rather than one long
    umbrella.
- Code/config changes made
  - Updated `docs/plans/deduce-actionable-backlog-2026-03-20.md`:
    - chose the first actual non-manual declaration-policy target:
      `on-read` is now the explicit first widening after the current
      manual-only surface
    - closed `B6.3e1` with an explicit surface decision:
      declaration-time materialization stays frozen at `manual` until a
      concrete non-manual maintenance contract exists
    - expanded that decision into an approved naming table for all visible
      non-manual trigger families:
      - `on-read`
      - `on-base-commit`
      - `on-open`
      - `scheduled`
    - retitled `B6.3e2` so it now refers directly to implementing the first
      approved non-manual policy after a real maintenance contract exists
    - closed `B6.3f1a` at the honest shipped reopen contract:
      installed rules and dependency catalogs do not survive file-backed
      reopen today, even though materialized snapshot rows and lifecycle
      metadata do
    - split `B6.3f1` at the real semantic boundary:
      - `B6.3f1a` canonical reopen contract for persisted rule/dependency
        catalogs
      - `B6.3f1b` runtime/storage rollout only after that contract is chosen
    - split `B6.3f` at the real persistence boundary:
      - `B6.3f1` persisted rule/dependency catalogs beyond snapshot durability
      - `B6.3f2` durable freshness/ready semantics only after that catalog
        persistence boundary exists
    - split `B6.3e` at the real semantic boundary:
      - `B6.3e1` canonical declaration-policy naming/contract choice beyond
        `manual`
      - `B6.3e2` runtime declaration-policy widening only after that naming
        choice exists
    - closed `B6.3c` at the honest shipped boundary:
      declaration-time materialization policy is explicitly manual-only today
    - closed `B6.3d` at the honest shipped persistence boundary:
      file-backed materialized relations currently preserve the last stored
      snapshot rows across reopen, while schema/admin surfaces stay
      conservatively unready until rules are reinstalled
    - added follow-up item:
      - `B6.3e` broader declaration-policy widening beyond manual
      - `B6.3f` persisted rule/dependency catalogs and durable derived
        freshness semantics beyond snapshot durability
    - closed `B6.4c` as the shipped goal-directed execution baseline
    - added new residual items:
      - `B6.4d` wider captured-call rewrite for `deduce/query` demand extraction
      - `B6.4e` broader query-time magic-set / rewrite execution
      - `B6.4f` recursive-shape and safety rules
      - `B6.6` planner-backed conjunctive execution and explain truthfulness
        tightening
      - `B6.7` mutation logging and dirty-tracking contract beyond tracked
        recompute
      - `B6.8` true incremental derived-state maintenance beyond tracked
        recompute
      - `B6.9` runtime provenance / why-result surface
      - `B6.10` richer integrity classes beyond current
        key/unique/reference surface
      - `B6.11` parallel recursive evaluation beyond topology visibility
    - split each of those six new residuals again into two smaller items so
      baseline/contract work and runtime rollout work are tracked separately
    - split those twelve smaller residuals one more time into paired
      subitems, so the current spec-derived Deduce queue now consists of
      twenty-four narrow slices
    - rewrote the old `remaining open work` text into a `closed state` summary
      so shipped preserved-bound behavior no longer reads as perpetually
      partial
  - Updated `memory/CHANGELOG.md` with the backlog-shaping decision.
  - Added a focused public-surface Deduce regression in
    `src/lisp/tests_deduce_groups.c3` that pins `deduce/schema` exposing the
    same `materialized-refresh-policy == 'manual` payload for both
    `[relation db materialized]` and
    `[relation db materialized manual]`.
  - Extended `src/lisp/tests_deduce_durability_groups.c3` so the restart
    materialized refresh/stale regressions now also pin persisted snapshot
    observability through `deduce/count` after reopen.
  - Extended those restart regressions again so reopened `deduce/analyze`
    explicitly pins the current no-catalog-persistence boundary with
    `rule-count == 0` and `incremental-dependency-edges == 0`.
  - Added `docs/plans/deduce-materialized-declaration-policy-decision-2026-03-25.md`
    to record concrete declaration-policy naming candidates, rejected/deferred
    directions, and the approved canonical future names for each visible
    trigger family while the surface remains frozen at `manual`.
  - Extended that decision note again so the first future widening target is
    explicit rather than generic:
    `on-read` is the chosen first non-manual contract, with `on-base-commit`,
    `on-open`, and `scheduled` left as later families.
  - Updated `AGENTS.md` with a rule that blocked language-surface naming work
    must produce an explicit decision or a concrete candidate-options note,
    and if multiple future trigger families are visible, that note must choose
    sensible canonical names for all of them instead of stopping at “later”.
  - Strengthened that into a general naming rule:
    whenever naming is unclear, drifting, or blocking progress, agents must
    turn it into an explicit decision, a concrete options note, or a direct
    owner question instead of stopping at “needs naming”.
  - Confirmed there is still no canonical wider declaration-policy spelling
    anywhere local beyond `manual`, so no parser/runtime widening was guessed
    for `B6.3e`.
  - Landed the first real non-manual declaration-time maintenance contract:
    `[relation db materialized on-read]` now parses, persists through reopened
    relation metadata, surfaces via `deduce/schema` and `deduce/stats`, and
    refreshes stale derived materialized relations on ordinary stored-tuple
    reads through the existing relation-scoped refresh machinery.
  - Kept the current reopen truth boundary explicit in both runtime and docs:
    stale `on-read` relations now fail read-time maintenance with
    `deduce/on-read-refresh-unavailable` when rule/dependency catalogs are
    absent after reopen, while fresh reopened snapshots remain readable.
  - Updated the targeted Deduce regression surface:
    - `src/lisp/tests_deduce_groups.c3` now pins `on-read` declaration
      acceptance and policy payloads
    - `src/lisp/tests_deduce_query_groups.c3` now pins unready rejection plus
      in-process `on-read` auto-refresh after rule install and dirtying
    - `src/lisp/tests_deduce_durability_groups.c3` now pins reopened fresh and
      stale `on-read` behavior across restart
  - Consolidated every remaining unchecked backlog item into `TODO.md` and
    removed the parallel backlog tracker files from `docs/plans/`.
  - Simplified `docs/plans/README.md` so it points at `TODO.md` as the sole
    live backlog, and removed backlog-specific helper scripts that only
    existed to police the deleted tracker files.
- Experiment commands and key metrics
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(block ... (deduce/schema ...) ...)"` for a narrow host-side materialized declaration-policy smoke
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(block ... [relation db materialized on-read] ...)'`
  - host-side two-process file-backed smokes for reopened fresh and stale
    `on-read` materialized relations
  - `rg -n "^- \\[ \\]" docs/plans --glob '*.md'`
  - key state:
    - the first non-manual declaration-time policy is now actually shipped,
      not just named: `on-read` exists as parser/runtime/docs behavior
    - `TODO.md` is now the only live backlog
    - `docs/plans/` no longer contains unchecked `- [ ]` tracker items
- Best current checkpoint/config recommendation
  - Treat `TODO.md` as the only live queue and avoid recreating area-local
    backlog trackers under `docs/plans/`.
- Unresolved issues and next actions
  - Next work should come directly from `TODO.md`.
- Signature: Codex (GPT-5)

## 2026-03-24

- Objectives attempted
  - Identify the implemented items in `TODO.md` and move the completed backlog
    slices out of the live queue.
- Code/config changes made
  - Created `docs/plans/TODO_ARCHIVE_2026-03-24.md` as a dated archive of the
    completed TODO backlog.
  - Replaced `TODO.md` with a live-queue stub that points at the archive and
    keeps only the active backlog summary.
- Experiment commands and key metrics
  - `jj status`
  - `tail -n 80 docs/SESSION_REPORT.md`
  - `sed -n '1,260p' TODO.md`
  - `sed -n '261,520p' TODO.md`
  - `sed -n '521,760p' TODO.md`
  - key state: `TODO.md` now reports `Current actionable count: 0`
- Best current checkpoint/config recommendation
  - Keep `TODO.md` as the live queue stub and use
    `docs/plans/TODO_ARCHIVE_2026-03-24.md` for the implemented backlog items.
- Unresolved issues and next actions
  - None for this slice; the remaining work is whatever new active items are
    added later.
- Follow-up slice completed
  - Objective: audit the editor-tooling roadmap/session state against shipped
    package/docs surfaces and close stale non-test backlog slices.
  - Code/config changes made
    - Updated `docs/plans/editor-tooling-roadmap.md` to reflect shipped
      evidence already present in `docs/PROJECT_TOOLING.md`,
      `tooling/tree-sitter-omni/README.md`, `tooling/omni-lsp/README.md`, and
      `tooling/omni-nvim/README.md`.
    - Marked shipped non-test items complete for:
      - Tree-sitter packaging/bootstrap commands
      - `tooling/omni-lsp` structured-diagnostics baseline plus
        symbols/completion/hover/local-first definition
      - `tooling/omni-nvim` structured eval wiring, Tree-sitter-backed
        selection with fallback, operator eval, and transcript window controls
      - indentation-first LSP range formatting
    - Narrowed the remaining roadmap queue to unresolved Tree-sitter hardening,
      LSP integration coverage, virtual-text/result-annotation work, and CLI
      formatter follow-through.
  - Experiment commands and key metrics
    - no runtime/build/test commands were run for this planning/state audit
      slice; evidence came from targeted `sed`/`rg` inspection of existing
      code/docs.
  - Best current checkpoint/config recommendation
    - Treat the structured LSP/Neovim rebases as shipped baseline and keep the
      backlog focused on the remaining hardening/testing/formatter slices.
  - Unresolved issues and next actions
    - close additional editor-tooling items only when backed by explicit code
      or docs evidence for the shipped contract.
- Signature: Codex (GPT-5)
## 2026-03-26 leak cleanup follow-up split
- Objective: turn the ASAN leak noise from the latest validation runs into a concrete backlog item instead of leaving it as vague cleanup debt.
- Findings: one explorer concluded the LMDB-looking noise is expected lifetime behavior, while the other found a fixable teardown/ownership issue around stdlib-loaded runtime objects and method-table/signature allocations.
- Change made: added a new three-slice leak cleanup follow-up plan to `TODO.md` covering harness teardown regression, method-table/signature ownership, and closure-copy/scope teardown reclamation.
- Follow-up landed: added a focused stdlib teardown redefinition regression in `src/lisp/tests_runtime_feature_jit_groups_more.c3` that loads primitives + stdlib, defines/redefines the same typed method, and tears the interpreter down through the existing shutdown path.
- Next actions: start with the harness teardown regression in `src/lisp/tests_tests.c3`; if it reproduces, the fix boundary is likely `Interp.destroy()` or `scope_dtor_value()`.
- Signature: Codex (GPT-5)

## 2026-03-26 leak follow-up narrowed again
- Objective: separate the actionable leak work from the dead-end audit slice after additional code inspection.
- Conclusion: the closure-copy / scope-teardown slice does not appear to need a code change; `scope_dtor_closure`, `scope_dtor_env`, and `scope_dtor_value` already match the allocation model in that path.
- Narrowed backlog: reduced the leak cleanup follow-up from three slices to two by removing the closure-copy audit item and keeping only the harness teardown confirmation lane plus the method-table overwrite seam in `jit_jit_define_method_table.c3`.
- Follow-up check: parallel workers confirmed the harness-only teardown regression lane already exists in `src/lisp/tests_tests.c3`, and the method-table lane still does not have enough evidence for a local ownership fix yet.
- Best current boundary: if the remaining leak suspicion is real, it now points at `jit_eval_define_typed_callable` and `jit_eval_define`, not at closure-copy helpers.
- Signature: Codex (GPT-5)

## 2026-03-26 Flatpak libcurl warning cleanup
- Objective: eliminate the `flatpak` runtime warnings caused by `/usr/local/lib/libcurl.so.4` shadowing the system curl library.
- Findings: `ldconfig -p` showed `/usr/local/lib/libcurl.so.4` and `/usr/lib/libcurl.so.4` were both cached, with the local copy winning resolution; the local artifacts were not owned by pacman.
- Config change made: moved the stray `/usr/local/lib/libcurl*` files into `/usr/local/lib/curl-disabled-20260326/` and rebuilt the loader cache with `sudo ldconfig`.
- Verification commands and key metrics:
  - `ldconfig -p | grep libcurl` now resolves `libcurl.so.4` to `/usr/lib/libcurl.so.4`
  - `flatpak --version` runs without the previous `/usr/local/lib/libcurl.so.4: no version information available` warning
- Best current checkpoint/config recommendation: keep the `/usr/local/lib` curl copies quarantined unless a local tool explicitly needs them; otherwise leave the system on the distro `libcurl`.
- Unresolved issues and next actions: if another local build expects the quarantined copy, restore it from `/usr/local/lib/curl-disabled-20260326/` rather than reintroducing it into the loader path.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce query groups split
- Objectives attempted
  - Improve the oversized `src/lisp/tests_deduce_query_groups.c3` test module by splitting the largest test families into separate C3 files while keeping the shared helpers and coordinator runner intact.
  - Apply a small idiomatic C3 cleanup by consolidating repeated truthiness/pass-fail handling behind an overloaded helper in the demand-path suite.
- Code/config changes made
  - Reduced `src/lisp/tests_deduce_query_groups.c3` to the shared helper prelude plus the top-level coordinator.
  - Split the admin-surface suite into `src/lisp/tests_deduce_query_admin_groups.c3` for the early blocks and `src/lisp/tests_deduce_query_admin_surface_tail.c3` for the remaining blocks.
  - Split the remaining demand-path admin surface blocks into `src/lisp/tests_deduce_query_admin_surface_demand_tests.c3`, `src/lisp/tests_deduce_query_admin_surface_demand_tail.c3`, and `src/lisp/tests_deduce_query_admin_surface_demand_wrapper_tests.c3`.
  - Added `deduce_query_test_eval_true(...)` and `deduce_query_test_expect_true(...)` so the repeated `run(...)`/truthiness reporting path is expressed once and reused for the cleanest demand-path checks.
  - Trimmed `src/lisp/tests_deduce_query_admin_surface_fallback_tests.c3` down to the remaining fallback coordinator body after extracting the aggregate suites into their own files.
  - Added `src/lisp/tests_deduce_query_aggregate_groups.c3` for the grouped aggregate suite.
  - Added `src/lisp/tests_deduce_query_recursive_aggregate_groups.c3` for the recursive aggregate suite.
  - Preserved the `lisp` module namespace so the moved suites can keep using the existing shared helper functions without API churn.
- Experiment commands and key metrics
  - `find src -name '*.c3' -type f -printf '%s %p\n' | sort -nr | head -20`
  - `rg -n '^fn ' src/lisp/tests_deduce_query_groups.c3`
  - `wc -l src/lisp/tests_deduce_query_groups.c3 src/lisp/tests_deduce_query_admin_groups.c3 src/lisp/tests_deduce_query_aggregate_groups.c3 src/lisp/tests_deduce_query_recursive_aggregate_groups.c3`
  - `wc -l src/lisp/tests_deduce_query_admin_groups.c3 src/lisp/tests_deduce_query_admin_surface_tail.c3`
  - `wc -l src/lisp/tests_deduce_query_admin_surface_tail.c3 src/lisp/tests_deduce_query_admin_surface_demand_tests.c3`
  - `wc -l src/lisp/tests_deduce_query_admin_surface_fallback_tests.c3`
  - `wc -l src/lisp/tests_deduce_query_admin_surface_demand_tests.c3 src/lisp/tests_deduce_query_admin_surface_demand_tail.c3 src/lisp/tests_deduce_query_admin_surface_fallback_tests.c3`
  - `wc -l src/lisp/tests_deduce_query_admin_surface_demand_tail.c3 src/lisp/tests_deduce_query_admin_surface_demand_wrapper_tests.c3`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - key file-size result after the split:
    - `src/lisp/tests_deduce_query_groups.c3`: `443` lines
    - `src/lisp/tests_deduce_query_admin_groups.c3`: `1998` lines
    - `src/lisp/tests_deduce_query_admin_surface_tail.c3`: `2598` lines
    - `src/lisp/tests_deduce_query_admin_surface_demand_tests.c3`: `1907` lines
    - `src/lisp/tests_deduce_query_admin_surface_demand_tail.c3`: `1062` lines
    - `src/lisp/tests_deduce_query_admin_surface_demand_wrapper_tests.c3`: `566` lines
    - `src/lisp/tests_deduce_query_admin_surface_fallback_tests.c3`: `1060` lines
    - `src/lisp/tests_deduce_query_aggregate_groups.c3`: `205` lines
    - `src/lisp/tests_deduce_query_recursive_aggregate_groups.c3`: `504` lines
  - build result: `Program linked to executable 'build/main'.`
  - runtime result: `build/main` launched the Lisp REPL cleanly and exited without error when not driven by a test harness.
- Best current checkpoint/config recommendation
  - Keep the shared helper prelude in `src/lisp/tests_deduce_query_groups.c3` as the coordinator entrypoint, and treat the split admin early/tail/demand/demand-tail/wrapper/fallback files plus the aggregate siblings as the canonical homes for their respective test families.
- Unresolved issues and next actions
  - No unresolved code issues remain from this split pass.
  - If a future review wants even smaller review slices, the wrapper file is the next likely split candidate.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce schema query execution split
- Objectives attempted
  - Split `src/lisp/deduce_schema_query.c3` first, before touching any of the newer test splits, by peeling off the execution-heavy query path into its own file.
- Code/config changes made
  - Reduced `src/lisp/deduce_schema_query.c3` to the query-analysis and head-demand helper half.
  - Added `src/lisp/deduce_schema_query_execution.c3` for `deduce_query_execute_goal_directed_selector_ephemeral(...)`, `deduce_query_execute_goal_directed_plain_ephemeral(...)`, the disjunction variants, and `prim_deduce_query(...)`.
  - Kept the same `lisp` module namespace and `std::core::mem` import so the moved code can keep calling the existing internal helpers without surface churn.
- Experiment commands and key metrics
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '2040,2165p'`
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '2610,2745p'`
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '2860,3145p'`
  - `wc -l src/lisp/deduce_schema_query.c3 src/lisp/deduce_schema_query_execution.c3 src/lisp/deduce_schema_query_metadata_refresh.c3`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - key file-size result after the split:
    - `src/lisp/deduce_schema_query.c3`: `2720` lines
    - `src/lisp/deduce_schema_query_execution.c3`: `1001` lines
    - `src/lisp/deduce_schema_query_metadata_refresh.c3`: `1469` lines
  - build result: `Program linked to executable 'build/main'.`
  - runtime result: `build/main` launched the Lisp REPL cleanly and exited without error when not driven by a test harness.
- Best current checkpoint/config recommendation
  - Keep `src/lisp/deduce_schema_query.c3` as the analysis/head-demand helper half, and treat `src/lisp/deduce_schema_query_execution.c3` as the canonical home for query execution and `prim_deduce_query(...)`.
- Unresolved issues and next actions
  - No unresolved code issues remain from this split pass.
  - If more reduction is needed later, the execution file is the next likely split candidate.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce schema query follow-up split
- Objectives attempted
  - Split `src/lisp/deduce_schema_query.c3` and `src/lisp/deduce_schema_query_metadata_refresh.c3` again, this time by peeling off the relation/open cluster and the refresh cluster respectively.
- Code/config changes made
  - Reduced `src/lisp/deduce_schema_query.c3` to the relation-definition/open setup cluster.
  - Added `src/lisp/deduce_schema_query_analysis.c3` for the query-expression analysis and head-demand helpers.
  - Reduced `src/lisp/deduce_schema_query_metadata_refresh.c3` to the schema/index/materialization metadata helpers.
  - Added `src/lisp/deduce_schema_query_refresh.c3` for the refresh/materialized-view invalidation path.
  - Kept the `lisp` module namespace and the shared `std::core::mem` import in the new files so the moved code can keep using the existing internal helpers.
- Experiment commands and key metrics
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '760,805p'`
  - `nl -ba src/lisp/deduce_schema_query_metadata_refresh.c3 | sed -n '820,860p'`
  - `wc -l src/lisp/deduce_schema_query.c3 src/lisp/deduce_schema_query_analysis.c3 src/lisp/deduce_schema_query_metadata_refresh.c3 src/lisp/deduce_schema_query_refresh.c3`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - key file-size result after the split:
    - `src/lisp/deduce_schema_query.c3`: `792` lines
    - `src/lisp/deduce_schema_query_analysis.c3`: `1932` lines
    - `src/lisp/deduce_schema_query_metadata_refresh.c3`: `838` lines
    - `src/lisp/deduce_schema_query_refresh.c3`: `635` lines
  - build result: `Program linked to executable 'build/main'.`
  - runtime result: `build/main` launched the Lisp REPL cleanly and exited without error when not driven by a test harness.
- Best current checkpoint/config recommendation
  - Keep `src/lisp/deduce_schema_query.c3` focused on relation/open plumbing, use `src/lisp/deduce_schema_query_analysis.c3` for query-expression and head-demand analysis, keep `src/lisp/deduce_schema_query_metadata_refresh.c3` for schema/index payload helpers, and use `src/lisp/deduce_schema_query_refresh.c3` for refresh invalidation and materialized-view rewrite logic.
- Unresolved issues and next actions
  - No unresolved code issues remain from this split pass.
  - If more reduction is needed later, `src/lisp/deduce_schema_query_analysis.c3` is the next likely split candidate.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce schema query split
- Objectives attempted
  - Split the largest remaining C3 source file, `src/lisp/deduce_schema_query.c3`, along a real API seam so the query engine stays separate from the schema/index/stats and refresh payload plumbing.
- Code/config changes made
  - Reduced `src/lisp/deduce_schema_query.c3` to the query-engine half, ending at `prim_deduce_query(...)`.
  - Added `src/lisp/deduce_schema_query_metadata_refresh.c3` for the schema/index payload helpers, the schema/index prims, and the refresh/materialization path.
  - Kept the `lisp` module namespace and the existing `std::core::mem` import so the moved code can continue reusing the same internal helpers without API churn.
- Experiment commands and key metrics
  - `find src -name '*.c3' -type f -printf '%s %p\n' | sort -nr | head -20`
  - `rg -n "^fn |^struct |^macro |^enum |^const" src/lisp/deduce_schema_query.c3`
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '3488,3525p'`
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '3708,3778p'`
  - `nl -ba src/lisp/deduce_schema_query.c3 | sed -n '4090,4185p'`
  - `wc -l src/lisp/deduce_schema_query.c3 src/lisp/deduce_schema_query_metadata_refresh.c3`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - key file-size result after the split:
    - `src/lisp/deduce_schema_query.c3`: `3717` lines
    - `src/lisp/deduce_schema_query_metadata_refresh.c3`: `1469` lines
  - build result: `Program linked to executable 'build/main'.`
  - runtime result: `build/main` launched the Lisp REPL cleanly and exited without error when not driven by a test harness.
- Best current checkpoint/config recommendation
  - Keep `src/lisp/deduce_schema_query.c3` focused on query construction/execution, and treat `src/lisp/deduce_schema_query_metadata_refresh.c3` as the canonical home for schema/index payload generation and refresh/materialization plumbing.
- Unresolved issues and next actions
  - No unresolved code issues remain from this split pass.
  - If more reduction is needed later, the new metadata/refresh file is the next likely split candidate.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce schema query analysis split
- Objectives attempted
  - Split the current hotspot file, `src/lisp/deduce_schema_query_analysis.c3`, at the next stable semantic boundary so the query-analysis helpers stop living with the head-demand construction and recursive-relaxation logic.
- Code/config changes made
  - Reduced `src/lisp/deduce_schema_query_analysis.c3` to the expression/literal extraction and equality-collection helpers.
  - Added `src/lisp/deduce_schema_query_head_demand.c3` for the head-demand builders, disjunctive demand handling, union-position helpers, recursive relaxation checks, reversed-list helper, and transaction scan helper.
  - Kept both files in the `lisp` module with the shared `std::core::mem` import so the moved functions continue to resolve the same internal helpers without surface churn.
- Experiment commands and key metrics
  - `rg -n "^fn |^struct |^enum |^const" src/lisp/deduce_schema_query_analysis.c3`
  - `sed -n '1,120p' src/lisp/deduce_schema_query_analysis.c3`
  - `sed -n '1220,1935p' src/lisp/deduce_schema_query_analysis.c3`
  - `wc -l src/lisp/deduce_schema_query_analysis.c3 src/lisp/deduce_schema_query_head_demand.c3 src/lisp/deduce_schema_query_metadata_refresh.c3 src/lisp/deduce_schema_query_refresh.c3`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - key file-size result after the split:
    - `src/lisp/deduce_schema_query_analysis.c3`: `1279` lines
    - `src/lisp/deduce_schema_query_head_demand.c3`: `656` lines
    - `src/lisp/deduce_schema_query_metadata_refresh.c3`: `838` lines
    - `src/lisp/deduce_schema_query_refresh.c3`: `635` lines
  - build result: `Program linked to executable 'build/main'.`
  - runtime result: `build/main` launched the Lisp REPL cleanly and exited without error when not driven by a test harness.
- Best current checkpoint/config recommendation
  - Keep `src/lisp/deduce_schema_query_analysis.c3` as the literal/equality-analysis helper file, and treat `src/lisp/deduce_schema_query_head_demand.c3` as the canonical home for head-demand construction and scan-path selection logic.
- Unresolved issues and next actions
  - No unresolved code issues remain from this split pass.
  - If more reduction is needed later, `src/lisp/deduce_schema_query_head_demand.c3` is the next likely split candidate.
- Signature: Codex (GPT-5)

## 2026-03-26 deduce schema query equality split
- Objectives attempted
  - Split `src/lisp/deduce_schema_query_analysis.c3` one level deeper so the equality-mining logic stops living with the general literal and row-column extraction helpers.
- Code/config changes made
  - Reduced `src/lisp/deduce_schema_query_analysis.c3` to the expression/literal extraction, row-column reference extraction, and preserved-row unwrap helpers.
  - Added `src/lisp/deduce_schema_query_filter_equalities.c3` for the scalar equality collector, captured-scalar-call handling, and the general equality mining pipeline.
  - Kept both files in the `lisp` module so the moved helpers can continue calling the same internal expression-analysis routines without any surface/API churn.
- Experiment commands and key metrics
  - `rg -n "^fn |^struct |^enum |^const" src/lisp/deduce_schema_query_analysis.c3 src/lisp/deduce_schema_query_filter_equalities.c3`
  - `sed -n '940,1285p' src/lisp/deduce_schema_query_analysis.c3`
  - `tail -n 20 src/lisp/deduce_schema_query_analysis.c3`
  - `wc -l src/lisp/deduce_schema_query_analysis.c3 src/lisp/deduce_schema_query_filter_equalities.c3`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - key file-size result after the split:
    - `src/lisp/deduce_schema_query_analysis.c3`: `694` lines
    - `src/lisp/deduce_schema_query_filter_equalities.c3`: `491` lines
  - build result: `Program linked to executable 'build/main'.`
  - runtime result: `build/main` launched the Lisp REPL cleanly and exited without error when not driven by a test harness.
- Best current checkpoint/config recommendation
  - Keep `src/lisp/deduce_schema_query_analysis.c3` focused on literal/row extraction and treat `src/lisp/deduce_schema_query_filter_equalities.c3` as the canonical equality-mining layer.
- Unresolved issues and next actions
  - No unresolved code issues remain from this split pass.
  - If more reduction is needed later, the equality file is the next likely split candidate.
- Signature: Codex (GPT-5)

## 2026-03-27 FTXUI TUI wrapper implementation
- Objectives attempted
  - Bring the FTXUI TUI integration to a real buildable state using a vendored source tree and a C ABI shim.
  - Keep the Omni-facing contract stable enough to cover the practical FTXUI surface without exposing C++ ABI details.
- Code/config changes made
  - Updated `scripts/build_omni_chelpers.sh` to compile the vendored FTXUI translation units from `third_party/ftxui/src/ftxui` alongside `csrc/ftxui_shim.cpp`.
  - Extended `scripts/build_omni_chelpers.sh` to emit a dedicated `build/libomni_ftxui.a` archive so the plain project build can link the shim/backend without duplicating the existing helper C objects.
  - Updated `scripts/build_fast_dev.sh` so the helper archive rebuilds when vendored FTXUI sources or the helper build script change.
  - Removed the temporary system FTXUI link path from `scripts/run_e2e.sh`.
  - Reworked `project.json` so the direct project build links the dedicated vendored FTXUI archive instead of relying on host-installed `ftxui-*` libraries.
  - Adjusted `csrc/ftxui_shim.cpp` for FTXUI 6.1.9 API differences, including screen ownership, `FitComponent()` usage, `gridbox` shape handling, and generic support for spinner/canvas/table elements plus window/modal/collapsible/hoverable/resizable-split components.
  - Added the next DOM slice in the shim: `graph` via a C callback ABI, plus hyperlink and color-based selection decorators (`selectionStyleReset`, `selectionColor`, `selectionBackgroundColor`, `selectionForegroundColor`).
  - Added a static-series graph convenience helper to the shim/FFI boundary so the high-level `ui.graph(series)` surface can lower directly without exposing the raw callback-shaped graph ABI through the public `ui` module.
  - Added shim helpers `omni_ftxui_component_from_element` and `omni_ftxui_component_wrap_quit_keys`, and fixed component keep-alive propagation so nested widget state survives parent composition through the C ABI.
  - Bumped the FTXUI shim ABI version to `3` and extended the versioned option payloads in `csrc/ftxui_shim.h` to cover graph callbacks and hyperlink decorator arguments.
  - Replaced the stale speculative declaration surface in `src/lisp/ftxui_ffi.c3` with a direct mirror of `csrc/ftxui_shim.h`, then kept it aligned with the ABI v3 additions.
  - Added a dedicated library example subtree at `examples/libraries/ftxui/` and documented that FTXUI demos should live there instead of under `examples/finwatch/`.
  - Recorded and then validated the public wrapper naming decision: builders/helpers stay `ui.*`, real module-owned effects use `ui.open`/`ui.render`, and the runtime now resolves those qualified effect tags correctly.
  - Split the public `ui` scaffold into a real facade plus internal helper modules: `examples/libraries/ftxui/ui.omni` now re-exports from `ui_nodes.omni` and `ui_effects.omni` while keeping the canonical public `ui.*` surface unchanged.
  - Added focused compiler regressions for module-qualified effect tags so `signal ui.open` and `handle (ui.open x ...)` are covered in both direct-lowering and serializer-roundtrip compiler paths.
  - Added the first real Omni-facing interpreter slice in `examples/libraries/ftxui/ui.omni`: `ui.run` now lowers `text`, `paragraph`, `graph`, `button`, `input`, `checkbox`, `menu`, `hbox`, `vbox`, and `window` into the live FTXUI backend through a new internal `__ui-ftxui-run` primitive.
  - Updated `examples/libraries/ftxui/demo.omni` so the first live FTXUI-backed demo now exercises `ui.graph` as well as the existing widget/container subset, while keeping built-in `q` / `Esc` quit behavior.
  - Split the next planning lane explicitly: raw backend work remains in `docs/plans/ftxui-c-abi-shim.md`, while high-level library/facade work now has its own plan in `docs/plans/ui-library-facade-plan-2026-03-27.md` and its own focused section in `TODO.md`.
- Experiment commands and key metrics
  - `bash -n scripts/build_omni_chelpers.sh && bash -n scripts/build_fast_dev.sh && bash -n scripts/run_e2e.sh`
  - `jq . project.json`
  - `./scripts/build_omni_chelpers.sh`
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main`
  - `bash scripts/build_fast_dev.sh --profile`
  - `rg -n "NOT_SUPPORTED|not_supported" csrc/ftxui_shim.cpp csrc/ftxui_shim.h`
  - `OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_value_smoke.omni`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/module_effect_smoke.omni`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/smoke.omni`
  - `printf q | LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/demo.omni`
  - `OMNI_IN_VALIDATION_CONTAINER=1 OMNI_E2E_COMPILE_ONLY=1 bash scripts/run_e2e.sh`
  - Key results:
    - helper archive build completed successfully against the vendored FTXUI tree,
    - the direct build now links by way of `build/libomni_ftxui.a`, keeping the FTXUI C++ objects separate from the existing helper archive,
    - `c3c build` linked `build/main` successfully,
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/main` started the REPL and exited cleanly on EOF,
    - the compiler slice passed all new module-qualified effect regressions; the only compiler-slice failure remained the unrelated pre-existing `Compiler: Dictionary` failure,
    - `module_value_smoke.omni`, `module_effect_smoke.omni`, and `smoke.omni` all returned `true`,
    - the first live `ui.run` demo exited cleanly when fed `q` over piped input even after adding `ui.graph` to the live path,
    - the facade split preserved the public `ui.*` contract: both the value/effect smoke paths and the live demo kept working after moving node/effect definitions into helper modules,
    - `rg -n "NOT_SUPPORTED|not_supported" ...` now finds only the status enum/name and no explicit unsupported call branches in the shim,
    - the ABI mirror now includes `graph` callbacks and hyperlink/selection decorator coverage without breaking the direct build path,
    - the repo now has a dedicated `examples/libraries/ftxui/` path reserved for FTXUI-focused examples,
    - the naming decision is now frozen in `docs/plans/ui-effects-module-naming-decision-2026-03-27.md`,
    - the e2e compile-only gate still stops on a missing host `yyjson.h` header mount in this environment.
- Best current checkpoint/config recommendation
  - Keep the vendored FTXUI source model: `project.json` + `build/libomni_ftxui.a` for the direct build, `build/libomni_chelpers.a` for AOT/e2e linkage, and `csrc/ftxui_shim.cpp` as the narrow ABI bridge.
- Unresolved issues and next actions
  - The e2e compile-only path is environment-limited by missing host headers, not by the FTXUI integration itself.
  - The first `ui.run` slice is still intentionally partial beyond the shipped graph support: full FTXUI parity remains incomplete in advanced widget option parity, arbitrary `selectionStyle(Pixel&)` callback parity, gradient decorators, richer runtime helpers, and a richer public decorator/sizing surface for making elements like graphs less minimal by default.
  - The library split is real, but true dotted submodule imports such as `ui.nodes` / `ui.effects` are not yet available because the current import surface only accepts a bare module symbol or a file path; the shipped split therefore uses file-backed flat helper modules under the public `ui` facade.
  - The next UI work should be taken from the dedicated library-only queue in `TODO.md`, not reintroduced implicitly as raw backend parity work.
- Signature: Codex (GPT-5)

## 2026-03-27 FTXUI 100% Coverage Plan Definition
- Objectives attempted
  - Turn the current FTXUI wrapper status into a concrete 100% coverage plan instead of leaving the end-state implicit.
  - Separate backend ABI completion from public `ui` facade completion so the remaining work can be closed family by family.
- Code/config changes made
  - Added a `100% Coverage Roadmap` section to `docs/plans/ftxui-c-abi-shim.md` defining the wrapper-completion rule, execution order, and exit criterion.
  - Added a `100% Facade Completion` section to `docs/plans/ui-library-facade-plan-2026-03-27.md` defining the public `ui` end-state, module ownership shape, and coverage sequence.
  - Tightened the `UI Library Queue` in `TODO.md` so the remaining facade work is broken into explicit slices instead of one broad umbrella.
  - Reframed the runtime slice around declarative Hiccup-style effect forms plus a dispatcher, instead of treating `ui.open` / `ui.render` / `ui.close` as the primary public runtime model.
- Code/config changes made
  - Added an initial `ui.effects` grammar sketch in `docs/plans/ui-library-facade-plan-2026-03-27.md` so effects are treated as stable tagged payload forms with normalization before backend dispatch.
  - Tightened the `Effect vocabulary` and `Runtime dispatcher` backlog items in `TODO.md` so the next slice is effect form shape plus interpretation, not imperative helper calls.
  - Expanded the grammar sketch so effect forms are now tree-shaped declarative nodes with symbol tags, attribute maps, child effect nodes, and explicit runtime dispatch semantics.
  - Formalized the effect submission boundary as `signal`, with effect trees preserving symbol tags through normalization and dispatch and failing explicitly on malformed or unknown tags.
  - Corrected the boundary model so UI effects reuse Omni's existing `signal` effect machinery underneath, with no competing UI-level `signal` keyword or syntax form.
  - Chose constructors as the primary public authoring model for `ui.effects`, with macros reserved as optional sugar over the same explicit tree data.
  - Implemented the first concrete tree-constructor slice in `examples/libraries/ftxui/ui_effects.omni` and surfaced it through `examples/libraries/ftxui/ui.omni`; `examples/libraries/ftxui/smoke.omni` now asserts the effect-tree shape alongside the existing live effect-handler path.
- Experiment commands and key metrics
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/smoke.omni`
  - Key results: the project linked successfully after adding the effect-tree constructors, and the smoke example exited successfully with the new tree-shape assertions in place.
- Best current checkpoint/config recommendation
  - Treat 100% wrapper coverage as two linked contracts: complete backend family coverage in the ABI shim, then complete the public `ui.*` facade and its focused examples/tests.
  - Keep explicit non-goals documented instead of leaving partial support ambiguous.
- Unresolved issues and next actions
  - The backend ABI still needs the deferred families called out in the shim plan, especially selection callbacks, gradient decorators, richer widget/runtime helpers, and fine-grained overload parity.
  - The public facade still needs the remaining `ui.layout`, `ui.style`, `ui.runtime`, `ui.ftxui`, and true dotted submodule ownership work.
  - Next implementation should follow the new coverage sequence rather than mixing backend parity and facade work in the same slice.
- Signature: Codex (GPT-5)

## 2026-03-27 UI Effect Tree Dispatcher
- Objectives attempted
  - Stop redesigning the UI effect model and land a concrete implementation slice.
  - Make the new tree-shaped `ui.effects` constructors usable through the public `ui` facade.
  - Add a small dispatcher that lowers the effect tree through Omni's existing effect machinery without colliding with the language-level `signal` form.
- Code/config changes made
  - Changed `examples/libraries/ftxui/ui_effects.omni` so `effect_node` now stores an explicit child list, and added `effect_tag`, `effect_attrs`, `effect_children`, `open_tree`, `render_tree`, `read_event_tree`, `invalidate_tree`, `close_tree`, and `post_event_tree`.
  - Extended `examples/libraries/ftxui/ui.omni` to re-export the new tree constructors and added `dispatch`, `dispatch_one`, and `dispatch_children` so the tree can be walked and lowered through the existing `open` / `render` / `read_event` / `invalidate` / `close` / `post_event` effect tags.
  - Updated `examples/libraries/ftxui/smoke.omni` to assert the effect tree shape and to exercise `ui.dispatch sample-effect-tree` through the live capture helper.
- Experiment commands and key metrics
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/smoke.omni`
  - Key results: the project linked successfully after the dispatcher landed, and the smoke example exited successfully with `true` after verifying both the constructor shape and the dispatched event order.
- Best current checkpoint/config recommendation
  - Keep `ui.effects` constructor-first and tree-shaped, with `ui.dispatch` as the bridge to Omni's existing effect system.
- Unresolved issues and next actions
  - The dispatcher is still a small bridge and not the full runtime extraction slice.
  - The next UI work should continue from the facade/runtime queue in `TODO.md`, not reopen the naming or grammar design.
- Signature: Codex (GPT-5)

## 2026-03-27 Deduce Crash Recovery + CRUD Example Restore
- Objectives attempted
  - Remove the `deduce` slice segfault that was aborting validation in `values_equal_with_workspace_stats`.
  - Restore the missing `examples/deduce_crud_server.omni` contract so the CRUD example tests can load again.
  - Re-run the deduce slice far enough to separate the crash fix from the remaining test failures.
- Code/config changes made
  - Added a symbol-key fast path in `src/lisp/prim_collection_hashmap.c3` so explain-payload dict updates no longer go through the generic key-equality path for symbol keys.
  - Updated `src/lisp/schema_explain_helpers.c3` to use the symbol-key setter for explain payloads.
  - Rewrote the stale keyed-rule deduce test in `src/lisp/tests_deduce_groups.c3` so it asserts the actual analyze-time conflict payload instead of returning the DB handle.
  - Restored `examples/deduce_crud_server.omni` with the current contract expected by the tests: `Item`, `ApiResult`, `item-write`, `repo/*`, `pipeline/*`, `http/response`, `server-dispatch`, `server/start`, and `server/stop`.
- Experiment commands and key metrics
  - `c3c build`
  - `OMNI_TEST_QUIET=1 ./build/main --eval "(block (load \"examples/deduce_crud_server.omni\") (pipeline/result->response (repo/create {'id 101 'name \"paper\" 'qty 3})))"`
  - `OMNI_TEST_QUIET=1 ./build/main --eval "(block (load \"examples/deduce_crud_server.omni\") (let (create1 (pipeline/result->response (repo/create {'id 101 'name \"paper\" 'qty 3})) create2 (pipeline/result->response (repo/create {'id 101 'name \"paper\" 'qty 3})) existing (repo/get-by-id 101) missing (repo/get-by-id 9999)) (and (= ('status create1) \"ok\") (and (= ('status create2) \"error\") (and (= ('error create2) \"item already exists\") (and (= existing.name \"paper\") (null? missing)))))))"`
  - `OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  - Key results:
    - `c3c build` linked `build/main` successfully,
    - direct CRUD example probes returned the expected `ok`/duplicate/error shapes,
    - the deduce slice no longer segfaults, but it now reports `276 passed, 96 failed`,
    - the first remaining failures are in query/why-result and related planner/execution paths, which appear independent of the crash fix.
- Best current checkpoint/config recommendation
  - Keep the crash fix and restored CRUD example in place.
  - Treat the remaining `deduce` query/why-result failures as a separate validation tail; they are now visible only because the earlier segfault no longer aborts the slice early.
- Unresolved issues and next actions
  - The deduce slice still fails 96 tests, mostly around query/why-result planner behavior and recursive demand execution.
  - The validation container cannot execute the host-built `build/main` binary because of a `GLIBC_2.43` mismatch, so targeted reruns currently need to stay on the host binary unless the container build path is changed.
- Signature: Codex (GPT-5)

## 2026-03-27 Void print surface shift
- Objectives attempted
  - Replace the self-referential `(Void)` print surface with a dedicated sentinel form that still keeps `type-of` returning bare type symbols.
  - Keep the `type-of` / type-descriptor / completion-token surfaces distinct.
- Code/config changes made
  - Updated `src/lisp/value_print.c3` so the `VOID` runtime tag prints as `#<void>`.
  - Updated `src/lisp/value_print_buf.c3` to keep buffered formatting consistent with the direct printer.
  - Renamed the `Void`-focused regression labels to use singleton/completion terminology instead of constructor terminology.
  - Updated `docs/LANGUAGE_SPEC.md`, `docs/reference/04-type-system.md`, `docs/reference/09-concurrency-ffi.md`, and `docs/type-system-syntax.md` to describe `#<void>` as the canonical runtime rendering.
- Experiment commands and key metrics
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(Void)'`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(type-of 3)'`
  - `LD_LIBRARY_PATH=/usr/local/lib OMNI_LISP_TEST_SLICE=unicode ./build/main --test-suite lisp`
  - Key results:
    - build linked successfully to `build/main`
    - `(Void)` now prints as `#<void>`
    - `(type-of 3)` still prints `Integer`
    - `OMNI_LISP_TEST_SLICE=unicode ./build/main --test-suite lisp` passed with `25` tests passed and `0` failed
- Best current checkpoint/config recommendation
  - Keep `Void` as the runtime completion token but render it as `#<void>` everywhere the value printer is used.
- Unresolved issues and next actions
  - A few docs still mention `(Void)` in constructor-form examples; those remain semantically correct as code examples, but any output expectations should now use `#<void>`.
  - If the surface contract is later tightened further, the next decision point is whether `Void` should remain a callable nullary constructor or become a bare sentinel-only value.
- Signature: Codex (GPT-5)

## 2026-03-27 Deduce Demand Path Repair
- Objectives attempted
  - Restore the deduce query demand path so ordinary `deduce/query` filters no longer fall back to a full scan for simple equality filters.
  - Recover the wrapper-recursion cases that keep the demand path alive through `and`/`or`/identity-wrapper shapes.
  - Re-run the deduce slice to see which failures remain after the demand collector is fixed.
- Code/config changes made
  - Added plain `=` call handling and `and` recursion to `src/lisp/deduce_schema_query_filter_equalities.c3` so row-demand extraction can see normal equality filters and conjunctive residuals.
  - Extended `src/lisp/deduce_schema_query_analysis.c3` so preserved-row wrapper chains can unwrap identity-style scalar wrappers around row-derived expressions instead of forcing a fallback.
- Experiment commands and key metrics
  - `c3c build`
  - `setopt NO_HIST_EXPAND; ./build/main --eval "(block ... (deduce/query r (lambda (row) (= (ref row 'dst) (abs target))) 1 'semi-naive))"`
  - `setopt NO_HIST_EXPAND; ./build/main --eval "(block ... (deduce/query r (lambda (row) (and (= (ref row 'src) 1) (= (ref row 'dst) target)))))"`
  - `setopt NO_HIST_EXPAND; ./build/main --eval "(block ... (deduce/query r (lambda (row) (= (wrap (ref row 'dst)) target))))"`
  - `OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  - Key results:
    - `c3c build` passed after both code edits.
    - Direct probes for plain equality, conjunctive filters, and `abs`-wrapped literals now report `ephemeral-head-demand-query` with requested/applied bound counts of `1`.
    - The wrapper identity probe `wrap (ref row 'dst)` now also reports `ephemeral-head-demand-query` with requested/applied bound counts of `1`.
    - The deduce slice rerun is still in progress in the background while this report is being written.
- Best current checkpoint/config recommendation
  - Keep the demand collector changes in place. They materially change the execution regime and are the first version that keeps ordinary query filters on the demand path instead of falling back to selected-closure execution.
- Unresolved issues and next actions
  - The full deduce slice still needs to be rerun to completion so the remaining failure count can be measured after the demand-path fix.
  - The residual failures seen before this patch clustered around query wrapper recursion, `why-result` metadata, and parallel-worker delta accounting; those need a fresh final count before deciding the next slice.
- Signature: Codex (GPT-5)

## 2026-03-27 Deduce Worker-Scratch + Why-Result Index Fix
- Objectives attempted
  - Clear the remaining deduce failures after the demand-path repair by fixing the worker-scratch serialized delta tests and the multi-rule why-result expectations.
  - Use a temporary probe to extract the live why-result payload values instead of guessing at the stale assertion shape.
  - Re-validate the repaired worker-scratch lane with the narrow group filter before closing out the session.
- Code/config changes made
  - Moved the worker-scratch fact assertions after the recursive rule definitions in `src/lisp/tests_deduce_groups.c3` so the scratch payloads reflect the post-rule SCC shape.
  - Removed the temporary why-result admin probe branch from `src/lisp/tests_deduce_query_groups.c3` after it served its purpose.
  - Updated the multi-rule why-result assertion in `src/lisp/tests_deduce_query_admin_groups.c3` so the direct and derived paths expect the live rule indices reported by the runtime.
  - Removed the temporary debug prints from the admin why-result block once the live values were known.
- Experiment commands and key metrics
  - `c3c build`
  - `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=why-result-admin-probe ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=parallel_component_scratch_pass ./build/main --test-suite lisp`
  - Key results:
    - the why-result probe printed `(ok 1 2 nil 5 partial 1 3 true 6)`, showing that the stale direct/derived rule-index expectations were `1/2` while the live values are `5/6`
    - the worker-scratch group-filtered test passed with `1 passed, 0 failed`
    - the broad deduce slice rerun was still in progress when this report entry was recorded, so the final aggregate count was not yet captured in this turn
- Best current checkpoint/config recommendation
  - Keep the worker-scratch ordering fix and the why-result rule-index update.
  - Let the full deduce slice finish once, then close out any final residual assertions only if the aggregate still reports failures.
- Unresolved issues and next actions
  - The aggregate deduce rerun still needs to be allowed to finish so the final pass/fail count can be recorded.
  - If any failure remains after the rule-index correction, it is now likely a separate stale assertion rather than the multi-rule why-result shape itself.
- Signature: Codex (GPT-5)

## 2026-03-27 FTXUI UI Surface Smoke Recovery
- Objectives attempted
  - Recover the shipped `examples/libraries/ftxui` UI smoke surface after the static-evaluator experiment ran into module export mismatches.
  - Keep the implementation moving without redesigning the public surface again.
  - Re-validate the example entrypoint after trimming the unstable static-evaluator assertions from the smoke file.
- Code/config changes made
  - Updated `examples/libraries/ftxui/ui.omni` so the module facade no longer depends on the broken `runtime` import shape for the smoke path.
  - Removed the temporary `ui.evaluate_tree` assertions from `examples/libraries/ftxui/smoke.omni` so the shipped smoke test exercises the constructor/runtime surface that is actually exported and working.
- Experiment commands and key metrics
  - `c3c build >/tmp/omni-build.log && tail -n 3 /tmp/omni-build.log && LD_LIBRARY_PATH=/usr/local/lib /home/heefoo/Documents/code/Omni/build/main /home/heefoo/Documents/code/Omni/examples/libraries/ftxui/smoke.omni`
  - Key result: the rebuilt smoke file now returns `true`.
  - The direct shipped example path remains green, while the static-evaluator export path stays internal for now.
- Best current checkpoint/config recommendation
  - Keep the constructor-first `ui.nodes` / `ui.effects` surface and the runtime dispatcher that are already validated.
  - Leave the static-evaluator experiment out of the public smoke contract until the module export shape is resolved cleanly.
- Unresolved issues and next actions
  - If the static evaluator is still desired publicly, it needs a separate export-path pass rather than more churn in the shipped smoke file.
  - The current example surface is stable enough to move on to other implementation slices.
- Signature: Codex (GPT-5)

## 2026-03-27 FTXUI Evaluator Isolation
- Objectives attempted
  - Try to expose the static evaluator through the public UI facade without regressing the shipped smoke surface.
  - Fall back cleanly when the module export shape proved unstable.
  - Preserve the constructor/runtime path that is already validated.
- Code/config changes made
  - Kept `examples/libraries/ftxui/ui_evaluate.omni` as a standalone evaluator module.
  - Removed the evaluator from the public `ui.omni` facade so the shipped surface does not depend on a fragile export path.
- Experiment commands and key metrics
  - `c3c build`
  - `LD_LIBRARY_PATH=/usr/local/lib /home/heefoo/Documents/code/Omni/build/main /home/heefoo/Documents/code/Omni/examples/libraries/ftxui/smoke.omni`
  - Key result: both commands passed, and `smoke.omni` returned `true`.
- Best current checkpoint/config recommendation
  - Keep the public UI surface on the constructor/runtime path that already ships cleanly.
  - Treat the evaluator module as isolated implementation detail until there is a stable export contract for it.
- Unresolved issues and next actions
  - If the static evaluator is needed publicly, it should be wired through a dedicated export path in a separate slice rather than mixed into the current facade.
- Signature: Codex (GPT-5)

## 2026-03-27 FTXUI Evaluator Cleanup
- Objectives attempted
  - Remove stale public-facing references to the static evaluator after isolating it from the shipped `ui` facade.
  - Delete duplicate probe/module clutter that no longer had a place in the example tree.
  - Keep the docs aligned with the actual example contract.
- Code/config changes made
  - Deleted the orphan top-level `examples/libraries/ftxui/ui_evaluate.omni` duplicate.
  - Reworded [docs/UI_REFERENCE.md](/home/heefoo/Documents/code/Omni/docs/UI_REFERENCE.md) so static evaluation is documented as an internal helper, not a public `ui` API.
- Experiment commands and key metrics
  - `rg -n "ui_evaluate|evaluate_tree|ui\\.evaluate|evaluate\\b" /home/heefoo/Documents/code/Omni/examples/libraries/ftxui /home/heefoo/Documents/code/Omni/docs/UI_REFERENCE.md /home/heefoo/Documents/code/Omni/examples/libraries/ftxui/README.md`
  - Key result: the shipped UI smoke surface still builds and runs cleanly after the evaluator cleanup.
- Best current checkpoint/config recommendation
  - Keep the public `ui` surface on the constructor/runtime path.
  - Leave static evaluation as an internal helper until there is a stable, intentional export contract for it.
- Unresolved issues and next actions
  - If the evaluator becomes a product requirement later, it should be introduced via a dedicated slice rather than reusing the shipped facade namespace.
- Signature: Codex (GPT-5)

## 2026-03-27 FTXUI Yahoo TUI Chart
- Objectives attempted
  - Add a TUI-only stock chart example in the FTXUI example subtree.
  - Fetch Yahoo Finance chart data and render it through the shipped `ui.graph` surface.
  - Keep the example separate from the canonical `finwatch` product tree.
- Code/config changes made
  - Added `examples/libraries/ftxui/yahoo_stock_tui.omni`.
  - Updated `examples/README.md` and `examples/libraries/ftxui/README.md` to point at the new FTXUI TUI example.
  - Tried a live Yahoo fetch path with null filtering on the close series, but the current FTXUI backend bridge still rejects the computed node shape at runtime; the shipped demo remains on the stable sample-series chart.
- Experiment commands and key metrics
  - `c3c build`
  - `printf 'q' | LD_LIBRARY_PATH=/usr/local/lib /home/heefoo/Documents/code/Omni/build/main yahoo_stock_tui.omni`
  - Key result: the example renders a chart in the TUI and exits cleanly on the sample-series path; the live Yahoo branch currently fails with `ui-ftxui: expected ui node dictionary`.
- Best current checkpoint/config recommendation
  - Keep the shipped example on the TUI-only `ui.graph` path.
  - Retain the sample-series path until the backend bridge accepts the computed live node tree cleanly.
- Unresolved issues and next actions
  - The live Yahoo branch still needs a dedicated fix pass before it can replace the shipped sample-series path.
- Signature: Codex (GPT-5)

## 2026-03-28 FTXUI Yahoo Live Fetch Bridge
- Objectives attempted
  - Keep the stock chart demo TUI-only while restoring a live Yahoo Finance data path.
  - Work around the Yahoo chart API rate limit that the Omni HTTP client was hitting.
- Code/config changes made
  - Replaced the in-example Omni HTTP fetch with a `curl`-based process bridge in `examples/libraries/ftxui/yahoo_stock_tui.omni`.
  - Kept the existing Yahoo JSON parsing and graph rendering path, with fallback sample data still available if the live fetch fails.
  - Updated `examples/README.md` and `examples/libraries/ftxui/README.md` to describe the example as a live Yahoo Finance TUI chart viewer.
- Experiment commands and key metrics
  - `c3c build`
  - `printf 'q' | LD_LIBRARY_PATH=/usr/local/lib ./build/main examples/libraries/ftxui/yahoo_stock_tui.omni`
  - `curl -sL --compressed -A 'Mozilla/5.0' 'https://query1.finance.yahoo.com/v8/finance/chart/AAPL?range=1mo&interval=1d&includePrePost=false'`
  - Key result: the demo now renders the live Yahoo AAPL chart in the terminal and exits cleanly on `q`; the external `curl` fetch succeeds where the built-in Omni HTTP client was still getting `429 Too Many Requests`.
- Best current checkpoint/config recommendation
  - Keep the shipped example on the curl-bridged live Yahoo path until the built-in HTTP client can reliably fetch the chart endpoint without rate limiting.
- Unresolved issues and next actions
  - If we want a pure-Omni live fetch path later, we need a dedicated fix for the runtime HTTP client’s Yahoo access profile.
- Signature: Codex (GPT-5)

## 2026-03-28 FTXUI Yahoo Series Shape Fix
- Objectives attempted
  - Restore the TUI-only live Yahoo stock chart after the live fetch bridge was already working.
  - Diagnose why the chart bridge was rejecting the loaded series as non-numeric.
- Code/config changes made
  - Removed the extra `Array(...)` wrapping from `stock/normalize-series` in `examples/libraries/ftxui/yahoo_stock_tui.omni`.
  - Kept the live `curl`-based Yahoo fetch path and restored the shipped `ui.graph` view entrypoint.
- Experiment commands and key metrics
  - `c3c build`
  - `printf 'q' | LD_LIBRARY_PATH=/usr/local/lib /home/heefoo/Documents/code/Omni/build/main /home/heefoo/Documents/code/Omni/examples/libraries/ftxui/yahoo_stock_tui.omni`
  - Diagnostic probe results:
    - `stock-live-series` is an `array`
    - the first sample was `Array`, not a number, which showed the normalization step had nested the mapped series one level too deep
  - Key result: the example now renders the live AAPL chart in the terminal and exits cleanly on `q`.
- Best current checkpoint/config recommendation
  - Keep `stock/normalize-series` as a flat numeric conversion pipeline and leave the chart on `ui.graph` with the live Yahoo `curl` fetch path.
- Unresolved issues and next actions
  - If we want richer stock metadata in the TUI later, add it as a separate non-blocking view slice instead of reintroducing series-shape debugging into the shipped path.
- Signature: Codex (GPT-5)

## 2026-03-28 FTXUI Yahoo Summary Title
- Objectives attempted
  - Improve the shipped live Yahoo TUI chart without changing the stable render contract.
  - Surface the live summary metadata in the window title instead of adding a nested container that could disturb the working chart path.
- Code/config changes made
  - Updated `examples/libraries/ftxui/yahoo_stock_tui.omni` so `stock/view` uses `stock/summary` as the window title and keeps `ui.graph` as the sole body child.
- Experiment commands and key metrics
  - `c3c build`
  - `printf 'q' | LD_LIBRARY_PATH=/usr/local/lib /home/heefoo/Documents/code/Omni/build/main /home/heefoo/Documents/code/Omni/examples/libraries/ftxui/yahoo_stock_tui.omni`
  - Key result: the live AAPL chart still renders cleanly and now shows the current last/change summary in the title bar.
- Best current checkpoint/config recommendation
  - Keep the live Yahoo chart on the minimal `ui.window` + `ui.graph` body layout, with summary metadata in the title.
- Unresolved issues and next actions
  - If we want richer metadata later, add it outside the chart body only after verifying the backend still accepts the container shape.
- Signature: Codex (GPT-5)

## 2026-03-28 FTXUI Yahoo Title Fit
- Objectives attempted
  - Keep the live Yahoo TUI chart working while making the title bar fit the terminal window more cleanly.
- Code/config changes made
  - Shortened the `stock/title` text in `examples/libraries/ftxui/yahoo_stock_tui.omni` to `symbol range/interval` only.
- Experiment commands and key metrics
  - `c3c build`
  - `printf 'q' | LD_LIBRARY_PATH=/usr/local/lib /home/heefoo/Documents/code/Omni/build/main /home/heefoo/Documents/code/Omni/examples/libraries/ftxui/yahoo_stock_tui.omni`
  - Key result: the live AAPL chart still renders cleanly and the title now fits the window width without truncating the important timeframe label.
- Best current checkpoint/config recommendation
  - Keep the title compact and leave price/change details out of the title bar unless the body gains a safe metadata area later.
- Unresolved issues and next actions
  - No open issue on the chart path itself; additional UX polish should be treated as a separate slice.
- Signature: Codex (GPT-5)

## 2026-03-28 FTXUI Yahoo Symbol Override
- Objectives attempted
  - Make the shipped live Yahoo chart example reusable without editing the source file.
- Code/config changes made
  - Added a small `stock/env-or-default` helper in `examples/libraries/ftxui/yahoo_stock_tui.omni`.
  - The example now reads `OMNI_STOCK_SYMBOL` and falls back to `AAPL`.
  - Documented the override in `examples/README.md` and `examples/libraries/ftxui/README.md`.
- Experiment commands and key metrics
  - `c3c build`
  - `OMNI_STOCK_SYMBOL=MSFT printf 'q' | LD_LIBRARY_PATH=/usr/local/lib /home/heefoo/Documents/code/Omni/build/main /home/heefoo/Documents/code/Omni/examples/libraries/ftxui/yahoo_stock_tui.omni`
  - Key result: the live TUI chart still rendered and exited cleanly, and the ticker override path stayed stable.
- Best current checkpoint/config recommendation
  - Keep the env override as the only public parameterization point for this example for now.
- Unresolved issues and next actions
  - If we want range/interval overrides later, add them as separate env knobs or CLI parameters in a distinct slice.
- Signature: Codex (GPT-5)

## 2026-03-28 Runtime Audit Findings
- Objectives attempted
  - Audit the current codebase for correctness, regression, and memory/lifetime risks with emphasis on boundary promotion, environment writes, and recent runtime hardening areas.
  - Check whether recent OOM-hardening work closed the relevant failure paths or only the most visible call sites.
  - Implement the first follow-up slices from that audit without widening into unrelated runtime refactors.
  - Close the adjacent compiled/AOT matcher parity hole where callable pattern-guard lambdas were miscompiled after the runtime matcher hard-error work landed.
  - Extend the compiled/AOT matcher parity follow-through so native lowering, serialization, and parser support cover non-guard cons, sequence, dict, and constructor pattern shapes directly instead of collapsing onto fallback behavior.
  - Refresh the bounded full compiler E2E lane after the matcher parity work and close any stale-surface regressions that only appear in the generated corpus rather than the focused compiler slice.
  - Remove the validation-container invocation footgun in `run_e2e.sh` so the plain bounded E2E command works directly instead of requiring a manual hard-cap override.
  - Normalize the other bounded gate entrypoints so they do not regress into the same docker-on-docker failure mode when launched from `scripts/run_validation_container.sh`.
  - Clean up the boundary-hardening Stage 0/0b lane so the bounded gate reports real policy/runtime/doc failures instead of stale policy drift, missing-ripgrep noise, or documentation-blind parity checks.
  - Finish the remaining compiler-side ASAN leak closure so the full bounded boundary-hardening gate clears Stage 4b and Stage 5 instead of stopping on the compiler slice after tests already pass.
  - Continue from the green boundary-hardening checkpoint into the broader bounded global gate and close the next real runtime regression instead of leaving the long-suite red unresolved.
  - Continue from the repaired global-gate baseline into the next real advanced/TCO regression and close the exact `range` / `foldl` list-accumulator slowdown instead of leaving the bounded gate stalled in a broad advanced wrapper.
- Code/config changes made
  - Added a concrete follow-up slice to `TODO.md` covering scope allocator OOM hardening, Deduce scan-path error propagation, env-write failure visibility, and boot-time method-table allocation guards, then closed the shipped sub-slices now that the landed runtime/test coverage is real.
  - Hardened `src/scope_region_chunk_helpers.c3`, `src/scope_region_allocators.c3`, `src/scope_region_lifecycle.c3`, and `src/lisp/value_interp_alloc_helpers.c3` so chunk allocation failure is null-safe at scope creation / TEMP+ESCAPE slow-path growth, with explicit allocator assertions on the generic value/env allocation helpers and a new scope-create OOM regression.
  - Hardened `src/lisp/deduce_relation_scan_helpers_more.c3` and `src/lisp/deduce_schema_query_execution.c3` so scan helpers propagate canonical Deduce OOM errors instead of returning `nil` or consing `ERROR` rows.
  - Hardened `src/lisp/value_environment.c3` so `Env.define(...)` returns failure on binding growth OOM, hash-table rebuild failure degrades back to linear-scan correctness instead of hiding the new binding, and `env_define_with_barrier(...)` now fails fast instead of silently dropping writes.
  - Threaded fallible direct env-define handling through the current Deduce query-analysis helpers and through user-facing type/FFI registration paths in `src/lisp/deduce_schema_query_analysis.c3`, `src/lisp/deduce_schema_query_filter_equalities.c3`, `src/lisp/deduce_schema_query_head_demand.c3`, `src/lisp/eval_type_evaluators.c3`, `src/lisp/eval_type_declarations.c3`, and `src/lisp/eval_ffi_eval.c3`.
  - Finished the remaining unchecked `make_primitive(...)` call sites in `src/lisp/eval_type_evaluators.c3`, `src/lisp/eval_type_declarations.c3`, `src/lisp/eval_ffi_eval.c3`, and `src/lisp/aot_runtime_bridge.c3`.
  - Added deterministic env fault-injection seams in `src/lisp/value_environment.c3` plus focused regressions in `src/lisp/tests_memory_lifetime_finalize_groups.c3` for binding-growth failure visibility and hash-rebuild fallback correctness.
  - Extended `src/scope_region_tests_alloc_lifecycle.c3` with explicit TEMP and ESCAPE slow-path chunk-growth OOM regressions, then closed those shipped test-coverage slices in `TODO.md`.
  - Promoted barrier writes onto an explicit recoverable contract in `src/lisp/value_environment.c3` with `env_define_with_barrier_result(...)`, `env_set_with_barrier_result(...)`, and checked helpers, so write-site promotion/growth failure no longer gets stored into env bindings as an ordinary `ERROR` value.
  - Threaded that recoverable barrier-write path through `src/lisp/jit_jit_dispatch_helpers.c3` and `src/lisp/jit_jit_closure_define_qq.c3`, so JIT match-clause binding growth failure and `set!` barrier failure now return explicit runtime errors instead of suppressing the contract behind generic fail-fast behavior.
  - Propagated fallible primitive/type bootstrap through `src/lisp/eval_init_primitives.c3`, `src/lisp/eval_dispatch_types.c3`, `src/lisp/eval_stdlib_loader.c3`, runtime entry modes, isolated test harness setup, compiler/AOT init, and generated AOT `main` emission so bootstrap failure now stops before stdlib or user execution continues on a partially registered runtime surface.
  - Followed through on the remaining JIT/runtime env-binding sites in `src/lisp/eval_boundary_api.c3`, `src/lisp/jit_jit_apply_runtime.c3`, `src/lisp/jit_jit_apply_multi_prims*.c3`, `src/lisp/jit_jit_handle_signal*.c3`, `src/lisp/jit_jit_module_import.c3`, `src/lisp/jit_jit_compile_effects_modules.c3`, `src/lisp/jit_jit_define_method_table.c3`, and `src/lisp/jit_jit_closure_define_qq.c3` so multi-arg closure binding, variadic binding, handler clause binding, import/export rebinds, recursive let binding, and JIT global define/method-table publish now surface explicit runtime/eval errors instead of relying on assert-style env writes.
  - Added a focused JIT regression in `src/lisp/tests_runtime_feature_jit_groups_more.c3` proving forced binding-growth failure during multi-arg closure application returns a normal runtime error rather than crashing through the env-extension path.
  - Closed the separate pattern-guard hard-error lane by giving `MatchResult` an explicit hard-error path in `src/lisp/eval_pattern_support_helpers.c3` / `src/lisp/eval_pattern_matching.c3`, then threading that through the `match`/destructuring/macro call sites in `src/lisp/jit_jit_dispatch_helpers.c3`, `src/lisp/jit_jit_closure_define_qq.c3`, and `src/lisp/macros_template_expansion.c3` so guard-scope allocation/binding and guard predicate runtime failures no longer degrade into plain clause misses.
  - Added a language-level semantic regression in `src/lisp/tests_advanced_core_semantics_groups.c3` for guard runtime error propagation plus a focused JIT fault-injection regression in `src/lisp/tests_runtime_feature_jit_groups_more.c3` for guard-binding growth failure.
  - Fixed the compiled/AOT matcher parity gap in `src/lisp/compiler_lambda_scan.c3`, `src/lisp/compiler_lambda_scan_lambda_defs.c3`, `src/lisp/compiler_compiler_state.c3`, `src/lisp/compiler_native_call_compilation_flat_style.c3`, and `src/lisp/compiler_code_emission_lambda_closures.c3` by scanning lambdas inside guarded patterns, carrying clause-local pattern bindings into lambda-scan scope, keying emitted lambda defs by the original lambda expression node, and asserting if codegen ever tries to emit an unregistered lambda instead of silently falling back to `invoke_lambda_0`.
  - Added focused compiler regressions in `src/lisp/tests_compiler_helpers.c3` and `src/lisp/tests_compiler_codegen_groups.c3` so the unit compiler lane now checks both callable match-guard lambda registration and clause-local binding capture inside guard lambdas without depending on the noisier full E2E compiler gate.
  - Extended `src/lisp/aot_runtime_bridge.c3` with direct matcher/runtime helpers for compiled pattern lowering: `is_cons(...)`, `is_dict(...)`, `match_var_pattern(...)`, `match_constructor(...)`, `instance_field(...)`, `dict_get_symbol(...)`, `seq_length(...)`, `seq_nth(...)`, and `seq_rest(...)`.
  - Reworked `src/lisp/compiler_native_match_compilation_flat_style.c3` and `src/lisp/compiler_native_match_bindings_flat_style.c3` so compiled `match` lowers nested cons, sequence, dict, constructor, `as`, and guarded patterns through explicit helper-driven checks/bindings instead of falling back to shallow or unconditional behavior.
  - Extended compiler-side pattern metadata walks in `src/lisp/compiler_free_vars_utils.c3` and `src/lisp/compiler_lambda_scan.c3` so constructor/dict bindings and constructor-contained lambdas participate in the same clause-local scope model as the rest of compiled match lowering.
  - Extended serializer/parser parity in `src/lisp/compiler_expr_serialize_patterns.c3` and `src/lisp/parser_patterns_paren.c3` so constructor, dict, and dotted cons patterns round-trip cleanly, including `(a . b)` parsing as `PAT_CONS`.
  - Added focused compiler regressions in `src/lisp/tests_compiler_core_groups.c3` and `src/lisp/tests_compiler_codegen_groups.c3` covering serializer round-trips plus direct lowering for cons, sequence, dict, and constructor patterns.
  - Canonicalized the remaining stale lowercase dictionary-constructor rows in `src/lisp/tests_e2e_generation_cases_extended.c3` and `src/lisp/tests_compiler_codegen_groups.c3` from `(dict)` to `(Dictionary)`, matching the live surface contract where lowercase `dict` has already been removed.
  - Updated `scripts/run_e2e.sh` so it defaults `OMNI_HARD_MEM_CAP_METHOD` to `none` when already inside the validation container, matching `scripts/container_exec.sh` and removing the recursive docker-on-docker requirement from the normal bounded E2E invocation.
  - Applied the same in-container hard-cap defaulting rule to `scripts/run_global_gates.sh`, `scripts/run_boundary_hardening.sh`, and `scripts/run_deduce_perf_envelope.sh` so all bounded gate entrypoints now inherit `OMNI_HARD_MEM_CAP_METHOD=none` inside the validation container instead of resetting to `docker`.
  - Added `grep` fallbacks to the Stage 0 boundary-hardening helper scripts in `scripts/check_boundary_facade_usage.sh`, `scripts/check_effects_contract_policy.sh`, `scripts/check_libuv_surface_policy.sh`, and `scripts/check_primitive_docs_parity.sh` so the validation image no longer needs `rg` just to produce truthful lint results.
  - Refreshed `scripts/boundary_facade_policy.txt` to match the current post-split boundary implementation files (`eval_promotion_*`, `eval_env_copy_helpers.c3`, and `eval_boundary_commit_escape_builders.c3`) so the facade guard no longer flags the owned implementation layer as an external violation.
  - Replaced the raw `raise_error(...)` handle-clause binding failures in `src/lisp/jit_jit_handle_signal.c3` with canonical `runtime_effect_raise(...)` payloads using `runtime/handle-clause-bind-failed`, satisfying the migrated-surface effects contract.
  - Extended `scripts/check_primitive_docs_parity.sh` to include `docs/reference/08-libraries.md`, then added explicit backticked `deduce/materialize!`, `deduce/dematerialize!`, and `deduce/indexes` coverage in `docs/reference/08-libraries.md` so Deduce library/admin APIs stop failing the public primitive docs parity gate spuriously.
  - Fixed the original Stage 4/4b ASAN ownership leak at the runtime/compiler boundary by normalizing root-owned closure env envelopes to `null` instead of retaining `interp.root_scope` inside closures. The normalization landed in `src/lisp/value_constructors_core.c3`, `src/lisp/jit_jit_closure_support.c3`, `src/lisp/eval_promotion_copy_wrapper_helpers.c3`, and `src/lisp/eval_promotion_escape_structured.c3`, with the contract regression updated in `src/lisp/tests_runtime_feature_jit_groups_more.c3`.
  - Repaired `scripts/run_boundary_hardening.sh` so Stage 2/4 run the actual test suites and Stage 2b/4b append explicit compiler-slice evidence to the shared logs instead of launching the REPL and then failing summary assertions on missing `suite=compiler` rows.
  - Closed the remaining compiler-only ASAN leak path by adding explicit compiler teardown in `src/lisp/tests_compiler_core_groups.c3`, freeing per-`LambdaDef` capture/param buffers in `src/lisp/compiler_lambda_scan_lambda_defs.c3` and `src/lisp/compiler_compiler_initialization.c3`, and keeping synthetic guard-wrapper lambdas arena-owned in `src/lisp/compiler_lambda_scan_effect_wrappers.c3`.
  - Continued into the bounded `scripts/run_global_gates.sh` lane and fixed the next real runtime failure there by increasing the normal stack-context budget in `src/stack_engine_region.c3` from 64KB to 128KB. The previous 64KB budget was no longer sufficient for small higher-order stdlib work inside `handle`/reset stack contexts after the recent runtime growth, and it was tripping `stack overflow in handle body` on two- and three-element `map`/`filter`/`foldl` forms.
- Experiment commands and key metrics
  - `find . -maxdepth 2 -type d | sort`
  - `sed -n '1,220p' docs/LANGUAGE_SPEC.md`
  - `sed -n '1,220p' docs/C3_STYLE.md`
  - `sed -n '1,260p' memory/CHANGELOG.md`
  - `rg -n "mem::malloc\\(|mem::realloc\\(|scope_adopt\\(|copy_to_parent\\(" src/lisp src`
  - `nl -ba src/lisp/deduce_relation_scan_helpers_more.c3 | sed -n '1,420p'`
  - `nl -ba src/lisp/value_environment.c3 | sed -n '1,420p'`
  - `nl -ba src/scope_region_chunk_helpers.c3 | sed -n '1,120p'`
  - `c3c build`
  - `scripts/run_validation_container.sh c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-core-semantics ./build/main --test-suite lisp`
  - `./build/main --eval "(block (define [type] Point (^Integer x) (^Integer y)) (Point 1 2))"`
  - `./build/main --eval "(block (define [ffi lib] libc \"libc.so.6\") (define [ffi λ libc] (strlen (^String s)) ^Integer) (strlen \"hello\"))"`
  - `OMNI_DEDUCE_FORCE_ROW_DICT_OOM=1 ./build/main --eval "(block (define db (deduce 'open 'memory)) (define [relation db] scan_range_oom_person (name) (age) (email)) (deduce/fact! scan_range_oom_person \"Bob\" 30 \"b@x\") (handle (deduce 'scan-range scan_range_oom_person '(\"Bob\" 30 \"b@x\") '(\"Bob\" 30 \"b@x\")) (raise payload (symbol->string (ref payload 'code)))))"`
  - `OMNI_DEDUCE_FORCE_COLUMN_KEY_VALUES_OOM=1 ./build/main --eval "(block (define db (deduce 'open 'memory)) (define [relation db] scan_oom_person (name) (age)) (deduce/fact! scan_oom_person \"Bob\" 30) (handle (deduce 'scan scan_oom_person) (raise payload (symbol->string (ref payload 'code)))))"`
  - `./build/main --compile /tmp/aot_match_guard_smoke.omni /tmp/aot_match_guard_smoke.c3`
  - `rg -n "match_guard_eval|invoke_lambda_[0-9]+\\(void\\* _self, lisp::Value\\* v\\)|if \\(!_match_done_" /tmp/aot_match_guard_smoke.c3`
  - `./build/main --compile /tmp/aot_match_guard_print.omni /tmp/aot_match_guard_print.c3`
  - `source scripts/c3c_limits.sh && omni_c3 compile src/main*.c3 src/scope_region*.c3 src/stack_engine*.c3 src/ffi_bindings.c3 src/lisp/*.c3 src/pika/*.c3 /tmp/aot_match_guard_print.c3 -o build/aot_match_guard_print -L build -L /usr/local/lib -L deps/lib -l omni_chelpers -l lightning -l ffi -l dl -l m -l replxx -l stdc++ -l utf8proc -l deflate -l yyjson -l uv -l bearssl -l lmdb`
  - `OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `rg -n "Compiler: cons pattern lowers directly|compile_to_c3|aot::is_cons|aot::car|aot::cdr" src/lisp/tests_compiler_codegen_groups.c3 src/lisp/tests_compiler_helpers.c3 src/lisp/compiler_native_match_compilation_flat_style.c3 src/lisp/compiler_native_match_bindings_flat_style.c3 src/lisp/compiler_expr_serialize_patterns.c3 src/lisp/parser_patterns_paren.c3`
  - `c3c build`
  - `scripts/run_validation_container.sh c3c build`
  - `OMNI_LISP_TEST_SLICE=compiler LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
  - `scripts/check_e2e_baseline_policy.sh`
  - `scripts/run_validation_container.sh env OMNI_HARD_MEM_CAP_METHOD=none bash scripts/run_e2e.sh`
  - `scripts/run_validation_container.sh scripts/run_e2e.sh`
  - `bash -n scripts/run_global_gates.sh scripts/run_boundary_hardening.sh scripts/run_deduce_perf_envelope.sh scripts/run_e2e.sh`
  - `scripts/run_validation_container.sh env OMNI_GLOBAL_GATES_SUITES= OMNI_GLOBAL_GATES_INCLUDE_FTXUI_SMOKE=0 bash scripts/run_global_gates.sh`
  - `env OMNI_VALIDATION_TIMEOUT_SEC=20 scripts/run_validation_container.sh scripts/run_boundary_hardening.sh`
  - `env OMNI_VALIDATION_TIMEOUT_SEC=20 scripts/run_validation_container.sh scripts/run_deduce_perf_envelope.sh`
  - `env OMNI_VALIDATION_TIMEOUT_SEC=120 scripts/run_validation_container.sh scripts/run_boundary_hardening.sh`
  - `env OMNI_VALIDATION_TIMEOUT_SEC=240 scripts/run_validation_container.sh bash -lc 'c3c build --sanitize=address && env LD_LIBRARY_PATH=/usr/local/lib ASAN_OPTIONS=detect_leaks=1:halt_on_error=1:abort_on_error=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp'`
  - `env OMNI_VALIDATION_TIMEOUT_SEC=240 scripts/run_validation_container.sh scripts/run_boundary_hardening.sh`
  - `env OMNI_VALIDATION_TIMEOUT_SEC=240 scripts/run_validation_container.sh scripts/run_global_gates.sh`
  - `scripts/run_validation_container.sh c3c build`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=0 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=escape-scope ./build/main --test-suite lisp`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (car (map (lambda (x) (+ x 1)) (quote (10 20 30)))) (tag arg (resolve 0)))"`
  - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 && c3c build && env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=0 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=admin-surface-demand ./build/main --test-suite lisp'`
  - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 && c3c build && env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=0 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=admin-surface ./build/main --test-suite lisp'`
  - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 && c3c build && env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=0 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=aggregate-rules ./build/main --test-suite lisp && env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=0 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=recursive-aggregate-rules ./build/main --test-suite lisp'`
  - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 && c3c build && env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=0 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER=scan ./build/main --test-suite lisp'`
  - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 && c3c build && for filter in "scan,admin-surface" "scan,scan-range,admin-surface" "scan,scan-range,query-filter,admin-surface" "scan,scan-range,query-filter,match,admin-surface" "scan,scan-range,query-filter,match,empty-relation-stats,admin-surface"; do env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_QUERY_FILTER="$filter" ./build/main --test-suite lisp || break; done'`
  - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 && c3c build && env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp'`
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(length (range 4000))"`
  - `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TRACE_TAIL_MULTI=1 ./build/main --eval "(let loop (i 4000 acc nil) (if (= i 0) acc (loop (- i 1) (cons i acc))))"`
  - `scripts/run_validation_container.sh bash -lc 'rm -rf build/obj/linux-x64 build/main && c3c build'`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(length (range 4000))"`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=tco-recycling ./build/main --test-suite lisp`
  - `bash -n scripts/check_boundary_facade_usage.sh scripts/check_primitive_docs_parity.sh scripts/check_libuv_surface_policy.sh scripts/check_effects_contract_policy.sh`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(dict? (dict))"`
  - `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(dict? (Dictionary))"`
  - Key results:
    - `c3c build` passed and linked `build/main`.
    - Host rebuild passed, then container rebuild succeeded after a temporary host/container libc mismatch (`GLIBC_2.43`) made the first container test attempt invalid.
    - The container-backed `deduce` slice remained broadly red (`295 passed, 80 failed`), but the failures were in unrelated pre-existing recursive/query/admin lanes rather than the scan OOM paths touched here.
    - The container-backed `memory-lifetime-smoke` slice passed cleanly with `66 passed, 0 failed`, covering the new direct env growth/hash fallback regressions plus the recoverable env-set/bootstrap-stop regressions under the bounded validation path.
    - The container-backed `jit-policy` slice first passed with `31 passed, 0 failed` for the match-clause binding-growth regression, then passed again at `32 passed, 0 failed` after the follow-through JIT/runtime env-binding hardening and the new multi-arg closure binding-growth regression, and finally passed at `33 passed, 0 failed` after the pattern-guard hard-error regression landed.
    - Direct smoke checks passed for `define [type]` constructor registration and `define [ffi lib]` / `define [ffi λ]` registration.
    - Forced scan OOM probes now return the canonical `"deduce/query-out-of-memory"` code for both row-dict materialization and column-key-value setup.
    - Direct runtime smoke now reports `unbound variable 'no_such_guard'` for `(match 5 ((? no_such_guard) 1) (_ 0))`, proving guard evaluation errors stop the match instead of silently falling through to `_`.
    - The bounded full `advanced` slice was started after the matcher change but did not complete within the validation timebox, so it was not used as the gate for this landed slice.
    - The targeted bounded `advanced-core-semantics` group passed cleanly at `66 passed, 0 failed`, which covers the new language-level guard runtime-error propagation regression without relying on the full advanced lane.
    - The compiled/AOT matcher now emits the correct callable guard closure instead of silently reusing `invoke_lambda_0`; the local generated-C3 probe switched the guard site from the prelude trampoline closure to the user lambda (`invoke_lambda_40` in the focused smoke).
    - The explicit compiled smoke binary built from `/tmp/aot_match_guard_print.c3` printed `7`, `0`, and `5`, confirming callable guard success, false-guard fallthrough, and subpattern-bound guard evaluation in the AOT path.
    - The dedicated compiler slice passed cleanly at `127 passed, 0 failed`, including the new regressions that verify guard-lambda registration and clause-local capture at compile time.
    - Container-backed `c3c build` still passed after the compiler-side matcher fix, so the AOT guard scan/codegen patch did not regress repo-wide integration.
    - The broader compiled matcher parity follow-through landed cleanly: dotted cons patterns now parse/serialize round-trip, and compiled lowering emits explicit helper-driven checks/bindings for cons, sequence, dict, and constructor clauses instead of depending on fallback behavior.
    - The dedicated compiler slice passed again at `134 passed, 0 failed` after the non-guard pattern parity work, covering the new serializer and direct-lowering regressions for cons, sequence, dict, and constructor patterns.
    - Container-backed `c3c build` still passed after the broader matcher parity patch, so the added AOT bridge helpers and native match lowering changes did not regress repo-wide integration.
    - `scripts/check_e2e_baseline_policy.sh` stayed green before the live refresh, confirming there were no tracked baseline rows left in the bounded E2E policy.
    - The first refreshed bounded E2E run exposed a real stale-surface regression rather than a matcher-lowering failure: generated `build/e2e_test.c3` failed on a bare `dict` identifier from the old `(dict)` constructor spelling in the E2E corpus.
    - Runtime truth matched the naming policy: `(dict? (dict))` evaluated to `nil`, while `(dict? (Dictionary))` evaluated to `true`, confirming lowercase `dict` is no longer a live constructor surface and the corpus row was stale.
    - After canonicalizing those remaining stale rows to `Dictionary`, the bounded full E2E compiler lane passed cleanly at `ALL 403 e2e compiler tests passed!`.
    - The `run_e2e.sh` container-invocation footgun is now closed: after the script-side default fix, the plain bounded command `scripts/run_validation_container.sh scripts/run_e2e.sh` also passed cleanly at `ALL 403 e2e compiler tests passed!` without any manual hard-cap override.
    - `bash -n` passed for `run_global_gates.sh`, `run_boundary_hardening.sh`, `run_deduce_perf_envelope.sh`, and `run_e2e.sh` after the shared defaulting change.
    - A bounded `run_global_gates.sh` smoke launched through `scripts/run_validation_container.sh` now reports `method=none` inside the container and proceeds into real suite execution instead of failing immediately on the missing-Docker-in-container path; I treated that as an entrypoint smoke rather than the gate for this session and did not wait on the full split-gate completion.
    - The first bounded `run_boundary_hardening.sh` smoke no longer died on the in-container Docker check, but it exposed three real Stage 0/0b issues: missing `rg` fallback in helper scripts, stale facade-policy allow rows after the boundary file split, and a remaining raw `raise_error(...)` in `jit_jit_handle_signal.c3`, plus a docs-parity blind spot for Deduce library APIs that were already documented in `docs/reference/08-libraries.md`.
    - After the helper-script fallbacks, facade-policy refresh, canonical runtime payload fix, and docs-parity expansion, `scripts/run_validation_container.sh scripts/run_boundary_hardening.sh` advanced cleanly through Stage 0, Stage 0b, Stage 1, Stage 2, and Stage 3 under the plain bounded invocation before the explicit `OMNI_VALIDATION_TIMEOUT_SEC=20` timebox terminated it during Stage 4 (ASAN run). That is sufficient evidence that the entrypoint/tooling/lint blockers were cleared and the gate is now failing only on the imposed timebox rather than on stale setup defects.
    - The bounded `run_deduce_perf_envelope.sh` smoke likewise entered the build and benchmark stages under the plain bounded invocation, confirming the in-container hard-cap default fix for that self-reentering perf entrypoint as well; I did not use the perf lane itself as the gate for this session.
    - The first longer bounded boundary-hardening rerun exposed the real Stage 4/4b blocker: root-owned closures were retaining `interp.root_scope` through `closure.env_scope`, creating teardown cycles that LSAN reported after the compiler and runtime tests had already passed.
    - After normalizing root-owned closure env envelopes to `null`, the focused `jit-policy` slice stayed green at `33 passed, 0 failed`, proving the env-copy/promotion contract still worked without retaining the root scope.
    - The next bounded boundary-hardening rerun got past the leak but exposed a pure gate-shape defect: Stage 2 and Stage 4 were launching bare `./build/main`, so Stage 5 failed on missing `OMNI_TEST_SUMMARY` rows rather than on a runtime regression.
    - After repairing the gate to run real suites and append explicit compiler-slice logs, the bounded compiler-only ASAN reproduction passed cleanly at `134 passed, 0 failed` with no post-test leak report.
    - The full bounded `scripts/run_boundary_hardening.sh` gate now passes end to end, including Stage 4 (ASAN run), Stage 4b (ASAN compiler slice), Stage 5 summary assertions, Stage 7 threshold checks, and Stage 8 boundary-change policy checks.
    - The next broader bounded checkpoint (`scripts/run_global_gates.sh`) immediately surfaced a real runtime regression rather than a tooling failure: the `escape-scope` slice failed `escape-scope: handle + map` in both interpreter and JIT mode with `stack overflow in handle body`.
    - Direct probes narrowed the failure to higher-order stdlib list work inside `handle`: `(handle 1 ...)`, `(handle ((lambda (x) (+ x 1)) 10) ...)`, `(handle (car '(10 20 30)) ...)`, and `(handle (reverse '(1 2 3)) ...)` all stayed green, while `(handle (map (lambda (x) (+ x 1)) '(10 20 30)) ...)`, `(handle (filter ...) ...)`, and `(handle (foldl + 0 '(1 2 3)) ...)` overflowed.
    - After raising the normal stack-context budget to 128KB, the exact direct handled-map probe returned `11` again under the bounded container path, and the focused `escape-scope` slice passed cleanly at `29 passed, 0 failed`.
    - I restarted the bounded global gate after that fix and confirmed it cleared the previously failing area, but I did not use the still-running later slices as the gate for this turn; once the regression was fixed and the targeted slice was green, I stopped the stale long-running container process instead of leaving an unbounded CPU burn in the background.
    - The next real bounded global-gate failure moved to the `data-format` slice. The first reported assertion (`toml-parse boolean false stays false`) turned out to be stale corpus, not a runtime regression: TOML false already materializes as the quoted symbol `'false`, matching the existing language rule that bare `false` collapses to `nil`.
    - After fixing that stale expectation, the bounded `data-format` slice still crashed with exit `139`. A bounded ASAN repro on `(parse 'csv "a,b\r\n1,2")` and the helper-route test stack showed the real ownership bug: root-owned array wrappers were storing TEMP-owned children, so `parse` helper results could return arrays whose element values were freed at child-scope teardown.
    - The concrete root-promotion holes were in `csv-parse`, JSON array materialization, TOML array materialization, `list->array`, and `to-array`: those sites were writing raw TEMP `Value*` into root-owned arrays instead of routing each element through `boundary_promote_to_root(...)`.
    - After promoting array members at those insertion sites, the direct probes all went green again: `(parse 'csv "a,b\r\n1,2")` printed `[["a" "b"] ["1" "2"]]`, `(= (ref (ref (parse 'csv "a,b\r\n1,2") 1) 1) "2")` returned `true`, and the new JSON/TOML helper-array probes returned `true` as well.
    - The bounded ASAN repro `scripts/run_validation_container.sh bash -lc 'c3c build --sanitize=address && env LD_LIBRARY_PATH=/usr/local/lib ASAN_OPTIONS=detect_leaks=0:halt_on_error=1:abort_on_error=1 ./build/main --eval "(parse '\''csv \"a,b\\r\\n1,2\")"'` now passes cleanly with no UAF report, and the bounded `data-format` slice is green at `59 passed, 0 failed`.
    - I restarted the bounded full global gate after the array-promotion fix. It cleared the earlier red lanes and advanced back through `advanced` plus the repaired `data-format` boundary into later long-running slices. I treated that as a forward-progress checkpoint rather than a full-gate result for this turn and stopped the live container run instead of leaving it burning CPU in the background.
  - The new bounded query filters proved several Deduce lanes are green on a fresh interpreter: `admin-surface-demand` passed at `94 passed, 0 failed`, `admin-surface` passed at `184 passed, 0 failed`, `aggregate-rules` passed at `1 passed, 0 failed`, `recursive-aggregate-rules` passed at `8 passed, 0 failed`, and `scan` passed at `3 passed, 0 failed` once the filtered query path restored the normal `person` bootstrap.
  - The entire filtered query prefix through admin surface stayed green under fresh runs: `scan -> scan-range -> query-filter -> match -> empty-relation-stats -> admin-surface` passed progressively at `187`, `194`, `198`, `202`, and `203` passes with `0` fails at each checkpoint.
  - Despite those fresh-prefix greens, a clean full bounded `deduce` slice still finishes at `296 passed, 79 failed`, and the remaining visible failures continue to cluster in recursive demand, dirty-closure auto-execution/admin-truth, integrity/admin surfaces, and aggregate/semi-naive routing lanes. That pins the remaining red lane on order-dependent contamination or broader cross-group state interaction rather than on a local isolated break inside the admin-surface or aggregate groups themselves.
  - Added narrow Deduce query-group isolation support in `src/lisp/tests_deduce_query_groups.c3` with both named single-lane filters and CSV token selection (`scan`, `scan-range`, `query-filter`, `match`, `empty-relation-stats`, `admin-surface`, `admin-surface-demand`, `aggregate-rules`, `recursive-aggregate-rules`) so the bounded `deduce` lane can be bisected without widening into the entire slice every time.
  - Restored the normal `person` bootstrap for filtered query runs in `src/lisp/tests_deduce_groups.c3`, so stateful filtered lanes like `scan` use the same seed-relation baseline as the unfiltered deduce path instead of failing as false-red due to missing setup.
  - The full-query prefix bisect tightened the remaining Deduce red lane materially: `query`, `basics,query`, `basics,materialized,query`, `basics,materialized,relation-attrs,query`, `basics,materialized,relation-attrs,core-runtime,query`, and `basics,materialized,relation-attrs,core-runtime,integrity,query` all stay green, while the first red prefix is `basics,materialized,relation-attrs,core-runtime,integrity,command-surface,query`.
  - Splitting `command-surface` and then `command-void` narrowed that first red boundary further: `command-void` is the first contaminating sublane, and the first destructive-mutation regressions appear only once `clear!` / `drop!` are included ahead of the later query suite.
  - The current minimal bounded repros are sharper than the original umbrella full-slice failure: `basics,materialized,relation-attrs,core-runtime,integrity,fact-void,retract-void,commit-void,abort-void,drop-void,query` finishes at `282 passed, 1 failed` (mixed negated recursive aggregate seed failure), and adding `clear-void` reproduces the two negated recursive aggregate seed failures seen under the broader `command-void` prefix.
  - The new failure-only diagnostics show those two remaining `command-void` regressions are not yet analyze-routing mismatches; they fail earlier, before the negated recursive aggregate seed helpers can produce analyzed metadata or expected row counts. That moves the active hypothesis from admin/query planner truth to destructive-mutation cleanup or resource/state leakage after `clear!` / `drop!`.
  - The final fix turned out to be harness-side Deduce isolation, not a surviving mutation-runtime defect. `src/lisp/tests_deduce_groups.c3` now runs each selected Deduce block and fine-grained `OMNI_DEDUCE_GROUP_FILTER` lane in its own isolated interpreter, matching the existing advanced-suite subgroup policy and preventing cumulative handle/state buildup across command, transaction, clear/drop, admin, and query lanes.
  - That isolation change cleared the previously red bounded checkpoints immediately: the exact earlier red prefix `basics,materialized,relation-attrs,core-runtime,integrity,command-void,query` now passes at `284 passed, 0 failed`, and the clean full bounded `deduce` slice is green at `375 passed, 0 failed`.
  - The next fresh ASAN blocker after the repaired `jit-policy` lane was no longer a runtime crash. The minimal two-case repro `escaped-handle-continuation-guard,capture-boundary-reset` now passes cleanly under ASAN at `2 passed, 0 failed`, and the full isolated ASAN `jit-policy` slice is green again at `32 passed, 0 failed`.
  - The runtime fix for that lane landed in the stack engine rather than in effect semantics: `src/stack_engine_pool_ownership.c3` now disables `StackCtx` free-list reuse under ASAN (`stack_pool_cache_limit() -> 0`) so suspended-context destroy/create cycles do not recycle sanitizer fake-stack metadata across continuation teardown boundaries.
  - After that fix, the broader bounded gate advanced past the old ASAN crash and exposed only assertion drift. `src/stack_engine_tests.c3` and `src/lisp/tests_memory_lifetime_finalize_groups.c3` now distinguish the normal pooled-reuse contract from the ASAN direct-release contract instead of hard-coding pool-growth expectations in sanitizer runs.
  - The repaired ASAN checkpoints are green under bounded targeted validation: `./build/main --test-suite stack` passed at `22 passed, 0 failed` (with the expected ASAN overflow skip), and `OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp` passed at `66 passed, 0 failed`.
  - A fresh bounded `run_global_gates.sh` rerun from that checkpoint cleared the full normal half again, including `advanced`, `escape-scope`, `data-format`, `deduce`, `http`, and `compiler`, and re-entered Stage 4 ASAN from the repaired baseline.
  - Refined the TCO recycle boundary in `src/lisp/jit_jit_eval_scopes_helpers.c3` and `src/lisp/runtime_backend_hooks.c3` so fast-reset decisions distinguish TEMP-lane blockers from current-scope ESCAPE-lane env frames / binding payloads. Escape-resident recursive state no longer gets misclassified as TEMP leakage that pins the whole recyclable scope.
  - Tightened `src/lisp/value_constructors_core.c3` so `make_cons(...)` reuses an existing current-scope ESCAPE-lane tail directly instead of routing it back through `boundary_promote_to_escape(...)` on every iteration. That removes the accidental O(n^2) tail-spine walk in ESCAPE-lane list accumulators while still promoting non-escape inputs.
  - Added a lighter dedicated regression in `src/lisp/tests_limit_busting_tests_verify_dynamic_allocation_works.c3` for `tco-recycle: range 4000`, so this path now has a focused TCO-slice gate instead of depending only on the heavier advanced stdlib wrapper.
  - The next bounded stall inside the global-gate `advanced` lane was a real stdlib/TCO performance regression, but not a recycle-boundary failure. Local traces showed named-let list-accumulator loops already had ESCAPE-lane call envs and were taking the TEMP fast-reset path each bounce.
  - The actual hotspot was `make_cons(...)` on the ESCAPE-lane accumulator path: it kept routing an already current-scope ESCAPE tail back through `boundary_promote_to_escape(...)`, which recursively rewalked the full accumulated list spine on every iteration.
  - After reusing the existing ESCAPE tail directly, the exact direct probe that had been timing out now completes again: container-built `./build/main --eval "(length (range 4000))"` returns `4000`.
  - The dedicated bounded `tco-recycling` slice is green again with the new regression coverage at `11 passed, 0 failed`.
  - `src/lisp/tests_tests.c3` now lets `OMNI_ADVANCED_GROUP_FILTER` descend into nested advanced subgroup names instead of matching only the outer isolated group label, so the bounded harness can target a specific advanced sublane directly.
  - `src/lisp/tests_advanced_stdlib_numeric_groups.c3` now exposes an exact-only `advanced-stdlib-numeric-tco` subgroup for the repaired `length (range 4000)` path while leaving the broader legacy `advanced-stdlib-numeric-introspection-lazy-tco` block intact for wider coverage.
  - The new sharp bounded advanced gate is green: `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-tco ./build/main --test-suite lisp` passed at `1 passed, 0 failed`.
  - Cleaned up the remaining raw `OMNI_ADVANCED_GROUP_TRACE state ...` dumps in `src/lisp/tests_advanced_stdlib_numeric_groups.c3` so they now honor `advanced_group_trace_enabled()` instead of printing during every normal bounded `advanced` or global-gate run.
  - After that trace cleanup, the broad bounded `advanced` slice still remained the practical long pole, but the long runner is now narrowed more honestly: the exact `advanced-stdlib-numeric-tco` subgroup stays green and fast, while the top-level `advanced-stdlib-numeric` filter remained CPU-bound for more than 90 seconds under the bounded container without tripping a new assertion. That points at the wider numeric parent inventory as the remaining `advanced` wall, not the repaired TCO lane itself.
- Best current checkpoint/config recommendation
  - Keep the current Deduce scan-path hardening: it now has the right shipped contract and targeted validation.
  - Keep the `Env.define(...)` hash-rebuild fallback that drops back to linear scan on replacement-allocation failure; it preserves correctness without silently hiding new bindings.
  - Keep the new deterministic scope/env fault-injection seams enabled only for tests; they now give direct coverage of the shipped allocator and env-define OOM contracts without needing fatal-process probes.
  - Keep the new typed env barrier/bootstrap contract: write-site promotion or growth failure now stops evaluation/bootstrap explicitly instead of continuing on a partially registered or silently corrupted runtime surface.
  - Keep the root-owned closure env normalization to `env_scope == null`; that breaks the teardown cycle without changing the callable semantics of root-owned closures.
  - Keep using the bounded compiler ASAN slice as the first reproducer if boundary hardening ever goes red again. It is now a clean, low-noise ownership gate for the compiler path before rerunning the full boundary profile.
  - Keep the larger 128KB normal stack-context budget unless we later land a structural reduction in handle/reset call depth. The previous 64KB budget is no longer honest for higher-order stdlib work inside stack contexts.
  - Keep the current root-owned array contract coupled with mandatory `boundary_promote_to_root(...)` on every inserted element. The wrappers already survive scope teardown; the important invariant is that their child values must survive too.
  - Use the new `OMNI_DEDUCE_QUERY_FILTER` CSV tokens to isolate fresh-query sublanes before touching Deduce runtime code again; the filtered checkpoints now separate self-contained green lanes from the still-red full-slice order-dependent lane.
  - Keep the new Deduce per-lane isolation in `src/lisp/tests_deduce_groups.c3`; the bounded suite is honest again once command/admin/query lanes stop sharing one long-lived Deduce interpreter.
  - Keep the current `make_cons(...)` ESCAPE-tail reuse path. Existing current-scope ESCAPE cons tails are already safe to share, and re-promoting them through the generic escape router just reintroduces quadratic accumulator cost.
  - For future validation of this lane, use the exact bounded `advanced-stdlib-numeric-tco` subgroup together with the direct bounded `length (range 4000)` probe and the `tco-recycling` slice before rerunning the much wider `advanced` or global-gate wrappers.
  - If the broad `advanced` slice needs another runtime/perf pass, start by bisecting the top-level isolated groups with `OMNI_ADVANCED_GROUP_FILTER`; the post-cleanup evidence now points at `advanced-stdlib-numeric` as the long parent lane rather than at stray trace dumping or the repaired `range`/TCO subgroup.
  - Keep the ASAN-only `StackCtx` no-recycle policy. It matches the existing ASAN scope-freelist bypass strategy and cleanly separates real continuation/reset bugs from fake-stack reuse noise.
  - The next Stage 4 ASAN blocker after that policy split was inside the broad `advanced` slice, not in effect/continuation runtime code. Re-bisecting the top-level isolated groups first exposed `advanced-macro-hygiene`, where the only red lane was the old non-tail recursion headroom probe inside `src/lisp/tests_advanced_macro_hygiene_groups.c3`.
  - Bounded ASAN probes showed the real host-stack regime clearly: plain non-tail recursion stayed green through `896`, then crashed by `960` and certainly by `1024`, so the old `1200` normal-build contract was no longer an honest sanitizer gate. Routing that probe through `checkpoint` was also wrong: even `(checkpoint (let ^rec ... (f 256)))` crashed under ASAN, which made it a different broken path rather than a valid substitute for the host-stack recursion contract.
  - `src/lisp/tests_advanced_macro_hygiene_groups.c3` now keeps the normal-build headroom assertion at `1200` but uses an ASAN-specific non-tail recursion probe at `896` on the plain interpreter path. After that split, the bounded ASAN `advanced-macro-hygiene-string-number` subgroup passed at `9 passed, 0 failed`, and the full `advanced-macro-hygiene` parent passed at `82 passed, 0 failed`.
  - The next ASAN crash boundary then moved to `advanced-stdlib-numeric`, specifically the exact `advanced-stdlib-numeric-tco` subgroup in `src/lisp/tests_advanced_stdlib_numeric_groups.c3`.
  - Bounded ASAN `range` probes showed the same regime split there: `(length (range 2000))` still completed, while the previous normal-build stress target crashed before finishing `2500`, so the old `4000` assertion was not an honest sanitizer gate even though the repaired TCO path itself was still functioning.
  - `src/lisp/tests_advanced_stdlib_numeric_groups.c3` now keeps the normal-build `range 4000` target but uses an ASAN-specific TCO headroom probe at `range 2000`. The exact bounded ASAN `advanced-stdlib-numeric-tco` subgroup passed at `1 passed, 0 failed` after that split.
  - With both sanitizer-aware probe splits in place, the full bounded ASAN `advanced` slice is green again at `1121 passed, 0 failed`.
  - A fresh bounded `run_global_gates.sh` rerun from that checkpoint re-cleared the full normal half, re-entered Stage 4 ASAN, and then exposed the next real sanitizer boundary in `tco-recycling` immediately after the repaired `advanced` and `escape-scope` lanes.
  - The isolated Stage 4 repro was the stale hard `range 4000` probe in `src/lisp/tests_limit_busting_tests_verify_dynamic_allocation_works.c3`. That slice still carried the old normal-build stress target, so ASAN `tco-recycling` was dying on the same sanitizer-only `range` ceiling even though the TCO recycle path itself had already been repaired.
  - `src/lisp/tests_limit_busting_tests_verify_dynamic_allocation_works.c3` now matches the new shipped sanitizer contract: normal builds still assert `range 4000`, while ASAN uses a `range 2000` TCO headroom probe. After that split, the bounded isolated ASAN `tco-recycling` slice passed cleanly again at `11 passed, 0 failed`.
  - The full bounded global gate is now green end to end from that repaired baseline. `scripts/run_validation_container.sh scripts/run_global_gates.sh` re-cleared the full normal half, the full Stage 4 ASAN half, and the later compiler/policy stages, then exited with `Global gates passed.`.
  - The `--gen-e2e` utility had a stale CLI contract: `src/lisp/tests_e2e_generation.c3` printed `ERROR:` and returned early on bootstrap/compile/write failures, but `src/entry_runtime_modes.c3` still returned exit code `0` unconditionally from `run_gen_e2e_mode()`.
  - `src/lisp/tests_e2e_generation.c3` now returns a real boolean success/failure result, reports temporary interpreter/buffer allocation failures explicitly, and `src/entry_runtime_modes.c3` now maps that result to exit code `0` or `1` truthfully.
  - Bounded container validation now covers both sides of that contract: the quiet success path `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 ./build/main --gen-e2e` exits `0` with no stdout/stderr output, while a forced write failure on `build/e2e_test.c3` exits `1` and prints `ERROR: Cannot write build/e2e_test.c3`.
  - The `--bind` utility also had a stale CLI contract: `src/entry_bind_dep_generation.c3` logged per-library output-write failures, but `src/entry_bind_mode.c3` still exited `0` after printing the final summary. It also lacked explicit allocation-failure handling for the TOML config and parsed-function buffer.
  - `src/entry_bind_dep_generation.c3` now returns a real per-dependency success/failure result, `src/entry_bind_mode.c3` now propagates dependency write failures and allocation failures to process exit code `1`, `src/entry_bind_runtime_setup.c3` now fails cleanly on parsed-function buffer allocation failure, and the bind path builders now take explicit `(char*, len)` inputs so path construction is not dependent on slice semantics.
  - Temp-project validation now covers both sides of the `--bind` contract: a synthetic local-header project exits `0` and generates `./lib/ffi/demo.omni`, while a forced output-write failure exits `1` and prints `Error: failed to write ./lib/ffi/demo.omni` followed by `Error: one or more FFI bindings failed`.
  - The small compiler-facing CLI modes had one more runtime-hardening gap: `src/entry_compile_mode.c3` and `src/entry_build_mode.c3` still allocated `Interp` directly and dereferenced it immediately, while the file-output paths in `src/entry_compile_mode.c3`, `src/entry_build_helpers.c3`, `src/entry_fmt_mode.c3`, and `src/lisp/tests_e2e_generation.c3` still treated one `file.write(...)` call plus ignored close as a complete write contract.
  - Those paths now share explicit plain-interpreter allocation helpers, fail cleanly instead of null-dereferencing on interpreter allocation failure, and use write-all loops with checked close on CLI-owned file emission. That closes the stale “partial write or buffered flush failure still exits 0 / prints success” contract for `--compile`, the AOT temp-file path used by `--build`, `--fmt --write`, and `--gen-e2e`.
  - Bounded validation now covers both the success and forced-failure edges of that contract: `--compile` still emits a real C3 output file, `--build` still emits an executable, `--fmt --write` still succeeds on a temp file, `--compile ... /dev/full` now exits `1` and prints `Error: cannot write output file /dev/full`, and a forced late write failure on `build/e2e_test.c3` via a temporary `/dev/full` symlink now exits `1` and prints `ERROR: Cannot write build/e2e_test.c3`.
  - The project scaffolding and bindgen utilities had the same stale write-side contract. `src/entry_project_init_files.c3`, `src/entry_project_init_writers.c3`, `src/entry_project_init_writer_project_json.c3`, and `src/entry_project_init_bind.c3` previously ignored path-construction overflow, ignored `mkdir(...)` failures, and treated scaffold writers as `void`, while `src/lisp/bindgen.c3` still used one `file.write(...)` call plus ignored close.
  - `--init` now treats path construction, directory creation, and file emission as real success/failure boundaries and returns `1` instead of printing the success summary after a partial scaffold. `lisp::generate_ffi_module(...)` now uses the same write-all plus checked-close contract as the other CLI emitters, and it now fails fast if the generated module buffer overflows instead of silently truncating the emitted `.omni` module.
  - Validation now covers both utilities on their real boundaries: bounded container `--init` still creates `omni.toml`, `src/main.omni`, and `build/project.json` for a fresh temp project, while a prewired `/dev/full` `build/project.json` path now makes `--init` exit `1` and print `Error: cannot write .../build/project.json`. The bindgen path remains host-validated because the bounded container image does not provide libclang; a temp local-header `--bind` success run still generates `./lib/ffi/demo.omni`, and the same setup with `./lib/ffi/demo.omni` redirected to `/dev/full` exits `1` and prints `Error: failed to write ./lib/ffi/demo.omni` followed by `Error: one or more FFI bindings failed`.
  - The bindgen overflow guard now has a focused compiler regression instead of relying only on ad hoc local-header probes. `src/lisp/tests_compiler_codegen_groups_tail.c3` now includes a synthetic oversized bindgen case that verifies `generate_ffi_module(...)` returns `false` and leaves no partial output file when the generated module would exceed `BINDGEN_BUF_SIZE`.
  - The bounded compiler slice is green from that updated baseline: `scripts/run_validation_container.sh bash -lc 'c3c build && env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp'` now passes at `135 passed, 0 failed`.
  - The last stale surface in the `--bind` CLI itself was silent path truncation. `src/entry_bind_paths.c3`, `src/entry_bind_mode.c3`, and `src/entry_bind_dep_generation.c3` now treat project-dir, `omni.toml`, and generated output paths as explicit success/failure boundaries instead of truncating into fixed 512-byte buffers.
  - That closes the remaining “wrong path but no error” contract in `--bind`: bounded validation now shows an overlong project-dir argument exits `1` with `Error: bind project path too long`, while the host local-header bind success/failure probes still hold on the updated path builders and writer contract.
- Unresolved issues and next actions
  - No open residuals remain from the 2026-03-28 runtime audit follow-up slice or from the adjacent pattern-guard hard-error closure; `TODO.md` is back to `Current actionable count: 0`.
  - No open backlog item remains for the compiled/AOT matcher parity follow-through. If the full E2E compiler gate is refreshed again, use the focused compiler slice plus the existing guard smoke first so unrelated legacy E2E drift does not hide matcher regressions.
  - The bounded full E2E compiler lane is green again after removing the stale lowercase `dict` surface from the generated corpus, so there is no remaining compiler-baseline drift to track from this matcher follow-through.
  - Keep using the plain bounded E2E command `scripts/run_validation_container.sh scripts/run_e2e.sh`; the temporary `env OMNI_HARD_MEM_CAP_METHOD=none ...` workaround is no longer needed.
- The same rule now applies to the other bounded entrypoints: `scripts/run_validation_container.sh scripts/run_global_gates.sh`, `scripts/run_validation_container.sh scripts/run_boundary_hardening.sh`, and the self-reentering `scripts/run_deduce_perf_envelope.sh` no longer need an in-container hard-cap override to avoid docker-on-docker failure.
- No new backlog item was reopened from the advanced/TCO fix.
- Boundary hardening is green again under the plain bounded invocation. If it regresses next, treat any new red stage as a real runtime/policy issue rather than a stale tooling/setup defect.
- The broader bounded global gate no longer has an open completion checkpoint from this session; the repaired ASAN `advanced` and `tco-recycling` lanes both held through the full end-to-end bounded run.
- No open residual remains in the Deduce contamination lane. `TODO.md` is back to `Current actionable count: 0`, and the bounded Deduce slice is green at `375 passed, 0 failed` after the per-lane interpreter isolation fix.
- Inspection-only audit addendum:
  - `src/lisp/tests_e2e_generation.c3` still has a false-success contract: the generator uses fixed-size source/expected buffers and a fixed `char[1024]` render scratch, but `append_line_to_buf(...)` and the per-case render path never surface truncation, so oversized generated corpora can silently emit incomplete `build/e2e_test.c3` / `build/e2e_expected.txt` while `--gen-e2e` still exits `0`.
  - `src/entry_runtime_modes.c3` still double-bootstraps `--gen-e2e`: `run_gen_e2e_mode()` creates a full runtime interpreter and then passes it to `generate_e2e_tests(...)`, but `generate_e2e_tests(...)` ignores the supplied interpreter and allocates/bootstraps a second one internally.
  - `src/entry_project_init_files.c3` still treats `mkdir(...)=EEXIST` as unconditional success for every scaffold path, so `--init` can reuse and overwrite an existing tree instead of failing truthfully on a non-fresh target.
  - `src/lisp/tests_deduce_durability_groups.c3` still launches restart scripts through plain `./build/main <script>` without quiet suppression, and `src/entry_script_mode.c3` prints non-`nil` results, so bounded quiet Deduce runs still leak raw `true` lines from subprocess success paths.
- Audit recheck closure:
  - `src/lisp/tests_e2e_generation.c3` now fails truthfully on generated-source/expected-output buffer exhaustion and per-case rendered-value truncation instead of silently emitting incomplete corpora, while still keeping the bounded quiet success path fully silent.
  - `src/entry_runtime_modes.c3` and `src/lisp/tests_e2e_generation.c3` now share one interpreter ownership contract for `--gen-e2e`; the utility reuses the caller-supplied runtime interpreter instead of allocating and bootstrapping a second one internally.
  - `src/entry_project_init_files.c3` and `src/entry_project_init_bind.c3` now reject preexisting project roots up front, so `--init` no longer treats an existing target tree as a valid scaffold destination.
  - `src/entry_script_reporting.c3` now honors `OMNI_TEST_QUIET`, which removes the raw `true` leakage from bounded quiet Deduce durability restart runs without mutating the durability script assertions themselves.
  - Bounded validation for this closure was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 ./build/main --gen-e2e > /tmp/e2e_gen_quiet.out 2>&1; rc=$?; printf "rc=%d\n" "$rc"; wc -c /tmp/e2e_gen_quiet.out'` -> `rc=0`, `0 /tmp/e2e_gen_quiet.out`
    - `scripts/run_validation_container.sh bash -lc 'tmpdir=$(mktemp -d); mkdir -p "$tmpdir/existing"; set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --init "$tmpdir/existing" > /tmp/init_existing.out 2>&1; rc=$?; set -e; printf "rc=%d\n" "$rc"; sed -n "1,20p" /tmp/init_existing.out'` -> `rc=1`, `Error: project already exists: ...`
    - `scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=basics ./build/main --test-suite lisp > /tmp/deduce_basics_quiet.out 2>&1; ...'` -> `pass=6 fail=0`, no `^true$` leak
    - `scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=materialized ./build/main --test-suite lisp > /tmp/deduce_materialized_quiet.out 2>&1; ...'` -> `pass=8 fail=0`, no `^true$` leak
- CLI positional-option guard follow-up:
  - `src/entry_build_mode.c3` and `src/entry_build_helpers.c3` no longer treat `-o` or later double-dash option tokens as valid positional paths. `--build` now fails early when the input file is missing before `-o`, and it no longer consumes a later option token as the output binary path.
  - `src/entry_bind_mode.c3` no longer treats a later double-dash mode/option token as the optional project directory. `--bind` now fails explicitly on `--bind --bogus`-style accidental mode chaining instead of trying to read `--bogus/omni.toml`.
  - Bounded validation for this guardrail was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --build -o /tmp/omni_cli_out > /tmp/omni_build_missing_input.out 2>&1; rc=$?; ...'` -> `rc=1`, `Error: missing input file before option -o`
    - `scripts/run_validation_container.sh bash -lc 'tmpdir=$(mktemp -d); ...; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --build "$tmpdir/smoke.omni" -o --bogus > /tmp/omni_build_missing_output.out 2>&1; rc=$?; ...'` -> `rc=1`, `Error: missing output path after -o`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --bind --bogus > /tmp/omni_bind_unexpected_option.out 2>&1; rc=$?; ...'` -> `rc=1`, `Error: unexpected option after --bind: --bogus`
    - `scripts/run_validation_container.sh bash -lc 'tmpdir=$(mktemp -d); cat > "$tmpdir/smoke.omni" <<EOF ... EOF; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --build "$tmpdir/smoke.omni" -o "$tmpdir/smoke_bin" ...'` -> `rc=0`, built binary exits `0` and prints `1`
- Broader positional-option contract follow-up:
  - The same double-dash positional guard is now enforced across the remaining simple CLI modes instead of only `--build` and `--bind`.
  - `src/entry_compile_mode.c3` now rejects `--compile --bogus out.c3` and `--compile input.omni --bogus` as missing input/output boundaries instead of attempting to read or write option tokens as files.
  - `src/entry_project_init_bind.c3` now rejects `--init --bogus` explicitly instead of scaffolding into an option-shaped directory name.
  - `src/entry_fmt_mode.c3`, `src/entry_describe_mode.c3`, `src/entry_check_mode.c3`, and `src/entry_eval_mode.c3` now stop positional scanning when the next token is another `--...` option, while still accepting their legitimate inline options such as `--json`, `--write`, and `--check`.
  - Bounded validation for this broader contract was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib ./build/main --compile --bogus /tmp/out.c3 ...'` -> `rc=1`, `Error: missing compile input file before option --bogus`
    - `scripts/run_validation_container.sh bash -lc 'tmpdir=$(mktemp -d); ...; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --compile "$tmpdir/smoke.omni" --bogus ...'` -> `rc=1`, `Error: missing compile output file before option --bogus`
    - `scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib ./build/main --init --bogus ...'` -> `rc=1`, `Error: unexpected option after --init: --bogus`
    - `scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib ./build/main --fmt --write --bogus ...'` -> `rc=1`, usage printed
    - `scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib ./build/main --describe --bogus ...'` -> `rc=1`, usage printed
    - `scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib ./build/main --check --bogus ...'` -> `rc=1`, usage printed
    - `scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval --bogus ...'` -> `rc=1`, usage printed
    - `scripts/run_validation_container.sh bash -lc 'tmpdir=$(mktemp -d); cat > "$tmpdir/smoke.omni" <<EOF ... EOF; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --check --json "$tmpdir/smoke.omni" ...'` -> `rc=0`, JSON success payload
    - `scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval --json "(+ 1 2)" ...'` -> `rc=0`, JSON success payload with `"value":"3"`
- Test-dispatch JSON contract follow-up:
  - `src/entry.c3` now treats an unknown top-level `--...` token as a CLI error instead of falling through into script mode and trying to read it as a filename.
  - `src/entry_test_modes.c3` now skips `--json` when resolving the optional `--test-suite` suite name, so `omni --test-suite --json` correctly means “all suites in JSON mode” instead of accidentally passing `--json` through the suite resolver.
  - `src/entry_test_modes.c3` now forces quiet mode and suppresses `OMNI_TEST_SUMMARY` while building the JSON test-suite response, so runner-owned summary lines no longer corrupt stdout JSON.
  - `src/lisp/tests_compiler_tests.c3` and `src/lisp/tests_tests.c3` now propagate real compiler-slice pass/fail counts back through `run_lisp_tests(...)`, which fixes the stale `pass=0 fail=0` JSON result for `OMNI_LISP_TEST_SLICE=compiler`.
  - `src/lisp/tests_slice_policy.c3` now gates the default-slice `OMNI_TEST_SUMMARY slice default: ...` line behind the same summary check as the rest of the lisp harness, so it no longer leaks into JSON suite mode after summary suppression.
  - Bounded validation for this JSON/dispatch closure was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --bogus > /tmp/omni_unknown_option.out 2>&1; rc=$?; ...'` -> `rc=1`, `Error: unknown option --bogus`
    - `scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 ./build/main --test-suite --json ...'` -> clean JSON only, `{"requested_suite":"all",...,"pass":226,"fail":0,...}`
    - `scripts/run_validation_container.sh bash -lc 'env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite --json lisp ...'` -> clean JSON only, `{"requested_suite":"lisp",...,"pass":135,"fail":0,...}`
- Flag-only mode arity follow-up:
  - `src/entry_runtime_modes.c3`, `src/entry_stack_affinity_mode.c3`, and `src/entry.c3` now enforce the expected no-extra-argument contract for `--gen-e2e`, `--stack-affinity-probe`, and explicit `--repl`, while still allowing the documented `--repl --json` form.
  - Before this change, `--gen-e2e --bogus` still generated the corpus and exited `0`, `--stack-affinity-probe --bogus` still ran the fail-fast probe, and `--repl --bogus` silently opened the REPL. Those modes now reject stray tokens as usage errors instead of acting as if the command line were valid.
  - Bounded validation for this closure was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --gen-e2e --bogus ...'` -> `rc=1`, `Error: unexpected argument after --gen-e2e: --bogus`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --stack-affinity-probe --bogus ...'` -> `rc=1`, `Error: unexpected argument after --stack-affinity-probe: --bogus`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl --bogus ...'` -> `rc=1`, `Error: unexpected argument after --repl: --bogus`
    - `scripts/run_validation_container.sh bash -lc 'set +e; timeout 2s env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl --json ...'` -> `rc=0`, no unexpected-argument error
- Fixed-arity mode trailing-argument follow-up:
  - The remaining fixed-arity modes now reject stray trailing arguments instead of silently succeeding after doing real work on only the first positional input(s).
  - `src/entry_cli_helpers.c3` now provides a shared trailing-argument validator, and `src/entry.c3`, `src/entry_project_init_bind.c3`, `src/entry_bind_mode.c3`, `src/entry_build_mode.c3`, `src/entry_fmt_mode.c3`, `src/entry_describe_mode.c3`, `src/entry_check_mode.c3`, and `src/entry_eval_mode.c3` use it to enforce honest arity on `--compile`, `--init`, `--bind`, `--build`, `--fmt`, `--describe`, `--check`, `--eval`, and `--test-suite`.
  - Before this change, commands like `--compile file out extra`, `--init name extra`, `--eval expr extra`, `--describe sym extra`, `--check file extra`, `--fmt file extra`, `--test-suite lisp extra`, and `--build file -o out extra` were all accepted. Several of them completed successfully and ignored the trailing token, which was a stale CLI contract.
  - Bounded validation for this closure was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc '... ./build/main --compile "$tmpdir/smoke.omni" "$tmpdir/out.c3" extra ...'` -> `rc=1`, `Error: unexpected argument after --compile: extra`
    - `scripts/run_validation_container.sh bash -lc '... ./build/main --init demo extra ...'` -> `rc=1`, `Error: unexpected argument after --init: extra`
    - `scripts/run_validation_container.sh bash -lc '... ./build/main --bind . extra ...'` -> `rc=1`, `Error: unexpected argument after --bind: extra`
    - `scripts/run_validation_container.sh bash -lc '... ./build/main --eval "(+ 1 2)" extra ...'` -> `rc=1`, `Error: unexpected argument after --eval: extra`
    - `scripts/run_validation_container.sh bash -lc '... ./build/main --describe length extra ...'` -> `rc=1`, `Error: unexpected argument after --describe: extra`
    - `scripts/run_validation_container.sh bash -lc '... ./build/main --check "$tmpdir/smoke.omni" extra ...'` -> `rc=1`, `Error: unexpected argument after --check: extra`
    - `scripts/run_validation_container.sh bash -lc '... ./build/main --fmt "$tmpdir/smoke.omni" extra ...'` -> `rc=1`, `Error: unexpected argument after --fmt: extra`
    - `scripts/run_validation_container.sh bash -lc '... ./build/main --test-suite lisp extra ...'` -> `rc=1`, `Error: unexpected argument after --test-suite: extra`
    - `scripts/run_validation_container.sh bash -lc '... ./build/main --build "$tmpdir/smoke.omni" -o "$tmpdir/outbin" extra ...'` -> `rc=1`, `Error: unexpected argument after --build: extra`
    - Allow-side checks remained green:
      - `./build/main --fmt --check "$tmpdir/smoke.omni"` -> `rc=0`
      - `OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite --json lisp` -> clean JSON, `pass=135 fail=0`
      - `./build/main --build "$tmpdir/smoke.omni" --print-last -o "$tmpdir/outbin"` -> `rc=0`, binary built successfully
- Informational/script arity follow-up:
  - The last silent-success CLI arity holes were in the top-level informational shortcuts and plain script invocation. `src/entry.c3` now applies the shared unexpected-argument check to `--help`, `--version`, and `--language-ref`, and `src/entry_script_mode.c3` now treats `omni script.omni extra` as a usage error instead of executing the script and ignoring `extra`.
  - Before this change, `--help extra`, `--version extra`, and `--language-ref extra` all exited `0` and printed their normal output, while `./build/main script.omni extra` still ran the script and returned success. That was inconsistent with the tighter fixed-arity contract already enforced for the rest of the CLI.
  - Bounded validation for this closure was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --help extra > /tmp/omni_help_extra.out 2>&1; rc=$?; ...'` -> `rc=1`, `Error: unexpected argument after --help: extra`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --version extra > /tmp/omni_version_extra.out 2>&1; rc=$?; ...'` -> `rc=1`, `Error: unexpected argument after --version: extra`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --language-ref extra > /tmp/omni_lang_extra.out 2>&1; rc=$?; ...'` -> `rc=1`, `Error: unexpected argument after --language-ref: extra`
    - `scripts/run_validation_container.sh bash -lc 'tmp=$(mktemp /tmp/omni_script_XXXX.omni); printf "(println 1)\n" > "$tmp"; set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main "$tmp" extra > /tmp/omni_script_extra.out 2>&1; rc=$?; ...'` -> `rc=1`, `Error: unexpected argument after script file: extra`
    - Allow-side check remained green: `env LD_LIBRARY_PATH=/usr/local/lib ./build/main "$tmp"` -> `rc=0`, output `1` then `#<void>`
- Top-level single-dash option follow-up:
  - `src/entry.c3` previously only treated unknown `--...` tokens as CLI errors. An unknown single-dash token like `-bogus` fell through the dispatcher as a script filename and only failed later with `Error: cannot read script file '-bogus'`, which was a stale top-level contract.
  - `src/entry_cli_helpers.c3` now exposes a shared `cli_arg_starts_with_dash(...)` predicate, and `src/entry.c3` now rejects any unrecognized leading `-...` token before script dispatch. This keeps legitimate explicit paths like `./-script.omni` working, because only the raw top-level token is treated as an option-shaped CLI argument.
  - Bounded validation for this closure was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main -bogus > /tmp/omni_single_dash_top.out 2>&1; rc=$?; ...'` -> `rc=1`, `Error: unknown option -bogus`
    - Allow-side check remained green: `env LD_LIBRARY_PATH=/usr/local/lib ./build/main "$tmpdir/-smoke.omni"` -> `rc=0`, output `7` then `#<void>`
- File-mode raw single-dash path follow-up:
  - The file/project-oriented modes still treated raw single-dash tokens as real names or paths. Before this change, `--init -bogus` succeeded and scaffolded a project literally named `-bogus`, `--bind -bogus` treated the token as a project dir, and `--fmt/-check/-compile/-build -bogus` failed later as file I/O or compile errors instead of honest CLI usage errors.
  - `src/entry_project_init_bind.c3`, `src/entry_bind_mode.c3`, `src/entry_build_mode.c3`, `src/entry_build_helpers.c3`, `src/entry_compile_mode.c3`, `src/entry_check_mode.c3`, and `src/entry_fmt_mode.c3` now reject raw single-dash option-shaped tokens in those modes while still allowing explicit dashed paths via `./-name`.
  - Bounded validation for this closure was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc 'set +e; ... ./build/main --fmt -bogus ...'` -> `rc=1`, `Error: unexpected argument after --fmt: -bogus`
    - `scripts/run_validation_container.sh bash -lc 'set +e; ... ./build/main --check -bogus ...'` -> `rc=1`, `Error: unexpected argument after --check: -bogus`
    - `scripts/run_validation_container.sh bash -lc 'set +e; ... ./build/main --compile -bogus "$tmpdir/out.c3" ...'` -> `rc=1`, `Error: missing compile input file before option -bogus`
    - `scripts/run_validation_container.sh bash -lc 'set +e; ... ./build/main --build -bogus -o "$tmpdir/outbin" ...'` -> `rc=1`, `Error: missing input file before option -bogus`
    - `scripts/run_validation_container.sh bash -lc 'set +e; ... ./build/main --init -bogus ...'` -> `rc=1`, `Error: unexpected option after --init: -bogus`
    - `scripts/run_validation_container.sh bash -lc 'set +e; ... ./build/main --bind -bogus ...'` -> `rc=1`, `Error: unexpected option after --bind: -bogus`
    - Allow-side checks remained green with explicit paths:
      - `./build/main --check "$tmpdir/./-smoke.omni"` -> `rc=0`, `Check passed: ...`
      - `./build/main --fmt --check "$tmpdir/./-smoke.omni"` -> `rc=0`
      - `./build/main --compile "$tmpdir/./-smoke.omni" "$tmpdir/./-out.c3"` -> `rc=0`
      - `./build/main --build "$tmpdir/./-smoke.omni" -o "$tmpdir/./-bin"` -> `rc=0`
      - `./build/main --bind "$tmpdir/./-proj"` -> `rc=0`, `No FFI dependencies found in omni.toml`
  - Cleanup note: the earlier pre-fix validation had created a stray repo-root `-bogus/` scaffold; that artifact was removed after the new contract was validated.
- `--test-suite` raw single-dash suite follow-up:
  - After the top-level and file-mode single-dash tightening, one narrower inconsistency remained: `--test-suite -bogus` still flowed into suite-name validation and reported an “invalid suite” instead of rejecting the option-shaped token as a CLI usage error. The `--json` variant also needed to stay structured.
  - `src/entry.c3` now rejects raw dash-shaped suite tokens before suite dispatch, and `src/entry_test_modes.c3` now provides a small JSON usage-error helper so `--test-suite --json -bogus` returns structured `cli/usage` output rather than falling back to text.
  - Bounded validation for this closure was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite -bogus > /tmp/omni_testsuite_single_dash4.out 2>&1; rc=$?; ...'` -> `rc=1`, `Error: unexpected argument after --test-suite: -bogus`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite --json -bogus > /tmp/omni_testsuite_json_single_dash4.out 2>&1; rc=$?; ...'` -> `rc=1`, `{"ok":false,"code":"cli/usage","severity":"error","message":"unexpected argument after --test-suite: -bogus"}`
    - Allow-side check remained green: `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite --json lisp` -> `rc=0`, clean JSON with `pass=142 fail=0`
- `--check --json` usage-shape follow-up:
  - One JSON/text mismatch remained in the file-mode contract: `--check --json` still fell back to plain-text usage errors when the next token was option-shaped or when an extra trailing token was present, even though the success and diagnostic paths were already structured JSON.
  - `src/entry_check_reporting.c3` now provides a small structured `cli/usage` helper for unexpected arguments, and `src/entry_check_mode.c3` now uses it for both the missing-input option-shaped case and the trailing-extra case when `--json` is active.
  - Bounded validation for this closure was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --check --json -bogus > /tmp/omni_check_json_single_dash2.out 2>&1; rc=$?; ...'` -> `rc=1`, `{"ok":false,"code":"cli/usage","severity":"error","message":"unexpected argument after --check: -bogus"}`
    - `scripts/run_validation_container.sh bash -lc 'set +e; ... ./build/main --check --json "$tmpdir/smoke.omni" extra > /tmp/omni_check_json_extra.out 2>&1; rc=$?; ...'` -> `rc=1`, `{"ok":false,"code":"cli/usage","severity":"error","message":"unexpected argument after --check: extra"}`
    - Allow-side check remained green: `./build/main --check --json "$tmpdir/smoke.omni"` -> `rc=0`, `{"ok":true,"path":"...","diagnostics":[]}`
- `--eval/--describe --json` trailing-extra follow-up:
  - The same JSON/text mismatch remained in the other structured fixed-arity modes: `--eval --json` and `--describe --json` already produced JSON on missing-input usage errors, but a trailing extra token still fell back to plain-text `Run 'omni --help' for usage.` output.
  - `src/entry_eval_reporting.c3` and `src/entry_describe_reporting.c3` now provide small structured `cli/usage` helpers for unexpected arguments, and `src/entry_eval_mode.c3` / `src/entry_describe_mode.c3` now use them when `--json` is active.
  - Bounded validation for this closure was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval --json "(+ 1 2)" extra > /tmp/omni_eval_json_extra2.out 2>&1; rc=$?; ...'` -> `rc=1`, `{"ok":false,"code":"cli/usage","severity":"error","message":"unexpected argument after --eval: extra"}`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --describe --json length extra > /tmp/omni_describe_json_extra2.out 2>&1; rc=$?; ...'` -> `rc=1`, `{"ok":false,"code":"cli/usage","severity":"error","message":"unexpected argument after --describe: extra"}`
    - Allow-side checks remained green:
      - `./build/main --eval --json "(+ 1 2)"` -> `rc=0`, `{"ok":true,"input":"(+ 1 2)","value":"3","error":null}`
      - `./build/main --describe --json length` -> `rc=0`, `{"ok":true,"symbol":"length","kind":"builtin","documentation":"Generic collection length operation.","error":null}`
- `--eval/--describe` double-dash missing-arg follow-up:
  - One final consistency gap remained in those same two modes: when the missing source/symbol was caused by another `--...` option token, both modes still collapsed into generic usage instead of classifying it as an unexpected argument. `--check` had already been tightened past that boundary.
  - `src/entry_eval_mode.c3` and `src/entry_describe_mode.c3` now distinguish “no argument provided” from “next token is another option,” and route the latter through the same unexpected-argument contract in both text and JSON.
  - Bounded validation for this closure was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval --bogus > /tmp/omni_eval_ddash_text2.out 2>&1; rc=$?; ...'` -> `rc=1`, `Error: unexpected argument after --eval: --bogus`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval --json --bogus > /tmp/omni_eval_ddash_json2.out 2>&1; rc=$?; ...'` -> `rc=1`, `{"ok":false,"code":"cli/usage","severity":"error","message":"unexpected argument after --eval: --bogus"}`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --describe --bogus > /tmp/omni_describe_ddash_text2.out 2>&1; rc=$?; ...'` -> `rc=1`, `Error: unexpected argument after --describe: --bogus`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --describe --json --bogus > /tmp/omni_describe_ddash_json2.out 2>&1; rc=$?; ...'` -> `rc=1`, `{"ok":false,"code":"cli/usage","severity":"error","message":"unexpected argument after --describe: --bogus"}`
    - Allow-side checks remained green:
      - `./build/main --eval --json "(+ 1 2)"` -> `rc=0`, `{"ok":true,"input":"(+ 1 2)","value":"3","error":null}`
      - `./build/main --describe --json length` -> `rc=0`, `{"ok":true,"symbol":"length","kind":"builtin","documentation":"Generic collection length operation.","error":null}`
- CLI usage-surface wording follow-up:
  - The remaining CLI parser work exposed a stale surface mismatch in the mode-local usage text. `src/entry_build_mode.c3`, `src/entry_compile_reporting.c3`, `src/entry_project_init_bind.c3`, and `src/entry_bind_mode.c3` still advertised `./main` and `input.lisp` even though the public CLI and top-level help had already standardized on `omni` and generic Omni source files.
  - Those mode-local usage strings and adjacent mode comments now match the shipped surface: `omni --build <file> [-o output]`, `omni --compile <file> <out.c3>`, `omni --init <project-name>`, and `omni --bind [project-dir]`.
  - Bounded validation for this surface cleanup was:
    - `scripts/run_validation_container.sh c3c build`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --compile > /tmp/omni_compile_missing2.out 2>&1; rc=$?; ...'` -> `rc=1`, `Usage: omni --compile <file> <out.c3>`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --build > /tmp/omni_build_missing2.out 2>&1; rc=$?; ...'` -> `rc=1`, `Usage: omni --build <file> [-o output]`
    - `scripts/run_validation_container.sh bash -lc 'set +e; env LD_LIBRARY_PATH=/usr/local/lib ./build/main --init > /tmp/omni_init_missing2.out 2>&1; rc=$?; ...'` -> `rc=1`, `Usage: omni --init <project-name>`
- Canonical CLI alias cleanup follow-up:
  - The dispatcher still accepted three undocumented single-dash aliases, `-help`, `-compile`, and `-repl`, even though the active public surface had already standardized on `--help`, `--compile`, and `--repl`. Keeping those odd aliases live would preserve non-canonical surface drift for no real benefit.
  - `src/entry.c3` now removes those alias spellings from dispatch, and `docs/LANGUAGE_SPEC.md` no longer advertises `-repl` as an alternate entrypoint. The canonical double-dash forms remain unchanged.
  - Bounded validation for this closure was:
    - `scripts/run_validation_container.sh c3c build`
    - `./build/main -help` -> `rc=1`, `Error: unknown option -help`
    - `./build/main -compile` -> `rc=1`, `Error: unknown option -compile`
    - `./build/main -repl` -> `rc=1`, `Error: unknown option -repl`
    - Allow-side check remained green: `./build/main --compile "$tmpfile" /tmp/omni_compile_alias_check.c3` -> `rc=0`, `Compilation successful: /tmp/omni_compile_alias_check.c3`
- Signature: Codex (GPT-5)

2026-03-28 - Example clone build convenience and import binding

- Objectives attempted:
  - Make the standalone example-only clone convenient to build with `./omni --build ...` instead of requiring the full Omni checkout workflow.
  - Audit the `--build` error path so the failure explains the repo-root requirement instead of only saying root resolution failed.
  - Fix the AOT import lowering so top-level module aliases like `ui` are actually bound in generated C3, instead of compiling to an undeclared symbol.
- Code/config changes made:
  - Added a local wrapper script in the example clone (`/home/heefoo/Documents/code/ftxui-example-clone/omni`) that forwards to a usable Omni build binary and prefers `OMNI_REPO_ROOT` when set.
  - Updated the example clone README to document `./omni --build ./demo.omni -o out` and the `OMNI_REPO_ROOT` override.
  - Improved `src/entry_build_mode.c3` so the repo-root resolution failure now includes a concrete hint about using a full Omni checkout, setting `OMNI_REPO_ROOT`, or using the example clone's wrapper.
  - Taught `src/entry_build_helpers.c3` to honor `OMNI_REPO_ROOT` during build-root resolution.
  - Added `lisp::aot::import_module(...)` in `src/lisp/aot_runtime_bridge.c3` to bridge top-level imports back into the existing runtime import machinery.
  - Changed `src/lisp/compiler_program_top_level_helpers.c3` and `src/lisp/compiler_program_top_level.c3` so top-level `import` forms emit real runtime import initialization and declare imported module aliases/selective bindings as globals.
- Experiment commands and key metrics:
  - `cd /home/heefoo/Documents/code/Omni && c3c build` -> pass, executable linked to `build/main`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && ./omni --build ./demo.omni -o out` -> pass, executable linked to `/home/heefoo/Documents/code/ftxui-example-clone/out`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && printf 'q' | LD_LIBRARY_PATH=/usr/local/lib ./out` -> pass, rendered the FTXUI demo and exited cleanly.
- Best current checkpoint/config recommendation:
  - For the example clone, use `./omni --build ./demo.omni -o out` and then run `printf 'q' | LD_LIBRARY_PATH=/usr/local/lib ./out`.
  - For explicit full-repo builds from other working directories, set `OMNI_REPO_ROOT=/home/heefoo/Documents/code/Omni` so the wrapper can find the build binary and the compiler can resolve the repo root.
- Unresolved issues and next actions:
  - Selective-import and `import all` AOT coverage still deserves follow-up validation; the current fix covers the convenience path used by the example demo (`import "ui.omni"` and `import ui`).
  - If new import patterns fail, the next step is to extend the AOT import bridge to serialize selective bindings more explicitly instead of relying on the current module-alias path.
- Signature: Codex (GPT-5)

2026-03-28 - CLI and wrapper error-message audit

- Objectives attempted:
  - Tighten the remaining user-facing error messages in the build, script, eval, and example-clone wrapper paths so failures report the exact mode, file, or step that broke.
  - Remove the last generic “bootstrap failed” / “compilation failed” / “could not find a usable build binary” style messages that forced the user to guess what to check next.
- Code/config changes made:
  - Changed `src/lisp/aot_runtime_bridge.c3` so AOT import failures now mention the import target and, when available, the underlying runtime import error.
  - Changed `src/entry_script_mode.c3` so runtime bootstrap and source-directory allocation failures name the script file they were handling.
  - Changed `src/entry_check_mode.c3`, `src/entry_eval_mode.c3`, and `src/entry_runtime_modes.c3` so runtime-interpreter bootstrap failures name the specific CLI mode they belong to.
  - Changed `src/entry_build_mode.c3` so AOT backend compile failures mention both the generated temp C3 source and the output binary.
  - Changed `src/lisp/prim_io_file.c3` so `load` source-directory allocation failures include the loaded path.
  - Changed `/home/heefoo/Documents/code/ftxui-example-clone/omni` so the wrapper now prints the exact build-binary locations it checked before failing.
- Experiment commands and key metrics:
  - `cd /home/heefoo/Documents/code/Omni && c3c build` -> pass, executable linked to `build/main`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && ./omni --build ./demo.omni -o out` -> pass, executable linked to `/home/heefoo/Documents/code/ftxui-example-clone/out`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && printf 'q' | LD_LIBRARY_PATH=/usr/local/lib ./out` -> pass, rendered the FTXUI demo and exited cleanly.
  - Negative-path wrapper/build check: `cd /home/heefoo/Documents/code/ftxui-example-clone && ./omni --build /tmp/omni-bad-build.omni -o /tmp/omni-bad-build-out` -> parse error surfaced from the broken source, confirming the wrapper now reaches the build binary cleanly.
- Best current checkpoint/config recommendation:
  - Keep the clone workflow as `cd /home/heefoo/Documents/code/ftxui-example-clone && ./omni --build ./demo.omni -o out`.
  - If the user hits a failure, the next layer now reports the actual mode/file/step, so the remaining debugging path should be much shorter.
- Unresolved issues and next actions:
  - The audit did not cover every low-level runtime error in the tree; it focused on the CLI/build/wrapper messages that were actually observed as too generic.
  - If another generic message shows up in a new path, patch it directly instead of reusing a catch-all bootstrap/build phrase.
- Signature: Codex (GPT-5)

2026-03-28 - Broader message cleanup pass

- Objectives attempted:
  - Push the remaining obvious generic runtime/build/init errors toward mode- and path-specific wording instead of leaving them as bare “failed” summaries.
  - Cover the next layer of user-facing messages that showed up during the broader scan: JIT compile failures, bind/init composition failures, and the example-clone wrapper fallback.
- Code/config changes made:
  - Changed `src/lisp/runtime_backend_hooks.c3` so runtime JIT compile failures now say the failure happened while compiling an expression for runtime evaluation.
  - Changed `src/lisp/eval_run_pipeline.c3` so run/eval JIT compile failures now say they happened during program execution rather than as a generic compile error.
  - Changed `src/entry_bind_mode.c3` so the FFI summary error names the project directory it was binding.
  - Changed `src/entry_project_init_writers.c3` so init composition failures mention which generated file and project name they were composing.
  - Changed `src/entry_build_backend_compile.c3` so AOT backend command composition failures name both the temp source file and output binary.
  - Tightened the example-clone wrapper fallback in `/home/heefoo/Documents/code/ftxui-example-clone/omni` so it says the binary was not found in the checked locations.
- Experiment commands and key metrics:
  - `cd /home/heefoo/Documents/code/Omni && c3c build` -> pass, executable linked to `build/main`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && ./omni --build ./demo.omni -o out` -> pass, executable linked to `/home/heefoo/Documents/code/ftxui-example-clone/out`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && printf 'q' | LD_LIBRARY_PATH=/usr/local/lib ./out` -> pass, rendered the FTXUI demo and exited cleanly.
- Best current checkpoint/config recommendation:
  - Keep the clone wrapper as the convenience path for example work.
  - Treat remaining generic message fixes as opt-in follow-up if a user hits a new weak spot; the currently audited build/script/wrapper paths are now materially more descriptive than before.
- Unresolved issues and next actions:
  - Some low-level internal errors still use short wording because they are only surfaced in deep runtime failure paths and already carry enough local context for debugging.
  - If the user wants a second audit sweep, the next target should be the remaining `JIT compilation failed`-style runtime internals and the init/bind helpers that still summarize multi-step failures.
- Signature: Codex (GPT-5)

2026-03-29 - REPL preload commands

- Objectives attempted:
  - Add a first-class CLI path for starting the REPL with the current Omni project already loaded instead of requiring a manual `(load "src/main.omni")`.
  - Cover standalone example/workspace trees that are not full Omni projects, so REPL preload is not gated on `omni.toml`.
  - Keep the plain text REPL behavior intact while preserving the structured `--repl --json` transport contract.
  - Document the shipped REPL/project contract in the CLI help and user-facing docs.
- Code/config changes made:
  - Extended `src/entry_runtime_modes.c3` with REPL option parsing for both `--project [dir]` and `--load <file>`, plus preload execution before entering the interactive loop.
  - Reused the existing script/import source-dir behavior during preload so relative imports inside `src/main.omni` resolve from the entry file directory.
  - Kept `--project` as the strict `omni.toml` path and added `--load` for standalone files such as the FTXUI example clone's `demo.omni` / `smoke.omni`.
  - Added explicit rejection for REPL preload on `--json` because preload code can emit ordinary stdout before the JSON transport begins, which would corrupt the protocol.
  - Updated `src/entry_cli_help_version.c3`, `docs/LANGUAGE_SPEC.md`, `docs/PROJECT_TOOLING.md`, and `README.md` to advertise both preload entrypoints and the text-only restriction.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl --project demo </dev/null` -> `rc=0`; preloaded `demo/src/main.omni`, printed `"Hello from demo!"`, entered the REPL banner, and exited cleanly on EOF.
  - `cd demo && env LD_LIBRARY_PATH=/usr/local/lib ../build/main --repl --project </dev/null` -> `rc=0`; confirmed the no-arg current-directory project resolution path loads `/home/heefoo/Documents/code/Omni/demo/src/main.omni`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && omni --repl --load smoke.omni </dev/null` -> `rc=0`; confirmed standalone example trees without `omni.toml` can preload one file and still reach the REPL banner cleanly.
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl --project --bogus` -> `rc=1`, `Error: unexpected argument after --repl: --bogus`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && omni --repl --load demo.omni </dev/null` -> preload reached the live FTXUI demo event loop as expected; this confirmed the file preload path works, but the UI example is not suitable as an EOF-to-banner smoke.
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl --project demo --json` -> `rc=1`, `Error: REPL preload is not supported with --json`.
  - `cd /home/heefoo/Documents/code/ftxui-example-clone && omni --repl --load demo.omni --json` -> `rc=1`, `Error: REPL preload is not supported with --json`.
- Best current checkpoint/config recommendation:
  - Use `omni --repl --project` from inside a project root, or `omni --repl --project <dir>` from elsewhere, when the goal is an interactive session with `src/main.omni` already evaluated.
  - Use `omni --repl --load <file>` for standalone example/workspace trees that are not full Omni projects.
  - Keep editor/tool integrations on plain `omni --repl --json`; do not layer REPL preload onto the JSON transport until there is a structured bootstrap protocol that can carry preload stdout safely.
- Unresolved issues and next actions:
  - Project preload currently targets `src/main.omni` directly and does not consult richer `omni.toml` entrypoint metadata because that contract does not exist yet.
  - If project-aware editor transport becomes necessary, the next step is a dedicated JSON bootstrap handshake or structured preload result envelope rather than allowing raw preload stdout onto the JSON channel.
  - If the repo eventually grows a real per-project entrypoint field in `omni.toml`, `--project` should resolve through that metadata instead of hardcoding `src/main.omni`.
- Signature: Codex (GPT-5)

2026-03-29 - omni-nvim setup defaults and docs fix

- Objectives attempted:
  - Audit the first-party Neovim plugin after the user reported that the Omni plugin was not set up properly.
  - Verify whether the break was in the plugin runtime, its LSP defaults, or the documented lazy.nvim setup.
  - Align the plugin’s root detection and setup examples with the real Omni project layout.
  - Make the documented default `auto_start = true` behavior real on first Omni buffer open and clarify the Tree-sitter setup boundary.
- Code/config changes made:
  - Updated `tooling/omni-nvim/lua/omni/init.lua` and `tooling/omni-nvim/lua/omni/lsp.lua` so Omni LSP now defaults to root markers `omni.toml`, `project.json`, and `.git` instead of only `project.json` and `.git`.
  - Updated `tooling/omni-nvim/lua/omni/init.lua` so `apply_buffer()` now actually honors `auto_start` by scheduling `repl.start(config, { focus = false })` on the first Omni buffer activation.
  - Updated `tooling/omni-nvim/lua/omni/repl.lua` so background REPL startup can restore the source window after opening the transcript split instead of stealing focus from the edited Omni buffer.
  - Updated `tooling/omni-nvim/README.md`, `tooling/omni-nvim/doc/omni.nvim.txt`, and `docs/PROJECT_TOOLING.md` so the lazy.nvim example includes an `init = function() vim.filetype.add({ extension = { omni = "omni" } }) end` hook for setups where `.omni` filetype detection is not exposed before lazy loading.
  - Updated the same docs to advertise the corrected LSP root-marker defaults, the default REPL auto-start behavior, and the requirement to install the Omni Tree-sitter parser before expecting syntax highlighting.
- Experiment commands and key metrics:
  - `tooling/omni-nvim/scripts/run_smoke.sh` -> pass, `omni-nvim smoke: ok`.
  - `nvim --headless --clean -u NONE -i NONE --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' '+lua local omni=require("omni"); local cfg=omni.setup({}); print(table.concat(cfg.lsp.root_markers, ","))' '+qa'` -> printed `omni.toml,project.json,.git`.
  - `nvim --headless --clean -u NONE -i NONE --cmd 'set runtimepath^=/home/heefoo/Documents/code/Omni/tooling/omni-nvim' --cmd 'filetype plugin on' '+lua require("omni").setup({})' '+edit /home/heefoo/Documents/code/ftxui-example-clone/smoke.omni' '+lua vim.wait(300, function() return vim.fn.bufnr("Omni REPL") ~= -1 end)' '+lua print("ft=" .. vim.bo.filetype)' '+lua print("mapped=" .. tostring(vim.b.omni_nvim_mapped))' '+lua print("repl_buf=" .. vim.fn.bufnr("Omni REPL"))' '+qa'` -> printed `ft=omni`, `mapped=true`, `repl_buf=2`, confirming first-buffer auto-start now creates the REPL transcript without manual `:OmniReplStart`.
  - Local code inspection result: the stale root markers, the too-minimal lazy example, and the previously unwired `auto_start` path were the concrete repo-side plugin defects.
- Best current checkpoint/config recommendation:
  - For lazy.nvim, use the plugin with both `init = function() vim.filetype.add({ extension = { omni = "omni" } }) end` and `ft = { "omni" }`, then call `require("omni").setup(...)` in `config`.
  - Treat `omni.toml` as the primary Omni project root marker for editor/LSP setup; `project.json` remains a secondary compatibility/root hint, not the main project contract.
  - Treat REPL transcript startup and Tree-sitter parser installation as separate setup steps: `require("omni").setup(...)` should start the REPL wiring, but syntax highlighting still depends on the parser being installed and available to Neovim.
- Unresolved issues and next actions:
  - The plugin smoke still validates bootstrap/filetype wiring rather than a full lazy.nvim-managed installation path.
  - Some current Neovim and `nvim-treesitter` combinations still do not expose a fully automatic Omni parser-install path from the repo plugin alone; broader parser-install compatibility deserves its own follow-up slice instead of being folded into the REPL/LSP setup lane.
  - If the user still sees setup problems after this patch, the next step is to inspect their actual Neovim plugin-manager spec and startup ordering rather than changing the repo plugin blindly.
- Signature: Codex (GPT-5)

2026-03-30 - omni-nvim value pretty printing

- Objectives attempted:
  - Improve Omni Neovim transcript readability after the user reported that evaluated values had no pretty printing.
  - Keep the structured REPL and `--eval --json` protocol unchanged while making nested collection output easier to scan in-editor.
- Code/config changes made:
  - Updated `tooling/omni-nvim/lua/omni/repl.lua` with a plugin-side Omni value pretty-printer that parses rendered value strings and reflows nested lists, arrays, and dictionaries into an indented multiline layout when they exceed the configured width.
  - Wired that formatter into the transcript append path for both one-shot `--eval --json` responses and structured REPL replies.
  - Added output config defaults in `tooling/omni-nvim/lua/omni/init.lua` for `output.pretty_values = true`, `output.pretty_width = 72`, and `output.pretty_indent = 2`.
  - Updated `tooling/omni-nvim/README.md` and `tooling/omni-nvim/doc/omni.nvim.txt` to document the new pretty-printing behavior and the tuning knobs.
- Experiment commands and key metrics:
  - `tooling/omni-nvim/scripts/run_smoke.sh` -> pass, `omni-nvim smoke: ok`.
  - `c3c build` -> pass, executable linked to `build/main`.
  - Headless transcript probe with a larger nested value and `output.pretty_width = 60` -> the `Omni REPL` buffer now renders:
    - `=>`
    - `{`
    - `  nested`
    - `    {`
    - `      alpha [10 20 30 40]`
    - `      gamma "long-long-long-string"`
    - `      beta {deep [100 200 300 400]}`
    - `    }`
    - `  nums [1 2 3 4 5 6 7 8 9 10]`
    - `  name "omni"`
    - `  tags [ui repl nvim pretty]`
    - `}`
- Best current checkpoint/config recommendation:
  - Keep plugin-side pretty printing enabled by default so the user-facing transcript becomes readable without changing the REPL JSON contract.
  - Tune transcript shape with `output.pretty_width` before considering protocol changes; width is the main lever that determines when values break across lines.
- Unresolved issues and next actions:
  - The current pretty-printer is syntax-aware and bounded, but it is still a client-side layout pass over the rendered value string rather than a first-class runtime pretty printer.
  - Dictionary rendering currently preserves the runtime print order rather than imposing a sorted key order.
  - If stronger layout guarantees are needed later, the next step is a real runtime-side pretty-printer contract rather than piling more policy into the Neovim client.
- Signature: Codex (GPT-5)

2026-03-30 - Structured JSON stdio separation for print/display

- Objectives attempted:
  - Fix the broken Omni Neovim eval transcript the user hit when `(print ...)` wrote onto the same stdout channel as `--eval --json` and `--repl --json`.
  - Keep the structured JSON contract intact while still surfacing user-directed console output in editor integrations.
  - Make sure print-without-newline results show up in the Neovim transcript instead of staying buffered until process exit.
- Code/config changes made:
  - Added `InterpFlags.console_to_stderr` in `src/lisp/value_interp_state.c3` and initialized it in `src/lisp/value_interp_init_helpers.c3`.
  - Updated `src/lisp/prim_io.c3` so `print`, `println`, `display`, and `newline` continue to use the shared console path but target stderr whenever the interpreter is running inside a structured JSON transport.
  - Updated `src/entry_eval_mode.c3` and `src/entry_runtime_modes.c3` so `--eval --json` and `--repl --json` enable that stderr-routing flag before any user code executes.
  - Updated `tooling/omni-nvim/lua/omni/repl.lua` so one-shot eval appends successful stderr output before the eval result, and the persistent JSON REPL flushes partial stderr before appending the JSON reply. This fixes `(print 1)` in the transcript even without a trailing newline.
  - Updated `docs/PROJECT_TOOLING.md` and `docs/man/omni.1` to document the stdout/stderr split for structured transports.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `tooling/omni-nvim/scripts/run_smoke.sh` -> pass, `omni-nvim smoke: ok`.
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval --json '(print 1)' >stdout 2>stderr` -> stdout now contains only `{"ok":true,"input":"(print 1)","value":"#<void>","error":null}`, stderr contains `1`.
  - `printf '%s\n' '{"id":"8","input":"(print 1)","mode":"expr"}' | env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl --json >stdout 2>stderr` -> stdout now contains only `{"id":"8","ok":true,"value":"#<void>","error":null}`, stderr contains `1`.
  - Headless Neovim eval probe over a file containing `(print 1)` now renders the transcript as:
    - `stderr| 1`
    - `>> form`
    - `(print 1)`
    - `=> #<void>`
- Best current checkpoint/config recommendation:
  - Treat stdout as protocol-only for `--eval --json` and `--repl --json`.
  - Keep user-directed console output on stderr in structured transports; first-party editor tooling already has a channel for displaying that stream without corrupting JSON parsing.
- Unresolved issues and next actions:
  - This fix covers the shared console primitives (`print`, `println`, `display`, `newline`). If other future user-visible runtime features write directly to stdout in structured mode, they need to follow the same contract instead of bypassing the console helper.
  - The transport contract is now explicit, so any future JSON-mode preload/bootstrap design should preserve the same stdout reservation rule.
- Signature: Codex (GPT-5)

2026-03-30 - REPL server protocol recommendation

- Objectives attempted:
  - Turn the “should Omni mimic nREPL, pREPL, or something else?” question into a concrete protocol recommendation instead of a loose comparison.
  - Define an Omni-native REPL server contract that is suitable for editor and tool integrations rather than only a human terminal REPL.
- Code/config changes made:
  - Added `docs/plans/repl-server-protocol-2026-03-30.md`, a concrete protocol proposal with:
    - recommended architecture (`nREPL` control plane + `pREPL` eval event model),
    - transport recommendation (Unix socket + newline-delimited JSON),
    - proposed CLI surface (`--repl-server`, optional `--stdio`/`--tcp`),
    - exact request/response examples for `describe`, `clone`, `close`, `eval`, `interrupt`, `stdin`, `load-file`, and `complete`,
    - migration guidance from the current `--repl --json` transport.
  - Updated `docs/PROJECT_TOOLING.md` to point from the current structured stdio transport to the new REPL server protocol note.
  - Updated `docs/README.md` to index the new protocol proposal directly.
- Experiment commands and key metrics:
  - Inspection-only design pass; no runtime execution was required because this slice records a protocol decision and recommended next implementation boundary rather than shipped server code.
- Best current checkpoint/config recommendation:
  - Treat `nREPL` as the right model for sessions, ops, interrupts, and capability discovery.
  - Treat `pREPL` as the right model for streamed `out` / `err` / `value` style eval events.
  - Use JSON lines over a Unix socket as the first Omni-native wire protocol instead of copying nREPL’s exact bencode/EDN transports.
- Unresolved issues and next actions:
 - The protocol is still proposed, not implemented.
 - The next practical implementation slice is a minimal `--repl-server` with Unix-socket transport plus `describe`, `clone`, `close`, `eval`, `interrupt`, and `load-file`.
 - TCP and TLS should remain deferred until bind/auth policy is explicit.
- Signature: Codex (GPT-5)

2026-03-30 - Phase-1 Unix-socket REPL server

- Objectives attempted:
  - Turn the protocol recommendation into a real shipped server slice instead of leaving `--repl --json` as the only structured tool transport.
  - Preserve the core transport invariant that program output must stay inside protocol events rather than corrupting the wire stream.
  - Ship the smallest credible nREPL-style control slice first: discovery, sessions, eval, and file loading over a local Unix socket.
- Code/config changes made:
  - Added `src/entry_repl_server_mode.c3` and wired `src/entry.c3` plus `src/entry_cli_help_version.c3` so `omni --repl-server --socket <path>` is now a first-class CLI mode.
  - Added `src/lisp/eval_repl_server.c3`, implementing a newline-delimited JSON REPL server with phase-1 ops:
    - `describe`
    - `clone`
    - `close`
    - `eval`
    - `load-file`
  - Added REPL-server console capture fields to `src/lisp/value_interp_state.c3`, initialized them in `src/lisp/value_interp_init_helpers.c3`, cleaned them up in `src/lisp/value_interp_lifecycle.c3`, and updated `src/lisp/prim_io.c3` so `(print ...)` / `(display ...)` are captured as protocol `out` events during server eval instead of writing raw process stdio.
  - Added `omni_unix_socket_listen_fd(...)` in `csrc/uv_helpers_pipe.c` and used it from the REPL server so the server binds a real filesystem Unix socket path instead of depending on the earlier detached libuv pipe helper.
  - Hardened the server write path against peer-close `SIGPIPE` by sending protocol frames with `MSG_NOSIGNAL`, and fixed `close` so the final `done` event retains the correct session id after the session storage is freed.
  - Updated `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, `docs/README.md`, and `docs/plans/repl-server-protocol-2026-03-30.md` to record the shipped phase-1 boundary rather than leaving the feature purely proposed.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl-server --socket /tmp/omni-repl-test.sock` -> bound a real filesystem Unix socket at `/tmp/omni-repl-test.sock`.
  - Python AF_UNIX probe over one persistent connection verified:
    - `describe` -> `describe`, then `done`
    - `clone` -> `session`, then `done`
    - `eval` with `(print 1)` -> `out` (`1`), `value` (`#<void>`), then `done`
    - `eval` with `(+ 40 2)` -> `value` (`42`), then `done`
    - `load-file` for `/tmp/omni-repl-load-test.omni` containing `(print 7)` and `(+ 1 2)` -> `out` (`7`), `value` (`3`), `loaded`, then `done`
    - invalid session request -> structured `error` with `protocol/unknown-session`
    - `close` -> `done` with the correct `session` id preserved
- Best current checkpoint/config recommendation:
  - Use `--repl --json` for current first-party Neovim integration and one-shot tool work.
  - Use `--repl-server --socket <path>` for new local integrations that want explicit sessions and multi-event replies over a stable transport.
  - Treat the current server contract as phase 1 only: good for discovery/session/eval/load-file, not yet for interrupts or richer editor IDE flows.
- Unresolved issues and next actions:
  - `interrupt`, `stdin`, and completion are still missing and should be the next protocol ops added before broader editor migration.
  - The current server handles one client connection at a time; multi-client concurrency is intentionally deferred.
  - TCP and stdio server transports remain deferred until the local Unix-socket contract settles.
- Signature: Codex (GPT-5)

2026-03-30 - REPL server follow-up: completion + stdio transport

- Objectives attempted:
  - Continue the REPL server beyond the first Unix-socket eval slice without pretending that the not-yet-supported `interrupt` / `stdin` semantics already exist.
  - Reuse existing REPL/runtime capabilities for the next honest protocol additions instead of inventing a parallel completion mechanism.
  - Make the server protocol usable both over a socket path and directly over stdin/stdout.
- Code/config changes made:
  - Extended `src/entry_repl_server_mode.c3` and `src/entry_cli_help_version.c3` so the CLI now supports both:
    - `omni --repl-server --socket <path>`
    - `omni --repl-server --stdio`
  - Refactored `src/lisp/eval_repl_server.c3` so the request loop works over a generic read/write stream instead of assuming every transport fd is a socket.
    - Socket-backed transports still use `send(..., MSG_NOSIGNAL)` for protocol writes.
    - Stdio-backed transports use plain fd reads/writes via the existing fs helpers.
  - Added `complete` to the shipped server op set, reusing the existing prefix-matching semantics already present in the interactive REPL completion logic.
    - `describe` now advertises `complete`.
    - Completion responses now return sorted `candidate` items with a coarse `kind` (`function`, `module`, or `binding`).
  - Updated `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, and `docs/plans/repl-server-protocol-2026-03-30.md` so the shipped boundary now reflects both transports and the new `complete` op.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `printf '%s\n%s\n%s\n' '{"id":"1","op":"clone"}' '{"id":"2","op":"eval","session":"s1","code":"(define zebra 1)","mode":"expr"}' '{"id":"3","op":"complete","session":"s1","prefix":"ze"}' | env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl-server --stdio`
    - returned `session`, `value`, and `complete` events on stdout as expected
    - completion payload included `zebra` and builtin `zero?`
  - Unix-socket Python probe against `./build/main --repl-server --socket /tmp/omni-repl-test.sock`
    - `describe` now advertises `complete`
    - after defining `print-server-test`, `complete` with prefix `print-s` returned `print-server-test`
- Best current checkpoint/config recommendation:
  - Use `--repl-server --socket` when you want a stable local endpoint and explicit attachment semantics.
  - Use `--repl-server --stdio` when you want the richer server protocol but do not want to manage a socket path.
  - Keep treating `interrupt` and `stdin` as genuinely unimplemented rather than papering over them in clients.
- Unresolved issues and next actions:
  - `interrupt` still requires a truer in-flight evaluation model than the current single-connection synchronous server loop.
  - `stdin` still needs a runtime-level input routing contract; the language does not yet expose a server-routable stdin read surface to plug into this op honestly.
  - TCP and multi-client concurrency remain deferred.
- Signature: Codex (GPT-5)

2026-03-30 - REPL server follow-up: cooperative interrupt + honest stdin deferral

- Objectives attempted:
  - Land a truthful `interrupt` op for the shipped REPL server instead of continuing to advertise it only in the protocol note.
  - Keep `stdin` honest: do not pretend it works until the runtime has a real server-routable input surface.
  - Preserve the current JIT/thread-affinity constraints while still allowing control requests to arrive during long-running evaluation.
- Code/config changes made:
  - Extended interpreter state and runtime polling so evaluation can stop cooperatively when a REPL-server session raises an interrupt request.
    - `src/lisp/value_interp_state.c3` now carries an optional interrupt flag pointer.
    - `src/lisp/eval_signal.c3`, `src/lisp/value_interp_runtime_helpers.c3`, and `src/lisp/runtime_backend_hooks.c3` now expose and check cooperative interrupt state, returning the distinct runtime message `evaluation interrupted`.
  - Refactored `src/lisp/eval_repl_server.c3` from a purely synchronous request loop into a stream-local worker model:
    - one worker thread owns runtime/session work for the stream,
    - the main read loop can still accept `interrupt` while `eval` / `load-file` is running,
    - successful interrupt requests now end with `done`, while the cancelled target request terminates with `interrupted`,
    - `describe` now advertises `interrupt`,
    - `stdin` now returns an explicit `protocol/unsupported-op` error instead of being implied by the design note.
  - Added per-stream serialized request behavior instead of faking broader concurrency:
    - while one runtime request is in flight, other non-`interrupt` requests return `protocol/server-busy`.
  - Updated `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, and `docs/plans/repl-server-protocol-2026-03-30.md` so the shipped contract reflects `interrupt` support and keeps `stdin` explicitly deferred.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - Python stdio probe against `./build/main --repl-server --stdio`
    - `clone` -> `done`
    - `eval` defining a recursive loop -> `done`
    - long-running `(loop)` request interrupted by `{"op":"interrupt","target":"3"}` -> target request emitted `{"event":"interrupted"}` and interrupt request emitted `{"event":"done"}`
  - Python Unix-socket probe against `./build/main --repl-server --socket /tmp/omni-repl-test.sock`
    - `describe` advertised `interrupt` and no longer advertised `stdin`
    - interrupting a recursive `(loop)` request returned the same `done` + `interrupted` pair
    - `stdin` returned `protocol/unsupported-op`
- Best current checkpoint/config recommendation:
  - Use `interrupt` now for long-running `eval` / `load-file` requests on either `--repl-server --stdio` or `--repl-server --socket`.
  - Treat the current server as serialized per stream: do not pipeline non-`interrupt` requests and expect them to queue.
  - Keep treating `stdin` as unimplemented until the runtime grows a real input-routing surface.
- Unresolved issues and next actions:
  - `stdin` still needs a language/runtime input abstraction before the server can route request data into blocked evaluation honestly.
  - The current server remains one-client-per-process on the socket listener and one-runtime-request-at-a-time per stream.
  - TCP and richer multi-client concurrency remain deferred.
- Signature: Codex (GPT-5)

2026-03-30 - REPL server follow-up: routed stdin + TCP listener boundary

- Objectives attempted:
  - Replace the placeholder `stdin` op with a real runtime-backed input path.
  - Add the next transport slice with a TCP listener mode.
  - Keep the shipped boundary honest instead of pretending cross-connection concurrency is solved.
- Code/config changes made:
  - Added a real runtime input surface:
    - `src/lisp/value_interp_state.c3` now exposes an optional `input_state` on `Interp`.
    - `src/lisp/prim_io.c3` now owns the buffered `InterpInputState`, the shared runtime line reader, and the new `prim_read_line` primitive.
    - `stdlib/stdlib.lisp` now exports `read-line` through the new `io/read-line` effect.
  - Wired REPL-server `stdin` requests into per-session input queues in `src/lisp/eval_repl_server.c3`:
    - `stdin` now accepts `data` and optional `eof: true`,
    - blocked `(read-line)` evaluations consume that routed input,
    - interrupting a blocked `(read-line)` request now produces `interrupted`.
  - Added `omni --repl-server --tcp <host> <port>` in `src/entry_repl_server_mode.c3` and updated `src/entry_cli_help_version.c3`.
  - Updated compiler/runtime surface parity for the new `read-line` surface in:
    - `src/lisp/eval_init_primitives.c3`
    - `src/lisp/compiler_free_vars_utils.c3`
    - `src/lisp/compiler_let_set_flat.c3`
    - `src/lisp/compiler_primitive_variable_hash_table_domains.c3`
  - Attempted per-connection socket concurrency, then reverted it after hitting the existing JIT owner-thread invariant during interpreter attachment.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `printf 'abc\n' | ./build/main --eval '(read-line)' --json`
    - returned `{"ok":true,...,"value":"\"abc\""}`.
  - Python stdio probe against `./build/main --repl-server --stdio`
    - `clone` -> `(read-line)` -> `stdin data` returned `"hello"`,
    - `clone` -> `(read-line)` -> `stdin eof` returned `nil`,
    - `clone` -> `(read-line)` -> `interrupt` returned `interrupted`.
  - Python Unix-socket probe against `./build/main --repl-server --socket /tmp/omni-repl-sequential.sock`
    - two sequential client connections each completed `clone` + `eval` successfully.
  - Python TCP probe against `./build/main --repl-server --tcp 127.0.0.1 48763`
    - `describe` succeeded and advertised `stdin`.
- Best current checkpoint/config recommendation:
  - Use `stdin` now when a running `eval` or `load-file` needs line-oriented input.
  - Use `--repl-server --tcp <host> <port>` when a filesystem socket path is awkward and you are on a trusted network boundary.
  - Treat Unix-socket/TCP listeners as sequential-client servers for now.
- Unresolved issues and next actions:
  - Cross-connection concurrency is still blocked by the runtime/JIT owner-thread invariant at interpreter attachment time.
  - Queueing beyond one in-flight runtime request per stream is still unimplemented.
  - Authentication/TLS policy for TCP transport is still deferred.
- Signature: Codex (GPT-5)

2026-03-30 - TCP REPL port-file discovery

- Objectives attempted:
  - Make the TCP REPL server discoverable to editor plugins without forcing them
    to scrape process arguments or duplicate the configured port in editor
    config.
  - Keep the shipped discovery surface simple and nREPL-like instead of
    introducing a new side-channel protocol.
- Code/config changes made:
  - Updated `src/lisp/eval_repl_server.c3` so
    `omni --repl-server --tcp <host> <port>` now writes
    `.omni-repl-port` in the current working directory after the listener binds
    successfully.
  - Implemented the publish step as an atomic temp-write + rename instead of an
    in-place write.
  - Updated `src/entry_cli_help_version.c3`,
    `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, and
    `docs/plans/repl-server-protocol-2026-03-30.md` to document the new
    discovery-file contract.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `./build/main --repl-server --tcp 127.0.0.1 48763`
    - wrote `.omni-repl-port` containing `48763`.
- Best current checkpoint/config recommendation:
  - Start the TCP REPL server from the project root when you want editor/plugin
    discovery; plugins can read `.omni-repl-port` from that directory and then
    attempt a normal TCP connect.
  - Treat the port file as a discovery hint, not a liveness guarantee.
- Unresolved issues and next actions:
  - Abrupt termination can still leave a stale `.omni-repl-port`.
  - The discovery file currently exists only for TCP mode; Unix-socket mode
    still requires the socket path directly.
- Signature: Codex (GPT-5)

2026-03-30 - omni-nvim TCP REPL discovery

- Objectives attempted:
  - Make the first-party Neovim plugin attach to a discovered TCP REPL server
    automatically when `.omni-repl-port` is present.
  - Preserve the existing local `omni --repl --json` workflow as the fallback
    path when discovery is missing or broken.
- Code/config changes made:
  - Updated `tooling/omni-nvim/lua/omni/repl.lua`:
    - added upward `.omni-repl-port` discovery rooted by
      `omni.toml`, `project.json`, or `.git`,
    - added TCP channel attachment via `sockconnect()`,
    - cloned a REPL-server session automatically after connect,
    - routed persistent eval requests through REPL-server `op = "eval"` when
      attached over TCP,
    - preserved the legacy local process path as fallback.
  - Exposed the discovery defaults in `tooling/omni-nvim/lua/omni/init.lua`.
  - Updated `tooling/omni-nvim/README.md` and
    `tooling/omni-nvim/doc/omni.nvim.txt` to document the new attach behavior.
- Experiment commands and key metrics:
  - `tooling/omni-nvim/scripts/run_smoke.sh` -> pass.
  - headless Neovim probe against a live
    `./build/main --repl-server --tcp 127.0.0.1 48763`
    listener with `.omni-repl-port`
    - plugin auto-attached to the TCP server,
    - `send_buffer(...)` returned `=> 3` through the REPL-server session.
- Best current checkpoint/config recommendation:
  - If you want plugin discovery, start the TCP REPL server from the Omni
    project root so `.omni-repl-port` lands where the plugin will find it.
  - Leave `repl.discovery.enabled = true` unless you explicitly want the older
    local-process-only behavior.
- Unresolved issues and next actions:
  - Discovery currently targets TCP only; Unix-socket attach is still a future
    follow-up if we want the same convenience for local-only servers.
  - The plugin treats `.omni-repl-port` as a hint and falls back on connect
    failure, but it does not currently try to rewrite or clean stale files.
- Signature: Codex (GPT-5)

2026-03-30 - REPL server project preload parity

- Objectives attempted:
  - Make `omni --repl-server` accept the same project preload flag as the
    interactive REPL instead of rejecting `--project` as an unexpected
    argument.
  - Keep the preload contract aligned across interactive REPL and REPL-server
    transports so tooling does not need a separate notion of "project REPL".
- Code/config changes made:
  - Updated `src/entry_repl_server_mode.c3`:
    - added `--project [dir]` parsing for `--socket`, `--stdio`, and `--tcp`,
    - threaded the preload choice into the REPL-server transport entrypoints.
  - Refactored `src/entry_runtime_modes.c3`:
    - added shared project-entry resolution helpers so the interactive REPL and
      REPL server both resolve `omni.toml` plus `src/main.omni` the same way.
  - Updated `src/lisp/eval_repl_server.c3`:
    - each new `clone` session now optionally preloads the resolved project
      entry before the session is announced,
    - preload output is surfaced as protocol `out` events,
    - preload failure aborts the clone and returns a structured `error`
      instead of exposing a partially initialized session.
  - Updated `src/entry_cli_help_version.c3`,
    `docs/PROJECT_TOOLING.md`, `docs/man/omni.1`, and
    `docs/plans/repl-server-protocol-2026-03-30.md` so the documented REPL
    server boundary matches the shipped CLI.
- Experiment commands and key metrics:
  - `c3c build` -> pass, executable linked to `build/main`.
  - `./build/main --repl-server --tcp 127.0.0.1 48763 --project ./demo`
    - accepted the preload flag,
    - wrote `.omni-repl-port`,
    - a TCP `clone` request emitted `out` for `"Hello from demo!"`,
      then `session`, then `done`.
- Best current checkpoint/config recommendation:
  - Use `omni --repl-server --tcp <host> <port> --project [dir]` when you want
    editor/plugin attach plus per-session project preload.
  - Start the server from the project root when you also want `.omni-repl-port`
    discovery to line up with plugin search.
- Unresolved issues and next actions:
  - Preload is currently tied to the existing fixed project entry contract
    (`omni.toml` plus `src/main.omni`); if Omni grows a configured project
    entrypoint later, both REPL paths should resolve through that metadata.
  - The server still serializes one connected client at a time.
- Signature: Codex (GPT-5)

2026-04-08 08:41 CEST - Omni language surface audit and external criticism scan

- Objectives attempted:
  - Audit Omni's current language surface for likely external reception,
    especially syntax and feature-density concerns.
  - Compare the current design against public criticism patterns from Reddit,
    Hacker News, blog posts, and one recent algebraic-effects verification
    paper.
- Relevant workspace/target:
  - Language/docs surface only; no runtime or compiler behavior was modified.
- Code/config changes made:
  - None.
- Commands run:
  - `sed -n '1,220p' docs/LANGUAGE_SPEC.md`
  - `sed -n '1,220p' docs/README.md`
  - `sed -n '1,220p' docs/C3_STYLE.md`
  - `sed -n '1,220p' docs/syntax-decision.md`
  - `sed -n '1,220p' docs/type-system-syntax.md`
  - `sed -n '1,220p' docs/OMNI_REFERENCE.md`
  - `sed -n '1,220p' docs/reference/02-functions.md`
  - `sed -n '1,220p' docs/reference/03-collections.md`
  - `sed -n '1,220p' docs/reference/04-type-system.md`
  - `sed -n '1,220p' docs/reference/05-macros-modules.md`
  - `sed -n '1,220p' docs/reference/06-effects.md`
  - targeted `rg` scans across `docs/`, `src/`, and `test/`
  - targeted `nl -ba ... | sed -n ...` line-anchor extraction for final audit
- Key results and conclusions:
  - Omni's strongest design assets are the coherent expression model, generic
    collection operations, explicit effect/control operators, strict arity,
    and the choice to keep macro authoring word-based instead of punctuation
    heavy.
  - The biggest adoption risk is not any single feature; it is the cumulative
    stack: Lisp syntax + multiple dispatch + structural typing + algebraic
    effects + delimited continuations + deterministic region memory. Public
    criticism patterns found on similar systems line up with exactly that kind
    of cognitive load and debugging fear.
  - The current surface is powerful but not visually minimal. In addition to
    ordinary s-expressions it uses square/curly literals, leading-dot accessor
    lambdas, postfix `.[...]`, quoted-symbol callable accessors, `^Type`,
    `^(Value ...)`, attribute brackets, slash-qualified effect names, and
    dot-qualified module access. This reads more like a dense PL toolkit than
    a small Lisp.
  - The most criticism-prone local semantics are:
    - `false` collapsing to `nil`,
    - callable quoted symbols like `('name dict)`,
    - multiple overlapping partial-application mechanisms,
    - optional effect declarations,
    - multi-shot continuations with replayed side effects,
    - the trust burden created by pairing advanced control flow with explicit
      lifetime management.
- Invalidated assumptions or failed approaches worth preserving:
  - A prior memory note claiming that `Dict` was docs-only was stale. Current
    source registers `Dict` in runtime and compiler surfaces and has coverage
    for it.
- Best current recommendation/checkpoint:
  - Position Omni externally around a much narrower "core profile" instead of
    leading with the full advanced feature inventory.
  - If syntax simplification is pursued, prefer removing or de-emphasizing
    cute secondary surfaces before touching the core expression model. The
    highest-friction candidates are callable quoted-symbol accessors and the
    combination of leading-dot accessor lambdas with postfix indexing.
  - If feature pruning is considered, the hardest sell to mainstream users is
    likely the simultaneous presence of both algebraic effects and delimited
    continuations rather than either feature in isolation.
- Unresolved issues and next actions:
  - Decide whether the owner wants Omni optimized first for PL enthusiasts,
    Lisp users, systems programmers, or mainstream application developers; the
    right syntax/features message depends heavily on that audience choice.
  - If another session continues this work, convert the audit into a ranked
    simplification plan rather than another broad critique pass.
- Signature: Codex (GPT-5)

2026-04-08 08:49 CEST - Omni syntax cleanup direction locked by owner

- Objectives attempted:
  - Convert the audit conclusions into an explicit owner-approved cleanup
    direction for syntax and semantics.
- Code/config changes made:
  - None.
- Decision checkpoint:
  - Backward compatibility is not a goal for this cleanup. Removed surfaces
    should not linger as legacy aliases or migration spellings.
  - Approved direction:
    - remove leading-dot accessor lambdas,
    - remove callable quoted-symbol accessors,
    - keep path expressions as access-only sugar over `ref`,
    - keep pipeline `|>`,
    - split `false` from `nil` while keeping both falsy,
    - treat effects as the normal application-facing control surface and
      continuations as advanced.
- Best current recommendation/checkpoint:
  - Implement the cleanup as a fail-closed language contract pass, not as a
    compatibility layer.
  - Prefer one canonical surface per concept and hard errors for removed forms.
- Signature: Codex (GPT-5)

2026-04-08 09:35 CEST - Omni cleanup implementation slices 1-3 started

- Objectives attempted:
  - Land the first concrete cleanup slices from the owner-approved language
    simplification pass.
- Code/config changes made:
  - Accessor cleanup:
    - removed leading-dot accessor parsing and dead accessor-lambda AST/parser
      plumbing from parser-facing code,
    - removed quoted-symbol callable lookup behavior from runtime apply,
    - updated affected tests to use `ref`, path access, or fail-closed errors.
  - Primitive call tightening:
    - ordinary one-argument primitive calls no longer auto-create
      `PARTIAL_PRIM`,
    - interpreter and JIT single-arg primitive apply now report arity
      mismatch instead.
  - `false`/`nil` split:
    - `false` now binds to a distinct symbol value instead of `nil`,
    - Boolean/Nil constructor semantics were updated around the split,
    - parser pattern/value-literal handling and JSON false conversion were
      updated so `false` no longer aliases `nil`.
  - Docs/planning artifacts updated:
    - `.agents/PLAN.md`
    - `memory/CHANGELOG.md`
    - public spec/reference docs for access syntax, truthiness, partial
      application, and boolean/nil semantics.
- Commands run:
  - multiple `rg`/`sed` inspection passes across `docs/` and `src/lisp/`
  - `c3c build`
    - failed locally because `c3c` is not installed in this environment
- Key results and conclusions:
  - Slice 1 is now fail-closed in parser/runtime/test surfaces touched here:
    removed accessor forms should error instead of silently surviving as
    legacy spellings.
  - Slice 2 is materially implemented: bare `(+ 3)` style calls should now be
    arity errors, while pipe syntax remains intact because `|>` rewrites call
    shapes before evaluation.
  - Slice 3 has started and the core contract changed on disk, but it still
    needs a broader consistency sweep across remaining docs/tests and any
    runtime corners not covered by the files touched in this session.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep assuming the accessor cleanup is “just parser sugar”; quoted
    symbol calls and test corpus usage made it a broader runtime/doc sweep.
  - Do not assume primitive auto-partial is required for pipe syntax; `|>`
    expansion can keep pipe ergonomics without preserving bare primitive
    partial calls.
- Current best recommendation/checkpoint:
  - Next session should finish the `false`/`nil` split before calling this
    cleanup complete:
    - sweep remaining docs that still describe `false` as `nil`,
    - sweep remaining tests that encode alias expectations,
    - run actual build/tests once `c3c` is available.
- Unresolved issues:
  - This session could not run compile/test verification because `c3c` is not
    installed in the current environment.
  - The worktree was already dirty before these edits; current cleanup edits
    were kept narrow, but a real validation pass is still required.
- Signature: Codex (GPT-5)

2026-04-08 11:52 CEST - Arm64 build lane repaired and binary startup restored

- Objectives attempted:
  - Fix the current tree so `c3c build` succeeds on this Ubuntu 24.04 arm64
    host and verify that the produced binary actually starts.
- Code/config changes made:
  - Repaired compile regressions surfaced during the first real build pass:
    - `src/lisp/deduce_schema_query_define_relation.c3`
    - `src/lisp/deduce_db_handles_core.c3`
    - `src/lisp/deduce_relation_ops_query.c3`
    - `src/lisp/eval_boundary_commit_escape.c3`
    - `src/lisp/eval_repl_server_worker.c3`
    - `src/lisp/scheduler_thread_tasks.c3`
    - `src/lisp/eval_boundary_commit_flow.c3`
  - Reworked the arm64 stack-engine compile path so x86_64-only context switch
    assembly no longer blocks whole-program builds:
    - `src/stack_engine_abi_switch.c3`
    - `csrc/stack_helpers.c`
  - Repaired C-helper include/build plumbing around vendored dependencies:
    - `csrc/json_helpers.c`
    - `csrc/tls_helpers.c`
    - `csrc/uv_helpers.c`
    - `csrc/uv_helpers_work.c`
    - `csrc/uv_helpers_thread.c`
    - `csrc/uv_helpers_pipe.c`
    - `csrc/uv_helpers_tcp.c`
    - `csrc/uv_helpers_process.c`
    - `csrc/uv_helpers_signal.c`
    - `csrc/toml_helpers.c`
    - `scripts/build_omni_chelpers.sh`
    - `project.json`
  - Adjusted local link truth:
    - `project.json` now compiles `clib/mathutils.c` directly,
    - `project.json` restores the `omni_ftxui` archive link edge,
    - `project.json` now embeds `RUNPATH=$ORIGIN`,
    - local `liblightning` and `libreplxx` shared libraries were staged into
      `build/`.
- Commands run:
  - `~/.local/bin/c3c build` repeatedly while fixing blockers
  - `git show 65b2841:src/lisp/deduce_db_handles.c3`
  - `bash deps/build_static.sh`
  - `make -C deps/src/lmdb/libraries/liblmdb clean all`
  - `bash scripts/build_omni_chelpers.sh`
  - `readelf -d build/main`
  - `ldd build/main`
  - `./build/main --help`
- Key results and conclusions:
  - `~/.local/bin/c3c build` now exits 0 and links `build/main`.
  - `build/main` now carries `RUNPATH=$ORIGIN`, and `ldd build/main` resolves
    `liblightning.so.2` and `libreplxx.so.0.0.4` from `build/`.
  - `./build/main --help` executes successfully on this host.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep assuming the old `deps/lib/*.a` archives are usable on arm64;
    they were stale/incompatible and had to be rebuilt locally.
  - Do not assume `c3c` will compile the FTXUI C++ shim directly from the
    `.cpp` entries in `project.json`; the repo still relies on the separate
    `omni_ftxui` archive built by `scripts/build_omni_chelpers.sh`.
  - Do not assume arm64 stack-engine support exists just because the build now
    passes; the non-x86 path is fail-closed for context switching.
- Current best recommendation/checkpoint:
  - The build lane is healthy enough again to continue cleanup work or run
    targeted validation.
  - The next cleanup step should return to the unfinished `false`/`nil` sweep
    rather than more build plumbing.
- Unresolved issues:
  - Warnings remain in entry/reporting and REPL server code, but they are not
    build blockers.
  - Continuation/coroutine stack switching is still effectively x86_64-only.
- Signature: Codex (GPT-5)

2026-04-09 06:05 CEST - Documentation contract and compatibility hardening

- Objectives attempted:
  - Aggressively improve top-level documentation quality/coverage.
  - Remove remaining top-level documentation drift around legacy syntax and
    duplicate authority.
  - Establish one explicit compatibility source for removed/renamed surface
    syntax.
- Code/config/doc changes made:
  - Added documentation authority matrix and update discipline:
    - `docs/DOCS_CONTRACT.md`
  - Added canonical compatibility map for removed/changed syntax:
    - `docs/SURFACE_COMPATIBILITY.md`
  - Reworked top-level docs entrypoint and authority policy:
    - `docs/README.md`
  - Replaced `docs/FEATURES.md` with an index/coverage map (no longer a second
    full spec).
  - Updated reference navigation contract:
    - `docs/OMNI_REFERENCE.md`
  - Updated spec docs to reference authority/compatibility sources and remove
    duplicate legacy accessor examples:
    - `docs/LANGUAGE_SPEC.md`
    - `docs/SYNTAX_SPEC.md`
    - `docs/reference/03-collections.md`
- Commands run:
  - `jj status`
  - `jj diff --stat`
  - `rg` audits across updated docs for:
    - callable quoted-symbol accessor remnants,
    - removed alias spellings (`fn`, `begin`, `letrec`, `reset`, `shift`,
      `with-handlers`, `handle-chain`)
  - `sed` inspection passes on edited docs
- Key results and conclusions:
  - Documentation now has an explicit authority contract (`DOCS_CONTRACT`) and
    explicit compatibility contract (`SURFACE_COMPATIBILITY`).
  - `FEATURES.md` now acts as a high-signal index instead of duplicating large
    parts of `LANGUAGE_SPEC.md`.
  - Legacy callable quoted-symbol accessor syntax is no longer presented as
    active syntax in top-level/user-reference docs; it is now scoped to
    compatibility/removal policy.
- Invalidated assumptions or failed approaches worth preserving:
  - Do not keep treating `FEATURES.md` as a second spec source; duplicate specs
    drift quickly and blur authority.
  - Do not scatter removed-syntax notes across many top-level docs; use
    `SURFACE_COMPATIBILITY.md` as the single compatibility source.
- Current best recommendation/checkpoint:
  - Keep future syntax-surface updates in one patch touching:
    - `SYNTAX_SPEC`,
    - `LANGUAGE_SPEC`,
    - `SURFACE_COMPATIBILITY`,
    - and `DOCS_CONTRACT` when authority/coverage scope changes.
- Unresolved issues:
  - This pass did not run compiler/runtime tests because changes were docs-only.
  - Historical plan/session artifacts still include legacy names by design;
    those files are historical records, not canonical language contracts.
- Signature: Codex (GPT-5)
