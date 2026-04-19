# Memory Changelog Index Part 08

Source: `memory/CHANGELOG.md`

    - direct smokes for BigFloat dtype/ref/inferred construction/Array/List
      conversion/scalar fill/concrete copy/map rejection
    - focused advanced collections/module group on host
      -> `257 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `257 passed, 0 failed`
    - bounded container `memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`
  - ASAN note: `c3c build main --sanitize=address --output-dir build/asan --build-dir build/obj-asan`
    failed immediately with the compiler message `Address sanitizer is only
    supported on Linux, FreeBSD, NetBSD, Darwin and Windows.` No ASAN binary
    was produced in this environment.

- Completed Tensor real numeric narrowing for native `Double` constructors:
  - Routed Tensor constructor and Tensor scalar-map conversion through the
    shared `try_numeric_to_double` path, so native `Double` Tensor inputs now
    accept `Integer`, `Double`, `BigInteger`, and `BigFloat` values when they
    can narrow to finite `Double`.
  - BigComplex values and out-of-`Double`-range BigFloat/BigInteger values
    fail closed instead of silently truncating or fabricating storage values.
  - Added focused advanced collections/module regressions for inferred
    BigInteger/BigFloat leaves, explicit BigFloat scalar fill, explicit
    BigInteger flat data, out-of-range BigFloat rejection, and BigComplex
    rejection.
  - Updated language/reference docs and Tensor plan/status artifacts.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for BigFloat Tensor data, BigInteger Tensor data,
      out-of-range BigFloat rejection, and BigComplex rejection
    - focused advanced collections/module group on host
      -> `248 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `248 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-076` inferred Tensor constructor overloads:
  - Added `(Tensor data)`, `(Tensor data Double)`, and `(Tensor Double data)`
    as constructor-dispatch surfaces that infer native `Double` tensor shape
    from numeric scalars or rectangular nested arrays/proper lists.
  - Preserved the explicit `(Tensor Double shape data-or-scalar)` constructor
    for scalar fills and exact-length flat data.
  - Extended the `Tensor` primitive registration to variable arity and accepted
    both `Double` and `'Double` as the dtype marker.
  - Added focused regressions for inferred vector, matrix, scalar rank-0,
    dtype prefix/suffix, quoted dtype, empty vector, ragged rejection, and
    non-numeric rejection.
  - Updated language/reference docs and Tensor plan/status artifacts.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for inferred vector, inferred matrix shape, dtype-prefix
      construction, and ragged rejection
    - focused advanced collections/module group on host
      -> `242 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `242 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed Tensor collection constructor conversions:
  - Added `(Array tensor)` and `(List tensor)` support to the canonical
    collection constructor/conversion surfaces.
  - Tensor conversions force lazy tensor expressions when needed and return
    flat row-major element values. Shape/rank metadata remains explicit through
    `shape` and `rank`; conversion does not synthesize nested collections.
  - Added focused advanced collections/module regressions for concrete Tensor
    conversion, lazy `map` conversion, and lazy `contract` conversion.
  - Updated language/reference docs and Tensor plan/status artifacts.
  - validation:
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for `(Array tensor)`, `(List tensor)`, lazy map
      conversion, and zero-size Tensor conversion
    - focused advanced collections/module group on host
      -> `232 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `232 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed `TENSOR-090C` BLAS `dgemv` contraction acceleration:
  - Extended the private Tensor BLAS C shim to resolve `cblas_dgemv`, expose
    availability and call-count probes, and call `dgemv` without adding a
    public backend-specific Tensor surface.
  - Added Tensor evaluator eligibility for contiguous row-major rank-2/rank-1
    and rank-1/rank-2 single-axis `Double` contracts, including transposed
    matrix-vector and vector-matrix layouts.
  - Unsupported rank/layout/symbol cases continue through the pure C3
    contraction fallback.
  - Updated Tensor plan/status artifacts and focused path-sensitive advanced
    collection/module regressions.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for matrix-vector, transposed matrix-vector,
      vector-matrix, and vector-transposed-matrix contraction results
    - focused advanced collections/module group on host
      -> `226 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `226 passed, 0 failed`

- Completed BigComplex component access:
  - Added C++ helper exports for BigComplex real component extraction,
    imaginary component extraction, and conjugation.
  - Added `real-part`, `imag-part`, and `conjugate` as numeric primitives with
    primitive-table registration and AOT lookup.
  - `real-part` and `imag-part` return `BigFloat` components for BigComplex
    inputs. Real scalar inputs keep their existing value as the real part,
    use `0` as the imaginary part, and are preserved by `conjugate`.
  - Updated language/reference docs, the scalar numerics plan, and focused
    advanced numeric regressions.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for BigComplex `real-part`, `imag-part`, `conjugate`, and
      real-scalar `conjugate`
    - focused advanced numeric float-math group on host
      -> `172 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `172 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed hyperbolic scalar math:
  - Added `sinh`, `cosh`, and `tanh` as standard math primitives.
  - Routed Double inputs through the C math library, BigFloat inputs through
    Boost.Multiprecision-preserving helper ops, and BigComplex inputs through
    complex-preserving helper ops.
  - Added primitive registration, AOT lookup, and focused advanced numeric
    regressions for Double, BigFloat, and BigComplex behavior.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for Double, BigFloat, and BigComplex hyperbolic results
    - focused advanced numeric float-math group on host
      -> `163 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `163 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed the first BigComplex scalar math extension:
  - Extended `csrc/big_complex_helpers.cpp` with BigComplex-preserving
    `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `exp`, `log`, `log10`,
    `sqrt`, and `pow`.
  - Routed the matching math primitives through BigComplex before BigFloat or
    Double paths. `atan2` remains a real-plane helper and now rejects complex
    operands explicitly.
  - Added advanced numeric regressions for BigComplex trigonometric,
    exponential/logarithmic, square-root, power, and `atan2` rejection paths.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for BigComplex `sqrt`, `exp`, `log`, `sin`, `cos`, `pow`,
      and `atan2` rejection
    - focused advanced numeric float-math group on host
      -> `152 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `152 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed the first BigComplex numeric slice:
  - Added Boost.Multiprecision decimal complex helper plumbing through
    `csrc/big_complex_helpers.cpp` and the C3 `BIG_COMPLEX` runtime value.
  - Registered `BigComplex` as a callable constructor/type descriptor and a
    `Number` subtype.
  - Added `String`, printing, equality/hash, scope-boundary copy/promotion,
    `+`, `-`, `*`, `/`, unary `-`, `zero?`, and `abs` returning a `BigFloat`
    magnitude.
  - Ordered operations fail closed for complex operands: `<`, `>`, `<=`, `>=`,
    `min`, `max`, `positive?`, and `negative?` report that complex numbers
    are not ordered.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for constructor/type identity, `Number` identity,
      addition, multiplication, division, `abs`, and ordering rejection
    - focused advanced numeric float-math group on host
      -> `145 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `145 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed exact BigFloat rounding-to-integer support:
  - Added `omni_big_float_round_to_integer_string` in
    `csrc/big_float_helpers.cpp` so `floor`, `ceiling`, `round`, and
    `truncate` operate on `cpp_dec_float_50` directly instead of narrowing
    through `Double`.
  - Routed BigFloat inputs in the rounding primitives to exact integer
    construction. Results narrow to `Integer` when representable and promote to
    `BigInteger` otherwise.
  - Added a bounded decimal-digit allocation cap for huge BigFloat integer
    materialization; over-cap results fail closed with the primitive-specific
    BigFloat integer-range error.
  - Negative-memory note: do not convert rounded `cpp_dec_float_50` directly to
    `cpp_int` for this path. Local Boost conversion saturated near fixed-width
    limits; rendering the rounded value as fixed decimal text and reparsing via
    the BigInteger constructor is the validated path.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build main --output-dir build --build-dir build/obj2`
    - direct smokes for large BigFloat floor promotion, result type,
      negative `round`, negative `truncate`, and over-cap failure
    - focused advanced numeric float-math group on host
      -> `134 passed, 0 failed`
    - bounded container rerun of the same focused group
      -> `134 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
    - `git diff --check`

- Completed the BigFloat scalar math slice:
  - Extended `csrc/big_float_helpers.cpp` with BigFloat-preserving wrappers for
    trigonometric, inverse trigonometric, exponential/logarithmic, power/root,
    `math/lgamma`, `math/erf`, `math/erfc`, `stats/normal-cdf`, and
    `stats/normal-quantile`.
  - Updated the math primitives so `BigFloat` inputs no longer have to narrow to
    `Double` for those operations; non-`BigFloat` inputs keep the existing
    `Double` result path.
  - `stats/normal-quantile` keeps the existing strict probability domain for
    BigFloat inputs.
  - validation so far:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build main --output-dir build --build-dir build/obj2`
    - focused advanced numeric float-math group
      -> `127 passed, 0 failed`

- Completed the first full BigFloat numeric slice:
  - Added Boost.Multiprecision `cpp_dec_float_50` helper plumbing and runtime
    `BIG_FLOAT` values with scope-region destruction, copy-to-parent,
    escape-promotion, printing, `String`, `Double`, and `Integer` conversion
    paths.
  - Registered `BigFloat` as a callable constructor/type descriptor and a
    `Number` subtype. `number?` / `is?` now recognize `BigFloat` through the
    type hierarchy.
  - Added BigFloat arithmetic and comparison support for `+`, `-`, `*`, `/`,
    `<`, `>`, `<=`, `>=`, `=`, `abs`, `min`, and `max`. When a `BigFloat`
    participates, the result stays `BigFloat` for arithmetic.
  - `parse-number` now promotes syntactically valid floating inputs that
    overflow `Double`, for example `"1e309"`, to `BigFloat`.
  - Existing Double-returning transcendentals still require values
    representable as `Double`; out-of-range `BigFloat` inputs fail closed
    instead of silently narrowing.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `115 passed, 0 failed`
    - bounded container rerun of the same advanced numeric group
      -> `115 passed, 0 failed`
    - direct smokes:
      - `(String (+ (BigFloat "1.25") 2))` -> `"3.25"`
      - `(String (/ (BigFloat "10") 4))` -> `"2.5"`
      - `(= (type-of (parse-number "1e309")) 'BigFloat)` -> `true`
      - `(String (parse-number "1e309"))` -> `"1e+309"`
      - `(is? (BigFloat "1.25") 'Number)` -> `true`
      - `(/ (BigFloat "1") 0)` -> `/: division by zero`
      - `(sin (BigFloat "1e309"))`
        -> `sin: argument 1 expected number representable as Double`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed

- Narrowed the StackCtx boundary-copy fix after efficiency review:
  - Replaced the broad "any StackCtx leaf/data-container return copies
    defensively" rule with a low-headroom gate. StackCtx returns now skip the
    reuse classifier only when the current stack is already below
    `BOUNDARY_ALIAS_STACK_MIN_HEADROOM`; otherwise they use the normal
    scope-aware fast-reuse path.
  - This keeps the original nested effect payload safety while avoiding an
    unnecessary defensive-copy tax for StackCtx payloads that still have enough
    continuation-stack headroom to classify safely.
  - validation:
    - `c3c build --obj-out obj`
    - direct nested effect payload predicate -> `true`
    - bounded container `OMNI_LISP_TEST_SLICE=memory-lifetime-smoke`
      -> `225 passed, 0 failed`
    - same bounded memory-smoke run with boundary traversal summary
      -> `copy_fast_reuse=3`, `copy_defensive=89`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-tco`
      -> `1 passed, 0 failed`, `copy_tag_cons=0`, `copy_site_tco=0`
    - `OMNI_LISP_TEST_SLICE=limit-busting` -> `17 passed, 0 failed`
    - `OMNI_LISP_TEST_SLICE=tco-recycling`
      -> `11 passed, 0 failed`, `copy_site_tco=20`
    - `(length (range 4000))` -> `4000`, about 0.32s; `(length (range 16000))`
      -> `16000`, about 4.18s.

- Completed the `parse-number` BigInteger promotion slice:
  - Updated `prim_string_to_number` so syntactically valid decimal integer
    overflow/underflow returns `BigInteger` instead of `nil`.
  - Kept malformed integer strings maybe-valued: overflow-looking strings with
    a non-digit suffix still return `nil` rather than raising through the
    `BigInteger` constructor.
  - Exact fixed-width boundaries remain fixed-width: `-9223372036854775808`
    parses to `Integer`; positive/negative decimal strings outside that range
    parse to `BigInteger`.
  - Updated `Double` string coercion to accept a `parse-number` BigInteger
    result when it can narrow to a finite `Double`.
  - validation:
    - `c3c build --obj-out obj`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `101 passed, 0 failed`
    - bounded container rerun of the same advanced numeric group
      -> `101 passed, 0 failed`
    - direct smokes:
      - `(String (parse-number "9223372036854775808"))`
        -> `"9223372036854775808"`
      - `(String (parse-number "-9223372036854775809"))`
        -> `"-9223372036854775809"`
      - `(= (type-of (parse-number "9223372036854775808")) 'BigInteger)`
        -> `true`
      - `(= (type-of (parse-number "-9223372036854775808")) 'Integer)`
        -> `true`
      - `(parse-number "9223372036854775808x")` -> `nil`
      - `(Double "9223372036854775808")` -> `9.22337203685478e+18`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed

- Completed the BigInteger bitwise primitive slice:
  - Extended the Boost.Multiprecision `cpp_int` helper op set with
    `bitwise-and`, `bitwise-or`, `bitwise-xor`, `bitwise-not`, `lshift`, and
    `rshift` support.
  - Updated bitwise primitives so `Integer`/`BigInteger` operands use exact
    integer semantics. Small fixed-width results narrow back to `Integer` for
    `Integer` shift inputs when representable; BigInteger operands preserve
    BigInteger results.
  - `lshift 1 64` now promotes to `BigInteger` instead of returning `0`.
    Negative shift counts keep the existing `0` result. Shift counts that
    cannot narrow to `Integer`, or that exceed the bounded exact-shift cap
    (`1048576` bits), fail closed with `shift count out of range`.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-sort-bitwise-hof OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `35 passed, 0 failed`
    - bounded container rerun of the same advanced bitwise group
      -> `35 passed, 0 failed`
    - direct smokes:
      - `(String (bitwise-and (BigInteger "18446744073709551615") 255))`
        -> `"255"`
      - `(String (lshift 1 64))` -> `"18446744073709551616"`
      - `(String (rshift (BigInteger "1267650600228229401496703205376") 100))`
        -> `"1"`
      - `(lshift (BigInteger "1") -1)` -> `0`
      - `(lshift 1 (BigInteger "9223372036854775808"))`
        -> `lshift: shift count out of range`
      - `(lshift 1 1048577)` -> `lshift: shift count out of range`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed

- Fixed the signed integer lexer boundary:
  - `Lexer.scan_number(...)` now allows exactly the negative `long.min`
    magnitude while preserving overflow rejection for positive
    `9223372036854775808` and negative `-9223372036854775809`.
  - The scanner keeps a separate floating integer-part accumulator so
    `-9223372036854775808.0` does not accidentally flip sign while supporting
    the `long.min` integer token boundary.
  - Added a basic-suite regression for the raw `-9223372036854775808` literal
    and negative-underflow rejection.
  - The previous operational warning to avoid source literal
    `-9223372036854775808` is now superseded for current source; it remains
    relevant only when interpreting older checkpoints before this lexer fix.
  - validation:
    - `c3c build --obj-out obj`
    - direct smokes:
      - `-9223372036854775808` -> `-9223372036854775808`
      - `-9223372036854775809` -> `integer literal overflow`
      - `9223372036854775808` -> `integer literal overflow`
      - `-9223372036854775808.0` -> `-9.22337203685478e+18`
    - `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `144 passed, 0 failed`
    - bounded container basic slice -> `144 passed, 0 failed`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`

- Completed the next BigInteger exact-number primitive slice:
  - Extended the Boost.Multiprecision `cpp_int` helper op set with `gcd` and
    `lcm`.
  - Updated `abs`, `min`, `max`, `gcd`, and `lcm` to use the BigInteger-aware
    exact numeric path instead of the older fixed-width-only `is_number` /
    `is_int` gates.
  - `gcd`/`lcm` now accept `Integer`/`BigInteger` operands. `Integer`
    overflow boundaries use the BigInteger helper path and narrow back to
    `Integer` when the exact result fits; otherwise they return `BigInteger`.
  - Stale advanced tests that expected `long.min / -1`, `long.min % -1`,
    `gcd(long.min, ...)`, and `lcm(long.min, ...)` to fail were updated to the
    current auto-promotion contract.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `98 passed, 0 failed`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-string-predicate-format OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `61 passed, 0 failed`
    - bounded container rerun of the same two advanced numeric group filters
      -> `98 passed, 0 failed` and `61 passed, 0 failed`
    - direct smokes:
      - `(String (gcd (BigInteger "18446744073709551616") 24))` -> `"8"`
      - `(String (lcm (Integer "-9223372036854775808") 2))` -> `"9223372036854775808"`
      - `(String (abs (BigInteger "-5")))` -> `"5"`
      - `(String (max (BigInteger "9223372036854775808") 1))` -> `"9223372036854775808"`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed

- Completed the next BigInteger exact-arithmetic slice:
  - Extended the Boost.Multiprecision `cpp_int` helper op set with integer
    division and modulo.
  - Updated `/` so `Integer`/`BigInteger` combinations use exact integer
    quotient semantics when no `Double` participates, while mixed `Double`
    operations continue to narrow representable `BigInteger` values to finite
    `Double`.
  - Updated `%` to accept `Integer` and `BigInteger` operands, with deterministic
    division-by-zero failures. The `long.min % -1` boundary now returns `0`
    instead of raising an artificial overflow.
  - Updated `<`, `>`, `<=`, and `>=` to compare `BigInteger` exactly against
    `Integer`/`BigInteger`; comparisons involving `Double` require finite
    `Double` conversion and fail closed if a `BigInteger` is out of range.
  - Updated the direct JIT `<`/`>` helpers to use the same numeric comparison
    path as the interpreter.
  - validation:
    - `./scripts/build_omni_chelpers.sh`
    - `c3c build --obj-out obj`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric-float-math OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`
      -> `90 passed, 0 failed`
    - direct smokes:
      - `(String (/ (BigInteger "18446744073709551616") 2))` -> `"9223372036854775808"`
      - `(String (% (BigInteger "9223372036854775810") 3))` -> `"1"`
      - `(< (BigInteger "9223372036854775808") (BigInteger "9223372036854775809"))` -> `true`
    - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`
      -> passed
    - `git diff --check`
