# Math and Stats Scientific Module Plan - 2026-04-23

Status: Accepted direction; implementation pending.

This plan closes the naming decision from `SURFACE-NAMING-005` and opens the
implementation roadmap for moving Omni's scientific numeric surface from
top-level and slash primitives into real core modules.

## Decision

`math` and `stats` should become real core scientific modules/facades.

The final canonical surface should use module member access:

```lisp
math.sin
math.log
math.lgamma
math.erf
math.erfc
stats.normal-cdf
stats.normal-quantile
```

Current bare elementary functions (`sin`, `log`, `sqrt`, etc.) and slash
special/statistical functions (`math/erf`, `stats/normal-cdf`, etc.) are
transitional inputs to the migration. They should not be expanded as permanent
parallel APIs, and new scientific functions should not be added as deeper slash
pseudo-paths.

No unprefixed special-function names should be introduced. In particular, do
not add global `erf`, `erfc`, `lgamma`, `gamma`, `digamma`, `bessel-j`, or
similar scientific names.

## Current Surface To Migrate

### `math` module

The first `math` module/facade should export the existing shipped scientific
numeric functions:

| Current callable | Target module member | Notes |
|---|---|---|
| `abs` | `math.abs` | Core numeric magnitude; Tensor-aware where current primitive is Tensor-aware. |
| `sqrt` | `math.sqrt` | Elementary scientific function. |
| `sin` | `math.sin` | Trigonometric function. |
| `cos` | `math.cos` | Trigonometric function. |
| `tan` | `math.tan` | Trigonometric function. |
| `asin` | `math.asin` | Inverse trigonometric function. |
| `acos` | `math.acos` | Inverse trigonometric function. |
| `atan` | `math.atan` | Inverse trigonometric function. |
| `atan2` | `math.atan2` | Two-argument inverse tangent. |
| `sinh` | `math.sinh` | Hyperbolic function. |
| `cosh` | `math.cosh` | Hyperbolic function. |
| `tanh` | `math.tanh` | Hyperbolic function. |
| `exp` | `math.exp` | Exponential function. |
| `log` | `math.log` | Natural logarithm. |
| `log10` | `math.log10` | Base-10 logarithm. |
| `pow` | `math.pow` | Power function. |
| `floor` | `math.floor` | Rounding function with current BigFloat exact-integer behavior. |
| `ceiling` | `math.ceiling` | Rounding function with current BigFloat exact-integer behavior. |
| `round` | `math.round` | Rounding function with current BigFloat exact-integer behavior. |
| `truncate` | `math.truncate` | Rounding function with current BigFloat exact-integer behavior. |
| `real-part` | `math.real-part` | Complex component extraction. |
| `imag-part` | `math.imag-part` | Complex component extraction. |
| `conjugate` | `math.conjugate` | Complex conjugation. |
| `gcd` | `math.gcd` | Integer number-theory helper. |
| `lcm` | `math.lcm` | Integer number-theory helper. |
| `math/lgamma` | `math.lgamma` | Special function; do not introduce global `lgamma`. |
| `math/erf` | `math.erf` | Special function; do not introduce global `erf`. |
| `math/erfc` | `math.erfc` | Special function; do not introduce global `erfc`. |

Future scientific special functions should be added as `math.*` module members.
Initial candidates include:

- gamma family: `math.gamma`, `math.lgamma`, `math.digamma`,
  `math.trigamma`;
- beta family: `math.beta`, `math.lbeta`;
- Bessel family: `math.bessel-j`, `math.bessel-y`, `math.bessel-i`,
  `math.bessel-k`;
- Airy family: `math.airy-ai`, `math.airy-bi`;
- zeta/factorial/binomial family: `math.zeta`, `math.factorial`,
  `math.binomial`;
- special numeric helpers only when they have a clear scientific contract and
  dtype/backend policy.

Do not add these future functions as top-level primitives unless the owner
explicitly approves a prelude exception. The default is module-only.

### `stats` module

The first `stats` module/facade should export the shipped distribution
functions:

| Current callable | Target module member | Notes |
|---|---|---|
| `stats/normal-cdf` | `stats.normal-cdf` | Standard normal CDF. |
| `stats/normal-quantile` | `stats.normal-quantile` | Standard normal inverse CDF with existing probability-domain fail-closed behavior. |

Future statistical functions should be grouped as module members rather than
slash primitives:

- normal distribution: `stats.normal-pdf`, `stats.normal-logpdf`,
  `stats.normal-cdf`, `stats.normal-quantile`;
- common distributions: `stats.student-t-cdf`, `stats.student-t-quantile`,
  `stats.chi-square-cdf`, `stats.chi-square-quantile`,
  `stats.gamma-cdf`, `stats.gamma-quantile`, `stats.beta-cdf`,
  `stats.beta-quantile`;
- descriptive statistics where they are statistical operations rather than
  collection generics: `stats.mean`, `stats.variance`, `stats.stddev`,
  `stats.covariance`, `stats.correlation`, `stats.quantile`.

If a name is already a cross-cutting generic operation, do not duplicate it in
`stats` unless the module member has stricter statistical semantics. For
example, a future `stats.mean` should define statistical dtype/axis/missing-data
rules rather than being only a second spelling for a generic reducer.

## Vulkan Extension Direction

The Vulkan extension should live behind the same `math.*` and `stats.*`
user-facing module members. Do not expose backend-specific public call names
such as `vulkan.math.erf` or `stats/normal-cdf/vulkan`.

Backend selection remains data-driven through Tensor placement:

```lisp
(math.erf (to-device xs 'vulkan))
(stats.normal-quantile (to-device ps 'vulkan))
```

Required extension rules:

- preserve explicit Tensor placement and dtype;
- fail closed for unsupported Vulkan dtype/layout/function combinations;
- never fall back to CPU for Vulkan-placed operands;
- report granular capability bits so `tensor-backends` can distinguish partial
  math and stats coverage from broad scientific coverage;
- keep status-bearing kernels for domain-sensitive functions such as
  quantiles;
- document approximation/tolerance contracts before advertising support.

Initial Vulkan work should be split by numerical risk:

1. Re-export existing Vulkan-supported elementary map functions through the
   `math` module facade without changing their backend behavior.
2. Re-export existing Vulkan normal distribution support through the `stats`
   module facade, preserving current `stats-normal-float64` and
   `stats-normal-float32` capability semantics.
3. Landed 2026-04-23: `math.erf` and `math.erfc` Vulkan support for dense
   row-major `Float32` tensors, with shader validation and CPU oracle
   tolerance tests.
4. Add `math.erf` and `math.erfc` Vulkan `Float64` only after a double
   approximation policy is documented and validated.
5. Treat `math.lgamma` Vulkan support as a separate hardening item. It needs a
   stronger approximation/domain policy than `erf`/`erfc`; do not bundle it
   into the first Vulkan special-function slice unless validation evidence is
   already available.

Suggested capability keys for the migration:

- `math-elementary-float64`
- `math-elementary-float32`
- `math-error-function-float64`
- `math-error-function-float32`
- `stats-distribution-float64`
- `stats-distribution-float32`

The error-function keys intentionally cover `math.erf` and `math.erfc`; they do
not imply `math.lgamma` coverage. Existing broad `scientific-map-*` and
`stats-normal-*` keys should not be
silently removed. During migration, either derive them from the new granular
keys or document them as compatibility capability fields until consumers have
moved.

## Migration Plan

### Phase 1: module facade - closed 2026-04-23

- Core `math` and `stats` module/facade bindings are prebound during
  interpreter primitive initialization.
- Every current callable listed above is exported through the module facade.
- Module member calls share existing primitive values and backend behavior with
  the current primitive spellings.
- Runtime parity tests cover direct module access, import rebinding, selective
  import, Tensor unary behavior, map-callable behavior, and the regression that
  importing `math` must not replace global math fallbacks.
- Module docs now state that `math` and `stats` are always-available core
  scientific modules.

Implementation note:

- The first file-backed facade attempt was rejected during validation because
  `(define abs abs)` inside a module could observe the parent global method
  table and mutate its fallback. The shipped implementation avoids file-backed
  self-alias modules and fixes `define` so method-table fallback replacement
  only considers the current frame.

### Phase 2: docs and tests - closed 2026-04-23

- Active scientific Tensor docs, examples, and runtime tests now prefer
  `math.*` and `stats.*`.
- CUDA/Vulkan scientific map docs now talk about module members while
  preserving the same backend contracts.
- Runtime diagnostic strings for the scientific special/stat functions now use
  the dotted module surface.
- Negative tests for removed slash names landed in Phase 3.

### Phase 3: remove transitional callables - closed 2026-04-23

- Removed `math/lgamma`, `math/erf`, `math/erfc`,
  `stats/normal-cdf`, and `stats/normal-quantile` as public primitive
  spellings.
- `math` / `stats` module exports now create their own dotted-name primitive
  values for the special/stat functions, so backend callable recognition can
  use the canonical module surface without retaining public slash bindings.
- Added negative runtime coverage for removed old slash spellings.
- Bare elementary functions remain as prelude/core primitive exports for now.
  Removing them is a separate language-prelude decision, not part of the
  special/stat slash cleanup.

### Phase 4: Vulkan extension - active

Closed 2026-04-23:

- Existing Vulkan-supported elementary math and normal distribution callables
  are exercised through the `math.*` / `stats.*` module surface.
- Dense row-major Vulkan `Float32` `math.erf` and `math.erfc` now route
  through the shared unary Float32 SPIR-V helper as op ids `17` and `18` for
  direct Tensor unary calls, public `map`, and checked `kernel/run` unary
  `erf-f32` / `erfc-f32` operations.
- Vulkan `Float64` `math.erf` and `math.erfc` remain fail-closed pending an
  explicit double approximation policy.
- `tensor-backends` now reports granular scientific capability keys:
  `math-elementary-float64`, `math-elementary-float32`,
  `math-error-function-float64`, `math-error-function-float32`,
  `stats-distribution-float64`, and `stats-distribution-float32`.
- The broad `scientific-map-*` keys and legacy `stats-normal-*` keys remain
  compatibility fields; they no longer need to carry the full partial-support
  meaning by themselves.

Remaining:

- `MATHSTATS-VK-003` records the remaining numerical policy boundary: decide
  whether and how to support Vulkan `Float64` `math.erf` / `math.erfc` and
  Vulkan `math.lgamma`, with approximation, domain, tolerance, and
  status-diagnostic contracts documented before implementation.

## Validation Path

For the planning slice:

- `git diff --check`
- `scripts/check_status_consistency.sh`

For module facade implementation:

- `c3c build --obj-out obj`
- focused advanced collections-module scientific Tensor tests
- primitive/module docs parity checks
- status consistency and file-size gate

Module facade validation completed 2026-04-23:

- `c3c build --obj-out obj`
- direct REPL/import smoke for `(import math)`, `(abs 0.0)`, and
  `(math.erf 1.0)`
- direct eval smoke for prebound `math` / `stats`
- `/tmp` script smoke proving `(import math)` and `(import stats)` no longer
  depend on repo-relative `lib/*.omni` files
- focused advanced collections-module slice:
  `pass=2053 fail=0`
- focused compiler slice:
  `pass=301 fail=0`
- `scripts/check_e2e_baseline_policy.sh`
- `scripts/check_status_consistency.sh`
- `scripts/check_file_size_gate.sh`
- `scripts/check_primitive_docs_parity.sh`
- `git diff --check`

Docs/tests migration validation completed 2026-04-23:

- `c3c build --obj-out obj`
- focused advanced collections-module slice:
  `pass=2053 fail=0`
- focused advanced stdlib numeric slice:
  `pass=411 fail=0`
- focused compiler slice:
  `pass=301 fail=0`
- `scripts/check_e2e_baseline_policy.sh`
- `scripts/check_file_size_gate.sh`
- `scripts/check_primitive_docs_parity.sh`

Transitional callable removal validation completed 2026-04-23:

- `c3c build --obj-out obj`
- direct eval smoke for `math.erf`, `stats.normal-cdf`, `map math.erf`, and
  removed old slash spellings
- focused advanced collections-module slice:
  `pass=2055 fail=0`
- focused advanced stdlib numeric slice:
  `pass=411 fail=0`
- focused compiler slice:
  `pass=301 fail=0`

For Vulkan extension slices:

- shader compile and `spirv-val` for changed GLSL/SPIR-V
- `scripts/build_omni_chelpers.sh`
- direct CPU/Vulkan oracle smokes for each function/dtype
- focused advanced collections-module tests
- bounded-container validation when the slice is broad or high-memory
- no-hidden-CPU-fallback negative tests for Vulkan operands

Vulkan `Float32` `math.erf` / `math.erfc` validation completed 2026-04-23:

- `glslangValidator -V csrc/tensor_vulkan_map_unary_f32.comp`
- `spirv-val /tmp/tensor_vulkan_map_unary_f32.spv`
- `scripts/build_omni_chelpers.sh`
- `c3c build --obj-out obj`
- direct Vulkan smokes for `map math.erf`, direct `math.erfc`, and fail-closed
  Vulkan `Float64` `math.erf`
- focused advanced collections-module slice:
  `pass=2060 fail=0`

Granular scientific capability reporting validation completed 2026-04-23:

- `c3c build --obj-out obj`
- focused advanced collections-module slice:
  `pass=2062 fail=0`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_file_size_gate.sh`
- `scripts/check_status_consistency.sh`
- `scripts/check_e2e_baseline_policy.sh`
- `git diff --check`

## Negative-Memory Constraints

- Do not add global special-function names such as `erf`, `erfc`, `lgamma`,
  `gamma`, or `digamma`.
- Do not create deeper slash pseudo-paths such as `math/special/erf` or
  `stats/distribution/normal-cdf`.
- Do not expose backend-specific public call names for Vulkan scientific math.
  Backend support is selected by Tensor placement and reported by capability
  dictionaries.
- Do not treat broad `scientific-map-*` capability as proof that every
  `math.*` or `stats.*` function is supported on a backend.
- Do not ship domain-sensitive Vulkan functions without status propagation,
  deterministic diagnostics, and documented approximation tolerances.
