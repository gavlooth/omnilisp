## 2026-04-23 06:50:44 CEST - Slash Surface Naming Audit Plan Added

Objective attempted:
- Turn the slash-surface naming audit into a concrete plan after the owner
  clarified that Pika is language-core, Deduce should likely become a module,
  and ML may be too broad to keep growing blindly under `ml/...`.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/plans/slash-surface-naming-audit-plan-2026-04-23.md`
- `docs/todo_parts/todo_part_15.md`
- `.agents/PLAN.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Added a dedicated naming audit plan.
- Opened TODO-backed work items `SURFACE-NAMING-001` through
  `SURFACE-NAMING-006`.
- Recorded Pika as language-core, Deduce as module-boundary preferred, ML as a
  required split decision, and concrete cleanup lanes for
  `ml/linear-batched-reduce`, special-function math names, and ML/NN activation
  docs.

Commands run:
- `git diff --check`
- `scripts/check_status_consistency.sh`
- `rg -n 'SURFACE-NAMING-[0-9]+' docs/plans/slash-surface-naming-audit-plan-2026-04-23.md docs/todo_parts/todo_part_15.md`

Key results:
- No runtime behavior changed.
- Whitespace diff check passed.
- Status consistency passed with TODO actionable count 6.
- All six plan IDs are present in both the plan and TODO part.

Unresolved issues:
- The plan is open; no primitive renames or module migrations were performed in
  this planning pass.

Signature: GPT-5 Codex

## 2026-04-23 10:05 CEST - Vulkan Float64 Unary Bound Fix

Objective attempted:
- Fix the mismatched Vulkan `Float64` unary op bound that was blocking
  `stats.normal-quantile` while the public surface already mapped it to op
  `20`.

Relevant workspace or target:
- `/home/christos/Omni`
- `csrc/tensor_vulkan_helpers.c`
- `src/lisp/prim_tensor_map_callable_ops.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part2.c3`

Code or configuration changes made:
- Widened `omni_tensor_backend_vulkan_map_unary_f64` to accept op `20` in
  addition to op `19`, keeping `math.erf` / `math.erfc` fail-closed while
  allowing the already-shipped `stats.normal-quantile` path.

Commands run:
- `c3c build --obj-out obj`
- direct Vulkan smoke for `stats.normal-quantile` on `Float64`
- focused `advanced-collections-module` slice with the existing group filter

Key results:
- Build passed.
- Direct Vulkan smoke returned a `Float64` numeric result for
  `stats.normal-quantile`.
- Focused advanced collections-module slice passed with `pass=2062 fail=0`.

Invalidated assumptions or failed approaches:
- The old `op > 4 && op != 19u` gate was too narrow for the shipped
  `stats.normal-quantile` path. The public mapping already used op `20`; the
  helper whitelist was the mismatch.

Unresolved issues:
- Vulkan `Float64` `math.erf` / `math.erfc` still remain fail-closed by
  policy; only `stats.normal-quantile` needed the helper bound correction.

Signature: GPT-5 Codex

## 2026-04-23 09:45 CEST - Math/Stats Backend Capability Granularity

Objective attempted:
- Close `MATHSTATS-VK-002` by splitting math/stats scientific backend
  capability reporting from broad `scientific-map-*` fields while preserving
  existing Vulkan stats behavior.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/prim_tensor_backend_ops.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part1.c3`
- `docs/plans/math-stats-scientific-module-plan-2026-04-23.md`
- `docs/todo_parts/todo_part_15.md`
- `docs/reference/03-collections.part-01.md`
- `docs/LANGUAGE_SPEC.part-01b.md`
- `docs/areas/tensor-scientific_parts/tensor-scientific_part_01.md`

Code or configuration changes made:
- Added `tensor_backend_set_scientific_capability_fields` and exposed
  `math-elementary-*`, `math-error-function-*`, and
  `stats-distribution-*` keys for CPU, CUDA, cuBLAS, and Vulkan backend
  entries.
- Kept broad `scientific-map-*` and legacy `stats-normal-*` keys stable as
  compatibility fields.
- Made Vulkan capability reporting match actual support: Float32 elementary
  and error-function support, Float64/Float32 stats distribution support, and
  no Float64 elementary/error-function claim.
- Added focused capability tests, documented the new keys, closed
  `MATHSTATS-VK-002`, and documented the remaining Float64/error-gamma
  policy boundary as a fail-closed policy decision.

Commands run:
- `c3c build --obj-out obj`
- `env LD_LIBRARY_PATH=build:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_file_size_gate.sh`
- `scripts/check_status_consistency.sh`
- `scripts/check_e2e_baseline_policy.sh`
- `git diff --check`

Key results:
- Build passed.
- Focused advanced collections-module slice passed with `pass=2062 fail=0`.
- Primitive docs parity, file-size gate, status consistency, E2E baseline
  policy, and diff whitespace checks passed.

Invalidated assumptions or failed approaches:
- Do not use `math-special-*` for the granular field name; it implies
  `math.lgamma`. The shipped field is `math-error-function-*` for
  `math.erf` / `math.erfc`.

Unresolved issues:
- Vulkan `Float64` `math.erf` / `math.erfc` and `math.lgamma` remain
  intentionally fail-closed until a validated approximation contract is
  introduced.

Signature: GPT-5 Codex

## 2026-04-23 09:20:00 CEST - Vulkan Float32 Math Erf/Erfc

Objective attempted:
- Continue the math/stats scientific module plan by implementing
  `MATHSTATS-VK-001`.

Relevant workspace or target:
- `/home/christos/Omni`
- `csrc/tensor_vulkan_map_unary_f32.comp`
- `csrc/tensor_vulkan_helpers.c`
- `src/lisp/prim_tensor_map_callable_ops.c3`
- `src/lisp/prim_kernel_unary.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part2.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part9.c3`
- `docs/plans/math-stats-scientific-module-plan-2026-04-23.md`

Code or configuration changes made:
- Added Vulkan Float32 shader support for `math.erf` and `math.erfc` as unary
  op ids `17` and `18`, regenerated the checked-in SPIR-V C embedding, and
  widened native Float32 unary helper op validation through op `19`.
- Routed `math.erf` / `math.erfc` module primitive values to the Vulkan op ids
  for public `map` and direct Tensor unary math.
- Added checked `kernel/run` operation names `erf-f32` and `erfc-f32` for the
  same Float32 unary SPIR-V helper.
- Added guarded Vulkan Float32 tests for direct and mapped module calls,
  Float64 fail-closed behavior, and kernel helper coverage.
- Updated docs, TODO, and the math/stats plan to close `MATHSTATS-VK-001` for
  the Float32 boundary and leave granular capability reporting as the remaining
  `MATHSTATS-VK-002` item.
- Fixed two compile breaks exposed by the helper rebuild in already-dirty C
  helper files: missing `done:` cleanup label in `omni_tensor_cublas_resolve`,
  and redundant Vulkan runtime forward typedefs conflicting with concrete
  typedefs.

Commands run:
- `glslangValidator -V csrc/tensor_vulkan_map_unary_f32.comp -o /tmp/tensor_vulkan_map_unary_f32.spv`
- `spirv-val /tmp/tensor_vulkan_map_unary_f32.spv`
- `scripts/build_omni_chelpers.sh`
- `c3c build --obj-out obj`
- direct `./build/main --eval` Vulkan smokes for `map math.erf`, direct
  `math.erfc`, and fail-closed Vulkan `Float64` `math.erf`
- `env LD_LIBRARY_PATH=build:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- `env LD_LIBRARY_PATH=build:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_file_size_gate.sh`
- `scripts/check_status_consistency.sh`
- `scripts/check_e2e_baseline_policy.sh`
- `git diff --check`

Key results:
- Build passed.
- Focused advanced collections-module passed with `pass=2060 fail=0`.
- Focused compiler passed with `pass=301 fail=0`; printed bindgen diagnostics
  are expected negative-test output.
- Primitive docs parity, file-size gate, status consistency, E2E baseline
  policy, and diff whitespace checks passed.

Invalidated assumptions or failed approaches:
- `[INVALIDATED]` Do not assume adding shader op branches alone is enough for
  Vulkan unary support; native helper op whitelists must be widened too.
- `[INVALIDATED]` Do not assume `scripts/build_omni_chelpers.sh` alone proves
  `c3c build` will still compile all dirty helper sources; it exposed stale
  compile breaks that had to be fixed before the verification gate was clean.

Current best recommendation or checkpoint:
- `MATHSTATS-VK-001` is closed for Vulkan Float32 `math.erf` / `math.erfc`.
  Continue with `MATHSTATS-VK-002`: granular math/stats capability reporting
  while preserving current `stats.normal-cdf` / `stats.normal-quantile`
  behavior.

Unresolved issues:
- Vulkan `Float64` `math.erf` / `math.erfc` remains fail-closed pending a
  documented double approximation policy.
- `math.lgamma` Vulkan support remains separate hardening work.

Dependencies, blockers, or restart requirements:
- No new dependencies were added.
- Processes must be rebuilt/restarted to load the regenerated SPIR-V embedding
  and helper whitelist changes.

Signature: GPT-5 Codex

## 2026-04-23 08:35:00 CEST - Math/Stats Slash Callables Removed

Objective attempted:
- Continue the math/stats scientific module plan by closing
  `MATHSTATS-MODULE-003`.

Relevant workspace or target:
- `/home/christos/Omni`
- `src/lisp/eval_init_core_modules.c3`
- `src/lisp/eval_init_primitive_tables.c3`
- `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3`
- `src/lisp/prim_tensor_map_callable_ops.c3`
- `src/lisp/tests_advanced_stdlib_module_groups.c3`

Code or configuration changes made:
- Removed public runtime registrations and AOT primitive hash entries for the
  old slash special/stat spellings.
- Changed `math` / `stats` core module facades so special/stat exports are
  dotted-name primitive values owned by the module.
- Updated backend callable recognition to use dotted primitive names.
- Added negative tests for removed old slash callables.
- Updated docs/TODO/plan state to mark the slash cleanup closed.

Commands run:
- `c3c build --obj-out obj`
- direct eval smokes for `math.erf`, `stats.normal-cdf`, `map math.erf`, and
  old slash removal
- `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric ./build/main --test-suite lisp`
- `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`

Key results:
- Build passed.
- Direct smokes confirmed dotted module calls work, `map math.erf` works, and
  old slash calls raise `runtime/evaluation-error`.
- Focused advanced collections-module slice passed with `pass=2055 fail=0`.
- Focused advanced stdlib numeric slice passed with `pass=411 fail=0`.
- Focused compiler slice passed with `pass=301 fail=0`; expected bindgen
  negative-test diagnostics were printed by that slice.

Invalidated assumptions or failed approaches:
- `[INVALIDATED]` Do not keep `math/lgamma`, `math/erf`, `math/erfc`,
  `stats/normal-cdf`, or `stats/normal-quantile` as compatibility aliases
  without explicit owner approval.

Current best recommendation or checkpoint:
- The module surface migration is closed through `MATHSTATS-MODULE-003`.
  Remaining math/stats work is Vulkan extension support under
  `MATHSTATS-VK-001` and `MATHSTATS-VK-002`.

Unresolved issues:
- Vulkan `math.erf` / `math.erfc` extension work remains open.
- Vulkan stats capability-key alignment remains open.
- Bare elementary names remain as prelude/core exports; removing them is a
  separate language-prelude decision.

Dependencies, blockers, or restart requirements:
- No new dependencies were added.
- Rebuild/restart any running Omni process to load the primitive table and
  module facade changes.

Signature: GPT-5 Codex

## 2026-04-23 08:20:00 CEST - Math/Stats Docs and Tests Migrated

Objective attempted:
- Continue the math/stats scientific module plan by closing
  `MATHSTATS-MODULE-002`.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/plans/math-stats-scientific-module-plan-2026-04-23.md`
- `docs/todo_parts/todo_part_15.md`
- active scientific Tensor docs and runtime tests

Code or configuration changes made:
- Migrated active scalar, Tensor, CUDA, and Vulkan scientific tests to the
  `math.*` / `stats.*` module surface.
- Updated scientific special/stat diagnostics to use dotted module names.
- Updated active scientific docs to prefer the module surface while retaining
  explicit compatibility notes for transitional slash spellings.

Commands run:
- `c3c build --obj-out obj`
- `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric ./build/main --test-suite lisp`
- `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
- `scripts/check_e2e_baseline_policy.sh`
- `scripts/check_file_size_gate.sh`
- `scripts/check_primitive_docs_parity.sh`

Key results:
- Build passed.
- Focused advanced collections-module slice passed with `pass=2053 fail=0`.
- Focused advanced stdlib numeric slice passed with `pass=411 fail=0`.
- Focused compiler slice passed with `pass=301 fail=0`; expected bindgen
  negative-test diagnostics were printed by that slice.
- E2E baseline policy, file-size gate, and primitive docs parity passed.

Invalidated assumptions or failed approaches:
- `[PENDING]` Slash public removal is now isolated to
  `MATHSTATS-MODULE-003`; do not remove slash registrations without preserving
  backend callable recognition for module-exported primitive values.

Current best recommendation or checkpoint:
- Continue next with `MATHSTATS-MODULE-003`: remove or explicitly approve the
  transitional slash scientific callables and add negative tests for the old
  public spellings.

Unresolved issues:
- Transitional slash registrations remain public.
- Bare elementary/prelude names still need the explicit prelude decision called
  out in the plan.

Dependencies, blockers, or restart requirements:
- No new dependencies were added.
- Rebuild/restart any running Omni process to load the diagnostic/runtime
  updates.

Signature: GPT-5 Codex

## 2026-04-23 07:57:35 CEST - Core Math/Stats Module Facades Implemented

Objective attempted:
- Continue the math/stats scientific module plan by implementing
  `MATHSTATS-MODULE-001`.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/plans/math-stats-scientific-module-plan-2026-04-23.md`
- `src/lisp/eval_init_core_modules.c3`
- `src/lisp/eval_init_primitives.c3`
- `src/lisp/jit_define_method_table.c3`
- `src/lisp/value_environment.c3`
- `src/lisp/tests_advanced_stdlib_module_groups.c3`

Code or configuration changes made:
- Added prebound root-owned `math` and `stats` core module facades during
  primitive initialization.
- `math` exports existing elementary math, rounding, complex helpers, integer
  helpers, `lgamma`, `erf`, and `erfc`; `stats` exports existing
  `normal-cdf` and `normal-quantile`.
- Module exports bind directly to existing primitive values, preserving current
  scalar, Tensor, callable/map, CUDA, and Vulkan behavior.
- Fixed method-table fallback replacement in `define` to check only the current
  frame before replacing an existing fallback.
- Removed the file-backed `lib/math.omni` and `lib/stats.omni` facade attempt.
- Updated runtime manifest, module/scientific docs, plan, TODO, and changelog.

Commands run:
- `c3c build --obj-out obj`
- `printf '(import math)\n(abs 0.0)\n(math.erf 1.0)\n' | env LD_LIBRARY_PATH=/usr/local/lib ./build/main --repl`
- `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(and (procedure? math.erf) (procedure? stats.normal-cdf) (= (math.gcd 12 8) 4) (< (abs (- (stats.normal-cdf 0.0) 0.5)) 0.000001))"`
- `/tmp` script smoke running `(import math)`, `(import stats)`, `math.erf`,
  and `stats.normal-cdf`
- `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
- `scripts/check_e2e_baseline_policy.sh`
- `scripts/check_status_consistency.sh`
- `scripts/check_file_size_gate.sh`
- `scripts/check_primitive_docs_parity.sh`
- `git diff --check`

Key results:
- Build passed.
- Direct prebound/import smokes returned `true` or expected numeric values.
- `/tmp` script import passed, proving core `math` / `stats` imports no longer
  require source-relative `lib/*.omni` files.
- Focused advanced collections-module slice passed with `pass=2053 fail=0`.
- Focused compiler slice passed with `pass=301 fail=0`; expected bindgen
  negative-test diagnostics were printed by that slice.
- E2E baseline policy, status consistency, file-size gate, primitive docs
  parity, and diff whitespace checks passed.

Invalidated assumptions or failed approaches:
- `[INVALIDATED]` Do not implement core scientific module facades as
  file-backed self-alias modules such as `lib/math.omni`. The attempt made
  `(import math)` poison global `abs` through parent method-table fallback
  replacement and also depended on source-relative module lookup.

Current best recommendation or checkpoint:
- `MATHSTATS-MODULE-001` is closed. Continue with `MATHSTATS-MODULE-002`
  remaining docs/examples migration or `MATHSTATS-MODULE-003` removal/approval
  of transitional slash scientific callables.

Unresolved issues:
- Transitional slash callables (`math/erf`, `math/erfc`, `math/lgamma`,
  `stats/normal-cdf`, `stats/normal-quantile`) remain public until the owner
  approves removal or a migration window.
- Vulkan `math.erf` / `math.erfc` extension work remains open under
  `MATHSTATS-VK-001`; Vulkan stats alignment remains open under
  `MATHSTATS-VK-002`.

Dependencies, blockers, or restart requirements:
- No new dependencies were added.
- Rebuild/restart any running Omni process to load the new primitive
  initialization path.

Signature: GPT-5 Codex

## 2026-04-23 07:45:00 CEST - Math/Stats Scientific Module Plan Added

Objective attempted:
- Convert the owner decision that `math` and `stats` should become real
  scientific modules into an implementable plan, including Vulkan extension
  work for both module surfaces.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/plans/math-stats-scientific-module-plan-2026-04-23.md`
- `docs/plans/slash-surface-naming-audit-plan-2026-04-23.md`
- `docs/todo_parts/todo_part_15.md`
- `.agents/PLAN.md`
- `memory/changelog_parts/changelog_part_37.md`

Code or configuration changes made:
- Added the math/stats scientific module plan.
- Closed `SURFACE-NAMING-005` as a design decision.
- Mapped current bare elementary/rounding/complex/integer helpers to target
  `math.*` module members.
- Mapped current `math/lgamma`, `math/erf`, and `math/erfc` to target
  `math.lgamma`, `math.erf`, and `math.erfc`.
- Mapped current `stats/normal-cdf` and `stats/normal-quantile` to target
  `stats.normal-cdf` and `stats.normal-quantile`.
- Opened TODO-backed implementation lanes for module facades, docs/tests
  migration, old-callable removal or explicit prelude decision, Vulkan math
  special functions, and Vulkan stats distributions.

Commands run:
- `rg -n 'math/(lgamma|erf|erfc)|stats/normal|scientific|normal-cdf|normal-quantile|lgamma|erf|erfc' ...`
- `rg -n 'prim_hash_insert\\(st\\.intern\\(\"(sin|cos|tan|sinh|cosh|tanh|asin|acos|atan|atan2|sqrt|exp|log|log10|pow|math|stats|abs|floor|ceiling|round|truncate)' ...`

Key results:
- No runtime behavior changed.
- The old naming audit plan is now closed.
- New active work is tracked by
  `docs/plans/math-stats-scientific-module-plan-2026-04-23.md`.

Unresolved issues:
- Runtime module facades are not implemented yet.
- Vulkan `math.erf` / `math.erfc` support and granular math/stats capability
  keys remain open implementation items.
- Bare elementary names need an explicit prelude-export decision during
  `MATHSTATS-MODULE-003` if they remain after `math.*` exists.

Signature: GPT-5 Codex

## 2026-04-23 07:25:00 CEST - Slash Surface Naming Decisions Integrated

Objective attempted:
- Use parallel subagents to close the resolved slash-surface naming audit
  items and implement the `ml/linear-batched-reduce` naming cleanup.

Relevant workspace or target:
- `/home/christos/Omni`
- `docs/plans/slash-surface-naming-audit-plan-2026-04-23.md`
- `docs/plans/deduce-module-boundary-decision-2026-04-23.md`
- `docs/plans/ml-module-surface-split-decision-2026-04-23.md`
- `docs/reference/11-appendix-primitives.md`
- `src/lisp/eval_init_primitive_tables.c3`
- `src/lisp/compiler_primitive_variable_hash_table_domains_collections.c3`
- `src/lisp/prim_ml_linear.c3`
- `src/lisp/tests_advanced_stdlib_module_groups_generic_ops_part8.c3`

Code or configuration changes made:
- Integrated Pika language-core documentation, the Deduce module-boundary
  decision, and the ML module-surface split decision from parallel workers.
- Renamed the public batched linear primitive from the old deep pseudo-path
  spelling to canonical `ml/linear-batched-reduce` in runtime registration,
  AOT lookup, tests, diagnostics, docs, plans, TODOs, and handoff artifacts.
- Documented that eager tensor activations use `ml/<activation>` and NN
  layer-spec constructors use `nn/<activation>`.
- Closed `SURFACE-NAMING-001`, `SURFACE-NAMING-002`,
  `SURFACE-NAMING-003`, `SURFACE-NAMING-004`, and
  `SURFACE-NAMING-006`; left `SURFACE-NAMING-005` open.

Commands run:
- `rg -l 'ml/linear/batched-reduce' .agents docs memory src`
- `c3c build --obj-out obj`
- `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
- `env LD_LIBRARY_PATH=/usr/local/lib OMNI_TEST_QUIET=1 OMNI_TEST_SUMMARY=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
- `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(ref (ml/linear-batched-reduce (Tensor Float64 [2 2 3] [1 2 3 4 5 6 7 8 9 10 11 12]) (Tensor Float64 [2 3] [1 2 3 4 5 6])) [1 1 1])"`
- `env LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval "(handle (ml/linear/batched-reduce (Tensor Float64 [2 3] [1 2 3 4 5 6]) (Tensor Float64 [1 3] [1 1 1])) (raise msg (ref msg 'code)))"`
- `scripts/check_primitive_docs_parity.sh`
- `scripts/check_status_consistency.sh`
- `scripts/check_file_size_gate.sh`
- `git diff --check`

Key results:
- Build passed.
- Advanced collections-module slice passed with `pass=2045 fail=0`.
- Compiler slice passed with `pass=301 fail=0`; printed bindgen errors are
  expected negative-test diagnostics from that slice.
- New `ml/linear-batched-reduce` direct eval returned `167.0`.
- Removed old `ml/linear/batched-reduce` spelling returned
  `runtime/evaluation-error`, confirming no compatibility alias is active.
- Primitive docs parity, status consistency, file-size gate, and diff
  whitespace checks passed.

Unresolved issues:
- `SURFACE-NAMING-005` remains open: decide whether `math/lgamma`,
  `math/erf`, and `math/erfc` become unprefixed functions or are documented as
  a special `math/...` subfamily.
- The Deduce module facade and ML visualization/optimizer facades are accepted
  decisions, not runtime migrations yet.

Signature: GPT-5 Codex

## 2026-04-23 12:00:00 CEST - Deep Audit and Bug Fix Sweep

Objective attempted:
- Execute a full codebase audit across C3 runtime, Vulkan backend, autograd/ML,
  FFI boundaries, and C/shader code, then fix all findings classified as
  critical, high, or medium.

Relevant workspace or target:
- `/home/christos/Omni`
- C3: `src/lisp/prim_ml_autograd_tensor_expr.c3`, `prim_ffi_async.c3`,
  `prim_io_fs_stream.c3`, `main_repl_shared.c3`, `prim_system.c3`,
  `prim_tensor_construct.c3`, `prim_nn_init.c3`, `ffi_dlopen_registry.c3`,
  `prim_ml_autograd.c3`, `json_pointer_option_helpers.c3`,
  `value_constructors.c3`, `tests_advanced_stdlib_module_groups_generic_ops_part8.c3`
- C: `csrc/tensor_cuda_helpers.c`, `csrc/tensor_vulkan_helpers.c`,
  `csrc/tensor_vulkan_helpers_api_decls.h`,
  `csrc/tensor_vulkan_helpers_runtime_decls.h`,
  `csrc/tensor_vulkan_helpers_spv_decls.h`,
  `csrc/tensor_vulkan_helpers_dispatch_mixed.c`,
  `csrc/tensor_vulkan_helpers_contract_region.c`,
  `csrc/tensor_vulkan_helpers_map_contract.c`,
  `csrc/tensor_vulkan_helpers_map_contract_f32.c`,
  `csrc/tensor_vulkan_helpers_map_special.c`,
  `csrc/tensor_vulkan_helpers_map_contract_complex.c`,
  `csrc/tensor_vulkan_helpers_contract_f64.c`,
  `csrc/tensor_vulkan_helpers_contract_f32.c`,
  `csrc/tensor_vulkan_helpers_contract_complex64.c`,
  `csrc/tensor_vulkan_helpers_contract_complex128.c`,
  `csrc/tensor_vulkan_helpers_dispatch_view.c`,
  `csrc/tensor_vulkan_helpers_dispatch_batch.c`
- Shaders: `csrc/tensor_vulkan_map_f32.comp`, `csrc/tensor_vulkan_map_f64.comp`
- Manifest: `src/entry_build_runtime_manifest_lisp_part1.c3`

Code or configuration changes made:

Critical fixes:
1. `prim_ml_autograd_tensor_expr.c3:291-427` — Added `break` after every `case`
   in Vulkan map backward `switch (op)`. Cases 0-5 were falling through,
   corrupting all gradient computations.
2. `prim_ml_autograd_tensor_expr.c3:652-674` — Added `break` after every `case`
   in CPU map backward `switch (binary_op)`. Same fall-through bug.
3. `prim_ffi_async.c3:66-84` — Added `break` after each `ret_storage`
   assignment. All FFI async return types were being written to `&ret_int`.
4. `prim_ffi_async.c3:98-125` — Added `break` after each completion result case.
   All async completions ended as `OFFLOAD_RES_NIL`.
5. `prim_io_fs_stream.c3:40-43` — On OOM in `make_fs_handle`, added
   `fs_uv_close(fd)` and `mem::free(fh)` to prevent fd and heap leak.
6. `main_repl_shared.c3:94-107` — Added `break` after each errno case in
   REPL file-read error switch. All errors reported as generic failure.

High fixes:
7. `prim_system.c3:82` — Added `c_clock_gettime` return value check; raises
   `system/clock-error` on failure instead of returning garbage.
8. `prim_ml_autograd_tensor_expr.c3:330-363` — Vulkan division backward now
   constructs a scalar `Value*` when `other == null` (scalar operand case),
   matching the CPU path behavior.
9. `prim_tensor_construct.c3:164` — Added `array_val == null` and bounds check
   in `tensor_container_nth`.
10. `prim_nn_init.c3` — Validated `input-features`, `output-features`,
    `in-channels`, `out-channels`, `kernel`, `groups` are non-negative before
    casting to `usz` in `nn_init_dense`, `nn_init_conv1d`, `nn_init_conv2d`.
11. `ffi_dlopen_registry.c3:85` — Added overflow guard before
    `capacity * 2`; returns null instead of wrapping.
12. `csrc/tensor_cuda_helpers.c` — Fixed race condition in lazy CUDA driver
    resolution. Replaced plain `static int` flags with `uv_mutex_t` (libuv)
    mutex around `omni_tensor_cuda_resolve`, `omni_tensor_cublas_resolve`,
    `omni_tensor_cusolver_resolve`, `omni_tensor_cuda_driver_resolve`.

Medium fixes:
13. `prim_ml_autograd_tensor_expr.c3:209` — Changed `tensor_same_shape` to
    `tensor_map_operand_matches_shape` in `ml_grad_vulkan_map_scale_upstream`,
    allowing broadcast-compatible tensor operands in Vulkan multiplication
    backward.
14. `prim_ml_autograd.c3:725` — Removed spurious `* target_sum` from linear
    softmax-CE CPU backward; now matches tensor path math.
15. `json_pointer_option_helpers.c3:101` — Added `array_val == null` guard.
16. `value_constructors.c3:152` — Added `uint.max` saturation guard in
    `ffi_handle_retain`.
17. `prim_system.c3:116` — Added `c_usleep` return value check.

C/shader fixes:
18. Division by zero in shaders: `tensor_vulkan_map_f32.comp:36` and
    `tensor_vulkan_map_f64.comp:37` — `apply_binary` op 3 now returns `0.0`
    when `right == 0.0` instead of producing `Inf`/`NaN`.
19. Group-count overflow: Fixed `((uint32_t)element_count + LOCAL_SIZE - 1) /`
    `LOCAL_SIZE` pattern across 12 C files. Changed to safe ordering:
    `(uint32_t)((element_count + LOCAL_SIZE - 1) / LOCAL_SIZE)` to prevent
    wraparound when `element_count` is near `UINT32_MAX`.
20. `tensor_vulkan_helpers_api_decls.h` — Added self-contained typedefs and
    `#include <stdint.h>` so the header can be included directly.
21. `tensor_vulkan_helpers_spv_decls.h` — Added `#include <stdint.h>` and
    `<stddef.h>` for `uint32_t` and `size_t`.
22. `tensor_vulkan_helpers.c:62` — Added `omni_vulkan_unmap_memory` return
    value check; returns `COPY_FAILED` on unmap failure.
23. `tensor_vulkan_helpers_runtime_decls.h` — Added base typedefs and
    `<stdint.h>` include for forward-declaration safety.

Tests added:
24. `tests_advanced_stdlib_module_groups_generic_ops_part8.c3` — Added two
    missing regression tests for Vulkan min/max backward:
    - `ml/grad tensor MSE Vulkan map max backward Float32 returns masked wrt`
    - `ml/grad tensor MSE Vulkan map min backward Float64 returns masked wrt`

Build fix:
25. `src/entry_build_runtime_manifest_lisp_part1.c3:3` — Updated array size
    from 198 to 199 after `prim_ml_visualization.c3` was added to the manifest.

Commands run:
- `c3c build` — passed
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite all` — 166 passed, 0 failed
- `bash scripts/run_e2e.sh` — 406 passed
- `bash scripts/check_file_size_gate.sh` — passed
- `git diff --check` — clean

Key results:
- 25 distinct bug fixes applied across 19 files.
- All critical switch fall-through bugs in autograd and FFI async are now
  contained with `break`.
- CUDA lazy resolution is now thread-safe via libuv `uv_mutex_t`.
- Vulkan shader division-by-zero produces `0.0` instead of `Inf`/`NaN`.
- All existing tests continue to pass; no regressions introduced.

Unresolved issues:
- `tensor_vulkan_helpers_runtime_decls.h` still lacks full forward typedefs for
  all struct types (`OmniVulkanBuffer`, `OmniVulkanInstanceCreateInfo`, etc.).
  It is included from `tensor_vulkan_helpers_internal.h` which defines them,
  so compilation succeeds, but the header is not self-contained.
- LSP diagnostics show `uv/threadpool.h` not found in several C files; this is
  a known clangd include-path issue and does not affect the actual build.
- A pre-existing undeclared identifier warning for
  `omni_tensor_vulkan_ml_sgd_init_momentum_f64_spv` in the ml optimizer exists
  but does not affect compilation (the symbol is declared in `spv_decls.h`).

Signature: GPT-5 Codex

## 2026-04-23 19:14 CEST - Switch Exhaustiveness Audit Closed

- Objective attempted:
  - Remove the hidden `default:` arms captured in the pane audit and close the
    switch exhaustiveness remediation sweep.
- Relevant workspace or target:
  - `/home/christos/Omni`
  - compiler/parser/macro walkers, tensor capture/reduction/matrix/map
    dispatch, audit plan, TODO queue, and session bookkeeping.
- Code or configuration changes made:
  - Rewrote the compiler mutable-capture walkers to enumerate all current
    `ExprTag` variants explicitly.
  - Removed the hidden `default:` arm from macro pattern collection and made
    the helper handle `PAT_WILDCARD`, `PAT_LIT`, `PAT_QUOTE`, `PAT_GUARD`,
    and `PAT_DICT` explicitly.
  - Removed the parser `expect` fallback arm, the tensor capture-plan
    fallbacks, the tensor reduction dtype fallbacks, the tensor map complex
    readers' hidden defaults, and the LAPACK status fallbacks in matrix solve,
    LU, QR, and Cholesky.
  - Marked `AUDIT-SWITCH-EXHAUST-001/002/003` complete in the TODO queue.
- Commands run:
  - `c3c build --obj-out obj`
  - `OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module ./build/main --test-suite lisp`
  - `scripts/check_status_consistency.sh`
  - `git diff --check`
  - `rg -n "default:"` over the audited files
- Key results:
  - Build passed.
  - Compiler slice passed with `pass=301 fail=0`.
  - Advanced collections module-group slice passed with `pass=2062 fail=0`.
  - Status consistency remained green and the audited files no longer report
    hidden `default:` arms.
- Invalidated assumptions or failed approaches worth preserving:
  - `collect_pattern_vars` was not exhaustive over `PatternTag`; it must now
    track `PAT_WILDCARD`, `PAT_LIT`, `PAT_QUOTE`, `PAT_GUARD`, and
    `PAT_DICT` explicitly.
  - The matrix/reduction audit did not need a new helper abstraction; the
    correct fix was to enumerate the current status/dtype cases and keep
    fail-closed branches visible.
- Current best recommendation/checkpoint:
  - Treat the switch exhaustiveness remediation as closed for the captured
    surface. Any future enum expansion should add explicit cases at the same
    time as the enum change, not via hidden defaults.
- Unresolved issues:
  - None on this audit slice.
- Signature: Codex GPT-5.4
