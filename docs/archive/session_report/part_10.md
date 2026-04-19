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
