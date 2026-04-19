# Session Report Index Part 25

Source: `.agents/SESSION_REPORT.md`

## 2026-04-12 16:38:56 CEST - Runtime Boundary Audit and Fix Pass

Objective attempted: continue the whole-codebase audit/fix pass with emphasis on runtime boundary promotion/copy paths, closure env copy/promotion memoization, allocator growth, and stale validation status wording.

Workspace: `/home/christos/Omni`.

Code/configuration changes made:
- Squashed source/doc/script changes into `master`; final source commit is `edb06c3b` (`Harden type registry and allocator growth paths`).
- Fixed `ast_arena_alloc` inverted `math::overflow_add` checks so normal arena growth no longer returns null.
- Fixed stack defer reserve overflow test expectation to preserve the first grown capacity after a rejected overflow request.
- Repaired `src/lisp/tests_core_groups.c3` after a raw patch-marker corruption in the parent commit and tightened the type-registry allocation-failure recovery assertion.
- Added checked allocation growth guards in bind runtime setup, Pika named grammar growth, and FTXUI helper pointer/component vectors.
- Added early promotion-context memoization for closure payload copy/promotion and shared wrapper container copy paths, and installed the passed promotion context during parent-site boundary copy so nested copy helpers see the active context.
- Added env-copy regression coverage for a self-referential closure payload cloned through promotion-context memoization.
- Hardened boundary provenance child traversal so root-owned child graphs are not recursively walked during releasing-scope reuse checks, non-root graph-bearing children retain target-chain checks, and child recursion has a depth guard.
- Updated validation/status docs and `scripts/run_validation_status_summary.sh` wording to avoid stale host-local e2e/full-suite claims.

Commands run and key results:
- `jj git fetch`: `Nothing changed`.
- `git diff --check -- src docs scripts`: passed.
- patch-marker/stale-wording scan over `src docs scripts`: no matches.
- `c3c build`: passed with pre-existing `errno::*` deprecation warnings in `src/main_repl_shared.c3`.
- `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `suite=unified pass=156 fail=0`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack`: passed, `Stack engine: 24 passed, 0 failed`.
- `scripts/run_ftxui_smoke.sh`: passed; `module_value_smoke`, `module_effect_smoke`, full `smoke.omni`, and `demo.omni` all completed.
- `c3c build --sanitize=address`: unavailable in this toolchain, reporting that address sanitizer is unsupported.

Invalidated assumptions / failed approaches:
- Do not trust the `agent-1776003689746-p0w3r1` Deduce refresh output. It wrote raw `<<<<<<< SEARCH` markers and truncated four Deduce refresh files; those files were restored with `jj restore` and should not be used as a basis for future work.
- Do not assume `math::overflow_add` returns true on success. The corrected code treats true as overflow/failure.
- Provenance child traversal cannot safely recurse through root-owned cyclic module/registry maps during releasing-boundary reuse classification. Treat root-owned children as lifetime-stable in that walk and inspect non-root graph children instead.

Current best recommendation / checkpoint:
- Source/doc/script work, including the final FTXUI resolve fix, is committed in `master` at `edb06c3b`; the remaining working-copy changes are operational artifacts under `.agents/`, `.claude-flow/`, `.serena/`, and `.swarm/`.
- The FTXUI `resolve` continuation smoke is now green after keeping the active promotion-context memo lookup, avoiding recursive alias walks for edge-free values, adding alias-walk depth/headroom guards, and raising the normal stack-context budget to 256KB.
- Memory-lifetime smoke/full ownership slices were not run because repo policy requires container-bound execution for memory ownership slices, and the bounded container path was previously unavailable in this environment.

Unresolved issues:
- The new self-referential env-copy regression is compiled but not executed through the memory-lifetime smoke slice locally because that slice is container-only by repo policy.

### 2026-04-12 15:16 CEST - Value Environment Hardening Patch

Objective attempted:
- Apply narrow, fail-closed hardening in `value_environment.c3` and `value_environment_storage.c3` for capacity growth and hash rebuild arithmetic.

Code/configuration changes made:
- `src/lisp/value_environment.c3:22-63`: added overflow and OOM-closure guards in `Env.define`.
  - guard `capacity > usz.max / 2` before doubling
  - guard `new_cap > usz.max / Binding.sizeof` before allocation
  - avoid overflow in load-factor path with `hash_capacity > usz.max / 10`
  - replace `binding_count * 10 > hash_capacity * 7` with `binding_count > (hash_capacity * 7) / 10`
- `src/lisp/value_environment_storage.c3:142-156`: added capacity/size guards in `Env.build_hash_table`.
  - return-fail when `binding_count > usz.max / 2`
  - growth loop guard `cap > usz.max / 2` before doubling
  - guard allocation bytes with `binding_count * 2 > usz.max / EnvHashEntry.sizeof`

Commands and results:
- `git diff --check -- src/lisp/value_environment.c3 src/lisp/value_environment_storage.c3`: clean.
- `rg -n '^(<<<<<<<|>>>>>>>)|\*\*\* (Search|Replace)' src/lisp/value_environment.c3 src/lisp/value_environment_storage.c3`: no matches.
- `c3c build`: blocked by pre-existing merge markers in `src/lisp/eval_pattern_equality_helpers.c3`.

Unresolved / residual risk:
- End-to-end compilation and broader test slices remain unavailable until unrelated merge markers in `src/lisp/eval_pattern_equality_helpers.c3` are resolved.

Signature: Codex GPT-5

## 2026-04-12 17:46:11 CEST - Whole-Codebase Allocation Hardening Audit/Fix Continuation

Objective attempted: continue the whole-codebase audit and fix pass using parallel GPT-5.4 high audit agents and scoped GPT-5.3 Spark implementation workers, then commit non-artifact source changes and fetch remote updates.

Workspace: `/home/christos/Omni`.

Code/configuration changes made:
- Hardened parser AST, module, relation-attribute, pattern, handle, pipe, import/export, collection literal, and type annotation allocation paths with checked byte-size/count arithmetic before arena allocation.
- Hardened Pika named grammar rule table registration/growth to fail closed on capacity/registration failure, including the clause symbol-ref placeholder path.
- Hardened JIT/effect handler staged argument, closure/method signature, module export, and method table allocation/copy paths with overflow guards.
- Hardened Deduce rule signature persistence/record codec size computation and cursor movement, SCC/reachability matrix allocations, row materialization dictionary capacity, column key allocation, explain/list builders, schema query refresh payload handling, and why-result list concatenation error propagation.
- Hardened environment hash rebuild behavior so post-mutation rebuild failure falls back to linear lookup instead of reporting mutation failure after state has already changed.
- Hardened REPL server/session and JSON request size arithmetic.
- Kept operational artifacts under `.agents/`, `.claude-flow/`, `.serena/`, and `.swarm/` out of the planned source-only commit.

Commands run and key results:
- `c3c clean && c3c build`: passed with existing `errno::*` deprecation warnings in `src/main_repl_shared.c3`.
- `c3c build`: passed again after final hygiene checks.
- `rg -n '^(<<<<<<<|=======|>>>>>>>)|\*\*\* (Search|Replace)' src/lisp src/pika src/stack_engine_lifecycle.c3`: no matches.
- `git diff --check -- src/lisp src/pika src/stack_engine_lifecycle.c3`: passed.
- `OMNI_LISP_TEST_SLICE=basic OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=156 fail=0`.
- `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=195 fail=0`.
- `OMNI_LISP_TEST_SLICE=jit-policy OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=49 fail=0`.
- `OMNI_LISP_TEST_SLICE=pika OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=83 fail=0`.
- `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=materialized OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=8 fail=0`.
- `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=relation-attrs OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=13 fail=0`.
- `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=integrity OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=34 fail=0`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite stack`: passed, `24 passed, 0 failed`.
- `scripts/run_ftxui_smoke.sh`: passed.
- Manual recursive Deduce `--eval` expression returned `true`.
- Initial parent comparison in `/tmp/omni-parent-edb06c3b` at `edb06c3b` reproduced the then-current grouped Deduce failures, which showed they were not caused by the allocation-hardening runtime paths.
- Follow-up source fixes in the committed tree resolved the grouped Deduce parser failures:
  - `src/lisp/tests_deduce_rule_groups_more_tail_head.c3` no longer carries stale duplicated analysis tests that conflicted with the split `more_tail_analysis_tests` coverage.
  - `src/lisp/tests_deduce_query_admin_surface_fallback_tests.c3` has the integrity-reporting assertion flattened to the intended parseable shape.
  - `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=rule-validation OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=79 fail=0`.
  - `OMNI_LISP_TEST_SLICE=deduce OMNI_DEDUCE_GROUP_FILTER=query OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=254 fail=0`.
  - `OMNI_LISP_TEST_SLICE=deduce OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`: passed, `pass=432 fail=0`.

Invalidated assumptions / failed approaches:
- Do not rely on the earlier `AUDIT-DEDUCE-GROUPED-PARSER-STATE-040` open TODO entry or the earlier report text that said broad Deduce still failed; those artifacts were stale after the committed tree fixed and verified the grouped parser failures.
- Do not rely on `jj diff` rendering alone for source verification when it appears to fuse identifiers; use `git diff` and direct file reads for final inspection.

Current best recommendation / checkpoint:
- Broad Deduce grouped parser failures are verified fixed in the current source tree. `TODO.md` was updated to close `AUDIT-DEDUCE-GROUPED-PARSER-STATE-040`.
- Proceed with normal source commit/push for the stale backlog cleanup if this working copy is being published.

Unresolved issues:
- Full heavy/container-only memory lifetime gates were not run in this host session.

Signature: Codex GPT-5

## 2026-04-13 22:45:08 CEST - Constrained Reader Tag Surface

Objective attempted:
- Implement the owner-requested hybrid reader-tag surface: Clojure-like
  `#tag form` use without a full Common Lisp readtable facility.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added a `T_READER_TAG` token and `#tag` lexer dispatch while preserving
  `#r"..."`, `#_`, `#N_`, and `#| ... |#`.
- Added parser lowering for `#tag form` to the normal one-argument call
  `(tag form)`.
- Added `(define [reader tag] name (syntax-match ...))` as the canonical
  reader-tag macro declaration surface by reusing the existing `E_DEFMACRO`
  parser path.
- Added reader-dispatch regressions for ordinary function tags, slash tags,
  reader-tag macros, malformed reader-tag definitions, numeric `#1x`, and
  bare `#`.
- Updated `memory/CHANGELOG.md` plus language/reference/syntax/compatibility
  docs for the new canonical surface.

Commands run and key results:
- `c3c build --obj-out obj`: passed and linked `build/main`; existing
  `errno::*` deprecation warnings in `src/main_repl_shared.c3` remain.
- `OMNI_LISP_TEST_SLICE=reader-dispatch LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `16 passed, 0 failed`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(block (define (spy x) x) #spy 7)'`:
  returned `7`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(block (define [reader tag] spy (syntax-match ([x] (template (block (insert x)))))) #spy 7)'`:
  returned `7`.
- `git diff --check -- ...`: passed for the touched source/docs/changelog files.

Current best recommendation / checkpoint:
- Treat `[reader tag]` as the chosen canonical declaration spelling. Do not add
  parallel `[reader/tag]`, `[reader-tag]`, or `defreader` aliases unless the
  owner explicitly requests a migration or alternate surface.

Unresolved issues:
- Full global/container gates were not run; this was verified with the narrow
  reader-dispatch slice and a local build using the workspace's known
  `--obj-out obj` recovery path.

Signature: Codex GPT-5
## 2026-04-16 05:57 CEST - Float String Precision

Objective attempted:
- Close the approved `Float` constructor precision spelling gap by accepting
  string precision arguments such as `(Float x "64")`, while preserving the
  fail-closed `Float32` boundary.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Updated `prim_ctor_float` so the optional precision argument accepts either
  integer precision (`64`, `32`) or string precision (`"64"`, `"32"`).
- `(Float x "64")` delegates to the existing `Float64` constructor.
- `(Float x "32")` fails closed with the same unimplemented `Float32` storage
  error as `(Float x 32)`.
- Added constructor regressions for string precision `64` and string precision
  `32`.
- Updated current language/reference docs and `memory/CHANGELOG.md`.

Commands run and key results:
- `c3c build main --output-dir build --build-dir build/obj2`: passed with
  existing `errno::*` / `process::create` deprecation warnings.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-core-semantics OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=71 fail=0`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (Float "1.25" "64"))'`:
  returned `"1.25"`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(Float 1 "32")'`:
  failed closed with `Float: precision 32 requires Float32 runtime storage, which is not implemented yet`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(Float 1 "bad")'`:
  failed with `Float: unsupported precision; expected 64`.
- `git diff --check`: passed.

Current best recommendation / checkpoint:
- Treat integer and string precision spellings as accepted for `Float`.
  Implementing actual `Float32` remains a separate runtime/tensor storage
  slice, not a constructor parsing task.

Unresolved issues:
- Full heavy/container-only gates were not run for this narrow constructor
  surface slice.

Signature: Codex GPT-5

## 2026-04-16 05:16 CEST - Float64 Surface Canonicalization

Objective attempted:
- Replace the public binary64 `Double` surface with canonical `Float64`, while
  adding the approved `(Float value [precision])` constructor form and keeping
  unimplemented `Float32` fail-closed.

Workspace:
- `/home/christos/Omni`.

Code/configuration changes made:
- Added `Float`, `Float32`, and `Float64` callable constructor registrations.
- Changed the existing binary64 symbol/type/dtype display surface to
  `Float64`; internal storage names like `DOUBLE`, `sym_Double`,
  `is_double`, and `TENSOR_DTYPE_DOUBLE` remain implementation names.
- Added `(Float x)` as default `Float64`, `(Float x 64)` as explicit binary64,
  and fail-closed errors for `(Float x 32)` / `(Float32 x)`.
- Removed the public `Double` constructor surface instead of keeping an alias.
- Renamed the public predicate from `double?` to `float64?`.
- Updated Tensor dtype spelling, FFI annotation docs, compiler primitive
  lookup tables, examples, and current reference/spec/planning docs.
- Added constructor regressions for default `Float`, explicit precision 64,
  fail-closed `Float32`, `Float64` type identity, and removed `Double`.

Commands run and key results:
- `c3c build main --output-dir build --build-dir build/obj2`: passed with
  existing `errno::*` / `process::create` deprecation warnings.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (type-of (Float 1)))'`:
  returned `"Float64"`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(String (Float "1.25" 64))'`:
  returned `"1.25"`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(Float32 1)'`:
  failed closed with `Float32: runtime storage is not implemented yet`.
- `LD_LIBRARY_PATH=/usr/local/lib ./build/main --eval '(Double 1)'`:
  failed as `unbound variable 'Double'`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-core-semantics OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=71 fail=0`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=240 fail=0`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-ffi-system OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=75 fail=0`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-stdlib-numeric OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=411 fail=0`.
- `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=392 fail=0`.
- `OMNI_LISP_TEST_SLICE=compiler OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
  passed, `pass=276 fail=0`.

Invalidated assumptions / failed approaches:
- Do not preserve `Double` as a public compatibility alias. The current
  pre-alpha surface intentionally removes it and tests that `(Double 3)` is
  unbound.
- Do not treat `Float32` as implemented just because the constructor symbol is
  registered; it is reserved and must fail closed until storage/kernels exist.

Current best recommendation / checkpoint:
- Continue scientific numeric work using `Float64` as the native hardware
  floating dtype and `(Float x [precision])` as the precision-selecting
  constructor surface. Add real `Float32` only as a storage/kernel slice.

Unresolved issues:
- Full heavy/container-only memory lifetime gates were not run for this naming
  cleanup; the touched behavior was covered with focused constructor, Tensor,
  FFI, dispatch, numeric, collections, and compiler slices.

Signature: Codex GPT-5

## 2026-04-17 09:38:59Z
- Objective: fix the Vulkan singular-norm helper so backend availability,
  Float64 support, and shared shape validation run before the zero-size success
  path.
- Workspace: `/home/christos/Omni`
- Changes made:
  - Reordered `omni_tensor_backend_vulkan_singular_norm_f64` in
    `csrc/tensor_vulkan_helpers.c` to check Vulkan availability, Float64
    support, and `omni_tensor_backend_vulkan_singular_values_validate_shape_f64`
    before the zero-size return.
  - Added `omni_tensor_backend_vulkan_singular_norm_zero_size_validation_probe_for_tests`
    in `csrc/tensor_vulkan_helpers.c`.
  - Added the corresponding extern declaration in
    `src/lisp/tensor_vulkan_backend.c3`.
  - Added a narrow regression in
    `src/lisp/tests_advanced_stdlib_module_groups.c3` covering the probe.
- Commands run:
  - `c3c build`
  - `git diff --check -- csrc/tensor_vulkan_helpers.c src/lisp/tensor_vulkan_backend.c3 src/lisp/tests_advanced_stdlib_module_groups.c3`
  - `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/workspace/build OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 ./build/main --test-suite lisp`
- Key result:
  - Targeted bounded advanced-collections-module validation passed with
    `pass=898 fail=0`.
- Current recommendation:
  - No further code change is needed for this audit item.
- Open issues:
  - None for this patch; broader unrelated worktree changes remain present but
    untouched.
- Next actions:
  - Leave the rest of the tensor Vulkan surface alone unless a separate audit
    item lands.
- Signature: Codex GPT-5

## 2026-04-17 11:48 CEST - Vulkan Lazy Contract And Backlog Audit Fixes

- Objective attempted:
  - Continue the Tensor/Vulkan audit and fix pass with GPT-5.4 high auditors
    and local implementation after Spark/fast agents were disabled by quota
    policy.
- Workspace/target:
  - `/home/christos/Omni`.
- Code or configuration changes made:
  - Added `tensor_contract_try_device_value` in `src/lisp/prim_tensor.c3` and
    routed one-argument `realize` for lazy contract payloads through it before
    CPU fallback.
  - Added public and internal regressions in
    `src/lisp/tests_advanced_stdlib_module_groups.c3`. The internal C3 harness
    constructs lazy map/contract payloads directly so the new helper is
    exercised instead of only covering the eager public Vulkan route.
  - Reordered `omni_tensor_backend_vulkan_singular_norm_f64` validation in
    `csrc/tensor_vulkan_helpers.c`, added
    `omni_tensor_backend_vulkan_singular_norm_zero_size_validation_probe_for_tests`,
    and exposed that probe through `src/lisp/tensor_vulkan_backend.c3`.
  - Normalized backlog/docs state: `TODO.md` now has one live item
    (`TENSOR-100F`), `TENSOR-100E` is closed as the correctness-first Vulkan
    baseline, and `TENSOR-100G` is closed as the measured parallel-solve
    baseline. Updated the Vulkan plan index, backend decision note, tensor
    scientific plan, area status, and Vulkan roadmap wording to match.
  - Updated `.agents/PLAN.md` and `memory/CHANGELOG.md` with this checkpoint.
- Commands run and key results:
  - `./scripts/build_omni_chelpers.sh`: passed.
  - `c3c build --obj-out obj`: passed with existing deprecation warnings.
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-collections-module OMNI_TEST_SUMMARY=1 LD_LIBRARY_PATH=/usr/local/lib ./build/main --test-suite lisp`:
    passed, `pass=913 fail=0`.
  - `./scripts/check_primitive_docs_parity.sh`: passed.
  - `./scripts/check_e2e_baseline_policy.sh --stage3-source-parity`: passed.
  - Targeted `git diff --check`: passed.
- Invalidated assumptions or failed approaches worth preserving:
  - Public `(map + (to-device ...) 0.0)` contract tests are eager Vulkan-route
    coverage, not lazy-contract-helper coverage. Keep the internal harness
    tests for `tensor_contract_try_device_value`.
- Current best recommendation / checkpoint:
  - Remaining live Vulkan work is `TENSOR-100F`: helper/library factoring,
    dense row-major `Float64` expansion, and future dtype/layout work only
    after native storage/layout contracts exist.
- Unresolved issues:
  - Full heavy/container gates were not rerun for this checkpoint. The worker
    did run a bounded focused advanced collections pass before the final local
    test count changed; the final verified local focused pass is `913/0`.
- Signature: Codex GPT-5.4
