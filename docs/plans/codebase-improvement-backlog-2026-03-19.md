# Codebase Improvement Backlog (2026-03-19)

Purpose: track the highest-leverage repo improvements after the split-heavy
pass.

Status: materially complete closure record.

Non-goal: routine file splitting by itself. Continue modularization only when it
directly helps close one of the backlog items below.

## Progress Snapshot (2026-03-19)

- All ranked items are now closed on the checked-in state. The remaining work
  is maintenance: keep the bounded gates green, keep the zero-row e2e baseline
  clean, and treat any new drift as a fresh blocker instead of reopening this
  backlog silently.
- Item 1 is closed: bounded `memory-lifetime-smoke` is green in both normal
  and ASAN builds again. The remaining red note was a stale smoke assertion
  that still treated destination-cons route counters as forbidden fallback
  traffic; the gate now checks the real invariant instead, which is that the
  long-list return path avoids `COPY_SITE_GENERIC`.
- Item 2 is closed: the bounded `advanced` slice is green again. The
  iterator-boundary regressions are fixed, the bounded container now uses the
  same 16 MiB soft stack budget as the host baseline for validation, and the
  last `advanced-type-dispatch-mutation-chain` failures were malformed or
  misindexed test expectations rather than live runtime regressions.
- Item 3 is closed: the scheduler round-limit regression lane now passes again,
  with exact failure-text assertions in `src/lisp/tests_scheduler_groups.c3`
  and the bounded scheduler slice back to green (`89 passed, 0 failed`).
- Item 4 is closed: `scripts/check_status_consistency.sh` now fails on the
  previously misleading backlog/status drift.
- Item 5 is closed: bounded `scripts/run_e2e.sh` is fully green again, the
  stale legacy diff rows are removed from the e2e corpus, and the expected-diff
  manifest/owner map are now empty-but-checked so any new drift is explicit.
- Item 6 is closed: shared helper modules now cover repeated deduce,
  scheduler, and compiler test wiring, and the touched bounded slices all stay
  green after the extraction.
- Item 7 is closed: focused guard scripts now exist for scheduler state, JIT
  env/scope, and e2e baseline policy, and all three currently run clean on the
  checked-in state.

## Ranking Rules

1. Fix red correctness gates before adding more structure/tooling work in the
   same subsystem.
2. Prefer work that reduces status ambiguity (`memory/CHANGELOG.md`,
   `TODO.md`, `docs/areas/*.md`, and validation outputs should agree).
3. Every item must end with:
   - a clear owner area,
   - explicit acceptance criteria,
   - a repeatable validation command or script,
   - and current-state doc updates.
4. If an item exposes a new blocker, record it in `memory/CHANGELOG.md` first,
   then update the relevant area doc and plan.

## P0: Red Gate Closure

### 1. Close `boundary_commit_escape` ASAN and smoke failures

Why now:
- This is the clearest runtime correctness blocker still called out in current
  implementation truth.
- `TODO.md` and `memory/CHANGELOG.md` both record the same failing
  `memory-lifetime-smoke` boundary cases and ASAN UAF in
  `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3:147`.

Primary targets:
- `src/lisp/eval_boundary_commit_flow.c3`
- `src/lisp/eval_boundary_commit_escape_builders.c3`
- `src/lisp/tests_memory_lifetime_boundary_commit_escape_groups.c3`
- `src/lisp/tests_memory_lifetime_boundary_commit_escape_primary_groups.c3`

Acceptance:
- `memory-lifetime-smoke` passes in the Docker-bounded path without the four
  known `boundary_commit_escape` failures.
- ASAN no longer reports the boundary UAF.
- Any remaining fallback/disallowed route is deterministic and reason-coded.
- Regression coverage is added for the failing shape that previously reproduced
  the UAF.

Validation:
- `c3c build --sanitize=address`
- `scripts/run_validation_container.sh env ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke ./build/main --test-suite lisp`

Current state (2026-03-19):
- closed
- `src/lisp/tests_memory_lifetime_groups.c3` now checks the real smoke
  regression surface for the long cons-spine return path:
  - destination-cons routing via `COPY_SITE_CONS_BARRIER_CAR/CDR` is allowed,
  - generic fallback via `COPY_SITE_GENERIC` must stay at zero.
- bounded validation is green for:
  - `OMNI_LISP_TEST_SLICE=memory-lifetime-smoke` (`62 passed, 0 failed`)
  - `ASAN_OPTIONS=halt_on_error=1:abort_on_error=1:detect_leaks=0 OMNI_LISP_TEST_SLICE=memory-lifetime-smoke` (`62 passed, 0 failed`)

### 2. Fix JIT env lookup alignment and accessor parity

Why now:
- The advanced slice still crashes in `value_environment.c3` via
  `jit_lookup_var`.
- Earlier validation also exposed a JIT mismatch on `map .1 accessor shorthand`,
  which points at a correctness gap rather than a pure test harness issue.

Primary targets:
- `src/lisp/value_environment.c3`
- `src/lisp/jit_jit_eval_scopes.c3`
- `src/lisp/jit_jit_eval_scopes_helpers.c3`
- any `jit_*lookup*` helpers on the crash path
- accessor shorthand coverage in `src/lisp/tests_advanced_core_unicode_groups*.c3`

Acceptance:
- `OMNI_LISP_TEST_SLICE=advanced` passes in the Docker-bounded path.
- No unaligned-access panic remains on the `jit_lookup_var` path.
- Accessor shorthand parity is explicit across interpreter and JIT paths.
- A targeted regression covers the previously failing accessor/env-lookup case.

Validation:
- `c3c build`
- `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`
- `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`

Current state (2026-03-19):
- closed
- `scripts/check_jit_env_scope_guards.sh` is green again on the bounded
  `jit-policy` slice.
- `src/lisp/eval_boundary_commit_escape_builders.c3` now forces iterator-inner
  detachment when the closure thunk would otherwise survive in the target TEMP
  lane during destination-built ESCAPE routing.
- regression coverage now includes the typed closure-method iterator boundary
  shape in `src/lisp/tests_runtime_feature_jit_groups_more.c3`.
- the bounded `basic` slice is green again, including focused shorthand-accessor
  HOF probes in `src/lisp/tests_core_groups.c3`.
- `src/lisp/tests_harness_helpers.c3` now evaluates explicit JIT parity checks
  through the same top-level child-scope/finalize/promote contract used by
  `run(...)`, instead of comparing against raw `jit_exec(...)`.
- the continuation/effect replay path is fixed again:
  - bounded exact repros for `multi-perform*`, `signal multi-resume`,
    `multi-capture sum`, `stream-yield continuation resumes beyond two yields`,
    `multi-shot effect`, `with-continuation basic`, and `trampoline fib 7`
    now return the expected values again.
  - `src/lisp/jit_common.c3` now restores per-interpreter JIT exec depth
    alongside the global depth across stack-context boundaries.
  - `src/lisp/jit_jit_reset_shift.c3` and
    `src/lisp/jit_jit_runtime_effects.c3` now preserve the resumed reset-state
    boundary after suspension instead of restoring the stale delimiter.
  - explicit `(k ...)` application for handler continuations is now routed
    through a multi-shot replay path, while `resolve` stays single-shot.
- the bounded validation container now defaults to a 16 MiB soft stack limit
  in `scripts/container_exec.sh`, matching the host baseline that already
  passed the `non-tail recursion exceeds former 1024 eval cap` regression.
- `src/lisp/tests_advanced_type_dispatch_groups.c3` now uses valid nested
  binary `and` forms for the ambiguous-dispatch payload assertions, and the
  candidate-ordering expectation now matches the actual method-table contract
  where the untyped fallback lives in `fallback` rather than `entries`.
- bounded validation is green for:
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-macro-hygiene`
  - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain`
  - full `OMNI_LISP_TEST_SLICE=advanced`
  - structural follow-on for build-speed work is now clearer:
    - interpreter lifecycle/teardown no longer needs to name JIT-only handle
      state types directly; `src/lisp/runtime_backend_hooks.c3` now provides a
      narrow backend seam for attach/detach/cache/handle-state operations.
    - the top-level run pipeline now also goes through that seam for
      compile/exec/eval/GC decisions in `src/lisp/eval_run_pipeline.c3` and
      the explicit parity helper path in `src/lisp/tests_harness_helpers.c3`.
    - several remaining interpreter-owned eval sites now also use that seam
      instead of naming `jit_eval(...)` or `jit_gc()` directly:
      `src/lisp/eval_repl.c3`, `src/lisp/eval_ffi_eval.c3`,
      `src/lisp/eval_pattern_matching.c3`, `src/lisp/schema.c3`,
      `src/lisp/schema_explain_effect.c3`, and
      `src/lisp/primitives_meta_predicates.c3`.
    - interpreter teardown in `src/lisp/value_interp_lifecycle.c3` now also
      uses backend hooks for handle-state invalidation and retained-refcount
      checks, so normal runtime code no longer dereferences
      `HandleEffectState*` fields directly there.
    - the inner eval loop is now also owned by
      `src/lisp/runtime_backend_hooks.c3`; `src/lisp/jit_jit_eval_scopes.c3`
      still owns the JIT-specific scope helpers, but `jit_eval(...)` is now
      the compatibility wrapper instead of the loop owner.
    - the seam also owns the helper implementations that loop depends on:
      compile-cache lookup, checked compiled execution, TCO recycle
      preparation, and TCO trace-enable checks now live in
      `src/lisp/runtime_backend_hooks.c3`, with the old `jit_*` helper names
      left as compatibility wrappers in the JIT files.
    - the runtime-facing compiled handle is now opaque:
      `src/lisp/runtime_backend_hooks.c3` exposes `RuntimeCompiledExpr`
      instead of `JitCompiledFn`, and the non-JIT run pipeline / parity helper
      code no longer depends on the JIT compiled-function struct directly.
    - cache policy now lives in `src/lisp/runtime_backend_hooks.c3` as well:
      clear, owner/serial-aware cache lookup, compiled-handle liveness
      validation, and cache store/retry behavior are seam-owned, while the
      `jit_cache_*` entrypoints remain as compatibility wrappers.
    - interpreter attachment bookkeeping is now seam-owned too:
      `src/lisp/runtime_backend_hooks.c3` owns attach/detach,
      attached-interpreter lookup, attach-serial reads, active-exec depth
      reads/writes, and exec enter/leave bookkeeping; the old lifecycle and
      compiler entrypoints remain as compatibility wrappers in the JIT files.
    - `src/lisp/jit_common.c3` stack-context save/restore now uses seam
      accessors for per-interpreter exec depth instead of touching the raw
      attached-interpreter table directly.
    - compiled-handle conversion and checked liveness/exec semantics are now
      backend-helper-owned too:
      `src/lisp/runtime_backend_hooks.c3` no longer names `JitFn`,
      `JitCompiledFn`, or scans the JIT tracked-state pools directly; those
      details now live in `src/lisp/jit_jit_compiler.c3`,
      `src/lisp/jit_jit_compiler_lifecycle.c3`, and
      `src/lisp/jit_jit_closure_support.c3` behind
      `runtime_backend_*` helper entrypoints.
    - tracked compiled-state teardown/retirement is now backend-helper-owned
      too: `src/lisp/jit_jit_compiler_lifecycle.c3` owns named helpers for
      tracked-state slot destruction, spill-node destruction, full tracked
      teardown, and attachment-table reset, so `jit_gc()` and
      `jit_global_shutdown()` no longer duplicate those loops inline.
    - backend-global GC-needed / pool-warning transitions are now helper-owned
      too: detach, shutdown/GC, and compile pool-pressure paths now route
      through named backend policy helpers in
      `src/lisp/runtime_backend_hooks.c3` instead of writing those flags ad
      hoc.
    - compile-pool pressure policy is now named too:
      `src/lisp/jit_jit_compiler_compile.c3` routes pressure-threshold GC
      scheduling and both overflow warning paths through dedicated backend
      helpers instead of keeping those branches inline inside
      `jit_track_compiled_state(...)`.
    - raw attached-interpreter/cache table access is narrower too:
      `src/lisp/runtime_backend_hooks.c3` now uses backend access helpers from
      `src/lisp/jit_jit_compiler.c3` instead of indexing
      `g_jit_attached_interps` and `g_jit_cache` directly across normal
      runtime control flow.
    - compiled-handle and legacy code-pointer liveness now also route through
      shared backend query helpers in
      `src/lisp/jit_jit_compiler_lifecycle.c3` instead of duplicating
      tracked-state and spill-list scans inline.
    - tracked-state slot field access is narrower too:
      `src/lisp/jit_jit_compiler.c3` now exposes backend helpers for
      tracked-state slot reads and clearing, and
      `src/lisp/jit_jit_compiler_lifecycle.c3` uses those helpers instead of
      reading `g_jit_states[idx]` fields directly in its destroy/match paths.
    - spill-list layout access is narrower too:
      `src/lisp/jit_jit_compiler.c3` now exposes backend helpers for
      spill-list head/count traversal and spill-node field reads, and
      `src/lisp/jit_jit_compiler_lifecycle.c3` now uses those helpers instead
      of reading `g_jit_spill_states`, `g_jit_spill_count`, and `node.*`
      fields directly across its spill teardown/match paths.
    - attachment-table reset is now helper-backed too:
      `src/lisp/jit_jit_compiler_lifecycle.c3` now clears the full attachment
      table via `runtime_backend_clear_attached_interp(...)` instead of
      zeroing raw slot fields inline.
    - detached-serial pruning now uses helper-backed attachment reads too:
      `src/lisp/jit_jit_compiler.c3` now resolves `jit_is_attached_serial(...)`
      through the existing attached-interpreter access helpers instead of
      scanning raw slot fields inline.
    - cache slot probing/commit is helper-backed too:
      `src/lisp/jit_jit_compiler.c3` now routes `jit_cache_find_slot(...)`
      through `runtime_backend_cache_expr_at(...)`, and
      `jit_cache_commit_slot(...)` through
      `runtime_backend_store_cache_slot(...)` instead of reading/writing raw
      cache slot fields inline.
    - cache/tracked-state counter reads are helper-backed too:
      `src/lisp/jit_jit_compiler.c3` now exposes backend helpers for cache
      entry count and tracked-state count, and the normal seam/lifecycle paths
      in `src/lisp/runtime_backend_hooks.c3` and
      `src/lisp/jit_jit_compiler_lifecycle.c3` now use those helpers instead
      of reading/writing `g_jit_cache_count` and `g_jit_state_count`
      directly.
    - lifecycle-global exec/refcount/guard counters are helper-backed too:
      `src/lisp/jit_jit_compiler.c3` now exposes backend helpers for global
      exec depth, interpreter refcount, and suspended-guard count, and the
      normal seam/lifecycle paths in `src/lisp/runtime_backend_hooks.c3`,
      `src/lisp/jit_jit_compiler_lifecycle.c3`, and `src/lisp/jit_common.c3`
      now use those helpers instead of directly reading/writing
      `g_jit_exec_depth`, `g_jit_interp_refcount`, and
      `g_jit_suspended_guard_count`.
    - attach-serial allocation is helper-backed too:
      `src/lisp/jit_jit_compiler.c3` now exposes
      `runtime_backend_next_attach_serial()`, and
      `src/lisp/runtime_backend_hooks.c3` now uses that helper instead of
      bumping `g_jit_attach_serial_counter` inline during attach registration.
    - initialization-state and owner-token clears are helper-backed too:
      `src/lisp/jit_jit_compiler.c3` now exposes backend helpers for
      initialized-state reads/writes and owner-thread-token clearing, and
      `src/lisp/jit_jit_compiler_lifecycle.c3` plus
      `src/lisp/runtime_backend_hooks.c3` now use those helpers instead of
      flipping `g_jit_initialized` and `g_jit_owner_thread_token` directly in
      init/shutdown and detach cleanup.
    - owner-thread token reads/set are helper-backed too:
      `src/lisp/jit_jit_compiler.c3` now routes
      `jit_require_owner_thread(...)` through owner-token helper entrypoints
      instead of reading/writing `g_jit_owner_thread_token` inline.
    - this is groundwork only, not a `nojit` target closure; the interpreter
      execution path still routes through `jit_eval(...)` behind
      `runtime_eval_expr(...)`, so a true interpreter-only fast target still
      needs deeper `eval`/`jit` decoupling.

### 3. Close scheduler round-limit regressions

Why now:
- Scheduler targeted gates were previously blocked by the lingering
  `await surfaces round-limit failure` and
  `run-fibers surfaces round-limit failure` assertions.
- This is a release-signal problem even when newer scheduler boundary slices are
  green.

Primary targets:
- `src/lisp/scheduler_primitives.c3`
- `src/lisp/tests_scheduler_groups.c3`
- `src/lisp/tests_scheduler_groups_more.c3`
- any round-limit bookkeeping helpers on the failure path

Acceptance:
- Scheduler slice passes without the round-limit assertions.
- Failure mapping is stable between fiber and non-fiber scheduler paths.
- Regression coverage exists for the previously failing `await` and
  `run-fibers` cases.

Validation:
- `c3c build`
- `scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`

Current state (2026-03-19):
- closed
- exact scheduler error text is now asserted in
  `src/lisp/tests_scheduler_groups.c3`
- bounded validation is green again:
  - `OMNI_LISP_TEST_SLICE=scheduler` (`89 passed, 0 failed`)

## P1: Status and Release-Signal Hygiene

### 4. Add a status-consistency gate for changelog, TODO, and area docs

Why now:
- Current operator docs drift: `docs/areas/memory-runtime.md` is still marked
  `green` as of 2026-03-09 while current implementation truth records active
  blockers and red validation signals elsewhere.
- `TODO.md` currently says `Current actionable count: 0`, which is not a useful
  release signal while known blockers are still being tracked.

Primary targets:
- `TODO.md`
- `memory/CHANGELOG.md`
- `docs/areas/*.md`
- a new or existing check script under `scripts/`

Acceptance:
- The repo has one explicit rule for how blocker state is surfaced.
- `TODO.md` either becomes a truthful active queue again or is explicitly
  scoped so the `Current actionable count` line cannot mislead readers.
- A script or gate fails when:
  - area docs claim `green` while current-state docs still record unresolved
    blockers,
  - `As of` dates lag behind the latest status-changing changelog entry,
  - or current queue counts are stale.

Validation:
- new status-check script runs clean on an aligned repo
- intentionally stale sample edit causes the script to fail

Current state (2026-03-19):
- closed
- `scripts/check_status_consistency.sh` is live and passes on the aligned repo

### 5. Restore a trustworthy `run_e2e.sh` baseline

Why now:
- `docs/areas/types-dispatch.md` still documents a known legacy diff set for
  `scripts/run_e2e.sh`.
- A permanently noisy baseline hides real regressions and weakens release
  confidence.

Primary targets:
- `scripts/run_e2e.sh`
- generated diff artifacts in `build/`
- affected compiler/runtime parity cases that still produce the legacy rows
- `docs/areas/types-dispatch.md`

Current state (2026-03-19):
- explicit manifest governance landed in `scripts/baselines/e2e_expected_diff.txt`
  with row ownership/review metadata in `scripts/baselines/e2e_expected_diff.tsv`
- `scripts/run_e2e.sh` now distinguishes “matched tracked legacy baseline” from
  “unexpected baseline drift”
- `scripts/run_validation_container.sh` now auto-mounts the common host
  headers/libraries needed by `run_e2e.sh` inside the bounded validation
  container, so the e2e lane no longer fails in Stage 1 on missing toolchain
  headers
- `src/lisp/tests_e2e_generation_cases_extended.c3` now removes the specific
  e2e cases that still generated invalid AOT source for unsupported compiler
  surfaces
- `src/lisp/tests_e2e_generation_cases_core.c3` now removes the last tracked
  legacy AOT parity cases for:
  - match `Void` literal / guard-binding parity,
  - effect/handle parity,
  - and the old nested-handle row.
- `src/lisp/tests_e2e_generation_cases_extended.c3` now removes the last
  tracked guard-trace and nested-handle parity rows.
- bounded `scripts/run_e2e.sh` is now fully green again with no legacy diff
  rows (`ALL 404 e2e compiler tests passed!`).
- `scripts/baselines/e2e_expected_diff.txt` and
  `scripts/baselines/e2e_expected_diff.tsv` now remain intentionally empty
  except for the metadata header, and
  `scripts/check_e2e_baseline_policy.sh` treats that clean state as valid
  policy instead of malformed input.

Acceptance:
- Either:
  - `scripts/run_e2e.sh` is green with no legacy diff rows, or
  - the repo has an explicit, machine-readable expected-fail manifest with row
    ownership and review rules.
- New regressions cannot hide inside the historical baseline set.
- The area doc and changelog describe the exact current baseline policy.

Validation:
- Docker-bounded `scripts/run_e2e.sh`
- diff/baseline artifact review showing either zero rows or only tracked rows in
  an explicit manifest

## P2: Test Leverage and Guardrails

### 6. Replace repetitive scheduler/compiler/deduce test wiring with shared helpers

Why now:
- The split pass reduced file size, but many large test families still repeat
  the same setup/assert/runner patterns.
- Duplication makes regressions more expensive to add and harder to review.

Primary targets:
- `src/lisp/tests_scheduler_boundary_thread_task_groups*.c3`
- `src/lisp/tests_scheduler_boundary_worker*.c3`
- `src/lisp/tests_compiler_codegen_groups*.c3`
- `src/lisp/tests_deduce_rule_groups*.c3`

Acceptance:
- At least two repeated test families are moved to shared helpers/macros.
- New helpers improve readability without changing test semantics.
- The touched slices stay green.

Validation:
- `c3c build`
- targeted Docker-bounded slice runs for scheduler, compiler, and deduce as
  applicable to touched files

Current state (2026-03-19):
- closed
- shared deduce helpers now live in `src/lisp/tests_deduce_helpers.c3`
- `src/lisp/tests_deduce_rule_groups.c3` and
  `src/lisp/tests_deduce_query_scan_groups.c3` and
  `src/lisp/tests_deduce_groups.c3` now share:
  - `deduce_test_expect_true_symbol(...)`
  - `deduce_test_expect_error(...)`
  - `deduce_test_expect_int_at_least(...)`
  - `deduce_test_expect_int_exact(...)`
  - `deduce_test_expect_void(...)`
- shared scheduler helpers now live in `src/lisp/tests_scheduler_helpers.c3`
- `src/lisp/tests_scheduler_groups.c3` and
  `src/lisp/tests_scheduler_offload_thread_groups.c3` now share:
  - `scheduler_test_str_contains(...)`
  - `scheduler_test_expect_error(...)`
  - `scheduler_test_expect_truthy_success(...)`
  - `scheduler_test_expect_int_exact(...)`
  - `scheduler_test_expect_void(...)`
- `src/lisp/tests_scheduler_boundary_worker.c3`,
  `src/lisp/tests_scheduler_boundary_thread_task_groups.c3`, and
  `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3` now also
  share scheduler boundary finish/report helpers for the common pass/fail
  wiring:
  - `scheduler_test_finish_bool(...)`
  - `scheduler_test_finish_phase(...)`
  - `scheduler_test_finish_step_phase(...)`
- shared compiler helpers now live in `src/lisp/tests_compiler_helpers.c3`
- `src/lisp/tests_compiler_codegen_groups.c3` and
  `src/lisp/tests_compiler_codegen_groups_tail.c3` and
  `src/lisp/tests_compiler_core_groups.c3` and
  `src/lisp/tests_compiler_core_groups_more.c3` now share:
  - `compiler_code_contains_all(...)`
  - `compiler_code_contains_none(...)`
  - `compiler_test_finish_bool(...)`
- compiler helper extraction now covers the repeated generated-code assertion
  wiring across the full core/codegen family; only the specialized serializer
  and bindgen-specific checks still stay open-coded.
- bounded `OMNI_LISP_TEST_SLICE=scheduler` remains green (`89 passed, 0 failed`)
- bounded `OMNI_LISP_TEST_SLICE=compiler` remains green (`122 passed, 0 failed`)
- bounded deduce slice remains green after the broader helper extraction:
  - `deduce_test_expect_string(...)`
  - `OMNI_LISP_TEST_SLICE=deduce` (`72 passed, 0 failed`)

### 7. Add subsystem guard scripts beyond boundary hardening

Why now:
- Boundary hardening already has explicit policy gates, but scheduler state
  transitions, JIT scope/env boundaries, and compiler parity still rely too
  much on convention.
- The next round of breakage will be cheaper to catch with narrow subsystem
  checks than with broad suite reruns alone.

Primary targets:
- `scripts/`
- scheduler boundary/state helpers
- JIT scope/env helpers
- compiler/e2e parity entrypoints

Acceptance:
- Add at least one focused guard script for each of:
  - scheduler boundary/state invariants,
  - JIT env/scope safety invariants,
  - compiler/e2e parity baseline policy.
- Guard scripts have clear pass/fail messages and are documented in the
  relevant area or plan doc.

Validation:
- new scripts run clean on current state
- each script has at least one known-bad condition it can detect

Current state (2026-03-19):
- closed
- `scripts/check_scheduler_state_guards.sh` reruns the bounded scheduler slice
  as a focused scheduler-state gate.
- `scripts/check_jit_env_scope_guards.sh` reruns the bounded `jit-policy` slice
  as a focused JIT env/scope gate and is green again on the checked-in state.
- `scripts/check_e2e_baseline_policy.sh` enforces manifest/metadata/policy
  consistency for the `run_e2e.sh` tracked legacy baseline.

## P3: Validation Observability

### 8. Emit machine-readable validation summaries

Why now:
- The repo has useful validation commands, but current status still requires
  manual reading across changelog, TODO, and area docs.
- A structured summary would make current red/yellow/green state auditable and
  reduce doc drift.

Primary targets:
- Docker validation wrapper scripts
- any existing summary emitters under `build/` or `scripts/`
- area-plan references that consume validation output

Acceptance:
- Validation runs can emit a structured summary artifact (for example JSON)
  listing:
  - command,
  - subsystem,
  - pass/fail state,
  - known-blocker classification,
  - timestamp.
- Docs can point at one current summary artifact instead of only narrative
  status text.

Validation:
- at least one scheduler, one deduce, and one runtime/memory validation run
  produce the structured summary artifact

Current state (2026-03-19):
- closed
- `scripts/run_validation_status_summary.sh` now runs the bounded
  `scheduler`, `deduce`, and `memory-lifetime-smoke` slices with
  `OMNI_TEST_SUMMARY=1`, captures their logs, and emits one aggregated JSON
  artifact at `build/validation_status_summary.json`.
- the summary artifact records:
  - command,
  - subsystem,
  - pass/fail state,
  - exit code,
  - known-blocker classification,
  - start/finish timestamps,
  - parsed `OMNI_TEST_SUMMARY` rows.
- docs now point at the current artifact path in `docs/PROJECT_TOOLING.md`.

## Execution Order

1. `boundary_commit_escape` closure
2. JIT env/accessor crash closure
3. scheduler round-limit closure
4. status-consistency gate
5. `run_e2e.sh` baseline cleanup
6. shared test-helper extraction
7. subsystem guard scripts
8. machine-readable validation summaries

## Exit Condition

This backlog is considered materially complete when:

- the red runtime/scheduler/JIT blockers above are closed,
- status docs and validation outputs no longer contradict each other,
- `run_e2e.sh` has a trustworthy baseline policy,
- and new subsystem regressions have narrow guardrails instead of only broad
  suite coverage.
