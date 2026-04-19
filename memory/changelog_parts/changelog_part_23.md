# Memory Changelog Index Part 23

Source: `memory/CHANGELOG.md`

  - bounded `memory-lifetime-smoke` is green again on both normal and ASAN
    builds (`62 passed, 0 failed` in each profile).
  - the remaining red smoke note was a stale assertion in
    `src/lisp/tests_memory_lifetime_groups.c3`: the test still treated
    `COPY_SITE_CONS_BARRIER_CAR/CDR` as forbidden fallback traffic even though
    those counters now represent the normal destination-cons route used by
    `boundary_commit_escape(...)`.
  - the smoke gate now checks the real invariant for the long cons-spine return
    path:
    - destination-cons routing is allowed,
    - `COPY_SITE_GENERIC` must stay at zero.

- Advanced bounded validation closure:
  - `scripts/container_exec.sh` now sets a 16 MiB soft stack limit by default
    inside the validation container, matching the host baseline (`16384 KB`)
    instead of the prior 8 MiB container default (`8192 KB`).
  - this closes the bounded `advanced-macro-hygiene` crash on the
    `non-tail recursion exceeds former 1024 eval cap` regression without
    changing the runtime recursion semantics on the host path.
  - `src/lisp/tests_advanced_type_dispatch_groups.c3` also now uses valid
    nested binary `and` forms for the ambiguous-dispatch payload assertions,
    and the candidate-ordering expectation now matches the actual method-table
    layout where the untyped fallback lives in `MethodTable.fallback`.
  - bounded validation is green again for:
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-macro-hygiene`
    - `OMNI_LISP_TEST_SLICE=advanced OMNI_ADVANCED_GROUP_FILTER=advanced-type-dispatch-mutation-chain`
    - full `OMNI_LISP_TEST_SLICE=advanced` (`1086 passed, 0 failed`)

- Runtime backend seam for interpreter lifecycle:
  - added `src/lisp/runtime_backend_hooks.c3` as a narrow runtime/backend
    facade for interpreter-owned lifecycle code.
  - moved the fast-dev entry files out of the default source tree into
    `tools/fast-dev/` so `project.json` no longer pulls them into the full
    `build/main` target by accident.
  - `src/lisp/value_interp_lifecycle.c3` and
    `src/lisp/value_interp_state.c3` now call backend-neutral wrappers for:
    - interp attach/detach,
    - cache clearing,
    - global runtime shutdown,
    - live handle-state release checks.
  - `src/lisp/eval_run_pipeline.c3` and
    `src/lisp/tests_harness_helpers.c3` now route top-level
    compile/exec/eval/GC control through the same backend seam instead of
    calling `jit_compile(...)`, `jit_exec(...)`, `jit_eval(...)`, and `jit_gc()`
    directly from the run pipeline.
  - additional interpreter-owned eval sites now also go through the same seam:
    - `src/lisp/eval_repl.c3` now routes REPL pre-eval GC through the backend
      hook instead of calling `jit_gc()` directly.
    - `src/lisp/eval_ffi_eval.c3`, `src/lisp/eval_pattern_matching.c3`,
      `src/lisp/schema.c3`, `src/lisp/schema_explain_effect.c3`,
      `src/lisp/primitives_meta_predicates.c3`, and targeted trace coverage in
      `src/lisp/tests_advanced_stdlib_numeric_groups.c3` now use
      `runtime_eval_expr(...)` instead of naming `jit_eval(...)` directly.
  - `src/lisp/value_interp_lifecycle.c3` now also routes continuation-handle
    invalidation and retained-refcount checks through
    `src/lisp/runtime_backend_hooks.c3`, so normal interpreter teardown no
    longer dereferences `HandleEffectState*` fields directly.
  - the inner eval loop now also lives behind the backend seam:
    - `src/lisp/runtime_backend_hooks.c3` owns the current-backend eval loop,
      including compile lookup, checked execution, TCO bounce handling, and
      TCO trace emission.
    - `src/lisp/jit_jit_eval_scopes.c3` keeps the JIT-specific scope helpers,
      but `jit_eval(...)` now delegates to `runtime_eval_expr(...)` instead of
      owning the loop itself.
  - the seam now also owns the helper implementations that loop depends on:
    - compile-cache lookup / compile fallback,
    - checked compiled-function execution,
    - TCO recycle preparation,
    - and TCO trace-enable checks.
    - `src/lisp/jit_jit_eval_scopes.c3` and
      `src/lisp/jit_jit_closure_support.c3` keep the old `jit_*` helper names
      for existing callers and tests.
  - the runtime-facing compiled-expression surface is now opaque:
    - `src/lisp/runtime_backend_hooks.c3` exposes `RuntimeCompiledExpr`
      instead of `JitCompiledFn`.
    - `src/lisp/eval_run_pipeline.c3` and
      `src/lisp/tests_harness_helpers.c3` no longer mention the JIT compiled
      handle type directly.
    - JIT modules still use `JitCompiledFn` internally, with explicit
      wrap/unwrap at the seam boundary.
  - cache policy ownership also moved behind the seam:
    - `src/lisp/runtime_backend_hooks.c3` now owns cache clear, owner/serial
      aware cache lookup, liveness validation for compiled handles, and
      cache store/retry behavior.
    - `src/lisp/jit_jit_compiler.c3` and
      `src/lisp/jit_jit_compiler_lifecycle.c3` keep `jit_cache_*` and
      `jit_compiled_fn_is_live_for_interp(...)` helper entrypoints.
  - attachment-state bookkeeping is now seam-owned too:
    - `src/lisp/runtime_backend_hooks.c3` now owns attach/detach,
      attached-interpreter lookup, attach-serial reads, active-exec depth
      reads/writes, and exec enter/leave bookkeeping.
    - `src/lisp/jit_jit_compiler.c3` and
      `src/lisp/jit_jit_compiler_lifecycle.c3` keep the old attached-interp
      and lifecycle entrypoints.
    - `src/lisp/jit_common.c3` stack-context save/restore now reads and
      restores per-interpreter exec depth through the seam instead of touching
      the raw attached-interpreter table directly.
  - compiled-state conversion and liveness/execution details moved back behind
    backend helper entrypoints:
    - `src/lisp/runtime_backend_hooks.c3` no longer names `JitFn`,
      `JitCompiledFn`, or scans `g_jit_states` / `g_jit_spill_states`
      directly.
    - `src/lisp/jit_jit_compiler.c3` now owns the
      `RuntimeCompiledExpr` <-> `JitCompiledFn` wrap/unwrap helpers.
    - `src/lisp/jit_jit_compiler_lifecycle.c3` now owns backend-side
      compiled-handle liveness checks.
    - `src/lisp/jit_jit_closure_support.c3` now owns backend-side compile,
      checked exec, and exec wrapper entrypoints for `RuntimeCompiledExpr`.
  - tracked compiled-state teardown/retirement is now backend-helper-owned:
    - `src/lisp/jit_jit_compiler_lifecycle.c3` now owns named backend helpers
      for tracked-state slot destruction, spill-node destruction, full tracked
      state teardown, and attachment-table reset.
    - `jit_global_shutdown()` and `jit_gc()` no longer open-code the tracked
      state / spill teardown loops directly; they route through the backend
      helpers instead.
  - backend-global GC / pool-warning policy now has named helper transitions:
    - `src/lisp/runtime_backend_hooks.c3` now owns helper entrypoints for
      reading/setting/clearing the GC-needed flag and the pool-warning flag.
    - interpreter detach, shutdown/GC paths, and compile pool-pressure paths
      now route through those helpers instead of writing `g_jit_gc_needed` and
      `g_jit_pool_warned` ad hoc.
  - compile-path pool-capacity policy is now named too:
    - `src/lisp/jit_jit_compiler_compile.c3` now uses dedicated backend helper
      functions for:
      - scheduling GC when tracked-state usage crosses the pressure threshold,
      - warning on spill-allocation failure,
      - and warning when overflow tracking moves into the spill list.
    - `jit_track_compiled_state(...)` no longer embeds those warning / policy
      branches inline.
  - raw attached-interpreter/cache table access is narrower now:
    - `src/lisp/jit_jit_compiler.c3` now exposes backend access helpers for
      attached-interpreter table reads/writes and cache-slot reads/writes.
    - `src/lisp/runtime_backend_hooks.c3` no longer indexes
      `g_jit_attached_interps` or `g_jit_cache` directly for normal runtime
      control flow; it now goes through those backend helpers instead.
  - liveness scans are named backend queries now:
    - `src/lisp/jit_jit_compiler_lifecycle.c3` now routes compiled-handle and
      older code-pointer liveness checks through shared backend query helpers
      for tracked-state and spill-node matching, instead of duplicating those
      scans inline.
  - tracked-state slot field access is narrower now too:
    - `src/lisp/jit_jit_compiler.c3` now exposes backend access helpers for
      tracked-state slot reads and slot clearing.
    - `src/lisp/jit_jit_compiler_lifecycle.c3` destroy/match helpers now use
      those accessors instead of reaching into `g_jit_states[idx]` fields
      directly for normal lifecycle control flow.
  - spill-list layout access is narrower too:
    - `src/lisp/jit_jit_compiler.c3` now exposes backend access helpers for
      spill-list head/count traversal and spill-node field reads.
    - `src/lisp/jit_jit_compiler_lifecycle.c3` spill teardown and spill
      liveness traversal now use those helpers instead of reading
      `g_jit_spill_states`, `g_jit_spill_count`, and `node.*` fields directly
      in normal lifecycle control flow.
  - attachment-table reset now also uses slot helpers:
    - `src/lisp/jit_jit_compiler_lifecycle.c3` now clears the attachment table
      through `runtime_backend_clear_attached_interp(...)` instead of zeroing
      raw slot fields inline during full backend reset.
  - detached-serial pruning now uses slot helpers too:
    - `src/lisp/jit_jit_compiler.c3` now checks attached serials through the
      existing attached-interpreter access helpers instead of reading raw
      attachment slots inline in `jit_is_attached_serial(...)`.
  - cache slot probing/commit now uses helper-backed slot access too:
    - `src/lisp/jit_jit_compiler.c3` now routes `jit_cache_find_slot(...)`
      through `runtime_backend_cache_expr_at(...)`, and
      `jit_cache_commit_slot(...)` through `runtime_backend_store_cache_slot(...)`
      instead of reading/writing `g_jit_cache[slot]` fields inline.
  - cache/tracked-state counter reads are narrower now too:
    - `src/lisp/jit_jit_compiler.c3` now exposes backend helpers for cache
      entry count and tracked-state count.
    - `src/lisp/runtime_backend_hooks.c3` cache clear/store control flow and
      `src/lisp/jit_jit_compiler_lifecycle.c3` tracked-state teardown/liveness
      loops now use those helpers instead of reading/writing
      `g_jit_cache_count` and `g_jit_state_count` directly.
  - lifecycle-global exec/refcount/guard counters are helper-backed too:
    - `src/lisp/jit_jit_compiler.c3` now exposes backend helpers for global
      exec depth, interpreter refcount, and suspended-guard count.
    - `src/lisp/runtime_backend_hooks.c3`, `src/lisp/jit_jit_compiler_lifecycle.c3`,
      and `src/lisp/jit_common.c3` now use those helpers instead of directly
      reading/writing `g_jit_exec_depth`, `g_jit_interp_refcount`, and
      `g_jit_suspended_guard_count` across normal seam/lifecycle control flow.
  - attach-serial allocation is helper-backed too:
    - `src/lisp/jit_jit_compiler.c3` now exposes
      `runtime_backend_next_attach_serial()`.
    - `src/lisp/runtime_backend_hooks.c3` now allocates interpreter attach
      serials through that helper instead of bumping
      `g_jit_attach_serial_counter` inline.
  - initialization-state and owner-token clears are helper-backed too:
    - `src/lisp/jit_jit_compiler.c3` now exposes backend helpers for
      initialized-state reads/writes and owner-thread-token clearing.
    - `src/lisp/jit_jit_compiler_lifecycle.c3` and
      `src/lisp/runtime_backend_hooks.c3` now use those helpers instead of
      flipping `g_jit_initialized` and `g_jit_owner_thread_token` directly in
      lifecycle shutdown/init and detach cleanup.
  - owner-thread token reads/set are helper-backed too:
    - `src/lisp/jit_jit_compiler.c3` now routes `jit_require_owner_thread(...)`
      through owner-token helper entrypoints instead of reading/writing
      `g_jit_owner_thread_token` inline in that migration path.
  - `src/lisp/value_interp_lifecycle.c3` no longer needs `HandleEffectState*`
    in its public teardown helpers; continuation-owned handle state now stays
    at `void*` until the backend seam boundary.
  - status:
    - this is structural groundwork for future interpreter-only fast-build
      profiles.
    - it does not make a true `nojit` target viable yet, because the
      inner interpreter execution path still bottoms out in `jit_eval(...)`
      behind `runtime_eval_expr(...)`.
    - after this pass, non-test runtime code no longer calls
      `jit_eval(...)`, `jit_compile(...)`, `jit_exec(...)`, or `jit_gc()`
      directly outside `src/lisp/runtime_backend_hooks.c3`; the remaining
      non-`jit_*` references are comments and `void*` ownership notes.
    - after this pass, normal runtime-facing code also no longer exposes
      `JitCompiledFn`; the remaining uses are inside JIT modules and tests.
    - cache policy is now also seam-owned; the remaining direct `jit_cache_*`
      usage is migration/test-facing rather than normal runtime control
      flow.
    - interpreter attachment identity and active-exec bookkeeping are now
      seam-owned too; direct `g_jit_attached_interps` manipulation is reduced
      to the seam instead of being scattered across runtime-side helpers.
    - compiled-handle conversion and liveness/exec semantics are now backend
      helper-owned too; the seam still routes through JIT-backed behavior, but
      it no longer interprets JIT compiled state layouts itself.
    - tracked-state retirement policy is now concentrated in backend helpers
      instead of being duplicated in both `jit_gc()` and
      `jit_global_shutdown()`.
    - backend-global “between runs” flag transitions are now concentrated too;
      the remaining raw flag references are narrower and mostly test-facing.
    - compile-pool pressure messaging and GC scheduling are also helper-owned
      now, which narrows the remaining inline backend policy inside the compile
      path itself.
    - direct raw storage access is narrower too; the seam now depends less on
      the attached-interpreter and cache table layouts.
    - liveness query logic is narrower too; tracked-state and spill-list scan
      policy is now centralized instead of duplicated across two call sites.
  - validation:
    - `c3c build`
    - `scripts/build_fast_dev.sh`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/dev-fast/main-dev --eval '(+ 2 40)'`
      => `42`
    - `scripts/build_fast_nodeduce_dev.sh`
    - `LD_LIBRARY_PATH=/usr/local/lib ./build/dev-fast-nodeduce/main-dev-nodeduce --eval '(+ 1 2)'`
      => `3`

- JIT continuation/effect replay repair:
  - fixed handler-continuation replay semantics in
    `src/lisp/jit_jit_runtime_effects.c3` and
    `src/lisp/jit_jit_reset_shift.c3`:
    - `resolve` remains single-shot,
    - explicit `(k ...)` application from `with-continuation` now uses a
      multi-shot replay path,
    - resumed continuations may suspend again when a later `signal` or
      `capture` is semantically valid,
    - resumed `capture`/checkpoint paths now preserve the fresh resume
      delimiter instead of restoring the stale original reset state.
  - fixed stack-switch JIT bookkeeping drift in `src/lisp/jit_common.c3` by
    restoring per-interpreter active exec depth alongside the global JIT exec
    depth.
  - validation:
    - `c3c build`
    - `./scripts/check_jit_env_scope_guards.sh` passed
      (`OMNI_LISP_TEST_SLICE=jit-policy`: `26 passed, 0 failed`)
    - bounded exact repros now return expected values:
      - `(handle (+ (signal bounce 10) (signal bounce 20)) (bounce x (resolve x)))` => `30`
      - `(handle (+ (signal bounce 1) (+ (signal bounce 2) (signal bounce 3))) (bounce x (resolve x)))` => `6`
      - `(handle (+ 1 (signal choose 0)) (choose x (with-continuation k (+ (k 10) (k 20)))))` => `32`
      - `(handle (+ 1 (signal dup 0)) (dup x (with-continuation k (+ (k 10) (k 20)))))` => `32`
      - `(checkpoint (+ (capture k1 (k1 10)) (capture k2 (k2 20))))` => `30`
      - `(block (define (gen-s3) (checkpoint (block (stream-yield 1) (stream-yield 2) (stream-yield 3) nil))) (car (drop 2 (stream-take 3 (gen-s3)))))` => `3`
      - `(block (define fib-t (lambda (n) (if (< n 2) n (+ (signal bounce (lambda (xx) (fib-t (- n 1)))) (signal bounce (lambda (xx) (fib-t (- n 2)))))))) (with-trampoline (lambda (xx) (fib-t 7))))` => `13`
  - status:
    - the former bounded `advanced` continuation/effect failure cluster is
      closed by exact repro.
    - the bounded `advanced` slice still appears to hang late without emitting
      a clean final summary, so backlog item 2 remains open as a runner-level
      blocker rather than an effect/continuation correctness blocker.

- JIT parity harness alignment:
  - updated `src/lisp/tests_harness_helpers.c3` so explicit JIT parity checks
    now evaluate through the same top-level child-scope/finalize/promote flow
    used by `run(...)`, instead of comparing interpreter results against raw
    `jit_exec(...)`.
  - added focused shorthand-accessor HOF probes in
    `src/lisp/tests_core_groups.c3`:
    - `map .1 accessor shorthand car`
    - `map .1 accessor shorthand second`
  - validation:
    - `c3c build`
    - bounded `basic` slice passed:
      `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=basic ./build/main --test-suite lisp`
      (`141 passed, 0 failed`)
    - `./scripts/check_jit_env_scope_guards.sh` passed
      (`OMNI_LISP_TEST_SLICE=jit-policy`: `26 passed, 0 failed`).
  - current follow-up:
    - the old `jit_lookup_var` unaligned-access crash no longer reproduces on
      the bounded `advanced` slice rerun.
    - remaining observed `advanced` failures now cluster around continuation
      and multi-shot effect semantics (`multi-perform*`, `signal multi-resume`,
      `multi-capture sum`, `stream-yield continuation resumes beyond two yields`,
      `multi-shot effect`, `with-continuation basic`, `trampoline fib 7`).
  - updated `docs/plans/codebase-improvement-backlog-2026-03-19.md` and
    `docs/areas/memory-runtime.md` to reflect the narrowed remaining scope.

- JIT env/scope boundary fix:
  - fixed the narrow `jit_lookup_var` unaligned-access crash on the
    iterator-return boundary by forcing iterator-inner thunk detachment during
    destination-built ESCAPE routing in
    `src/lisp/eval_boundary_commit_escape_builders.c3` when the thunk would
    otherwise remain physically resident in the target TEMP lane.
  - added regression
    `run_jit_policy_closure_method_iterator_boundary_test(...)` in
    `src/lisp/tests_runtime_feature_jit_groups_more.c3` for the typed
    closure-method iterator shape that previously crashed through
    `value_environment.c3`.
  - validation:
    - bounded repro is now green:
      `OMNI_VALIDATION_TOOLCHAIN_ROOT=/usr/local OMNI_VALIDATION_EXTRA_ARGS='--mount type=bind,src=/usr/lib/libreplxx.so.0,dst=/usr/lib/libreplxx.so.0,readonly' scripts/run_validation_container.sh env LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_SKIP_TLS_INTEGRATION=1 ./build/main --eval '(length (List (Iterator [1 2 3])))'`
      (`3`)
    - `./scripts/check_jit_env_scope_guards.sh` passed
      (`OMNI_LISP_TEST_SLICE=jit-policy`: `26 passed, 0 failed`).
  - updated `docs/plans/codebase-improvement-backlog-2026-03-19.md` and
    `docs/areas/memory-runtime.md` to mark the narrow JIT guard green while
    keeping the broader `basic`/`advanced` backlog closure work open.

- Subsystem guard scripts:
  - added `scripts/check_scheduler_state_guards.sh` to build locally and rerun the bounded `OMNI_LISP_TEST_SLICE=scheduler` gate as a focused scheduler-state check.
  - added `scripts/check_jit_env_scope_guards.sh` to build locally and rerun the bounded `OMNI_LISP_TEST_SLICE=jit-policy` gate as a focused JIT env/scope check.
  - added `scripts/check_e2e_baseline_policy.sh` to enforce the checked-in `run_e2e.sh` baseline policy (`scripts/baselines/e2e_expected_diff.txt` + `scripts/baselines/e2e_expected_diff.tsv`) even when a live e2e run is not available.
  - validation:
    - `./scripts/check_scheduler_state_guards.sh` passed (`OMNI_LISP_TEST_SLICE=scheduler`: `89 passed, 0 failed`).
    - `./scripts/check_e2e_baseline_policy.sh` passed (manifest + ownership policy aligned; no live diff artifact present in `build/`).
    - `./scripts/check_jit_env_scope_guards.sh` passed after the iterator-boundary fix (`OMNI_LISP_TEST_SLICE=jit-policy`: `26 passed, 0 failed`).
  - updated `docs/plans/codebase-improvement-backlog-2026-03-19.md`, `docs/areas/memory-runtime.md`, and `docs/areas/types-dispatch.md` to record the focused guard-script coverage.

- E2E baseline governance:
  - added `scripts/baselines/e2e_expected_diff.txt` as the checked-in baseline
    `run_e2e.sh` diff manifest and `scripts/baselines/e2e_expected_diff.tsv`
    as the row ownership/review map.
  - updated `scripts/run_e2e.sh` so an exact match against the tracked baseline
    diff no longer masks new regressions; baseline drift still fails with a
    manifest-vs-actual diff preview.
  - updated `docs/areas/types-dispatch.md` and
    `docs/plans/codebase-improvement-backlog-2026-03-19.md` to record the new
    baseline policy and the remaining cleanup target.

- Status-consistency gate:
  - added `scripts/check_status_consistency.sh` to enforce cross-doc status consistency for the active backlog queue.
  - updated `TODO.md` to point the zero-actionable header at `docs/plans/codebase-improvement-backlog-2026-03-19.md`.
  - updated `docs/areas/memory-runtime.md` and `docs/areas/types-dispatch.md` to `yellow` so the area statuses match the still-open blocker and baseline-cleanup queue.

- Runtime modularization batch:
  - `src/lisp/tests_scheduler_boundary_worker.c3` now retains the offload-worker retry, wakeup barrier, and worker-cancel interleave tests, with the nonexecuted-payload release and prestart-cancel shared-release tests moved to `src/lisp/tests_scheduler_boundary_worker_tail.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
      (`89 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_compiler_codegen_groups.c3` now retains the compiler codegen phases 1-4 plus the `qq_macro_primitive` wrapper, with the mutable-capture, integration, iterative-stdlib, and bindgen helper tail moved to `src/lisp/tests_compiler_codegen_groups_tail.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_advanced_core_unicode_groups.c3` now retains the Unicode, constructor, iterator, logic, and effect-continuation test helpers, with the runtime-control and block-syntax contract matrix tests moved to `src/lisp/tests_advanced_core_unicode_groups_runtime.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/value_print.c3` now retains the direct value-printing entrypoints, with the buffer-backed print helpers and `PrintBuf` moved to `src/lisp/value_print_buf.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_ops.c3` now retains parsing, IR, and safety-validation helpers, with the planning/scoring and predicate-index helpers moved to `src/lisp/deduce_rule_ops_planning.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/jit_jit_handle_signal.c3` now retains the signal/dispatch entrypoints, with the shared effect-state, continuation scan, and fast-path helpers moved to `src/lisp/jit_jit_handle_signal_helpers.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/jit_jit_eval_scopes.c3` now retains the JIT lookup/eval wrappers and TCO recycle entrypoints, with the scope-chain, finalize, and recycle helper layer moved to `src/lisp/jit_jit_eval_scopes_helpers.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_deduce_rule_groups.c3` now retains the deduce rule validation helpers and planner-order checks, with the explain payload validation block moved to `src/lisp/tests_deduce_rule_groups_explain.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3` now retains the waiter, cancel, and cancel-conversion boundary families, with the join-wait mapping and mixed boundary-state restore families moved to `src/lisp/tests_scheduler_boundary_thread_task_groups_more_tail.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
      (`89 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups.c3` now retains the scheduler/thread-task core tests, with the waiter and cancel-conversion boundary families moved to `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
      (`89 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_db_handles_mutation.c3` now retains the schema, dirty-tracking, and rule-signature helpers, with the transaction mutation-log helpers moved to `src/lisp/deduce_db_handles_mutation_txn.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_deduce_rule_groups_more.c3` now retains the first four join validation families, with the adaptive-join validation families moved to `src/lisp/tests_deduce_rule_groups_more_join.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/jit_jit_runtime_effects.c3` now retains the resolve-side runtime effect helpers, with the handler-application helpers moved to `src/lisp/jit_jit_runtime_effects_handle.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_scheduler_groups.c3` now retains the spawn/await and run-loop failure scheduler tests, with the scheduler wakeup helpers/tests moved to `src/lisp/tests_scheduler_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
      (`89 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_eval_exec.c3` now retains the delta-set, SCC, and naive-rule execution helpers, with the seminaive execution helpers moved to `src/lisp/deduce_rule_eval_exec_seminaive.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/jit_jit_compiler.c3` now retains the JIT compiler state, cache, and attachment bookkeeping, with lifecycle, GC, exec-depth, and liveness helpers moved to `src/lisp/jit_jit_compiler_lifecycle.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_runtime_feature_jit_groups.c3` now retains the cache, GC, and pre-interpreter-lifetime policy tests, with the multi-interpreter, continuation, handle-state, and capture-boundary policy tests moved to `src/lisp/tests_runtime_feature_jit_groups_tail.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/aot.c3` now retains AOT type/definition helpers and the core runtime bridge entrypoints, with the tail-call trampoline state, invoke/apply helpers, and debug/value helpers moved to `src/lisp/aot_runtime_bridge.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_relation_scan_helpers.c3` now retains iterator/join/count helpers, with relation materialization, comparison, bound parsing, and scan entrypoints moved to `src/lisp/deduce_relation_scan_helpers_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_memory_lifetime_env_copy_groups.c3` now retains the memo, parent rewrite, mixed-chain rewrite, and closure-retain stress tests, with escape/fault/guard/reject/rollback env-copy tests moved to `src/lisp/tests_memory_lifetime_env_copy_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_compiler_core_groups.c3` now retains the syntax, stdlib, feature, and serializer compiler tests, with set/path/continuation groups moved to `src/lisp/tests_compiler_core_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=compiler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=compiler ./build/main --test-suite lisp`
      (`122 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/jit_jit_compiler.c3` now retains the runtime cache and lifecycle helpers, with JIT state tracking and `jit_compile(...)` moved to `src/lisp/jit_jit_compiler_compile.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_ops.c3` now retains parsing, validation, planning, and predicate-index helpers, with explain-plan rendering helpers and the public `deduce/explain` primitive moved to `src/lisp/deduce_rule_ops_explain.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_scheduler_boundary_thread_task_groups.c3` now retains the stress/offload/wakeup/cancel boundary tests, with join-wait failure mapping and mixed state restoration tests moved to `src/lisp/tests_scheduler_boundary_thread_task_groups_more.c3`.
  - `src/lisp/jit_jit_handle_signal.c3` now retains the lower-level effect state and signal dispatch machinery, with eval-result conversion, pending-raise dispatch, warm-clauses handling, handle-state setup, body switching, no-signal finish, implementation, and continuation application moved to `src/lisp/jit_jit_handle_signal_handle.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
      (`89 passed, 0 failed`).
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_eval_exec.c3` now retains seminaive evaluation helpers, with naive evaluation helpers moved to `src/lisp/deduce_rule_eval_exec_naive.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_scheduler_boundary_worker.c3` now retains the offload, wakeup, and thread-task prestart/cancel boundary tests, with the DNS/connect, join-timeout, and shared-retire queue boundary tests moved to `src/lisp/tests_scheduler_boundary_worker_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=scheduler` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=scheduler ./build/main --test-suite lisp`
      (`89 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_deduce_rule_groups_more.c3` now retains the join and schema validation blocks, with the schema-estimate and recursive/analyze validation blocks moved to `src/lisp/tests_deduce_rule_groups_more_tail.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/aot.c3` now retains AOT type/definition helpers, with the closure/runtime bridge helpers moved to `src/lisp/aot_runtime_bridge.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=jit-policy` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=jit-policy ./build/main --test-suite lisp`
      (`24 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_advanced_core_unicode_groups.c3` now retains the unicode, logic, effect, and runtime-control test groups, with block-syntax, lambda-syntax, and binding/mutation groups moved to `src/lisp/tests_advanced_core_unicode_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=advanced` gate inside the Docker-bounded validation wrapper still hits the pre-existing unaligned-access panic in `value_environment.c3` via `jit_lookup_var`:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=advanced ./build/main --test-suite lisp`

- Runtime modularization batch:
  - `src/lisp/deduce_db_handles.c3` now retains LMDB externs, schema registration, and relation/index helpers, with lower mutation/state bookkeeping and transaction helpers moved to `src/lisp/deduce_db_handles_mutation.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_deduce_query_bench_groups.c3` now retains seed/assert helpers and query benchmark support, with benchmark runner/reporting entrypoints moved to `src/lisp/tests_deduce_query_bench_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_eval.c3` now retains SCC planning, with execution/context helpers and rule evaluation dispatch moved to `src/lisp/deduce_rule_eval_exec.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/deduce_rule_eval.c3` now retains SCC planning, delta helpers, and naive/semi-naive rule evaluation, with recursive/component fixpoint evaluation helpers moved to `src/lisp/deduce_rule_eval_fixpoint.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
      `LD_LIBRARY_PATH=/usr/lib:/usr/local/lib OMNI_TEST_QUIET=1 OMNI_SKIP_TLS_INTEGRATION=1 OMNI_LISP_TEST_SLICE=deduce ./build/main --test-suite lisp`
      (`72 passed, 0 failed`).

- Runtime modularization batch:
  - `src/lisp/tests_deduce_rule_groups.c3` now retains helper utilities plus rule-definition and explain smoke coverage, with join/schema/analyze validation blocks moved to `src/lisp/tests_deduce_rule_groups_more.c3`.
  - validation:
    - `c3c build`
    - `OMNI_LISP_TEST_SLICE=deduce` gate passed inside the Docker-bounded validation wrapper:
